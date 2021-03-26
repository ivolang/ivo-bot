---
--- HACK:
--- This entire program was just quickly hacked together.
--- It is in dire need of cleanup, and I certainly do intend to clean it up ASAP!
--- However, for now, it /works/, and I want a break from working on it for the night.
---
--- FIXME: Don't let the bot get caught in an infinite loop
--- TODO: commands regarding typechecking/clearing history, like with other stuff
--- TODO: a per-user persistent definition history (1 week time limit?), allow importing from other users
--- TODO: also implement an IRC bot
--- TODO: better output format (e.g. I defined this crap: ... I evaluated these values: ...)
--- TODO: add help message
--- TODO: add support for DMs
--- TODO: allow dumping traces to text files and uploading them
--- TODO: allow listing current definitions/dumping session history or definitions to file
--- ^ this stuff belongs in issues once this code is cleaned up
---
module Main where

import LambdaCalculus

import Discord
import Discord.Internal.Rest (Request)
import Discord.Requests
import Discord.Types

import Control.Monad (void, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError, liftEither, catchError, lift)
import Control.Monad.State (StateT, State, runState, runStateT, get, modify')
import Data.Either (lefts, rights)
import Data.Foldable (fold)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
import Data.Text qualified as T
import Data.Text.IO (readFile, putStrLn)
import Prelude hiding (readFile, putStrLn)
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec.Text (Parser)
import UnliftIO (MonadUnliftIO, liftIO)

main :: IO ()
main = do
  token <- readFile "private/token"
  emoji <- readEmoji <$> readFile "private/emoji"
  defs <- newIORef HM.empty

  t <- runDiscord $ def { discordToken = token
                        , discordOnStart = startHandler
                        , discordOnEnd = liftIO $ putStrLn "Goodbye!"
                        , discordOnEvent = eventHandler emoji defs
                        , discordOnLog = \s -> putStrLn s >> putStrLn ""
                        }

  putStrLn t

type Definitions = HashMap Text CheckExpr

readEmoji :: Text -> (EmojiId, Text)
readEmoji emoji = (emojiId, emoji)
  where emojiId = read $ T.unpack $ last $ T.split (== ':') emoji

startHandler :: DiscordHandler ()
startHandler = do
  sendCommand $ UpdateStatus $ UpdateStatusOpts
    { updateStatusOptsSince     = Nothing
    , updateStatusOptsGame      = Just
        Activity { activityName = "@ me your Ivo code"
                 , activityType = ActivityTypeGame
                 , activityUrl  = Just "https://github.com/ivolang/ivo-bot"
                 }
    , updateStatusOptsNewStatus = UpdateStatusOnline
    , updateStatusOptsAFK       = False
    }

eventHandler :: (EmojiId, Text) -> IORef Definitions -> Event -> DiscordHandler ()
eventHandler (emojiId, emojiRef) defs = \case
  MessageCreate message -> handleMessage emojiRef defs message
  MessageReactionAdd reactionInfo -> handleReaction emojiId reactionInfo
  _ -> pure ()

type MyHandler a = ExceptT Text DiscordHandler a

-- | Respond to messages that mention me.
-- Always mention the person who sent the message
-- and add an @:ivo:@ reaction so that they can delete the message.
handleMessage :: Text -> IORef Definitions -> Message -> DiscordHandler ()
handleMessage emoji defsRef msg = logErrors do
  let mentionSender = mentionUser $ userId $ messageAuthor msg

  let code = extractCodeBlocks $ messageText msg
  let mExprs = map (parse programParser "message") code
  let exprOrDecls = concat $ rights mExprs

  let syntaxErrors = lefts mExprs
  let syntaxErrorMessage =
        mentionSender <> " I couldn't parse that!\n" <>
        formatCodeBlocks (fmap (T.pack . show) syntaxErrors)

  myId <- lift getMyId

  when (messageMentionsP myId msg) if
    | not (null syntaxErrors) ->
        void $ sendMessage emoji "syntax err" (messageChannel msg) syntaxErrorMessage
    | otherwise -> messageErrors emoji mentionSender "Type checking error" (messageChannel msg) $ do
        defs <- liftIO $ readIORef defsRef
        let (stuffWithDefs, defs') = applyDefs defs exprOrDecls
        types <- mapM inferType stuffWithDefs
        let exprs = map (eval . check2eval) $ rights $ stuffWithDefs
        liftIO $ atomicWriteIORef defsRef defs'
        void $ sendMessage emoji "types and exprs" (messageChannel msg) $
          formatTypesAndExprsMessage mentionSender types exprs
  pure ()

applyDefs :: Definitions -> [Either (Text, AST) AST]
                -> ([Either (Text, CheckExpr) CheckExpr], Definitions)
applyDefs defs junk = runState (traverse go junk) defs
  where
    go :: Either (Text, AST) AST -> State Definitions (Either (Text, CheckExpr) CheckExpr)
    go it = do
      defs' <- get
      case it of
        Left (name, expr) -> do
          let expr' = substitute defs' $ ast2check $ expr
          modify' $ HM.insert name expr'
          pure $ Left (name, expr')
        Right expr -> do
          pure $ Right $ substitute defs' $ ast2check $ expr

inferType :: Either (Text, CheckExpr) CheckExpr -> MyHandler (Maybe Text, Scheme)
inferType = \case
  Left (name, expr) -> do
    ty <- liftEither $ infer expr
    pure (Just name, ty)
  Right expr -> (,) Nothing <$> liftEither (infer expr)

formatTypesAndExprsMessage :: Text -> [(Maybe Text, Scheme)] -> [EvalExpr] -> Text
formatTypesAndExprsMessage mention types exprs =
  mention <> " I inferred these types: \n"
  <> formatCodeBlock (T.unlines $ map formatType types)
  <> if not (null exprs)
        then "I computed these values: \n" <> formatCodeBlock (T.unlines $ map unparseEval exprs)
        else ""
  where
    formatType (Nothing, ty) = ": " <> unparseScheme ty
    formatType (Just name, ty) = name <> " : " <> unparseScheme ty

-- | Delete messages if the person who triggered them
-- reacts with the @:ivo:@ emoji.
handleReaction :: EmojiId -> ReactionInfo -> DiscordHandler ()
handleReaction emoji react = logErrors do
  let msgRef = (reactionChannelId react, reactionMessageId react)

  when (maybe False (emoji ==) $ emojiId (reactionEmoji react)) do
    myId <- lift getMyId
    msg <- restCall' "Err getting react msg: " $ GetChannelMessage msgRef
    when (myId == userId (messageAuthor msg) &&
          messageMentionsP (reactionUserId react) msg
         ) $
      restCall' "Err deleting msg from react: " $ DeleteMessage msgRef

  pure ()

getMyId :: DiscordHandler UserId
getMyId = userId . _currentUser <$> readCache

logErrors :: MyHandler a -> DiscordHandler ()
logErrors m = do
  result <- runExceptT m
  case result of
    Left err -> liftIO $ putStrLn err
    Right _ -> pure ()

messageErrors :: Text -> Text -> Text -> ChannelId -> MyHandler a -> MyHandler a
messageErrors emoji mention msgType chanId m = catchError m \e -> do
  let msg = mention <> " " <> msgType <> ":\n" <> formatCodeBlock e
  void $ sendMessage emoji msgType chanId msg
  throwError e

withIOState :: MonadUnliftIO m => IORef a -> StateT a m b -> m b
withIOState ref m = do
  deref <- liftIO $ readIORef ref
  (x, deref') <- runStateT m deref
  liftIO $ atomicWriteIORef ref deref'
  pure x

restCall' :: (FromJSON a, Request (r a)) => Text -> r a -> MyHandler a
restCall' msg req = do
  result <- lift $ restCall req
  case result of
    Left err -> throwError $ msg <> T.pack (show err)
    Right ok -> pure ok

sendMessage :: Text -> Text -> ChannelId -> Text -> MyHandler Message
sendMessage emoji msgType chanId txt = do
  msg <- restCall' ("Err sending " <> msgType <> ": ") $ CreateMessage chanId txt
  () <- restCall' ("Err reacting to " <> msgType <> ": ") $
    CreateReaction (chanId, messageId msg) $ "<" <> emoji <> ">"
  pure msg

messageMentionsP :: UserId -> Message -> Bool
messageMentionsP myId = any (\user -> userId user == myId) . messageMentions

mentionUser :: UserId -> Text
mentionUser id = "<@" <> T.pack (show id) <> ">"

formatCodeBlock :: Text -> Text
formatCodeBlock msg = "```\n" <> msg <> "\n```\n"

formatCodeBlocks :: [Text] -> Text
formatCodeBlocks = foldr (\msg -> (<>) $ formatCodeBlock msg) ""

extractCodeBlocks :: Text -> [Text]
extractCodeBlocks = fold . parse codeBlockParser "message"
  where
    codeBlockParser :: Parser [Text]
    codeBlockParser = many (skipJunk *> (codeBlock <|> codeBit) <* skipJunk)

    skipJunk :: Parser ()
    skipJunk = skipMany (noneOf "`")

    codeBlockDelimiter = string "```"

    codeBlock :: Parser Text
    codeBlock = do
      _ <- try codeBlockDelimiter
      optional $ string "ivo"
      code <- many1 (notFollowedBy codeBlockDelimiter >> anyChar)
      codeBlockDelimiter
      pure $ T.pack code

    codeBit :: Parser Text
    codeBit = do
      char '`'
      code <- many1 (noneOf "`")
      char '`'
      pure $ T.pack code
