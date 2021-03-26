# ivo-bot
This is a Discord bot which provides interactive access
to the Ivo interpreter for demonstration in chat.

Format your Ivo code as code in Discord (e.g. with code blocks),
*and `@mention` the bot*, and it will typecheck and evaluate your code.

You may define values using the bot as you would in a REPL.
Defined values persist across messages (but not when the bot is shut down)
and the definitions are globally shared between all users,
so if someone else defines something, you can use it too.

If you triggered the bot to respond to you (so that it `@mention`ed you),
you can ask the bot to delete its message
by reacting to its message with the emoji specified in the bot's configuration.
(The bot itself will automatically react to its own message,
so all you have to do is click the emoji.)

## Usage
Create these two files *with no trailing newlines* to configure the bot:

* Put your Discord bot token in `private/token`

* Put the emoji you want the bot to use for deleting messages in `private/emoji`
  (it should look something like `<:ivo:824858787532898354>`)
  
You can get the raw emoji using `\:emoji_name:` in your Discord client.
The bot currently only allows for server-defined emoji, not Unicode emoji.

The bot can then be run with `stack run`.
Don't forget the add the bot to your server!
