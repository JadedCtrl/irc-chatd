# utils

Some simple scripts that can make irc-chatdir a friendly option for a client.

## chatdir-in.sh
Messages are sent, in chatdir, via .in/ directories under each channel's dir.
The file can be named whatever, it just has to be a text-file, and it'll be
sent. But `$ echo "Hi there!" > #general/.in/msg` is annoying.

This is a wrapper around that. It will send whatever you type to the selected
channel; selecting a channel is done by just typing a channel name verbatim.
There is auto-completion for nicks and channel-names.

You might run it like, `$ chatdir-in.sh ~/chat/libera.chat/`


## irc_file_notify.sh
Sends a pretty notification (with icon, sender, and message text!) whenever a
message is sent. You might run like, `$ irc_file_notify.sh ~/chat/libera.chat/*`


## proc_notify.sh
Displays a warning notifcation when irc-chatdir dies.  
You might run like, `$ proc_notify.sh irc-chatd.scm.*leagueh`


