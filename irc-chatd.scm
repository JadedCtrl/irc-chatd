;;
;; Copyright 2023, Jaidyn Levesque <jadedctrl@posteo.at>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;

(import scheme
		(chicken file) (chicken file posix) (chicken io) (chicken keyword) (chicken random)
		(chicken pathname)
		(chicken pretty-print) (chicken process-context)
		(chicken process-context posix) (chicken string)
		srfi-1 srfi-13 srfi-18 srfi-19 srfi-69 srfi-180
		inotify
		ircc
		xattr
		getopt-long)


;; Wrapper around `directory` that lists a dir's files as a relative path
(define (directory-rel #!optional (path "./"))
  (let ([relative-parent (normalize-pathname (string-append path "/"))])
	(map (lambda (leaf)
		   (string-append relative-parent leaf))
		 (directory path))))


;; Returns an inotify watch-descriptor according the given path
(define (path->wd path)
  (car
   (filter (lambda (wd)
			 (string=? (normalize-pathname (wd->path wd))
					   (normalize-pathname path)))
		   (wd-list))))


;; Attempt to remove an inotify watch; if it's already been removed, no sweat
;; (This happens sometimes when inotify automatically deletes a watch)
(define (attempt-remove-watch! watch)
  (handle-exceptions exn
	  #t
	(remove-watch! watch)))


;; Returns the path of a channel's directory
(define (channel-directory-path conn channel)
  (string-append (hash-table-ref conn 'directory)
				 "/" channel "/"))


;; Returns the .users/ path of a channel
(define (channel-users-directory-path conn channel)
  (string-append (channel-directory-path conn channel)
				 ".users/"))


;; Main directory path of the given user
(define (channel-user-directory-path conn channel hostmask #!optional (state "all"))
  (string-append (channel-users-directory-path conn channel)
				 state "/" (irc:hostmask-nick hostmask)))


;; Main directory path of the given user
(define (user-directory-path conn channel hostmask)
  (string-append (channel-users-directory-path conn channel)
				 "all/" hostmask))


;; Tidies up a channel directory; removes `online` and `offline` user links, etc.
(define (cleanup-channel conn channel)
  (let ([users-dir (channel-users-directory-path conn channel)])
	(map
	 (lambda (state-dir)
	   (if (not (substring-index state-dir "/all"))
		   (map
			(lambda (link)
			  (let ([link-path (string-append users-dir state-dir "/" link)])
				(if (symbolic-link? link-path)
					(delete-file link-path))))
			(directory (string-append users-dir state-dir)))))
	   (directory users-dir))))



;; Creates a channel's file hierarchy, if need be
(define (make-channel conn channel)
  (let* ([path (channel-directory-path conn channel)]
		 [subpath (lambda (leaf) (string-append path leaf))])
	(create-directory (subpath ".in") #t)
	(create-directory (subpath ".users/online") #t)
	(create-directory (subpath ".users/offline") #t)
	(create-directory (subpath ".users/all") #t)
	(cleanup-channel conn channel)))


;; Creates a user's info files in the given channel, if need bee
(define (make-user conn channel hostmask)
  (create-directory (user-directory-path conn channel hostmask) #t))


;; Disables a user-state (that is, removes a symlink from a .users directory
(define (user-disable-state conn channel hostmask state)
  (let ([state-link
		 (create-directory (channel-user-directory-path conn channel hostmask state) #t)])
	(if (or (file-exists? state-link)
			(symbolic-link? state-link))
		(delete-file state-link))))


;; Enables a user-state (that is, makes a symlink to a .users directory
(define (user-enable-state conn channel hostmask state)
  (let ([state-link
		 (create-directory (channel-user-directory-path conn channel hostmask state) #t)])
	(if (not (or (file-exists? state-link)
				 (symbolic-link? state-link)))
		(create-symbolic-link (string-append "../all/" hostmask)
							  state-link))))


;; Ensures the enabled-state is enabled, and it's opposite (disabled-state) is not
(define (user-toggle-state conn channel hostmask enabled-state disabled-state)
  (user-disable-state conn channel hostmask disabled-state)
  (user-enable-state conn channel hostmask enabled-state))


;; Returns the appropriate, non-colliding file path of a hypothetical message
(define (message-file-path conn channel #!optional (suffix ""))
  (let ([path
		 (string-append (channel-directory-path conn channel)
						(date->string (current-date) "[~m-~d] ~H:~M:~S")
						suffix)])
	(if (file-exists? path)
		(message-file-path conn channel
						   (number->string (+ (or (string->number suffix) 0) .1)))
		path)))


;; Create a message file; putting metadata in xattrs, and text directly in the file
(define (make-message-file conn channel sender message)
  (let ([file (message-file-path conn channel)])
	(call-with-output-file file
	  (lambda (out-port) (write-string message #f out-port)))
	(set-xattr file "user.chat.sender" sender)
	(set-xattr file "user.chat.date" (date->string (current-date) "~1T~2"))
	(set-xattr file "user.chat.channel" channel)
	(set-xattr file "user.chat.mime" "text/plain")))


;; Sets a channel's .topic file
(define (set-channel-topic conn channel topic #!optional (username #f) (date #f))
  (let ([topic-path (string-append (channel-directory-path conn channel)
								   ".topic")])
	(if (string? topic)
		(call-with-output-file
			topic-path
		  (lambda (out-port)
			(write-string topic #f out-port))))
	(if username
		(set-xattr topic-path "user.chat.sender" (irc:hostmask-nick username)))))


;; Join an IRC channel
(define (join-channel connection channel)
  (irc:write-cmd connection "JOIN" channel)
  (make-channel connection channel))


;; Send message to an IRC channel
(define (send-message connection channel message)
  (irc:write-cmd connection "PRIVMSG" channel message)
  (make-message-file connection channel
					 (hash-table-ref connection 'nick)
					 message))


;; Hook function for irc:loop; handles all IRC commands
(define (on-command conn cmd params #!optional sender)
  (cond
   [(and (string=? cmd "PRIVMSG")
		 (string? sender)
		 (irc:hostmask? sender))
	(let ([target (if (irc:user-is-self? conn (car params))
					  (irc:hostmask-nick sender)
					  (car params))])
	  (make-message-file conn target (irc:hostmask-nick sender) (last params)))]

   [(or (string=? cmd "NOTICE")
		(and (string=? cmd "PRIVMSG")
			 (or (string-null? sender) (not (irc:hostmask? sender)))))
	(make-message-file conn ".server" "server" (last params))]

   [(and (string=? cmd "JOIN") (irc:user-is-self? conn sender))
	(make-channel conn (last params))]

   [(string=? cmd "JOIN")
	(make-user conn (last params) sender)]

;;   [(string=? cmd "NICK")
;;	(chatd-json-write conn
;;	 (compose-event-alist conn "user-info" #:user (last params)))])
))


;; Hook function for irc:loop; handles all IRC errors and replies
(define (on-reply conn reply params #!optional sender)
  (cond
	  ;; If topic set, output to a channel's .topic file
	  [(and (eq? reply RPL_TOPIC)
			(irc:channel? (second params)))
	   (set-channel-topic conn (second params) (last params))]

	  [(and (eq? reply RPL_TOPICWHOTIME)
			(irc:channel? (second params)))
	   (set-channel-topic conn (second params) #f (third params) (last params))]

	  ;; We've got to add users, when they join the room!
	  [(or (and (irc:capability? conn 'userhost-in-names)
				(eq? reply RPL_ENDOFNAMES))
		   (eq? reply RPL_ENDOFWHO))
	   (map (lambda (nick)
			  (let ([hostmask (irc:user-get conn nick 'hostmask)]
					[channel (second params)])
				(make-user conn channel hostmask)
				(user-toggle-state conn channel hostmask "online" "offline")))
			(irc:channel-users conn (second params)))]

	  [#t
	   (make-message-file conn ".server" "server" (last params))]))


(define *help-msg*
  (string-append
   "usage: irc-chatd [-h] [-n nick] [-u user] [-p password] hostname\n\n"
   "`chatd` is a standard format for chat client-daemons; the goal being that a\n"
   "chat client should be able to work with any chat protocol (IRC, XMPP, etc)\n"
   "just by reading and writing to files served by a `chatd` daemon, without\n"
   "having to worry about the protocol in use.\n\n"
   "irc-chatd is a `chatd`-compliant IRC client-daemon, that outputs all messages\n"
   "from the server in parseable format to an output file, and receives input\n"
   "from a FIFO File.\n".))


(define *opts*
  '((help
	 "Print a usage message"
	 (single-char #\h))
	(nickname
	 "Your preferred nickname. Default is your system username."
	 (single-char #\n)
	 (value (required NICK)))
	(username
	 "Username of the connection. Default is your system username."
	 (single-char #\u)
	 (value (required USERNAME)))
	(password
	 "The password optionally used in connection."
	 (single-char #\p)
	 (value (required PASSWORD)))
	(name
	 "Set the realname of your connection."
	 (value (required NAME)))
	(directory
	 "Root directory for channels and messages. Defaults to CWD."
	 (single-char #\o)
	 (value (required PATH)))))


;; Prints cli usage to stderr.
(define (help)
  (write-string *help-msg* #f (open-output-file* fileno/stderr))
  (write-string (usage *opts*) #f (open-output-file* fileno/stderr))
  (exit 1))


(define (wait-for-registration connection)
  (if (not (hash-table-exists? connection 'registered))
	  (begin
		(thread-sleep! .1)
		(wait-for-registration connection))
	  #t))


;; Initialization for the input loop
(define (input-loop-init connection)
  (let ([irc-dir (hash-table-ref connection 'directory)])
	(init!)
	;; Start watching top-level IRC dir (for new channel joins, etc)
	(add-watch! irc-dir
				'(onlydir moved-to moved-from delete delete-self create))


	;; Can't actually join channels below, unless we're connected! :P
	(wait-for-registration connection)

	;; Start watching input dirs of all pre-existing channel dirs
	(map (lambda (path)
		   (let ([in-path (normalize-pathname (string-append path "/.in"))]
				 [dirname (pathname-file (pathname-directory (string-append path "/")))])
			 (join-channel connection dirname)

			 (add-watch! in-path '(moved-to close-write))
			 (print "Joined and watching: " in-path)))
		 (filter directory-exists? (directory-rel irc-dir)))))


;; Handles all inotify-watched file events from the top-level IRC-directory.
;; Mainly, checking for newly-joined or left channels.
(define (handle-main-dir-event connection event)
  (let ([flags (event-flags event)])
	(cond
	 ;; If a channel dir's been moved or removed, stop watching (ofc)
	 ;; … Also quit that room! Heck them!
	 [(or (member 'moved-from flags)
		  (member 'delete flags)
		  (member 'delete-self flags))
	  (let* ([channel (event-name event)]
			 [channel-inpath (string-append (wd->path (event-wd event)) channel "/.in")]
			 [channel-wd (path->wd channel-inpath)])
		(print "Leaving " channel "…")
		(print "Remove watch for " channel-inpath "…")

		(if (and channel-wd (member channel-wd (wd-list)))
			(attempt-remove-watch! channel-wd))
		(if (member channel (irc:channels connection))
			(irc:write-cmd connection "PART" channel)))]

	 ;; If a dir's been created for a channel, maybe-join, then watch input!
	 [(or (member 'create flags)
		  (member 'moved-to flags))
	  (let* ([path (event->pathname event)]
			 [in-path (normalize-pathname (string-append path "/.in"))]
			 [dirname (pathname-file (pathname-directory (string-append path "/")))])
		(print "Joining channel " dirname "…")
		(join-channel connection dirname)

		(add-watch! in-path '(moved-to close-write))
		(print "Began watching input " in-path "."))])))


;; Handles an inotify event that pertains to a channel's .in/ directory
(define (handle-channel-dir-event connection event)
  (let* ([event-dir (pathname-directory (event->pathname event))]
		 [dirname (pathname-file event-dir)]
		 [parent-dirname (pathname-file (pathname-directory event-dir))])
	(cond
	 ;; If input is given to an `.in` dir, and its channel is still valid…
	 ;; well, send that darn message(s)! What're you waiting for?!
	 [(and (string=? dirname ".in")
		   (member parent-dirname (irc:channels connection)))
	  (print "Sending message(s) [" (event->pathname event) "] to " parent-dirname "…")
	  (map (lambda (message)
			 (send-message connection parent-dirname message))
		   (with-input-from-file (event->pathname event)
			 read-lines))
	  (delete-file* (event->pathname event))]

	 ;; If input is given to `.in`, but its channel is invalid… let's give up.
	 [(string=? dirname ".in")
	  (print "Removing watch on " dirname "…")
	  (attempt-remove-watch! (event-wd event))])))


;; Handle a single inotify file event, as part of the input loop
(define (handle-file-event connection event)
  (if (not (member 'ignored (event-flags event)))
	  (let* ([flags (event-flags event)]
			 [wd-path (wd->path (event-wd event))]
			 [main-dir? (string=? wd-path (hash-table-ref connection 'directory))])
		(if main-dir?
			(handle-main-dir-event connection event)
			(handle-channel-dir-event connection event)))))


;; The FS-backed input loop, to be run in a seperate thread (so as to not block)
;; This handles channel leaving/joining, and sending messages
(define (input-loop connection #!optional (init #f))
  (if (not init)
	  (input-loop-init connection))

  (map (lambda (event) (handle-file-event connection event))
	   (next-events!))

  (input-loop connection #t))


;; The `main` procedure that should be called to run feedsnake-unix for use as script.
(define (main)
  (let* ([args (getopt-long (command-line-arguments) *opts*)]
		 [free-args (alist-ref '@ args)])
	(if (or (null? free-args) (alist-ref 'help args))
		(help))

	(let*
		([username (or (alist-ref 'username args)
					   (current-effective-user-name))]
		 [password (alist-ref 'password args)]
		 [nickname (or (alist-ref 'nickname args)
					   (current-effective-user-name))]
		 [fullname (alist-ref 'name args)]
		 [server (last free-args)]
		 [hostname (first (string-split server ":"))]
		 [port (or (string->number (last (string-split server ":")))
				   6697)]
		 [directory (or (alist-ref 'directory args) "./")]
		 [connection (if server
						 (irc:connect server port username nickname password fullname)
						 #f)])

	  (unless connection
		(help))

	  (hash-table-set! connection 'directory
					   (normalize-pathname (string-append directory "/")))

	  ;; Kick off the input loop, which monitors channels' .in/ dirs
	  (thread-start!
	   (make-thread
		(lambda ()
		  (input-loop connection))
		"Chat input"))

	  ;; Kick off the mani loop!
	  (irc:loop connection
				on-command
				on-reply))))


(main)
