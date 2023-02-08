;;
;; Copyright 2022, Jaidyn Levesque <jadedctrl@posteo.at>
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
		(chicken pretty-print) (chicken process-context)
		(chicken process-context posix) (chicken string)
		srfi-1 srfi-13 srfi-19 srfi-69 srfi-180
		ircc
		xattr
		getopt-long)


;; Returns the path of a channel's directory
(define (channel-directory-path conn channel)
  (string-append (hash-table-ref conn 'directory)
				 "/" channel "/"))


;; Returns the .users/ path of a channel
(define (users-directory-path conn channel)
  (string-append (channel-directory-path conn channel)
				 ".users/"))


;; Tidies up a channel directory; removes `online` and `offline` user links, etc.
(define (cleanup-channel conn channel)
  #t)


;; Creates a channel's file hierarchy, if need be
(define (make-channel conn channel)
  (let ([path (channel-directory-path conn channel)])
	(create-directory (string-append path "/.users/online") #t)
	(create-directory (string-append path "/.users/offline") #t)
	(create-directory (string-append path "/.users/all") #t)
	(cleanup-channel conn channel)))


;; Creates a user's info files in the given channel, if need bee
(define (make-user conn channel hostmask)
  (let ([path (string-append (channel-directory-path conn channel)
							 "/.users/all/"
							 (irc:hostmask-nick hostmask) "/")])
	(create-directory path #t)
	(call-with-output-file (string-append path "hostmask")
	  (lambda (out-port)
		(write-string hostmask #f out-port)))))


;; Removes/Adds a symbolic link to a subdirectory of users/ named `state`.
(define (user-toggle-state conn channel user state)
  #f)


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
      [(eq? reply RPL_WELCOME)
	   (irc:write-cmd conn "JOIN" "#thevoid")]

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
			  (make-user conn (second params)
							   (irc:user-get conn nick 'hostmask)))
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

	  (hash-table-set! connection 'directory directory)

	  (irc:loop connection
				on-command
				on-reply))))


(main)
