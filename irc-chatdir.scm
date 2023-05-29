 #! /bin/sh
 #|
 exec csi -s "$0" "$@"
 |#

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
        (chicken file) (chicken io) (chicken pathname)
        (chicken process-context) (chicken process-context posix)
        (chicken string)
        srfi-1 srfi-18 srfi-69
        (prefix chatdir chatdir:) (prefix chatdir-inotify chatdir:)
        ircc
        getopt-long)


;; Join an IRC channel
(define (make-join-channel-callback connection)
  (let ([root-dir (hash-table-ref connection 'directory)])
    (lambda (channel)
      (irc:write-cmd connection "JOIN" channel))))


;; Leave an IRC channel
(define (make-leave-channel-callback connection)
  (let ([root-dir (hash-table-ref connection 'directory)])
    (lambda (channel)
      (irc:write-cmd connection "PART" channel))))


;; Send message to an IRC channel
(define (make-send-message-callback connection)
  (let ([root-dir (hash-table-ref connection 'directory)])
    (lambda (channel message)
      (irc:write-cmd connection "PRIVMSG" channel message)
      (chatdir:channel-message-add! root-dir channel message
                                    (hash-table-ref connection 'nick)))))


;; Hook function for irc:loop; handles all IRC commands
(define (make-irc-command-callback conn)
  (let ([root-dir (hash-table-ref conn 'directory)])
    (lambda (conn cmd params #!optional sender)
      (cond
       [(and (string=? cmd "PRIVMSG")
             (string? sender)
             (irc:hostmask? sender))
        (let ([target (if (irc:user-is-self? conn (car params))
                          (irc:hostmask-nick sender)
                          (car params))])
          (chatdir:channel-message-add! root-dir target
                                        (last params) (irc:hostmask-nick sender)))]

       [(or (string=? cmd "NOTICE")
            (and (string=? cmd "PRIVMSG")
                 (or (string-null? sender) (not (irc:hostmask? sender)))))
        (chatdir:channel-message-add! root-dir ".server" (last params))]

       [(and (string=? cmd "JOIN") (irc:user-is-self? conn sender))
        (chatdir:channel-add! root-dir (last params))]

       [(string=? cmd "JOIN")
        (let ([channel (car params)]
              [nick (irc:hostmask-nick sender)])
          (chatdir:channel-user-add! root-dir channel nick)
          (chatdir:channel-user-toggle-states! root-dir channel nick
                                               "online" "offline"))]

       [(string=? cmd "PART")
        (chatdir:channel-user-toggle-states!
         root-dir (car params) (irc:hostmask-nick sender)
         "offline" "online")]))))
    ;;   [(string=? cmd "NICK")
    ;; (chatd-json-write conn
;;	 (compose-event-alist conn "user-info" #:user (last params)))])


;; Hook function for irc:loop; handles all IRC errors and replies
(define (make-irc-reply-callback conn)
  (let ([root-dir (hash-table-ref conn 'directory)])
    (lambda (conn reply params #!optional sender)
      (cond
       ;; If topic set, output to a channel's .topic file
       [(and (eq? reply RPL_TOPIC)
             (irc:channel? (second params)))
        (chatdir:channel-metadata-set! root-dir (second params)
                                       "topic" (last params))]

       [(and (eq? reply RPL_TOPICWHOTIME)
             (irc:channel? (second params)))
        (chatdir:channel-metadata-set! root-dir (second params)
                                       "topic" #f
                                       `((user.chat.sender . ,(third params))
                                         (user.chat.date . ,(last params))))]

       ;; We've got to add users, when they join the room!
       [(or (and (irc:capability? conn 'userhost-in-names)
                 (eq? reply RPL_ENDOFNAMES))
            (eq? reply RPL_ENDOFWHO))
        (map (lambda (nick)
               (let ([hostmask (irc:user-get conn nick 'hostmask)]
                     [channel (second params)])
                 (chatdir:channel-user-add! root-dir channel nick)
                 (chatdir:channel-user-toggle-states! root-dir channel nick
                                                      "online" "offline")))
             (irc:channel-users conn (second params)))]

       [#t
        (chatdir:channel-message-add! root-dir ".server" (last params) "server")]))))


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
		 [port (string->number (or (last (string-split server ":"))
								   "6697"))]
		 [directory (normalize-pathname
					 (string-append (or (alist-ref 'directory args) "./")
									"/"))]
		 [connection (if server
						 (irc:connect hostname port username nickname password fullname)
						 #f)])

	  (unless connection
		(help))

	  (hash-table-set! connection 'directory
					   (normalize-pathname (string-append directory "/")))
	  (create-directory (string-append directory "/.server"))

	  ;; Kick off the input loop, which monitors channels' .in/ dirs
	  (thread-start!
	   (make-thread
		(lambda ()
          (let ([callbacks
                 `((join-channel . ,(make-join-channel-callback connection))
                   (leave-channel . ,(make-leave-channel-callback connection))
                   (send-message . ,(make-send-message-callback connection)))])
            (thread-sleep! 10)
            (chatdir:input-loop-init directory callbacks)
		    (chatdir:input-loop directory callbacks)))
		"Chat input"))

	  (print (hash-table-ref connection 'directory))
	  ;; Kick off the main loop!
	  (irc:loop connection
				(make-irc-command-callback connection)
				(make-irc-reply-callback connection)))))


(main)
