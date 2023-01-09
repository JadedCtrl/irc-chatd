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
		(chicken file) (chicken file posix) (chicken io) (chicken process-context)
		(chicken process-context posix) (chicken string)
		srfi-1 srfi-69
		ircc
		json
		getopt-long)


(define (on-command conn cmd params #!optional sender)
  (print sender ":" cmd params))


(define (on-reply conn reply params #!optional sender)
  (print (hash-table->alist conn))
  (print (hash-table->alist (hash-table-ref conn 'users)))
;;  (print sender ":" reply params)
  (cond
      [(eq? reply RPL_WELCOME)
	(irc:write-cmd conn "JOIN" "#thevoid")]))


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
	 (value (required NAME)))))


;; Prints cli usage to stderr.
(define (help)
  (write-string *help-msg* #f (open-output-file* fileno/stderr))
  (write-string (usage *opts*) #f (open-output-file* fileno/stderr)))


;; The `main` procedure that should be called to run feedsnake-unix for use as script.
(define (main)
  (let* ([args (getopt-long (command-line-arguments) *opts*)]
		 [free-args (alist-ref '@ args)])
	(if (or (null? free-args) (alist-ref 'help args))
		(help)
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
					   6697)])
		  (if server
			  (irc:loop (irc:connect server port username nickname password fullname)
						on-command
						on-reply)
			  (help))))))


(main)
