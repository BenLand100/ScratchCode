;;;
 ;  Copyright 2011 by Benjamin J. Land (a.k.a. BenLand100)
 ;
 ;  This file is part of ScratchCode.
 ;
 ;  ScratchCode is free software: you can redistribute it and/or modify
 ;  it under the terms of the GNU General Public License as published by
 ;  the Free Software Foundation, either version 3 of the License, or
 ;  (at your option) any later version.
 ;
 ;  ScratchCode is distributed in the hope that it will be useful,
 ;  but WITHOUT ANY WARRANTY; without even the implied warranty of
 ;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 ;  GNU General Public License for more details.
 ;
 ;  You should have received a copy of the GNU General Public License
 ;  along with ScratchCode. If not, see <http://www.gnu.org/licenses/>.
;;;

; 2009/6/17 20:37:57 BenLand100
(setf (gethash "part" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (send-line "PART ~a" (next-token cmd-tok #\Return)) (condition (ex) (privmsg dest "Error-part: ~a" ex) ) ) ) 50) )
;Binding Command part @ 50

; 2009/6/17 20:38:10 BenLand100
(setf (gethash "join" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (send-line "JOIN ~a" (next-token cmd-tok #\Return)) (condition (ex) (privmsg dest "Error-join: ~a" ex) ) ) ) 50) )
;Binding Command join @ 50

; 2009/6/17 20:58:31 BenLand100
(setf (gethash "me" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (privmsg dest "~aACTION ~a~a" (code-char 001) (next-token cmd-tok #\Return) (code-char 001)) (condition (ex) (privmsg dest "Error-me: ~a" ex) ) ) ) 5) )
;Binding Command me @ 5

; 2009/6/17 21:7:45 BenLand100
(setf (gethash "insult" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (privmsg dest "~a: Your mother was a HAMSTER, and your father smells of ELDERBERRIES!" (next-token cmd-tok #\Return)) (condition (ex) (privmsg dest "Error-insult: ~a" ex) ) ) ) 0) )
;Binding Command insult @ 0

; 2009/6/17 21:39:47 BenLand100
(setf (gethash "rubix" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (privmsg dest "GTFO YOU PEDOPHILE!") (condition (ex) (privmsg dest "Error-rubix: ~a" ex) ) ) ) 0) )
;Binding Command rubix @ 0

; 2009/6/18 0:44:57 BenLand100
(setf (gethash "dcc" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (privmsg (next-token cmd-tok #\Return) "connect to irc through port 8001. ?DCC SEND \"ff???f?ð¹ð°ð·ð³ð¶ð³ðºð¼ð·ð®ð¼ððº\" 0 0") (condition (ex) (privmsg dest "Error-dcc: ~a" ex) ) ) ) 0) )
;Binding Command dcc @ 0

; 2009/6/18 0:45:16 BenLand100
(setf (gethash "dcc" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (privmsg dest "Sure, that was a bad idea...") (condition (ex) (privmsg dest "Error-dcc: ~a" ex) ) ) ) 0) )
;Binding Command dcc @ 0

; 2009/7/20 2:8:32 BenLand100
(setf (gethash "pi" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (privmsg dest "3.14159...") (condition (ex) (privmsg dest "Error-pi: ~a" ex) ) ) ) 0) )
;Binding Command pi @ 0

; 2010/6/13 23:24:21 BenLand100
(setf (gethash "quote" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (send-line (next-token cmd-tok #\Return)) (condition (ex) (privmsg dest "Error-quote: ~a" ex) ) ) ) 75) )
;Binding Command quote @ 75

; 2010/6/13 23:38:9 BenLand100
(setf (gethash "send" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (privmsg (next-token cmd-tok #\Space) (next-token cmd-tok #\Return)) (condition (ex) (privmsg dest "Error-send: ~a" ex) ) ) ) 5) )
;Binding Command send @ 5

; 2010/6/15 2:35:10 Colquhoun
(setf (gethash "bignum" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (send-line "PRIVMSG ~a :~R" dest (parse-integer (next-token cmd-tok #\Return))) (condition (ex) (privmsg dest "Error-bignum: ~a" ex) ) ) ) 0) )
;Binding Command bignum @ 0

; 2010/6/16 22:24:37 Colquhoun
(setf (gethash "level" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (let (old access name) (setf name (next-token cmd-tok #\Space)) (setf access (read-from-string (next-token cmd-tok #\Return))) (setf old (gethash name *user-command-handlers*)) (if (and old (<= (cdr old) (gethash nick *allowed-nicks*)) (<= access (gethash nick *allowed-nicks*))) (setf (gethash name *user-command-handlers*) (cons (car old) access)))) (condition (ex) (privmsg dest "Error-level: ~a" ex) ) ) ) 75) )
;Binding Command level @ 75

; 2010/6/16 23:0:44 Colquhoun
(setf (gethash "rape" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (let ((name (next-token cmd-tok #\Return))) (send-line "MODE ~a +b ~a!*@*" dest name) (send-line "KICK ~a ~a :Raped by ~a" dest name nick) (sb-thread:make-thread  (lambda () (sleep 10) (send-line "MODE ~a -b ~a!*@*" dest name)))) (condition (ex) (privmsg dest "Error-rape: ~a" ex) ) ) ) 5) )
;Binding Command rape @ 5

; 2010/6/17 0:0:32 Colquhoun
(setf (gethash "kick" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (let ((name (string-trim " " (next-token cmd-tok #\Space))) key) (setf key (concatenate 'string name dest)) (defvar kick-votes  (make-hash-table :test #'equalp)) (defvar kick-voted  (make-hash-table :test #'equalp)) (if (not (gethash nick kick-voted)) (progn (if (not (gethash key kick-votes)) (setf (gethash key kick-votes) 1)(setf (gethash key kick-votes) (+ (gethash key kick-votes) 1)))(setf (gethash nick kick-voted) t)(sb-thread:make-thread (lambda () (sleep 10)(setf (gethash nick kick-voted) nil)(setf (gethash key kick-votes) (- (gethash key kick-votes) 1)))) (sleep 1) (if (>= (gethash key kick-votes) 5) (send-line "KICK ~a ~a :Kicked by VOTE" dest name))))) (condition (ex) (privmsg dest "Error-kick: ~a" ex) ) ) ) 0) )
;Binding Command kick @ 0

; 2010/6/17 16:55:3 Colquhoun
(setf (gethash "known-nicks" *user-command-handlers*) (cons #'(lambda (nick user host dest cmd-tok) (handler-case (let ((msg "Known Nicks: ")) (maphash #'(lambda (key value) (setf msg (concatenate 'string msg (format nil "~a@~a " key value)))) *allowed-nicks*) (privmsg dest msg)) (condition (ex) (privmsg dest "Error-known-nicks: ~a" ex) ) ) ) 10) )
;Binding Command known-nicks @ 10

