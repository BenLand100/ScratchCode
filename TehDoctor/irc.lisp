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

(require :sb-bsd-sockets)
(defpackage irc-bot (:use :cl :sb-bsd-sockets))
(in-package irc-bot)

(defvar *allowed-nicks* (make-hash-table :test #'equalp))
(setf (gethash "BenLand100" *allowed-nicks*) 100)

(defvar *host* "irc.rizon.net")
(defvar *port* 6667)
(defvar *real* "The One and Only Doctor")
(defvar *nick* "TehDoctor")
(defvar *user* "sbcl")
(defvar *channel* "#thedoctor")
(defvar *nickserv-pass* "")
(defvar *evallog* "evals.lisp")
(defvar *bindlog* "bindings.lisp")
(defvar *hooklog* "hooks.lisp")

(defvar *irc-fd*)
(defvar *irc-stream*)
(defvar *irc-command-handlers*)
(defvar *user-command-handlers*)
(defvar *message-hooks* '())
(defvar *threads* (make-hash-table :test #'equalp))
(defvar *silent* nil)
        
(defun handle-quit (nick user host dest cmd-tok) 
    (declare (ignore nick user host dest cmd-tok))
    (send-line "QUIT :success")
    (socket-close *irc-fd*))

(defun handle-say (nick user host dest cmd-tok) 
    (declare (ignore nick user host))
    (privmsg dest (buffer cmd-tok)))

(defun handle-define (nick user host dest cmd-tok) 
    (declare (ignore nick user host))
    (privmsg dest (get-def (remove #\" (next-token cmd-tok #\Return)))))

(defun handle-kill (nick user host dest cmd-tok)
    (declare (ignore nick user host))
    (let ( (thread (gethash (next-token cmd-tok #\Return) *threads*)) )
        (if (and thread (sb-thread:thread-alive-p thread)) 
            (progn 
                (sb-thread:terminate-thread thread) 
                (privmsg dest "I smote the thread upon the mountainside!"))
            (privmsg dest "I smote the thread upon the mountainside!"))))

(defun handle-help (nick user host dest cmd-tok) 
    (declare (ignore nick user host cmd-tok))
    (let ((message "Commands: "))
        (maphash #'(lambda (key val) (setf message (concatenate 'string message (format nil "~a@~a; " key (cdr val))))) *user-command-handlers*)
        (privmsg dest message)))

(defun handle-silent (nick user host dest cmd-tok) 
    (declare (ignore nick user host))
    (setf *silent* (eval (read-from-string (next-token cmd-tok #\Return))))
    (if *silent* (privmsg dest "Fine, I won't talk anymore") (privmsg dest "Hey, I can talk again!")))

(defun handle-rem (nick user host dest cmd-tok) 
    (declare (ignore user host))
    (let (his-nick his-access my-access)
        (setf his-nick (next-token cmd-tok #\Return))
        (setf his-access (gethash his-nick *allowed-nicks*))
        (setf my-access (gethash nick *allowed-nicks*))
        (if his-access 
            (if (>= my-access his-access) 
                (progn
                    (remhash his-nick *allowed-nicks*)
                    (privmsg dest "Stripped this sucker of access: ~a" his-nick)))
            (remhash his-nick *allowed-nicks*))))

(defun handle-add (nick user host dest cmd-tok) 
    (declare (ignore user host))
    (let (his-nick his-access my-access)
        (setf his-nick (next-token cmd-tok #\Space))
        (setf his-access (read-from-string (next-token cmd-tok #\Return)))
        (setf my-access (gethash nick *allowed-nicks*))
        (if (>= my-access his-access)
            (progn
                (setf (gethash his-nick *allowed-nicks*) his-access)
                (if (< his-access 0) 
                    (privmsg dest "I'll just pretend this guy doesn't exist: ~a @ ~a" his-nick his-access)
                    (privmsg dest "Gave this new recruite an access level: ~a @ ~a" his-nick his-access))))))

(defun handle-access (nick user host dest cmd-tok) 
    (declare (ignore nick user host cmd-tok))
    (let ((message "AccessList: "))
        (maphash #'(lambda (key val) (setf message (concatenate 'string message (format nil "~a@~a; " key val)))) *allowed-nicks*)
        (privmsg dest message)))
   
(defun handle-hook (nick user host dest cmd-tok)
    (declare (ignore user host))
    (let ( ex-str hook )
        (with-open-file (fd *hooklog* :direction :output :if-exists :append :if-does-not-exist :create)
            (multiple-value-bind (sec min hour date month year) 
                (get-decoded-time) 
                (format fd "; ~a/~a/~a ~a:~a:~a ~a~%" year month date hour min sec nick))
            (setf ex-str (next-token cmd-tok #\Return))
            (setf hook (format nil "#'(lambda (nick user host dest cmd-tok) (handler-case ~a (condition (ex) (format t \"Error-Hook: ~~a\" ex) ) ) )" ex-str))
            (format t "~a~%" hook)
            (handler-case 
                (handler-bind 
                    ((condition #'(lambda (ex) (declare (ignore ex)) (let ((muffle (find-restart 'MUFFLE-WARNING nil))) (if muffle (invoke-restart muffle))))))
                    (setf *message-hooks* (append *message-hooks* (cons (eval (progn (in-package irc-bot) (read-from-string hook) )) nil)))
                    (format fd "(setf *message-hooks* (append *message-hooks* (cons ~a nil)))~%" hook)
                    (format fd ";Adding Hook...")
                    (privmsg dest "Hook Activated"))
                (condition (ex) 
                    (privmsg dest "Error-Hook: ~A" ex)
                    (format fd "#|~%~a~%|#~%~%" ex))))))


(defun handle-bind (nick user host dest cmd-tok)
    (declare (ignore user host))
    (let ( old to-bind access ex-str binding )
        (with-open-file (fd *bindlog* :direction :output :if-exists :append :if-does-not-exist :create)
            (multiple-value-bind (sec min hour date month year) 
                (get-decoded-time) 
                (format fd "; ~a/~a/~a ~a:~a:~a ~a~%" year month date hour min sec nick))
            (setf to-bind (next-token cmd-tok #\Space))
            (setf access (next-token cmd-tok #\Space))
            (setf old (gethash to-bind *user-command-handlers*))
            (let ( (cmd-access (read-from-string access)) (user-access (gethash nick *allowed-nicks*)) )
                (if (or (and old (<= (cdr old) user-access) (<= cmd-access user-access)) (<= cmd-access user-access))
                    (progn
                        (setf ex-str (next-token cmd-tok #\Return))
                        (setf binding (format nil "(cons #'(lambda (nick user host dest cmd-tok) (handler-case ~a (condition (ex) (privmsg dest \"Error-~a: ~~a\" ex) ) ) ) ~a)" ex-str to-bind access))
                        (format t "~a~%" binding)
                        (handler-case 
                            (handler-bind 
                                ((condition #'(lambda (ex) (declare (ignore ex)) (let ((muffle (find-restart 'MUFFLE-WARNING nil))) (if muffle (invoke-restart muffle))))))
                                (setf (gethash to-bind *user-command-handlers*) (eval (progn (in-package irc-bot) (read-from-string binding))) )
                                (format fd "(setf (gethash \"~a\" *user-command-handlers*) ~a )~%" to-bind binding)
                                (format fd ";Binding Command ~a @ ~a~%~%" to-bind access)
                                (privmsg dest "Giving this word a list to process: ~a @ ~a" to-bind access))
                            (condition (ex) 
                                (privmsg dest "Error-Bind: ~A" ex)
                                (format fd "#|~%~a~%|#~%~%" ex)))))))))

(defun handle-eval (nick user host dest cmd-tok) 
    (declare (ignore user host))
    (let ((ex-str (buffer cmd-tok)) result)
        (with-open-file (fd *evallog* :direction :output :if-exists :append :if-does-not-exist :create)
            (multiple-value-bind (sec min hour date month year) 
                (get-decoded-time) 
                (format fd "; ~a/~a/~a ~a:~a:~a ~a~%" year month date hour min sec nick))
            (handler-case
                (handler-bind 
                    ((condition #'(lambda (ex) (declare (ignore ex)) (let ((muffle (find-restart 'MUFFLE-WARNING nil))) (if muffle (invoke-restart muffle))))))
                    (setf result (eval (read-from-string ex-str)))
                    (format fd "~a~%~%" ex-str)
                    (privmsg dest "=> ~A" result))
                (condition (ex) 
                    (privmsg dest "Error-Eval: ~A" ex)
                    (format fd "#|~%~a~%|#~%~%" ex))))))

(defun do-command (nick user host dest message)
    (let ((access (gethash nick *allowed-nicks*)) (cmd-tok (make-instance 'string-tokenizer :buffer message)) cmd handle )
        (setf cmd (next-token cmd-tok #\Space))
        (if (not cmd) (setf cmd (next-token cmd-tok #\Return)))
        (setf handle (gethash cmd *user-command-handlers*))
        (if (not access) (setf access 0))
        (handler-case 
            (let ( (thread (gethash nick *threads*)) )
                (if (and thread (sb-thread:thread-alive-p thread)) (sb-thread:terminate-thread thread) )
                (if (and handle (>= access (cdr handle))) 
                    (setf (gethash nick *threads*) (sb-thread:make-thread 
                        (lambda () 
                            (handler-case 
                                (funcall (car handle) nick user host dest cmd-tok) 
                                (condition (ex) (privmsg dest "Error-Command: ~A" ex)))) 
                        :name nick))))
            (condition (ex) (privmsg dest "Error-Command: ~A" ex)))))

(defmacro with-irc-source ((source nick user host) &body body)
    (let ( (sourcetok-name (gensym)) )
        `(let ( (,sourcetok-name (make-instance 'string-tokenizer :buffer ,source)) ,nick ,user ,host )
            (setf ,nick (subseq (next-token ,sourcetok-name #\!) 1))
            (setf ,user (next-token ,sourcetok-name #\@))
            (setf ,host (buffer ,sourcetok-name))
            ,@body)))

(defun on-privmsg (linetok source)
    (let (dest message)
        (setf dest (next-token linetok #\Space))
        (setf message (subseq (buffer linetok) 1))
        (with-irc-source (source nick user host)
            (if dest (if (or *silent* (equalp dest *nick*)) (setf dest nick)))
            (if (char= #\! (char message 0))
                (do-command nick user host dest (subseq message 1)))
            (dolist (cell *message-hooks*)
                (handler-case 
                    (funcall cell nick user host dest (make-instance 'string-tokenizer :buffer message))
                    (condition (ex) (format t "~a" ex)))))))

(defun on-start (linetok source)
    (declare (ignore linetok source))
    (send-line "PRIVMSG NickServ :identify ~A" *nickserv-pass*)
    (send-line "JOIN ~A" *channel*))
  
(defun on-need-ghost (linetok source)
    (declare (ignore linetok source))
    (privmsg "nickserv" "ghost ~a ~a" *nick* *nickserv-pass*)
    (send-line "NICK ~A" *nick*))
  
(defun on-ping (linetok source) 
    (declare (ignore source))
    (send-line "PONG ~a" (buffer linetok)))

(defun on-kick (linetok source)
    (declare (ignore source))
    (send-line "JOIN ~a" (next-token linetok #\Space)))

(defclass string-tokenizer () (
    (buffer :initarg :buffer :accessor buffer :initform (error "must specify string."))
    (pos :initarg :pos :accessor pos :initform 0)))

(defgeneric next-token (tokenizer char))

(defmethod next-token ((tokenizer string-tokenizer) char)
    (let (i result)
        (setf i (position char (buffer tokenizer) :start (pos tokenizer)))
        (if i
            (progn
                (setf result (subseq (buffer tokenizer) (pos tokenizer) i))
                (setf (buffer tokenizer) (subseq (buffer tokenizer) (+ 1 i))))
            (setf result nil))
        result))

(defun send-line (fstr &rest args)
    (let ( (str (format nil "~A~%" (apply #'format nil fstr args))) )
        (format t "=> ~A" str)
        (format *irc-stream* str))
    (force-output *irc-stream*))

(defun privmsg (dest fstr &rest args)
    (send-line (format nil "PRIVMSG ~A :~A" dest (apply #'format nil fstr args))))

(defun startup-irc (host port)
    (let (addr)
        (setf *irc-fd* (make-instance 'inet-socket :type :stream :protocol :tcp))
        (setf addr (car (host-ent-addresses (get-host-by-name host))))
        (format t "connecting to ~a~%" addr)
        (socket-connect *irc-fd* addr port)
        (setf *irc-stream* (socket-make-stream *irc-fd* :output t :input t))
        (send-line "USER ~A 6 6 :~A" *user* *real*)
        (send-line "NICK ~A" *nick*)))

(defun handle-line (line)
    (format t "<= ~a~%" line)
    (let ( (linetok (make-instance 'string-tokenizer :buffer line)) source command handler)
        (if (char= #\: (char line 0))
            (setf source (next-token linetok #\Space)))
        (setf command (next-token linetok #\Space))
        (setf handler (gethash command *irc-command-handlers*))
        (if handler (funcall handler linetok source))
        nil))

(defun attempt-resync (c)
    (format t "stream decoding error: ~A~%" c)
    (invoke-restart 'sb-int:attempt-resync))

(defun get-next-line ()
    (handler-bind ((sb-int:stream-decoding-error #'attempt-resync))
        (read-line *irc-stream*)))

(defun get-page (host &optional (uri "/"))
    (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
        (socket-connect socket (car (host-ent-addresses (get-host-by-name host))) 80)
        (let ( (stream (socket-make-stream socket :output t :input t)) (line t) (page "") )
            (format stream "GET ~a HTTP/1.0~a~a~a~a" uri (code-char 13) (code-char 10) (code-char 13) (code-char 10))
            (force-output stream)
            (loop while line do (progn (setf line (read-line stream nil)) (setf page (concatenate 'string page line)) ) )
            page)))

(defun get-def (word) 
    (let ((page (get-page "wordnetweb.princeton.edu" (format nil "/perl/webwn?s=~a" word))))
        (if (search "Your search did not return any results." page) 
            (format nil "~a: definition not found" word) 
            (progn 
                (setf page (subseq page (+ 6 (search "S:</a>" page)) (search "</i></li>" page)))
                (let ((removals (count #\< page)))
                    (if (> removals 0) (dotimes (i removals) (setf page (concatenate 'string (subseq page 0 (position #\< page)) (subseq page (+ 1 (position #\> page))))) ) )
                    (concatenate 'string (format nil "~a:" word) page))))))

(defun irc-bot ()
    (setf *irc-command-handlers* (make-hash-table :test #'equalp))
    
    (setf (gethash "privmsg" *irc-command-handlers*) #'on-privmsg)
    (setf (gethash "376" *irc-command-handlers*) #'on-start)
    (setf (gethash "422" *irc-command-handlers*) #'on-start)
    (setf (gethash "ping" *irc-command-handlers*) #'on-ping)
    (setf (gethash "kick" *irc-command-handlers*) #'on-kick)

    (setf *user-command-handlers* (make-hash-table :test #'equalp))
    
    (load "bindings.lisp")
    (load "hooks.lisp")
    
    (setf (gethash "quit" *user-command-handlers*) (cons #'handle-quit 100))
    (setf (gethash "access" *user-command-handlers*) (cons #'handle-access 100))
    (setf (gethash "eval" *user-command-handlers*) (cons #'handle-eval 75))
    (setf (gethash "bind" *user-command-handlers*) (cons #'handle-bind 75))
    (setf (gethash "hook" *user-command-handlers*) (cons #'handle-hook 75))
    (setf (gethash "kill" *user-command-handlers*) (cons #'handle-kill 75))
    (setf (gethash "add" *user-command-handlers*) (cons #'handle-add 10))
    (setf (gethash "rem" *user-command-handlers*) (cons #'handle-rem 10))
    (setf (gethash "silent" *user-command-handlers*) (cons #'handle-silent 5))
    (setf (gethash "say" *user-command-handlers*) (cons #'handle-say 5))
    (setf (gethash "define" *user-command-handlers*) (cons #'handle-define 0))
    (setf (gethash "help" *user-command-handlers*) (cons #'handle-help 5))

    (startup-irc *host* *port*)
    (handler-case
        (let ((r nil))
            (loop until r do
                (setf r (handle-line (get-next-line)))))
        (condition (ex) (format t "Error: ~a~%" ex) )))

(irc-bot)
