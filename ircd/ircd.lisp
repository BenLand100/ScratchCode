#|
 *  Copyright 2011 by Benjamin J. Land (a.k.a. BenLand100)
 *
 *  This file is part of ScratchCode.
 *
 *  ScratchCode is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  ScratchCode is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with ScratchCode. If not, see <http://www.gnu.org/licenses/>.
 |#
 
(require :sb-bsd-sockets)
(defpackage ircd (:use :cl :sb-bsd-sockets))

(in-package ircd)

(defvar *server-name* "IRCd in Lisp v1.0")
(defvar *server-host* "bland.no-ip.org")
(defvar *server-addr* '(192 168 0 100))
(defvar *server-port* 6667)
(defvar *server-maxc* 100)

(defvar *irc-command-handlers*)

(defvar *users*)
(defvar *channels*)

(defclass irc-channel () 
    (
        (name :initarg :name :accessor name :initform (error "no name specified"))
        (topic :initarg :topic :accessor topic :initform nil)
        (users :initarg :users :accessor users :initform (make-hash-table :test #'equalp))
        (modes :initarg :modes :accessor modes :initform '())
    )
)

(defclass irc-user ()
    (
        (host :initarg :host :accessor host :initform (error "no host specified"))
        (socket :initarg :socket :accessor socket :initform nil)
        (name :initarg :name :accessor name :initform nil)
        (user :initarg :user :accessor user :initform nil)
        (nick :initarg :nick :accessor nick :initform nil)
        (channels :initarg :channels :accessor channels :initform (make-hash-table :test #'equalp))
        (modes :initarg :modes :accessor modes :initform '())
    )
)

(defun full-name (user)
    (format nil "~a!~a@~a" (nick user) (user user) (host user))
)

(defun check-mode (mode-list what-mode)
    (member t (mapcar #'(lambda (mode-args) 
        (char= (car mode-args) what-mode)
    ) mode-list))
)

(defun add-mode (mode-list what-mode)
    (if (check-mode mode-list what-mode)
        mode-list
        (cons (list what-mode) mode-list)
    )
)

(defun rem-mode (mode-list what-mode)
    (mapcan #'(lambda (mode-args) 
        (if (char= (car mode-args) what-mode)
            nil
            (list mode-args)
        )
    ) mode-list)  
)

(defun get-mode-args (mode-list what-mode)
    (mapcan #'(lambda (mode-args)
        (if (char= (car mode-args) what-mode) 
            (cdr mode-args)
            nil
        )
    ) mode-list)
)

(defun add-mode-arg (mode-list what-mode what-arg)
    (let ((mode-list (add-mode mode-list what-mode)))
        (mapcar #'(lambda (mode-args) 
            (if (char= (car mode-args) what-mode) 
                (append mode-args (list what-arg))
                mode-args
            )
        ) mode-list)    
    )
)

(defun rem-mode-arg (mode-list what-mode what-arg)
    (mapcar #'(lambda (mode-args) 
        (if (char= (car mode-args) what-mode) 
            (cons (car mode-args) (mapcan #'(lambda (arg)
                (if (equalp what-arg arg)
                    nil
                    (list arg)
                )
            ) (cdr mode-args)))
            mode-args
        )
    ) mode-list)
)

(defun is-chanop (nick chan) 
    (member t (mapcar #'(lambda (arg)
        (equalp arg nick)
    ) (get-mode-args (modes chan) #\o)))
)

(defun set-chanop (nick chan needs-chanop)
    (cond
        ((and needs-chanop (not (is-chanop nick chan))) (setf (modes chan) (add-mode-arg (modes chan) #\o nick)))
        ((not needs-chanop) (setf (modes chan) (rem-mode-arg (modes chan) #\o nick)))
    )        
)

(defun set-mode (chan-or-user mode mode-set)
    (cond
        ((and mode-set (not (check-mode (modes chan-or-user) mode))) (setf (modes chan-or-user) (add-mode (modes chan-or-user) mode)))
        ((not mode-set) (setf (modes chan-or-user) (rem-mode (modes chan-or-user) mode)))
    ) 
)

(defclass string-tokenizer ()
    ( 
        (buffer :initarg :buffer :accessor buffer :initform (error "must specify string."))
        (pos :initarg :pos :accessor pos :initform 0)
    )
)

(defgeneric next-token (tokenizer char))

(defmethod next-token ((tokenizer string-tokenizer) char)
    (let ( (i (position char (buffer tokenizer) :start (pos tokenizer)) ) )
        (if i
            (let ( (result (subseq (buffer tokenizer) (pos tokenizer) i) ) )
                (setf (buffer tokenizer) (subseq (buffer tokenizer) (+ 1 i)))
                result
            )
            nil
        )
    )
)

(defun send-channel (channel fstr &rest args)
    (let ((message (apply #'format nil fstr args)))
        (maphash #'(lambda (nick user) (send-user user message)) (users channel))
    )
)

(defun send-channel-exclude (channel excluded-user fstr &rest args)
    (let ((message (apply #'format nil fstr args)))
        (maphash #'(lambda (nick user) (if (not (equalp excluded-user user)) (send-user user message))) (users channel))
    )
)

(defun send-user (user fstr &rest args)
    (format t "<=~a~%" (apply #'format nil fstr args))
    (socket-send (socket user) (format nil "~a~a~a" (apply #'format nil fstr args) #\Return #\Newline) nil)
)

(defun new-user-added (user) 
    (send-user user ":~a 001 ~a :Welcome to IRCd in Lisp! ~a" *server-host* (nick user) (full-name user))
    (send-user user ":~a 002 ~a :Your host is ~a, running version ~a" *server-host* (nick user) *server-host* *server-name*)
    (send-user user ":~a 003 ~a :This server was created ~a" *server-host* (nick user) "at some point...")
    (send-user user ":~a 004 ~a :~a ~a ~a ~a" *server-host* (nick user) *server-host* *server-name* "no-user-modes" "t")
    (send-user user ":~a 375 ~a :- ~a Message of the day -" *server-host* (nick user) *server-host*)
    (send-user user ":~a 372 ~a :- Running IRCd in LISP!" *server-host* (nick user))
    (send-user user ":~a 376 ~a :End of /MOTD command" *server-host* (nick user))
)

(defmacro with-channel-name-except ((user channel-name channel-object) is-channel-form not-channel-form)
   `(case (char ,channel-name 0)
        ((#\#)
            (let ((,channel-object (gethash (subseq ,channel-name 1) *channels*)))
                ,is-channel-form
            )
        )
        (otherwise
            ,not-channel-form
        )
    )
)

(defmacro with-channel-name ((user channel-name channel-object) &body forms)
   `(with-channel-name-except (,user ,channel-name ,channel-object) 
       (progn ,@forms)
       (send-user ,user ":~a 403 ~a ~a :No such channel" *server-host* (nick ,user) ,channel-name)
   )
)

(defmacro with-args ((user command arg-list num-args &body conditions) &body forms)
   `(if (and ,@conditions (>= (list-length ,arg-list) ,num-args))
        (progn ,@forms)
        (send-user ,user ":~a 461 ~a ~a :Not enough params" *server-host* ,command (nick ,user))
    )
)
        

(defun join-channel (user channel-name)
    (with-channel-name (user channel-name channel)
        (if (not (and channel (gethash (nick user) (users channel)))) 
            (progn
                (if channel
                    (progn
                        (setf (gethash (nick user) (users channel)) user)
                        (setf (gethash (name channel) (channels user)) channel)
                    )
                    (progn
                        (setf (gethash (subseq channel-name 1) *channels*) (setf channel (make-instance 'irc-channel :name (subseq channel-name 1))))
                        (setf (gethash (nick user) (users channel)) user)
                        (setf (gethash (name channel) (channels user)) channel)
                        (set-chanop (nick user) channel t)
                    )
                )
                (send-channel channel ":~a JOIN ~a" (full-name user) channel-name)
                (if (topic channel)
                    (send-user user ":~a 332 ~a ~a :~a" *server-host* (nick user) channel-name (topic channel))
                    (send-user user ":~a 331 ~a ~a :No topic set" *server-host* (nick user) channel-name )
                )
                (let ((names ""))
                    (maphash #'(lambda (chan-nick chan-user) 
                        (setf names (concatenate 'string names (format nil "~a~a " (if (is-chanop (nick chan-user) channel) "@" "") (nick chan-user))))
                    ) (users channel))
                    (send-user user ":~a 353 ~a ~a ~a :~a" *server-host* (nick user) "=" channel-name names)
                    (send-user user ":~a 366 ~a ~a :End of /NAMES list" *server-host* (nick user) channel-name)
                )
            )
        )
    )
)

(defun part-channel (user channel-name) 
    (with-channel-name (user channel-name channel)
        (if (and channel (gethash (nick user) (users channel)))
            (progn
                (set-chanop (nick user) channel nil)
                (send-channel channel ":~a PART ~a" (full-name user) channel-name) 
                (remhash (nick user) (users channel))
                (remhash (name channel) (channels user))
                (if (= 0 (hash-table-count (users channel))) (remhash (name channel) *channels*)) 
            )
            (send-user user ":~a 442 ~a ~a :You're not on that channel" *server-host* (nick user) channel-name)  
        )
    )
)

(defun handle-user (user args)
    (with-args (user 'USER args 4 (not (or (name user) (user user))))
        (setf (user user) (nth 0 args))
        (setf (name user) (nth 3 args))
        (if (nick user) (new-user-added user))
    )
)

(defun handle-nick (user args)
    (with-args (user 'NICK args 1)
        (if (gethash (nth 0 args) *users*)
            (send-user user ":~a 433 ~a ~a :Nickname already in use" *server-host* (if (nick user) (nick user) "*") (nth 0 args))
            (if (nick user)
                (let ((new-nick (nth 0 args)) (old-nick (nick user)))
                    (setf (nick user) new-nick)
                    (setf (gethash new-nick *users*) user)
                    (maphash #'(lambda (name channel) 
                        (set-chanop new-nick channel (is-chanop old-nick channel))
                        (set-chanop old-nick channel nil)
                        (send-channel-exclude channel user ":~a NICK ~a" old-nick new-nick)
                    ) (channels user))
                    (send-user user ":~a NICK ~a" old-nick new-nick)
                    (if (or (name user) (user user)) 
                        (progn
                            (remhash old-nick *users*) 
                            (new-user-added user)
                        )
                    )
                )
                (let ((new-nick (nth 0 args)))
                    (setf (nick user) new-nick)
                    (setf (gethash new-nick *users*) user)
                    (send-user user ":* NICK ~a" new-nick)
                    (if (or (name user) (user user)) (new-user-added user))
                )
            )
        )
    )
)

(defun handle-join (user args) 
    (with-args (user 'JOIN args 1)
        (join-channel user (car args))
    )
)

(defun handle-part (user args)
    (with-args (user 'PART args 1)
        (part-channel user (car args))
    )
)

(defun handle-kick (user args)
    (with-args (user 'KICK args 2)
        (let ((target-channel-name (car args)) (kicked-user-nick (cadr args)) (message (caddr args)))
            (with-channel-name (user target-channel-name target-chan)
                (if target-chan
                    (if (is-chanop (nick user) target-chan)
                        (let ((kicked-user (gethash kicked-user-nick (users target-chan))))
                            (if kicked-user
                                (progn
                                    (set-chanop (nick kicked-user) target-chan nil)
                                    (send-channel target-chan ":~a KICK #~a ~a :~a" (full-name user) (name target-chan) (nick kicked-user) (if message message "")) 
                                    (remhash (nick kicked-user) (users target-chan))
                                    (remhash (name target-chan) (channels kicked-user))
                                    (if (= 0 (hash-table-count (users target-chan))) (remhash (name target-chan) *channels*)) 
                                )
                                (send-user user ":~a 441 ~a ~a ~a :User not in channel" *server-host* (nick user) kicked-user-nick (name target-chan))  
                            )
                        )
                        (send-user user ":~a 482 ~a ~a :You are not a channel operator" *server-host* (nick user) (name target-chan))
                    )
                    (send-user user ":~a 403 ~a ~a :No such channel" *server-host* (nick user) target-channel-name)
                )
            )
        )
    )
)

(defun handle-notice (user args)
    (with-args (user 'NOTICE args 2)
        (let ((target (car args)) (message (cadr args)))
            (with-channel-name-except (user target target-chan)
                (if target-chan
                    (send-channel-exclude target-chan user ":~a NOTICE ~a :~a" (full-name user) target message)
                    (send-user user ":~a 411 ~a :No such recipent" *server-host* (nick user))
                )
                (let ((target-user (gethash target *users*)))
                    (if target-user
                        (send-user target-user ":~a NOTICE ~a :~a" (full-name user) target message)
                        (send-user user ":~a 411 ~a :No such recipent" *server-host* (nick user))
                    )
                )
            )
        )
    )
)

(defun handle-privmsg (user args)
    (with-args (user 'PRIVMSG args 2)
        (let ((target (nth 0 args)) (message (nth 1 args)))
            (with-channel-name-except (user target target-chan)
                (if target-chan
                    (send-channel-exclude target-chan user ":~a PRIVMSG ~a :~a" (full-name user) target message)
                    (send-user user ":~a 411 ~a :No such recipent" *server-host* (nick user))
                )
                (let ((target-user (gethash target *users*)))
                    (if target-user
                        (send-user target-user ":~a PRIVMSG ~a :~a" (full-name user) target message)
                        (send-user user ":~a 411 ~a :No such recipent" *server-host* (nick user))
                    )
                )
            )
        )
    )
)

(defun handle-topic (user args)
    (cond 
        ((>= (list-length args) 2)
            (let ((target (car args)))
                (with-channel-name (user target chan)
                    (if chan
                        (if (gethash (nick user) (users chan))
                            (if (or (not (check-mode (modes chan) #\t)) (is-chanop (nick user) chan))
                                (let ((topic (nth 1 args)))
                                    (setf (topic chan) topic)
                                    (send-channel chan ":~a TOPIC ~a :~a" (full-name user) target topic)
                                )
                                (send-user user ":~a 482 ~a ~a :You are not a channel operator" *server-host* (nick user) target)
                            )
                            (send-user user ":~a 442 ~a ~a :You're not on that channel" *server-host* (nick user) target)
                        )
                        (send-user user ":~a 403 ~a ~a :No such channel" *server-host* (nick user) target)
                    )
                )
            )
        )
        ((= (list-length args) 1)
            (let ((target (car args)))
                (with-channel-name (user target chan)
                    (if chan
                        (if (gethash (nick user) (users chan))
                            (if (topic chan)
                                (send-user user ":~a 332 ~a ~a :~a" *server-host* (nick user) target (topic chan))
                                (send-user user ":~a 331 ~a ~a :No topic set" *server-host* (nick user) target)
                            )
                            (send-user user ":~a 442 ~a ~a :Not on channel" *server-host* (nick user) target)
                        )
                        (send-user user ":~a 403 ~a ~a :No such channel" *server-host* (nick user) target)
                    )
                )
            )
        )
        (t (send-user user ":~a 461 ~a TOPIC :Not enough params" *server-host* (nick user)))
    )
)
        

(defun handle-names (user args) 
    (with-args (user 'NAMES args 1)
        (mapcar #'(lambda (target)
            (with-channel-name (user target chan)
                (if chan
                    (let ((names ""))
                        (maphash #'(lambda (chan-nick chan-user) 
                            (setf names (concatenate 'string names (format nil "~a~a " (if (is-chanop (nick chan-user) chan) "@" "") (nick chan-user))))
                        ) (users chan)) 
                        (send-user user ":~a 353 ~a ~a ~a :~a" *server-host* (nick user) "=" target names)
                        (send-user user ":~a 366 ~a ~a :End of /NAMES list" *server-host* (nick user) target)
                    )
                    (send-user user ":~a 403 ~a ~a :No such channel" *server-host* (nick user) target)
                )
            )
        ) args)
    )
)
        

(defun handle-mode (user args)
    (with-args (user 'MODE args 1)
        (let ((target (car args)) (mode-list (cadr args)) (mode-args (cddr args)))
            (with-channel-name-except (user target target-chan)
                (if target-chan
                    (if mode-list
                        (if (is-chanop (nick user) target-chan)
                            (let (add-rem (arg-pos 0) (arg-count (list-length mode-args)))
                                (map 'string #'(lambda (char) 
                                    (cond
                                        ((or (char= #\+ char) (char= #\- char))
                                            (setf add-rem char)
                                        )
                                        (add-rem
                                            (if (char= add-rem #\+)
                                                (if (not (case char
                                                    ((#\o) 
                                                        (if (>= (- arg-count arg-pos) 1)
                                                            (let ((target-user (gethash (nth arg-pos mode-args) *users*)))
                                                                (if target-user
                                                                    (progn
                                                                        (set-chanop (nick target-user) target-chan t)
                                                                        (setf arg-pos (+ 1 arg-pos))
                                                                        (send-channel target-chan ":~a MODE #~a +o ~a" (full-name user) (name target-chan) (nick target-user))
                                                                    )
                                                                    (send-user user ":~a 401 ~a ~a :No such nick" *server-host* (nick user) (nth arg-count mode-args))
                                                                )
                                                            )
                                                            (send-user user ":~a 461 ~a MODE :Not enough params" *server-host* (nick user))
                                                        )
                                                        t
                                                    )
                                                    ((#\t) 
                                                        (set-mode target-chan #\t t) 
                                                        (send-channel target-chan ":~a MODE #~a +t" (full-name user) (name target-chan))
                                                        t
                                                    )
                                                )) (send-user user ":~a 472 ~a ~a :Mode not recongnized" *server-host* (nick user) char))
                                                (if (not (case char
                                                    ((#\o) 
                                                        (if (>= (- arg-count arg-pos) 1)
                                                            (let ((target-user (gethash (nth arg-pos mode-args) *users*)))
                                                                (if target-user
                                                                    (progn
                                                                        (set-chanop (nick target-user) target-chan nil)
                                                                        (setf arg-pos (+ 1 arg-pos))
                                                                        (send-channel target-chan ":~a MODE #~a -o ~a" (full-name user) (name target-chan) (nick target-user))
                                                                    )
                                                                    (send-user user ":~a 401 ~a ~a :No such nick" *server-host* (nick user) (nth arg-count mode-args))
                                                                )
                                                            )
                                                            (send-user user ":~a 461 ~a MODE :Not enough params" *server-host* (nick user))
                                                        )
                                                        t
                                                    )
                                                    ((#\t) 
                                                        (set-mode target-chan #\t nil) 
                                                        (send-channel target-chan ":~a MODE #~a -t" (full-name user) (name target-chan))
                                                        t
                                                    )
                                                )) (send-user user ":~a 472 ~a ~a :Mode not recongnized" *server-host* (nick user) char))
                                            )
                                        ) 
                                        (t (send-user user ":~a 472 ~a ~a :Mode state undefined" *server-host* (nick user) char))
                                    )
                                    char ;collects the chars again
                                ) mode-list)
                            )
                            (send-user user ":~a 482 ~a ~a :You are not a channel operator" *server-host* (nick user) target)
                        )
                        (let ((set-modes "+"))
                            (mapcar #'(lambda (mode) 
                                (setf set-modes (concatenate 'string set-modes (format nil "~a" (car mode))))
                            ) (modes target-chan))
                            (send-user user ":~a 324 ~a ~a ~a" *server-host* (nick user) target set-modes)
                        )
                    )
                )
                (let ((target-user (gethash target *users*)))
                    (if (and target-user (equalp (nick target-user) (nick user)))
                        (if mode-list
                            (let (add-rem (arg-pos 0) (arg-count (list-length mode-args)))
                                (map 'string #'(lambda (char) 
                                    (cond
                                        ((or (char= #\+ char) (char= #\- char))
                                            (setf add-rem char)
                                        )
                                        (add-rem
                                            (if (char= add-rem #\+)
                                                (if (not (case char
                                                    
                                                )) (send-user user ":~a 472 ~a ~a :Mode not recongnized" *server-host* (nick user) char))
                                                (if (not (case char
                                                    
                                                )) (send-user user ":~a 472 ~a ~a :Mode not recongnized" *server-host* (nick user) char))
                                            )
                                        ) 
                                        (t (send-user user ":~a 472 ~a ~a :Mode state undefined" *server-host* (nick user) char))
                                    )
                                    char ;collects the chars again
                                ) mode-list)
                            )
                            (let ((set-modes "+"))
                                (mapcar #'(lambda (mode) 
                                    (setf set-modes (concatenate 'string set-modes (format nil "~a" (car mode))))
                                ) (modes target-user))
                                (send-user user ":~a 221 ~a ~a" *server-host* (nick user) set-modes)
                            )
                        )
                        (send-user user ":~a 502 ~a ~a :Users don't match" *server-host* (nick user) target)
                    )
                )
            )
        )
    )
)

(defun handle-who (user args) 
    (with-args (user 'WHO args 1)
        (let ((target (nth 0 args)))
            (with-channel-name (user target target-chan)
                (if target-chan
                    (progn
                        (maphash #'(lambda (nick chan-user) 
                            (send-user user ":~a 352 ~a ~a ~a ~a ~a ~a ~a~a :~a ~a" 
                                *server-host* 
                                (nick user) 
                                target 
                                (user chan-user) 
                                (host chan-user) 
                                *server-host* 
                                (nick chan-user) 
                                "H" 
                                (if (is-chanop (nick chan-user) target-chan) "@" "") 
                                0 
                                (name chan-user)
                            )
                        ) (users target-chan))
                        (send-user user ":~a 315 ~a :End of /WHO list" *server-host* (nick user))
                    )
                    (send-user user ":~a 403 ~a ~a :No such channel" *server-host* (nick user) target)
                )
            )
        )
    )
)

(defun handle-ping (user args)
    (with-args (user 'PING args 1)
        (send-user user ":~a PONG ~a" *server-host* (nth 0 args))
    )
)

(define-condition quit-condition (condition)
    ((message :initarg :message :accessor message :initform "Client Quit"))
)

(defun handle-quit (user args)
    (declare (ignore user))
    (if (>= (list-length args) 1)
        (error 'quit-condition :message (car args))
        (error 'quit-condition)
    )
)

(defun resolve-hostname (name) 
    (cond
        ((eql name :any)  #(0 0 0 0))
        ((typep name '(vector * 4)) name)
        (t (car (sb-bsd-sockets:host-ent-addresses (sb-bsd-sockets:get-host-by-name name))))
    )
)

(defun open-server (&key (host :any) (port 0) (reuse-address t) (backlog 1) (protocol :tcp))
    (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol protocol)))
        (when reuse-address
            (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
        )
        (sb-bsd-sockets:socket-bind  sock (resolve-hostname host) port)
        (sb-bsd-sockets:socket-listen sock backlog)
        sock
    )
)

(defmacro with-server ((name arguments) &body forms)
    `(let (,name)
        (unwind-protect
            (progn
                (setf ,name (open-server ,@arguments))
                ,@forms
            )
            (when ,name (sb-bsd-sockets:socket-close ,name))
        )
    )
)

(defconstant +buflen+ 16)	 

(defstruct server-session sock fd buffer handler command-buffer irc-user)

(defun data-received-handler (session)
    (let ( (buffer (server-session-buffer session)) (sock (server-session-sock session)) )
        (do ((fin nil)) (fin t)
            (setf (fill-pointer buffer) +buflen+)
            (handler-case
                (multiple-value-bind (buf len raddr) (sb-bsd-sockets:socket-receive sock buffer nil) (declare (ignore raddr))
                    (if (null buf)
                        (setf fin t)
                        (setf (fill-pointer buffer) len)
                    )
                )
                (condition (ex) 
                    (let ((irc-user (server-session-irc-user session)))
                        (maphash #'(lambda (name channel) 
                            (set-chanop (nick irc-user) channel nil)
                            (send-channel channel ":~a QUIT :~a" (nick irc-user) ex) 
                            (remhash (nick irc-user) (users channel))
                        ) (channels irc-user))
                        (remhash (nick irc-user) *users*)
                        (sb-bsd-sockets:socket-close sock)
                        (sb-sys:remove-fd-handler (server-session-handler session))
                        (setf fin t)
                    )
                )
            )
            (cond 
                ((= (length buffer) 0)
                    #|Closing Socket|#
                    (sb-bsd-sockets:socket-close sock)
                    (sb-sys:remove-fd-handler (server-session-handler session))
                    (setf fin t)
                )
                (fin #|Recieved NIL|# )
                (t 
                    (let (cutoff (return (position #\Return buffer)) (newline (position #\NewLine buffer)))
                        (if (or return newline)
                            (progn
                                (cond
                                    ((not newline) (setf cutoff return))
                                    ((not return) (setf cutoff newline))
                                    ((= (+ 1 return) newline) (setf cutoff newline))
                                    ((= (+ 1 newline) return) (setf cutoff return))
                                    ((> newline return) (setf cutoff return))
                                    ((> return newline) (setf cutoff newline))
                                )
                                (let ((irc-user (server-session-irc-user session)) (line (format nil "~a~a" (server-session-command-buffer session) (subseq buffer 0  (+ 1 cutoff)))))
                                    (setf (server-session-command-buffer session) (subseq buffer (+ 1 cutoff)))
                                    (if (> (length line) 1)
                                        (progn
                                            (if (not (position #\Newline line)) (setf line (concatenate 'string line (string #\Newline))))
                                            (format t "=>~a" line)
                                            (handler-case
                                                (let ((linetok (make-instance 'string-tokenizer :buffer line)) handler)
                                                    (setf handler (gethash (next-token linetok #\Space) *irc-command-handlers*))
                                                    (if handler 
                                                        (let ((args '()))
                                                            (do
                                                                ((next (next-token linetok #\Space) (next-token linetok #\Space)))
                                                                (
                                                                    (or (equal next nil) (char= #\: (char next 0)))
                                                                    (let ((rest (next-token linetok #\Return)))
                                                                        (if (not rest) (setf rest (next-token linetok #\Newline)))
                                                                        (if next 
                                                                            (setf args (cons (concatenate 'string (if (char= #\: (char next 0)) (subseq next 1) next) " " rest) args))
                                                                            (if (not (equalp rest "")) (setf args (cons (if (char= #\: (char rest 0)) (subseq rest 1) rest) args)))
                                                                        )
                                                                    )
                                                                )
                                                                (setf args (cons (string-trim " " next) args))
                                                            )  
                                                            (setf args (reverse args))                         
                                                            (funcall handler irc-user args)
                                                        )
                                                        (send-user irc-user "ERROR :Unknown command")
                                                    ) 
                                                )
                                                (quit-condition (quit)
                                                    (maphash #'(lambda (name channel) 
                                                        (set-chanop (nick irc-user) channel nil)
                                                        (send-channel channel ":~a QUIT :~a" (nick irc-user) (message quit))
                                                        (remhash (nick irc-user) (users channel))
                                                    ) (channels irc-user))
                                                    (remhash (nick irc-user) *users*)
                                                    (sb-bsd-sockets:socket-close sock)
                                                    (sb-sys:remove-fd-handler (server-session-handler session))
                                                    (setf fin t)
                                                )
                                                (condition (ex) 
                                                    (format t "Internal Error: ~a~%" ex)
                                                    (maphash #'(lambda (name channel) 
                                                        (set-chanop (nick irc-user) channel nil)
                                                        (send-channel channel ":~a QUIT :~a" (nick irc-user) "Connection terminated") 
                                                        (remhash (nick irc-user) (users channel))
                                                    ) (channels irc-user))
                                                    (remhash (nick irc-user) *users*)
                                                    (sb-bsd-sockets:socket-close sock)
                                                    (sb-sys:remove-fd-handler (server-session-handler session))
                                                    (setf fin t)
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            (setf (server-session-command-buffer session) (concatenate 'string (server-session-command-buffer session) buffer))  
                        )
                    )
                )
            )
        )
    )
)

(defun accept-handler (socket)
    (let ((conn (sb-bsd-sockets:socket-accept socket)))
        (let ((fd (sb-bsd-sockets:socket-file-descriptor conn)))
            (let 
                (
                    (session (make-server-session 
                        :sock conn 
                        :fd fd
                        :buffer (make-array +buflen+ :element-type 'character :adjustable nil :fill-pointer t)
                        :command-buffer ""
                        :irc-user (make-instance 'irc-user :socket conn :host (host-ent-name (get-host-by-address (socket-name conn))))
                    ))
                )
                (let ((handler (sb-sys:add-fd-handler fd :input #'(lambda (fd) (declare (ignore fd)) (data-received-handler session)))))
                    (format t "New Client: ~a~%" conn)
                    (force-output)
                    (setf (sb-bsd-sockets:non-blocking-mode conn) t)
                    (setf (server-session-handler session) handler)
                )
            )
        )
    )
)

(defun ircd () 
    (setf *irc-command-handlers* (make-hash-table :test #'equalp))
    (setf (gethash "USER" *irc-command-handlers*) #'handle-user)
    (setf (gethash "NICK" *irc-command-handlers*) #'handle-nick)
    (setf (gethash "QUIT" *irc-command-handlers*) #'handle-quit)
    (setf (gethash "JOIN" *irc-command-handlers*) #'handle-join)
    (setf (gethash "PART" *irc-command-handlers*) #'handle-part)
    (setf (gethash "KICK" *irc-command-handlers*) #'handle-kick)
    (setf (gethash "MODE" *irc-command-handlers*) #'handle-mode)
    (setf (gethash "WHO" *irc-command-handlers*) #'handle-who)
    (setf (gethash "PING" *irc-command-handlers*) #'handle-ping)
    (setf (gethash "NAMES" *irc-command-handlers*) #'handle-names)
    (setf (gethash "PRIVMSG" *irc-command-handlers*) #'handle-privmsg)
    (setf (gethash "NOTICE" *irc-command-handlers*) #'handle-notice)
    (setf (gethash "TOPIC" *irc-command-handlers*) #'handle-topic)

    (setf *users* (make-hash-table :test #'equalp))
    (setf *channels* (make-hash-table :test #'equalp))

    (with-server (server-socket (:port 6667 :backlog *server-maxc*))
        (sb-sys:with-fd-handler ( (sb-bsd-sockets:socket-file-descriptor  server-socket) :input #'(lambda (fd) (declare (ignore fd)) (accept-handler server-socket)))
            (loop
                (sb-sys:serve-all-events)
            )
        )
    )
)

(ircd)
