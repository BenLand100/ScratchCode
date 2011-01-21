(require 'sb-bsd-sockets)


(defun resolve-hostname (name) 
    (cond
        ((eql name :any)  #(0 0 0 0))
        ((typep name '(vector * 4)) name)
        (t (car (sb-bsd-sockets:host-ent-addresses (sb-bsd-sockets:get-host-by-name name))))
    )
)

(defun open-server (&key (host :any) (port 0) (reuse-address t) (backlog 1) (protocol :tcp))
    "Returns a server socket"
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

(defstruct server-session sock fd stream buffer handler command-buffer)

(defun data-received-handler (session)
    (format t "Got incoming data event ... ~%")
    (let ( (buffer (server-session-buffer session)) (sock (server-session-sock session)) )
        (do ((fin nil)) (fin t)
            (setf (fill-pointer buffer) +buflen+)
            (multiple-value-bind (buf len raddr) (sb-bsd-sockets:socket-receive sock buffer nil) (declare (ignore raddr))
                (if (null buf)
                    (setf fin t)
                    (setf (fill-pointer buffer) len)
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
                                (let ((line (format nil "~a~a" (server-session-command-buffer session) (subseq buffer 0  (+ 1 cutoff)))))
                                    (setf (server-session-command-buffer session) (subseq buffer (+ 1 cutoff)))
                                    (if (> (length line) 1)
                                        (progn
                                            (if (not (position #\Newline line)) (setf line (concatenate 'string line (string #\Newline))))
                                            (format t "=>~a" line)
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
    (format t "I've got a new connection on fd ~a~%" socket)
    (let ((conn (sb-bsd-sockets:socket-accept socket)))
        (let ((fd (sb-bsd-sockets:socket-file-descriptor conn)))
            (let 
                (
                    (session (make-server-session 
                        :sock conn 
                        :fd fd 
                        :stream (sb-bsd-sockets:socket-make-stream conn :input t :output t :element-type 'character :buffering :none)
                        :buffer (make-array +buflen+ :element-type 'character :adjustable nil :fill-pointer t)
                        :command-buffer ""
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

(defun start-server ()
    (with-server (server-socket (:port 6667 :backlog 50))
        (sb-sys:with-fd-handler ( (sb-bsd-sockets:socket-file-descriptor  server-socket) :input #'(lambda (fd) (declare (ignore fd)) (accept-handler server-socket)))
            (loop
                (sb-sys:serve-all-events)
            )
        )
    )
)
