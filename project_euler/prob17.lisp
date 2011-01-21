#!/usr/bin/sbcl --script

(defun replace-all (string part replacement &key (test #'char=))
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string :start2 old-pos :test test)
            do (write-string string out :start old-pos :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 


(format t "~a~%" (let ((strs (loop for x from 1 while (<= x 1000) collect (format nil "~R" x)) ))
    (apply #'+ (map 'list #'length
        (let ((list (map 'list
            (lambda (s) (remove #\- (remove #\Space (replace-all s "hundred " "hundred and")))) strs))) 
            (print list) 
            list) )) ) )
