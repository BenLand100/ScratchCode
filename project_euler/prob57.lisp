#!/usr/bin/sbcl --script

(defun sqrt2m1 (i) (if (> i 0) (/ 1 (+ 2 (sqrt2m1 (- i 1)))) 0))
(defun sqrt2 (i) (+ 1 (sqrt2m1 i)))
(format t "~a~%" (loop for i from 1 to 1000 count (let ((str (format nil "~a" (sqrt2 i)))) 
    (> (position #\/ str) (- (/ (+ 1 (length str)) 2) 1) ) )))
