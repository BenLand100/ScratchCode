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

; 2009/6/17 20:49:55 BenLand100
#|
The function SEND-LINE is undefined.
|#

; 2009/6/17 20:50:19 BenLand100
(in-package irc-bot) (send-line "privmsg chanserv :invite #cafebebe")

; 2009/6/17 20:50:48 Jason2gs
#|
The variable HELLO is unbound.
|#

; 2009/6/17 20:50:55 Jason2gs
#|
The function SAY is undefined.
|#

; 2009/6/17 20:51:41 Jason2gs
#|
illegal function call
|#

; 2009/6/17 20:51:53 Jason2gs
#|
The function EXEC is undefined.
|#

; 2009/6/17 20:52:50 BenLand100
#|
SB-INT:SIMPLE-READER-PACKAGE-ERROR at 16 (line 1, column 16) on #<SB-IMPL::STRING-INPUT-STREAM {B2F03D1}>:
  The symbol "PRIVMSG" is not external in the IRC-BOT package.
|#

; 2009/6/17 20:55:38 BenLand100
(progn (in-package irc-bot) (send-line "privmsg chanserv :invite #cafebebe"))

; 2009/6/17 20:55:51 BenLand100
(progn (in-package irc-bot) (send-line "join #cafebebe"))

; 2009/6/17 20:59:5 Jason2gs
#|
illegal function call
|#

; 2009/6/17 20:59:36 Jason2gs
#|
The function ME is undefined.
|#

; 2009/6/17 21:0:8 BenLand100
'hello

; 2009/6/17 21:8:46 Jason2gs
#|
The function QUIT is undefined.
|#

; 2009/6/17 21:8:56 Jason2gs
#|
The function ¡ is undefined.
|#

; 2009/6/17 21:9:43 Jason2gs
#|
The function EXIT is undefined.
|#

; 2009/6/17 21:9:52 Jason2gs
#|
The function QUIT is undefined.
|#

; 2009/7/20 2:4:7 BenLand100
'lisp

; 2009/10/1 23:56:49 BenLand100
#|
end of file on #<SB-IMPL::STRING-INPUT-STREAM {AAD37A1}>
|#

; 2009/10/1 23:56:51 BenLand100
t

; 2009/10/1 23:57:0 BenLand100
pi

; 2009/10/1 23:57:40 BenLand100
(defun msg (x y) (privmsg x y))

; 2009/10/1 23:57:55 BenLand100
(msg "#sanddm" "wat")

; 2009/10/1 23:58:41 BenLand100
'wat

; 2009/10/1 23:58:45 Sandstorm
#|
end of file on #<SB-IMPL::STRING-INPUT-STREAM {AB617A1}>
|#

; 2009/10/1 23:58:51 Sandstorm
pi

; 2009/10/1 23:58:54 Sandstorm
#|
The variable 3*3 is unbound.
|#

; 2009/10/1 23:59:0 Sandstorm
#|
The variable |3^3| is unbound.
|#

; 2009/10/1 23:59:7 BenLand100
(* 3 3)

; 2009/10/1 23:59:10 BenLand100
#|
The function ^ is undefined.
|#

; 2009/10/1 23:59:14 BenLand100
#|
invalid number of arguments: 2
|#

; 2009/10/1 23:59:19 BenLand100
#|
The function POW is undefined.
|#

; 2009/10/1 23:59:19 Sandstorm
#|
The function |3^3^26| is undefined.
|#

; 2009/10/1 23:59:36 Sandstorm
pi

; 2009/10/1 23:59:56 BenLand100
(dolist (x '(3 3 3)) x)

; 2009/10/2 0:0:31 BenLand100
(dolist (x '(3 3 3)) (privmsg "#sanddm" "~A" x))

; 2009/10/2 0:0:53 BenLand100
#|
The variable CHAN is unbound.
|#

; 2009/10/2 0:1:8 BenLand100
(defvar chan "#SandDM")

; 2009/10/2 0:1:29 BenLand100
chan

; 2009/10/2 0:2:16 BenLand100
(setf chan "#srl")

; 2009/10/2 15:22:45 BenLand100
'wizzup

; 2009/10/11 2:11:38 BenLand100
#|
The value 1 is not of type CHARACTER.
|#

; 2009/10/11 2:12:3 BenLand100
#|
The function PRIVMSG is undefined.
|#

; 2009/10/11 2:12:26 BenLand100
#|
SB-INT:SIMPLE-READER-PACKAGE-ERROR at 16 (line 1, column 16) on #<SB-IMPL::STRING-INPUT-STREAM {B3A37A1}>:
  The symbol "PRIVMSG" is not external in the IRC-BOT package.
|#

; 2009/10/11 2:12:33 BenLand100
#|
The function IRC-BOT.PRIVMSG is undefined.
|#

; 2009/10/11 2:12:57 BenLand100
#|
The function COMMON-LISP-USER::PRIVMSG is undefined.
|#

; 2009/10/11 2:13:12 BenLand100
(irc-bot::privmsg "Furry" "!rape ~a~a~aPRIVMSG #SRL HAI!!!" (code-char 1) (code-char 10) (code-char 13))

; 2009/10/11 2:14:15 BenLand100
(irc-bot::privmsg "Furry" "!rape ~a~aPRIVMSG #SRL HAI!!!" (code-char 10) (code-char 13))

; 2009/10/11 2:15:16 BenLand100
(irc-bot::privmsg "Furry" "!rape ~a~a~aPRIVMSG #SRL HAI!!!~a" (code-char 1)(code-char 10) (code-char 13)(code-char 1))

; 2009/10/11 2:15:38 BenLand100
(irc-bot::privmsg "Furry" "~a!rape ~a~aPRIVMSG #SRL HAI!!!~a" (code-char 1)(code-char 10) (code-char 13)(code-char 1))

; 2010/6/15 2:8:17 Colquhoun
nil

; 2010/6/15 2:8:37 Colquhoun
#|
arithmetic error DIVISION-BY-ZERO signalled
Operation was SB-KERNEL::DIVISION, operands (1 0).
|#

; 2010/6/15 2:11:25 Colquhoun
pi

; 2010/6/15 2:15:16 [-jesus-]
#|
The variable |1+1| is unbound.
|#

; 2010/6/15 2:15:31 Colquhoun
(+ 1 1)

; 2010/6/15 2:15:34 [-jesus-]
(+ 1 1)

; 2010/6/15 2:15:46 [-jesus-]
#|
The function ^ is undefined.
|#

; 2010/6/15 2:15:51 Colquhoun
#|
The function EVEN is undefined.
|#

; 2010/6/15 2:16:12 [-jesus-]
(* 2 2)

; 2010/6/15 2:16:22 Colquhoun
(defun even (x) (= (mod x 2) 0))

; 2010/6/15 2:16:28 Colquhoun
(even (+ 1 1))

; 2010/6/15 2:16:42 Colquhoun
(even (+ 1 2))

; 2010/6/15 2:19:0 [-jesus-]
(* 65537 65537)

; 2010/6/15 2:19:31 Colquhoun
(* 1535436457436342  34632768467867532543)

; 2010/6/15 2:28:38 Colquhoun
#|
The function NUMBER-NAME is undefined.
|#

; 2010/6/15 2:30:17 Colquhoun
#|
The variable DEST is unbound.
|#

; 2010/6/15 2:30:37 Colquhoun
#|
The function PRIVMSG is undefined.
|#

; 2010/6/15 2:39:7 [-jesus-]
(+ 1 1)

; 2010/6/15 2:44:16 Colquhoun
#|
The variable OGRE is unbound.
|#

; 2010/6/15 2:44:25 Colquhoun
#|
The value :OGRE is not of type NUMBER.
|#

; 2010/6/15 2:44:48 Colquhoun
#|
The value "Ogre" is not of type NUMBER.
|#

; 2010/6/15 2:45:35 Colquhoun
#|
The value :SS23 is not of type NUMBER.
|#

; 2010/6/15 2:45:44 Colquhoun
(equalp :ss23 :smart)

; 2010/6/15 2:46:38 Colquhoun
(let ((ss23 t)(stupid t)) (equalp ss23 stupid))

; 2010/6/15 2:47:14 Colquhoun
(* 2 2 2)

; 2010/6/16 22:41:5 Colquhoun
(if (and :python :sucks) "yup, it fails")

; 2010/6/16 22:49:23 Colquhoun
#|
The variable COCKS is unbound.
|#

; 2010/6/16 23:50:45 Colquhoun
NxTitle

; 2010/6/16 23:51:5 Colquhoun
(setf NxTitle 0)

; 2010/6/16 23:51:29 Colquhoun
ss23

; 2010/6/16 23:51:36 Colquhoun
ss23

; 2010/6/16 23:52:35 Colquhoun
ss23 

; 2010/6/17 0:2:32 Colquhoun
Colquhoun 

; 2010/6/17 0:2:47 Colquhoun
Colquhoun 

; 2010/6/17 0:2:54 Colquhoun
Colquhoun 

; 2010/6/17 0:3:1 Colquhoun
Colquhoun 

; 2010/6/17 0:4:0 Colquhoun
BenLand100

; 2010/6/17 15:51:39 Colquhoun
(load "bindings.lisp")

; 2010/6/17 15:58:43 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:2:8 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:5:2 Colquhoun
(gethash "BenLand100#SRL" kick-votes)

; 2010/6/17 16:5:29 Colquhoun
(gethash "BenLand100#SRL" kick-votes)

; 2010/6/17 16:6:32 Colquhoun
(gethash "BenLand100" kick-voted)

; 2010/6/17 16:6:37 Colquhoun
(gethash "BenLand100" kick-voted)

; 2010/6/17 16:6:47 Colquhoun
(gethash "BenLand100" kick-voted)

; 2010/6/17 16:7:40 Colquhoun
kick-voted

; 2010/6/17 16:8:21 Colquhoun
(gethash "Colquhoun" kick-voted)

; 2010/6/17 16:9:37 Colquhoun
(gethash "TehDoctor#thedoctor" kick-votes)

; 2010/6/17 16:11:24 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:12:3 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:12:46 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:13:22 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:15:16 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:16:50 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:18:13 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:20:34 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:21:31 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:21:39 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:21:54 Colquhoun
(gethash "TehDoctor#thedoctor" kick-votes)

; 2010/6/17 16:22:2 Colquhoun
(gethash "TehDoctor#thedoctor" kick-votes)

; 2010/6/17 16:22:26 Colquhoun
(setf (gethash "TehDoctor#thedoctor" kick-votes) 0)

; 2010/6/17 16:22:28 Colquhoun
(gethash "TehDoctor#thedoctor" kick-votes)

; 2010/6/17 16:22:39 Colquhoun
(gethash "BenLand100#SRL" kick-votes)

; 2010/6/17 16:22:51 Colquhoun
(gethash "BenLand100#SRL" kick-votes)

; 2010/6/17 16:22:53 Colquhoun
(gethash "BenLand100#SRL" kick-votes)

; 2010/6/17 16:22:57 Colquhoun
(gethash "BenLand100#SRL" kick-votes)

; 2010/6/17 16:23:10 Colquhoun
(SETF (gethash "BenLand100#SRL" kick-votes) 0)

; 2010/6/17 16:23:34 Colquhoun
(load "bindings.lisp")

; 2010/6/17 16:24:12 Colquhoun
(setf (gethash "NxTitle#srl" kick-votes) 4)

; 2010/6/17 16:24:47 Colquhoun
(gethash "NxTitle#srl" kick-votes)

; 2010/6/17 16:25:30 Colquhoun
(setf (gethash "NxTitle#srl" kick-votes) 0)

; 2010/6/17 16:37:5 Colquhoun
(setf (gethash "rubix#srl" kick-votes) 4)

; 2010/6/17 16:37:49 Colquhoun
(gethash "rubix#srl" kick-votes)

; 2010/6/17 16:44:37 Colquhoun
(gethash "rubix#srl" kick-votes)

; 2010/6/17 16:44:46 Colquhoun
(setf (gethash "rubix#srl" kick-votes) 0)

; 2010/6/17 16:46:18 Colquhoun
#|
end of file on #<SB-IMPL::STRING-INPUT-STREAM {B17DDD9}>
|#

; 2010/6/17 16:46:26 Colquhoun
#|
The variable LOLHAX is unbound.
|#

; 2010/6/17 16:46:34 Colquhoun
#|
SB-INT:SIMPLE-READER-ERROR at 12 (line 1, column 12) on #<SB-IMPL::STRING-INPUT-STREAM {B18DD61}>:
  non-list following #S
|#

; 2010/6/17 16:49:8 Colquhoun
(gethash "navatwo#srl" kick-votes)

; 2010/6/17 16:49:24 Colquhoun
(gethash "navatwo#srl" kick-votes)

; 2010/6/17 16:49:26 Colquhoun
(gethash "navatwo#srl" kick-votes)

; 2010/6/17 16:49:35 Colquhoun
(gethash "navatwo#srl" kick-votes)

; 2010/6/17 16:49:46 Colquhoun
(gethash "navatwo#srl" kick-votes)

; 2010/6/17 22:6:14 Colquhoun
*message-hooks*

; 2010/6/17 22:10:14 Colquhoun
(setf *message-hooks* nil)

; 2010/6/17 23:16:23 Colquhoun
#|
unknown LOOP keyword: NIL
current LOOP context: NIL.
|#

; 2010/6/17 23:16:31 Colquhoun
#|
The function NIL is undefined.
|#

; 2010/6/17 23:23:0 Colquhoun
*message-hooks*

; 2010/6/17 23:23:14 Colquhoun
(setf *message-hooks* nil)

; 2010/6/18 3:4:5 Colquhoun
(cons (cons 'a 'b) (cons 'c 'd))

; 2010/6/18 3:4:57 Colquhoun
#|
end of file on #<SB-IMPL::STRING-INPUT-STREAM {B97ADC1}>
|#

; 2010/6/18 3:5:2 Colquhoun
(cons 'a (cons 'b (cons 'c (cons 'd nil))))

; 2010/6/18 3:5:51 Colquhoun
(cons (cons 'a 'b) (cons (cons 'c 'd) nil))

; 2010/6/18 3:6:39 Colquhoun
#|
invalid number of arguments: 1
|#

; 2010/6/18 3:6:56 Colquhoun
#|
invalid number of arguments: 3
|#

; 2010/6/18 3:7:10 Colquhoun
(cons (cons 'a  (cons 'c 'd)) nil)

; 2010/6/18 3:7:44 Colquhoun
(car (cons 'a 'b))

; 2010/6/18 3:7:49 Colquhoun
(cdr (cons 'a 'b))

; 2010/6/18 3:8:33 Colquhoun
(setf (car (cons 'a 'b)) 'lol)

; 2010/6/18 3:9:27 Colquhoun
(let ((hax (cons 'a 'b))) (setf (car hax) 'lol) hax)

; 2010/6/18 3:10:20 Colquhoun
#|
The value BENLAND100 is not of type NUMBER.
|#

; 2010/6/18 3:10:27 Colquhoun
(equalp 'BenLand100 'ss23)

; 2010/6/18 3:11:9 Colquhoun
(if 'sleep 'go-to-sleep 'stay-up)

; 2010/6/19 23:51:26 Colquhoun
(gethash "Why_So_Serious#SRL" kick-votes)

; 2010/6/19 23:51:45 Colquhoun
(gethash "Why_So_Serious#SRL" kick-votes)

; 2010/6/21 2:7:42 Colquhoun
'lisp

