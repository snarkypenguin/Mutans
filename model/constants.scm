; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	constants.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.11.10
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/constants.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2016 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

;-- Mathematical constants

;;(define pi (acos -1.))
(define 2pi (* 2.0 pi))
(define pi*2 2pi)
(define tau 2pi)
(define sqrt2pi (sqrt 2pi))

;(define e (exp 1))
(define e*2 (* 2 e))
(define e*-2 (* -2 e))

(define 100pi 314)
(define 10000pi 31416)

(define 1-1/e (- 1 (/ 1 e))) ;; ~ .6321

;; Used as the placeholder for uninitialised things in classes
(define uninitialised (lambda args (abort 'uninitialised-function)))



;-  The End 


;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
