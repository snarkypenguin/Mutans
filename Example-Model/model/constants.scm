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

(define pi (acos -1.))
(define 2pi (* 2.0 pi))
(define tau 2pi)
(define e (exp 1))
(define sqrt2pi (sqrt 2pi))

(define 100pi 314)
(define 10000pi 31416)



;; Used as the placeholder for uninitialized things in classes
(define uninitialized (lambda args (abort)))


;-  The End 


;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
