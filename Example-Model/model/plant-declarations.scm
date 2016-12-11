(include "framework")
; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	plant-declarations.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.05
;		Location: zero:/home/randall/Thesis/Example-Model/model/plant-declarations.scm
;
;	History:

;-  Code 

;; Individual-based plants
(declare-method plant-radius "returns the radius of the plant")
(declare-method fruit-count "returns the number of fruit produced by an agent")
(declare-method add-fruit "adjusts the number of fruit in a cell")

		
;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
