(include "remodel-framework")
; -*- mode: scheme; -*-
;-  Identification and Changes

;***** When you add things to this file (or any other *-declarations.scm file)
;***** you *must* run "make declarations.scm" or a more comprehensive make.
;*****
;***** This is because the model includes "declarations.scm" rather than the many
;***** other *-declaractions.scm files -- "declarations.scm" is filtered to exclude
;***** duplicate declarations, and this stops methods simply disappearing.



;--
;	plant-declarations.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.05
;		Location: zero:/home/randall/Thesis/Example-Model/model/plant-declarations.scm
;
;	History:

;-  Code 

;; Individual-based plants

(declare-method radius "returns the radius of the plant")
(declare-method fruit-count "returns the number of fruit produced by an agent")
(declare-method add-fruit "adjusts the number of fruit in a cell")
(declare-method growth-rate "rate dependent on water stress and current mass")
(declare-method resolve-assessment "deal with potential changes in representation")

		
;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
