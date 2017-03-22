; -*- mode: scheme; -*-
;-  Identification and Changes

;***** When you add things to this file (or any other *-declarations.scm file)
;***** you *must* run "make declarations.scm" or a more comprehensive make.
;*****
;***** This is because the model includes "declarations.scm" rather than the many
;***** other *-declaractions.scm files -- "declarations.scm" is filtered to exclude
;***** duplicate declarations, and this stops methods simply disappearing.



;--
;	monitor-declarations.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.01
;		Location: zero:/home/randall/Thesis/Example-Model/model/monitor-declarations.scm
;
;	History:
;
;-  Code 

;; Defined in framework-declarations
;(declare-method pass-preparation "collect any data which the (process-agent...) routines need")
;(declare-method process-agent "do what ever it needs to do")
;(declare-method pass-resolution "generate a list of actions to take")
;(declare-method action "trigger a representation change, or whatever")

;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
