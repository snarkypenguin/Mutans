; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	run-model.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.11.09
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/run-model.scm
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

(load "remodel")        ;; load the modelling framework
(include "remodel.config")  ;; configure any debugging/informational flags & functions
(include "model.config")    ;; configure the model (define spatial and temporal domains, create agents (monitors, loggers, ecosystems....)
(Doit Q)                    ;; run it


;-  The End 


;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
