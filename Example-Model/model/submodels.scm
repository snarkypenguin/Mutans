; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	submodels.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.07.13
;		Location: zero:/home/randall/Thesis/Example-Model/model/submodels.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2016 Randall Gray
;   All rights reserved
;

;-  Discussion 

;;  Pulls in the submodels

(load "log.scm")         ;; loggers 
(load "landscape.scm")   ;; spatial environments and populations
(load "plant.scm")   ;; spatial environments and populations
(load "animal.scm")      ;; individual-based animals

;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
