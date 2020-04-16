; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	remodel-defaults.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2020.04.16
;		Location: ponder:/home/randall/Remodel/remodel/remodel-defaults.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2020 Randall Gray
;   All rights reserved
;

;-  Discussion 

; This file contains "default" values used to control some of the less critical aspects
; of a remodel simulation.  These may well be overridden in the 'model' file


;-  Variables/constants both public and static

;-- Location of a directory with support files (e.g. compiled libraries)
(define support-library-dir "./")


;-- Random numbers
;; Set this to #f  if you want "predictable" rng sequences
(define randomised-rng #t)

(if #t
	 (random-source-randomize! default-random-source))


;-- Default output conventions

(define paper-format isoA4)
;(define paper-format (assoc 'isoA4 paper-alist)) ;; works too

(define time-field-width 8)
(define ps-default-margin 10) ;; implicitly mm NOTE



;-- Directories holding data

(define parameter-directory "parameters")    ;; for class/taxon configuration

(define input-data "input-data")   ;; Things like topography, temperature records ...

(define use-output-date-tag #t)    ;; if true, the output-data-directory will be suffixed by a julian date
(define output-data "output-data") ;; This will be created if it doesn't exist



;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
