; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	introspection-classes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.12.29
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/introspection-classes.scm
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
(include "framework")

(define-class <introspection> (inherits-from <agent>)
  (state-variables introspection-targets ;; agents if the selector is false, otherwise classes, strings or lists
						 timestep-epsilon
						 ))

;; introspection-targets is either a list of agents or a selector function which is
;; used with filter and the runqueue.

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
