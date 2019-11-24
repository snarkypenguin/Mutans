;-  Identification and Changes

;--
;	savanna-init.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2014.10.12
;		Location: pooh:/local/home/randall/study/src/seabed-init.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2014 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(load 'load-list?)

;; (generate-a-habitat namn typ default-ht domain terrain-function npatches populations subdivisions)
(define H (generate-a-habitat "Savanna" 'savanna 0 (list '(0 0 10) domain )  (plane '(10 4 6 0)) 2 savanna-population 12))

(if (not (member 'nested-habitat nested-agents))
	 (set! Q (add-habitat-to-queue Q H)) ;; this introduces all of the "sub-agents" into the run-queue
	 (set! Q (list H)))

(if (member 'logger model-list) (set-introspection-list! logger (copy-list (patch-list H))))

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
