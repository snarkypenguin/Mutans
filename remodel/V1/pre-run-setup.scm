;-  Identification and Changes

;--
;	pre-run-setup.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2014.10.12
;		Location: pooh:/local/home/randall/study/src/pre-run-setup.scm
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


(if (member 'logger model-list)
	 (begin
		(for-each (lambda (x) (set-map-projection! x mm->points)) Q)
		(set! Q (cons logger Q)) ;; insert the logger at the head of the list
		))


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
