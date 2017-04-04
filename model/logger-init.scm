;-  Identification and Changes

;--
;	logger-init.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2014.10.12
;		Location: pooh:/local/home/randall/study/src/logger-init.scm
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

(define logger 
  (create <log-data> "datalogger" (list 'name "Data" 
								 'dt 4
								 'missing-val missing-value
								 'timestep-schedule schedtimes 
								 'filename "Data"
								 'variables '()) ;; log-table does not automatically log the name at the front of the line
		  )
  )

;;; Do not 
;;     (set! Q (cons logger Q)) ;; insert the logger at the head of the list
;;  this gets done in the pre-run-setup file.

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
