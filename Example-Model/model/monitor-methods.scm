; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	monitor-methods.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.01
;		Location: zero:/home/randall/Thesis/Example-Model/model/monitor-methods.scm
;
;	History:
;

;-  Code

"First just get it polling the correct set of agents each pass, then we worry about aggregating and such"


(model-method <monitor> (initialize self args)
				  (set-state-variables self (list 'specific-targets '() 'class-targets '() 'predicate-targets '()
												 'predicate (lambda x #f) accessor #f))
				  (initialize-parent)
				  ;; call "parents" last to make the initialisation list work
				  )

(model-method (pass-preparation self subject-list args)
				  (kdnl* "Preparing to process list" (slot-ref subject 'name) "at" t "+" dt)
				  #t
				  )

(model-method (process-agent self subject args)
				 (kdnl* "Processing" (slot-ref subject 'name) "at" t "+" dt))
				 #t)

(mode-method (pass-resolution self subject-list args)
				 (kdnl* "Tidying up" (slot-ref subject 'name) "at" t "+" dt)
				 #t)


(model-body <monitor>
				(kdnl* '(monitor-bodies model-bodies) "In" (class-name-of self))
				(for-each
				 (lambda (x)
					(process-agent self x))
				 agent-list)
				(parent-body)
				dt)
				



;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
