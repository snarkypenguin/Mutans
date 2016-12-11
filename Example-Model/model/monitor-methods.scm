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
(include "framework")


"First just get it polling the correct set of agents each pass, then we worry about aggregating and such"


(agent-initialisation-method (<monitor> args) (no-default-args)
				  (set-state-variables self (list 'specific-targets '() 'class-targets '() 'predicate-targets '()
												 'predicate (lambda x #f) 'accessor #f))
				  (initialise-parent)
				  ;; call "parents" last to make the initialisation list work
				  )


(model-method (<monitor> <list>) (pass-preparation self subject-list args)
				  (for-each
					(lambda (subject)
					  (kdnl* "Preparing to process list" (slot-ref subject 'name) "at" t "+" dt))
					  subject-list)
				  #t
				  )

(model-method (<monitor> <agent>) (process-agent self subject args)
				  (for-each
					(lambda (subject)
					  (kdnl* "Processing" (slot-ref subject 'name) "at" t "+" dt))
					  subject-list)
				 #t)

(model-method (<monitor> <list>) (pass-resolution self subject-list args)
				  (for-each
					(lambda (subject)
					  (kdnl* "Tidying up" (slot-ref subject 'name) "at" t "+" dt))
					  subject-list)
					#t)



(model-body <monitor>
				(kdnl* '(monitor-bodies model-bodies) "In" (class-name-of self))
				(for-each
				 (lambda (x)
					(process-agent self x))
				 (kernel 'runqueue)) ;; gets the runqueue.  Does not consider agents which are not in the runqueue.
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
