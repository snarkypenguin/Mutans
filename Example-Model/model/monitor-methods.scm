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
"The monitor collects a list of agents for polling, polls them for the important state information, then 
looks to see if there are any rules for aggregating, then it acts appropriately.

Clearly, there may be some deep knowledge required in terms of how representations work to effect some 
changes, we hope not too many.
"

(agent-initialisation-method (<monitor> args) (no-default-args)
				  (initialise-parent)
				  ;; call "parents" last to make the initialisation list work
				  )

(agent-initialisation-method (<simple-monitor> args)
									  (specific-targets '() selector #f group-selector #f test #f group-test #f)
									  (set-state-variables self (list 'specific-targets '() ;; specific agents to be polled
																				 'selector #f       ;;; eg (lambda x #f) -- a predicate which
                                                                                ;;; indicates if an agent ought to be polled
																				 'group-selector #f ;;; 
																				 'test #f           ;;; tests the list of individuals
																				 'group-test #f     ;;; tests the broader configuration.
																				 ))
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
