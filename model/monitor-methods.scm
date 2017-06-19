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
;-- fundamental monitor class methods

;;; ;--- initialisation -;
;;; (agent-initialisation-method <monitor> (args) (specific-targets '() selector (lambda x #f) tree '(0 0 {})) -;
;;; 									  (parent-initialise) -;
;;; 									  ;; call "parents" last to make the initialisation list work -;
;;; 									  ) -;

;--- pass-preparation
;; This method sets the list of agents to be dealt with to the empty list
(model-method (<monitor> <list>) (pass-preparation self subject-list args)
				  (parent-body)
				  (slot-set! tree '(0 0 {}))
				  (slot-set! self 'flagged-agents '())
				  )

;--- process-agent
;; the selector function returns #f only if an agent should be excluded
(model-method (<monitor> <agent>) (process-agent self subject args)
				  ;; Usually restricted to a domain, niche or agent representation
				  ;; primarily used to construct a tree to represent the configuration's suitability 
				  ;; and interdependencies.
				  ;; The "trunk" of the tree is constructed here, agents may populate the leaves
				  (kdebug "Processing" (slot-ref subject 'name) "at" t "+" dt)
				  (let ((status ((my 'trigger-selector) subject)))
					 (if status (set-my! 'flagged-agents (cons subject (my 'flagged-agents)))))
				  #t)

;--- pass-resolution
(model-method (<monitor> <list>) (pass-resolution self subject-list args)
				  (for-each
					(lambda (subject)
					  (resolve-agent self subject-list args)
					  (kdebug "Tidying up" (slot-ref subject 'name) "at" t "+" dt))
					subject-list)
				  #t)

;--- model-body
(model-body <monitor>
				(kdebug '(monitor-bodies model-bodies) "In" (cnc self))
				;; pass-preparation is subclass specific
				(pass-preparation self (unique (append (slot-ref self 'target-list) (slot-ref self 'specific-agents))) '())
				(for-each
				 (lambda (x)
					(process-agent self x)) ;; defined by derived classes
				 (kernel 'runqueue)) ;; gets the runqueue.  Does not consider agents which are not in the runqueue.
				;; pass resolution *may* be subclass specific.
				;;    The default calls (resolve-agent ...) for each of the members of flagged-agents
				(pass-resolution self (slot-ref self 'flagged-agents) '())
				(parent-body)
				dt)

;-- Niche monitors -- These create and store trees representing the possible configurations and their strengths/weaknesses
;--- Generic infrastructure 
;---- (assess-representations domain) assesses the niches in a given domain
(define (assess-representations domain)
  (let* ((candidate-reps (slot-ref self 'representation-list)) ;; representations which the domain is sensitive to
			(candidate-trees (map (lambda (x) (list-copy zerotree)) candidate-reps)) ;; list of trees to hold the "state" of reps
			(candidate-agents '()) ;; a-list of agents in domain keyed by representation class
			(niches (slot-ref domain 'niche-list))
			)
	 ;; Collect information about what agents are in each representation 
	 (for-each
	  (lambda (agent)
		 (let* ((agent-rep (slot-ref agent 'representation))
				  )
			(set! candidate-agents (assoc-append candidate-agents agent-rep agent)) ;; makes new entry or appends agent to a-list entry
			))
	  (slot-ref domain 'agent-list))

	 ;; Construct set of assessment trees for each representation group
	 (let ((nca (count-keyed-members candidate-agents)))
		(map
		 (lambda (i)
			(list (car list-ref candidate-agents i)
					(apply
					 tree+
					 (map (lambda (j) ;; (assess-rep rep) returns a function to assess a particular representation
							  ((assess-rep (car (list-ref candidate-agents i)))
								(list-ref (cdr candidate-agents) j)))
							(seq (list-ref nca i))	
							)
					 ))
			(seq (length candidate-agents)))))
	 ;; returns a list of keyed lists containing the aggregated assessment tree
	  ))
					 

;--- (assess-rep repclass) and (set-rep-assessor! rep assessor)
"The assess-rep function shares hidden information with the set-rep-assessor! function.
The idea here is that the model cannot (accidentally) modify the
assessment of a particular representation.  This could be implemented
as a class with methods, and such, but a more direct approach feels
more appropriate. The implementations of each representation will call
set-rep-assessor! to set the assessment routine.

Because we use a symbol, rather than the class of an agent, to
determine the representation, we can lump multiple 'species' into one
bit of assessment.  This is more flexible wsince the actual class
implementation and gross agent specification is decoupled from the effective grouping. 

Be warned, though: blithely using a label like 'animal may lump things together in an
inappropriate way.
"

;;(define (delete-rep-assessor! rep assessor)  (lambda x #f()))
(define (set-rep-assessor! rep assessor) (void))
(define (assess-rep agnt)  #f)

;; Encapsulate the rep register
(let ((repreg '()) ;; repreg will only be visible *inside* the functions...
		(none (lambda x 0))
		)
  (set! set-rep-assessor! ;;; Calling this twice will replace the first assignment with the second
		  (lambda (rep assessor) ;; rep is the class of agents -- DO NOT MAKE THIS A PARENT CLASS WITHOUT THINKING ABOUT IT!
			 (set! repreg (acons (!filter (lambda (x) (eq? (car x) rep)) repreg) rep assessor))))
  (set! assess-rep (lambda (rep) ;; rep is the symbol associated with the representation
							(let ((a (assoc repreg rep)))
							  (if (or (not a) (null? a))
									none ;; if there is no matching assessor, indicate that no change is needed
									(cdr a)))))
  )



  





	 



;--



				



;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
