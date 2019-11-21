; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	monitor-methods.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.01
;		Location: zero:/home/randall/Thesis/Example-Model/model/monitor-methods.scm
"
    Copyright 2017 Randall Gray

    This file is part of Remodel.

    Remodel is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Remodel is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Remodel.  If not, see <http://www.gnu.org/licenses/>.
"
;
;	History:

;

;-  Code
(include "remodel")

"First just get it polling the correct set of agents each pass, then we worry about aggregating and such"
"The monitor collects a list of agents for polling, polls them for the important state information, then 
looks to see if there are any rules for aggregating, then it acts appropriately.

Clearly, there may be some deep knowledge required in terms of how representations work to effect some 
changes ... we hope not too much.

"
;-- fundamental monitor class methods

;--- model-body
(model-body <monitor>
				(let ((sched (my 'timestep-schedule))
						)
				  
				  (if (kdebug? 'introspection-trace)
						(pp (dumpslots self)))
				  
				  (set! dt (if (and (pair? sched) (< (car sched) (+ t dt)))
									(- (car sched) t)
									dt))

				  (kdebug '(monitor-* introspection-trace)
							 "      list:     " (map name (my-list self)))
				  (kdebug '(monitor-* introspection-trace)
							 "      schedule: "
							 (list-head (my 'timestep-schedule) 3)
							 (if (> (length (my 'timestep-schedule)) 3)
								  '... ""))

				(kdebug '(monitor-bodies model-bodies) "In" (cnc self))
				;; pass-preparation is subclass specific

				(manage-configuration self)

				(call-all-parents)
				dt)
				)

"do-assessment is analogous to the log-data methods in classes like <plant>, <plant-array>  and the like.
 It's role is to assess/accumulate the appropriate state information an agent at a time.
"

;--- manage-configuration -- equivalent to emit-page
(model-method (<monitor>) (manage-configuration self)
				  (let ((kind (my 'assessment-domain)))
				  (if (not (or (my 'file) (file-handle? (my 'file)) (output-port? (my 'file))))
						(open-output-handle self))


				  (pass-preparation self (unique (append (slot-ref self 'target-list) (slot-ref self 'specific-agents))) '())


				  (let ((proc (lambda (ila)
									 (kdebug '(monitor-* introspection-trace) " ==> processing "
												(cnc ila) " "  (procedure? ila) kind)

									 (kdebug '(chaintrack) "about to call do-assessement" (my-list self))
									 (do-assessment ila self kind (my 'variables))
									 (kdebug '(chaintrack) "back from do-assessment")
									 (kdebug '(monitor-* introspection-trace) " <== PROCESSED "
												(cnc ila) " "  (procedure? ila))
									 #f
									 )))
					 (for-each proc (my-list self))
					 )

				  ;; pass resolution *may* be subclass specific.
				  ;;    The default calls (resolve-agent ...) for each of the members of flagged-agents

				  (pass-resolution self kind)
				  ))




;--- pass-preparation -- equivalent to page-preamble
;; This method sets the list of agents to be dealt with to the empty list
;; format indicates which level of assessment is happening
(model-method (<monitor> <list>) (pass-preparation self format)   
				  (parent-body)
				  (slot-set! tree '(0 0 {}))
				  (slot-set! self 'flagged-agents '())
				  ;; The flagged-agents will change representation

				  (do-assessment ila self format)

				  ;; Set things up for constructing the set of trees that describe the configuration
				  
				  )

;--- do-assessement -- equivalent to log-data
;; the selector function returns #f only if an agent should be excluded
(model-method (<monitor> <agent>) (do-assessment self subject args)       ;; t
				  ;; Usually restricted to a domain, niche or agent representation
				  ;; primarily used to construct a tree to represent the configuration's suitability 
				  ;; and interdependencies.

				  ;; This should mainly be a self assessement by the subject 

				  (kdebug "Processing" (slot-ref subject 'name) "at" t "+" dt)
				  (self-assess subject self args)

				  #t)

;--- pass-resolution --  equivalent of page-epilogue

(model-method (<monitor> <list>) (pass-resolution self)  
				  (for-each
					(lambda (subject)
					  (resolve-assessment self subject-list args) ;; 
					  (kdebug "Tidying up" (slot-ref subject 'name) "at" t "+" dt))
					subject-list)
				  #t)


;-- Generic infrastructure 
  
;-- Agent monitors -- collects data regarding the state of an agent's appropriateness

(model-method (<agent> <agent-monitor>) (do-assessment self monitor)
				  (let ((assessment ((slot-ref monitor 'assessment-function) self))
						  )
					 (slot-set! monitor 'aggregate-data (cons assessment (slot-ref monitor 'aggregate-data)))
					 )
				  )


(declare-method self-assessment "self assess suitability in a context")
(model-method (self-assessment <agent> <monitor> <list>) (self-assessment self monitor list-of-others)
				  (if (not (adaptable? self)) ;; #t indicates that a change is *essential*, #f means that change is impossible
						#f
						(let ()
						  #f)
						  
						)
				  )
									 

(declare-method resolve-assessment "act on a monitor's assessment")
(model-method (<agent>) (adaptable? self))





;-- Niche monitors -- These create and store trees representing the possible configurations and their strengths/weaknesses

;-- Domain monitors -- collects data regarding the state of the representation of a domain (such as a study area)







;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
