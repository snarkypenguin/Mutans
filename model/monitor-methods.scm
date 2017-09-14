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
				  ;; The flagged-agents will change representation
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
  

;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
