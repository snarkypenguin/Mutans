; -*- mode: scheme; -*-

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






(define trap-model-bodies-with-bad-return-values #f) ;; This is a good idea in development and when
                                                     ;; things seem to be going off the rails

(define use-agent-register #f) ;; enables or disables the agent register -- if the agent register is active
                               ;; garbage collection of agents is stopped

(define use-proxies #f)        ;; enables or disables the use of the <proxy> class

(define production-run #f)     ;; This indicates whether to include protective code in a number of fundamental
                               ;; routines [like the (my 'variable) and (set-my! 'variable val) calls]

"
The following flags control output during the model run.  These flags 
have effect when the model is compiled and when the model is interpreted.

The preferred way of adding things is to insert either a kdebug message 
with an appropriate symbolic tag or list of symbolic tags, or by using 
the predicate kdebug? with a list of tags, such as 

    (if (kdebug? 'look!) (pp (inspect-object complex-list-structure)))
    (if (kdebug? '(recast reorder)) (pp (refactor-object current-strategy complex-list-structure)))
"
;(kdebug-message-state #t/#f) ; empty returns state, #t emits msgs, #f suppresses

;(kdebug-msg-tag? tag)
;(clear-kdebug-msg-tags)
;(set-kdebug-msg-tags tag/taglist)
;(get-kdebug-msg-tags)
;(add-kdebug-msg-tag tag)
;(remove-kdebug-msg-tag tag)
;(kdebug tag/taglist message)
;(get-kdebug-messages)
;(clear-kdebug-messages)
;(set-kdebug-msg-collection)
;(set-kdebug-msg-display)

;(add-kdebug-msg-tag 'deep-debugging)

;(add-kdebug-msg-tag 'make-grid)
;(add-kdebug-msg-tag 'creation)
;(add-kdebug-msg-tag 'agent-creation)
;(add-kdebug-msg-tag 'object-creation)
;(add-kdebug-msg-tag 'initialisation) ;; report values on initialisation and other places
;(add-kdebug-msg-tag 'initialisation-C--) ;; name of class
;(add-kdebug-msg-tag 'initialisation-P--) ;; parents of class
;(add-kdebug-msg-tag 'initialisation-U--) ;; note setting things to <uninitialised>
;(add-kdebug-msg-tag 'initialisation-CLASS--) ;; note setting things to <uninitialised>
;(add-kdebug-msg-tag 'state-vars) ;; report values on initialisation and other places
;(add-kdebug-msg-tag 'parameter-load)
;(add-kdebug-msg-tag 'parameter-file)
;(add-kdebug-msg-tag 'running)
;(add-kdebug-msg-tag 'trace-bodies) ;; Entry and exit w.r.t. whole thing
;(add-kdebug-msg-tag 'model-body) ;; the model body & exit value, and calls to parents
;(add-kdebug-msg-tag 'time-team) ;; tracepoints for tracking temporal problems
;(add-kdebug-msg-tag 'methodical-madness) ;; for debugging compute-method stuff
;(add-kdebug-msg-tag 'run-agent)      ;; outside the agent
;(add-kdebug-msg-tag 'model-methods)
;(add-kdebug-msg-tag 'trace-model-body)
;(add-kdebug-msg-tag 'introspection*)
;(add-kdebug-msg-tag 'log*)
;(add-kdebug-msg-tag '<ecoservice>-method)
;(add-kdebug-msg-tag '<ecoservice>-body)
;(add-kdebug-msg-tag 'introspection-trace)
;(add-kdebug-msg-tag 'log-horrible-screaming)

;(add-kdebug-msg-tag '*)



"These flags control output during the syntactic processing 
associated with loading the framework ... this is really only
relevant when models as interpreted code."

;; THIS IS LOADED BETWEEN sclos+extn and the rest


;; (define-macro (display-model-classes) #f)

;; (define-macro (display-model-methods) #f)
;; (define-macro (display-expanded-model-methods) #f)

;; (define-macro (display-model-bodies) #f)
;; (define-macro (display-expanded-model-bodies) #f)


;; This should be true if you want UNFINISHED-BUSINESS reported, such as
;;     (UNFINISHED-BUSINESS "This is as coherent as a busload of clowns")
;; ... it's an aide memoir....
(define UNFINISHED #f) 

;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
