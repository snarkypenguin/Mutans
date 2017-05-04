"These flags control output during the model run.  These flags have
effect when the model is compiled and when the model is interpreted.

The preferred way of adding things is to insert either a kdebug message 
with an appropriate symbolic tag or list of symbolic tags, or by using 
the predicate kdebug? with a list of tags, such as 

    (if (kdebug? 'look!) (pp (inspect-object complex-list-structure)))
    (if (kdebug? '(recast reorder)) (pp (refactor-object current-strategy complex-list-structure)))
"



;(kdebug-message-state #t/#f/) ; empty returns state, #t emits msgs, #f suppresses

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
;(add-kdebug-msg-tag 'trace-model-methods)
;(add-kdebug-msg-tag 'trace-model-body)
;(add-kdebug-msg-tag 'introspection*)
(add-kdebug-msg-tag 'log*)
;(add-kdebug-msg-tag '<ecoservice>-method)
;(add-kdebug-msg-tag '<ecoservice>-body)
;(add-kdebug-msg-tag 'introspection-trace)
;(add-kdebug-msg-tag 'log-horrible-screaming)

;(add-kdebug-msg-tag '*)
(kdebug-message-state #t)



