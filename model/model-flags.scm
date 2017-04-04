"These flags control output during the model run.  These flags have
effect when the model is compiled and when the model is interpreted.

The preferred way of adding things is to insert either a kdnl* message 
with an appropriate symbolic tag or list of symbolic tags, or by using 
the predicate kdnl*? with a list of tags, such as 

    (if (kdnl*? 'look!) (pp (inspect-object complex-list-structure)))
    (if (kdnl*? '(recast reorder)) (pp (refactor-object current-strategy complex-list-structure)))
"




;(kernel-message-state #t/#f/) ; empty returns state, #t emits msgs, #f suppresses

;(kernel-msg-tag? tag)
;(clear-kernel-msg-tags)
;(set-kernel-msg-tags tag/taglist)
;(get-kernel-msg-tags)
;(add-kernel-msg-tag tag)
;(remove-kernel-msg-tag tag)
;(kdnl* tag/taglist message)
;(get-kernel-messages)
;(clear-kernel-messages)
;(set-kernel-msg-collection)
;(set-kernel-msg-display)

(kernel-message-state #t)
;(add-kernel-msg-tag 'deep-debugging)

(add-kernel-msg-tag '*)

;(add-kernel-msg-tag 'make-grid)
;(add-kernel-msg-tag 'creation)
;(add-kernel-msg-tag 'agent-creation)
;(add-kernel-msg-tag 'object-creation)
;(add-kernel-msg-tag 'initialisation) ;; report values on initialisation and other places
;(add-kernel-msg-tag 'initialisation-C--) ;; name of class
;(add-kernel-msg-tag 'initialisation-P--) ;; parents of class
;(add-kernel-msg-tag 'initialisation-U--) ;; note setting things to <uninitialised>
;(add-kernel-msg-tag 'initialisation-CLASS--) ;; note setting things to <uninitialised>
;(add-kernel-msg-tag 'state-vars) ;; report values on initialisation and other places
;(add-kernel-msg-tag 'parameter-load)
;(add-kernel-msg-tag 'parameter-file)
(add-kernel-msg-tag 'run-agent)      ;; outside the agent
(add-kernel-msg-tag 'run-model-body) ;; outside the agent
;(add-kernel-msg-tag 'running)
;(add-kernel-msg-tag 'trace-bodies) ;; Entry and exit w.r.t. whole thing
;(add-kernel-msg-tag 'model-body) ;; the model body & exit value, and calls to parents
;(add-kernel-msg-tag 'time-team) ;; tracepoints for tracking temporal problems
;(add-kernel-msg-tag 'methodical-madness) ;; for debugging compute-method stuff
(add-kernel-msg-tag 'introspection*)
(add-kernel-msg-tag 'log*)
(add-kernel-msg-tag 'introspection-trace)
(add-kernel-msg-tag 'log-horrible-screaming)





