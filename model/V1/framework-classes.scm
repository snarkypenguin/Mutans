;- Identification and Changes

;- Load initial libraries 

;; support.scm *must* be loaded before this file
(load "support.scm")

;;-------------------------------------------;;
;; This is the order things must happen in   ;;
;; any file defining methods or model bodies ;;
;;---------------------------------------------
;; (include "framework-preamble%.scm")
;; (load-model-framework)
;;---------------------------------------------


;-- Define/allocate new classes

;--- substrate

;(define <pair>        (make-primitive-class))
(define <list>        (make-primitive-class)) ;; add a "list" primitive ... different from "pair"

;(define <null>        (make-primitive-class))
;(define <symbol>      (make-primitive-class))
;(define <boolean>     (make-primitive-class))
;(define <procedure>   (make-primitive-class <procedure-class>))
;(define <number>      (make-primitive-class))
;(define <vector>      (make-primitive-class))
;(define <char>        (make-primitive-class))
;(define <string>      (make-primitive-class))
;(define  <input-port> (make-primitive-class))
;(define <output-port> (make-primitive-class))


;--- helpers/warts



;; sclos classes
(register-class <pair>)
(register-class <null>)
(register-class <boolean>)
(register-class <symbol>)
(register-class <procedure>)
(register-class <number>)
(register-class <vector>)
(register-class <char>)
(register-class <string>)
(register-class <input-port>)
(register-class <output-port>)
(register-class <class>)
(register-class <top>)
(register-class <object>)
(register-class <procedure-class>)
(register-class <entity-class>)
(register-class <generic>)
(register-class <method>)





;--- agent based classes


(define <agent> (make-class (list <object>) 
									 '(name type representation agent-state subjective-time dt 
											  migration-test timestep-schedule kernel counter
											  map-projection
											  state-flags
											  agent-epsilon
											  agent-schedule
											  dont-log
											  agent-body-ran

											  ;; as a parent of other agents
											  subsidiary-agents active-subsidiary-agents parent-nesting-state

											  ;; as a child of other agents
											  nest-parent child-nesting-state)
											  ))

;; subsidiary-agents are agents which may be embedded in a larger dynamic agent. Agents know 
;; what their parent agent is (if they have one) and may indicate to the parent that they should be
;; added to the active list. The parent agent is the one that actually decides if a agent is to move 
;; into the active queue or out of the active queue.  Whe things get moved, "value" from the parent is 
;; moved into the relevant sub-agents.  The set of ecoservices of the parent contains all of the types 
;; represented in its sub-agents.






(register-class <agent>)

;; name is a string
;; representation is a symbol
;; subjective-time, and dt are numbers representing time and an interval, respectively
;; body is a function  (lambda (self t df . args) ...)
;; migration-test is a function  (lambda (self t df . args) ...)
;; timestep-schedule is a list of times at which the agent needs to run (a list of monotonically increasing numbers)
;; kernel is a function that can be used to interact with the kernel of the simulation


(load "log-classes.scm") ;; These are used to generate output.


(define <tracked-agent> (make-class (list <agent>) '(track tracked-paths track-schedule track-epsilon)))
;; "track" will either be a list like (... (t_k x_k y_k) ...) or false
;; "tracked-paths" is a list of non-false traces or false
(register-class <tracked-agent>)

(define <thing> (make-class (list <tracked-agent>) '(mass dim location direction speed)))
(register-class <thing>)
;;(class-register 'add <thing> "<thing>")

(define <environment> (make-class (list <agent>) '(default-value minv maxv))) ;; bounding volume

(register-class <environment>)

;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:


