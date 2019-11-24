;- Identification and Changes

;- Load initial libraries 

;; sclos.scm *must* be loaded before this file


;;-------------------------------------------;;
;; This is the order things must happen in   ;;
;; any file defining methods or model bodies ;;
;;---------------------------------------------
;; (include "framework-preamble%.scm")
;; (load-model-framework)
;;---------------------------------------------


;-- Define/allocate new classes

;--- substrate

;; we will add a "list" primitive ... different from "pair"
;(define <pair>        (make-primitive-class))
(define <list>         (make-primitive-class)) 
(define <integer>      (make-primitive-class))
(define <rational>     (make-primitive-class))
(define <real>         (make-primitive-class))
(define <complex>      (make-primitive-class))

(define general-class-of
  (let* ((primitive-class-of class-of)
			(co (lambda (x)
					(cond ;; these are carefully ordered!
					 ((list? x)        <list>)
					 ((integer? x)     <integer>) 
					 ((rational? x)    <rational>)
					 ((real? x)        <real>)
					 ((complex? x)     <complex>)
					 (#t (primitive-class-of x))))))
	 (set! class-of co)))
			



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
(register-class <integer>)
(register-class <rational>)
(register-class <real>)
(register-class <complex>)
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





(model-class <agent> (inherits-from <object>) 
				 (state-variables name type representation agent-state
										note
										kernel 
										subjective-time priority jiggle 
										dt
										schedule
										migration-test timestep-schedule counter
										map-projection
										state-flags
										agent-epsilon
										agent-schedule
										dont-log
										agent-body-ran

										;; as a parent of other agents
										subsidiary-agents active-subsidiary-agents
										parent-nesting-state

										;; as a child of other agents
										nest-parent child-nesting-state
										)
				 )

;; subsidiary-agents are agents which may be embedded in a larger
;; dynamic agent. Agents know what their parent agent is (if they have
;; one) and may indicate to the parent that they should be added to
;; the active list. The parent agent is the one that actually decides
;; if a agent is to move into the active queue or out of the active
;; queue.  Whe things get moved, "value" from the parent is moved into
;; the relevant sub-agents.  The set of ecoservices of the parent
;; contains all of the types represented in its sub-agents.
;;
;; priority is an integer, the higher the integer the greater the
;; priority.  The default value is zero jiggle is a real number in (0,
;; 1).  Setting jiggle to values outside that domain suppresses the
;; the randomisation of the jiggle.  If an agent has a schedule, then
;; the schedule WILL be used in determining the next dt.



;; name is a string
;; representation is a symbol
;; subjective-time, and dt are numbers representing a time and an interval,
;; body is a function (lambda (self t df . args) ...)
;; migration-test is a function (lambda (self t df . args) ...)
;; timestep-schedule is a list of times at which the agent needs to run
;;    (a list of monotonically increasing numbers)
;; kernel is a function that can be used to interact with
;;    the kernel of the simulation


(load "log-classes.scm") ;; These are used to generate output.


(make-class <tracked-agent> (inherits-from <agent>)
				(state-variables track tracked-paths track-schedule track-epsilon))

;; "track" will either be a list like (... (t_k x_k y_k) ...) or false
;; "tracked-paths" is a list of non-false traces or false

(model-class <monitor> (inherits-from <agent>)
				  (state-variables targetlist))


(model-class <thing> (inherits-from <tracked-agent>)
				 (state-variables mass dim location direction speed))


;;(class-register 'add <thing> "<thing>")


(model-class <environment> (inherits-from <agent>)
				  (state-variables default-value minv maxv)) ;; bounding volume


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:




