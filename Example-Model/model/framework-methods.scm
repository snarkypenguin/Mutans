(include "framework")
;- Identification and Changes

;- Load initial libraries 

;; these must be loaded before this is
;;(include
;;(load "maths.scm")
;;(load "integrate.scm")


;;-------------------------------------------;;
;; This is the order things must happen in   ;;
;; any file defining methods or model bodies ;;
;;---------------------------------------------
;; (include "%framework.scm")
;; (load-model-framework)
;;---------------------------------------------


;;---------------------------------------------------
;; Important routines which I Really Ought to Know ;;
;;---------------------------------------------------




;- Utility functions

(define temporal-fascist #f) ;; This can make things quite picky.

(define FirstJiggle 1.0)
(define LastJiggle 0.0)
(define DefaultPriority 0)


;; the list representation of a vector from s to d, but smart about <agent>s
(define (vector-to s d)
  (let ((src (if (isa? s <thing>) (slot-ref s 'location) s))
		  (dst (if (isa? d <thing>) (slot-ref d 'location) d)))
	 (map - dst src)))

(define (do-map-conversion pfn gfn)
  (let ((cmd (string-append "gs -q -dQUIET -dNOPAUSE -r300x300 -sDEVICE=pnggray -sOutputFile=" gfn " - " pfn " < /dev/null &")))
	 (shell-command cmd)))

(define (arg-pairings symlist objlist)
  (if (> (length objlist) (length symlist))
		(error "There are unpaired objects!\n" symlist objlist))
  
  (let ((n (min (length symlist) (length objlist))))
	 (apply append (map list (list-head symlist n) (list-head objlist n)))))


;-- Define/allocate new classes

;--- substrate

;--- helpers/warts

;--- agent based classes

;-- handy macros that need class definitions

;-- Define/allocate new generic methods

;; This is defined all in one place so we don't get rogue
;; redefinitions of a generic method clobbering the definitions which
;; occur earlier in the file. I don't know if tiny-clos can trap such
;; things, but it seems simple enough to split things so that it
;; doesn't happen, though I prefer to keep a class all in one place.
;; Ditto for the "make-class" calls.


;--- maintenance code (for submodels maintaining data for another representation)

(default-initialization <model-maintenance>)


(model-body <model-maintenance>
				(let ((status-list (map (lambda (kernel t dt maint-routine)
												  (maint-routine kernel t dt))
												(slot-ref self 'maintenance-list)
												)))
				  ;; Now do something with  the status-list!
				dt))



; none yet

;--- Agent classes

;---- <agent>

;--- Helper classes (wart classes)



(attribute-method <attribute> (initialize self args)
						(initialize-parent)
						(set-state-variables ;; We set some reasonable default values for
						 ;; some of the slots
						 self '()
						 )
						;; call "parents" last to make the initialisation list work
						(set-state-variables self args) ;; we now set-state-variables the slot values passed in args
						)

;--- Agent classes
;---- <agent> methods

;----- (initialize) 


(default-initialization <agent> 'state-flags '()
								  'subjective-time 0.0
								  'dt 1.0
								  'maintenance-list '() ;; this is a list of funcs
								  'jiggle 0.0
								  'priority DefaultPriority
								  'migration-test (lambda args #f)  ;; Don't migrate by default
 								  'counter 0 'map-projection (lambda (x) x)
								  'agent-schedule '() 'agent-epsilon 1e-6
								  'agent-state 'ready-for-prep 
								  'agent-body-ran #f)


(model-method (<class>) (run self pt pstop pkernel)
				  (if (not (isa? self <agent>))
						(begin (display "Attempt to (run ...) a non-agent\n")
								 (error "+++Curcurbit Error+++"
										  (slot-ref self 'name)))))



(model-method <agent> (initialize self args)
				  (kdnl* '(track-init) "<agent> initialise---")(pp args)
				  (set-state-variables ;; We set some reasonable default values for
					;; some of the slots
					self (list 'state-flags '()
								  'subjective-time 0.0
								  'dt 1.0
								  'maintenance-list '() ;; this is a list of funcs
								  'jiggle LastJiggle
								  'priority DefaultPriority
								  'migration-test  (lambda args #f) ;; Don't migrate by default
								  'counter 0 'map-projection (lambda (x) x)
								  'agent-schedule '() 'agent-epsilon 1e-6
								  'agent-state 'ready-for-prep 
								  'agent-body-ran #f
								  ))
				  (initialize-parent)
				  ;; call "parents" last to make the initialisation list work
				  (set-state-variables self args) ;; we now set-state-variables the slot values passed in args
				  )


;(model-method <agent> (agent-prep self start end)
(model-method (<agent> <number> <number>) (agent-prep self start end)
				  (kdnl* 'prep (slot-ref self 'name) "entered prep: " start end)
				  (slot-set! self 'timestep-schedule
								 (unique (sort (slot-ref self 'timestep-schedule) <)))
				  ;; ensures no duplicate entries
				  (if (eqv? (slot-ref self 'agent-state) 'ready-for-prep)
						(slot-set! self 'agent-state 'ready-to-run)
						(error (string-append
								  (name self)
								  " has been instructed to prep but it's state is "
								  (slot-ref self 'agent-state)))
						))


;; Termination can happen from any state
(model-method <agent> (agent-shutdown self) 
				  (slot-set! self 'agent-state 'terminated))


;----- (dump) ;; This dumps all the slots from agent up.  

;(model-method <agent> (dump% self)
;				  (dump% self 0))


(model-method (<object>) (dump% self count)
				  (set! count (cond
									((number? count) count)
									((not (pair? count)) 0)
									(#t (car count))))
				  (let* ((slots (class-slots-of self))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y)
									 (display (make-string count #\space))
									 (display "[")
									 (display x) (display ": ")
									 (display y)(display "]")(newline))
								  slots vals)))

(model-method (<agent>) (dump% self count)
				  (set! count (cond
									((number? count) count)
									((not (pair? count)) 0)
									(#t (car count))))
				  (let* ((slots (class-slots-of self))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y)
									 (display (make-string count #\space))
									 (display x) (display ": ")
									 (display y)(newline))
								  slots vals)))


(model-method (<agent>) (log-data% self logger format caller targets)
				  (kdnl* '(log-* log-data)
							(name self)
							"[" (my 'name) ":"
							(class-name-of self) "]" "in <agent>:log-data")
				  (let ((file (slot-ref logger 'file))
						  (show-field-name (slot-ref logger 'show-field-name))
						  (missing-val (slot-ref logger 'missing-val))
						  (spaced-out #f)
						  (cntr 0)
						  )

					 (for-each ;; field in the variable list
					  (lambda (field)
						 (if show-field-name
							  (begin
								 (if (not spaced-out)
									  (set! spaced-out #t)
									  (display " " file))
								 (display field file)))
						 
						 (cond
						  ((has-slot? self field)
							(kdnl* '(log-* log-data logging-debug)      
									 "  " (name self) (class-name-of self)
									 "Dumping " field "=" (if (has-slot? self field)
																	  (slot-ref self field)
																	  "missing!"))
							(if (not spaced-out)
								 (set! spaced-out #t)
								 (display " " file))
							(display (slot-ref self field) file)
							)
						  ((member field (extra-variable-list self))
							(kdnl* '(log-* log-data logging-debug)
									 "  " (name self) (class-name-of self)
									 "Dumping extra " field "="
									 (extra-variable self field))
							(if (not spaced-out)
								 (set! spaced-out #t)
								 (display " " file))
							(display (extra-variable self field) file)
							)
						  (missing-val
							(if (not spaced-out)
								 (set! spaced-out #t)
								 (display " " file))
							(display missing-val file))
						  (else
							(if (not spaced-out)
								 (set! spaced-out #t)
								 (display " " file))
							)	
						  )
						 )
					  (unique (if #t targets
									  (filter (not-member (my 'dont-log)) targets))))
					 (newline file)
					 )
				  )

(model-method (<monitor> <list>) (pass-preparation self  agentlist)
				  (kdnl* 'monitor "Default monitor prep pass" (name self)))

(model-method (<monitor> <list>) (pass-resolution self  agentlist)
				  (kdnl* 'monitor "Default monitor resolve pass" (name self)))



(definition-comment 'run-agents
  "is called by run-nested-agents and is used as a proxy for the call to queue;"
  "this may occur when a habitat takes over patches, for example")
(model-method (<agent>) (run-agents self t dt agentlist run)
				  (let ((monitor-list (filter (lambda (x) (isa? x <monitor>)) agentlist))
						  )
					 (for-each (lambda (x)
									 (pass-preparation x agentlist))
								  monitor-list)
					 (for-each (lambda (x) ;; Note! The agentlist here may be 
									 (pass-resolution x agentlist))
								  monitor-list)
					 )
				  
				  ;; This is wonky if the agent list changes ...
				  (for-each (lambda (x) 
								  (if (< (subjective-time x) (+ t dt))
										(run x t (+ t dt) (my 'kernel))
										(kdnl* 'info "run-agents: skipping" (name x)))
								  )
								agentlist
								)
				  ;; Do I need a dead-agent class and "clean dead agents" ?
				  )

(definition-comment 'run-nested-agents
  "is used to run agents in a nested queue, such as when a habitat takes over patches")
(model-method (<agent>) (run-nested-agents self t dt run)
				  (let ((al (my 'subsidiary-agents)))
					 (if (not (null? al))
						  (run-agents self t dt al run))
					 ))



;----- (name) 

(model-method (<agent>) (name self)
				  (if (not (or (string? (my 'name)) (eqv? (my 'name) #f)))
						(error "agent:name -- not a string")
						(my 'name)))

;(model-method <agent> (name self)
;				  (my 'name))

;; (add-method name
;; 				(make-method (list <agent>)
;; 								 (lambda (name-parent self)
;; 									(slot-ref self 'name))))

;----- (set-name!) 

(model-method (<agent> <string>) (set-name! self n)
				  (if (string? n)
						(set-my! self 'name n)
						(error "agent:set-name! -- arg is not a string")))



(define undefined (lambda x 'undefined))
(define undefined-state-flag (lambda x 'undefined-state-flag))

(model-method (<agent>) (type self)
				  (my 'type))

;;

(model-method <agent> (set-type! self newtype)
				  (set-my! 'type newtype))

;;

(model-method (<agent> <symbol>) (set-state-flag! self sym val)
				  (let ((v (assoc sym (my 'state-flags))))
					 (if v
						  (set-cdr! v val)
						  (set-my! 'state-flags
									  (cons (cons sym val)  (my 'state-flags)))
						  )
					 ))

(model-method (<agent> <symbol>) (add-state-flag self sym val)
				  (if (assoc sym (my 'state-flags))
						(set-state-flag! self sym val)
						(set-my! 'state-flags
									(cons (cons sym val)  (my 'state-flags)))
						))


(model-method (<agent> <symbol>) (state-flag self sym)
				  (let ((r (assoc sym (my 'state-flags))))
					 (if r 
						  (cdr r)
						  undefined-state-flag)))


;----- (representation) 
(model-method <agent> (representation self)
				  (let ((rep (my 'representation)))
					 (if (symbol? rep)
						  rep
						  (error "agent:representation --  not a symbol"))))



;----- (set-representation!) 
(model-method (<agent> <string>) (set-representation! self n)
				  (if (symbol? n)
						(set-my! self 'representation n)
						(error "agent:set-representation! -- arg is not a symbol"))
				  )


;----- (subjective-time) 
(model-method <agent> (subjective-time self)
				  (my 'subjective-time))


;----- (set-subjective-time!) 
(model-method (<agent>) (set-subjective-time! self n)
				  (if (number? n)
						(slot-set! self 'subjective-time n)
						(error "agent:set-subjective-time! -- arg is not a number")))


;----- (priority) 
(model-method (<agent>) (priority self)
				  (my 'priority))


;----- (set-priority!) 
(model-method (<agent> <number>) (set-priority! self n)
				  (if (number? n)
						(slot-set! self 'priority n)
						(error "agent:set-priority! -- arg is not a number")))


;----- (jiggle) 
(model-method (<agent>) (jiggle self)
				  (my 'jiggle))

;----- (set-jiggle!) 
(model-method (<agent> <number>) (set-jiggle! self n)
				  (if (number? n)
						(slot-set! self 'jiggle n)
						(error "agent:set-jiggle! -- arg is not a number")))

;----- (migration-test) 
(model-method <agent> (migration-test self)
				  (my 'migration-test))

;(add-method migration-test
;				(make-method (list <agent>)
;								 (lambda (migration-test self)
;									(slot-ref self 'migration-test))))

;----- (set-migration-test!) 
(model-method (<agent> <number>) (set-migration-test! self ntest)
				  (if (procedure? ntest)
						(slot-set! self 'migration-test ntest)
						(error (string-append
								  "agent:set-migration-test! -- "
								  "arg is not a procedure")))
				  )

;----- (timestep-schedule) 
(model-method <agent> (timestep-schedule self)
				  (slot-ref self 'timestep-schedule))


;----- (set-timestep-schedule!) 
(model-method (<agent> <number>) (set-timestep-schedule! self nbody)
									(if (list? nbody)
										 (slot-set! self 'timestep-schedule nbody)
										 (error (string-append
													"agent:set-timestep-schedule! -- "
													"arg is not a procedure")))
									(slot-set! self 'timestep-schedule nbody)
									)

;----- (kernel) 
(model-method (list <agent>) (kernel self)
									(slot-ref self 'kernel))

;----- (set-kernel!) 

(model-method (<agent> <number>) (set-kernel! self n)
				  (if (number? n)
						(slot-set! self 'kernel n)
						(error (string-append
								  "agent:set-kernel! -- "
								  "arg is not a number"))
						))

(model-method (<agent>) (snapshot self)
				  (map (lambda (x) (list x (slot-ref self x)))
						 (class-slots-of self)))


(model-method <agent> (i-am self) (my 'representation))

(model-method (<agent>) (is-a self list-of-kinds)
				  (member (my 'representation) list-of-kinds))

(model-method <agent> (parameter-names self)
				  (class-slots-of self))
(model-method <agent> (parameters self)
				  (map (lambda (x) (slot-ref self x))
						 (class-slots-of self)))

(model-method (<agent> <pair>) (set-parameters! self newparams)
				  (for-each (lambda (x y) (slot-set! self x y))
								(parameter-names self) newparams))

(model-method (<agent> <symbol>) (extra-variable self field) #!void)
(model-method (<agent> <symbol>) (extra-variable-list self) '())


(define (query self . args)
  (kquery self args))
  
(model-method <agent> (kquery self kernel args)
				  (apply (my 'kernel) (append (list 'query) args)))

(model-method <agent> (run-at self x) 
				  (let ((tq (cons x (my 'timestep-schedule))))
					 (set-my! 'timestep-schedule (sort tq <=))))

;;; (definition-comment 'interval
;;;   "returns an interval (tick-length) based on the current time, the"
;;;   "desired tick length, the nominated end of the run and a list of"
;;;   "target times")

;;; (define (interval t ddt stopat tlist)
;;;   ;; tlist is a sorted queue of times to run
;;;   (if (< (- stopat t) ddt)
;;; 		(set! ddt (- stopat t)))

;;;   (cond
;;; 	((null? tlist)	
;;; 	 ddt)
;;; 	((and (list? tlist) 
;;; 			(number? (car tlist))
;;; 			(= (car tlist) t)
;;; 			)
;;; 	 ddt)
;;; 	((and (list? tlist) 
;;; 			(number? (car tlist))
;;; 			)
;;; 	 (- (car tlist) t))
;;; 	(else 'bad-time-to-run)))

(definition-comment 'prune-local-time-queue
  "remove stale times in the time-to-run queue")
(define (prune-local-time-queue tm ttr)
  (dnl* 'PRUNE-LOCAL-TIME-QUEUE tm ttr)
  (let ((r '())
		  )
	 (if (uninitialised? ttr) (set! ttr (list 0)))
	 (set! r (let loop ((l ttr))
				  (if (or (null? l)
							 (> tm (car l))
							 )
						l
						(loop (cdr l)))))
;	 (set! kernel-time (+ kernel-time (- (cpu-time) call-starts)))
	 r )
  )

(add-method
 run
 (make-method
  (list <agent>)
  (lambda (run-parent self T pstop pkernel)
	 (let ((my (lambda (x) (slot-ref self x)))
			 (set-my! (lambda (x y) (slot-set! self x y)))
			 (kernel (slot-ref self 'kernel)))
		(kdnl* 'run-model-body "<agent>" (class-name-of self))
		(let ((t T))
		  (let ((stop pstop))
			 (let ((kernel pkernel))
				(let ((ttr (begin
								 (set-my! 'timestep-schedule
											 (prune-local-time-queue
											  (my 'subjective-time)
											  (my 'timestep-schedule)))
								 (my 'timestep-schedule))))
				  (let ((dt (interval t (slot-ref self 'dt) stop ttr)))
					 (let ((subj-time (my 'subjective-time)))
						(let ((DT 0))
						  (set-my! 'kernel kernel)
						  (cond
							((< subj-time t)
							 (if temporal-fascist
								  (begin
									 (kdnl* "[" (my 'name) ":"(class-name-of self) "]"
											  "a/an" (my 'representation)
											  "is lost in time at" subj-time "or" t)
									 'missing-time)
								  ((letrec
										 ((loop-through-time
											(lambda (st ddt)
											  (kdnl* 'passing-control-to-model
														"["(my 'name)":"(class-name-of self)"]"
														"Passing control to the model at" t 
														"for" (if (< st t)
																	 ddt
																	 (- (+ t dt) subj-time)))
											  (let ((m (run-model-body
															self
															subj-time
															(if (< st t)
																 ddt
																 (- (+ t dt)
																	 subj-time)))))
												 (cond
												  ((eqv? m #!void)
													(error (string-append
															  "The model body for "
															  (class-name-of
																self)
															  " returned an error: #!void")))
												  ((eqv? dt #!void)
													(error (string-append
															  "dt for "
															  (class-name-of
																self)
															  " is somehow #!void (error)")))
												  ((eqv? DT #!void)
													(error (string-append
															  "DT for "
															  (class-name-of
																self)
															  " is somehow #!void (error)")))
												  ((number? m)
													(set! DT (+ DT m))
													(set! st (+ st ddt))
													(set! subj_time st)
													(cond
													 ((< st t)
													  (loop-through-time st (min (- t
																							  subj-time)
																						  (my 'dt))))
													 ((< st (+ t dt))
													  (loop-through-time
														st
														(min (- t subj-time)
															  (my 'dt)
															  dt)))
													 (#t #!void))
													((or (symbol? m) (list? m))
													 (kdnl* "BORK!!!" m))
													(#t (kdnl* "BORK!!!" m)))
												  (#t #!void))

												 m))
											))
									  loop-through-time)
									subj-time
									(min (- t subj-time) (my 'dt)))))
							((and (> dt 0.) (>= subj-time (+ t dt)))
							 (kdnl* "["
									  (my 'name)
									  ":"
									  (class-name-of self)
									  "]"
									  "a/an"
									  (my 'representation)
									  "is driving a DeLorian."
									  "  Expected subjective-time to be"
									  t
									  "but it was"
									  subj-time
									  "and dt ="
									  dt)
							 'back-to-the-future)
							(#t
							 (kdnl* 'passing-control-to-model
									  "["
									  (my 'name)
									  ":"
									  (class-name-of self)
									  "]"
									  "Passing control to the model at"
									  t
									  "for"
									  dt)

							 ;;; (let ((m (if (isa? self <agent>)
							 ;;; 				  (run-model-body self t dt)
							 ;;; 				  dt)))
							 (let ((m (if (isa? self <agent>)
											  (run-model-body self t dt)
											  dt)))
								(if (not (number? m)) (error 'bummer))

								(set! DT (+ DT m))
								m))
							)
						  (if (zero? DT)
								(and (dnl "*******************************")
									  (error "BAD TICK")))
						  (if (isa? self <agent>)
								(set-subjective-time!
								 self
								 (+ DT (my 'subjective-time))))
						  (kdnl* '(nesting run-model-body)
									(class-name-of self)
									(name self)
									"Leaving run with "
									DT
									" @ "
									(my 'subjective-time)
									"["
									(my 'dt)
									"]")
						  'ok)))))))))))


(define blue-meanie #f)


;; This routine does the running since "run" has fixed up the ticks
;; It looks like (run-model-body me t dt) in code
(model-method <agent> (run-model-body self t ldt)
				  (let ((RTN
							(begin
							  ;;  The model returns the amount of time it actually ran for
							  (kdnl* '(nesting run-model-body) (class-name-of self)
										"Running at " t "+" ldt "[" (my 'dt) "]")
							  (if (< dt 0.0) (error 'bad-dt))

							  (let* ((return (model-body self t ldt)))
								 
								 ;; The model's tick is done now, adjust the
								 ;; subjective_time to reflect how much time it took
								 ;; for the tick
								 (if (number? return) 
									  ;;(set-my! 'subjective-time (+ t return)) 
									  ;; Any non-numeric return value indicate a "condition"
									  ;; which by definition means that no time should
									  ;; have been used else ... Huston, we have a
									  ;; problem....

									  (begin
										 (kdnl* 'model-body  (my 'name)
												" returned from its model body with " return)
										 return
										 ;; Just deal with it.
										 ))

								 ;; deal with any changes in the entity's
								 ;; representation, or the general configuration of the
								 ;; model as a whole

								 ;; prefix a symbol ('migrate, for
								 ;; example) to the return value if it needs to change,
								 ;; last bit should be "return"
								 
								 (let ((mtrb ((my 'migration-test) self t ldt return)))
									;; we don't want to make calls to a closure that has vanished
									(if blue-meanie (set-my! 'kernel #f))  

									(if mtrb
										 (if (pair? return)
											  (cons mtrb return)
											  (cons mtrb (list return)))
										 return))
								 )
							  ))
						  );; returns the amount of time it ran in the last
					 ;;(dnl* (my 'name) (my 'type) RTN)
					 RTN))
							
;; tick, some request to the scheduler or an error
;; condition



;; model-body knows "self" "t" "dt" and all its state variables.  
;; This particular version of the routine should not call parent-body.
(model-body <agent>
				(if #t
					 (begin
						(kdnl* 'track-subjective-times
								 "[" (my 'name) ":" (class-name-of self) "]"
								 " running at " (my 'subjective-time) ":" t)
						
						(if (pair? (my 'maintenance-list))
							 (let ((ml (my 'maintenance-list)))
								;; Now run any agent representations which
								;; have indicated that they need data
								;; maintained
								(for-each
								 (lambda (x)
									(x t dt self))
								 ml)
								))
						(skip-parent-body)
						))
				dt)



;---- <tracked-agent> methods

(default-initialization <tracked-agent> 'track #f 'tracked-paths #f 'track-schedule '() 'track-epsilon 1e-6)



(model-method (<tracked-agent> <number> <pair>) (track-locus! self t loc)
				  (let ((tr (my 'track)))
					 (set-my! 'track 
								 (if tr 
									  (append (my 'track) (list (cons t loc)))
									  (list (cons t loc))
									  ))
					 )
				  )

(model-method (<tracked-agent>) (track self)
				  (my 'track))


(model-method (<tracked-agent> <number> <pair>) (set-track! self t)
				  (set-my! 'track (deep-copy t))) ;; we copy it so that we
;; aren't subject to the
;; track changing under
;; our feet


(model-method (<tracked-agent>) (new-track! self)
				  (let ((p (my 'tracked-paths))
						  (t (my 'track)))
					 (cond 
					  ((and p t) (set-my! 'tracked-paths (cons p t)))
					  (t (set-my! 'tracked-paths (list t))))
					 (set-my! 'track #f)))

(model-method (<tracked-agent>) (tracks self)
				  (my 'tracked-paths))


(model-body <tracked-agent>
				(track-locus! self t (my 'location)) ;; even if they
				;; aren't
				;; moving
				(parent-body)
				dt
				)




;---- <thing> methods

(default-initialization 'dim #f 'location #f
  'direction #f
  'speed #f 'mass #f
  'track #f
  'tracked-paths #f)

;----- (mass) 
(model-method
 (<thing>)
 (mass self)
 (slot-ref self 'mass))


;----- (set-mass!) 
(model-method
 (<thing> <number>)
 (set-mass! self n)
 (if (not (number? n))
	  (error "thing:set-mass! -- bad number")
	  (slot-set! self 'mass n)))

;----- (dim) 
(model-method
 (<thing>)
				 (dim self)
					(slot-ref self 'dim))


;----- (set-dim!) 
(model-method
 (<thing> <number>)
				 (set-dim! self n)
					(if (not (integer? n))
						 (error "thing:set-dim! -- bad integer")
						 (slot-set! self 'dim n)))

;----- (speed) 
(model-method
 (<thing>)
				 (speed self)
					(slot-ref self 'speed))


;----- (set-speed!) 
(model-method 
 (<thing> <number>)
 (set-speed! self n)
 (if (not (number? n))
	  (error "thing:set-speed! -- bad number")
	  (slot-set! self 'speed n)))


;----- (location) 
(model-method 
 (<thing>)
 (location self)
 (slot-ref self 'location))


;----- (set-location!) 
(model-method 
 (<thing>)
 (set-location! self vec)
 (if (not (= (length vec) (slot-ref self 'dim)))
	  (error "thing:set-location! -- bad list length")
	  (slot-set! self 'location vec)))


;----- (direction) 
(model-method 
 (<pair>)
 (direction self)
 (slot-ref self 'location))


;----- (set-direction!) 
(model-method 
 (<thing> <pair>)
 (set-direction! self vec)
 (if (not (= (length vec) (slot-ref self 'dim)))
	  (error "thing:set-direction! -- bad list length")
	  (slot-set! self 'direction vec)))





;---- environment methods

(model-method <environment> (min-bound self)
				  (copy-list (my 'minv)))

(model-method <environment> (max-bound self)
				  (copy-list (my 'maxv)))

(model-method (<environment> <pair>) (contains? self loc)
				  (let ((mbounds (min-bound self))
						  (Mbounds (max-bound self))
						  )
					 (apply andf (append (map < mbounds loc)
												(map < loc Mbounds)))))


;; Default environment only has the default value, oddly enough
(model-method (<environment> <pair>) (value self loc)
				  (my 'default-value))

(model-method (<environment> <pair>) (set-value! self loc val)
				  (set-my! 'default-value val))

(model-method (<environment> <thing>) (contains? self entity)
				  (contains? (location entity))
				  )

(model-method (<environment> <symbol> <pair>) (value self tag loc)
				  (my 'default-value))

(model-method (<environment> <symbol> <pair>) (set-value! self tag loc val)
				  (set-my! 'default-value val))

(model-method (<environment> <thing>) (contains? self entity)
				  (contains? (location entity))
				  )


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
