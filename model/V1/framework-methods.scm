o;- Identification and Changes

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


;(define wally (make <animal> '(location (0 0 0) dim 3 direction (1 0 0) mass 3.2 age 1 dt 0.1 representation wombat name "wally" )))

;- Utility functions

(define show-warnings #f)
(define kernel-messages '())
(define temporal-fascist #f) ;; This can make things quite picky.

(define (kdnl* msg . args)
  (if (or (member msg kernel-messages) (member '* kernel-messages)
			 (and (list? msg) (not (null? (intersection msg kernel-messages)))))
		(begin
		  (display msg)(display " ==> ")
		  (apply dnl* args))))

(define (warning . args)
  (if show-warnings
		(begin
		  (display "*** warning: ")
		  (apply dnl* args)
		  )))

(load "log-methods.scm")




;-- Define/allocate new classes

;--- substrate

;--- helpers/warts

;--- agent based classes

;-- handy macros that need class definitions

;-- Define/allocate new generic methods

;; This is defined all in one place so we don't get rogue redefinitions of a generic method clobbering 
;; the definitions which occur earlier in the file. I don't know if tiny-clos can trap such things, but
;; it seems simple enough to split things so that it doesn't happen, though I prefer to keep a class all 
;; in one place.  Ditto for the "make-class" calls.


;--- Helper classes (wart classes)
; none yet

;--- Agent classes

;---- <agent>

;--- Helper classes (wart classes)

;----- (initialise) ;; All subclasses can use this to  initialise things (or make mass assignments 
(sclos-method <object> (initialise self args) ;; args should be a list of the form ('tag value 'tag value ...), slotlist is a list of valid slotnames
				  (kdnl* 'initialise "[" (my 'name) ":" (class-name-of self) "]" "running initialise with " args)
				  (if (and (pair? args) (pair? (car args)) (= (length args) 1))
						(set! args (car args)))
				  (let ((slots  (map car (class-slots (class-of self)))))
					 (kdnl* 'initialise-slots "[" (my 'name) ":" (class-name-of self) "]" "Slots available are: " slots)
					 (for-each 
					  (lambda (slotname argval)
						 (if (member slotname slots)
							  (set-my! slotname argval)
							  (display (string-append "Use of undeclared class variable: "
															  (if (string? slotname) slotname (object->string slotname)) " in " (symbol->string (class-name-of self)) "\n"))
							  )
						 )
					  (evens args) (odds args)))
				  )

(sclos-method <class> (run self pt pstop pkernel)
				  (kdnl* self "[" (my 'name) ":" (class-name-of self) "]" " is not an agent, so we cannot run it.!"))
	
(sclos-method <object> (run self pt pstop pkernel)
				  (kdnl* self "[" (my 'name) ":" (class-name-of self) "]" " is not an agent, so we cannot run it.!"))
	

;--- Agent classes
;---- <agent> methods


;----- (initialize) 

(sclos-method <agent> (initialize self args)
				  (initialise self (list 'state-flags '() 'subjective-time 0.0 'dt 1.0 
												 'migration-test uninitialised 
												 'counter 0 'map-projection (lambda (x) x)
												 'agent-schedule '() 'agent-epsilon 1e-6
												 'agent-state 'ready-for-prep 
												 'agent-body-ran #f

												 'dont-log '(ready-for-prep agent-body-ran agent-schedule agent-epsilon map-projection ;; agent things
																					 counter migration-test state-flags dont-log
																					 timestep-schedule kernel 
																					 introspection-list introspection-schedule timestep-epsilon ;; log agent things
																					 dims ;; thing things
																					 default-value minv maxv ;; environment things
																					 plateau-interval growth-rate ;; ecoservice things
																					 service-list service-update-map update-equations terrain-function dump-times scale ;; landscape things
																					 log-services-from-patch log-patches-from-habitat
																					 domain-attraction food-attraction ;; animal things
																					 near-food-attraction searchspeed wanderspeed foragespeed	
																					 movementspeed foodlist homelist breedlist habitat
																					 )
												 ))
				  (initialize-parent) ;; call "parents" last to make the initialisation list work
				  (initialise self args))


(sclos-method <agent> (agent-prep self . args)
				  (slot-set! self 'timestep-schedule (unique (sort (slot-ref self 'timestep-schedule) <))) ;; ensures no duplicate entries
				  (if (eq? (slot-ref self 'agent-state) 'ready-for-prep)
						(slot-set! self 'agent-state 'ready-to-run)
						(abort (string-append (name self) " has been instructed to prep but it's state is " (my 'agent-state)))
))


(sclos-method <agent> (agent-shutdown self . args) ;; Termination can happen from any state
				  (slot-set! self 'agent-state 'terminated))

;; (add-method initialize
;; 			(make-method (list <agent>)
;; 								 (lambda (initialize-parent self args)
;; 									;;(dnl "<agent> init")
;; 									(initialise self (list 'subjective-time 0.0 'dt 1.0 'migration-test uninitialised))
;; 									(initialize-parent) ;; call "parents" last to make the initialisation list work
;; 									(initialise self args)
;; 									)))

;----- (dump) ;; This dumps all the slots from agent up.  The class-slots of <class> aren't included 

(sclos-method <agent> (dump self . count)
				  (set! count (if (null? count) 0 (car count)))
				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) (display (make-string count #\space)) (display x) (display ": ") (display y)(newline))
								  slots vals)))

;; This is the default log method in the absence of more specific code.

(sclos-method (<agent>) (log-data self logger format caller targets)
				  (kdnl* '(log-* log-data) (name self) "[" (my 'name) ":" (class-name-of self) "]" "in <agent>:log-data")
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
							(kdnl* '(log-* log-data logging-debug) "     " (name self) (class-name-of self) "Dumping " field "=" (if (has-slot? self field) (slot-ref self field) "missing!"))
							(if (not spaced-out)
								 (set! spaced-out #t)
								 (display " " file))
							(display (slot-ref self field) file)
							)
						  ((member field (extra-variable-list self))
							(kdnl* '(log-* log-data logging-debug) "     " (name self) (class-name-of self) "Dumping extra " field "=" (extra-variable self field))
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
					  (unique (if #t targets (filter (not-member (my 'dont-log)) targets))))
					 (newline file)
					 )
				  )

(sclos-method (<agent>) (run-agents self t dt agentlist run)
							 (for-each (lambda (x) 
											 (if (< (subjective-time x) (+ t dt))
														(run x t (+ t dt) (my 'kernel))
														(dnl* "Skipping" (name x)))
											 )
										  agentlist
										  )
							 )

(sclos-method (<agent>) (run-nested-agents self t dt run)
				  (let ((al (my 'subsidiary-agents)))
					 (if (not (null? al))
						  (run-agents self t dt al run))
					 ))
					 


;----- (name) 

(sclos-method <agent> (name self)
				  (if (not (or (string? (my 'name)) (eq? (my 'name) #f)))
						(abort "agent:name -- not a string")
						(my 'name)))

;(sclos-method <agent> (name self)
;				  (my 'name))

;; (add-method name
;; 				(make-method (list <agent>)
;; 								 (lambda (name-parent self)
;; 									(slot-ref self 'name))))

;----- (set-name!) 

(sclos-method (<agent> <string>) (set-name! self n)
				  (if (string? n)
						(set-my! self 'name n)
						(abort "agent:set-name! -- arg is not a string")))

;(sclos-method (<agent> <string>) (set-name! self n)
;				(set-my! self 'name n))


;; (add-method set-name!
;; 				(make-method (list <agent> <string>)
;; 								 (lambda (set-name!-parent self n)
;; 									(if (string? n)
;; 										 (slot-set! self 'name n)
;; 										 (abort "agent:set-name! -- arg is not a string"))
;; 										 )))

(define undefined (lambda x 'undefined))
(define undefined-state-flag (lambda x 'undefined-state-flag))

(sclos-method <agent> (type self)
				  (my 'type))

;;

(sclos-method <agent> (set-type! self newtype)
				  (set-my! 'type newtype))

;;

(sclos-method (<agent> <symbol>) (set-state-flag! self sym val)
				  (let ((v (assoc sym (my 'state-flags))))
					 (if v
						(set-cdr! v val)
						(set-my! 'state-flags (cons (cons sym val)  (my 'state-flags)))
						)
					 ))

(sclos-method (<agent> <symbol>) (add-state-flag self sym val)
				  (if (assoc sym (my 'state-flags))
						(set-state-flag! self sym val)
						(set-my! 'state-flags (cons (cons sym val)  (my 'state-flags)))
						))


(sclos-method (<agent> <symbol>) (state-flag self sym)
				  (let ((r (assoc sym (my 'state-flags))))
				  (if r 
						(cdr r)
						undefined-state-flag)))
						

;----- (representation) 
(sclos-method <agent> (representation self)
				  (let ((rep (my 'representation)))
					 (if (symbol? rep)
						  rep
						  (abort "agent:representation --  not a symbol"))))


;; (add-method representation
;; 				(make-method (list <agent>)
;; 								 (lambda (representation-parent self)
;; 									(my 'representation))))

;----- (set-representation!) 
(sclos-method (<agent> <string>) (set-representation! self n)
				  (if (symbol? n)
						(set-my! self 'representation n)
						(abort "agent:set-representation! -- arg is not a symbol"))
				  )

;; (add-method set-representation!
;; 				(make-method (list <agent> <string>)
;; 								 (lambda (set-representation!-parent self n)
;; 									(if (symbol? n)
;; 										 (slot-set! self 'representation n)
;; 										 (abort "agent:set-representation! -- arg is not a symbol"))
;; 										 )))

;----- (subjective-time) 
(sclos-method <agent> (subjective-time self)
				  (my 'subjective-time))

;; (add-method subjective-time
;; 				(make-method (list <agent>)
;; 								 (lambda (subjective-time-parent self)
;; 									(my 'subjective-time))))

;----- (set-subjective-time!) 
(sclos-method (<agent> <number>) (set-subjective-time! self n)
				  (if (number? n)
						(slot-set! self 'subjective-time n)
						(abort "agent:set-subjective-time! -- arg is not a number")))

;; (add-method set-subjective-time!
;; 				(make-method (list <agent> <number>)
;; 								 (lambda (set-subjective-time!-parent self n)
;; 									(if (number? n)
;; 										 (slot-set! self 'subjective-time n)
;; 										 (abort "agent:set-subjective-time! -- arg is not a number"))
;; 										 )))

;----- (migration-test) 
(add-method migration-test
				(make-method (list <agent>)
								 (lambda (migration-test self)
									(slot-ref self 'migration-test))))

;----- (set-migration-test!) 
(add-method set-migration-test!
				(make-method (list <agent> <number>)
								 (lambda (set-migration-test!-parent self ntest)
									(if (procedure? ntest)
										 (slot-set! self 'migration-test ntest)
										 (abort "agent:set-migration-test! -- arg is not a procedure"))
									)))

;----- (timestep-schedule) 
(add-method timestep-schedule
				(make-method (list <agent>)
								 (lambda (timestep-schedule self)
									(slot-ref self 'timestep-schedule))))


;----- (set-timestep-schedule!) 
(add-method set-timestep-schedule!
				(make-method (list <agent> <number>)
								 (lambda (set-timestep-schedule!-parent self nbody)
									(if (list? nbody)
										 (slot-set! self 'timestep-schedule nbody)
										 (abort "agent:set-timestep-schedule! -- arg is not a procedure"))
									(slot-set! self 'timestep-schedule nbody)
									)))

;----- (kernel) 
(add-method kernel
				(make-method (list <agent>)
								 (lambda (kernel self)
									(slot-ref self 'kernel))))

;----- (set-kernel!) 
(add-method set-kernel!
				(make-method (list <agent> <number>)
								 (lambda (set-kernel! self n)
									(if (number? n)
										 (slot-set! self 'kernel n)
										 (abort "agent:set-kernel! -- arg is not a number"))
									)))



(sclos-method (<agent>) (snapshot self)
				  (map (lambda (x) (list x (slot-ref self x))) (class-slots (class-of self))))


(sclos-method <agent> (i-am self) (my 'representation))
(sclos-method (<agent>) (is-a self list-of-kinds) (member (my 'representation) list-of-kinds))
(sclos-method <agent> (parameter-names self) (map car (class-slots (class-of self))))
(sclos-method <agent> (parameters self) (map (lambda (x) (slot-ref self x)) (map car (class-slots (class-of self)))))
;;(sclos-method <agent> (set-parameters! newparams) (for-each (lambda (x y) (slot-set! self x y)) (parameter-names self) (parameters self)))
(sclos-method (<agent> <pair>) (set-parameters! self newparams) (for-each (lambda (x y) (slot-set! self x y)) (parameter-names self) newparams))

(sclos-method (<agent> <symbol>) (extra-variable self field) #!void)
(sclos-method (<agent> <symbol>) (extra-variable-list self) '())


(sclos-method <agent> (query self kernel . args) (apply (my 'kernel) (append (list 'query) args)))
(sclos-method <agent> (run-at self x) 
				  (let ((tq (cons x (my 'timestep-schedule))))
					 (set-my! 'timestep-schedule (sort tq <=))))

;; This gets called by the kernel, and in turn calls "run-model-body"
;; which calls "model-body" ...  The main purpose of this layer is to
;; rectify time (e.g. ensure that the upcoming tick starts when it is
;; supposed to) and to update the subjective time.

#|
(sclos-method <agent> (run self T pstop pkernel)
				  (kdnl* 'run-model-body "<agent>" (class-name-of self))
				  (let* ((t T)         ;; T is the time to start, 
							(stop pstop)     ;; pstop is the upper limit of the step
							(kernel pkernel) ;; this is used to get non-local information	
							(ttr (let ()     ;; reset the times to run list
									 (set-my! 'timestep-schedule (prune-local-run-queue (my 'subjective-time) (my 'timestep-schedule)))
									 (my 'timestep-schedule)))
							(dt (interval t (slot-ref self 'dt) stop ttr))  ;; work out the dt for this step
							(subj-time (my 'subjective-time)) ;; get the subjective time for the agent
							(DT 0) ;; this will be the total elapsed time relative to subj-time
							)
					 (set-my! 'kernel kernel)   ;; This is the function that handles kernel queries

					 (cond
					  ((< subj-time t)  ;; This will be the usual case
						(if temporal-fascist
							 (begin
								(kdnl* "[" (my 'name) ":" (class-name-of self) "]" "a/an" (my 'representation) "is lost in time at" subj-time "or" t) 
								'missing-time)

							 (let loop-through-time ((st subj-time)
															 (ddt (min (- t subj-time) (my 'dt)))
															 )
								(kdnl* 'passing-control-to-model "[" (my 'name) ":" (class-name-of self) "]" "Passing control to the model at" t "for" (if (< st t) ddt  (- (+ t dt) subj-time)))
								(let ((m (run-model-body self subj-time (if (< st t) ddt  (- (+ t dt) subj-time)) )))
								  (cond
									((eq? m #!void) (abort (string-append "The model body for " (class-name-of self) " returned #!void which is an error")))
									((eq? dt #!void) (abort (string-append "dt for " (class-name-of self) " is somehow #!void which is an error")))
									((eq? DT #!void) (abort (string-append "DT for " (class-name-of self) " is somehow #!void which is an error")))
									((number? m) 
									 (set! DT (+ DT m))
									 (set! st (+ st ddt))
									 (set! subj_time st)

									 (cond
									  ((< st t) (loop-through-time st (min (- t subj-time) (my 'dt))))
									  ((< st (+ t dt)) (loop-through-time st (min (- t subj-time) (my 'dt) dt)))
									  )
									 ((or (symbol? m) (list? m))
									  (kdnl* "BORK!!!" m))
									 (else
									  (kdnl* "BORK!!!" m))
									 ))
								  m)
								))
						)
						
						((and (> dt 0.0) ;; 
								(>= subj-time (+ t  dt)))
						 (kdnl* "[" (my 'name) ":" (class-name-of self) "]" "a/an" (my 'representation) "is driving a DeLorian.  Expected subjective-time to be" t "but it was" subj-time "and dt =" dt) 
						 'back-to-the-future)
						(#t
						 (kdnl* 'passing-control-to-model "[" (my 'name) ":" (class-name-of self) "]" "Passing control to the model at" t "for" dt)
						 (let ((m (run-model-body self t dt)))
							(set! DT (+ DT m))
							m)
						 )
						)

					 (if (zero? DT) (and (dnl "*******************************") (abort "BAD TICK")))

					 (set-subjective-time! self (+ DT (my 'subjective-time)))
					 (kdnl* '(nesting run-model-body) (class-name-of self) (name self) "Leaving run with " DT " @ " (my 'subjective-time) "[" (my 'dt) "]")
					 'ok
					 )
				  )
|#

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
                                    (prune-local-run-queue
                                     (my 'subjective-time)
                                     (my 'timestep-schedule)))
                           (my 'timestep-schedule))))
                (let ((dt (interval t (slot-ref self 'dt) stop ttr)))
                  (let ((subj-time (my 'subjective-time)))
                    (let ((DT 0))
                      (set-my! 'kernel kernel)
                      (cond ((< subj-time t)
                             (if temporal-fascist
                                 (begin
                                   (kdnl* "["
                                          (my 'name)
                                          ":"
                                          (class-name-of self)
                                          "]"
                                          "a/an"
                                          (my 'representation)
                                          "is lost in time at"
                                          subj-time
                                          "or"
                                          t)
                                   'missing-time)
                                 ((letrec ((loop-through-time
                                            (lambda (st ddt)
                                              (kdnl* 'passing-control-to-model
                                                     "["
                                                     (my 'name)
                                                     ":"
                                                     (class-name-of self)
                                                     "]"
                                                     "Passing control to the model at"
                                                     t
                                                     "for"
                                                     (if (< st t)
                                                         ddt
                                                         (- (+ t dt)
                                                            subj-time)))
                                              (let ((m (run-model-body
                                                        self
                                                        subj-time
                                                        (if (< st t)
                                                            ddt
                                                            (- (+ t dt)
                                                               subj-time)))))
                                                (cond ((eq? m #!void)
                                                       (abort (string-append
                                                               "The model body for "
                                                               (class-name-of
                                                                self)
                                                               " returned #!void which is an error")))
                                                      ((eq? dt #!void)
                                                       (abort (string-append
                                                               "dt for "
                                                               (class-name-of
                                                                self)
                                                               " is somehow #!void which is an error")))
                                                      ((eq? DT #!void)
                                                       (abort (string-append
                                                               "DT for "
                                                               (class-name-of
                                                                self)
                                                               " is somehow #!void which is an error")))
                                                      ((number? m)
                                                       (set! DT (+ DT m))
                                                       (set! st (+ st ddt))
                                                       (set! subj_time st)
                                                       (cond ((< st t)
                                                              (loop-through-time
                                                               st
                                                               (min (- t
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                               subj-time)
                            (my 'dt))))
                     ((< st (+ t dt))
                      (loop-through-time st (min (- t subj-time) (my 'dt) dt)))
                     (else #!void))
               ((or (symbol? m) (list? m)) (kdnl* "BORK!!!" m))
               (else (kdnl* "BORK!!!" m)))
              (else #!void))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                m))))
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
                                    "is driving a DeLorian.  Expected subjective-time to be"
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
                             (let ((m (run-model-body self t dt)))
                               (set! DT (+ DT m))
                               m))
                            (else #!void))
                      (if (zero? DT)
                          (and (dnl "*******************************")
                               (abort "BAD TICK")))
                      (set-subjective-time! self (+ DT (my 'subjective-time)))
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
(sclos-method <agent> (run-model-body self t ldt) ;looks like (run-model-body me t dt) in code
				  ;;  The model returns the amount of time it actually ran for
				  (kdnl* '(nesting run-model-body) (class-name-of self) "Running at " t "+" ldt "[" (my 'dt) "]")
				  (if (< dt 0.0) (abort 'bad-dt))

				  (let* ((return (model-body self t ldt)))
					 
					  ;; The model's tick is done now, adjust the subjective_time to reflect how much time it took for the tick
					 (if (number? return) 
					  	  ;;(set-my! 'subjective-time (+ t return)) 
					  	  ;; Any non-numeric return value indicate a "condition"
					  	  ;; which by definition means that no time should have been used
					  	  ;;else ... Huston, we have a problem....
					  	  (begin
					  		 (dnl (my 'name) " returned from its model body with " return)
					  		 return
					  		 ;; Just deal with it.
					  		 ))

					 ;; deal with any changes in the entity's representation, or the general configuration of the model as a whole
					 ;; prefix a symbol ('migrate, for example) to the return value if it needs to change, last bit should be "return"
					 
					 (let ((mtrb ((my 'migration-test) self t ldt return)))
						(if blue-meanie (set-my! 'kernel #f))  ;; we don't want it to make calls to a closure that has vanished

						(if mtrb
							 (if (pair? return) (cons mtrb return) (cons mtrb (list return)))
							 return))
					 )
				  )  ;; returns the amount of time it ran in the last tick, some request to the scheduler or an error condition



;; model-body knows "self" "t" "dt" and all its state variables.  
;; This particular version of the routine should not call parent-body.
(sclos-model-body <agent>
						(if #t
							 (begin
								(kdnl* 'track-subjective-times "[" (my 'name) ":" (class-name-of self) "]" " running at " (my 'subjective-time) ":" t)
								(skip-parent-body)
								))
						dt)



;---- <tracked-agent> methods

;----- (initialize) 
(add-method initialize
				(make-method (list <tracked-agent>)
								 (lambda (initialize-parent self args)
									;;(dnl "<thing> init")
									(initialise self '(track #f tracked-paths #f track-schedule '() track-epsilon 1e-6))
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))


(sclos-method (<tracked-agent> <number> <pair>) (track-locus! self t loc)
				  (let ((tr (my 'track)))
					 (set-my! 'track 
								 (if tr 
									  (append (my 'track) (list (cons t loc)))
									  (list (cons t loc))
								 ))
				  )
)

(sclos-method (<tracked-agent>) (track self)
				  (my 'track))


(sclos-method (<tracked-agent> <number> <pair>) (set-track! self t)
				  (set-my! 'track (deep-copy t))) ;; we copy it so that we aren't subject to the track changing under our feet


(sclos-method (<tracked-agent>) (new-track! self)
				  (let ((p (my 'tracked-paths))
						  (t (my 'track)))
					 (cond 
					  ((and p t) (set-my! 'tracked-paths (cons p t)))
					  (t (set-my! 'tracked-paths (list t))))
					 (set-my! 'track #f)))

(sclos-method (<tracked-agent>) (tracks self)
				  (my 'tracked-paths))

						  
(sclos-model-body <tracked-agent>
						(track-locus! self t (my 'location)) ;; even if they aren't moving
						(parent-body)
						dt
						)
						



;---- <thing> methods

;----- (initialize) 
(add-method initialize
				(make-method (list <thing>)
								 (lambda (initialize-parent self args)
									;;(dnl "<thing> init")
									(initialise self '(dim #f location #f direction #f speed #f mass #f track #f tracked-paths #f))
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))

;----- (mass) 
(add-method mass
				(make-method 
				 (list <thing>)
				 (lambda (call-parent-method self)
					(slot-ref self 'mass))))


;----- (set-mass!) 
(add-method set-mass!
				(make-method 
				 (list <thing> <number>)
				 (lambda (call-parent-method self n)
					(if (not (number? n))
						 (abort (string-append "thing:set-mass! -- bad number"))
						 (slot-set! self 'mass n)))))

;----- (dim) 
(add-method dim
				(make-method 
				 (list <thing>)
				 (lambda (call-parent-method self)
					(slot-ref self 'dim))))


;----- (set-dim!) 
(add-method set-dim!
				(make-method 
				 (list <thing> <number>)
				 (lambda (call-parent-method self n)
					(if (not (integer? n))
						 (abort (string-append "thing:set-dim! -- bad integer"))
						 (slot-set! self 'dim n)))))

;----- (speed) 
(add-method speed
				(make-method 
				 (list <thing>)
				 (lambda (call-parent-method self)
					(slot-ref self 'speed))))


;----- (set-speed!) 
(add-method set-speed!
				(make-method 
				 (list <thing> <number>)
				 (lambda (call-parent-method self n)
					(if (not (number? n))
						 (abort (string-append "thing:set-speed! -- bad number"))
						 (slot-set! self 'speed n)))))


;----- (location) 
(add-method location
				(make-method 
				 (list <thing>)
				 (lambda (call-parent-method self)
					(slot-ref self 'location))))


;----- (set-location!) 
(add-method set-location!
				(make-method 
				 (list <thing>)
				 (lambda (call-parent-method self vec)
					(if (not (= (length vec) (slot-ref self 'dim)))
						 (abort (string-append "thing:set-location! -- bad list length"))
						 (slot-set! self 'location vec)))))


;----- (direction) 
(add-method direction
				(make-method 
				 (list <pair>)
				 (lambda (call-parent-method self)
					(slot-ref self 'location))))


;----- (set-direction!) 
(add-method set-direction!
				(make-method 
				 (list <thing> <pair>)
				 (lambda (call-parent-method self vec)
					(if (not (= (length vec) (slot-ref self 'dim)))
						 (abort (string-append "thing:set-direction! -- bad list length"))
						 (slot-set! self 'direction vec)))))





;---- environment methods

(sclos-method <environment> (min-bound self)
				  (copy-list (my 'minv)))

(sclos-method <environment> (max-bound self)
				  (copy-list (my 'maxv)))

(sclos-method (<environment> <pair>) (contains? self loc)
				  (let ((mbounds (min-bound self))
						  (Mbounds (max-bound self))
						  )
					 (apply andf (append (map < mbounds eloc) (map < eloc Mbounds)))))


;; Default environment only has the default value, oddly enough
(sclos-method (<environment> <pair>) (value self loc)
				  (my 'default-value))

(sclos-method (<environment> <pair>) (set-value! self loc val)
				  (set-my! 'default-value val))

(sclos-method (<environment> <thing>) (contains? self entity)
				  (contains? (location entity))
				  )

(sclos-method (<environment> <symbol> <pair>) (value self tag loc . args)
				  (my 'default-value))

(sclos-method (<environment> <symbol> <pair>) (set-value! self tag loc val)
				  (set-my! 'default-value val))

(sclos-method (<environment> <thing>) (contains? self entity)
				  (contains? (location entity))
				  )












;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
