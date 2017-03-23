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


;(include "heritability")

(model-method <agent> (change-taxon self taxon)
				 (apply-parameters self (slot-ref self 'taxon)))


;; This is called with a class, a taxon symbol, and a list of initialisation pairs symbol value ...
;; and an optional, final init function that expects "self"
		  

;- Utility functions

(define temporal-fascist #f) ;; This can make things quite picky.

(define track-initialisations #t)

(define FirstJiggle 1.0)
(define LastJiggle 0.0)
(define DefaultPriority 0)

(define class? class-name-of)


(define (dump whatsit)
  (let* ((C (if (class? whatsit) whatsit (class-of whatsit)))
			)
	 (display (class-name-of C))
	 (display " ")
	 (pp (dumpslots A))
	 ))


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

;;; (define-macro (lookit-this)
;;;   (let ((tst (default-agent-initialisation <model-maintenance>)))
;;; 	 (pp tst)))

(model-body <model-maintenance>
				(let ((status-list (map (lambda (kernel t dt maint-routine)
												  (maint-routine kernel t dt))
												(slot-ref self 'maintenance-list)
												)))
				  ;; Now do something with  the status-list!
				dt))



; none yet

;--- Methods for <object> classes

;- Utility functions and data

;-- default projections ... mostly used by loggers.
"These function should be able to take either ordinates (if it merely 
straightforward scaling) or vectors (for more complex mappings, like LL->xy).
"



(define *default-projections*
  ;; The projections must take whole vectors since we cannot (properly) map
  ;; geographic ordinates otherwise.  mapf converts an ordinate-wise mapping
  ;; to a vector function.
  (list
	(cons 'I  (lambda (x) x)) ;; The identity mapping
	(cons 'mm->points (mapf mm->points)) ;; defined in postscript.scm
	(cons 'points->mm (mapf points->mm)) ;; defined in postscript.scm
	(cons 'inches->points (mapf inches->points)) ;; defined in postscript.scm
	(cons 'points->inches (mapf points->inches)) ;; defined in postscript.scm
	(cons 'mm->inches (mapf mm->inches)) ;; defined in postscript.scm
	(cons 'inches->mm (mapf inches->mm)) ;; defined in postscript.scm
	))

(define (add-to-*default-projections* p k)
  (if (not (assoc *default-projections* k))
		(set! *default-projections* (cons  (cons k p) *default-projections*))))

(define (*projection* ob sym)
				  (let* ((pal (slot-ref ob sym)))
					 (if pal
						  (cdr pal)
						  (error "There is no projection associated with" sym))))

(model-method <projection> (project-datum self sym datum)
				  (if (eqv? sym current-projection)
						((my 'map-projection) datum)
						(let* ((pal (assoc sym (my projection-assoc-list))))
						  (if pal
								((cdr pal) datum)
								(error "There is no projection associated with" sym)))))

						  
(model-method <object> (projection-assoc-list self key)
				  (let ((p (assoc (my 'projection-assoc-list) key)))
					 (if p (cdr p) #f)))

(model-method (<object> <procedure>) (set-projection-assoc-list! self p k)
				  (set-my! 'projection-assoc-list (cons (cons k p) (filter (lambda (x) (not (equal? (car x) k))) (my 'projection-assoc-list)))))

(model-method <projection> (map-projection self)
				  (my 'map-projection))

(model-method <projection> (set-map-projection! self key)
				  (set-my! 'map-projection (projection self key)))



;--- kernel Methods for <agent> classes

;--- Fundamental "run" routine -- accepts anything, but fails if it's inappropriate

(model-method (<class>) (run self pt pstop pkernel)
				  (if (not (isa? self <agent>))
						(begin (display "Attempt to (run ...) a non-agent\n")
								 (error "+++Curcurbit Error+++"  
										  (slot-ref self 'name)))))

;---- <query> 


;; A query to an agent must take a completely arbitrary list of arguments.  If the agent is unable to
;; recognise the query it returns (void)
(model-method <agent> (query self tag #!rest args)
				  (case tag
					 ((value) (if (pair? args) (slot-ref self (car args))))
					 (else (kquery self kernel tag args))
				  ))


;;; ;--- Helper classes (wart classes) 

;;; (object-initialisation-method <object> "this is done in sclos+extra", but somewhat differently
;;;  (initargs) ;; <object> is the most primitive of the framework classes,  there are no default intitialisation args
;;;  '(no-default-values) 				      ;; and the "make" initialisation args are called initargs
;;;  ;; Body:
;;;  (set-state-variables self initargs) ;; we now set-state-variables the slot values passed in args
;;;  )


;--- Fundamental "run" routine -- accepts anything, but fails if it's inappropriate

(model-method (<class>) (run self pt pstop pkernel)
				  (if (not (isa? self <agent>))
						(begin (display "Attempt to (run ...) a non-agent\n")
								 (error "+++Curcurbit Error+++"  
										  (slot-ref self 'name)))))

;--- Agent classes
"We have the agent initialisation here, rather than in
sclos+extras.scm, for a few reasons.  First, it is much more complex
and people may need to look at it to see what is happening.  Second,
<object>s are really not much more than data structures with
specialised methods.  <agent>s are much more complex, and might not be
commonly viewed as just a simple extension of sclos.
"
;---- <agent> methods
;----- (initialise) 


;;; (agent-initialisation-method <agent> (initargs) '(no-default-values)
;;; 				  (kdnl* '(track-init) "<agent> initialise---")
;;; 				  ;;(dnl* "In <agent> initialise:" (slot-ref self '<agent>-initialised))

;;; 				  ;;(pp args)
;;; 				  ;;(dnl "agent")
;;; 				  (slot-set! self '<agent>-initialised #t)

;;; 				  (initialise-parent) ;; the only parent is <object>

;;; 				  (set-state-variables ;; We set some reasonable default values for
;;; 					;; some of the slots
;;; 					self (list
;;; 							'active-subsidiary-agents '()
;;; 							'agent-body-ran #f
;;; 							'agent-epsilon 1e-6
;;; 							'agent-schedule '()
;;; 							'agent-state 'ready-for-prep 
;;; 							'counter 0
;;; 							'dt 1.0
;;; 							'jiggle LastJiggle
;;; 							'maintenance-list '() ;; this is a list of funcs
;;; 							'map-projection (lambda (x) x)
;;; 							'migration-test  (lambda args #f) ;; Don't migrate by default
;;; 							'name '<nameless>
;;; 							'needs-parent-initialisers #f ;; can be #f, 1 (primaries only), or #t (all)
;;; 							'needs-parent-bodies #f
;;; 							'note ""
;;; 							'priority DefaultPriority
;;; 							'state-flags '()
;;; 							'subjective-time 0.0
;;; 							'subsidiary-agents '()
;;; 							))
;;; 				  ;; call "parents" last to make the initialisation list work
;;; 				  (set-state-variables self initargs) ;; we now set-state-variables the slot values passed in args
				  
;;; 				  (let ((initslots (evens initargs)))
;;; 					 (if (member 'type initslots)
;;; 						  (apply-parameters self (slot-ref self 'type))))
;;; 				  )


;;; ;;; (model-method (<agent>) (set-parameters self type-name)
;;; ;;; 				  (set! type-name (if (string? type-name) type-name (object->string type-name)))
;;; ;;; 				  (let* ((type (string->symbol type-name))
;;; ;;; 							(fname (string-append "parameters/" type-name))
;;; ;;; 							(param-list (if (file-exists? fname) (with-input-from-file fname read-all) '()))
;;; ;;; 						  )
;;; ;;; 					 (slot-set! self 'type type)
;;; ;;; 					 (for-each
;;; ;;; 					  (lambda (setting)
;;; ;;; 						 (if (has-slot? self (car setting))
;;; ;;; 							  (let ((len (length setting))
;;; ;;; 									  (slot (car setting))
;;; ;;; 									  (implicit-lists #f)
;;; ;;; 									  )
;;; ;;; 								 (cond
;;; ;;; 								  ((= 1 len) (slot-set! self slot #t))
;;; ;;; 								  ((= 2 len) (slot-set! self slot (cadr setting)))
;;; ;;; 								  (implicit-lists (slot-set! self slot (cdr setting)))
;;; ;;; 								  (#t (error "parameter has more than one value!" type-name settings))))))
;;; ;;; 					  param-list)))


(model-method <agent> (change-type self newtype)
				 (apply-parameters self (slot-ref self 'type)))



(model-method <agent> (kernel-check self #!rest args)
				  ((slot-ref self 'kernel) 'check self args))



(model-method <agent> (provides self)
				  (list-copy (slot-ref self 'provides)))

(model-method <agent> (requires self)
				  (list-copy (slot-ref self 'requires)))

(model-method <agent> (provides? self args)
				  (if (not (pair? args)) (set! args (list args)))
				  (list-intersection args (slot-ref self 'provides)))

(model-method <agent> (requires? self args)
				  (if (not (pair? args)) (set! args (list args)))
				  (list-intersection args (slot-ref self 'requires)))


(model-method <agent> (provides! self service)
				  (if (not (pair? args)) (set! args (list args)))
				  (let ((pl (slot-ref self 'provides)))
					 (if (not (member service pl))
						  (slot-set! self 'provides (cons service pl)))))
						  
(model-method <agent> (requires! self service)
				  (if (not (pair? args)) (set! args (list args)))
				  (let ((pl (slot-ref self 'requires)))
					 (if (not (member service pl))
						  (slot-set! self 'requires (cons service pl)))))
						  

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
(model-method <agent> (agent-shutdown self #!rest args) 
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



;----- (name) 

(model-method (<agent>) (name self)
				  (let ((n (if (has-slot? self 'name) (my 'name) '<nameless>)))
					 (cond
					  ((eqv? n '<nameless>)
						"--nameless--")
					  ((string? n)
						n)
					  (#t	(error "agent:name -- not a string")))))

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
  ;;(dnl* 'PRUNE-LOCAL-TIME-QUEUE tm ttr)
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

(define ACTIVE #t)
(define INACTIVE #f)

(definition-comment 'acquire-agents
  "incorporate a nominated agent or list of agents into an agents internal 
subsidiary-agent list.  Use ACTIVE or INACTIVE ")
(model-method (<agent>) (acquire-agents self is-active agent*)
				  (if (agent? agent*) (set! agent* (list agent*)))  ;; simplify processing below
				  (if (not (list? (slot-ref self 'active-subsidiary-agents)))
						(begin
						  (slot-set! self 'active-subsidiary-agents '())
						  (slot-set! self 'subsidiary-agents '())))
				  
				  (if (apply andf (map agent? agent*))
						(let ((p (slot-ref self 'subsidiary-agents))
								(q (slot-ref self 'active-subsidiary-agents))
								)
						  (for-each
							(lambda (a)
							  (set! p (q-insert p a Qcmp))
							  (if (member is-active '(#t active Active ACTIVE)) (set! q (q-insert q a Qcmp))))
							agent*)
						  
						  (slot-set! self 'subsidiary-agents p)
						  (slot-set! self 'active-subsidiary-agents q)
						  )
						(error "Bad agent passed to acquire-agents" agent*)))


(model-method (<agent>) (transfer-agent self is-active agent* kernel)
				  (if (agent? agent*) (set! agent* (list agent*))) ;; simplify processing below

				  (if (apply andf (map agent? agent*))
						(let ((p (slot-ref self 'subsidiary-agents))
								(q (slot-ref self 'active-subsidiary-agents))
								)
						  
						  (for-each
							(lambda (a)
							  (if (list? p) (set! p (q-insert p a Qcmp)))
							  (if (and (member is-active '(#t active Active ACTIVE)) (list? q))
									(set! q (q-insert q a Qcmp))))
							agent*)
						  (slot-set! self 'active-subsidiary-agents p)
						  (kernel 'remove agent*)
						  )
						(error "Transferring an agent from nowhere")
						)

				  (error "Bad agent passed to transfer-agent" agent*)
				  )
				  
(model-method <agent> (adjust-state self mutator #!rest args)
				  (if mutator
						(mutator self args)
						(if (pair? args) (set-state-variables self args))
						)
				  )
					  
(model-method (<agent>) (activate self selector)
				  (let* ((subs (slot-ref self 'subsidiary-agents))
							(asubs (slot-ref self 'active-subsidiary-agents))
							(candidates (filter selector subs))
							(targets (filter (lambda (x) (not (memq x asubs))) candidates))
							)
					 (for-each
					  (lambda (a)
						 (slot-set! self 'active-subsidiary-agents
										(q-insert (slot-ref self 'active-subsidiary-agents) a Qcmp)))
					  targets)))

(model-method (<agent>) (deactivate self selector)
				  (let* ((subs (slot-ref self 'subsidiary-agents))
							(asubs (slot-ref self 'active-subsidiary-agents))
							(targets (filter selector subs))
							)
					 (for-each
					  (lambda (x)
						 (slot-set! x 'suspended-at (slot-ref x 'subjective-time))
						 (excise x asubs))
					  targets)))



(definition-comment 'run

"This is the routine which gives the agents a chance to do their thing. It is 
also present as an illustration of the low-level way of implementing a method.
The arguments are 
	self    -- the agent to be run
	T       -- the starting time of the interval to be simulated
	pstop   -- the end of the simulation interval
	pkernel -- a function which allows queries to the kernel (and hence other 
              agents
")


;; This is added using the fundamental routine that associates a method with a generic
;; in SCLOS.


(define blue-meanie #f)

;; blue-meanie blocks access to the kernel when the agent is not
;; currently running.  This is mainly for debugging in restricted
;; subsets of a model, since it blocks submodels from communicating
;; with each other.

;; This routine does the running since "run" has fixed up the ticks
;; It looks like (run-model-body me t dt) in code

;; Generally, model-body methods know about "self" "t" "dt" "kernel"
;; and all an agents state variables.  The variable all-parent-bodies
;; is a list of the "model-body" methods for all of its parents,
;; rather than just the ones that appear first in the inheritance
;; lists.  This *particular* version of the routine should not call
;; parent-body.



;; *** AGENTS RUN HERE ***
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
											  (begin
												 (run-model-body self t dt) ;;; The model runs before its subsidiaries or components
											  
												 ;;  The model returns the amount of time it actually ran for
												 (kdnl* '(nesting run-model-body) (class-name-of self)
														  "Running at " t "+" ldt "[" (my 'dt) "]")
												 (if (< dt 0.0) (error 'bad-dt))
												 (begin
													(kdnl* 'track-subjective-times
															 "[" (my 'name) ":" (class-name-of self) "]"
															 " running at " (my 'subjective-time) ":" t)
													
													(if (pair? (my 'active-subsidiary-agents))       ;;; Any agent can act as a kernel
														 (run-subsidiary-agents self t dt run kernel))
													
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

													;; deal with any changes in the entity's
													;; representation, or the general configuration of the
													;; model as a whole

													;; prefix a symbol ('migrate, for
													;; example) to the return value if it needs to change,
													;; last bit should be "return"
													
													(let ((mtrb ((my 'migration-test) self t dt)))
													  ;; we don't want to make calls to a closure that has vanished
													  (if blue-meanie (set-my! 'kernel #f))  
													  
													  (if mtrb
															(if (pair? return)
																 (cons mtrb return)
																 (cons mtrb (list dt)))
															dt))
													)
											  dt))))
								(if (not (number? m)) (error "Did not get a numeric return from run-model-body"  m))

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

				

(definition-comment 'run-agents
  "is called by run-subsidiary-agents and is used as a proxy for the call to queue;"
  "this may occur when a habitat takes over patches, for example")
(model-method (<agent>) (run-agents self t dt agentlist run kernel)
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
										(run x t (+ t dt) kernel)
										(kdnl* 'info "run-agents: skipping" (name x)))
								  )
								agentlist
								)
				  ;; Do I need a dead-agent class and "clean dead agents" ?
				  )

(definition-comment 'run-subsidiary-agents
  "run-subsidiary-agents is used to run agents in a nested queue, such as when a habitat takes over patches.
the nested agents will typically always run either before or after the nesting agent (as determined in the
containing agent's model-body), and they ALWAYS IN THE SAME ORDER.
This consistent order means that, should they interact, there may be processing artifacts
that 'look alright' but are not.  On the other side of the coin, it means that they can be
loaded to optimise some sorts of processes, and tooled to avoid those artifacts.  YMMV")

(model-method (<agent>) (run-subsidiary-agents self t dt run kernel )
				  (let ((al (my 'active-subsidiary-agents)))
					 (if (not (null? al))
						  (run-agents self t dt al run kernel))
					 ))


;---- <blackboard> methods

;;; (agent-initialisation-method <blackboard> (args) (no-default-values)
;;; 				  (kdnl* '(track-init) "<blackboard> initialise---")
;;; 				  ;;(pp args)
;;; 				  ;;(dnl "variable")
;;; 				  (set-state-variables ;; We set some reasonable default values for
;;; 					;; some of the slots
;;; 					self (list 'message-list '()
;;; 								  'label ""
;;; 								  ))
;;; 				  (initialise-parent)
;;; 				  ;; call "parents" last to make the initialisation list work
;;; 				  (set-state-variables self args) ;; we now set-state-variables the slot values passed in args
;;; 				  )

				
(model-method (<blackboard> <symbol>) (query self tag #!rest args)
				  ;; args should either be a list of tags (for 'erase and 'read)
				  ;; or a list of pairs (for 'write)
						

				  (let* ((messages (my 'message-list))
							(result
							 (map (lambda (x)
									  (case tag
										 (('erase)
										  (set-my! self 'message-list (assq-delete messages x))
										  )
										 (('read)
										  (assq (my 'message-list) x)
										  )
										 (('write)
										  (if (not (pair? x))
												(error "Missing value specified for <blackboard> 'write" args))
										  (set-my! 'message-list (assq-set! (my 'message-list) (car x) (cadr x)))
										  )
										 ))
									args)))
					 (case tag
						((read) result)
						((erase write) (my 'message-list))
						(else (kquery self tag args)))))


(model-body <blackboard>
				(parent-body)
				dt
				)


;---- <tracked-agent> methods

;;; (default-agent-initialisation <tracked-agent>
;;;   'track #f
;;;   'tracked-paths #f
;;;   'track-schedule '()
;;;   'track-epsilon 1e-6)

;;; (agent-initialisation-method <tracked-agent> () (
;;;   'track #f
;;;   'tracked-paths #f
;;;   'track-schedule '()
;;;   'track-epsilon 1e-6))


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

;;; (default-agent-initialisation <thing> 'dim #f 'location #f
;;;   'direction #f
;;;   'speed #f 'mass #f
;;;   'track #f
;;;   'tracked-paths #f)

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


;----- (distance loc) 
(model-method 
 (<thing>)
 (distance self loc)
 (let* ((d (map - (slot-ref self 'location) loc))
		  (d2 (map * d d)))
	(sqrt (apply + d2))))
		  


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

(define (environment-initfunc self)
  (set-state-variables self
							  'minv '(-inf.0 -inf.0 -inf.0)
							  'maxv '(+inf.0 +inf.0 +inf.0)
							  'split-flexibly #f
							  'split-at 12 ;; when a bottom node gets more than 12 elements, convert it into an intermediate with four other nodes
							  'location-tree  (list (list-head minv 2) (list-head maxv 2)) ;; we only do it in 2d ... ;-)
  ))

;; A node in a location tree is either a list of four lists, of the form
;;    (mincorner maxcorner list-of-entities)
;; or
;;    (mincorner maxcorner node node node node)
;;
;; The length of the list indicates whether it is a "bottom" node (three elements)
;; or an intermediate node (six elements)

(model-method <environment> (min-bound self)
				  (list-copy (my 'minv)))

(model-method <environment> (max-bound self)
				  (list-copy (my 'maxv)))

(UNFINISHED-BUSINESS "This spatial sorting is not finished yet. 
At the moment it seems a little touch and go as to whether the 
long term benefit of maintaining the structure outweighs the
cost of ad hoc queries.")


;; Default environment only has the default value, oddly enough
(model-method (<environment> <pair>) (value self loc)
				  (my 'default-value))

(model-method (<environment> <pair>) (set-value! self loc val)
				  (set-my! 'default-value val))

;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
