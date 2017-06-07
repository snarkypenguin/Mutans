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

;; cpl... class precedence list
;;; (define-macro (isa? self . classtype)
;;;   (letrec ((orf (lambda x (if (null? x #f) (or (car x) (apply orf (cdr x)))))))
;;; 	 (let ((txt `(let ((ancestors (class-cpl (class-of ,self))))
;;; 						(if (apply orf
;;;  									  (map (lambda (x) (member x ancestors))
;;;  											 (list ,@classtype))) 
;;;  							 #t #f)
;;;  						)
;;;  					))
;;;  		;;(pp txt) ;; Uncomment to print the constructed code during the startup phase 
;;;  		txt)))


(model-method (<agent>) (pp-a self)
				  (dnl* "(" (cnc self) (name self) (slot-ref self 'note))
				  (dnl* "  priority:" (slot-ref self 'priority) "initialised:" (slot-ref self 'initialised))
				  (dnl* "  subjective-time:" (slot-ref self 'subjective-time) "dt:" dt)
				  )

(define (isa? me #!rest classes)
  (let loop ((cpl (class-cpl (class-of me)))
				 (cllist classes))
	 (cond
	  ((or (null? cpl) (null? cllist)) #f)
	  ((memv (car cllist) cpl) #t)
	  (#t (loop cpl (cdr cllist))))))


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


(define (dump whatsit)
  (let* ((C (if (class? whatsit) whatsit (class-of whatsit)))
			)
	 (display (cnc C))
	 (display " ")
	 (pp (dumpslots C))
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
				;;(call-parents)
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

The default list of projections and the following functions (amongst
others) are defined in framework.scm:

(composite-prj_src->dst self src dest)
-- returns a projection that applies dest after src

(linear2d:model-space->output-space m-domain o-domain)
-- makes a linear mapping function, each of the args is a rectangular 
bounding box (ll ur). This mapping fits the o-domain by contraction.

(define (*linear2d:model-space->output-space m-domain o-domain)
  -- makes a *linear mapping function, each of the args is a rectangular 
  bounding box (ll ur). This mapping fits the o-domain by using 
  different scales for the axes

  (define (map:domain-to-postscript model-domain papersize margin #!rest use-*linear-map)
	 -- Defaults to a regular scale.
	 -- margin will be in mm, if margin is a pair, the car is side
	 -- margins and the cadr is the top and bottom length

	 "

(model-method (<projection> <procedure>) (ps-dump self ps projection) 
				  (dnl "In projection ps-dump")
				  (ps 'show-table
						(list "<projection>")))



(model-method <projection> (projection-assoc-list self key)
				  (let ((p (assoc (my 'projection-assoc-list) key)))
					 (if p (cdr p) #f)))

(model-method (<projection> <procedure>) (add-projection! self p k)
				  (set-my! 'projection-assoc-list
							  (cons (cons k p)
									  (filter (lambda (x) (not (eq? (car x) k)))
												 (my 'projection-assoc-list)))))

(model-method (<projection> <symbol> <procedure>) (add-projection! self key p)
				  (let ((q (assoc-delete (my 'projection-assoc-list) key p)))
					 (set! q (if (null? q)
									 (list (con key p))
									 (cons (cons key p) q)))
					 ))


(model-method (<projection> <symbol> <procedure>) (remove-projection! self key p)
				  (set-my! 'projection-assoc-list (assoc-delete (my 'projection-assoc-list) key p)))



;-- projection routines that actually map things into different spaces

(model-method (<projection> <projection> <projection>) (composite-prj_src->dst self src dest)
;  (dnl* (cnc submodel1) (cnc submodel2))

				  (let ((l->m (get-local->model src))
						  (m->l (get-model->local dest))
						  )
					 (lambda (x) 
						(m->l (l->m x)))))

(model-method (<projection> <projection>) (composite-prj_src->dst self dest)
;  (dnl* (cnc submodel1) (cnc submodel2))

				  (let ((l->m (get-local->model self))
						  (m->l (get-model->local dest))
						  )
					 (lambda (x) 
						(m->l (l->m x)))))

(define (local->model self location)
  (let ((p (slot-ref self 'local->model)))
	 (if (and (pair? p) (procedure? (cdr p)))
		  (begin
			 (slot-set! self 'local->model (cdr p))
			 (model->local self location))
		  (begin
			 (cond
			  ((symbol? p)
				(set! p (get-projection self p))
				(slot-set! self 'local->model p))
			  ((and (pair? p) (eq? (car p) 'local->model))
				(set! p (cdr p)))
			  ((not (procedure? p))
				(error "Something very bad has happened in the local->model projection code" p)))
			 
			 (if p
				  (p location)
				  location)))))

(define (model->local self location)
  (let ((p (slot-ref self 'model->local)))
	 (if (and (pair? p) (procedure? (cdr p)))
		  (begin
			 (slot-set! self 'model->local (cdr p))
			 (model->local self location))
		  (begin
			 (cond
			  ((symbol? p)
				(set! p (get-projection self p))
				(slot-set! self 'model->local p))
			  ((and (pair? p) (eq? (car p) 'model->local))
				(set! p (cdr p)))
			  ((not (procedure? p))
				(error "Something very bad has happened in the model->local projection code" p)))
			 
			 (if p (p location)
				  location)))))


;-- setters and getters for the inverse projection (into the submodel's space)

;-- get the projection that goes from model space to local space
(define (get-model->local self)
  (let ((p (slot-ref self 'model->local)))
	 
	 (if (and (pair? p)
				 ;;(eq? (car p) 'model->local)
				 )
		  (set! p (cdr p)))
	 (if (not (procedure? p))
		  (begin
			 (set! p (get-projection self p))
			 (slot-set! self 'model->local p)))
	 p)
  )

;-- get the projection that goes from local space to model space
(define (get-local->model self)
  (let ((p (slot-ref self 'local->model)))
	 (if (and (pair? p)
				 ;;(eq? (car p) 'local->model)
				 )
		  (set! p (cdr p)))
	 (if (not (procedure? p))
		  (begin
			 (set! p (get-projection self p))
			 (slot-set! self 'local->model p)))
	 p)
  )

;-- get an arbitrary projection
(define (get-projection self key)
  (if (not (symbol? key))
		(error "projections are keyed by symbols" key)
		(let* ((pal (slot-ref self 'projection-assoc-list))
				 (projections*
				  (if (or (not pal) (uninitialised? pal))
						(let ((lc (list-copy *default-projections*)))
						  (slot-set! self 'projection-assoc-list lc)
						  (set! pal lc)
						  lc)
						pal))
				 )
		  (let ((p (assoc key projections*)))
			 (if (and (pair? p) (procedure? (cdr p))) (cdr p) (mapf (lambda (x) (dnl "missing projection: " key) x) ))))
		)
  )


;-- set the projection that goes from model space to local space
(define (set-model->local! self key)
  (let ((p (assoc (slot-ref self 'projection-assoc-list))))
	 (if (and p (procedure? (cdr p)))
		  (slot-set! self 'model->local (cdr p))
		  (error "Projection not found in projection-assoc-list" key))))

;-- set the projection that goes from local space to model space
(define (set-local->model! self key)
  (let ((p (assoc (slot-ref self 'projection-assoc-list))))
	 (if (and p (procedure? (cdr p)))
		  (slot-set! self 'local->model (cdr p))
		  (error "Projection not found in projection-assoc-list" key))))

;--- kernel Methods for <agent> classes

(model-method (<agent> <procedure>) (ps-dump self ps projection)
				  (dnl "in agent ps-dump")
				  #t
				  )

;---- <query> -- seems to not work at all 

;;; ;; A query to an agent must take a completely arbitrary list of arguments.  If the agent is unable to
;;; ;; recognise the query it returns (void)
;;; (model-method <agent> (query self tag #!rest args)
;;; 				  (case tag
;;; 					 ((value) (if (pair? args) (slot-ref self (car args))))
;;; 					 (else (kquery self kernel tag args))
;;; 				  ))


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
										  (slot-ref self 'name)))
						))


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
;;; 				  (kdebug '(track-init) "<agent> initialise---")
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


(model-method (<agent>) (initialisation-checks self)
				  (void)
				  )
				  
						



(model-method (<thing> <procedure>) (ps-dump self ps projection)
				  (dnl "in thing ps-dump")
				  (ps 'moveto (projection (local->model (location self))))
				  (ps 'show-table
						(list (string-append "<thing>" (name self))
								(string-append "mass =" (number->string (my 'mass)))
								(string-append "location =" (number->string (my 'location)))
								(string-append "speed =" (number->string (my 'speed)))
								(string-append "direction =" (number->string (my 'direction)))
								))
				  (ps-dump-parent))

(model-method <agent> (kernel-check self #!rest args)
				  ((slot-ref self 'kernel) 'check self args))



(model-method <agent> (provides self)
				  (list-copy (append (list (cnc self) (my 'taxon))
											(let ((p (slot-ref self 'provides)))
											  (cond
												((uninitialised? p) '())
												((list? p) p)
												(#t (list p)))))))

(model-method <agent> (requires self)
				  (list-copy (slot-ref self 'requires)))

(model-method <agent> (provides? self args)
				  (if (not (pair? args)) (set! args (list args)))
				  (list-intersection args (provides self)))

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
				  (kdebug 'prep (slot-ref self 'name) "entered prep: " start end)
				  (if (slot-ref-self 'timestep-schedule)
						(slot-set! self 'timestep-schedule
									  (unique (sort (slot-ref self 'timestep-schedule) <)))
						(slot-set! self 'timestep-schedule '()))

				  ;; ensures no duplicate entries
				  (if (eq? (slot-ref self 'agent-state) 'ready-for-prep)
						(slot-set! self 'agent-state 'ready-to-run)
						(error (string-append
								  (name self)
								  " has been instructed to prep but it's state is "
								  (slot-ref self 'agent-state)))
						))


;; Termination can happen from any state
(model-method <agent> (agent-shutdown self #!rest args) 
				  (slot-set! self 'agent-state 'terminated))


;; Check and adjust agent states
(model-method <agent> (agent-state self)
				  (my 'agent-state))

(model-method <agent> (set-agent-state! self newstate)
				  (set-my! 'agent-state newstate))


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


(model-method (<agent> <log-introspection> <symbol> <list>) (log-data self logger format targets)
				  (kdebug '(log-* log-data)
							 (name self)
							 "[" (my 'name) ":"
							 (cnc self) "]" "in <agent>:log-data")
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
							(kdebug '(log-* log-data logging-debug)      
									  "  " (name self) (cnc self)
									  "Dumping " field "=" (if (has-slot? self field)
																		(slot-ref self field)
																		"missing!"))
							(if (not spaced-out)
								 (set! spaced-out #t)
								 (display " " file))
							(display (slot-ref self field) file)
							)
						  ((member field (extra-variable-list self))
							(kdebug '(log-* log-data logging-debug)
									  "  " (name self) (cnc self)
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
				  (let ((n (slot-ref self 'name)))
					 n
					 ))

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

;----- (taxon) 

(model-method (<agent>) (taxon self)
				  (let ((n (slot-ref self 'taxon)))
					 n
					 ))

;(model-method <agent> (taxon self)
;				  (my 'taxon))

;; (add-method taxon
;; 				(make-method (list <agent>)
;; 								 (lambda (taxon-parent self)
;; 									(slot-ref self 'taxon))))

;----- (set-taxon!) 

(model-method (<agent> <string>) (set-taxon! self n)
				  (if (string? n)
						(set-my! self 'taxon n)
						(error "agent:set-taxon! -- arg is not a string")))



(define undefined (lambda x 'undefined))
(define undefined-state-flag (lambda x 'undefined-state-flag))

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
					 (set-my! 'timestep-schedule (sort tq <))))

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
  (kdebug 'timestep-schedule "prune-local-time-queue enters with: " tm "--" ttr)
  (if (not ttr)
		(set! ttr '())
		(if (uninitialised? ttr) (set! ttr (list 0)))
		)
  (let ((r (filter (lambda (x) (> x tm)) ttr)))
	 (kdebug 'timestep-schedule "prune-local-time-queue leaves with: " tm "--" ttr)
	 r)
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

;; if it ever hits the agent body, it just returns the dt passed in.
(add-method run-model-body (make-method (list <agent> <number> <number>) (lambda (run-parent self T dt) dt))) 

;; *** AGENTS RUN HERE ***
(add-method
 run
 (make-method
  (list <agent>)
  (lambda (run-parent self T pstop pkernel)
	 (kdebug 'bigseparatorfor-run "###################################################################################")
	 (kdebug 'bigseparatorfor-run "## Running"  (name self) "in " T pstop "with" (slot-ref self 'timestep-schedule) "pending\n")
	 (kdebug 'bigseparatorfor-run "##       @"  (subjective-time self) "+" (slot-ref self 'dt))
	 (let ((my (lambda (x) (slot-ref self x)))
			 (set-my! (lambda (x y) (slot-set! self x y)))
			 (kernel (slot-ref self 'kernel)))

		;;		(if (has-slot? self 'introspection-targets)
		;;			 (slot-set! self 'introspection-targets (pkernel 'find-agents (slot-ref self 'introspection-selector))))

		;;(set-kernel! self pkernel)
		(kdebug 'run-model-body "<agent>" (cnc self))
		(kdebug (list 'run (slot-ref self 'name) (slot-ref self 'taxon)) "About to dispatch control to model body")
		;;(dnl* (list 'run (slot-ref self 'name) (slot-ref self 'taxon)) "About to dispatch control to model body")
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
						(if (< (+ t dt) subj-time)
							 (begin
								(dnl* "Trying to go back in time" (cnc (class-of self)) (slot-ref self 'name) (slot-ref self 'taxon) '@ t "+" dt "with a subjective time of" subj-time)
								(kdebug (list 'run (slot-ref self 'name) (slot-ref self 'taxon)) "... running from" t "to" (+ t dt) ", a step of" dt)
								'ok ;; report it and just skip this attempt
							 ))

						(let ((DT 0))
						  (set-my! 'kernel kernel)
						  (if (not (= t subj-time))
								(begin
								  (dnl* "Adjusting dt" (cnc (class-of self)) (slot-ref self 'name) 'st subj-time 'T t DT dt)
								  (set! dt (- (+ t dt) subj-time))))
						  (cond
							((< subj-time t)
							 (if temporal-fascist
								  (begin
									 (kdebug 'temporal-check "[" (my 'name) ":"(cnc self) "]"
												"a/an" (my 'representation)
												"is lost in time at" subj-time "or" t)
									 'missing-time)
								  ((letrec
										 ((loop-through-time
											(lambda (st ddt)
											  (kdebug 'passing-control-to-model
														 "["(my 'name)":"(cnc self)"]"
														 "Passing control to the model at" t 
														 "for" (if (< st t)
																	  ddt
																	  (- (+ t dt) subj-time)))
											  ;;*** Control passes to run-model-body here ***
											  (let ((m (run-model-body
															self
															subj-time
															(if (< st t)
																 ddt
																 (- (+ t dt)
																	 subj-time)))))
												 ;;*** Deal with the returned value appropriately ***
												 (cond
												  ((eqv? m #!void)
													(error (string-append
															  "The model body for "
															  (cnc
																self)
															  " returned an error: #!void")))
												  ((eqv? dt #!void)
													(error (string-append
															  "dt for "
															  (cnc
																self)
															  " is somehow #!void (error)")))
												  ((eqv? DT #!void)
													(error (string-append
															  "DT for "
															  (cnc
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
													 (kdebug 'run-trap "BORK!!!" m))
													(#t (kdebug 'run-trap "BORK!!!" m)))
												  (#t #!void))

												 m))
											))
									  loop-through-time)
									subj-time
									(min (- t subj-time) (my 'dt)))))
							((and (> dt 0.) (>= subj-time (+ t dt)))
							 (kdebug 'temporal-check "["
										(my 'name)
										":"
										(cnc self)
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
							 (kdebug 'passing-control-to-model
										"["
										(my 'name)
										":"
										(cnc self)
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
												 (kdebug '(nesting run-model-body) (cnc self)
															"Running at " t "+" dt "[" (my 'dt) "]")
												 (if (< dt 0.0) (error 'bad-dt))
												 (begin
													(kdebug 'track-subjective-times
															  "[" (my 'name) ":" (cnc self) "]"
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

						  (kdebug 'trace-time-update "SUBJECTIVE TIME FOR" (name self) "IS" (slot-ref self 'subjective-time) "; t =" t "DT =" DT "; dt =" dt)
						  (kdebug 'trace-time-update "has-slot? subjective-time" (has-slot? self 'subjective-time))


						  (if (has-slot? self 'subjective-time) ;; Anything (<agent> or otherwise) should have its time updated
								(begin
								  (kdebug 'trace-time-update "UPDATING...")
								  (slot-set!
									self 'subjective-time
									(+ DT (my 'subjective-time)))
								  (kdebug 'trace-time-update "..." (slot-ref self 'subjective-time))
								  ))

						  (slot-set! self 'timestep-schedule (sort (cons (+ t DT) (slot-ref self 'timestep-schedule)) <))

						  (kdebug '(nesting run)
									 (cnc self)
									 (name self)
									 "Leaving run after a tick of "
									 DT
									 " @ "
									 (my 'subjective-time)
									 "["
									 (my 'dt)
									 "]")
						  'ok)))))))
		))
  ))



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
								  (if (< (slot-ref x 'subjective-time) (+ t dt))
										(run x t (+ t dt) kernel)
										(kdebug 'info "run-agents: skipping" (name x)))
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
;;; 				  (kdebug '(track-init) "<blackboard> initialise---")
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


(model-method (<blackboard> <symbol>) (query self cmd #!rest args)
				  ;; args should either be a list of cmds (for 'erase and 'read)
				  ;; or a list of pairs (for 'write)
				  

				  (let* ((messages (my 'message-list))
							(result
							 (map (lambda (x)
									  (case cmd
										 (('erase)
										  (set-my! self 'message-list (assq-delete messages x))
										  )
										 (('read)
										  (assq (my 'message-list) x)
										  )
										 (('append)
										  (if (not (pair? x))
												(error "Missing value specified for <blackboard> 'write" args))
										  (set-my! 'message-list (assq-append (my 'message-list) (car x) (cadr x))))
										 (('write)
										  (if (not (pair? x))
												(error "Missing value specified for <blackboard> 'write" args))

										  (if (null? (my 'message-list))
												(set-my! 'message-list (assoc-append '()  (car x) (cadr x)))
												(assq-set! (my 'message-list) (car x) (cadr x)))
										  )
										 ))
									args)))
					 (case cmd
						((read) result)
						((erase write) (my 'message-list))
						(else (kquery self cmd args)))))


(model-body <blackboard>
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


;; This records the track in the native ordinate space of the submodel!
(model-method (<tracked-agent> <number> <pair>) (track-location! self t loc)
				  (let ((myt (my 'track)))
					 (if (and myt (not (list? myt)))
						  (set! myt '()))

					 (set-my! 'track
								 (if t
									  (append myt (list (cons t loc)))
									  (list (cons t loc))
									  )
								 ))
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
				(track-location! self t (my 'location)) ;; even if they
				;; aren't
				;; moving
				(call-parents)
				dt
				)




;---- <thing> methods

;;; (default-agent-initialisation <thing> 'dim #f 'location #f
;;;   'direction #f
;;;   'speed #f 'mass #f
;;;   'track #f
;;;   'tracked-paths #f)

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
;----- (mass) 
(model-method
 (<thing>)
 (mass self)
 (slot-ref self 'mass))


;----- (set-mass!) 
(model-method
 (<thing> <number>)
 (set-mass! self n)
 (if (or (not (number? n)) (negative? n))
	  (error "thing:set-mass! -- bad number")
	  (slot-set! self 'mass n)))

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
 (slot-set! self 'location vec))
 

;----- (direction) 
(model-method 
 (<thing>)
 (direction self)
 (slot-ref self 'direction))


;----- (set-direction!) 
(model-method  (<thing> <list>) (set-direction! self vec)
   (if (pair? vec) 
		 (let ((v (sqrt (apply + (map sqr vec)))))
			(if (positive? v)
				 (slot-set! self 'direction (map (lambda (x) (/ x v)) vec))))))

; Vectors should have a length of one.  We do not try and set a direction of "zero"





;---- environment methods

(model-body <environment> ;; does nothing.
				)
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
