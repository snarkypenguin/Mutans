(include "remodel-framework")
;- Identification and Changes

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
;- Load initial libraries 

;; these must be loaded before this is
;;(include
;;(load "maths.scm")
;;(load "integrate.scm")


;;-------------------------------------------;;
;; This is the order things must happen in   ;;
;; any file defining methods or model bodies ;;
;;---------------------------------------------
;; (include "%remodel.scm")
;; (load-model-remodel)
;;---------------------------------------------


;;---------------------------------------------------
;; Important routines which I Really Ought to Know ;;
;;---------------------------------------------------

;;; cpl... class precedence list
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


(define restricted-kernel-access #f)

;; restricted-kernel-access blocks access to the kernel when the agent is not
;; currently running.  This is mainly for debugging in restricted
;; subsets of a model, since it blocks submodels from communicating
;; with each other.
;; assesses the current representation of *this* agent

(model-method (<agent>) (number-represented self) 1)


(model-method (<agent> <symbol>) (has-data? self key)
				  (and (agent? self)
						 (has-slot? self key)))

(model-method (<agent> <symbol>) (data-ref self key)
				  (slot-ref self key))

(model-method (<agent> <symbol>) (data-set! self key value)
				  (slot-set! self key value))

;;(define has-data? (make-generic)) ;; this is done by the declare-method at the beginning of the file
(model-method (<agent> <symbol>) (has-data? self sym)
				  (or (and (object? self)
							  (has-slot? self sym))))

(define (my-map-color self #!rest classdepth)
  (if (not classdepth)
		(my-map-color self 0)
		(let ((mapcol (if (has-slot? self 'map-color) (slot-ref self 'map-color) #f))
				(defcol (if (has-slot? self 'default-color) (slot-ref self 'default-color) #f))
				)
		  (if (or (null? classdepth) (zero? classdepth))
				(or mapcol defcol 0)
				(map-saturate* (or mapcol defcol) 0 0.85 ccd))
		  )))

(define (my-contrast-color self)
  (let ((mapcol (if (has-slot? self 'map-contrast-color) (slot-ref self 'map-contrast-color) #f))
		  (defcol (if (has-slot? self 'default-color) (slot-ref self 'default-color) #f))
		  )
	 (or mapcol defcol 0)))

(define (my-default-color self)
  (let ((defcol (if (has-slot? self 'default-color) (slot-ref self 'map-color) #f))
		  )
	 (or defcol 0)))

(define (isa? me #!rest classes)
  (let loop ((cpl (class-cpl (class-of me)))
				 (cllist classes))
	 (cond
	  ((or (null? cpl) (null? cllist)) #f)
	  ((memv (car cllist) cpl) #t)
	  (#t (loop cpl (cdr cllist))))))

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
  (let ((deref (lambda (x)
					  (cond
						((and (list? x) (apply andf (map number? x))) x)
						((and (isa? x <object>) (has-slot? x 'location)) (slot-ref x 'location))
						(else (error bad-argument-to:vector-to))))))
	 (map - (deref dst) (deref src))))

(define (do-map-conversion pfn gfn)
  (let ((cmd (string-append "gs -q -dQUIET -dNOPAUSE -r300x300 -sDEVICE=pnggray -sOutputFile=" gfn " - " pfn " < /dev/null &")))
	 (shell-command cmd)))

(define (arg-pairings symlist objlist)
  (if (> (length objlist) (length symlist))
		(error "There are unpaired objects!\n" symlist objlist))
  
  (let ((n (min (length symlist) (length objlist))))
	 (apply append (map list (list-head symlist n) (list-head objlist n)))))





;----- (dump) ;; This dumps all the slots from agent up.  

;(model-method <agent> (dump% self maxtick)
;				  (dump% self 0))

(model-method (<object>) (dump% self spaces) ;; spaces indecates the indentation
				  (set! count (cond
									((number? count) spaces)
									((not (pair? spaces)) 0)
									(#t (car count))))
				  (let* ((slots (class-slots-of self))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y)
									 (display (make-string spaces #\space))
									 (display "[")
									 (display x) (display ": ")
									 (display y)(display "]")(newline))
								  slots vals)))

(model-method (<agent>) (has-many-loci? self)
				  #f)



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





;--- Agent classes
"We have the agent initialisation here, rather than in
sclos+extras.scm, for a few reasons.  First, it is much more complex
and people may need to look at it to see what is happening.  Second,
<object>s are really not much more than data structures with
specialised methods.  <agent>s are much more complex, and might not be
commonly viewed as just a simple extension of sclos.
"
;---- <agent> methods
;----- (initialise-instance) 

(model-method (<agent>) (supports-proxy? self)
				  #f)

(model-method (<agent>) (dead? self)
				  (eq? (my 'agent-state) 'dead)
				  )


(model-method (<agent>) (terminate self)
				  (set-my! 'agent-state 'terminated)
				  (set-my! 'queue-state 'terminated))

(model-method (<agent>) (terminated? self)
				  (or (eq? (my 'agent-state) 'terminated)
						(eq? (my 'queue-state) 'terminated)))


(model-method (<agent>) (initialise-instance self)
				  (void)
				  )

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

;; Check and adjust agent states
(model-method <agent> (queue-state self)
				  (my 'queue-state))

(model-method <agent> (set-queue-state! self newstate)
				  (set-my! 'queue-state newstate))

(model-method <agent> (agent-state self)
				  (my 'agent-state))

(model-method <agent> (set-agent-state! self newstate)
				  (set-my! 'agent-state newstate))

;(model-method <agent> (agent-prep self start end)
(model-method (<agent> <number> <number>) (agent-prep self start end)
				  (kdebug 'prep (slot-ref self 'name) "entered prep: " start end)
				  (if (slot-ref self 'timestep-schedule)
						(slot-set! self 'timestep-schedule
									  (unique (sort (slot-ref self 'timestep-schedule) <)))
						(slot-set! self 'timestep-schedule '()))

				  ;; ensures no duplicate entries
				  (if (eq? (slot-ref self 'agent-state) 'ready-for-prep)
						(begin
						  (slot-set! self 'agent-state 'active)
						  (slot-set! self 'subjective-time start)
						  (initialise-instance self))
						(error (string-append
								  (name self)
								  " has been instructed to prep but it's state is "
								  (slot-ref self 'agent-state)))
						)
				  (slot-set! self 'queue-state 'ready-to-run)
				  #t)


;----- Termination can happen from any state  */

;; Termination can happen from any state
(model-method <agent> (agent-shutdown self #!rest args) 
				  (slot-set! self 'queue-state 'terminated)
				  ;;(slot-set! self 'agent-state 'terminated)
				  )


;;  a few output routines

(model-method (<agent>) (pp-a self)
				  (dnl* "(" (cnc self) (name self) (slot-ref self 'note))
				  (dnl* "  priority:" (slot-ref self 'priority) "initialised:" (slot-ref self 'initialised))
				  (dnl* "  subjective-time:" (slot-ref self 'subjective-time) "dt:" dt)
				  )




(model-method (<agent> <log-data> <symbol> <list>) (log-data self logger format targets)
				  (dnl* "(model-method (<agent> <log-data> <symbol> <list>) (log-data self logger format targets)")
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
					 (if (output-port? file) (error))
					 
					 (cond
					  ((not (member format '(ps postscript map gif png mpg)))
						(for-each ;; field in the variable list
						 (lambda (field)
							(if show-field-name
								 (begin
									(if (not spaced-out)
										 (set! spaced-out #t)
										 (file 'display " "))
									(file 'display field)))
							
							(cond
							 ((has-slot? self field)
							  (kdebug '(log-* log-data logging-debug)      
										 "  " (name self) (cnc self)
										 "Dumping " field "=" (if (has-slot? self field)
																		  (slot-ref self field)
																		  "missing!"))
							  (if (not spaced-out)
									(set! spaced-out #t)
									(file 'display " "))
							  (file 'display (slot-ref self field))
							  )
							 ((member field (extra-variable-list self))
							  (kdebug '(log-* log-data logging-debug)
										 "  " (name self) (cnc self)
										 "Dumping extra " field "="
										 (extra-variable self field))
							  (if (not spaced-out)
									(set! spaced-out #t)
									(file 'display " "))
							  (file 'display (extra-variable self field))
							  )
							 (missing-val
							  (if (not spaced-out)
									(set! spaced-out #t)
									(file 'display " "))
							  (file 'display missing-val))
							 (else
							  (if (not spaced-out)
									(set! spaced-out #t)
									(file 'display " "))
							  )	
							 )
							)
						 (unique (if #t targets
										 (filter (not-member (my 'dont-log)) targets))))
						(file 'newline))
					  ((member format '(ps))
						#t)
					  )
					 )
				  )
;----- (s/n% self)
(model-method (<agent>) (s/n% self)
				  (if #t
						(string-append  (name self) "/"(symbol->string (representation self))"/" (number->string (my 's/n)))
						(string-append  (name self) "/"(cnc self)"/" (number->string (my 's/n)))
				  ))


;----- (name) 

(model-method (<agent>) (name self)
				  (let ((n (slot-ref self 'name)))
					 n
					 ))

;(model-method <agent> (name self)
;				  (my 'name))

;; (add-method name
;; 				(make-method (list <agent>)
;; 								 (lambda (parent-name self)
;; 									(slot-ref self 'name))))

;----- (set-name!) 

(model-method (<agent> <string>) (set-name! self n)
				  (if (string? n)
						(set-my! self 'name n)
						(error "agent:set-name! -- arg is not a string")))

;----- (taxon) 

(model-method (<agent>) (taxon self)
				  (if (has-slot? self 'taxon)
						(let ((n (slot-ref self 'taxon)))
						  n
						  )
						(cnc self)
						)
				  )

;(model-method <agent> (taxon self)
;				  (my 'taxon))

;; (add-method taxon
;; 				(make-method (list <agent>)
;; 								 (lambda (parent-taxon self)
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



(model-method <agent> (change-taxon self taxon)
				  (apply-parameters self (slot-ref self 'taxon)))

;(include "heritability")

(model-method <agent> (change-taxon self taxon)
				  (apply-parameters self (slot-ref self 'taxon)))


;; This is called with a class, a taxon symbol, and a list of initialisation pairs symbol value ...
;; and an optional, final init function that expects "self"



(model-method <agent> (modal-dt self #!optional prospective-dt)
				  (if (number? prospective-dt)
						prospective-dt
						(my 'dt))) ;; if not modal use default


;; This is an example of code to make accessors.  The general principle is that an accessor
;; is a function that only do one thing, and that the accessor maker doesn't *do* anything.
;; Using accessors means that when they go out of scope the ability to alter another's state
;; is automatically lost, that *only* the allowed changeds are possible, and that we can
;; enforce controls on what is granted access.

;; [EXAMPLE of constructing an accessor function]
(model-method (<agent> <agent> <symbol>) (request-accessor self intruder sym #!rest args)
				  (case sym
					 ((set!) ;; This shows how to restrict the ability to make a setter to members of  
					  (if (not (eq? (class-of self) (class-of intruder))) ;; the same class, for example
							(error)
							(let ((slt (car args)))
							  (lambda (val) (slot-set! self slt val)))))
					 ((ref)
					  (if (null? args) (error 'bad-args))
					  (let ((slt (car args)))
						 (lambda () (slot-ref self slt))))
					 ((adjust#)
					  (if (null? args) (error 'bad-args))
					  (let ((slt (car args)))
						 (lambda (val) (slot-set! self slt (+ (slot-ref self slt) val))))
					  )
					 )
				  )





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
				;;(call-all-parents)
				(let ((status-list (map (lambda (kernel t dt maint-routine)
												  (maint-routine kernel t dt))
												(slot-ref self 'maintenance-list)
												)))
				  ;; Now do something with  the status-list!
				  'ok))




; none yet

;--- Methods for <object> classes

;- Utility functions and data

;-- default projections ... mostly used by loggers.
"These function should be able to take either ordinates (if it merely 
straightforward scaling) or vectors (for more complex mappings, like LL->xy).

The default list of projections and the following functions (amongst
others) are defined in remodel.scm:

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

(model-method <agent> (location-needs-location@ self)
				  #f)

(define (location self #!rest args)
  ;; index
  ;; list
  ;; symbol
  ;; location radius
  ;; radius
  ;; polygon
  
  (if (location-needs-location@ self)
		(let ((p (apply location@ (cons self args))))
		  (if (not (or (point? p) (point-list? p)))
				(error 'bad-location-call:failed-location@)
				p))
		(if (has-many-loci? self)
			 (cond ;; many loci possible
			  ((null? args) (data-ref self 'location)) ;; return a list of all loci
			  ((number? (car args)) (data-ref self 'location (car args))) ;; return the location of the ith sub-entity 
			  ((and (point? (car args)) (pair? (cdr args)) (number? (cadr args)))
				(let ((l (car args))
						(r (cadr args)))
				  (filter (lambda (p) (distance p l) r) (location self))
				  )) ;; return all the loci that are within (cadr args) of the point
			  ((and (point? (car args)) (null? (cdr args)))
				
				);; return the point closest to the nominated location
			  ((and (list? (car args)) (apply andf (map number? (car args))))
				(map (lambda (x) (location self x)) (car args)))
			  (else (error 'bad-location-call:multiple-loci args))
			  )
			 (cond ;; entity only has a single locus (or none)
			  ((null? args) (data-ref self 'location)) ;; return a list of all loci
			  ;;((number? (car args)) (data-ref self 'location (car args))) ;; return the location of the ith sub-entity 
			  ;;((and (point? (car args)) (pair? (cdr args)) (number? (cadr args)))) ;; return all the loci that are within (cadr args) of the point
			  ;;((and (point? (car args)) (null? (cdr args)))) ;; return the point closest to the nominated location
			  ;;((and (list? (car args)) (apply andf (map number? (car args))))
			  ;; (map (lambda (x) (location self x)) (car args)))
			  (else (error 'bad-location-call:single-locus args))
			  )
			 )
		)
  )

(model-method (<projection> <projection>) (get-location self source)
				  ((composite-prj_src->dst source self) (location source)))

(model-method <plottable> (location* self)
				  (slot-ref self 'location)
				  )

(model-method <plottable> (location2 self)
				  (list-head (slot-ref self 'location) 2)
				  )


(model-method <plottable> (location3 self)
				  (list-head (slot-ref self 'location) 3)
				  '())

(model-method <plottable> (location@ self)
				  (case default-location-type
					 ((*) (location* self))
					 ((2 2d) (location2 self))
					 ((3 3d map) (location3 self))
					 (else (location* self)))
				  #f)



(define glyph-square   '((0.5 0.5) (-0.5 0.5) (-0.5 -0.5) (0.5 -0.5) (0.5 0.5)))
(define glyph-diamond (rotate-glyph (/ pi 4) glyph-square))
(define glyph-+cross '((0 0) (-0.5 0) (0.5 0) (0 0) (0 0.5) (0 -0.5) (0 0)))
(define glyph-xcross (rotate-glyph (/ pi 4) glyph-+cross))
(define glyph-ruler '((0 0) (0 0.15)(0 -0.15) (0 0) (1 0) (1 0.15)(1 -0.15) (1 -0.1) (0 -0.1) (0 0.1) (1 0.1) (1 0) (0 0)))

(model-method (<plottable> <log-map>) (plot-glyph self logger)
				  (let ((glyph (my 'glyph))
						  (dir (my 'direction))
						  (plot-scale (my 'plot-scale))
						  (prj (composite-prj_src->dst self logger))
						  (file (slot-ref logger 'file))
						  (facets 24)
						  (units 1)
						  (G #f)
						  )
					 
					 (cond
					  ((symbol? plot-scale) (set! plot-scale (my (my 'plot-scale))))
					  ((procedure? plot-scale) (set! plot-scale (plot-scale self)))
					  ((number? plot-scale) 'ok)
					  (else (set! plot-scale 1.0)))

					 ;; recognise (ruler unit) spec.
					 (if (and (pair? glyph) (eq? (car glyph) 'ruler) (number? (cadr glyph)))
						  (begin (set! units (cadr glyph)) (set! glyph (car glyph)))
						  )

					 (set! G (cond
								 ((member glyph '(circle square direction +cross xcross ruler)) glyph)
								 ((number? glyph) 'circle)
								 ((or (procedure? glyph) (point-list? glyph)) 'glyph)
								 (else 'circle)))


					 (cond
					  ((procedure? glyph) (set! glyph (glyph self)) 'glyph)
					  ((real? glyph) (set! plot-scale (* plot-scale glyph)) 'circle)
					  ((complex? glyph)
						(set! plot-scale (* plot-scale (real-part glyph)))
						(set! facets (imag-part glyph))
						(set! glyph 'circle)
						'circle
						)
					  (else (set! glyph 'circle)))

					 (set! glyph (cond
									  ((eq? G 'circle) (make-circle-perimeter '(0 0) 1 facets))
									  ((eq? G 'square) glyph-square)
									  ((eq? G 'diamond) glyph-diamond)
									  ((eq? G '+cross) glyph-+cross)
									  ((eq? G 'xcross) glyph-xcross)
									  ((eq? G 'ruler) (scale-pointlist units glyph-ruler))
									  ((point-list? glyph) glyph)
									  (else (make-circle-perimeter '(0 0) 1 6))))
					 
					 (if (or (not (number? plot-scale)) (< (abs (- 1.0 plot-scale)) 0.1))
						  'ignore-scaling
						  (set! glyph (rescale-glyph plot-scale glyph)))
					 
					 ;; calculate rotation to be applied to glyph and then rotate it
					 (set! glyph (rotate-glyph (if (positive? (cadr dir)) (acos (car dir)) (- (acos (car dir))))
														glyph))

					 (set! glyph (translate-glyph (my 'location) glyph))

					 (if (simple-glyph? glyph)
						  (adjusted-plot-polygon file 0.175 (my 'map-color) #t prj glyph)
						  (for-each
							(lambda (g)
							  (adjusted-plot-polygon file 0.1 (my 'map-contrast-color) #f prj g)
							  ) glyph))

					 (if (eq? G 'ruler)
						  (begin
							 (file 'push-color ps-black)
							 (file 'push-font 'Helvetica 7)
							 (file 'linefeed)
							 (if (number? units)
								  (file 'showright (number->string units))
								  (file 'showright  units)
								  )
							 ))
					 
					 )
				  )





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

;; A query to an agent must take a completely arbitrary list of arguments.  If the agent is unable to
;; recognise the query it returns (void)
(model-method (<agent> <agent> <symbol>)(query self questioner tag #!rest args)
				  (cond
					((member? tag (map car (class-slots (class-of self))))
					 (slot-ref self tag))
					(else (kquery self kernel tag args))
					))


;;; ;--- Helper classes (wart classes) 

;;; (object-initialisation-method <object> "this is done in sclos+extra", but somewhat differently
;;;  (initargs) ;; <object> is the most primitive of the remodel classes,  there are no default intitialisation args
;;;  '(no-default-values) 				      ;; and the "make" initialisation args are called initargs
;;;  ;; Body:
;;;  (set-state-variables self initargs) ;; we now set-state-variables the slot values passed in args
;;;  )



;--- Fundamental "run" routine -- accepts anything, but fails if it's inappropriate
;; This routine does the running since "run" has fixed up the ticks
;; It looks like (run-model-body me t dt) in code
;;
;; NOTE: "run" for agents is handled by run-model-body.  
;;
(model-method (<class>) (run self pt pstop pkernel)
				  (if (not (isa? self <agent>))
						(begin (display "Attempt to (run ...) a non-agent\n")
								 (error "+++Curcurbit Error+++"  
										  (slot-ref self 'name)))
						))

(model-method (<proxy>) (run self pt pstop pkernel)
				  (display "Attempt to (run ...) a <proxy>\n")
				  (error "+++Dunderflow Error+++"  
							(slot-ref self 'name)))

(model-method (<object>) (run self pt pstop pkernel)
				  (display "Attempt to (run ...) an <object>\n")
				  (error "+++Auditor Error+++"  
							(slot-ref self 'name)))


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

(model-method (<general-array>)
				  (number-represented self)
				  (length (slot-ref self 'data)))

(model-method% (<general-array> <log> <symbol>) (log-data self logger format targets)
				  (dnl* "(model-method% (<general-array> <log> <symbol>) (log-data self logger format targets)")
				  (let ((kdebug (if #t kdebug dnl*))
						  ;;(f (if (pair? args) (car args) #f))
						  ;;(p (if (and (pair? args)
						  ;;			  (pair? (cdr args)))
						  ;;		(cadr args)
						  ;;		#f))
						  )
					 (kdebug '(log-horrible-screaming array log-array) (cnc self) (cnc format) (cnc (my 'name)))
					 (if (or (my 'always-log) (and (is-class? self <plottable>) (is-class? logger <log-map>)) (emit-and-record-if-absent logger self (my 'subjective-time)))
						  (let ((file (slot-ref logger 'file)))
							 (kdebug '(log-* log-array)
										"[" (my 'name) ":" (cnc self) "]"
										"in log-data")
							 (let ((leading-entry #f))
								(for-each
								 (lambda (field)
									(kdebug '(log-* log-array) "[" (my 'name) ":"
											  (cnc self) "]" "checking" field)
									(if (has-slot? self field)
										 (let ((r (data-ref self field)))

											(case format
											  ((ps)
												(file 'push-font (my 'default-font) (my 'default-size))
												(file 'show (string-append " "
																					(if (string? r)
																						 r
																						 (object->string r)) " "))
												(file 'pop-font)
												)
;								 ((dump)
;								  (with-output-to-port file
;										(lambda ()
;										  (dump self))))

												  
											  ;;; 	  (display S file)))
											  ((text table dump)
												(let ((show-field-name
														 (slot-ref logger 'show-field-name))
														(missing-val
														 (slot-ref logger 'missing-val))
														)
												  (if #f
														(begin
														  (if leading-entry 
																(file 'show " ")
																(set! leading-entry #t))
														  (file 'show field)))
												  
												  )
												)
											  
											  (else
												(kdebug '(log-* log-ecoservice)
														  "[" (my 'name) ":" (cnc self) "]"
														  "Ignoring " field " because I don't have it")
												'ignore-unhandled-format)))
										 (begin
											(kdebug '(log-* log-array)
													  "[" (my 'name) ":" (cnc self) "]"
													  "no service" field)
											#f)))
								 (uniq (if #t
											  targets
											  (filter (not-member (slot-ref logger 'dont-log))
														 targets)))
								 )
								(file 'newline)
								)
							 )
						  )
					 )
				  )



(model-method% (<general-array> <integer>) (data-name self i)
				  (let ((dn (slot-ref self 'data-names)))
				  (if (and (integer? i) (positive? i) (< i (length dn)))
						(list-ref dn i)
						;;(error 'bad-data-name-request i)
						#f
						)))

;;; (model-method (<general-array> <agent> <symbol>) (alter self alien sym #!rest args)
;;; 				  (case sym
;;; 					 ((set-slot) (if (and (symbol? sym) (pair? args) (sym (car args)))
;;; 									 (slot-set! self (car args) (cadr args))
;;; 									 (error 'bad-arguments)))
;;; 					 ((adjust#) (if (and (symbol? sym) (pair? args) (sym (car args)) (number? (cadr args)))
;;; 										 (slot-set! self (car args)  (+ (cadr args) (slot-ref self (car args))))
;;; 										 (error 'bad-arguments)))
;;; 					 )
;;; 				  )



;; Allows the kernel to make queries -- we do not want the kernel to be
;; able to be accidentally "run" like an agent, hence the independent class
(model-method (<general-array> <kernel> <symbol>) (query self questioner tag #!rest args)
				  (let ((a (allocate-instance <agent>)))
					 (apply query (cons self (cons a (cons tag args))))))

(model-method (<general-array>) (has-many-loci? self)
				  #f)



(model-method (<array>) (has-many-loci? self)
				  #t)

(model-method% (<general-array> <agent> <symbol>)(query self questioner tag #!rest args)
				  (if (not (agent? questioner))
						(error 'non-canonical-use-voids-warranty))
				  (cond
					((eq? tag 'request-accessor)
					 (let ((subject self)
							 (instigator questioner))
						(lambda args
						  (let ((me instigator) (subject subject))
							 (cond
							  ((null? args) ;; no args returns the subject of the accessor
								thee)
							  ((eq? (car args) 'get)
								(apply data-ref subject (cons thee (cdr args))))
							  ((eq? (car args) 'set!)
								(apply data-set! (cons subject (cdr args))))
							  ((eq? (car args) 'adjust!) ;; This only works for numeric values
								(let* ((sgra (reverse (cdr args)))
										 (val (car sgra))
										 (addr (reverse (cdr sgra))))
								  (if (number? val)
										(apply data-set! (cons subject (cons addr (list (+ (apply data-ref (cons subject addr)) val)))))
										(error "attempt to adjust a non-numeric value using an accessor 'adjust! request"))))
							  (else "Unrecognised accessor query"))
							 ))))

					((and (has-data? questioner 'location) (eq? tag 'distance) (null? args))
					 (let ((L (location questioner)))
						(if (has-data? self 'location)
							 (let ((l (data-ref self 'location)))
								(cond
								 ((and (point? l) (point? L)) (distance l L))
								 ((and (point-list? l) (point? L)) 
								  (apply min (map (lambda (x) (distance x L)) l)))
								 ((and (point? l) (point-list? L))
								  (apply min (map (lambda (x) (distance l x)) L)))
								 (else (error "bad mojo in 'distance query"))
								 ))
							 #f))
					 )
					
					((eq? tag 'distance)
					 (let ((L (if (and (point-list? args) (= 1 (length args))) (car args) (location questioner))))
						(if (has-data? self 'location)
							 (apply min (map (lambda (x) (distance x L)) (data-ref self 'location)))
							 #f))
					 )
					
					((and (eq? tag 'location*) (null? (cdr args))) (has-data? self 'location)
					 (data-ref self 'location))

					((and (eq? tag 'location) (null? (cdr args)) (has-data? self 'location))
					 (if (has-data? questioner 'location)
						  (let* ((qloc (location questioner))
									(loc (data-ref self 'location))
									(points 
									 (cond
									  ((and (or (point? qloc) (point-list? qloc)) (point? loc))
										loc)
									  ((point-list? loc)
										(cond
										 ((point? qloc)
										  (let ((d (map (lambda (x) (list (distance x qloc) x)) loc)))
											 (let loop ((ld d)
															(best (car d))
															)
												(if (pair? ld) (dnl* 'location best (car ld)))
												(cond
												 ((null? ld) (cadr best))
												 ((< (caar ld) (car best))
												  (loop (cdr ld) (cadar ld)))
												 (else (loop (cdr ld) best))))))
										 ((point-list? qloc)
										  (let ((X (cross (location loc) (location qloc))))
											 (let ((d (map (lambda (x) (list (apply distance x) (car x))) X)))
												(let loop ((ld d)
															  (best (car ld))
															  )
												  (if (pair? ld) (dnl* 'Location  best (car ld)))
												  (cond
													((null? ld) (cadr best))
													((= (caar ld) (car best))
													 (loop (cdr ld) (cons (caar ld) (cons (caadar ld) (cdr best))))) 
													((< (caar ld) (car best))
													 (loop (cdr ld) (list (caar ld) (caadar ld))))
													(else (loop (cdr ld) best)))))))
										 (else #f)))
									  ))
									)
							 (if (point-list? points) (list-ref points (random-integer (length points)))
								  points))
						  ;; the questioner doesn't have a location, so we give a centroid
						  (let ((loc (data-ref self 'location)))
							 (cond
							  ((point? loc) loc)
							  ((point-list? loc) (centroid loc))
							  (else #f)))))

					((and (member tag (slot-ref self 'data-names)) (pair? args) (eq? (car args) '*))
					 ;; return all entries in the tag entry
					 (data-ref self tag))

					((and (eq? tag 'set!) (<= (length args) 2)) (member (car args) (slot-ref self 'data-names))
					 ;; set the data associated with (car args); args should be like one of '(sym)  '(sym ()) '(sym (.....))
					 (data-set! self tag (cadr args)))

					((and (eq? tag 'set!) (= (length args) 3) (number? (caddr args)) (member (car args) (slot-ref self 'data-names)))
					 ;; set the data associated with (car args); args should be like '(i sym val)
					 (data-set! self (car args) (cadr args) (caddr args)))

					((and (member tag (slot-ref self 'data-names)) (pair? args) (apply andf (map number? args)))
					 ;; return entry for all indicated indices
					 (list-ref% (data-ref self tag) (filter (lambda (x) (and (integer? x) (not (negative? x)) (< x (length (slot-ref self 'data))))) args)))

					;; (query arragent 'nearest [location])
					((and (eq? tag 'index-nearest) (has-data? self 'location))
					 (let* ((L (if (and (pair? args) (pair? (cdr args)) (point-list? (cdr args)) (= 2 (length args))) (cadr args) (location questioner)))
							  (N (if (pair? args) (car args) 1))
							  (proxylist '())
							  (countdown (car args))
							  (dix (data-index self 'location))
							  (D (slot-ref self 'data))
							  (k (length D))
							  (target (let search ((ix 0)
														  (bestindex 0)
														  (bestdist +inf.0)
														  )
											(if (>= ix k)
												 bestindex
												 (let ((ndist (distance (list-ref D ix) L)))
													(if (< ndist (distance (list-ref D ix) L) bestdist)
														 (search (+ 1 ix) ix ndist )
														 (search (+ 1 ix) bestix bestdist ))
													)
												 )))
							  )
						target
						))
					
					;; (query arragent 'proxy-nearest [17 [location]])
					((and (eq? tag 'proxy-nearest) (has-data? self 'location))
					 (let* ((L (if (and (pair? args) (pair? (cdr args)) (point-list? (cdr args)) (= 2 (length args))) (cadr args) (location questioner)))
							  (N (if (pair? args) (car args) 1))
							  (proxylist '())
							  (countdown (car args))
							  (dix (data-index self 'location))
							  (D (slot-ref self 'data))
							  (k (length D))
							  (target (let search ((ix 0)
														  (bestindex 0)
														  (bestdist +inf.0)
														  )
											(if (>= ix k)
												 bestindex
												 (let ((ndist (distance (list-ref D ix) L)))
													(if (< ndist (distance (list-ref D ix) L) bestdist)
														 (search (+ 1 ix) ix ndist )
														 (search (+ 1 ix) bestix bestdist ))
													)
												 )))
							  )
						(lambda (arr agent tag #!rest args)
						  (let ((data (slot-ref self 'data))
								  (data-names (slot-ref self 'data-names))
								  (ix ix))
							 (cond
							  ((and (member tag (slot-ref self 'data-names)) (pair? args) (eq? (car args) '*))
								(error) 
								)
							  ((and (eq? tag 'set!) (= (length args) 3)) (number? (caddr args) (member (car args) (slot-ref self 'data-names)))
								(error) 
								)
							  ((and (eq? tag 'set!) (<= (length args) 2)) (member (car args) (slot-ref self 'data-names))
								(error) 
								)
							  ((and (member tag (slot-ref self 'data-names)) (pair? args) (apply andf (map number? args)))
								(error) 
								)
							  ((eq? tag 'distance)
								(error) 
								)
							  ((and (eq? tag 'index-nearest) (has-data? self 'location))
								)
							)
						))
					))
					
					((member? tag (map car (class-slots (class-of self))))
					 (slot-ref self tag))

					(else (parent-query))
					))

(model-method (<array>) (initialise-instance self)
				  (parent-initialise-instance)
				  (set-boundaries! self)
				  )

(model-method (<array> <point> <boolean>) (get-loci% self locus)
				  ;; returns indices that will map to elements
				  (if (has-data? self 'location)
						(let* ((data (slot-ref self 'data))
								 (bolus (if dix (data-ref self 'location)))
								 )
						  bolus
						  )
						#f)
				  )

(model-method (<array> <point> <boolean>) (get-loci$ self locus)
				  ;; returns indices that will map to elements
				  (let* ((data (get-loci% self locus))
							)
					 (cond
					  ((pair? data) (sort (lambda (x y) (<= (distance x locus) (distance y locus)) )) )
					  ((null? data) '())
					  (else data)
					 )
				  ))
					  
(model-method% (<array> <point> <number>) (lookup-locus self locus radius) ;; returns indices that will map to elements
				  (if (has-data? self 'location)
						(let* ((L (data-ref self 'location))
								 (M (member (lambda (x) (eqv? locus x))  L))
								 )
						  (if M
								(- (length L) (length M) 1)
								#f))))


;(model-method (<array>) (location self) ;; this will generally have less area than a bounding circle
;				  (slot-ref self 'hull))


(model-method (<general-array>) (supports-proxy? self)
				  #f)

(define (random-array-agent n)
  (let ((radii (map (lambda (x) (random-integer n)) (seq n)))
		  (loci (map (lambda (x) (list (random-integer n)(random-integer n))) (seq n)))
		  (values (map (lambda (x) (+ n (random-integer n))) (seq n)))
		  )
	 (make-agent <general-array> "" 'data-names '(radius location value) 'data (map (lambda (r l v) (list r l v)) radii loci values))
	 ))

(model-method% (<general-array> <symbol>) (has-data? self k) ;; returns the data-name symbol if there is a data element, #t if it is a slot and # if neither
				  (let ((d (member k (slot-ref self 'data-names))))
					 (if d (car d)
						  (has-slot? self k))))

;(model-method (<general-array> <integer>) (location self ix)
;				  #f
;				  )

;(model-method (<general-array> <list>) (location self lst)
;				  #f)
	
(model-method (<general-array>) (radius self)
				  #f)



(model-method% (<general-array>) (data-names self)
				  (slot-ref self 'data-names))

(model-method (<general-array> <symbol>) (new-data-name self sym)
				  (let ((datasyms (my 'data-names)))
					 (if (null? (my 'data))
						  (set-my 'data-names (append datasyms (list sym)))
						  (error "data-names may not be altered if there is already data"))))

(model-method (<general-array> <list>) (new-data-names self syms)
				  (let ((datasyms (my 'data-names)))
					 (if (null? (my 'data))
						  (set-my 'data-names (append datasyms (list sym)))
						  (error "data-names may not be altered if there is already data"))))

(model-method% (<general-array>) (data-index self selector #!rest args)
				  (cond
					((symbol? selector) (apply data-field-index (cons self (cons selector args))))
					((point? selector) (apply data-record-index (cons self (cons selector args))))
					(else (error "Bad argument to (data-index...) for an array agent" selector))))

(model-method% (<general-array> <symbol>) (data-field-index self sym)
				  (let* ((data-names (my 'data-names))
							;;(data (my 'data))
							(ix (member sym data-names)))
					 (if (not ix)
						  #f
						  (- (length data-names) (length ix))
						  )
					 ))

(model-method (<general-array> <list> <symbol>) (rec-ref self rec sym)
				  (let ((i (data-field-index self sym)))
					 (if (and i (< i (length rec)))
						  (list-ref rec i)
						  (error "Invalid record reference call: " rec sym))))

(model-method (<general-array> <list> <symbol>) (rec-set! self rec sym val)
				  (let ((i (data-field-index self sym)))
					 (if (and i (< i (length rec)))
						  (list-set! rec i val)
						  (error "Invalid record reference call: " rec sym))))

(model-method (<general-array>) (data-length self)
				  (length (my data)))

(model-method (<general-array> <list>) (add-data-record self lst)
				  ;;(dnl* "Adding array element" (length (my 'data)) lst)
				  (if (>= (length (my 'data)) (my 'max-records))
						(error "Exceeded nominated <general-array> limits")
						(if (= (length lst) (length (my 'data-names)))
							 (set-my! 'data (append (my 'data) (list lst)))
							 (begin
								(dnl* "An array got" (length lst) "but expected" (length (my 'data-names)) "when adding a record")
								(error self lst)))))

(model-method (<general-array> <list> <integer>) (remove-data-record self lst i)
				  (let ((data (my 'data)))
					 (set-my! 'data (append (list-head data i) (list-tail data (+ 1 i))))))

(model-method (<general-array> <list> <list>) (filter-array-fields self fieldlist reclist)
					(map (lambda (r) (map (lambda (s) (list-ref r (data-field-index self s))) fieldlist)) reclist))

;;; ;; Returns the data from list element i in data
;;; (model-method (<general-array>) (all-data self)
;;; 				  (dnl* 'data-ref-null)
;;; 				  (slot-ref self 'data))

(model-method% <general-array> (has-data? self key)  
					(or (member key (slot-ref self 'data-names))
						 (parent-has-data?)))

(model-method% <general-array> (data-ref self #!rest args)
				  (let* ((data (my 'data))
							(h (if (null? args) #f (car args)))
							(t (if h (if (pair? (cdr args)) (cdr args) #f) #f)))
					 (cond
					  ((not h) (slot-ref self 'data))
					  ((and (symbol? h) (not t))
						(let ((i (data-field-index self h)))
						  (if i
								(map (lambda (x) (list-ref x i)) data)
								(if (has-slot? self h)
									 (slot-ref self h)
									 (error 'data-ref:bad-data/slot-symbol h)))))
					  ((and (integer? h) (not t))
						(let ((i h))
						  (if (and (<= 0 i) (< i (length data)))
								(list-ref data i)
								(error 'data-ref:bad-index i))))
					  ((and  (point? h) (not t))
						(let ((i (data-record-index self h)))
						  (if (and (number? i) (<= 0 i) (< i (length data)))
								(list-ref data i)
								#f)
						  ))
					  (t
						(cond
						 ((and (integer? h) (null? (cdr t)) (symbol? (car t)))
						  (list-ref (list-ref (slot-ref self 'data) h) (data-field-index self (car t))))

						 ((and (symbol? h) (null? (cdr t)) (integer? (car t)))
						  (list-ref (list-ref (slot-ref self 'data) (car t)) (data-field-index self h)))

						 ((apply andf (map symbol? args))
						  (filter-array-fields self args data)
						  ;;(map (lambda (r) (map (lambda (s) (list-ref r (data-field-index self s))) args)) data)
						  )

						 ((apply andf (map number? args))
						  (map (lambda (r)
									(list-ref data r)) args))

						 ((and (integer? h) (apply andf (map symbol? t)))
						  (let ((lst (data-ref self h)))
							 (map (lambda (s) (rec-ref self lst s)) t)));; extract (by symbol) from the record

						 ((and (symbol? h) (apply andf (map integer? t)))
								(let ((recs (data-ref self h)))
								  (map (lambda (i) (list-ref recs i)) t)))

						 ((and (point? h) (number? (car t)) (pair? (cdr t)) (apply andf (map symbol? (cdr t))) (has-data? self 'location))
						  ;;(dnl* 'psr*)
						  (filter-array-fields self (cdr t) (data-ref self h (car t))))
						 
						 ((and (point? h) (number? (car t)) (has-data? self 'location))
						  (let ((pt h) (rad (car t)))
							 (filter (lambda (rec) (<= (distance h (rec-ref self rec 'location)) rad)) (slot-ref self 'data))))
						 
						 ((and (point? h) (apply andf (map symbol? t)) (has-data? self 'location))
						  (let ((ix (data-field-index self (car t))))
							 ;;(dnl* 'ps* h t ix)
							 (if (not ix)
								  #f
								  (apply data-ref (cons self (cons ix t))))))

						 (else (error 'data-ref:bad-arguments args))
						 )
						)
					  (else (error 'data-ref:unfathomable-ingredients args))
					  ))
				  )

;; Returns the list element i from data
;;; (model-method (<general-array> <integer>) (data-ref self i) (list-ref (my 'data) i))

;; Returns all the indicated slot entries from the data. Useful for things like (apply + (data-ref trees 'leaf-area))
;; If we rebase this (give it a new parent) this may need to be modified
;;; (model-method (<general-array> <symbol>) (data-ref self s)
;;; 				  ;;(dnl* 'data-ref-S)
;;; 				  (let* ((data (my 'data))
;;; 							(i (data-field-index self s))
;;; 							)
;;; 					 (if i
;;; 						  (map (lambda (x) (list-ref x i)) data)
;;; 						  (if (has-slot? self s)
;;; 								(slot-ref self s)
;;; 								(error 'bad-data/slot-symbol)))))

(model-method% (<general-array> <point>) (data-ref* self i*)
				  (map (lambda (i) (data-ref self i)) i*))

(model-method% (<general-array> <point> <symbol>) (data-ref* self i* s)
				  (map (lambda (i) (data-ref self i s)) i*))

(model-method% (<general-array> <list>) (data-ref* self s*)
				  (map (lambda (s) (data-ref self s)) s*))

(model-method% (<general-array> <integer> <list>) (data-ref* self i s*)
				  (map (lambda (s) (data-ref self i s)) i s*))

(model-method% (<general-array> <point> <list>)  (data-ref* self i* s*)
				  (map (lambda (i)
							(map (lambda (s) (data-ref self i s)) s*)) i*))


;; Returns the s element from the i'th row in data
(model-method% (<general-array> <integer> <symbol>) (@data-ref self i s)
				  ;;(dnl* 'data-ref-IS)
				  (list-ref (data-ref self s) i))

;; Returns the s element from the i'th row in data
(model-method% (<general-array> <integer> <symbol>) (@data-ref self i s)
				  ;;(dnl* 'data-ref-IS)
				  (list-ref (data-ref self s) i))

;;; (model-method (<general-array> <integer> <symbol>) (data-ref self ix sym)
;;; 				  (@data-ref self ix sym))

;search by location
(model-method (<general-array> <point>) (data-record-index self loc)
				  (if (has-data? self 'location)
						(let* ((k (member loc (data-ref self 'location)))
								 (ix (if k (- (length (slot-ref self 'data)) (length k)) #f)))
						  (if ix  ix #f))
						(error 'no-location)))

;search by location
(model-method (<general-array> <point> <symbol>) (@data-ref self loc sym)
				  (if (has-data? self 'location)
						(let* ((k (member loc (data-ref self 'location)))
								 (ix (if k (- (length (slot-ref self 'data)) (length k)) #f)))
						  (if ix (@data-ref self ix sym) #f))
						(error 'no-location)))

;; If we rebase this (give it a new parent) this may need to be modified
(model-method% (<general-array> <symbol> <list>) (data-set! self s v)
				  (let ((ix (data-field-index self s)))
					 (if ix
						  (if (= (length v) (length (my 'data)))
								(for-each
								 (lambda (lst vv)
									(list-set! lst ix vv))
								 (my 'data))
								(error 'list-length-error)))
					 (slot-set! self v)))

(model-method% (<general-array> <integer> <list>) (data-set! self i s l) (set-car! (data-ref self i) (car l)) (set-cdr! (data-ref self i) (cdr l)))

(model-method% (<general-array> <integer> <symbol>) (@data-set! self i s v) (list-set! (data-ref self i) (data-field-index self s) v))

(model-method% (<general-array> <agent> <symbol> <integer> <symbol>) (request-accessor self external-agent sym i s #!rest args)
				  (case sym
					 ((set!)
					  (lambda (val) (@data-set! self i s val)))
					 ((ref)
					  (lambda (val) (@data-ref self i s)))
					 ((adjust#)
					  (lambda (val) (@data-set! self i s (+ (@data-ref self i s) val))))
					 ((args) (list external-agent sym i s args)))
					 (else (parent-request-accessor))
				  )

(model-method <general-array> (initialise-instance self)
	 (parent-initialise-instance)


	 (if (uninitialised? (slot-ref self 'data)) (slot-set! self 'data '()))
	 (if (uninitialised? (slot-ref self 'data-names)) (slot-set! self 'data-names '()))
	 )


(model-body% <general-array>
  (let ((parent-return (call-all-parents)))
	 (if (and (pair? (my 'data-names)) (pair? (my 'data)))
		  (begin
			 (set-my 'data (filter (lambda (x) (not (member (car x) '(remove terminated)))) (slot-ref self 'data)))
			 )
		  )
	 'ok
	 )
  )

				  

(model-method% (<array>) (set-boundaries! self)
				  (let ((data (slot-ref self 'data)))
					 (if (and (list? data)
								 (> (length data) 1)
								 (has-data? self 'location))
						  
						  (let* ((D (data-ref self 'location))
									(C (centroid D))
									(R (apply max (map car D)))
									(L (apply min (map car D)))
									(u (apply max (map cadr D)))
									(l (apply min (map cadr D)))
									(N (length (slot-ref self 'data)))
									)
							 (slot-set! self 'centre C)
							 (slot-set! self 'radius (apply min (map (lambda (p) (distance C p)) D)))
							 (slot-set! self 'hull (list (list L l) (list R l) (list R u) (list L u) (list L l))) ;; closed polygon
							 (slot-set! self 'refresh-boundaries #f)
							 ))))
						
									 
(model-method (<array>) (radius self)
				  (if (slot-ref  self 'refresh-boundaries)
						(set-boundaries! self))
				  (apply min (cons (slot-ref self 'radius) (map (lambda (x)  (distance (slot-ref self 'centre) x)) (slot-ref self 'hull)))))

(model-method (<array>) (Radius self)
				  (if (slot-ref  self 'refresh-boundaries)
						(set-boundaries! self))
				  (apply max (cons (slot-ref self 'radius) (map (lambda (x)  (distance (slot-ref self 'centre) x)) (slot-ref self 'hull)))))

;; NOTE: (data-ref self 'radius) is not necessarily equal to (radius self) or (Radius self)
;; (data-ref self 'radius) may find a 'radius field in the data-names, for a start, and even if it
;; doesn't, it would return the value which is the farthest point from the centroid.  (radius self),
;; in contrast, returns the size of the empty circle containing the centroid, and (Radius self)
;; returns the size of the smallest circle about the centroid which contains all the locations
;;
;;                                          BE WARNED
;;

(model-method (<array> <list>) (radius self lst)
				  (if (slot-ref  self 'refresh-boundaries)
						(set-boundaries! self))

				  (cond
					((member 'radius (my 'data-names))
					 (cond
					  ((and (apply andf (map integer? lst)) (apply andf (map (lambda (x) (not (negative? x))) lst)))
						(map (lambda (ix) (data-ref self ix 'radius)) lst))
					  ((point? lst) ;; find point nearest to lst
						(let loop ((ix 0)
									  (data (my 'data))
									  (got #f)
									  (bestd +inf.0))
						  (cond
							((null? data)
							 (data-ref self 'radius))
							((< (distance (data-ref self 'radius ix) lst)  bestd)
							 (loop (+ ix 1) (cdr data) ix (distance (data-ref self 'radius ix) lst)))
							(else (loop (+ ix 1) (cdr data) got bestd)))
						  )
						(else #f)
						)
					  ))
					(else (error 'uncool-call-to-radius))
					))

;; If we rebase this (give it a new parent) this may need to be modified
(model-method% (<array> <symbol> <list>) (data-set! self s v)
				  (if (eq? s 'location) (slot-set! self 'refresh-boundaries #t))

				  (let ((ix (data-field-index self s)))
					 (if ix
						  (if (= (length v) (length (my 'data)))
								(for-each
								 (lambda (lst vv)
									(list-set! lst ix vv))
								 (my 'data))
								(error 'list-length-error)))
					 (slot-set! self v)))

(model-method% (<array> <integer> <list>) (data-set! self i s l)
				  (if (eq? s 'location) (slot-set! self 'refresh-boundaries #t))
				  (set-car! (data-ref self i) (car l))
				  (set-cdr! (data-ref self i) (cdr l)))

(model-method% (<array> <integer> <symbol>) (@data-set! self i s v)
				  (if (eq? s 'location) (slot-set! self 'refresh-boundaries #t))

				  (list-set! (data-ref self i)
								 (data-field-index self s) v))





;	diffeq-system history:
;
;; This is a relatively small class -- it doesn't maintain variables
;; itself, rather we pass in closures that know how to get and set the
;; values in the variables of interest.  The actual names/symbols used
;; in the equations here may bear no relation at all to the sources of
;; the data --- all correspondences are an artifact of the data used
;; to initialise the variable-values, get-externals and
;; external-update


;; this expects a list of functions which return reals
;--- model-method <diffeq-system> (set-system-dynamics! self . d/dt-list)

;; (agent-initialisation-method <diffeq-system> (args) (no-default-variables)
;;    (set-state-variables self
;; 	  (list 'type 'diffeq
;; 			  'dont-log '(ready-for-prep
;; 							  ;; agent things
;; 							  agent-body-ran
;; 							  agent-epsilon counter 
;; 							  migration-test state-flags
;; 							  dont-log timestep-schedule kernel
							  
;; 							  ;; log agent things
;; 							  introspection-targets
;; 							  timestep-epsilon 

;; 							  dims ;; thing things

;; 							  ;; environment things
;; 							  default-value minv maxv 

;; 							  ;; ecoservice things
;; 							  plateau-interval growth-rate 

;; 							  ;; landscape things
;; 							  service-list service-update-map
;; 							  update-equations terrain-function
;; 							  dump-times scale 
;; 							  log-services-from-patch
;; 							  log-patches-from-habitat

;; 							  ;; animal things
;; 							  domain-attraction food-attraction 
;; 							  near-food-attraction search-speed
;; 							  wander-speed forage-speed	
;; 							  movement-speed foodlist homelist
;; 							  breedlist habitat
;; 							  )
;; 			  )
;; 	  )
;; 	(parent-initialise) ;; call "parents" last to make the
;; 	;; initialisation list work
;; 	(if (or (not (number? (my 'subdivisions))) (not (positive? (my 'subdivisions))))
;; 		 (slot-set! self 'subdivisions 3)) 
;; 	(set-state-variables self args)
;;  )


(model-method <diffeq-system> (set-system-dynamics! self d/dt-list)
				  (slot-set! self 'variable-definitions #f)
				  
				  (if (null? d/dt-list)
						(abort))

				  (if (pair? (car d/dt-list))
						(set! d/dt-list d/dt-list))       ;;; A *list* of functions was
																	 ;;; passed in the "rest" part of
				                                        ;;; the line. It's Christmas,
				                                        ;;; unwrap it
				  
				  (if (andf (map procedure? d/dt-list))
						(slot-set! self 'd/dt-list d/dt-list)
						(abort))
				  )

;--- model-method (<diffeq-system>) (define-system-dynamics! self pn ps pf domains getters setters)
"
This defines the system using a set of discrete lists of names, symbols, and relations. The alternative
is defined below (and is also called 'define-system-dynamics!', but has only two arguments.

pn -- a list of names (usually for printing)
ps -- a list of symbols used in equations
pf -- a list of the d/dt type functions
domains -- a list of procedures which take a value and typically clip it to an interval
getters -- getters for external variables
setters -- setters for external variables

"
(model-method (<diffeq-system>) (define-system-dynamics! self pn ps pf domains getters setters)
				  (if (not (let ((args (list pn ps pf domains getters setters)))
								 (and (apply andf (map list? args))
										(apply = (map length args)))))
						(error "define-system-dynamics! was passed non-lists or lists of differing length!" args))

				  (define-system-dynamics! self (map list pn ps pf domains getters setters)))


				  ;; (let ((tpn (apply andf (map string? pn)))     ;; names
				  ;; 		  (tps (apply andf (map symbol? ps)))     ;; symbols
				  ;; 		  (tpf (apply andf (map procedure? pf)))  ;; functions (d/dt)
				  ;; 		  ;;(td (apply andf (map procedure? domains)))  ;; functions 
				  ;; 		  ;;(tg (apply andf (map procedure? getters)))  ;; functions 
				  ;; 		  ;;(ts (apply andf (map procedure? setters)))  ;; functions
				  ;; 		  )

				  ;; 	 (if (not (and  tpn tps tpf
				  ;; 						 ;;td tg ts
				  ;; 						 ))
				  ;; 		  (begin
				  ;; 			 (ednl* (string-append "There was at least one erroneous"
				  ;; 										  "argument passed to define-system-dynamics!"))
				  ;; 			 (if (not tpn)
				  ;; 				  (let ((culprits (!filter string? pn)))
				  ;; 					 (ednl* "The following members of the name list should be strings:")
				  ;; 					 (apply ednl* (cons "   " culprits)))
				  ;; 				  )
				  ;; 			 (if (not tps)
				  ;; 				  (let ((culprits (!filter symbol? ps)))
				  ;; 					 (ednl "The following members of the symbol list should be symbols:")
				  ;; 					 (apply ednl* (cons "   " culprits)))
				  ;; 				  )
				  ;; 			 (if (not tpf)
				  ;; 				  (let ((culprits (!filter procedure? pf)))
				  ;; 					 (ednl "The following members of the d/dt list should be functions:")
				  ;; 					 (apply ednl* (cons "   " culprits)))
				  ;; 				  )
				  ;; 			 )))
				  ;; (slot-set! self 'variable-names pn)
				  ;; (slot-set! self 'variable-symbols ps)
				  ;; (slot-set! self 'variable-procedures pf)

				  ;; (if (apply andf (map procedure? domains))
				  ;; 		(slot-set! self 'domains domains)
				  ;; 		(error "the domains list should contain only functions" domains))
				  ;; (if (apply andf (map procedure? getters))
				  ;; 		(slot-set! self 'get-externals getters)
				  ;; 		(error "the getters list should contain only functions" getters))
				  ;; (if (apply andf (map procedure? setters))
				  ;; 		(slot-set! self 'external-update setters)
				  ;; 		(error "the setters list should contain only functions" setters))

				  ;; (slot-set! self 'd/dt-list (map list pn ps pf domains getters setters))
				  ;; )

;--- model-method (<diffeq-system>) (define-system-dynamics! self defns )
" 
THE PREFERRED VERSION FOLLOWS

This defines the system using a list of sets which contains the
names, symbols, and relations. The alternative is defined above (and
is also called 'define-system-dynamics!', but has many arguments.

A list of lists
pn -- a name (usually for printing)
ps -- symbol used in equations
pf -- the d/dt type function
domains -- a procedure that clips the value to an interval
getters -- a getter for an external variable, or I
setters -- a setter for an external variable or I

A typical defns list would be like
(list 
   (\"V.lagopus\" 'Vl (lambda (t Vl H) (- (* alpha Vl ) (* beta Vl H))) (lambda (v) (max 0 v)) (lambda (x) (slot-ref self x)) (slot-set! self x))
	(\"L.arcticus\" 'H (lambda (t Vl H) (- (* delta Vl H) (* gamma H)) ) (lambda (v) (max 0 v)) (lambda (x) (slot-ref self x)) (slot-set! self x))
)


"
(model-method (<diffeq-system>) (define-system-dynamics! self defns )
				  (if (and (apply andf (map (lambda (x) (= (length x) 6))
													 defns))
							  (>= (length defns) 1)
							  )
						(let (;;(pn (map car defns))
								;;(ps (map cadr defns))
								;;(pf (map caddr defns))
								;;(getters (map cadddr defns))
								;;(setters (map caddddr defns))
								;;(domains (map cadddddr defns))
								)
						  (slot-set! self 'variable-definitions defns)

						  (slot-set! self 'variable-names (map car defns))
						  (slot-set! self 'variable-symbols (map cadr defns))
						  (slot-set! self 'd/dt-list (map caddr defns))

						  (let ((domains (map cadddr defns)))
							 (if (apply andf (map procedure? domains))
								  (slot-set! self 'domains domains)
								  (error "the domains list should contain only functions" domains)))

						  (let ((getters (map caddddr defns)))
							 (if (apply andf (map procedure? getters))
								  (slot-set! self 'get-externals getters))
								  (error "the getters list should contain only functions" getters))

						  (let ((setters (map cadddddr defns)))
							 (if (apply andf (map procedure? setters))
								  (slot-set! self 'external-update setters))
								  (error "the setters list should contain only functions" setters))

						  )
						(abort
						 (string-append
						  " the format for defining a system is either:\n"
						  "  (define-system-dynamics! self \n"
						  "     (\"Grass\" 'G dG/dt)\n"
						  "     (\"Rabbit\" 'R dR/dt)\n"
						  "     (\"Fox\" 'F dF/dt)\n"
						  "     (\"Bear\" 'B dB/dt))"
						  " or\n"
						  "  (define-system-dynamics! self \n"
                    "      '(\"Grass\" \"Rabbit\" \"Fox\" \"Bear\")"
						  "      '(G R F B) \n"
						  "      (list (lambda (t G R F B) ;;; dG/dt\n"
						  "                ...)\n"
						  "            (lambda (t G R F B) ;;; dR/dt\n"
						  "                ...)\n"
						  "            (lambda (t G R F B) ;;; dF/dt\n"
						  "                ...)\n"
						  "            (lambda (t G R F B) ;;; dB/dt\n"
						  "                ...)\n"
						  "where the arguments to each of the above d/dt "
						  "are t grass rabbit fox bear"
						  )
						 )
						)
				  )

;---  model-body <diffeq-system
(model-body <diffeq-system>
						(kdebug '(model-bodies patch-running)"In " (cnc self) (name self) "@" t)

 						;; Ok, I need to be able to refer to service
						;; directly (names) and to classes (types). Type
						;; values are aggregates of the members of the
						;; service-list of that type excluding any of those
						;; members specified by name members.

						;; We can tell the difference because names are
						;; required to be strings and types are required to
						;; be symbols.

						;; Changes in a type value are implemented pro-rata.

						;;( dnl "Running <diffeq-system> model body")

						(if (not (list? (my 'd/dt-list)))
							 (abort "This diffeq-system has not been initialised properly: use define-system-dynamics!"))
						
						(if (<= dt (slot-ref self 'too-small))
							 (abort "Bad dt passed to <diffeq-system> model-body"))

						(set-my! self 'variable-values 
									(map (lambda (x) (x)) (my 'get-externals)))
						
						(let ((var-values (map (lambda (d v) (d v)) (my 'domains) (my 'variable-values)))
								(d/dt (my 'd/dt-list))
								(domains (my 'domains))
								)

						  (if (and (not (null? var-values)) (apply andf var-values))
								(let ((P (if (not (and d/dt (pair? d/dt)))
												 (lambda args 0)
												 (rk4* d/dt t (+ t dt) (/ dt (my 'subdivisions)) var-values)))
										)
								  ;;( dnl "Got past the rk4* for " (my 'name) "
								  ;;and " (my 'variable-names))

								  ;;(set-my! 'P P) ;; Don't really *need*
								  ;;this, except perhaps for debugging
								  (let ((deltas (P (+ t dt))))
									 (for-each 
									  (lambda (x v d)
										 (if (not (zero? (imag-part v)))
											  (abort "got complex number, wanted a real"))

										 (add! self x (- v (value self x)))
										 ;; These are the adjustments due to
										 ;; consumption and predation (slot-set!
										 ;; self x v)
									
										 (if (and (member x non-negative) (< (value self x) 0.0))
											  (set-value! self x 0.0))
										 )
									  (my 'variable-names) deltas domains))
								  )

								(error (cnc self)
										 " either has no variable names defined, or has strayed out of its domain!"
										 (map cons (my 'variable-names) var-values))
										 
						  )

						(parent-body)

						dt)
						)









(define (patches-containing-locus loc Q)
  (filter (lambda (x) (contains? x loc)) (filter (lambda (x) (is-class? x <patch>)) Q)))



(define (patches-containing loc Q)
  (cond
	((point? loc)
	 (sortless-unique
	  (patches-containing-locus loc Q)))

	((point-list? loc)
	 (sortless-unique
		 (map (lambda (p)
				 (patches-containing-locus p Q)
				 )
			  loc)))
	((isa? loc <array>)
	 (let ((loci (data-ref loc 'location)))
		(sortless-unique
		 (map (lambda (p)
				 (patches-containing-locus p Q)
				 )
			  loci)))		
	 )
	(else
		(let loop ((result '())
					  (q Q))
		  (if (null? q)
				result
				(if (isa? (car q) <patch>)
					 (if (point? loc)
						  (if (contains? (car q) loc)
								(loop (cons q result) (cdr q))
								(loop result (cdr q)))
						  (if (apply orf (map (lambda (p) (contains? (car q) p)) loc))
								(loop (cons q result) (cdr q))
								(loop result (cdr q)))
						  )
					 ))))
	))


;;; (define (patches-containing loc Q)
;;;   (if (isa? loc <array>)
;;; 		(let ((loc (data-ref loc 'location))
;;; 				)
;;; 		  (sortless-unique (apply append (map (lambda (l)
;;; 															 (dnl l)
;;; 															 (patches-containing l Q)) loc))))
		 
;;; 		(let loop ((result '())
;;; 					  (q Q))
;;; 		  (if (null? q)
;;; 				result
;;; 				(if (isa? (car q) <patch>)
;;; 					 (if (point? loc)
;;; 						  (if (contains? (car q) loc)
;;; 								(loop (cons q result) (cdr q))
;;; 								(loop result (cdr q)))
;;; 						  (if (apply orf (map (lambda (p) (contains? (car q) p)) loc))
;;; 								(loop (cons q result) (cdr q))
;;; 								(loop result (cdr q)))
;;; 						  )
;;; 					 ))))
;;;   )

;; We resolve the reference with slot-ref since we might otherwise recurse forever


(model-method (<proxy> <symbol>) (has-data? self sym)
				  ((slot-ref self 'getter)
					(slot-ref self 'super)
					sym))

(model-method (<proxy> <symbol>) (data-ref self sym)
				  ((slot-ref self 'getter)
					(slot-ref self 'super)
					sym))
(model-method (<proxy> <number>) (data-ref self i)
				  ((slot-ref self 'getter)
					(slot-ref self 'super)
					i))


(model-method (<proxy> <symbol>) (data-set! self sym value)
				  ((slot-ref self 'setter)
					(slot-ref self 'super)
					sym value))

(model-method (<proxy> <number>) (data-set! self i value)
				  ((slot-ref self 'setter)
					(slot-ref self 'super)
					i value))


(model-method (<proxy> <number>) (@data-ref self i s)
				  ((slot-ref self '@getter)
					(slot-ref self 'super)
					i))
(model-method (<proxy> <number>) (@data-set! self i s value)
				  ((slot-ref self '@setter)
					(slot-ref self 'super)
					i value))




(model-body <proxy>
				(error "this should never happen")
				'ok)

(define (proxify A Q #!rest args)
  (if (void? A) (bugrit))
  (if (list? A) (set! A (denull-and-flatten	 A)))

;  (dnl* "Proxification: " A)
;  (if (list? A) (dnl* " -->"(map cnc A)))
  
  (cond
	((list? A)
;	 (dnl* "remapping list of length" (length A))
	 (denull-and-flatten (map (lambda (a) (proxify a Q)) A)))
	(else
	 ;;; (let ((radius (if (> (length args) 1) (car args) #f))
	 ;;; 		 (polygon (if (= (length args) 1) (car args) #f))
	 ;;; 		 (loc (if (> (length args) 1) (cadr args) #f))
	 ;;; 		 (UL (has-data? A 'location)))

	 (if (supports-proxy? A)
		  (let* ((data (slot-ref A 'data))
				  (data-names (slot-ref A 'data-names))
				  (patch-list (if (member 'domain data-names)
										(let ((pl (patches-containing A Q)))
										  (if (pair? pl) (car pl) #f))
										'()
										))
				  )
			 (map
			  (lambda (ix)
				 (if (member 'domain data-names)
					  (make-agent (slot-ref A 'proxy-class) (slot-ref A 'taxon) 'domain patch-list
								 'super A 'ix ix 'record (list-ref (slot-ref A 'data) ix) 'data-names data-names)
					  (make-agent (slot-ref A 'proxy-class) (slot-ref A 'taxon)
								 'super A 'ix ix 'record (list-ref (slot-ref A 'data) ix) 'data-names data-names)
				 ))
			  (seq (length data)))
			 )
		  A)
	 )
	)
  )
		
		  

;---- <tracked-agent> methods

;;; (default-agent-initialisation <tracked-agent>
;;;   'track #f
;;;   'tracked-paths #f

;;; (agent-initialisation-method <tracked-agent> () (
;;;   'track #f
;;;   'tracked-paths #f


;; This records the track in the native ordinate space of the submodel!
(model-method (<tracked-agent> <number> <point>) (track-location! self t loc)
				  (let ((myt (my 'track))
						  (myts (my 'track-state)))
					 (if (eq? myt #t) (set! myt '()))
					 (if (eq? myts #t) (set! myts '()))

					 (set! myt (append myt (list (cons t loc)))) ;; t, (x y z) --> (t x y z)
					 (set-my! 'track myt)

					 (if (and (my 'track-state) (my 'track-state-vars))
						  (let ((stt (append (list (cons t (map (lambda (s) (slot-ref self s)) (my 'track-state-vars))) (my 'track-state)))))
							 (set-my! 'track-state (cons stt (my 'track-state)))))
					 ))

(model-method (<tracked-agent>) (track self)
				  (my 'track))


(model-method (<tracked-agent> <point-list>) (set-track! self t)
				  (set-my! 'track (deep-copy t))) ;; we copy it so that we
;; aren't subject to the
;; track changing under
;; our feet


(model-method (<tracked-agent>) (new-track! self)
				  (let ((p (my 'tracked-paths))
						  (t (my 'track)))
					 (cond 
					  ((and p t) (set-my! 'tracked-paths (append (list t) p)))
					  (t (set-my! 'tracked-paths (list t))))
					 
					 (set-my! 'track '())
					 ))

(model-method (<tracked-agent>) (tracks self)
				  (my 'tracked-paths))


;----- (log-track-segment
(model-method (<tracked-agent> <point-list> <procedure>) (log-track-segment self track prj ps #!rest args)
										 (let* ((m (if (pair? args) (car args) #f))
												  (col (if (> (length args) 1) (cadr args) #f))
												  (pr (lambda (x) (car (prj (list x 0)))))
												  (map-color (if col col (my 'map-color)))
												 ;;(default-color (my 'default-color))
												 )

											(if (kdebug? '(log <tracked-agent>))
												 (begin
													(dnl* "In log-track-segment" (cnc self) (name self))
													(dnl* "track:" (map cdr track))
													(dnl* "projected track:" (map prj (map cdr track)))))

											(if (eq? track #t) (set! track (my 'track))) ;; because we are lazy
											(if (list? track)
												 (let* ((xytrack (map txyz->xy track))
														  (ptrack (map prj xytrack)))
													
													(if (>= (length ptrack) 2)
														 (let ((startseg (list-head ptrack
																							 (1- (length ptrack))))
																 (finishseg (cdr ptrack)))
															(for-each
															 (lambda (n ss fs)
																(ps 'moveto ss)
																(ps 'lineto fs)
																(ps 'push-color map-color) ;; use the unaltered map-color if we are the outermost class
																(ps 'push-color (map-saturate* map-color 0.85 n)) ;; else attenuate the co
																(ps 'stroke)
																)
															 (sequence (length startseg)) startseg finishseg)
															(ps 'Helvetica 4.5)
															(ps 'moveto (prj (list-head (location self) 2)))

															(ps 'push-color map-color)
															(cond
															 ;;; ((not m)
															 ;;;  (ps 'show-centered "*")
															 ;;;  ;;(ps-circle ps  (pr (my 'ps-rad) )
															 ;;; 	;;			 (prj (list-head (location self) 2))
															 ;;; 	;;			 1.2 0.0 )
															 ;;;  )
															 
															 ((procedure? m)
															  (ps 'show-centered (object->string (m)))
															  ;;(ps-circle ps
																;;			 (pr (my 'ps-rad))
																;;			 (prj (list-head (location self) 2))
																;;			 1.2 0.0 )
															  )
															 ((number? m)
															  (ps 'show-centered (number->string m))
															  ;;(ps-circle ps
																;;			 (pr (min (my 'ps-rad)
																;;						(* 0.25 pi
																;;							(sqrt m))))
																;;			 (prj (list-head (location self) 2))
																;;			 1.2 0.0 )
															  )
															 ((string? m)
															  (ps 'show-centered m)
															  ;;(ps-circle ps  (pr (my 'ps-rad))
																;;			 (prj (list-head (location self) 2))
																;;			 1.2 0.0 )
															  )
															 )
															(ps 'pop-color)
															;;; (set! map-color (if (number? map-color)
															;;; 						  (* 0.85 map-color)
															;;; 						  (map (lambda (x) (* 0.85 x)) map-color)))
															(ps 'push-color map-color)
															(ps 'stroke)
															(ps 'pop-color)
															)
														 (or 'This-ought-to-emit-a-splot-for-a-starting-point #t)
														 )
													#t)
												 #f)
											))


(model-method (<plottable> <log-map> <symbol>) (log-data self logger format targets)
				  (dnl* "(model-method (<plottable> <log-map> <symbol>) (log-data self logger format targets)")
				  (if (or (my 'always-log) (and (member format '(ps)) (my 'always-plot)) (emit-and-record-if-absent logger self (my 'subjective-time)))
						(let ((file (slot-ref logger 'file))
								)
						  (kdebug '(log-* log-tracked-agent) ":" targets)
						  (cond 
							((postscript? file)
							 ;; might be a case statement here for different formats within the postscript doc
							 (plot-glyph self logger);; We always plot ourselves at the end.
							 )
							)
						  )
						)
				  )

(model-method (<tracked-agent> <log-map> <symbol>) (log-data self logger format targets)
				  (dnl* "(model-method (<tracked-agent> <log-map> <symbol>) (log-data self logger format targets)")
				  (parent-log-data)
				  (if (or (my 'always-log) (and (member format '(ps)) (my 'always-plot)) (emit-and-record-if-absent logger self (my 'subjective-time)))
						(let ((file (slot-ref logger 'file))
								)
						  (kdebug '(log-* log-tracked-agent) ":" targets)
						  (cond 
							((postscript? file)
							 ;; might be a case statement here for different formats within the postscript doc
							 (file 'comment "Tracking " (name self) " " (taxon self) " " (subjective-time self) " " (location self) " " (agent-state self) )
							 (let ((track (my 'track)) ;; track since last logging
									 (tracks (my 'tracked-paths)) ;; collection of past tracks
									 (prj (composite-prj_src->dst self logger))
									 (datum (if (my 'track-datum) (my 'track-datum) (my 'name)))
									 )

								(if track
									 (begin
										(kdebug '(log-data map) "Calling log-track-segment for current track" track)
										(log-track-segment self track prj file)
										)
									 )
								
								(if tracks
									 (let loop ((n (length tracks))
													(k 1.0)
													(tr tracks))
										(kdebug '(log-data map) "Calling log-track-segment with tracks" tracks)
										(if (not (null? tr))
											 (begin
												;;(log-track-segment self (- n k) (car tr) prj file (+ AMINGREY (* (/ k n) (- AMAXGREY AMINGREY))))
												(log-track-segment self (car tr) prj file)
												(loop (- n 1) (1+ k) (cdr tr))))))
								)
							 )
							)

						  (if (and (my 'tracked-paths)
									  (state-flag self 'tracked-paths))
								(new-track! self))
						  
						  )
						)
				  )


;----- (speed) 
(model-method (<tracked-agent>) (speed self)
					(slot-ref self 'speed))

(model-method (<tracked-agent>)
				  (number-represented self) 1)

;----- (set-speed!) 
(model-method (<tracked-agent> <number>) (set-speed! self n)
 (if (not (number? n))
	  (error "thing:set-speed! -- bad number")
	  (slot-set! self 'speed (magnitude (min n (my 'max-speed))))))


;----- (location) 
;(model-method (<tracked-agent>) (location self)
; (slot-ref self 'location))


;----- (set-location!) 
(model-method (<tracked-agent>) (set-location! self vec)
 (slot-set! self 'location vec))
 

;----- (direction) 
(model-method  (<tracked-agent>) (direction self)
 (slot-ref self 'direction))


;----- (set-direction!) 
(model-method  (<tracked-agent> <point>) (set-direction! self vec)
   (if (pair? vec) 
		 (let ((v (sqrt (apply + (map sqr (map (magnitude vec)))))))
			(if (positive? v)
				 (slot-set! self 'direction (map (lambda (x) (/ x v)) vec))))))

; Vectors should have a length of one.  We do not try and set a direction of "zero"




(model-method (<tracked-agent> <procedure>) (ps-dump self ps projection)
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


(model-body% <tracked-agent>
				(call-all-parents)
				(track-location! self t (my 'location)) ;; even if they
				(set-my! 'speed (min (my 'speed) (my 'max-speed)))
				;; aren't
				;; moving
				'ok
				)


;---- <thing> methods

;;; (default-agent-initialisation <thing> 'dim #f 'location #f
;;;   'direction #f
;;;   'speed #f 'mass #f
;;;   'track #f
;;;   'tracked-paths #f)

;----- (dim) 
(model-method (<thing>) (dim self)
					(slot-ref self 'dim))


;----- (set-dim!) 
(model-method (<thing> <number>)	 (set-dim! self n)
					(if (not (integer? n))
						 (error "thing:set-dim! -- bad integer")
						 (slot-set! self 'dim n)))
;----- (mass) 
(model-method (<thing>) (mass self)
 (slot-ref self 'mass))


;----- (set-mass!) 
(model-method (<thing> <number>) (set-mass! self n)
 (if (or (not (number? n)) (negative? n))
	  (error "thing:set-mass! -- bad number")
	  (slot-set! self 'mass n)))


(model-body% <thing>
				(call-all-parents)
				'ok
				)


(model-method (<thing> <agent> <symbol>) (request-accessor self intruder sym #!rest args)
				  (case sym
					 ((location)
					  (lambda () (location self)))
					 ((mass)
					  (lambda (val) (slot-ref self 'mass)))
					 ((adjust#)
					  (lambda (val) (slot-set! self 'mass (max 0 (+ (slot-ref self 'mass) val)))))
					 (else (parent-request-accessor))
				  ))
				  



; Living things

(model-method (<living-thing> <procedure>) (ps-dump self ps projection)
				  (call-all-parents)
				  (ps 'moveto (projection (local->model (location self))))
				  (ps 'show-table
						(list (string-append "<thing>" (name self))
								(string-append "mass =" (number->string (my 'mass)))
								(string-append "age =" (number->string (my 'age)))
								(string-append "location =" (number->string (my 'location)))
								(string-append "speed =" (number->string (my 'speed)))
								(string-append "direction =" (number->string (my 'direction)))
								))
				  (ps-dump-parent))


(model-method <living-thing> (die self)
				  (set-agent-state! self 'dead)
				  ;;(set! Mortuary (cons self Mortuary))
				  ;; dead things may persist and decay....
				  ;; (kernel 'shutdown self) 
				  'dead)

(model-method <living-thing> (initialise-instance self)
				  (call-all-parents)
				  (set-uninitialised-slots self '(age-at-instantiation) 0)
				  (fail-on-uninitialised-slots self
														 '(age longevity mass-at-age
																 decay-rate
																 ))
				  (if (undefined? (my 'next-env-check))
						(set-my! next-env-check 0))

				  (if (undefined? (my 'env-check-interval))
						(set-my! env-check-interval (my 'ndt))))
										
(model-method <living-thing> (agent-prep self start end)
				  (set-uninitialised-slots self '(age-at-instantiation) 0)
				  (fail-on-uninitialised-slots self '(age mass-at-age))
				  (set-uninitialised-slots self '(mass) ((my 'mass-at-age) (my 'age)))
				  #t
				  )
				  


;(model-method <living-thing> (mass-at-age self #!optional mymass)
;						((my 'mass-at-age) (if mymass mymass (my 'mass))))

(model-method <living-thing> (mass-at-age self #!optional mymass)
				  (let ((M ((slot-ref self 'mass-at-age) (slot-ref self 'age))))
					 (slot-set! self 'mass M)
					 M)
				  ;;; (let ((maa (if (not (uninitialised? (my 'mass-at-age)))
				  ;;; 					  ((my 'mass-at-age) (if mymass mymass (my 'mass)))
				  ;;; 					  (my 'mass))))
				  ;;; 	 (dnl "*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*")
				  ;;; 	 (dnl* (cnc self) (name self) (my 'mass-at-age) (my 'mass) maa)
				  ;;; 	 (set-my! 'mass maa)

				  ;;; 	 (if (and (number? maa) (positive? maa))
				  ;;; 		  maa
				  ;;; 		  (begin
				  ;;; 			 (dnl* (cnc self) (name self) "is missing its Higgs bosons")
				  ;;; 			 (error))))
)



(model-method <living-thing> (growth-rate self #!rest extras) ;; this is allegedly an instantaneous thing...
				  (cond
					((and (has-slot? self 'growth-rate)
							(or (procedure? (my 'growth-rate))
								 (number? (my 'growth-rate))))
					 (let ((gr (my 'growth-rate)))
						(if (number? gr) gr (apply gr extras))))
					 
					((has-slot? self 'mass-at-age)
					 (/ (- ((my 'mass-at-age) (my 'age))
							 ((my 'mass-at-age) (my 'age)))
						 (my 'dt)) ;; Not measured modally
					 )

					(else
					 (/ (- (my 'max-mass) (my 'mass)) (my 'longevity)))))

(define (cmp-by-distance pivot cmp)
  (let ((p (if (list? pivot) pivot (location pivot)))
		  )
	 (lambda (bit1 bit2)
		(set! bit1 (if (list? bit1) bit1 (location bit1)))
		(set! bit2 (if (list? bit2) bit2 (location bit2)))
		(cmp (distance p bit1) (distance p bit2)))))
		  

(model-method% <agent> (look-for self targets)
					(if (null? targets)
						 '()
						 (kernel 'locate (apply *provides-*? targets))))

(model-method% <agent> (look-for* self targets)
					(if (null? targets)
						 '()
						 (kernel 'locate* (apply *provides-*? targets))))

(model-method <agent> (look-for<= self targets)
					(if (null? targets)
						 '()
						 (kernel 'locate<= (apply *provides-*? targets))))

(model-method <agent> (look-for>= self targets)
					(if (null? targets)
						 '()
						 (kernel 'locate>= (apply *provides-*? targets))))


(model-method% <living-thing> (look-for self targets #!optional rad)
				  (if (not rad)
						(if (has-slot? self 'search-radius)
							 (set! rad (my 'search-radius))
							 (set rad +inf.0)))

				  ;; move about looking for a region with whatever it is
				  (let* ((candidates (kernel 'locate (apply *provides-*? targets) rad))
							(results
							 (best-N 4 <= distance)
							;(sort candidates (cmp-by-distance (location self) <=))
							))
					 results
					 ))

(model-method% <living-thing> (look-for* self targets #!optional rad)
				  (if (not rad)
						(if (has-slot? self 'search-radius)
							 (set! rad (my 'search-radius))
							 (set rad +inf.0)))

				  ;; move about looking for a region with whatever it is
				  (let* ((candidates (kernel 'locate* (apply *provides-*? targets) rad))
							(results
							 (best-N 4 <= distance)
							 ;;(sort candidates (cmp-by-distance (location self) <=))
							))
					 results
					 ))

(model-method <living-thing> (look-for<= self targets #!optional rad)
				  (if (not rad)
						(if (has-slot? self 'search-radius)
							 (set! rad (my 'search-radius))
							 (set rad +inf.0)))

				  ;; move about looking for a region with whatever it is
				  (let* ((candidates (kernel 'locate<= (apply *provides-*? targets) rad))
							(results
							 (best-N 4 <= distance)
							 ;;(sort candidates (cmp-by-distance (location self) <=))
							))
					 results
					 ))

(model-method <living-thing> (look-for>= self targets #!optional rad)
				  (if (not rad)
						(if (has-slot? self 'search-radius)
							 (set! rad (my 'search-radius))
							 (set rad +inf.0)))

				  ;; move about looking for a region with whatever it is
				  (let* ((candidates (kernel 'locate>= (apply *provides-*? targets) rad))
							(results
							 (best-N 4 <= distance)
							 ;;(sort candidates (cmp-by-distance (location self) <=))
							))
					 results
					 ))

(model-body% <living-thing>
				(call-all-parents)
				'ok
				)



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

;;; 				  (parent-initialise-instance)
;;; 				  ;; call "parents" last to make the initialisation list work
;;; 				  (set-state-variables self args) ;; we now set-state-variables the slot values passed in args
;;; 				  )


(model-method (<blackboard> <symbol>) (query self cmd #!rest args)
				  ;; args should either be a list of tags (for 'erase and 'read)
				  ;; or a list of pairs consisting of tags and values (for 'write)
				  

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
;;				(call-all-parents)
				'ok
				)


;; --- <service-agents>

;; (define sa (make-agent <service-agent> 'name "Wally" 'value 42 'sym 'blargh 'delta-T-max 240))
(define (make-service-agent printname symbol val max-dt #!optional extern-get extern-set! external-agents)
  (make-agent <service-agent> 'name printname 'sym symbol 'value val 'delta-T-max max-dt 'ext-get! extern-get 'ext-set! extern-set! 'external-rep-list external-agents)
  )


(model-method (<service-agent>) (symbol self) ;; service-agents can *only* provide their single service
				  (slot-ref self 'sym))

(model-method (<service-agent>) (provides? self sym)
				  (service? self sym))

(model-method (<service-agent> <symbol>) (service? self sym)
				  (or (eqv? (my 'sym) sym) (eqv? (my 'name) sym)))

(model-method (<service-agent> <pair>) (service? self symlist)
				  (or (member (my 'sym) symlist) (member (my 'name) symlist)))

(model-method (<service-agent>) (value self)
				  (if (not (and (slot-ref self 'running-externally)
									 (pair? (slot-ref self 'external-rep-list))
									 (slot-ref self 'ext-get)))
						(my 'value)
						(let ((accumulator 0))
						  (apply + 
									(map
									 (lambda (x)
										((slot-ref self 'ext-get) x)
										)
									 (slot-ref self 'external-rep-list))
						  ))
						)
				  )
(model-method (<service-agent>) (set-value! self val)
				  (set-my! 'value val)
				  (let ((asa (my 'active-subsidiary-agents)))
					 (if (pair? asa) 
						  (let ((L (length asa)))
							 (for-each
							  (lambda (a)
								 (UNFINISHED-BUSINESS "This is primitive and clunky")
								 (slot-set! a 'value  (/ val L))) 
							  asa))
						  ))
				  )


(model-method (<service-agent> <log> <symbol>) (log-data self logger format targets)
				  (dnl* "(model-method (<service-agent> <log> <symbol>) (log-data self logger format targets)")
				  (let ((kdebug (if #t kdebug dnl*))
						  ;;(f (if (pair? args) (car args) #f))
						  ;;(p (if (and (pair? args)
						  ;;			  (pair? (cdr args)))
						  ;;		(cadr args)
						  ;;		#f))
						  )
					 (kdebug '(log-horrible-screaming service-agent log-service-agent) (cnc self) (cnc format) (cnc (my 'name)))
					 (if (or (my 'always-log) (and (member format '(ps)) (my 'always-plot)) (emit-and-record-if-absent logger self (my 'subjective-time)))
						  (let ((file (slot-ref logger 'file)))
							 (kdebug '(log-* log-service-agent)
										"[" (my 'name) ":" (cnc self) "]"
										"in log-data")
							 (let ((leading-entry #f))
								(for-each
								 (lambda (field)
									(kdebug '(log-* log-service-agent) "[" (my 'name) ":"
											  (cnc self) "]" "checking" field)
									(if (has-slot? self field)
										 (let ((r (slot-ref self field)))

											(case format
											  ((ps)
												(file 'push-font (my 'default-font) (my 'default-size))
												(if (and (my 'always-log) (< t (my 'subjective-time)))
													 (file 'show "["))
												(file 'show (string-append " "
																					(if (string? r)
																						 r
																						 (object->string r)) " "))
												(if (and (my 'always-log) (< t (my 'subjective-time)))
													 (file 'show "]"))
												(file 'pop-font)
												)
;								 ((dump)
;								  (with-output-to-port file
;										(lambda ()
;										  (dump self))))

											  
											  ;;; ((text table dump)
											  ;;; 	(let ((S (with-output-to-string '()
											  ;;; 											  (lambda ()
											  ;;; 												 (let ((show-field-name
											  ;;; 														  (slot-ref logger 'show-field-name))
											  ;;; 														 (missing-val
											  ;;; 														  (slot-ref logger 'missing-val))
											  ;;; 														 )
											  ;;; 													(if show-field-name
											  ;;; 														 (begin
											  ;;; 															(if leading-entry 
											  ;;; 																 (display " ")
											  ;;; 																 (set! leading-entry #t))
											  ;;; 															(display field)))
																								
											  ;;; 													(let ((val (if (eqv? field 'name) 
											  ;;; 																		(if (slot-ref self 'patch)
											  ;;; 																			 (string-append
											  ;;; 																			  (slot-ref
											  ;;; 																				(slot-ref self 'patch) 
											  ;;; 																				'name) ":" (name self))
											  ;;; 																			 (name self))
											  ;;; 																		(if (has-slot? self field)
											  ;;; 																			 (slot-ref self field)
											  ;;; 																			 (slot-ref logger
											  ;;; 																						  'missing-val)))))
											  ;;; 													  (if leading-entry 
											  ;;; 															(display " " file)
											  ;;; 															(set! leading-entry #t))
											  ;;; 													  (display val))
											  ;;; 													)
											  ;;; 												 )
											  ;;; 											  )))
												  
											  ;;; 	  (display S file)))
											  ((text table dump)
												(let ((show-field-name
														 (slot-ref logger 'show-field-name))
														(missing-val
														 (slot-ref logger 'missing-val))
														)
												  (if show-field-name
														(begin
														  (if leading-entry 
																(file 'show " ")
																(set! leading-entry #t))
														  (file 'show field)))
												  
												  (if (and (my 'always-log) (< t (my 'subjective-time)))
														(file 'show "["))
												  (let ((val (if (eqv? field 'name) 
																	  (if (slot-ref self 'patch)
																			(string-append
																			 (slot-ref
																			  (slot-ref self 'patch) 
																			  'name) ":" (name self))
																			(name self))
																	  (if (has-slot? self field)
																			(slot-ref self field)
																			(slot-ref logger
																						 'missing-val)))))
													 (if leading-entry 
														  (file 'show " ")
														  (set! leading-entry #t))
													 (file 'show val))
												(if (and (my 'always-log) (< t (my 'subjective-time)))
													 (file 'show "]"))
												  
												  )
												)
											  
											  (else
												(kdebug '(log-* log-service-agent)
														  "[" (my 'name) ":" (cnc self) "]"
														  "Ignoring " field " because I don't have it")
												'ignore-unhandled-format)))
										 (begin
											(kdebug '(log-* log-service-agent)
													  "[" (my 'name) ":" (cnc self) "]"
													  "no service" field)
											#f)))
								 (uniq (if #t
											  targets
											  (filter (not-member (slot-ref logger 'dont-log))
														 targets)))
								 )
								(file 'newline)
								)
							 )
						  )
					 )
				  )


(model-method <service-agent> (dump% self count)
				  (display (make-string count #\space))
				  (display "<service-agent>\n")

				  (let* ((slots (class-slots-of self))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (display (make-string (+ 2 count) #\space))
									 (display x)
									 (display ": ")
									 (display y)
									 (newline))
								  slots vals)))


(model-method <service-agent> (number-represented self)
				  (slot-ref self 'value))

;---- query & set

(define (ext-get-func self)
  (lambda (other)
	 (cond
	  ((eqv? (slot-ref other 'sym) (slot-ref self 'sym)) (value other))
	  (#f 0)
	  (#t (error "bad request to external service-agent agent"))))
  )

(define (ext-set!-func self v)
  (lambda (other)
	 (cond
	  ((eqv? (slot-ref other 'sym) (slot-ref self 'sym)) (set-value! other v))
	  (#f 0)
	  (#t (error "bad request to external service-agent agent"))))
  )

(define (ext-add!-func self v)
  (lambda (other)
	 (cond
	  ((eqv? (slot-ref other 'sym) (slot-ref self 'sym)) (add! other v))
	  (#f 0)
	  (#t (error "bad request to external service-agent agent"))))
  )

(model-method (<service-agent>) (add! self val)
				  (let ((v (my 'value))
						  (asa (my 'active-subsidiary-agents)))
					 (if (number? v)
						  (begin
							 (set-my! 'value (+ v val) )
							 (if (pair? asa) 
								  (let ((L (length asa)))
									 (for-each
									  (lambda (a)
										 (slot-set! a 'value  (+ (slot-ref a 'value) (/ val L))) )
									  asa))
								  ))
						  (abort "service-agent:add!: value is not a number")
						  )))


(model-body% <service-agent>
						(kdebug '(model-bodies ecoservice-running) (cnc self) (my 'name)  "@"  t)
						(let ((h (slot-ref self 'history)))
						  (if h
								(slot-set! self 'history
											  (cons (cons t (my 'value)) h)))
						  )
						
						;;(dnl* (my 'name) (my 'sym) (my 'value))

						(call-all-parents) ;; chain to <agent>
						;;(parent-body)
						dt
						)


;---- environment methods

(model-body <environment> ;; does nothing.
				;;				(call-all-parents)
				'ok
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
(model-method (<environment> <point>) (value self loc)
				  (my 'default-value))

(model-method (<environment> <point>) (set-value! self loc val)
				  (set-my! 'default-value val))









;####################################################################
"Below this point are the routines that underpin the process of 
running agents and haaving them interact with the kernel.  They come at the 
end of the file because it's a readily located place.
"

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


;; Generally, model-body methods know about "self" "t" "dt" "kernel"
;; and all an agents state variables.  The variable all-parent-bodies
;; is a list of the "model-body" methods for all of its parents,
;; rather than just the ones that appear first in the inheritance
;; lists.  This *particular* version of the routine should not call
;; parent-body.

;; if it ever hits the agent body, it just returns the dt passed in.

(add-method run-model-body (make-method (list <agent> <number> <number>)
													 (lambda (run-parent self T dt) 'ok))) 

"
The 'run' method does the actual business of chaining to the model body, managing the update of 
the agent's subjective time, ensuring that agents with timestep schedules do not accidentally 
run past a scheduled tick, enforcing the arrow of time, catching some types of error (some of 
which would also be caught in run-agent).

'run' also provides the machinery for supporting subsidiary agents and for running maintenance 
routines which keep state data for representations which are no-longer current.
"

;; *** AGENTS RUN HERE ***
(add-method
 run
 (make-method
  (list <agent>)
  (lambda (run-parent self T pstop pkernel)
	 (kdebug 'bigseparatorfor-run "###################################################################################")
	 (kdebug 'bigseparatorfor-run "## Running"  (name self) "in " T pstop "with" (slot-ref self 'timestep-schedule) "pending\n")
	 (kdebug 'bigseparatorfor-run "##       @"  (subjective-time self) "+" (modal-dt self))
	 (let ((my (lambda (x) (slot-ref self x)))
			 (set-my! (lambda (x y) (slot-set! self x y)))
			 (kernel (slot-ref self 'kernel)))

		;;		(if (has-slot? self 'introspection-targets)
		;;			 (slot-set! self 'introspection-targets (pkernel 'find-agents (slot-ref self 'introspection-selector))))

		;;(set-kernel! self pkernel)
		(kdebug '(run run-model-body) "RUNNING <agent>" (cnc self))
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
				  (let ((dt (interval t (modal-dt self) stop ttr)))
					 (let ((subj-time (my 'subjective-time)))
						(if (< (+ t dt) subj-time)
							 (begin
								(dnl* "Trying to go back in time" (cnc (class-of self)) (slot-ref self 'name) (slot-ref self 'taxon) '@ t "+" dt "with a subjective time of" subj-time)
								(kdebug (list 'run (slot-ref self 'name) (slot-ref self 'taxon)) "... running from" t "to" (+ t dt) ", a step of" dt)
								'wait ;; report it and just skip this attempt
							 ))

						(let ((DT 0))
						  (set-my! 'kernel kernel)
						  (if (= t subj-time)
								(set! DT dt)
								(begin
								  (dnl* "Adjusting dt" (cnc (class-of self)) (slot-ref self 'name) 'st subj-time 'T t DT dt)
								  (set! DT (- (+ t dt) subj-time)))
								)
						  ;;(dnl* "TIME" (name self) (subjective-time self) t dt DT)
						  (let ((return-this
									(cond
									 ((< subj-time t) ;; The agent is really behind where it ought to be
									  (if temporal-fascist
											(begin
											  (kdebug 'temporal-check "[" (my 'name) ":"(cnc self) "]"
														 "a/an" (my 'representation)
														 "is lost in time at" subj-time "or" t)
											  'missing-time)
											(run run-parent self subj-time (+ t DT) pkernel)))
									 ((and (> DT 0.) (>= subj-time (+ t DT)))
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
												 dt "/" DT)

							 ;;; (let ((m (if (isa? self <agent>)
							 ;;; 				  (run-model-body self t dt)
							 ;;; 				  dt)))
									  (let ((m (if (isa? self <agent>)
														(let ((result (run-model-body self t DT))) ;;; The model runs before its subsidiaries or components
														  ;(dnl* "MODEL BODY FOR" (cnc self) "RETURNED" result)  
														  ;;  The model returns the amount of time it actually ran for
														  ;;(dnl* " # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #")
														  ;;(dnl* "Got result" result)

														  (kdebug '(nesting run-model-body) (cnc self)
																	 "Running at " t "+" DT "[" (modal-dt self) "]")
														  (cond
															((and (number? result) (< result 0.0)) (error 'bad-dt))
															((and (number? result) (> result DT)) (error 'executed-too-long!))
															((and (number? result) (<= DT result))
;															 (set! self 'subjective-time (+ (slot-ref self 'subjective-time) DT))
															 (set! result 'ok)
						 									 DT
															 )
															((eq? result 'ok)
;															 (set! self 'subjective-time (+ (slot-ref self 'subjective-time) DT))
															 DT
															 )
															((member (if (pair? result) (car result) result) '(introduce spawn remove))
															 ;; agent is returning a queue management op
															 result
															 )
															(#t
															 "not a number, probably a symbol or symbol and list"))
														  
														  (kdebug 'track-subjective-times
																	 "[" (my 'name) ":" (cnc self) "]"
																	 " running at " (my 'subjective-time) ":" t)
														  
														  (if (pair? (my 'active-subsidiary-agents))       ;;; Any agent can act as a kernel
																(run-subsidiary-agents self t DT run kernel))
														  
														  (if (pair? (my 'maintenance-list))
																(let ((ml (my 'maintenance-list)))
																  (dnl* "Need to get maintenance values")
																  ;; Now run any agent representations which
																  ;; have indicated that they need data
																  ;; maintained
																  (for-each
																	(lambda (x)
																	  (x t DT self))
																	ml)
																  ))


														  ;; deal with any changes in the entity's
														  ;; representation, or the general configuration of the
														  ;; model as a whole

														  ;; prefix a symbol ('migrate, for
														  ;; example) to the return value if it needs to change,
														  ;; last bit should be "return"
														  (if restricted-kernel-access (set-my! 'kernel #f))
														  result)
														'not-an-agent)
												  ))

										 (kdebug 'run "Returning " m " to run-agent")
										 (if (number? m) 'ok m)
										 ))
									 ))
								  )
							 ;;(dnl* " | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |")
							 ;;(dnl* "Returning return-this" return-this)

							 (kdebug 'trace-time-update "SUBJECTIVE TIME FOR" (name self) "IS" (slot-ref self 'subjective-time) "; t =" t "DT =" DT "; dt =" dt)
							 (kdebug 'trace-time-update "has-slot? subjective-time" (has-slot? self 'subjective-time))

							 (if (has-slot? self 'subjective-time) ;; Anything derived from <agent> should have a subjective-time and have it updated
								  (begin
									 (kdebug '(run trace-time-update) "UPDATING...")
									 (slot-set! self 'subjective-time (+ DT (my 'subjective-time)))
									 (kdebug '(run trace-time-update) "..." (slot-ref self 'subjective-time))
									 )
								  (END
									(dnl* "SO WHAT TIME DO *YOU* /THINK/ IT IS?."))
								  )

							 (slot-set! self 'timestep-schedule (sort (cons (+ t DT) (slot-ref self 'timestep-schedule)) <))

;;							 (dnl* kdebug '(nesting run) (cnc self) (name self)
;;										"Leaving run after a tick of " DT " @ " (my 'subjective-time)
;;										"[" (modal-dt self) "] returning " return-this )
							 return-this)))))))
		))
	 ))
 )

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
							d	dead)
				  ;; Do I need a dead-agent class and "clean  ?
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
					 (if (not (null? al) kernel)
						  (run-agents self t dt al run ))
					 ))	




;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
