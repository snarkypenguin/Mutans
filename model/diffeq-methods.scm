; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	diffeq-methods.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.12.07
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/diffeq-methods.scm
;
;	History:
;
;; Commentary: This is a relatively small class -- it doesn't maintain
;; variables itself, rather we pass in closures that know how to get and
;; set the values in the variables of interest.  The actual names/symbols
;; used in the equations here may bear no relation at all to the sources 
;; of the data --- all correspondences are an artifact of the data used
;; to initialise the variable-values, get-externals and external-update

(include "framework")

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
;; 							  near-food-attraction searchspeed
;; 							  wanderspeed foragespeed	
;; 							  movementspeed foodlist homelist
;; 							  breedlist habitat
;; 							  )
;; 			  )
;; 	  )
;; 	(initialise-parent) ;; call "parents" last to make the
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
THIS IS THE PREFERRED VERSION.

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




;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
