; -*- mode: scheme; -*-

;-  Identification and Changes

;--
;	specific-model.scm -- Written by Randall Gray 

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 
(include "remodel-framework") ;; This provides a number of useful bits

;- =================  Define the model domain now =======================
;========================================================================

"The model domain must be defined before the model itself is loaded."

;-- First,  space

;; (add-kdebug-msg-tag 'page-boundary)
;; (add-kdebug-msg-tag (construct-symbol '<jherb> 'methods))
;; (add-kdebug-msg-tag (construct-symbol '<aherb> 'methods))
;; (add-kdebug-msg-tag (construct-symbol '<acarn> 'methods))
;; (add-kdebug-msg-tag '<animal>-methods)
;; (add-kdebug-msg-tag '<tracked-agent>-methods)
;; (add-kdebug-msg-tag '<thing>-methods)
;; (add-kdebug-msg-tag (construct-symbol '<jherb> 'model-body))
;; (add-kdebug-msg-tag (construct-symbol '<aherb> 'model-body))
;; (add-kdebug-msg-tag (construct-symbol '<acarn> 'model-body))
;; (add-kdebug-msg-tag '<animal>-model-body)
;; (add-kdebug-msg-tag '<tracked-agent>)
;; (add-kdebug-msg-tag '<thing>-model-body)
;; (add-kdebug-msg-tag '<tracked-agent>)

(kdebug-production! #f) ;; suppress debugging messages
(kdebug-wildcards! #t)
(add-kdebug-msg-tag '*log*)
(add-kdebug-msg-tag '*page*)
(add-kdebug-msg-tag '*emit*)

(kdebug-production! #t)

;(define Model-Domain '((-500 -500 -50) (500 500 50)))
(define Model-Domain '((0 0 -50) (1000 1500 50)))
(define Model-Area (apply * (list-head (cadr Model-Domain) 2)))

;(define Model-Domain '((0 0 -50) (200 300 10)))
;; takes the ll, ur and any z component
(set-model-domain! (car Model-Domain) (cadr Model-Domain) isoA4)

(define domain-centre (list-head (map + (map (lambda (x) (/ x 2)) (map - (cadr Model-Domain) (car Model-Domain))) (car Model-Domain)) 2))

"Note that the parameter files are all loaded at this point, references to
mapping functions that are generated by the set-model-domain will not be 
accessible (and will cause a load failure) unless delayed evaluation is used
and the state variable is assigned a value returned by an (eval ..), like so:

	????
"

;;(set-model-domain '(-5040 -5040 -100) '(5040 5040 3000) isoA4)
;; ll ur optional-pagesize
;(set-model-domain '(-5040 -5040 -100) '(5040 5040 3000) isoA4r)
;; rotated


; (map (lambda (x) ((mapf (slot-ref pslog 'model->local)) x)) (map perimeter patchlist))

(define PLOT-SCALE 10)


;-- generate output mappings (must happen after the model domain is set
;;  using set-model-domain

;-- ...and time

(define start 0) ;; day zero
;(define end (* 5.1 days))
;(define end (* 41 days))
(define end (* 15 days)) ;; of christmas y'know
;(define end (* 12 days)) ;; of christmas y'know
;(define end (* 29 days)) 
;(define end (* 51 days)) ;; 
;(define end (* 61 days)) ;; 
;(define end year)
;(define end (* 3 years)) ;; 
;(define end (* 480 days)) ;; 
;(define end (* 72 years)) ;;

(if (< end start)
	 (error "The model doesn't work in that direction")
	 (dnl* "***** Running from" start "to" end))

;(disable-timers)



;--  the venue and cast....
(dnl "The stage") ;-------------------------------------------------------------------

(define N 2) ;; Number cells along the "east-west" axis
(define M 3) ;; Number cells along the "north-south" axis

(define use-plants #t)
(define use-jherb #t)
(define use-aherb #t)
(define use-acarn #f)

(define patch-carrying-capacity  25)
;; after this many trees in a patch, the reproductive potential is attenuated
;; Probably ought to be biomass, but this is quicker.


;; calculate area of patch, divide by number of clusters,
;; calculate radius of a circle with the same area and use that as the basis
;; for the clustering radius

(define cluster-radius (* 50 m))
(define seeding-radius (* cluster-radius 2.5))


(define bigtrees #t)
(define treescale 1)

'(wasteland onetree trees array array+trees)
;(define tree-configuration 'wasteland)
;(define tree-configuration 'bush)
;(define tree-configuration 'array)

(define tree-configuration 'array+trees)

(define chains-per-patch (* treescale 1))
(define clusters-per-chain (* treescale 1))
(define trees-per-cluster (* treescale 1))


(dnl "The set") ;-------------------------------------------------------------------

;--- First the cells, implemented as patches with water, fruit and seeds variables

(define initial-fruit-m^2  3)

(define ptax "B.exemplarii")

'(dnl* 'FRUIT  (* (numeric-parameter-lookup <example-plant> ptax 'fruit-mass) initial-fruit-m^2 (/ Model-Area (* M N))))

(define patchgrid
  (let* ((grid (make-grid <patch> "ch3cell" "gridcell"
					  'area ;; area is the representation
					  N M ;; array 
					  Domain ;; geographic ll and ur
					  (lambda (x y) ;; vertical displacement function
						 (let ((x (/ x 10))
								 (y (/ y 10)))
							(* 10 (sqrt (+ (* x x) (* y y))))))
					  'dt (* 1 days)
					  'caretaker
 					  (lambda (self t dt)
						 ;; The number of seeds is attenuated by the ecoservice;
						 ;; here, we potentially germinate some....
						 
						 ;; Filter the non-alive plants
						 (slot-set! self 'memory
										(let loop ((onpad (slot-ref self 'memory))
													  (nnpad '()))
										  (cond
											((null? onpad) (reverse nnpad))
											((and (is-class? (car onpad) <plant>)
													(eq? (agent-state (car onpad)) 'alive))
											 (loop (cdr onpad) (cons (car onpad) nnpad)))
											((and (list? (car onpad)) (eq? (caar onpad) 'alive))
											 (loop (cdr onpad) (cons (car onpad) nnpad)))
											(else (loop  (cdr onpad) nnpad)))))

						 (let* ((seeds (value self 'seeds))
								  (fruit (value self 'fruit))
								  (^4 (lambda (x) (square (square x))))
								  (competition
									(^4
									 (min 1. (/ patch-carrying-capacity
													(+ 1 (length (slot-ref self 'memory)))))))
								  (sgerminated
									(max
									 0 (+ -1
											(inexact->exact
											 (truncate
											  (* seeds (- 1 (exp (- (* competition 0.001 (/  dt year)))))))))))
								  (fgerminated
									(max
									 0 (+ -1
										   (inexact->exact
											 (truncate
											  (* fruit (- 1 (exp (- (* competition 0.0001 (/  dt year)))))))))))
								  (caretaker (list <agent> 'name "Treebeard" 'taxon 'O.tolkienii))
								  ;; Onodrim tolkienii -- only here because KCALL needs something in the role.
								  )
							'(dnl* "Taking care" (name self) 't t 'Δ dt "|-- competition" competition
									 "--|" seeds '+ fruit 'i '∈ (/ Model-Area (* M N)) "m^2"
									 '==> sgerminated '+ fgerminated 'i)

							(if (> (+ sgerminated fgerminated) 0)
								 (begin
									(if #t (let* ((nseeds (- seeds sgerminated))
													  (tnfruit (- fruit fgerminated))
													  (nfruit (if (< 0.5 tnfruit) 0 tnfruit)))
												
												(set-value! self 'seeds nseeds)
												(set-value! self 'fruit nfruit))
;(dnl* 'TRACKING (value self 'seeds) (value self 'fruit))
										 )
									
									(let ((K (min patch-carrying-capacity
													  (max 0 (+ sgerminated (magnitude fgerminated))))))
									  ;; Clip the maximum number that can germinate 
									  (if #t
											(begin
											  (dnl* "Caretaker| total number of seeds and fruit:"
													  (+ seeds (* fruit 0+1i)))
											  (dnl* "         |                      germinates:"
													  K (+ sgerminated (* fgerminated 0+1i)))
											  (dnl* "         |                Number of plants:"
													  (length (KCALL caretaker 'locate*
																		  (*is-class? <example-plant>))))
											  (dnl* "         |        Number of plants in array:"
													  (apply + (map (lambda (a)
																			(if a (length (slot-ref a 'data)) 0))
																		 (KCALL caretaker 'locate*
																				  (*is-class? <plant-array>) Q)))
													  )

											  
											  ))
									  
									  (if #t (let* ((age (lambda () ;; this keeps the place from looking too uniform
																  (* (+ 3 (random-real)) weeks))) 
														 (mass (lambda (a) ;; individual life-history perturbations
																	(* (+ 1 (* 0.1
																				  (- (random-real) 0.5)))
																		(B.ex-mass a)))) 
														 (lai (numeric-parameter-lookup <example-plant> ptax 'lai))
														 )
												  
												  (if #f
														(begin
														  (list 'introduce
																  (map
																	(lambda (i)
																	  (let* ((A (age))
																				(plant
																				 (create
																				  <example-plant> ptax
																				  'location (random-point self)
																				  'domain self 
																				  'age A 'mass (mass A)
																				  'plot-scale PLOT-SCALE
																				  ;; to make them easier to see
																				  )))
																		 (slot-set! self
																						'memory 
																						(cons plant
																								(slot-ref self 'memory)))
																		 ;; each patch has a list of trees so we can
																		 ;; keep density dependence effects
																		 plant
																		 ))
																	(seq K))
																  )
														  )
														(let* ((F-list (KCALL caretaker 'locate* "B.exemplarii"))
																 (forest (filter (*is-class? <plant-array>) F-list)))
														  (for-each
															(lambda (i)
															  (if (and forest (not (null? forest)))
																	(let* ((A (age))
																			 (M (mass A))
																			 (plant
																			  (list 'alive M M A
																					  (random-point self)
																					  (general-leaf-area M lai) 0 0 self 0)))
																	  (add-data-record (car forest) plant)
																	  (slot-set! self 'memory
																					 (cons plant (slot-ref self 'memory))))
																	;; each patch has a list of trees so we can
																	;; keep density dependence effects
																	;;(dnl* "NO FOREST")
																	)
															  )
															(seq K))
														  'ok
														  ))
												  'ok)
											'ok)
									  )
									)
								 ;; else nothing to do
								 )
							) ;; end of let*?
						 ) ;; end of caretaker lambda
					  ) ;; end of make-grid
					))
	 grid))

(define patchlist (patch-list-from-cover patchgrid))

;; We insert the patches into the queue so that their subjective time is incremented.
;; Bog standard patches don't *do* anything, so it is really just cosmetic ... if they
;; were dynamic-patches, it would be quite a different matter!

(for-each ;; Insert each patch into the queue
 (lambda (p)
	(set! Q (q-insert Q p Qcmp)))
 patchlist)

(dnl "Arden") ;-------------------------------------------------------------------


(define trees '())
"Clusters of trees are seeded randomly and without regard to the footprint of the patches.
when each tree is created, we check which patch it is in, and assign it appropriately.

Initially a cluster location within the domain is selected in a uniformly random way, the 
next cluster location is termined by a pprnd jump from the centroid of the first, and so
on for a chain with a lenght equal to the number of cells we are using.
"

(define  (make-daisy-chain domain scale n lst )
  (cond
	(#f (list (list-head (apply random-point-in-box domain) 2)))
	((<= n 0) lst)
	((null? lst)
	 (let ((p (list-head (apply random-point-in-box domain) 2)))
		(make-daisy-chain (list-head domain 2) scale (- n 1) (list p))
		))
	(else
	 (let loop ((p (map + (car lst) (map (lambda (x) (+/- (pprnd scale))) (make-list (length (car lst)) 0)) )))
		(if (Valid-location p)
			 (make-daisy-chain domain scale (- n 1) (cons p lst))
			 (loop (map + (car lst) (map (lambda (x) (+/- (pprnd scale))) (make-list (length (car lst)) 0)) ))
			 )
		))
	))


(define initial-loci
  (let* ((nC (apply + (map (lambda (x) (inexact->exact (round (nrnd clusters-per-chain)))) (make-list (* N M chains-per-patch) 0))))
			(nT (apply + (map (lambda (x) (inexact->exact (round (nrnd trees-per-cluster)))) (make-list nC 0))))
			(chains
			 (map (lambda (x) (make-daisy-chain Domain cluster-radius clusters-per-chain '()))
					(make-list (* N M chains-per-patch) '())))
			)
	 (apply append chains)))


(define Loci
  (map (lambda (pt)
			(cons pt (filter (lambda (pch) (contains? pch pt)) patchlist)
			))
		 initial-loci))


(set! trees
		(if use-plants
			 (case tree-configuration
				((wasteland)
				 '())
				((onetree)
				 (list
				  (create <example-plant> ptax
							 'age (* 36 years)
							 'age-at-instantiation (* 36 years)
							 'location domain-centre
							 'domain (car patchlist) 'domain (car patchlist)
							 'plot-scale PLOT-SCALE
							 )
				  t)) ;; mass is set using the mass-at-age function in the agent-prep stage
				
				((array+trees)
				 (let ((k (length Loci))
						 (n (inexact->exact (truncate (/ (length Loci) 2)))))
					(dnl* "Making" k "trees:" n "as <plant-array> vectors," (- k n) "as <plants>")
					(cons 
					 (create <plant-array> ptax
						'provides '(vegetation)
									
						;; this *should* come from the parameter file
									
						'data (map (lambda (loc)
										 (let* ((mass-at-age
													(procedure-parameter-lookup
													 <example-plant> ptax 'mass-at-age))
												  (age (+ 10 (pprnd (* 4 years))))
												  (mass (mass-at-age age))
												  (area (general-leaf-area
															mass
															(numeric-parameter-lookup
															 <example-plant> ptax 'lai)))
												  (damage (pprnd (* 0.5 area)))
												  )
											(list 'alive mass mass age (car loc)
													area damage 0.0 (cadr loc) 0)))
									  (list-tail Loci n)))
					 (map (lambda (x) ;; x marks the spot
							  (create <example-plant> ptax 'location (car x) 'domain (cadr x) 'domain (cadr x)
										 'plot-scale PLOT-SCALE)
							  ) ;; mass is set using the mass-at-age function in the agent-prep stage
							(list-head Loci n))
					 ))
				 )
				((array)
				 (list (create <plant-array> ptax
									'provides '(vegetation)
									
									;; this *should* come from the parameter file
									
									'data (map (lambda (loc)
													 (let* ((mass-at-age (procedure-parameter-lookup <example-plant> ptax 'mass-at-age))
															  (age (+ 10 (pprnd (* 4 years))))
															  (mass (mass-at-age age))
															  (area (general-leaf-area mass (numeric-parameter-lookup <example-plant> ptax 'lai)))
															  (damage (pprnd (* 0.5 area)))
															  )
														(list 'alive mass mass age (car loc) area damage 0.0 (cadr loc) 0)))
												  Loci)))
				 )
				
				(else
				 (map (lambda (x) ;; x marks the spot
						  (let ((p (create <example-plant> ptax 'location (car x)
												 'domain (cadr x) 'domain (cadr x)
												 'plot-scale PLOT-SCALE)
									  ))
							 (if (not (is-class? <example-plant>))
								  (bad seeds))
							 ;; mass is set using the mass-at-age function in the agent-prep stage
							 ))
						Loci)
				 ))
			 '())
		)

(dnl* "================>" (map cnc trees))

(for-each (lambda (t) (set! Q (q-insert Q t Qcmp))) trees)

(define N-trees (apply + (map (lambda (x) (cond ((is-class? x <plant>) 1) ((is-class? x <plant-array>) (length (slot-ref x 'data))) (else 0))) trees)))

(dnl* "The rangeland contains" N-trees "plants")
  
;; (make-copse (lambda (t)
;; 					 (set! Q (q-insert Q t Qcmp))
;; 					 (set! trees (cons t trees))
;; 					 (display "^"))
;; 				  <example-plant> ptax
;; 				  (nrnd trees-per-cluster 1.0 1 (* 2 trees-per-cluster))
;; 				  p
;; 				  (lambda () (pprnd (* 0.1 B.ex-longevity) B.ex-longevity)) ;; age
;; 				  C
;; 				  (* (radius p) 1/3))
;; )



(dnl " ... and their fruit")

(define ecological-services '())

;;(dnl* "       Using seed-death of " (numeric-parameter-lookup <> ptax 'seed-death))
;;(dnl* "        and fruit-decay of " (numeric-parameter-lookup <> ptax 'fruit-decay))

(for-each
 (lambda (c)
	(let ((seed-death (numeric-parameter-lookup <> ptax 'seed-death))
			(fruit-decay (numeric-parameter-lookup <> ptax 'fruit-decay)));; This is how to get a parameter from a taxon specific file ) ;; You can use numeric-... string-... symbol-... and list-parameter-lookup
	  ;;                               taxon  dt         name         variable value cap r maxdt growing? growthmodel . patch
	  (let ((seeds (simple-ecoservice "seeds" (* 4 days) "seeds" 'seeds (nrnd (* 1.1 initial-fruit-m^2 (/ Model-Area (* N M)))) +inf.0 0 (* 6 hour) #t
												 ;;(lambda (t dt val) (max 0 (* val (if seed-death (truncate (exp (* seed-death dt))) 1))))
												 (lambda (t dt val) (max 0 (truncate (* val (if seed-death (exp (* seed-death dt))) 1))))
												 c))

			  (fruit (simple-ecoservice "fruit" (* 2 days) "fruit" 'fruit (pprnd (* initial-fruit-m^2 (/ Model-Area (* N M)))) +inf.0 0 (* 6 hour) #t
												 (lambda (t dt val) (max 0 (* val (if fruit-decay (exp (* fruit-decay dt)) 1))))
												 c))
			  )
		 (slot-set! seeds 'history #t)
		 (slot-set! fruit 'history #t)

		 ;;(dnl*  (name c) "has"  (value seeds) "seeds and" (value fruit) "fruit")

		 (set! ecological-services (cons seeds ecological-services))
		 (set! ecological-services (cons fruit ecological-services))
		 ;;		 (set! ecological-services (cons jcarn ecological-services))
		 
		 (set! Q (q-insert Q seeds Qcmp))
		 (set! Q (q-insert Q fruit Qcmp))

		 ;;		 (set! Q (q-insert Q jcarn Qcmp))
		 
		 ;;		 (slot-set! c 'service-list (list fruit seeds jcarn)))))
		 (slot-set! c 'service-list (list seeds fruit)))))
 patchlist)

(newline)

(dnl "The players")

;; Herbivores and carnivores --------------------------------------------

;; ----- herbivores -----
;; Adult herbivores eat trees, juveniles only eat fruit.  We start with
;; 1/4 as many juveniles as adults.

(dnl "NOT DONE YET (specific-model.scm)")

(define HJ '())
(define HA '())
(define CA '())

(define use-animals-per-patch #t)
(define N-jherb 1)
(define N-aherb 1)
(define N-acarn 1)



(for-each
 (lambda (p)
	(if use-jherb
		 (for-each
		  (lambda (i)
			 (let ((a (create <jherb> "juvenile-herbivore" 'name (serial-number "herbivore") 'sex (if (odd? (random-integer 5)) 'male 'female)
							 'plot-scale PLOT-SCALE
									))
					 )
				(slot-set! a 'age-at-instantiation (slot-ref a 'age))
				(slot-set! a 'domain p)
				(slot-set! a 'domain p)
				(slot-set! a 'location (map + (location p) (list (- (random-integer 200) (random-integer 200)) (- (random-integer 100) (random-integer 100)))))
				(set! Q (q-insert Q a Qcmp))
				(set! HJ (cons a HJ))
				))
		  (seq N-jherb)
		  ))
	
	(if use-aherb
		 (for-each
		  (lambda (i)
			 (let ((a (create <aherb> "adult-herbivore" 'name (serial-number "herbivore") 'sex (if (odd? (random-integer 5)) 'male 'female)
							 'plot-scale PLOT-SCALE
									))
					 )
				(slot-set! a 'age-at-instantiation (slot-ref a 'age))
				(slot-set! a 'domain p)
				(slot-set! a 'location (map + (location p) (list (- (random-integer 200) (random-integer 200)) (- (random-integer 200) (random-integer 200)))))
				(set! Q (q-insert Q a Qcmp))
				(set! HA (cons a HA))
				))
		  ;;(seq 24/12)
		  (seq N-aherb)
		  )
		 )
	
	(if use-acarn
		 (for-each
		  (lambda (i)
			 (let ((a (create <acarn> "carnivore" 'name (serial-number "carnivore") 'sex (if (odd? (random-integer 3)) 'male 'female)
							 'plot-scale PLOT-SCALE
									))
					 )
				(slot-set! a 'age-at-instantiation (slot-ref a 'age))
				(slot-set! a 'domain p)
				(slot-set! a 'location (map + (location p) (list (- (random-integer 200) (random-integer 200)) (- (random-integer 300) (random-integer 300)))))
				(set! Q (q-insert Q a Qcmp))
				(set! CA (cons a CA))
				))
		  (seq  N-acarn))
		 )
	)
 patchlist)



;; ----- carnivores -----
;; Adults eat juvenile herbivores, juveniles are assumed to eat something
;; that is abundant and not modelled.


;; And the audience -----------------------------------------------------

;(define schedtimes (append 
;						 (cons 0 (seq 6))
;						 (map (lambda (x) (* 10.0 (1+ x))) (seq 360)))
;  ) ;; first six days, then on every tenth day from the beginning for 370 days


;; If the patches aren't in the Q, the loggers cannot find them
;;(for-each (lambda (p) (set! Q (q-insert Q p Qcmp))) patchlist)


(define sdlog
  (if #t
		(let ((sdlog (create <log-data> "service-logger" 'filename "service-data" 'format 'text
									;;'file (current-output-port)
									;; Recall: internally time---end for example---is in seconds.
									;;		 'timestep-schedule (map (lambda (x) (* x day)) (seq (+ 1 (/ end day)))) 
									'dt (* 5 days)
									'variables (list 'name 'seeds 'fruit)
									'introspection-targets patchlist)))
		  ;; We specify these by list since the number of services stays the same
		  (set! Q (q-insert Q sdlog Qcmp)) ;; these will go in earlier than the others 
		  sdlog
		  )
		))

(define adlog
  (if (and #t (or use-jherb use-aherb use-acarn))
		(let ((adlog (create <log-data> "animal-logger" 'filename "animal-data" 'format 'text
									'dt (* 4 hours)
									'variables (list 'subjective-time 'name 'agent-state 'age 'mass 'location 'direction 'sated-time 'period-of-hunger)
									'introspection-targets (list <animal>))))
		  ;; there will be more trees, so we want a mechanism that adapts
		  (set! Q (q-insert Q adlog Qcmp)) ;; these will go in earlier than the others 
		  adlog
		  )
		))

(define tdlog
  (if (and #t use-plants)
		(let ((tdlog (create <log-data> "tree-logger" 'filename "tree-data"
									;;'file (current-output-port)
									;; Recall: internally time---end for example---is in seconds.
									;;			 'timestep-schedule (map (lambda (x) (* x day)) (seq (+ 1 (/ end week))))
									'dt (* 5 days)
									'variables (list 'subjective-time 'name 'agent-state 'age 'mass 'leaf-area 'forage-damage)
									'introspection-targets (list <plant> <plant-array>))))
		  ;; there will be more trees, so we want a mechanism that adapts
		  (set! Q (q-insert Q tdlog Qcmp)) ;; these will go in earlier than the others 
		  tdlog
		  )
		))

	 
(define pslog
  (if (and #t (or use-plants use-jherb use-aherb use-acarn))
		;;                                       model  page  margin                           
		(let ((mapping (map:domain-to-postscript Domain isoA4 10))) ;; NOTE page dimensions are *always* specified in mm.
		  (let ((pslog
					(create <log-map> "map-maker" 'filename "domain-map.ps" 'format 'ps
							  ;; Recall: internally time---end for example---is in seconds.
							  ;;			 'timestep-schedule (map (lambda (x) (* x 4 weeks)) (seq (+ 1 (/ end (* 4 weeks)))))
							  'model->local mapping
							  'local->model (mapping 'inverse)
							  'dt (* 5 days)
							  'filename-timescale days
							  'introspection-targets
							  ;(list <plant-array> <plant> <animal> <patch>)
							  (list <patch> <plant-array> <plant> <aherb> <jherb> <acarn>)
							  ;(append (list <patch> <example-plant> <aherb> <jherb>) patchlist)
							  )))
			 (set! Q (q-insert Q pslog Qcmp)) ;; these will go in earlier than the others 
			 pslog
			 ))
		)          ;;; *is-class? creates a predicate function, there are also
                 ;;; *is-taxon? *has-slot?
  )


;========================================================================

;-- Example code to run things....



(define (ps-check filename)
  (let ((ps (make-postscript filename '(Helvetica))))
	 (ps 'moveto 300 300)
	 (ps 'display (string-append "File: " filename))
	 (map (lambda (x) (dnl* (name x)) (ps-dump x ps)) (filter (*has-slot? 'location) Q))
	 (close-output-port ps)))




	 
;-  The End


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
