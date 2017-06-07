(include "framework")
; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	plant-methods.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.03
;		Location: zero:/home/randall/Thesis/Example-Model/model/plant-methods.scm
;
;	History:
;
;-  Code

(define ignore-water #t)

(define (water-stress-level avail need)
  (if (zero? need)
		0
		(if (<= need avail)
			 0
			 (- 1.0 (sigmoid* (/ avail need))))))


;; **** This function is appropriate for patchy coverage (for flexible interpretations of "appropriate").
;; agefunc is either a number or a function of no arguments which returns a number
;; dispersion is the mean dispersion for a pprnd
(define (make-copse insertion-func clss tax n-trees patch agefunc location dispersion #!rest more)
  (if (number? agefunc) (set! agefunc (lambda () agefunc)))
  (for-each
	(lambda (q)
	  (let*  ((t (apply make-plant
							  (append (list clss tax patch (agefunc)
												 (list
												  'name (serial-number tax)
												  'age (agefunc)
												  'location (map + location
																		 (pprnd (make-list (length location) dispersion) ;;  @ position
																				  (make-list (length location)(/(+(radius patch)(Radius patch))2.0)))) ;; clipped @
												  'domain patch
												  'habitat patch
												 )
										 more))))
				 )
		 (slot-set! t 'mass ((slot-ref t 'mass-at-age) (agefunc)))
		 
		 ;; (slot-set! c ') ;;Does the habitat (patch) need to know of the trees?
		 (insertion-func t)
		 ))
	(seq n-trees)
	)
  )




(model-method <plant> (initialisation-checks self)
	(initialisation-checks-parent)

	(if (uninitialised? (my 'age))
		 (error "age not initialised for a <plant>" (name self)))

	;;(dnl* (my 'name) (my 'age) (my 'mass))
	(set-my! 'mass ((slot-ref self 'mass-at-age) (slot-ref self 'age)))
	
	(if (uninitialised#? self 'forage-damage)
		 (set-my! 'forage-damage 0))

	(if (uninitialised#? self 'peak-mass)
		 (set-my! 'peak-mass (my 'mass)))

	(if (uninitialised#? self 'leaf-area)
		 (let ((dmass (my 'growth-rate)))
			(set-my! 'leaf-area (plant-leaf-area self))
			(let ((dm (if (procedure? dmass) (dmass (my 'mass) 0) dmass)))
			  (set-my! 'regrowth-rate (* 1.8 dm)))
			)
		 )
	)


(model-method <plant> (fruit-count self)
				  (truncate (* (slot-ref self 'fruiting-rate
									(slot-ref self 'mass)
									(if (slot-ref self 'water-stress-effect)
										 (- 1 (sqr (slot-ref self 'water-stress)))
										 1.0)
									))
								))

;;; (agent-initialisation-method <plant> (args) (no-default-values) ;-
;;;  (set-state-variables self (list 'max-age 37.0 ;-
;;; 											'max-mass 300.0 ;-
;;; 											'age 12 ;-
;;; 											'lai 1.7 ;-
;;; 											'water-stress 0 ;-
;;; 											'water-stress-effect #t  ;-
;;; 											'water-use  4/5 ;; l/m^2 default is similar to oneoak ;-
;;; 											'reproduction-period  ;-
;;; 											(if #f ;-
;;; 												 (random-real) ;; a probability, or ;-
;;; 												 (+ 30 (random-integer 30)); an interval ;-
;;; 												 ) ;-
											
;;; 											'reproduction-offset (random-integer 20) ;-
;;; 											'reproduction-mechanism #f ;; <fruit> or a ;-
;;; 											;; procedure which performs an update on ;-
;;; 											;; some undefined thing.  ;-
											
;;; 											;; (mass (truncate rr)) > 0 ;-
;;; 											'fruiting-rate  0.01   ;; relative to mass ;-
;;; 											;; influenced by ... ;-

;;; 											'seeds-per-fruit 50 ;-
;;; 											;; gives about 15 fruiting trees per 100 ;-
;;; 											)) ;-
;;;  (initialise-parent) ;-
;;;  (set-state-variables self args) ;; set specifics passed in here... ;-

;;;  (slot-set! self 'mass (* (random-real) (random-real) ;-
;;; 								  (slot-ref self 'max-mass))) ;-
;;;  (slot-set! self 'age ;-
;;; 				(* (slot-ref self 'max-age) ;-
;;; 					(/ (slot-ref self 'mass) (slot-ref self 'max-mass)))) ;-
;;;  ) ;-
 

(model-method (<plant> <procedure>) (ps-dump self ps projection)
				  ;;(dnl* "In plant ps-dump")
				  (ps 'moveto (projection (local->model self (my 'location))))
				  (ps 'show-table
						(list (string-append "<plant>" (name self))
								(string-append "mass =" (number->string (my 'mass)))
								(string-append "radius =" (number->string (plant-radius self)))
								))
				  (ps 'moveto (projection (local->model self (my 'location))))
				  (adjusted-plot-polygon ps 0.4 0.5 #f (lambda (x) x)
												 (map projection (make-circle-perimeter (my 'location) (* 250 (plant-radius self)))))
				  )


;----- (self-assessment)
(UNFINISHED-BUSINESS "<plant> (representation-assessment self #!rest args): This is a placeholder ... it almost certainly needs to change")
(model-method <plant> (representation-assessment self #!rest args)
				  ;; the return value for an assessment is a (possibly complex) number, or a symbol
				  0
				  ;;; (if (null? args) ;; only passed self ;-
				  ;;; 		0 ;-
				  ;;; 		(let ((n (car args)) ;; mixing assumption  ;-
				  ;;; 				) ;-
				  ;;; 		  (+ (/ n (my 'population-switch))) ;-
				  ;;; 		  )) ;-
				  )

(model-method <plant> (lose-foliage self leafmass)
				  (let* ((la (leaf-area self))
							(leaf-area-foraged (* leafmass (my 'leaf-mass)))
							)
					 
					 (if (<= leaf-area-foraged la)
						  (begin
							 (set-my! 'forage-damage (+ (my 'forage-damage) leaf-area-foraged))
							 leaf-area-foraged)
						  (begin
							 (set-my! 'forage-damage la)
							 la))))
												 
(model-method <plant> (plant-radius self)
				  (plant-mass->radius (slot-ref self 'mass)))

(model-method <plant> (leaf-area self)
				  (- (my 'leaf-area) (my 'forage-damage)))

(model-method <plant> (leaf-mass self)
				  (* (leaf-area self) (my 'leaf-mass)))


(model-body <plant>
	;;(dnl* kdebug '(trace-bodies plant-running)  (cnc self) (name self) "@" t "/" dt)

	;; Calculate water requirements
	;;-- This is somewhat inaccurate in that each individual draws
	;;-- in  turn, depleting the pool without concurrency.
	;;-- We might argue that each tree "protects" its root zone and
	;;-- so has ensured a degree of independent operation, but this
	;;-- needs to be implemented within the environmental agent,
	;;-- which ought to site a micro-reservoir exclusively for each
	;;-- plant.  This micro-reservoir then gets replenished according
	;;-- to the groundwater available.

	(UNFINISHED-BUSINESS "Need to update the water ecoservice")

	(kdebug 'model-body (name self) "plant 1")
	(start-timer 'plant-model-body)
	(if (<= (my 'counter) 0)
		 (begin
			(void)
			;;(dnl* "INITIALISING PLANT")
			
			;;(dnl* '---> (my 'name) (my 'age) (my 'mass))
			;;; (let* ((domain (append (uniq 
			;;; 					 (map inexact->exact 
			;;; 							(map truncate 
			;;; 								  (map (lambda (x) (* x weeks)) 
			;;; 										 (sequence (inexact->exact (truncate (/ (my 'age-at-max-mass) weeks))))) 
			;;; 								  ))) 
			;;; 							  (list (my 'age-at-max-mass) +inf.0))) 
			;;; 		 (value (map (my 'mass-at-age) domain)))
			;;; 	'useless  
			;;;   )

			;;; (let* ((domain (append (uniq 
			;;; 					 (map inexact->exact 
			;;; 							(map truncate 
			;;; 								  (map (lambda (x) (* x weeks)) 
			;;; 										 (sequence (inexact->exact (truncate (/ (my 'age-at-max-mass) weeks))))) 
			;;; 								  ))) 
			;;; 							  (list (my 'age-at-max-mass) +inf.0))) 
			;;; 		 (value (map (my 'mass-at-age) domain))) 
			;;;   (set-my! 'age-at-mass (pwl (map cons domain value ))) 
			;;;   ) 
			))

	(kdebug 'model-body "plant 0")
	(stop-timer 'plant-model-body)
	(set! dt (call-parents))
	(start-timer 'plant-model-body)
		 
	(let ((return
			 (cond
			  ;; DEAD AGENT
			  ((eq? (agent-state self) 'dead)
				(set-my! 'mass (* (my 'mass) (exp (* (my 'decay-rate) dt))))
				(if (< (my 'mass) (* 0.2 (my 'peak-mass)))
					 (list 'remove dt)
					 dt))
			  ;; LIVE AGENT
			  (else 
				(let* ((available-water (if ignore-water +inf.0 (value (slot-ref self 'water-service) 'value)))
						 (pi (acos -1.0))
						 (waterstress (my 'water-stress))
						 (lai (my 'lai))
						 (r (plant-radius self))
						 (waterneeds (* (my 'water-use) lai pi r r)) ;; lai buy
						 (stressed (my 'water-stress-effect))
						 (age (my 'age))
						 (max-age (my 'max-age))
						 (mass (my 'mass))
						 (mass-at-age (my 'mass-at-age))
						 (deltaM (- (mass-at-age (+ age dt)) (mass-at-age age)))
						 )

				  (kdebug 'model-body "plant 2")

;(dnl* "Processing <plant> model-body")
;(dnl* (name self) "Mass delta" deltaM "at age" age"/" max-age "dt" dt)

				  (set-my! 'age (+ age dt))

				  ;; set water stress level and update hydrology

				  (set! waterstress (water-stress-level available-water waterneeds))
;(dnl* "water stress" waterstress)

				  (kdebug 'model-body "plant 3")

				  (do-fruiting self t dt)

				  ;; THIS (cond ...) RETURNS THE dt VALUE!!!
				  (cond	;; determine if the plant dies this step
					((not (number? dt))
					 dt)
					((< max-age age)
					 (die self)
					 'remove) ;; it's a stick, plant dies here.

					;; determine if the plant fruits or grows this step (not both)
					(#t ;; grow (or not)
;(dnl* "regrow <plant> if necessary" (my 'forage-damage))
					 (if (> (my 'forage-damage) 0)
						  (let ((regrowth (min (my 'forage-damage) (* (my 'regrowth-rate) dt))))
							 (set-my! 'forage-damage (max 0 (- (my 'forage-damage) regrowth)))
							 )
						  (begin ;; only grow when the forage-damage has been repaired
							 (if (= mass (+ mass deltaM))
								  (borked))

							 ;;((dnl* "adjust <plant> mass" mass deltaM)
							 (set-my! 'mass (+ mass deltaM)))
						  )

					 ;;		 (if (or (not (my 'water-stress-effect))
					 ;;					(< (my 'water-stress) 1))
					 ;;			  ;;			  (slot-set! self 'mass (+ mass (* dt (my 'growth-rate)))) ;; linear growth
					 ;;			  (slot-set! self 'mass dMatA) ;; keep decreased growth effects
					 ;;			  )
					 
					 (if (< (my 'peak-mass) (my 'mass))
						  (set-my! 'peak-mass (my 'mass)))

					 (kdebug 'model-body "plant 4b")
					 dt)
					)
				  )
				))
			 
			 ))
	  (stop-timer 'plant-model-body)
	  return
	  )
	)


(define plant-arg-order
  '(
     lai water-use  
       water-stress-effect reproduction-mass
       reproduction-mechanism
       fruiting-rate seeds-per-fruit
       reproduction-period
       reproduction-offset
       max-age max-mass))

(define (make-plant class taxon env age #!rest more)
  ;; otherargs is either a std init list, or a list of
  ;; numbers in the order:
  ;;   lai water-use  
  ;;     water-stress-effect reproduction-mass
  ;;     reproduction-mechanism
  ;;     fruiting-rate seeds-per-fruit
  ;;     reproduction-period
  ;;     reproduction-offset
  ;;     max-age max-mass

  ;; If no reproduction-mechanism is specified, the plant does not
  ;; reproduce -- never fruits

  (if (and (pair? more) (number? (car more)))
		(set! more (arg-pairings plant-arg-order more)))

  (kdebug '(init plants) "env:" env  "args:"  more)
  (set! more (append (list 'habitat env
									'location (random-point env)
									'age age) more))
  (kdebug '(init plants) "->args:"  more)
  (let ((sp (apply create (cons class (cons taxon more )))))
	 sp
	 ))



(define (make-plant-xy class taxon env loc age #!rest otherargs)
  ;; otherargs is either a std init list, or a list of
  ;; numbers in the order:
  ;;   lai water-use  
  ;;     water-stress-effect reproduction-mass
  ;;     reproduction-mechanism
  ;;     fruiting-rate seeds-per-fruit
  ;;     reproduction-period
  ;;     reproduction-offset
  ;;     max-age max-mass

  ;; If no reproduction-mechanism is specified, the plant does not
  ;; reproduce -- never fruits

  (if (not (null? otherargs))
		(set! otherargs (arg-pairings plant-arg-order otherargs)))

  (let ((sp (apply create (append (list class taxon 'habitat env
						  'location loc
						  'age (+ 8 (random-integer 24))
						  'age age) otherargs)) ))
	 (UNFINISHED-BUSINESS "The age here is totally bogus")

	 (if (contains? env loc)
		  sp
		  (error "location for plant is not in indicated environment" loc env))))


;--- Only logging methods below
		 


(define (plot-tree tree logger file)
  (let* ((inner-facets 6)
			(outer-facets 8)
			(scale 1.5)
			(iply (make-circle-perimeter
					(slot-ref tree 'location) (* 0.5 scale (plant-radius tree)) inner-facets))
			(oply (make-circle-perimeter
					(slot-ref tree 'location) (* scale (plant-radius tree)) outer-facets))
			(prj (composite-prj_src->dst tree logger))
			)

	 (adjusted-plot-polygon file 0.1 0.0 #f prj iply)
	 (adjusted-plot-polygon file 0.4 0.0 #f prj oply)
	 )
  )
  

(model-method (<plant> <log-introspection> <symbol> <list>)
				  (log-data self logger fmt targets)
				  (let ((file (slot-ref logger 'file))
						  (leading-entry #f)
						  (needs-newline #f)
						  )
					 ;;(dnl* "****"  (cnc logger) (cnc self) (name self) fmt  (symbol? fmt) (eq? fmt 'ps))
					 
					 (if (emit-and-record-if-absent logger self (my 'subjective-time))
						  (begin
							 ;; this handles "whole of agent" bits, like perimeters
							 (cond
							  ((member fmt '(ps))
								(let* ((ply (make-circle-perimeter
												 (my 'location) (* (my 'plot-magnification)  (plant-radius self)) (my 'circle-facets)))
										 (prj (composite-prj_src->dst self logger))
										 (pply (map prj ply))
										 )

								  (plot-tree self logger file)
								  ;;(adjusted-plot-polygon file 0.4 0.0 #f prj ply)
								  ))
							  )
							 
							 
							 ;; This handles fields
							 (for-each
							  (lambda (field)
								 ;;(dnl* "   fielding" field)
								 (kdebug '(log-* log-plant) "[" (my 'name) ":"
											(cnc self) "]" "checking" field)
								 ;;(dnl* "  " (cnc self) (if (has-slot? self field) "has" "doesn't have") field)
								 (if (not (symbol? fmt)) (buggrit))
								 (if (has-slot? self field)
									  (letrec ((r (slot-ref self field)))
										 #t
										 ;;(dnl* "   processing" field "for" fmt "format")
										 (case fmt
											((ps)
											 (set! needs-newline #t)
											 (file 'show (string-append
															  (if (string? r)
																	r
																	(object->string r)) " "))
											 #t
											 )
											((text table dump)
											 (let ((show-field-name
													  (slot-ref logger 'show-field-name))
													 (missing-val
													  (slot-ref logger 'missing-val))
													 )
												(if show-field-name
													 (begin
														(set! needs-newline #t)
														(if leading-entry
															 (display " " file)
															 (set! leading-entry #t))
														(set! needs-newline #t)
														(display field file)))
												
												(let ((val (if (eqv? field 'name) 
																	(if (slot-ref self 'habitat)
																		 (string-append
																		  (slot-ref (slot-ref self 'habitat) 'name) ":" (name self))
																		 (name self))
																	(if (has-slot? self field)
																		 (slot-ref self field)
																		 (slot-ref logger 'missing-val)))))
												  (if leading-entry 
														(display " " file)
														(set! leading-entry #t))
												  (set! needs-newline #t)
												  (display val file))
												)
											 )

											(else
											 (kdebug '(log-* log-ecoservice)
														"<plant>:log-data [" (my 'name) ":" (cnc self) "]"
														"Ignoring " field " because I don't have it")
											 'ignore-unhandled-format)))						 (begin
																											(kdebug '(log-* log-ecoservice)
																													  "<plant>:log-data [" (my 'name) ":" (cnc self) "]"
																													  "no service" field)
																											#f)))
							  (uniq (if #t
											targets
											(filter (not-memq (slot-ref logger 'dont-log)) targets)))
							  )




							 )
						  ;;(dnl* "Did not process" (name self))
						  )

					 (if needs-newline (newline file))

					 (kdebug (list 'log-* 'log-plant (my 'name) (my 'taxon))
								"<plant>:log-data --- Leaving [" (my 'name) ":" (cnc self) "]"
								"in log-data")
					 )
				  )


(UNFINISHED-BUSINESS "Need to flesh this out a bit")

;;; (agent-initialisation-method <example-plant> (args) (no-default-values) ;-
;;; 									  (set-state-variables self '()) ;-
;;; 									  (initialise-parent) ;-
;;; 									  (set-state-variables self args) ;; set specifics passed in here... ;-
;;; 									  ) ;-
 									  									  									  
(model-method (<plant> <number> <number>) (do-fruiting self t dt)
				  ;(dnl* "<plant> fruiting")
				  (let ((offset (my 'reproduction-offset))
						  (reproduction-point (modulo (- t offset) (max 1 (my 'reproduction-period)))) ;; one day minimum
						  (reprob (random-real))
						  (fruiting? #f))

					 (if (and (negative? reproduction-point)
								 (< reprob (my 'fruiting-probability))
								 (not (negative? (+ dt reproduction-point))))
				;;; (if (not water-stress-effect)
				;;; 	 #t
				;;; 	 'true-for-the-moment-but-need-to-have-a-test-here
				;;; 	 )
						
						  (let* ((here (my 'habitat))
									(num-fruit (fruit-count self))
									(current-fruit (value here 'fruit))
									)
							 (set-value! here 'f (+ current-fruit num-fruit))))))


(model-method <example-plant> (initialisation-checks self)
	(initialisation-checks-parent)
	;(call-parents)
	
	(if (and (has-slot? self 'mort-mass) (or (not (number? (my 'mort-mass))) (negative? (my 'mort-mass))))
		 (set-my 'mort-mass 0))
	)

(model-method (<example-plant> <number> <number>) (do-fruiting self t dt)
				  (let ((draw (random-real))
						  (f (my 'fruiting-mass))
						  (m (my 'mass))
						  (p (my 'fruiting-probability))
						  )
					 (if (or (not (number? m)) (< m 0) (> m 150)) (error "mass is invalid"))
					 (if (or (not (number? f)) (< f 0) (>= f 150)) (error "fruiting mass is invalid"))
					 (if (or (not (number? p)) (negative? p) (> p 1)) (error "fruiting probability is invalid"))

					 ;(dnl* "<example-plant> fruiting draws" draw )
					 ;; This comes before death, since plants will often seed/fruit in one last burst
					 (if (and (>= m f)
								 (< draw p))
						  (begin
							 (add-fruit self (power m 2/3))
							 )
						  ;(dnl*  "<example-plant> NOT fruiting" t)
						  )
					 ))
				  


(model-body <example-plant>
	(start-timer 'example-plant-model-body)
				
;	(if (<= (my 'counter) 0)
;		 (initialisation-checks self))
	
;(dnl* "Processing <example-plant> model-body")
	;; This is here so the parent bodies do not get executed when it ought to die.
	(let ((m (my 'mass))
			(pkmass (my 'peak-mass))
			(mm (my 'mort-mass))
			(mp (my 'mort-prob)) (draw (random-real)))

	  (let ((return
				(cond
				 ;; DEAD AGENT
				 ((eq? (agent-state self) 'dead)
				  (set-my! 'mass (* (my 'mass) (exp (* (my 'decay-rate) dt))))
				  (if (< (my 'mass) (* 0.2 (my 'peak-mass)))
						(list 'remove dt)
						dt))
				 (else
				  (if (or (< m (* pkmass mm)) (< draw mp))
						(begin
;(dnl* "Die:" m pkmass mm (* mm pkmass)  draw "/" mp)
						  (die self)
						  dt)
						(begin
						  (stop-timer 'example-plant-model-body)
						  (set! dt (call-parents))
						  (start-timer 'example-plant-model-body)
						  
						  ;;; (dnl* "Fruiting?" (name self))
						  ;;; (if (isa? self <example-plant>)
						  ;;; 		(begin
						  ;;; 		  (dnl* "... yup")
						  ;;; 		  (do-fruiting self t dt)
						  ;;; 		  ))

						  (let ((age (my 'age))) ;; This masks the generic method (age...) within the body of the let
							 (if (> age (my 'max-age))
								  (die self) ;; too old.
								  dt)
							 ))
						))
				 ))
			  )
		 (stop-timer 'example-plant-model-body)
		 return
		 )))

(model-method <example-plant> (add-fruit self fruiting-area)
				  (let ((cell (my 'habitat))
						  (spot (my 'location))
						  (rate (my 'fruiting-rate))
						  )
;(dnl* "FRUIT!" (* rate fruiting-area))
					 (add! cell 'fruit (* rate fruiting-area))
					 ))
				
(model-method <example-plant> (die self)
				  (dnl* kdebug 'death "<example-plant> dies: mass = " mass ", mort mass = " (* (my 'peak-mass) (my 'mort-mass)))
				  (set-agent-state! self 'dead)
				  ;;(kernel 'shutdown self) 
				  )


;; Plants are done.


;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: ";;; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
