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


(define (plant-location patch location dispersion)
  (let* ((displacement (map +/- (pprnd (make-list (length location) dispersion))))
			(dig (lambda ()
					(map + location
						  displacement)))) ;;  @ position
	 (make-list (length location) (/ (+ (radius patch) (Radius patch)) 2.0))
	 (let loop ((n 0)
					(p (dig)))
		(if (> n 2000) (error "Inescapably bad location for planting" (name patch) location dispersion))
		(if (or #t (Valid-location p))
			 p
			 (loop (+ n 1)
				  (dig))))))
  

;; **** This function is appropriate for patchy coverage (for flexible interpretations of "appropriate").
;; agefunc is either a number or a function of no arguments which returns a number
;; dispersion is the mean dispersion for a pprnd
(define (make-copse insertion-func clss tax n-trees patch agefunc location dispersion #!rest more)
  (if (number? agefunc) (set! agefunc (lambda () agefunc)))
  (for-each
	(lambda (q)
	  (let*  ((locus (plant-location patch location dispersion))
				 (t (apply make-plant
							  (append (list clss tax patch (agefunc)
												 (append (list
															 'name (serial-number tax)
															 'age (agefunc)
															 'location locus
															 'domain patch
															 )
															more)))))
				 )
		 ;;(dnl* "Tree:" (name t) "at" locus "in" (name patch))
		 (slot-set! t 'mass ((slot-ref t 'mass-at-age) (agefunc)))
		 
		 ;; (slot-set! c ') ;;Does the domain (patch) need to know of the trees?
		 (insertion-func t)
		 ))
	(seq n-trees)
	)
  )

(define (default-growth-rate self)
  (let ((dt (modal-dt self)))
	 (lambda x
		(/ (- ((my 'mass-at-age) (my 'age))
 				((my 'mass-at-age) (+ (my 'age) dt)))
 			dt))))

(define (plant-growth mass-at-age age dt)
  (/ (- (mass-at-age age)
		  (mass-at-age(+ age dt)))
	  dt))


(model-method <plant> (initialise-instance self)
	(kdebug '(<plant> initialisation) "IN <plant> initialise-instance")
	(parent-initialise-instance)

	(set-my! 'mass ((my 'mass-at-age) (my 'age)))

	(if (uninitialised? self 'mass-radius)
		 (set-my! 'mass-radius (std-mass-radius-function)))
	
	(let* ((age (my 'age))
			 (age-at-instantiation age)
			 (maa (my 'mass-at-age))
			 (mass (my 'mass))
			 (max-age (my 'max-age))
			 (L (my 'longevity))
			 )

	  (kdebug '(<plant> initialisation) 'age age 'mass mass)

	  (set-my! 'mass mass)

	  (if (uninitialised? (my 'max-age)) (set-my! 'max-age +inf.0))
	  (if (<= max-age age) (set-my! 'max-age (/ (+ age L) 2)))

	  (if (uninitialised? (my 'mass-at-age))
			(error "the mass-at-age function must be defined in a parameter file, or in the create call for " (cnc self) (taxon self)))
	
	  ;;(dnl* (my 'name) (my 'age) (my 'mass))
     ;;(set-my! 'mass ((slot-ref self 'mass-at-age) (slot-ref self 'age)))

;	  (if (uninitialised? (my 'radius-))
;			(error "the mass-at-age function must be defined in a parameter file, or in the create call for " (cnc self) (taxon self)))


	(cond
	 ((not (number? mass))
	  (set-my! 'mass (maa age))
	  'set )
	 ((> (abs (- mass (maa age))) (* 0.2 mass))
	  ;;(dnl* "**** Resetting mass for" (taxon self) (name self) "from" mass "to" (maa age))
	  (set-my! 'mass (maa age))
	  'reset)
	 (else 'ok))

	(kdebug '(<plant> initialisation) '--> 'age age 'mass mass)

	(if (or (uninitialised#? self 'peak-mass) (< (my 'peak-mass) (my 'mass)))
		 (set-my! 'peak-mass (my 'mass)))

	(if (uninitialised? self 'regrowth-rate-multiplier)
		 (set-my! 'regrowth-rate-multiplier 1.5))


	(if (uninitialised#? self 'leaf-area)
		 (let ((dmass (my 'growth-rate)))
			(if (uninitialised? dmass)
				 (set! dmass
						 (lambda x
							(let ((maa (my 'mass-at-age))
									(a (my 'age))
									(dt (my 'dt))) ;; Need a default dt for this!
							(/ (- (maa a)
									(maa (+ dt a)))
								dt)))
						 ))

		  (set-my! 'leaf-area (plant-leaf-area self))
	))))

(model-method <plant> (agent-prep self start end)
				  (parent-agent-prep)

				  (initialise-instance self)

				  (set-uninitialised-slots self '(peak-mass water-stress water-use reproduction-mechanism forage-damage ) 0)
				  (set-uninitialised-slots self '(water-stress-effect) #f)
				  (set-uninitialised-slots self '(glyph) 18)
				  (set-uninitialised-slots self '(plot-magnification) pi)
				  (fail-on-uninitialised-slots self '(decay-rate leaf-mass omega-ind max-mass lai reproduction-mass
																				 fruiting-probability fruiting-mass fruiting-rate
																				 growth-rate seeds-per-fruit mass-radius))
				  (if (uninitialised? (slot-ref self 'dead-color)) (slot-set! self 'dead-color ps-brown)) ;; half grey
				  (if (uninitialised? (slot-ref self 'radius-color)) (slot-set! self 'radius-color ps-dark-green)) ;; 
				  (if (uninitialised? (slot-ref self 'foliage-color)) (slot-set! self 'leaf-area-color ps-pale-green)) ;;
				  (if (uninitialised? (slot-ref self 'stress-color)) (slot-set! self 'leaf-area-color ps-mid-orange)) ;;
				  #t
				  )



(model-method <plant> (mass self #!optional locus)
				  (slot-ref self 'mass))

(model-method <plant> (set-mass! self nmass  #!optional locus)
				  (slot-set! self 'mass nmass))



(model-method <plant> (fruit-count self #!optional locus)
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
;;;  (parent-initialise-instance) ;-
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
								(string-append "radius =" (number->string (radius self)))
								))
				  (ps 'moveto (projection (local->model self (my 'location))))
				  (adjusted-plot-polygon ps 0.4 0.5 #f (lambda (x) x)
												 (map projection (make-circle-perimeter (my 'location) (* 250 (radius self)))))
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

;;; (model-method <plant> (lose-foliage self leafmass)
;;; 				  (set-my! 'leaf-area (plant-leaf-area self))
;;; 				  (let* ((la (leaf-area self))
;;; 							(leaf-area-foraged (/ leafmass (my 'leaf-mass)))
;;; 							)
					 
;;; 					 (if (<= leaf-area-foraged la)
;;; 						  (begin
;;; 							 (set-my! 'forage-damage (+ (my 'forage-damage) leaf-area-foraged))
;;; 							 leaf-area-foraged)
;;; 						  (begin
;;; 							 (set-my! 'forage-damage la)
;;; 							 la))))

;;; (model-method <plant> (<plant> <agent> <symbol>) (request-accessor self intruder sym #!rest args)
;;; 				  (case sym
;;; 					 ((adjust#)
;;; 					  (if (not (car args) 'leaf-area)
;;; 							(abort)
;;; 							(lambda (val)
;;; 							  (slot-set! self 'leaf-area (plant-leaf-area self))
;;; 							  (let* ((la (leaf-area self))
;;; 										(leaf-area-foraged (/ leafmass (slot-ref self 'leaf-mass)))
;;; 										)
								 
;;; 								 (if (<= leaf-area-foraged la)
;;; 									  (begin
;;; 										 (slot-set! self 'forage-damage (+ (slot-ref self 'forage-damage) leaf-area-foraged))
;;; 										 leaf-area-foraged)
;;; 									  (begin
;;; 										 (slot-set! self 'forage-damage la)
;;; 										 la))))
;;; 					 (else (parent-request-accessor))
;;; 				  ))

												 
(model-method <plant> (radius self)
				  (set-my! 'leaf-area (plant-leaf-area self))
				  (plant-mass->radius (slot-ref self 'mass locus)))

(model-method <plant> (leaf-area self #!optional locus)
				  (set-my! 'leaf-area (plant-leaf-area self locus))
				  (- (my 'leaf-area) (my 'forage-damage))) ;; forage-damage is to leaf area, *not* mass

(model-method <procedure> (leaf-area self)
				  ((self)))

(model-method <plant> (leaf-mass self #!optional locus)
				  (set-my! 'leaf-area (plant-leaf-area self locus))
				  (* (leaf-area self) (my 'leaf-mass)))

(model-method <plant> (pristine-leaf-mass self #!optional locus) ;; does not take forage damage into account
				  (set-my! 'leaf-area (plant-leaf-area self locus))
				  (* (my 'leaf-area) (my 'leaf-mass))) 


(model-body% <plant>
				  ;;(dnl* (cnc self) "model-body")
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

	(kdebug 'model-body "plant 0")
	(stop-timer 'plant-model-body)
	
	(if (not (my 'mass))
		 (set-my! 'mass ((my 'mass-at-age) (my 'age))))

	(let ((parent-return (call-all-parents)))
	  (start-timer 'plant-model-body)

	  (if (> (my 'mass) (my 'peak-mass)) (set-my! 'peak-mass (my 'mass)))

	  (let ((return
				(cond
				 ;; DEAD AGENT
				 ((dead? self)
				  (set-my! 'mass (* (my 'mass) (exp (* (my 'decay-rate) dt))))
				  (if (< (my 'mass) (* 0.2 (my 'peak-mass)))
						(begin
						  (set-my! 'agent-state 'terminated)
						  (set-my! 'queue-state 'terminated)
						  (list 'remove))
						dt))
				 ;; LIVE AGENT
				 (else 
				  (set-my! 'leaf-area (plant-leaf-area self))
				  (let* ((available-water (if ignore-water +inf.0 (value (slot-ref self 'water-service) 'value)))
							(pi (acos -1.0))
							(waterstress (my 'water-stress))
							(lai (my 'lai))
							(r (radius self))
							(waterneeds (* (my 'water-use) lai pi r r)) ;; lai buy
							(stressed (my 'water-stress-effect))
							(age (my 'age))
							(max-age (my 'max-age))
							(longevity (my 'longevity))
							(mass (my 'mass))
							(mass-at-age (my 'mass-at-age))
							(deltaM (- (mass-at-age (+ age dt)) (mass-at-age age)))
							(pmort (let ((pm (my 'omega-ind)))
										(if (procedure? pm) (pm age) pm)))
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
						;;(dnl* 'NOT-A-NUMBER!)
						dt)
					  ((or (not (number? pmort)) (and (number? pmort) (< pmort (random-real)))) ;; 

						(if (> (my 'forage-damage) 0)
							 (let ((regrowth (min (my 'forage-damage) (* (my 'regrowth-rate-multiplier) dt))))
								(set-my! 'forage-damage (max 0 (- (my 'forage-damage) regrowth)))
								)
							 (begin ;; only grow when the forage-damage has been repaired
								(if (= mass (+ mass deltaM))
									 ;;(dnl* "no growth at " (scaled-time age) " -> " (scaled-time (+ age dt)))
									 ;;((dnl* "adjust <plant> mass" mass deltaM)
									 (set-my! 'mass (+ mass deltaM)))
								)
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
					  (else
						;;(dnl* (taxon self) "I feel happy!" age '> max-age)
						(die self)
						dt) ;; it's a stick, plant dies here.
					  )
					 )
				  ))
				))
		 (stop-timer 'plant-model-body)
		 return
		 )
	  )
	dt
	)

(define plant-arg-order
  '(
     lai water-use  
       water-stress-effect reproduction-mass
       reproduction-mechanism
       fruiting-rate seeds-per-fruit
       reproduction-period
       reproduction-offset
       longevity max-mass))

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
  (set! more (append (list 'domain env
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

  ;; If no reproduction-mechanism is specified, the plant does not
  ;; reproduce -- never fruits
  (error "Not handling location correctly")

  (if (not (null? otherargs))
		(set! otherargs (arg-pairings plant-arg-order otherargs)))

  (let ((sp (apply create (append (list class taxon 'domain env
						  'location loc
						  'age (+ 8 (random-integer 24))
						  'age age) otherargs)) ))
	 (UNFINISHED-BUSINESS "The age here is totally bogus")

	 (if (contains? env loc)
		  sp
		  (error "location for plant is not in indicated environment" loc env))))


(define (exp-mass-radius-function decay)
	(lambda (X x) (* X (exp (* -decay (- X x))))))

(define (std-mass-radius-function #!rest forget)
  (lambda (X x)
	 (* 1/2 (+ (plant-mass->radius X) (plant-mass->radius x)))))


;--- Only logging methods below
;              _
;            _( )_
;           (_(#)_)
;            /(_)
;            | __
;          _ |/_/
;         \_\|
;        _-- |-- _
;       | --_|_-- |
;       |         |
;       |_       _|
;         --___--

(model-method <plant> (plot-plant self logger file)
  (let* ((inner-glyph (slot-ref self 'glyph))
			(outer-glyph (+ 3 (slot-ref self 'glyph)))
			(scale 1.5)
			(mass-radius (let ((mr (slot-ref self 'mass-radius)))
								(cond
								 ((procedure? mr) mr)
								 ((number? mr) (lambda (X x) (* mr X x)))
								 (else (lambda (X x) (* 1 x))))))
			
			(iply (if (number? inner-glyph)
						 (make-circle-perimeter
						  (slot-ref self 'location) (* scale (radius self)) inner-glyph)
						 (translate-glyph (slot-ref self 'location)
												(rescale-glyph (* scale (radius self))
																	inner-glyph))
						 ))
			(oply (if (number? outer-glyph)
						 (make-circle-perimeter
						  (slot-ref self 'location)
						  (* scale (mass-radius (pristine-leaf-mass self) (leaf-mass self)))
						  outer-glyph)
						 (translate-glyph (slot-ref self 'location)
												(rescale-glyph (* scale
																		(mass-radius
																		 (pristine-leaf-mass self)
																		 (leaf-mass self)))
																	outer-glyph))
						 ))
			(prj (composite-prj_src->dst self logger))
			)

	 (let ((dead (slot-ref self 'dead-color))
			 (radcol (slot-ref self 'radius-color))
			 (folcol (map (lambda (x y) (/ (+ x y) 2.0)) (slot-ref self 'foliage-color) (slot-ref self 'stress-color)))
			 )
		
		
		
		(if (dead? self)
			 (begin
				(file 'comment " " (name self) " " (location self))
				(file 'push-color (slot-ref self 'dead-color))
				(adjusted-plot-polygon file 0.1 dead #t prj oply)
				(adjusted-plot-polygon file 0.25 dead #f prj iply)
				(file 'pop-color)
				)
			 (begin
				(file 'comment " " (name self) " " (location self))
				(file 'push-color folcol)
				(adjusted-plot-polygon file 0.1 folcol #t prj oply)
				(file 'push-color radcol)
				(adjusted-plot-polygon file 0.25 radcol #f prj iply)
				(file 'pop-color)
				(file 'pop-color)
				)
			 )
		)
	 ))
  

(model-method (<plant> <log-introspection> <symbol> <list>)
				  (log-data self logger format targets)
				  (let ((file (slot-ref logger 'file))
						  (leading-entry #f)
						  (needs-newline #f)
						  )
					 ;;(dnl* "****"  (cnc logger) (cnc self) (name self) format  (symbol? format) (eq? format 'ps))
					 
					 (if (or (my 'always-log) (emit-and-record-if-absent logger self (my 'subjective-time)))
						  (begin
							 ;; this handles "whole of agent" bits, like perimeters
							 (cond
							  ((postscript? file)
								(let* (;;(ply (make-circle-perimeter
										 ;;		 (my 'location) (* (my 'plot-magnification)  (sqrt (leaf-area self))) (my 'glyph)))
										 ;;(prj (composite-prj_src->dst self logger))
										 ;;(pply (map prj ply))
										 )

								  (plot-plant self logger file)
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
								 (if (not (symbol? format)) (buggrit))
								 (if (has-slot? self field)
									  (letrec ((r (slot-ref self field)))
										 #t
										 ;;(dnl* "   processing" field "for" format "format")
										 (cond
										  ((postscript? file)
											(set! needs-newline #t)
											(file 'show (string-append
															 (if (string? r)
																  r
																  (object->string r)) " "))
											#t
											)
										  ((text? file)
											(set! needs-newline #t)
											(file 'show (string-append
															 (if (string? r)
																  r
																  (object->string r)) " "))
											#t
											)
										  ((output-port? file)
											(let ((show-field-name
													 (slot-ref logger 'show-field-name))
													(missing-val
													 (slot-ref logger 'missing-val))
													)
											  (if show-field-name
													(begin
													  (set! needs-newline #t)
													  (if leading-entry
															(file 'display " ")
															(set! leading-entry #t))
													  (set! needs-newline #t)
													  (file 'display field)))
											  
											  (let ((val (if (eqv? field 'name) 
																  (if (slot-ref self 'domain)
																		(string-append
																		 (slot-ref (slot-ref self 'domain) 'name) ":" (name self))
																		(name self))
																  (if (has-slot? self field)
																		(slot-ref self field)
																		(slot-ref logger 'missing-val)))))
												 (if leading-entry 
													  (file 'display " ")
													  (set! leading-entry #t))
												 (set! needs-newline #t)
												 (file 'display val))
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

					 (if needs-newline (file 'newline))

					 (kdebug (list 'log-* 'log-plant (my 'name) (my 'taxon))
								"<plant>:log-data --- Leaving [" (my 'name) ":" (cnc self) "]"
								"in log-data")
					 )
				  )


(UNFINISHED-BUSINESS "Need to flesh this out a bit")

;;; (agent-initialisation-method <example-plant> (args) (no-default-values) ;-
;;; 									  (set-state-variables self '()) ;-
;;; 									  (parent-initialise-instance) ;-
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
						  
						  (let* ((here (my 'domain))
									(num-fruit (fruit-count self))
									(current-fruit (value here 'fruit))
									)
							 (set-value! here 'f (+ current-fruit num-fruit))))))


(model-method <example-plant> (initialise-instance self)
				  (if (not (my 'mass)) (set-my! 'mass ((my 'mass-at-age) (my 'age))))

				  (parent-initialise-instance)
				  
				  (if (and (has-slot? self 'mort-mass) (or (not (number? (my 'mort-mass))) (negative? (my 'mort-mass))))
						(set-my! 'mort-mass ((my 'mass-at-age) (my 'longevity))))
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



(model-body% <example-plant>
				 (start-timer 'example-plant-model-body)
				 (call-all-parents)
				 
;	(if (<= (my 'counter) 0)
;		 (initialise-instance self))
				 
;(dnl* "Processing <example-plant> model-body")
				 ;; This is here so the parent bodies do not get executed when it ought to die.
				 (let ((m (my 'mass))
						 (pkmass (my 'peak-mass))
						 (mm (my 'mort-mass))
						 (mp (my 'mort-prob))
						 (draw (random-real)))
					(let ((return
							 (cond
							  ;; DEAD AGENT
							  ((dead? self)
								(set-my! 'mass (* (my 'mass) (exp (* (my 'decay-rate) dt))))
								(let ((r (if (< (my 'mass) (* 0.2 (my 'peak-mass)))
												 (list 'remove)
												 dt)))
								  ;;(dnl* "DEAD-->" *r)
								  r))

							  (else
								(if #f;(or (< m (* pkmass mm)) (< draw mp))
									 (begin
;(dnl* "Die:" m pkmass mm (* mm pkmass)  draw "/" mp)
										(die self)
										;;									  (dnl* "I'm not dead yet....")
										dt)
									 (begin
										(stop-timer 'example-plant-model-body)
										(let ((parent-returns (call-all-parents)))
										  (start-timer 'example-plant-model-body)
										  (let ((age (my 'age))) ;; This masks the generic method (age...) within the body of the let
											 (if (> age (my 'longevity))
												  (begin
													 ;;													(dnl* "I think I'll go for a walk...")
													 (die self)
													 ) ;; too old.
												  dt)
											 ))
										))
								))
							 ))
					  (stop-timer 'example-plant-model-body)
					  return
					  ))
				 )

(model-method <example-plant> (add-fruit self fruiting-area)
				  (let ((cell (my 'domain))
						  (spot (my 'location))
						  (rate (my 'fruiting-rate))
						  )
;(dnl* "FRUIT!" (* rate fruiting-area))
					 (add! cell 'fruit (* rate fruiting-area))
					 ))


;;; (model-method (<plant-array> <agent> <symbol>) (request-accessor self intruder sym locus #!rest args)
;;; 				  (if (member kind '(leaf-area forage-damage))
;;; 						(let* ((data (my 'data))
;;; 								 (nd (length data)))
;;; 						  (let ((ix (let loop ((i 0)
;;; 													  (k 0)
;;; 													  (kd +inf))
;;; 										  (if (>= i nd)
;;; 												k
;;; 												(let ((d (v-length (map - locus (cadddr (list-ref i data))))))
;;; 												  (if (< d kd)
;;; 														(loop (+ i 1) i d)
;;; 														(loop (+ i 1) k kd))))))) ;; finds the tree closest to locus
;;; 						  (case sym
;;; 							 ((leaf-area forage-damage )
;;; 							  (lambda ()
;;; 								 (data-ref self ix kind)))
;;; 							 ((adjust#)
;;; 							  (lambda (val)
;;; 								 (let ((nval (+ (data-ref self ix s) val)))
;;; 									(set! val (if (negative? nval)
;;; 													  (+ val nval)
;;; 													  val))
;;; 									(data-set! self ix kind (max 0 nval))
;;; 									val)))
;;; 							 (else (parent-request-accessor))
;;; 							 ))
;;; 						  )))

(model-method (<plant-proxy>) (initialise-instance self)
				  (set-slot! self 'lai (numeric-parameter-lookup <> (my 'taxon) 'lai))
				  (set-slot! self 'leaf-mass (numeric-parameter-lookup <> (my 'taxon) 'leaf-mass))
				  )

(model-method <plant-proxy> (location self  #!optional locus)
				  (data-ref self 'location))

(model-method <plant-proxy> (mass self #!optional locus)
				  (data-ref self 'mass))

(model-method <plant-proxy> (set-mass! self nmass #!optional locus)
				  (data-set! self 'mass nmass))

												 
(model-method <plant-proxy> (radius self #!optional locus)
				  (plant-mass->radius (slot-ref self 'mass)))

(model-method <plant-proxy> (leaf-area self #!optional locus)
				  (data-set! self 'leaf-area (general-leaf-area (mass self) (data-ref self 'lai)))
				  (- (data-ref self 'leaf-area) (data-ref self 'forage-damage))) ;; forage-damage is to leaf area, *not* mass

(model-method <plant-proxy> (leaf-mass self #!optional locus)
				  (* (leaf-area self) (data-ref self 'leaf-mass)))

(model-method <plant-proxy> (pristine-leaf-mass self #!optional locus) ;; does not take forage damage into account
				  (data-set! self 'leaf-area (general-leaf-area (mass self) (data-ref self 'lai)))
				  (* (data-ref self 'leaf-area) (data-ref self 'leaf-mass)))

(model-body <plant-proxy>
				(abort "This should never happen"))


(model-method (<plant-array> <log-introspection> <symbol> <list>)
				  (log-data self logger format targets)
				  (let ((file (slot-ref logger 'file))
						  (leading-entry #f)
						  (needs-newline #f)
						  )
					 ;;(dnl* "****"  (cnc logger) (cnc self) (name self) format  (symbol? format) (eq? format 'ps))
					 
					 (if (or (my 'always-log) (emit-and-record-if-absent logger self (my 'subjective-time)))
						  (begin
							 ;; this handles "whole of agent" bits, like perimeters
							 (cond
							  ((postscript? file)
								(let* (;;(ply (make-circle-perimeter
										 ;;		 (my 'location) (* (my 'plot-magnification)  (sqrt (leaf-area self))) (my 'glyph)))
										 ;;(prj (composite-prj_src->dst self logger))
										 ;;(pply (map prj ply))
										 )
								  (let ((P (create <plant> ptax 'agent-state 'alive 'mass 0 'peak-mass 0 'age 0 'location (list 0 0 0) 'leaf-area 0 'forage-damage 0 'water-stress 0)))
									 (for-each
									  (lambda (p)
										 (slot-set! P 'agent-state (car p))
										 (slot-set! P 'mass (cadr p))
										 (slot-set! P 'peak-mass (caddr p))
										 (slot-set! P 'age (list-ref p 3))
										 (slot-set! P 'location (list-ref p 4))
										 (slot-set! P 'leaf-area (list-ref p 5))
										 (slot-set! P 'forage-damage (list-ref p 6))
										 (slot-set! P 'water-stress (list-ref p 7))

										 (plot-plant P logger file)
										 )
									  (slot-ref self 'data)
									  ;;(adjusted-plot-polygon file 0.4 0.0 #f prj ply)
									  ))
								  ))
							  )
							 
							 ;; This handles fields
							 (let ((P (create <plant> (my 'taxon) 'agent-state 'indeterminate 'mass 0 'peak-mass 0 'age 0 'location #f 'leaf-area 0 'forage-damage 0 'water-stress 0)))
								(for-each
								 (lambda (p)
									(slot-set! P 'agent-state (car p))
									(slot-set! P 'mass (cadr p))
									(slot-set! P 'peak-mass (caddr p))
									(slot-set! P 'age (list-ref p 3))
									(slot-set! P 'location (list-ref p 4))
									(slot-set! P 'leaf-area (list-ref p 5))
									(slot-set! P 'forage-damage (list-ref p 6))
									(slot-set! P 'water-stress (list-ref p 7))
									(for-each
									 (lambda (field)
										;;(dnl* "   fielding" field)
										(kdebug '(log-* log-plant) "[" (my 'name) ":"
												  (cnc self) "]" "checking" field)
										;;(dnl* "  " (cnc self) (if (has-slot? self field) "has" "doesn't have") field)
										(if (not (symbol? format)) (buggrit))
										(if (has-slot? self field)
											 (letrec ((r (slot-ref self field)))
												#t
												;;(dnl* "   processing" field "for" format "format")
												(cond
												 ((postscript? file)
												  (set! needs-newline #t)
												  (file 'show (string-append
																	(if (string? r)
																		 r
																		 (object->string r)) " "))
												  #t
												  )
												 ((text? file)
												  (set! needs-newline #t)
												  (file 'show (string-append
																	(if (string? r)
																		 r
																		 (object->string r)) " "))
												  #t
												  )
												 ((output-port? file)
												  (let ((show-field-name
															(slot-ref logger 'show-field-name))
														  (missing-val
															(slot-ref logger 'missing-val))
														  )
													 (if show-field-name
														  (begin
															 (set! needs-newline #t)
															 (if leading-entry
																  (file 'display " ")
																  (set! leading-entry #t))
															 (set! needs-newline #t)
															 (file 'display field)))
													 
													 (let ((val (if (eqv? field 'name) 
																		 (if (slot-ref self 'domain)
																			  (string-append
																				(slot-ref (slot-ref self 'domain) 'name) ":" (name self))
																			  (name self))
																		 (if (has-slot? self field)
																			  (slot-ref self field)
																			  (slot-ref logger 'missing-val)))))
														(if leading-entry 
															 (file 'display " ")
															 (set! leading-entry #t))
														(set! needs-newline #t)
														(file 'display val))
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
								 (slot-ref self 'data)
								 )
								;;(dnl* "Did not process" (name self))
								)

							 (if needs-newline (file 'newline))

							 (kdebug (list 'log-* 'log-plant (my 'name) (my 'taxon))
										"<plant>:log-data --- Leaving [" (my 'name) ":" (cnc self) "]"
										"in log-data")
							 )
						  )
					 ))


(model-body% <plant-array>
  ;;(dnl* (cnc self) "model-body")
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


;;(error "the plant-array agent seems to be disappearing!")

   (dnl* "Plant-array model body" t dt)

  (let ((parent-return (call-all-parents)))
	 (start-timer 'plant-array-model-body)

	 (if (uninitialised? (slot-ref self 'test-subject)) (slot-set! self 'test-subject (create <example-plant> (my 'taxon))))
	 (if (uninitialised? (slot-ref self 'patch-list)) (slot-set! self 'patch-list (kernel 'locate* (*is-class? <patch>))))
	 (if (uninitialised? (slot-ref self 'lai)) (slot-set! self 'lai (numeric-parameter-lookup <> (my 'taxon) 'lai)))

	 (set-my! 'data (filter (lambda (x) (not (member (car x) '(terminated remove)))) (my 'data)))


	 (if (not (list? (my 'data))) (epic fail))
	 (let* ((test-subject (my 'test-subject))
			  (patch-list (my 'patch-list))
			  (mass-at-age (slot-ref test-subject 'mass-at-age))
			  ;; setters
			  (state          car)
			  ;; This will either be 'dead, 'alive, or 'remove
			  (mass           cadr)
			  (peak-mass      caddr)
			  (age            (lambda (x) (list-ref x 3)))
			  (location       (lambda (x) (list-ref x 4)))
			  (leaf-area      (lambda (x) (list-ref x 5)))
			  (forage-damage  (lambda (x) (list-ref x 6)))
			  (water-stress   (lambda (x) (list-ref x 7)))
			  (domain   		(lambda (x) (list-ref x 8)))

			  ;;getters                 ;\
			  (set-state!         set-car!)
			  (set-mass!          set-cadr!)
			  (set-peak-mass!     set-caddr!)
			  (set-age!           (lambda (x v) (list-set! x 3 v)))
			  (set-location!      (lambda (x v) (list-set! x 4 v)))
			  (set-leaf-area!     (lambda (x v) (list-set! x 5 v)))
			  (set-forage-damage! (lambda (x v) (list-set! x 6 v)))
			  (set-water-stress!  (lambda (x v) (list-set! x 7 v)))
			  (set-domain!			 (lambda (x v) (list-set! x 8 v)))

			  (fruiting-probability (slot-ref test-subject 'fruiting-probability))
			  (mort-prob (slot-ref test-subject 'mort-prob))
			  (decay-rate (slot-ref test-subject 'decay-rate))
			  (reproduction-offset (slot-ref test-subject 'reproduction-offset))
			  (reproduction-period (slot-ref test-subject 'reproduction-period))
			  (regrowth-rate-multiplier (slot-ref test-subject 'regrowth-rate-multiplier))
			  (lai (slot-ref test-subject 'lai))
			  )

		(for-each
		 (lambda (p)
			(if (not (isa?  (domain p) <patch>))
				 (let ((pl (filter (lambda (pch) (contains? pch (location p))) patch-list)))
					(set-domain! p pl)))
			
			(cond
			 ((eq? (state p) 'dead) (set-mass! p (* (mass p) (exp (* (slot-ref test-subject 'decay-rate) dt)))))
			 
			 ;; check for death
			 ((or (< (random-real) (slot-ref test-subject 'mort-prob))(< (- (mass p) (forage-damage p)) (* 1/3 (peak-mass p))))
			  (set-state! p 'dead))
			 ((let ((offset (slot-ref test-subject 'reproduction-offset))
					  (period (slot-ref test-subject 'reproduction-period)))
				 (and (< (remainder (inexact->exact (round (- t offset))) (inexact->exact (round period)))) (< (random-real) fruiting-probability)))
			  ;; fruit!

			  (slot-set! test-subject 'domain (domain p))
			  (slot-set! test-subject 'subjective-time t)
			  (slot-set! test-subject 'mass (mass p))
			  (slot-set! test-subject 'leaf-area (leaf-area p))
			  (slot-set! test-subject 'forage-damage (forage-damage p))
			  (slot-set! test-subject 'age (age p))

			  ;; add fruit to appropriate ecoservices
			  (do-fruiting test-subject t dt)

			  #t)
			 (else
			  (let ((gr (plant-growth mass-at-age (age p) dt)))
				 ;; check for water stress, reduce growth if necessary
				 (set-mass! p (+ (mass p) gr ))
				 (set-age! p (+ (age p) dt))
				 (set-forage-damage! p (max 0 (- (forage-damage p) (* gr (slot-ref test-subject 'regrowth-rate-multiplier)))))
				 (set-leaf-area! p  (general-leaf-area (mass p) (slot-ref test-subject 'lai)))
				 )
			  ))
			(stop-timer 'plant-model-body)
			)
		 (my 'data))
		dt
		)
	 ))


;; Plants are done.

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: ";;; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
