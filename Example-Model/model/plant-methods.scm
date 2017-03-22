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

"
Oak tree from https://sylvia.org.uk/oneoak/treefacts.php

Tree height 23.9m
Age 222 years
Stem diameter 89.8cm
Crown diameter 17.8m
Timber height 12.9m
Timber volume 4.96m3
Leaf area index 1.7
Tree weight 14.385 tonnes
	 The complete weight details:
	 Stem to timber height 	6,036kg
	 Branchwood to 7cm 	6,137kg
	 Branchwood 7 - 4cm 	1,000kg
	 Lop & top < 4cm 	1,212kg
	 Total Weight 	14,385kg
Tree volume 11.58 m3 (wood for parts greater than 7cm dia)
Dry mass 7.86 tonnes
Carbon content 3.93 tonnes


Also 

... transpiration figures vary a lot depending on local
climate type, current weather (humidity etc), availability of water,
exact tree size, tree variety, etc

    a large oak tree can transpire 40,000 gallons of water per year

vccs.edu

There are about 240 US Gallons of water in a ton. SO the above figure
equates to an average of about 0.45 tons of transpired water a day for
a full canopy. However, trees in temperate climates are much more
active in summer than in winter, so the peak daily value might be
several times higher.

    the full-grown oak (Quercus robur L.) tree ... [has] sap flow rate
values ... of up to 400 Kg per day ... 100 years of age, 33 m height.

Sap Flow Rates and Transpiration Dynamics in the Full-Grown Oak ...

400 Kg is about 0.44 tons or exactly 0.4 tonnes.


So, early in life, the height increases faster than mass, but later
(when gravity calls to collect the bill), mass must increase faster, 
... we'll consider a relationship like

   h ~ K sqrt(m)

for a suitable K.  For oaks, K might be 0.19176657111268274052, if we
use the oneoak example.  This overestimates the mass of very small
trees(a 30cm plant has a putative mass of ~2.25kg).
This is probably not  enough to wreck our plaything.

However, since the cube root of the tree's mass is remarkably close to
its height, and this relation give a mass of 670.g for a 30cm tree;
this seems a simpler basis from which to work.

Now let's consider the radius of the tree (above and below). From the
data, it looks as though a radius which is 3/8 * h may be close enough.

"


(define (leaf-area p) ;; leaf area
  (let ((pi (acos -1.0)))
	 (* (slot-ref p 'lai) pi (sqr (plant-radius p)))))

(define (plant-mass->height m) ;; given mass
  (power m 1/3))
 ;; h = m^{1/3}

(define (plant-height->mass h) ;; given height
  (power h 3))
 ;; h = m^{1/3}

(define (plant-mass->radius m) ;; given mass
  (* 3/8 (power m 1/3)))
;; r = 3/8 * m^{1/3}


(define (mass->half-sphere-area m) ;; given mass
  (let ((pi (acos -1.0)))
  (* 9/32 pi (power m 2/3))))
;; half the area of a sphere
;; A = 1/2 * 4 pi r^2
;;   = 2 pi (3/8 m^{1/3})^2
;;   = 9/32 pi m^{2/3}

(define (mass->half-sphere-vol m) ;; given mass
  (let ((pi (acos -1.0)))
  (* 9/256 pi m)))
;; half the volume of a sphere (since leaves aren't just at the
;; margins, L is a const

;; A = 1/2 L 4/3 pi r^3
;;   = 1/2 L pi 4/3 (3/8 * m^{1/3})^3
;;   = 1/2 4/3 (3/8)^3 L pi m
;;   = 9/256 L pi m

;; which indicates that the leaf area is proportional to the mass of
;; the tree ...  Since L is just a tuning constant and 9pi/256 is very
;; close to 0.11, we could just pick some appropriate L


(define (water-stress-level avail need)
  (if (< need avail)
		0
		(- 1.0 (sigmoid* (/ avail need)))))


(model-method <plant> (fruit-count self)
				  (* (slot-ref self 'fruiting-rate
									(slot-ref self 'mass)
									(if (slot-ref self 'water-stress-effect)
										 (- 1 (sqr (slot-ref self 'water-stress)))
										 1.0)
									))
				  )


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
 
;----- (self-assessment)
(UNFINISHED-BUSINESS "<plant>: This is a placeholder ... it almost certainly needs to change")
(model-method <plant> (representation-assessment self . args) 
				  (if (null? args) ;; only passed self
						0
						(let ((n (car args)) ;; mixing assumption 
								)
						  (+ (/ n (my 'population-switch)))
						  ))
				  )


												 
(model-method <plant> (plant-radius self)
				  (plant-mass->radius (slot-ref self 'mass)))

(define (simple-plant-initfunc self)
	 (set-state-variables self (list 'reproduction-period 
												(if #f
													 (random-real) ;; a probability, or
													 (+ 30 (random-integer 30)); an interval
													 )
												
												'reproduction-offset (random-integer 20)
												'reproduction-mechanism #f ;; <fruit> or a
												;; procedure which performs an update on
												;; some undefined thing. 
												))


	 (slot-set! self 'mass (* (random-real) (random-real)
									  (slot-ref self 'max-mass)))
	 (slot-set! self 'age
					(* (slot-ref self 'max-age)
						(/ (slot-ref self 'mass) (slot-ref self 'max-mass)))))

	

(model-body <plant>
	(kdnl* '(model-bodies plant-running)  (class-name-of self) (name self) "@" t "/" dt)

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


	(let* ((available-water (if ignore-water +inf.0 (value (slot-ref self 'water-service) 'value)))
			 (pi (acos -1.0))
			 (waterstress (my 'water-stress))
			 (lai (my 'lai))
			 (r (plant-radius self))
			 (waterneeds (* (my 'water-use) lai pi r r)) ;; lai buy
			 (stressed (my 'water-stress-effect))
			 (offset (my 'reproduction-offset))
			 (reproduction-point (modulo (- t offset) (my 'reproduction-period)))
			 (fruiting? #f)
			 (age (my 'age))
			 (mass (my 'mass))
			 )

	  (if (uninitialised? (my 'mass)) (error "mass not set for " (class-name-of (class-of self))))
	  (if (uninitialised? (my 'age)) (set-my! 'age (plant-mass->age (my 'mass))))
	  
	  ;; set water stress level and update hydrology

	  (set! waterstress (water-stress-level available-water waterneeds))

	  (cond	;; determine if the plant dies this step
		((or (>=  age (years 4)) (> mass 40000000))
		 (if halting-problem (error "Nothing to see here. Move along"))
		 'remove) ;; it's a stick, plant dies here.

		;; determine if the plant fruits or grows this step (not both)
		((and (negative? reproduction-point)
				(not (negative? (+ dt reproduction-point)))
				;;; (if (not water-stress-effect)
				;;; 	 #t
				;;; 	 'true-for-the-moment-but-need-to-have-a-test-here
				;;; 	 )
				)
		 (let ((num-fruit (fruit-count self))
				 (the-plot (slot-ref self 'reproduction-mechanism)))
			(if (list? the-plot)
				 ((car the-plot) (cadr the-plot) num-fruit)  ;; The list
																			;; is
																			;; constructed
																			;; with
																			;; "(list
																			;; method
																			;; ecoservice)"
				 (add! the-plot num-fruit))) ;; the-plot *must* be the
													  ;; ecoservice associated with
													  ;; fruit in the domain of
													  ;; this agent
		 dt)
		(#t ;; grow (or not)
		 (let* ((dm (- (my 'max-mass) mass))
				  (ma (my 'max-age))
				  (da (- ma age))
				  (ds (* (/ dm da) dt waterstress))
				  )
			(slot-set! self 'mass (+ mass ds)))
		 dt)
		)
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

(define (make-plant-xy class taxon env loc mass . otherargs)
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
						  'mass mass) otherargs)) ))
	 (UNFINISHED-BUSINESS "The age here is totally bogus")

	 (if (contains? env loc)
		  sp
		  (error "location for plant is not in indicated environment" loc env))))

(define (make-plant class taxon env mass . more)
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

  (kdnl* '(init plants) "env:" env  "args:"  more)
  (set! more (append (list 'habitat env
									'location (random-point env)
									'mass mass) more))
  (kdnl* '(init plants) "->args:"  more)
  (let ((sp (apply create (cons class (cons taxon more )))))
	 sp
	 ))


;--- Only logging methods below
		 
(model-method <plant> (map-log-data self logger format caller targets)
   (let ((file (slot-ref logger 'file)))
	  (kdnl* '(log-* log-plant)
				"[" (my 'name) ":" (class-name-of self) "]"
				"in log-data")

	  (for-each
		(lambda (field)
		  (kdnl* '(log-* log-plant) "[" (my 'name) ":"
					(class-name-of self) "]" "checking" field)
		  (if (has-slot? self field)
				(let ((r (slot-ref self field)))
				  (case format
					 ((ps)
					  (file 'show (string-append
										(if (string? r)
											 r
											 (object->string r)) " "))
					  )
					 ((text table dump)
					  (let ((show-field-name
								(slot-ref logger 'show-field-name))
							  (missing-val
								(slot-ref logger 'missing-val))
							  )
						 (if show-field-name
							  (begin
								 (if leading-entry 
									  (display " " file)
									  (set! leading-entry #t))
								 (display field file)))
						 
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
								 (display " " file)
								 (set! leading-entry #t))
							(display val file))
						 )
					  )

					 (else
					  (kdnl* '(log-* log-ecoservice)
								"[" (my 'name) ":" (class-name-of self) "]"
								"Ignoring " field " because I don't have it")
					  'ignore-unhandled-format)))
				(begin
				  (kdnl* '(log-* log-ecoservice)
							"[" (my 'name) ":" (class-name-of self) "]"
							"no service" field)
				  #f)))
		(uniq (if #t
					 targets
					 (filter (not-member (slot-ref logger 'dont-log))
								targets)))
		)
	  (newline file)
	  )
	)

(model-method <plant> (log-data% self logger format caller targets)
    (let ((file (slot-ref logger 'file)))
		(kdnl* '(log-* log-plant)
				 "[" (my 'name) ":" (class-name-of self) "]"
				 "in log-data")

		(for-each
		 (lambda (field)
			(kdnl* '(log-* log-plant) "[" (my 'name) ":"
					 (class-name-of self) "]" "checking" field)
			(if (has-slot? self field)
				 (let ((r (slot-ref self field)))
					(case format
					  ((ps)
						(file 'show (string-append
										 (if (string? r)
											  r
											  (object->string r)) " "))
						)
					  ((text table dump)
						(let ((show-field-name
								 (slot-ref logger 'show-field-name))
								(missing-val
								 (slot-ref logger 'missing-val))
								)
						  (if show-field-name
								(begin
								  (if leading-entry 
										(display " " file)
										(set! leading-entry #t))
								  (display field file)))
						  
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
								  (display " " file)
								  (set! leading-entry #t))
							 (display val file))
						  )
						)

					  (else
						(kdnl* '(log-* log-ecoservice)
								 "[" (my 'name) ":" (class-name-of self) "]"
								 "Ignoring " field " because I don't have it")
						'ignore-unhandled-format)))
				 (begin
					(kdnl* '(log-* log-ecoservice)
							 "[" (my 'name) ":" (class-name-of self) "]"
							 "no service" field)
					#f)))
		 (uniq (if #t
					  targets
					  (filter (not-memq (slot-ref logger 'dont-log))
								 targets)))
		 )
		(newline file)
		)
	 )

(UNFINISHED-BUSINESS "Need to flesh this out a bit")

;;; (agent-initialisation-method <example-plant> (args) (no-default-values) ;-
;;; 									  (set-state-variables self '()) ;-
;;; 									  (initialise-parent) ;-
;;; 									  (set-state-variables self args) ;; set specifics passed in here... ;-
;;; 									  ) ;-
 									  									  									  

(model-body <example-plant>
				(kdnl* 'model-body "pm 1")
				(let ((dt (apply min (*parent-bodies '*))))
				(kdnl* 'model-body "pm 2")
				  
				  (if (uninitialised? self 'peak-mass)
						(set-my! 'peak-mass (my 'mass)))

				  (if (uninitialised? self 'leaf-area)
						(set-my! 'leaf-area (leaf-area self)))

				  (let ((mass (my 'mass))
						  (pkmass (my 'peak-mass)))

					 (kdnl* 'model-body "pm 3")

					 ;; This comes before death, since plants will often seed/fruit in one last burst
					 (if (and (>= mass (my' fruiting-mass)) (> fruiting-prob (random-real)))
						  (add-fruit self (my 'cell) (power mass 2/3)))
					 
					 (if (< mass (* (my 'mort-mass) pkmass))
						  (die self)
						  (begin
							 (set! mass (grow dt mass))
							 (set-my! 'mass mass)
							 (if (> mass pkmass)
								  (set-my! 'peak-kmass mass))
							 )))
				(kdnl* 'model-body "pm 4")
				  dt
				  ))

(model-method <example-plant> (add-fruit self surface)
				  (let ((cell (my 'cell))
						  (spot (my 'location))
						  )
					 (kernel 'add-fruit cell (* (my 'fruiting-rate) surface))
				  ))
				
(model-method <example-plant> (die self)
				  (kdnl* 'death "<example-plant> dies: mass = " mass ", mort mass = " (* pkmass (my 'mort-mass)))
				  (set-my! 'agent-state 'dead)
				  (kernel 'shutdown self))


;; Plants are done.


;-  The End 


;;; Local Variables: 
;;; comment-end: ";-"
;;; comment-start: ";;; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
