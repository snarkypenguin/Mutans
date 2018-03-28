(include "framework")
; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	plant-methods.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.03
;		Location: zero:/home/randall/Thesis/Example-Model/model/plant-methods.scm
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
;
;	History:
;
;-  Code

(define ignore-water #t)

(define (water--level avail need)
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

;; This ties the growth rate to its age rather than its mass
(define (plant-growth mass-at-age age dt)
  (- (mass-at-age (+ age dt)) (mass-at-age age)))


(model-method <plant> (initialise-instance self)
	(kdebug '(<plant> initialisation) "IN <plant> initialise-instance")
	(parent-initialise-instance)

	(set-my! 'mass ((my 'mass-at-age) (my 'age)))

	(if (uninitialised? self 'mass-radius)
		 (set-my! 'mass-radius (std-mass-radius-function)))
	
	(if (uninitialised? self 'last-reproduced)
		 (set-my! 'last-reproduced 0))
	
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
				  (set-uninitialised-slots self '(plot-scale) 3)
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



(model-method (<plant> <number> <number>) (fruit-count self t dt #!optional locus)
				  (truncate (* (/ (slot-ref self 'fruiting-rate)
										(slot-ref self 'fruit-mass))
									dt (slot-ref self 'mass)
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
												 (map projection (make-circle-perimeter (my 'location) (* 250 (radius self)) (if (eq? (agent-state self) 'dead) 5 12))))
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

												 
(model-method <plant> (radius self #!optional locus)
				  (set-my! 'leaf-area (plant-leaf-area self))
				  (plant-mass->radius (slot-ref self 'mass)))

(model-method <plant> (leaf-area self #!optional locus)
				  (set-my! 'leaf-area (plant-leaf-area self))
				  (- (my 'leaf-area) (my 'forage-damage))) ;; forage-damage is to leaf area, *not* mass

(model-method <plant> (leaf-mass self #!optional locus)
				  (set-my! 'leaf-area (plant-leaf-area self))
				  (* (leaf-area self) (my 'leaf-mass)))

(model-method <plant> (pristine-leaf-mass self #!optional locus) ;; does not take forage damage into account
				  (set-my! 'leaf-area (plant-leaf-area self))
				  (* (my 'leaf-area) (my 'leaf-mass))) 


(model-body <plant>
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

	(kdebug 'model-body "plant 0")
	
	(if (not (my 'mass))
		 (set-my! 'mass ((my 'mass-at-age) (my 'age))))

	(let ((parent-return (call-all-parents)))
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

					 (if (ok-to-reproduce t (my 'age)
												 (my 'reproduction-age)
												 (my 'reproduction-offset)
												 (my 'reproduction-period)
												 (my 'reproduction-probability))
						  (do-fruiting self t dt))

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
		 return
		 )
	  dt)
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
				  (dnl* "plot-plant" (name  self) (name logger))
  (if (>= (mass self) (* 0.1 kg))				  
		(let* ((status (agent-state self))
				 (inner-glyph (if (eq? (agent-state self) 'dead) 5 (slot-ref self 'glyph)))
				 (outer-glyph (if (number? (slot-ref self 'glyph)) (+ 3 (slot-ref self 'glyph)) (slot-ref self 'glyph)))
				 (scale (let ((pscale (slot-ref self 'plot-scale)))
							 (cond
							  ((number? pscale) pscale)
							  ((symbol? pscale) (slot-ref self 'pscale))
							  ((procedure? pscale) (pscale self))
							  (else (error "bad plot-scale for plant" pscale)))))
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
																			  (leaf-mass self)))f
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
					 (file 'comment " dead plot-plant " (name self) " " (location self))
					 (file 'push-color (slot-ref self 'dead-color))
					 (adjusted-plot-polygon file 0.1 dead #t prj oply)
					 (adjusted-plot-polygon file 0.25 dead #f prj iply)
					 (file 'pop-color)
					 )
				  (begin
					 (file 'comment " plot-plant " (name self) " " (location self))
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
  )
  

(model-method (<plant> <log> <symbol> <list>) (log-data self logger format targets)
   (kdebug 'log* "(model-method (<plant> <log> <symbol> <list>) (log-data self logger format targets)")
   (dnl* "****"  (cnc logger) (cnc self) (name self) format  (symbol? format) (eq? format 'ps))
	(let ((file (slot-ref logger 'file))
			(leading-entry #f)
			(needs-newline #f)
			)
	  
	  
	  (if (or (my 'always-log) (and (member format '(ps)) (my 'always-plot)) (emit-and-record-if-absent logger self (my 'subjective-time)))
			  ;; this handles "whole of agent" bits, like perimeters
			  (cond
				((eq? format 'ps)
				 ;(dnl* "PSing match")
				 (plot-plant self logger file))
				((member format '(text))
				 ;; This handles fields
				 ;(dnl* "Not PSing around")
				 (for-each
				  (lambda (field)
					 ;;(dnl* "   fielding" field)
					 (kdebug '(log-* log-plant) "[" (my 'name) ":"
								(cnc self) "]" "checking" field)
					 ;;(dnl* "  " (cnc self) (if (has-data? self field) "has" "doesn't have") field)
					 (if (not (symbol? format)) (buggrit))
					 (if (has-data? self field)
						  (letrec ((r (data-ref self field)))
							 #t
							 ;;(dnl* "   processing" field "for" format "format")
							 (set! needs-newline #t)
							 (if (and (my 'always-log) (< t (my 'subjective-time)))
								  (file 'show "["))
							 (file 'show (string-append
											  (if (string? r)
													r
													(object->string r)) " "))
							 (if (and (my 'always-log) (< t (my 'subjective-time)))
								  (file 'show "]"))											 #t
								  )
							;;; ((output-port? file)
							;;;  (let ((show-field-name
							;;; 		  (data-ref logger 'show-field-name))
							;;; 		 (missing-val
							;;; 		  (data-ref logger 'missing-val))
							;;; 		 )
							;;; 	(if show-field-name
							;;; 		 (begin
							;;; 			(set! needs-newline #t)
							;;; 			(if leading-entry
							;;; 				 (file 'show " ")
							;;; 				 (set! leading-entry #t))
							;;; 			(set! needs-newline #t)
							;;; 			(file 'show field)))
								
							;;; 	(let ((val (if (eqv? field 'name) 
							;;; 						(if (data-ref self 'domain)
							;;; 							 (string-append
							;;; 							  (data-ref (data-ref self 'domain) 'name) ":" (name self))
							;;; 							 (name self))
							;;; 						(if (has-data? self field)
							;;; 							 (data-ref self field)
							;;; 							 (data-ref logger 'missing-val)))))
							;;; 	  (if leading-entry 
							;;; 			(file 'show " ")
							;;; 			(set! leading-entry #t))
							;;; 	  (set! needs-newline #t)
							;;; 	  (file 'display val))
							;;; 	)
							;;;  )

							(else
							 (kdebug '(log-* log-ecoservice)
										"<plant>:log-data [" (my 'name) ":" (cnc self) "]"
										"Ignoring " field " because I don't have it")
							 'ignore-unhandled-format)
							))
				  (uniq (if #t
								targets
								(filter (not-memq (slot-ref logger 'dont-log)) targets)))
				  )
				 )
				(else
				 (kdebug '(log-* log-ecoservice)
							"<plant>:log-data [" (my 'name) ":" (cnc self) "]"
							"no service" field)
				 #f))
			  (begin
				 #f
				 (dnl* "Did not process" (name self)))
			  )
	  
	  (if needs-newline (file 'newline))
	))


(UNFINISHED-BUSINESS "Need to flesh this out a bit")

;;; (agent-initialisation-method <example-plant> (args) (no-default-values) ;-
;;; 									  (set-state-variables self '()) ;-
;;; 									  (parent-initialise-instance) ;-
;;; 									  (set-state-variables self args) ;; set specifics passed in here... ;-
;;; 									  ) ;-


(model-method (<plant> <number> <number>) (do-fruiting self t dt)
				  (let* ((here (my 'domain))
							(num-fruit (fruit-count self t dt))
							(current-fruit (value here 'fruit))
							)
					 (set-value! here 'fruit (+ current-fruit num-fruit))))


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



(model-body <example-plant>
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
										(let ((parent-returns (call-all-parents)))
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

;(model-method <plant-proxy> (location self  #!optional locus)
;				  (if (not locus) (set! locus (data-ref self 'location)))
;				  (data-ref self 'location))

(model-method <plant-proxy> (mass self #!optional locus)
				  (if (not locus) (set! locus (data-ref self 'location)))
				  (data-ref self 'mass))

(model-method <plant-proxy> (set-mass! self nmass #!optional locus)
				  (if (not locus) (set! locus (data-ref self 'location)))
				  (data-set! self 'mass nmass))

												 
(model-method <plant-proxy> (radius self #!optional locus)
				  (if (not locus) (set! locus (data-ref self 'location)))
				  (plant-mass->radius (slot-ref self 'mass)))

(model-method <plant-proxy> (leaf-area self #!optional locus)
				  (if (not locus) (set! locus (data-ref self 'location)))
				  (data-set! self 'leaf-area (general-leaf-area (mass self) (data-ref self 'lai)))
				  (- (data-ref self 'leaf-area) (data-ref self 'forage-damage))) ;; forage-damage is to leaf area, *not* mass

(model-method <plant-proxy> (leaf-mass self #!optional locus)
				  (if (not locus) (set! locus (data-ref self 'location)))
				  (* (leaf-area self) (data-ref self 'leaf-mass)))

(model-method <plant-proxy> (pristine-leaf-mass self #!optional locus) ;; does not take forage damage into account
				  (if (not locus) (set! locus (data-ref self 'location)))
				  (data-set! self 'leaf-area (general-leaf-area (mass self) (data-ref self 'lai)))
				  (* (data-ref self 'leaf-area) (data-ref self 'leaf-mass)))

(model-body <plant-proxy>
				(abort "This should never happen"))


;-- Agent monitors -- collects data regarding the state of an agent's appropriateness

(model-method (<plant-array> <agent-monitor>) (do-assessment self monitor)
				  ;; This needs to deal with all the plants represented in the array

				  (let ((assessment ((slot-ref monitor 'assessment-function) self))
						  )
					 (slot-set! monitor 'aggregate-data (cons assessment (slot-ref monitor 'aggregate-data)))
					 )
				  )



(model-method (<plant-array>) (resolve-assessment self assessment-list)
				  "An assessment is one of the following:
                  #t or a self assessment in the form of some sort of 'error value' [0 would not necessarily be 
                  treated the same way as #t]

               a list of alien assessments -- each of the members of the list consists of lists comprised of 
               the entities with problems, their current representations, an indication of what sort of problem 
               they suffer from, and how severe the problem is (an indication of the magnitude of the error induced).
               
					The 'self' object must necessarily be a member of one of these lists, and it may peruse the 
               other members to address any impact their states may have on the potentially changing representation.
               
               The first thing the agent does is check its status-flags to see if it is in a 'resolved' state, or if
               there is an entry indicating that there is an agent it should migrate to.  If action has already been 
               taken on its behalf; if so, it quiently returns, other wise it will continue in order to address the 
               flagged condition itself.  Resolution may be a matter of migrating to more aggregated form, converting
               an aggregrated form into a less aggregated form, moving to a higher/lower fidelity representation or 
               simply indicating in its own status-flags slot that it is currently unable to change.

               An example in this context might be that a plant (the first in the list) may be passed a list of 
               eighty plants (including itself) which have been marked as numerous enough to warrant an array-like 
               approach.  The plant in question then triggers the code in the resolve assessment then creates a 
               plant-array if one doesn't exist, and populates it with the appropriate members of the assessment-list.
               These members then have their states changed to indicate that they have been subsumed.

               Problems are described as lists of the form '(error-type desired-rep-type appropriate-scale)
               where an error type might be things like 'spatial-representation, 'too-few,  'too-many
               and 'time-step-too-short, timestep-too-long ...; desired-rep-type would typically be either 
               a class, a 'requires' symbol, or a symbol indicating a needed attribute (a method, 
               a slot-variable, or a more generic tag); an indicated scale is typically a number or list of 
               numbers.
              "
				  (let ((my-status-flags (slot-ref self 'status-flags))
						  (my-assessment-record (assq self (assessment-list)))
						  )
					 (cond
					  ((null? assessment-list) #t ;; Just keep going
						#t ;; indicates that all is good here, 
						)
					  ((not (pair? assessment-list))
						(error 'bad-assessment-list assessment-list)
					  )
					  ((member 'unable-to-change-representation my-status-flags)
						'frozen-representation
						)
					  
					  )
					 )
				  )

(model-method% (<plant-array> <log> <symbol> <list>) (log-data self logger format targets)
		;;(dnl* "****"  (cnc logger) (cnc self) (name self) format  (symbol? format) (eq? format 'ps))
		(kdebug 'log* "(model-method% (<plant-array> <log> <symbol> <list>) (log-data self logger format targets)")

		(let* ((file (slot-ref logger 'file))
				 (leading-entry #f)
				 (needs-newline #f)
				 (show-field-name (data-ref logger 'show-field-name))
				 (missing-val (data-ref logger 'missing-val))
				 (P (slot-ref self 'test-subject))
				 )
		  (if (not (is-class? P <plant>)) (error "Reached log-data uninitialised"))

		  (kdebug 'log* "#####"  t "---" (cnc logger) (cnc self) (name self) 'format: format)

		  ;;(dnl* "plant-array:" format 'plot? (my 'always-plot) 'length (length (data-ref self 'data)))
		  (if (or (and (member format '(ps)) (my 'always-plot)) (my 'always-log) (emit-and-record-if-absent logger self (my 'subjective-time)))
				(begin
				  (set-state-variables P (list 'taxon (my 'taxon) 'agent-state 'alive 'mass 0 'peak-mass 0 'age 0 'location (list 0 0 0) 'leaf-area 0 'forage-damage 0 'water-stress 0))
						
				  ;; this handles "whole of agent" bits, like perimeters
				  (for-each
					(lambda (q)
					  (let ((p (data-ref self q)))
						 ;;(dnl* "Plotting <plant-array>:" p)
						 (slot-set! P 'name (string-append (name self) "[" (number->string q) "]"))
						 (slot-set! P 'agent-state (list-ref p 0))
						 (slot-set! P 'mass (list-ref p 1))
						 (slot-set! P 'peak-mass (list-ref p 2))
						 (slot-set! P 'age (list-ref p 3))
						 (slot-set! P 'location (list-ref p 4))
						 (slot-set! P 'leaf-area (list-ref p 5))
						 (slot-set! P 'forage-damage (list-ref p 6))
						 (slot-set! P 'water-stress (list-ref p 7))
						 (slot-set! P 'domain (list-ref p 8))
						 (slot-set! P 'last-reproduced (list-ref p 9))

						 ;;(dnl* "** Plotting" (name P))

						 (cond
						  ((postscript? file)
							;;(dnl* "PS: " (name P))
							(kdebug 'log* "postscript output")
							(let* (;;(ply (make-circle-perimeter
									 ;;		 (my 'location) (* (my 'plot-magnification)  (sqrt (leaf-area self))) (my 'glyph)))
									 ;;(prj (composite-prj_src->dst self logger))
									 ;;(pply (map prj ply))
									 )
							  (plot-plant P logger file)

							  (for-each
								(lambda (f)
								  (let* ((textoffset (list 0 (* 4 m))))
									 (file 'moveto (map + (list-ref P 4) textoffset))
									 (set! needs-newline #t)
									 (file 'show (string-append
													  (if (string? f)
															f
															(object->string f)) " "))
									 (file 'show (object->string (data-ref P f)))
									 ))
								(data-ref logger 'variables))
							  )
							)
						  ((text? file)
							(kdebug 'log* "text output")
							;;(dnl* "TXT: " (name P))
							(for-each
							 (lambda (f)
								(set! needs-newline #t)
								(if show-field-name
									 (file 'show (string-append
													  (if (string? f)
															f
															(object->string f)) " ")))
								(if (has-data? P f)
									 (file 'show (data-ref P f))
									 (file 'show (data-ref self f)))
								(file 'show " "))
							 (data-ref logger 'variables))
							(if needs-newline
								 (begin
									(set! needs-newline #f)
									(file 'newline)))
							)
						  (else (dnl* "Whaaaat?" (pp file) (name self) (name logger) ))
						  )
						 )
					  )
					(seq (length (slot-ref self 'data)))
					))
				)
		  ))


;; Isn't this a bit pointless ;-)
;(model-method <plant-array> (location self  #!optional locus radius)
;				  (dnl* 'plant-array:location)
;				  (let ((locix (if locus (lookup-locus self locus radius) #f)))
;					 (if (pair? locix)
;						  (data-ref self locix 'location)
;						  (data-ref self 'location))))

;(model-method <plant-array> (location* self #!optional locus radius)
;				  (dnl* 'plant-array:location*)
;				  (if (not (point? locus))
;						(data-ref self 'location)
;						(if (not (number? radius))
;							 (set! radius +inf.0)))
;
;				  (let ((data (self 'location)))
;					 (filter (lambda (x) (<= (distance x locus) radius))
;								data))
;				  )

(define (field-functor self field #!optional locus radius)
				  (if (not locus)
						(data-ref self field)
						(if (number? radius)
							 (let ((dd (data-ref* self (list 'location field))))
								(map cadr (filter (lambda (l) (<= (distance locus (rec-ref l field)) radius)) 
														
														(slot-ref self 'data))))
							 (data-ref self locus field))))


														
(model-method <plant-array> (mass self #!optional locus radius)
				  (let ((locix (if locus (lookup-locus self locus radius) #f)))
					 (if (pair? locix)
						  (data-ref self locix 'mass)
						  0)))

(model-method (<plant-array>) (mass* self #!optional locus radius)
				  (field-functor self 'mass locus radius))


(model-method (<plant-array> <real>)(set-mass! self newmass #!optional locus radius)
				  (let ((locix (if locus (lookup-locus self locus #t) #f)))
					 (if (pair? locix)
						  (data-set! self locix 'mass newmass)
						  (abort))))
						  
(model-method (<plant-array>) (radius* self #!optional locus rad)
				  (map plant-mass->radius (mass* self locus rad)))


(model-method (<plant-array>) (radius self #!optional locus radius)
				  (let ((locix (if locus (lookup-locus self locus radius) #f)))
					 (if locix
						  (plant-mass->radius (data-ref self locix 'mass))
						  0)))
						  
(model-method (<plant-array>) (leaf-area self #!optional locus radius)
				  (let ((locix (if locus (lookup-locus self locus radius) #f)))
					 (if (point? locix)
						  (begin
							 (data-set! self locix 'leaf-area (general-leaf-area (mass self) (data-ref self locix 'lai)))
							 (- (data-ref self locix 'leaf-area) (data-ref self locix 'forage-damage))) ;; forage-damage is to leaf area, *not* mass
						  0)))


(model-method (<plant-array>) (leaf-area* self #!optional locus rad)
				  (let ((lai (numeric-parameter-lookup <> ('my taxon) 'lai)))
					 (data-set! self 'leaf-area (map (lambda (x) (general-leaf-area x lai)) (mass* self locus rad)))
					 (map - (data-ref self 'leaf-area) (data-ref self 'forage-damage))

				  (let ((locix (if locus (lookup-locus self locus rad) #f)))
					 (if (point? locix)
						  (begin
							 (data-set! self locix 'leaf-area (general-leaf-area (mass self) (data-ref self locix 'lai)))
							 (- (data-ref self locix 'leaf-area) (data-ref self locix 'forage-damage))) ;; forage-damage is to leaf area, *not* mass
						  0)))
				  )

(model-method (<plant-array>) (leaf-mass self #!optional locus radius)
				  (let ((locix (if locus (lookup-locus self locus radius))))
					 (if (pair? locix)
						  (* (leaf-area self locix) (numeric-parameter-lookup <> ('my taxon) 'leaf-mass))
						  0)))

(model-method (<plant-array>) (leaf-mass* self #!optional locus rad)
				  (let ((lai (numeric-parameter-lookup <> ('my taxon) 'leaf-mass)))
					 (data-set! self 'leaf-area (map (lambda (x) (general-leaf-area x lai)) (mass* self locus rad)))
					 (map - (data-ref self 'leaf-area) (data-ref self 'forage-damage))

				  (let ((locix (if locus (lookup-locus self locus rad) #f)))
					 (if (point? locix)
						  (begin
							 (data-set! self locix 'leaf-area (general-leaf-area (mass self) (data-ref self locix 'lai)))
							 (- (data-ref self locix 'leaf-area) (data-ref self locix 'forage-damage))) ;; forage-damage is to leaf area, *not* mass
						  0))))



(model-method (<plant-array>) (pristine-leaf-mass self #!optional locus radius) ;; does not take forage damage into account
				  (let ((locix (if locus (lookup-locus self locus #f))))
					 (if (pair? locix)
						  (begin
							 (data-set! self locix 'leaf-area (general-leaf-area (mass self locix) (data-ref self locix 'lai)))
							 (* (data-ref self locix 'leaf-area) (data-ref locix self locus 'leaf-mass)))
						  0)))

(model-method <plant-array> (initialise-instance self)
  (parent-initialise-instance)

  (slot-set! self 'data-names '(state mass peak-mass age location leaf-area forage-damage water-stress domain last-reproduced))

  (if (uninitialised? (slot-ref self 'test-subject)) (slot-set! self 'test-subject (create <example-plant> (my 'taxon))))
  (if (uninitialised? (slot-ref self 'lai)) (slot-set! self 'lai (numeric-parameter-lookup <> (my 'taxon) 'lai)))
  )



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

  ;;(dnl* "Plant-array model body" t dt)

  (let ((parent-return (call-all-parents)))

	 ;; This must be in the model-body because it needs the kernel pointer to resolve the call to locate*
	 (if (uninitialised? (slot-ref self 'patch-list)) (slot-set! self 'patch-list (kernel 'locate* (*is-class? <patch>))))
	 ;; If patches "died" we would need to do this more often than just the once.

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
			  (last-reproduced (lambda (x) (list-ref x 9)))

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
			  (set-last-reproduced! (lambda (x v) (list-set! x 9 v)))

			  (fruiting-probability (slot-ref test-subject 'fruiting-probability))
			  (mort-prob (slot-ref test-subject 'mort-prob))
			  (decay-rate (slot-ref test-subject 'decay-rate))
			  (reproduction-age (slot-ref test-subject 'reproduction-age))
			  (reproduction-offset (slot-ref test-subject 'reproduction-offset))
			  (reproduction-period (slot-ref test-subject 'reproduction-period))
			  (reproduction-cycle (slot-ref test-subject 'reproduction-cycle))
			  (reproduction-prob (slot-ref test-subject 'reproduction-probability))
			  (regrowth-rate-multiplier (slot-ref test-subject 'regrowth-rate-multiplier))
			  (lai (slot-ref test-subject 'lai))
			  )

		(for-each
		 (lambda (p ix)
			(let ((here (domain p)))
			  (if (not (isa? here <patch>))
					(let ((pl (filter (lambda (pch) (contains? pch (location p))) patch-list)))
					  ;;(dnl* "Setting domain for plant at" (location p))
					  (set-domain! p pl)))
			  
			  (cond
				((eq? (state p) 'dead)
				 ;;(dnl* 'dead-one)
				 (set-mass! p (* (mass p) (exp (* (slot-ref test-subject 'decay-rate) dt)))))
				
				;; check for death
				((or (< (random-real) (slot-ref test-subject 'mort-prob))(< (- (mass p) (forage-damage p)) (* 1/3 (peak-mass p))))
				 ;;(dnl* 'Newly-dead)
				 (set-state! p 'dead))

				(else 
				 (slot-set! test-subject 'plot-scale (my 'plot-scale))
				 (slot-set! test-subject 'domain here)
				 (slot-set! test-subject 'subjective-time t)
				 (slot-set! test-subject 'mass (mass p))
				 (slot-set! test-subject 'leaf-area (leaf-area p))
				 (slot-set! test-subject 'forage-damage (forage-damage p))
				 (slot-set! test-subject 'age (age p))

				 (if (ok-to-reproduce t (age p) reproduction-age reproduction-offset reproduction-period reproduction-prob)
					  (begin
						 ;; fruit!
						 ;; add fruit to appropriate ecoservices
						 (let* ((num-fruit (fruit-count test-subject t dt))
								  (current-fruit (value here 'fruit))
								  )
							(set-value! here 'fruit (+ current-fruit num-fruit)))))

				 (let ((gr (plant-growth mass-at-age (age p) dt)))
					;;(dnl* "Setting mass from" (mass p) "to" (+ (mass p) gr))
					;; check for water stress, reduce growth if necessary
					(@data-set! self ix 'mass (+ (mass p) gr ))
					(@data-set! self ix 'age  (+ (age p) dt))
					(@data-set! self ix 'forage-damage (max 0 (- (forage-damage p) (* gr (slot-ref test-subject 'regrowth-rate-multiplier)))))
					(@data-set! self ix 'leaf-area (general-leaf-area (mass p) (slot-ref test-subject 'lai)))
					)
				 ))
			  ))
		 (my 'data) (seq (length (my 'data))))
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
