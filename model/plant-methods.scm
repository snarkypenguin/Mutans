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
 

(model-method (<plant> <procedure>) (ps-dump self ps)
				  (let ((ms->ps (let ((p (assoc 'model->ps *default-projections*))) (if p (cdr p) (error "bad projection to ps"))))
						  )
					 
					 (dnl* "In plant ps-dump")
					 (ps 'moveto (ms->ps (local->model self (my 'location))))
					 (ps 'show-table
						  (list (string-append "<plant>" (name self))
								  (string-append "mass =" (number->string (my 'mass)))
								  (string-append "radius =" (number->string (plant-radius self)))
								  ))
					 (dnl 'bork)
					 (ps 'moveto (ms->ps (local->model self (my 'location))))
					 (dnl 'Bork)
					 (adjusted-plot-polygon ps 0.4 0.5 #f (lambda (x) x)
													(map ms->ps (make-circle-perimeter (my 'location) (* 250 (plant-radius self)))))
				  (dnl* "leaving plant ps-dump")
				  ))


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
	(kdebug '(trace-bodies plant-running)  (class-name-of self) (name self) "@" t "/" dt)

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

	(if (uninitialised? (my 'mass))
		 (error "Mass not initialised for a <plant>" (name self)))

	(if (uninitialised#? self 'peak-mass)
		 (set-my! 'peak-mass (my 'mass)))

	(if (uninitialised#? self 'leaf-area)
		 (set-my! 'leaf-area (leaf-area self)))

	
	(kdebug 'model-body "plant 0")

	(call-next-parent-body)

	(let* ((available-water (if ignore-water +inf.0 (value (slot-ref self 'water-service) 'value)))
			 (pi (acos -1.0))
			 (waterstress (my 'water-stress))
			 (lai (my 'lai))
			 (r (plant-radius self))
			 (waterneeds (* (my 'water-use) lai pi r r)) ;; lai buy
			 (stressed (my 'water-stress-effect))
			 (offset (my 'reproduction-offset))
			 (reproduction-point (modulo (- t offset) (max 1 (my 'reproduction-period)))) ;; one day minimum
			 (fruiting? #f)
			 (age '?)
			 (mass '?)
			 )
	  (kdebug 'model-body "plant 2")
	 
	  
	  ;; 150 * (1-exp(0.2*ln(1-x)))
	  (if (uninitialised? (my 'age)) (set-my! 'age (* (my 'max-age) (- 1 (* (exp 0.2) (log (- 1 (abs (random-real)))))))))
	  (set! age (my 'age))
	  ;; 2*exp(0.86*log(x/37))/(x/37+1)
	  (if (uninitialised? (my 'mass)) (set-my! 'mass (* (my 'maxmass) (/ (* 2 (exp (* 0.86 (log (/ x (my 'max-age)))))) (+ (/ x (my 'max-age)) 1)))))
	  (set! mass (my 'mass))

	  ;; set water stress level and update hydrology

	  (set! waterstress (water-stress-level available-water waterneeds))

	  (kdebug 'model-body "plant 3")
	  (cond	;; determine if the plant dies this step
		((>=  age (* years (my 'max-age)))
		 ;;(if halting-problem (error "Nothing to see here. Move along"))
		 'remove) ;; it's a stick, plant dies here.

		;; determine if the plant fruits or grows this step (not both)
		((and (negative? reproduction-point))
				(not (negative? (+ dt reproduction-point)))
				;;; (if (not water-stress-effect)
				;;; 	 #t
				;;; 	 'true-for-the-moment-but-need-to-have-a-test-here
				;;; 	 )
				
				(let* ((here (my 'habitat))
						 (num-fruit (fruit-count self))
						 (current-fruit (value here 'fruit))
						 )
				  (set-value! here 'f (+ current-fruit num-fruit))
				  )
				(kdebug 'model-body "plant 4a")
				
				dt)
		(#t ;; grow (or not)
		 (if (or (not (my 'water-stress-effect))
					(< (my 'water-stress) 1))
			  (slot-set! self 'mass (+ mass (* dt (my 'growth-rate)))) ;; linear growth
			  )
		 
		 (kdebug 'model-body "plant 4b")
		 dt)
		)
	  ))

(define plant-arg-order
  '(
     lai water-use  
       water-stress-effect reproduction-mass
       reproduction-mechanism
       fruiting-rate seeds-per-fruit
       reproduction-period
       reproduction-offset
       max-age max-mass))

(define% (make-plant class taxon env mass #!rest more)
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
									'mass mass) more))
  (kdebug '(init plants) "->args:"  more)
  (let ((sp (apply create (cons class (cons taxon more )))))
	 sp
	 ))



(define% (make-plant-xy class taxon env loc mass . otherargs)
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


;--- Only logging methods below
		 
;;; (model-method (<plant> <log-introspection> <symbol> <list>) (map-log-data self logger fmt targets)
;;; 				  (let ((file (slot-ref logger 'file))
;;; 						  (leading-entry #f)
;;; 						  (needs-newline #f)
;;; 						  )
;;; 					 (display kdebug (list 'PSsst 'log-* 'log-plant (my 'name) (my 'taxon))
;;; 								"<plant>:map-log-data --- Entering [" (my 'name) ":" (class-name-of self) "]"
;;; 								"in log-data")

;;; 					 (if (emit-and-record-if-absent logger self (my 'subjective-time))
;;; 						  (for-each
;;; 							(lambda (field)
;;; 							  (if (has-slot? self field)
;;; 									(letrec ((r (slot-ref self field)))
;;; 									  #t

;;; 									  (case fmt
;;; 										 ((ps)
;;; 										  ;;							(set! needs-newline #t)
;;; 										  (file 'show (string-append
;;; 															(if (string? r)
;;; 																 r
;;; 																 (object->string r)) " "))
;;; 										  )
;;; 										 ((text table dump)
;;; 										  (let ((show-field-name
;;; 													(slot-ref logger 'show-field-name))
;;; 												  (missing-val
;;; 													(slot-ref logger 'missing-val))
;;; 												  )
;;; 											 (if show-field-name
;;; 												  (begin
;;; 													 (set! needs-newline #t)
;;; 													 (if leading-entry
;;; 														  (display " " file)
;;; 														  (set! leading-entry #t))
;;; 													 (set! needs-newline #t)
;;; 													 (display field file)))
											 
;;; 											 (let ((val (if (eqv? field 'name) 
;;; 																 (if (slot-ref self 'habitat)
;;; 																	  (string-append
;;; 																		(slot-ref (slot-ref self 'habitat) 'name) ":" (name self))
;;; 																	  (name self))
;;; 																 (if (has-slot? self field)
;;; 																	  (slot-ref self field)
;;; 																	  (slot-ref logger 'missing-val)))))
;;; 												(if leading-entry 
;;; 													 (display " " file)
;;; 													 (set! leading-entry #t))
;;; 												(set! needs-newline #t)
;;; 												(display val file))
;;; 											 )
;;; 										  )

;;; 										 (else
;;; 										  (kdebug '(log-* log-ecoservice)
;;; 													 "<plant>:log-data [" (my 'name) ":" (class-name-of self) "]"
;;; 													 "Ignoring " field " because I don't have it")
;;; 										  'ignore-unhandled-format)))						 (begin
;;; 																										(kdebug '(log-* log-ecoservice)
;;; 																												  "<plant>:log-data [" (my 'name) ":" (class-name-of self) "]"
;;; 																												  "no service" field)
;;; 																										#f)))
;;; 							(uniq (if #t
;;; 										 targets
;;; 										 (filter (not-memq (slot-ref logger 'dont-log)) targets)))
;;; 							)
;;; 						  (void))

;;; 					 (if needs-newline (newline file))

;;; 					 (kdebug (list 'log-* 'log-plant (my 'name) (my 'taxon))
;;; 								"<plant>:map-log-data --- Leaving [" (my 'name) ":" (class-name-of self) "]"
;;; 								"in log-data")
;;; 					 )	
;;; 				  )

(model-method (<plant> <log-introspection> <symbol> <list>)
				  (log-data self logger fmt targets)
	(let ((file (slot-ref logger 'file))
			(leading-entry #f)
			(needs-newline #f)
			)
		(kdebug '(log-horrible-screaming log-plant (my 'name) (my 'taxon))
				 "<plant>:log-data Entering [" (my 'name) ":" (cnc self) "]"
				 "in log-data")

;		(write fmt)(dnl* " ..." (cnc self) (cnc logger) " ... NARP.")
		
		(if (emit-and-record-if-absent logger self (my 'subjective-time))
			 (for-each
			  (lambda (field)
;				 (dnl* "ERRRRR," field)
				 (kdebug '(log-* log-plant) "[" (my 'name) ":"
							(class-name-of self) "]" "checking" field)
				 (if (has-slot? self field)
					  (letrec ((r (slot-ref self field)))
						 #t

						 (case fmt
							((ps)
							 (adjusted-plot-polygon file 0.4 0.0 #f
															(composite-prj_src->dest
															 self logger)
															(make-circle-perimeter
															 (my 'location) (plant-radius self)))
;							(set! needs-newline #t)
;							(file 'show (string-append
;											 (if (string? r)
;												  r
;												  (object->string r)) " "))
;							(dnl* "YARP.")
;							(log-map-circle logger
;												 (local->model (my 'location))
;												 (car (local->model
;														 (make-list (length (my 'location)) (plant-mass->radius (my 'mass)))))
;												 #t
;												 'black)
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
										"<plant>:log-data [" (my 'name) ":" (class-name-of self) "]"
										"Ignoring " field " because I don't have it")
							'ignore-unhandled-format)))						 (begin
							(kdebug '(log-* log-ecoservice)
									  "<plant>:log-data [" (my 'name) ":" (class-name-of self) "]"
									  "no service" field)
							#f)))
				 (uniq (if #t
							  targets
							  (filter (not-memq (slot-ref logger 'dont-log)) targets)))
			  )
			 (void))

		(if needs-newline (newline file))

		(kdebug (list 'log-* 'log-plant (my 'name) (my 'taxon))
					"<plant>:log-data --- Leaving [" (my 'name) ":" (class-name-of self) "]"
					"in log-data")
		)
	)


(UNFINISHED-BUSINESS "Need to flesh this out a bit")

;;; (agent-initialisation-method <example-plant> (args) (no-default-values) ;-
;;; 									  (set-state-variables self '()) ;-
;;; 									  (initialise-parent) ;-
;;; 									  (set-state-variables self args) ;; set specifics passed in here... ;-
;;; 									  ) ;-
 									  									  									  
(model-body <example-plant>
				(if (uninitialised#? self 'peak-mass)
					 (set-my! 'peak-mass (my 'mass)))

				(if (uninitialised#? self 'leaf-area)
					 (set-my! 'leaf-area (leaf-area self)))

				(call-next-parent-body)

				(let ((mass (my 'mass))
						(age (my 'age))
						(pkmass (my 'peak-mass)))

				  (let ((draw (random-real)))
				  ;; This comes before death, since plants will often seed/fruit in one last burst
					 (if (and (>= mass (my 'fruiting-mass))
								 (> (my 'fruiting-prob) draw))
						  (add-fruit self (power mass 2/3)))
					 )
				  
				  (if (> age (my 'max-age))
						(die self) ;; too old.
						(begin
						  ;;(set! mass (grow dt mass))
						  (set-my! 'mass mass)
						  (if (> mass pkmass)
								(set-my! 'peak-mass mass))
						  )))
				dt
				)


(model-method <example-plant> (add-fruit self fruiting-area)
				  (dnl* (name self) "is adding fruit!")
				  (let ((cell (my 'cell))
						  (spot (my 'location))
						  )
					 (add-value! cell (* (my 'fruiting-rate) fruiting-area))
				  ))
				
(model-method <example-plant> (die self)
				  (kdebug 'death "<example-plant> dies: mass = " mass ", mort mass = " (* pkmass (my 'mort-mass)))
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
