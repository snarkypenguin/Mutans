(include "framework")
; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	animal.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.19
;		Location: odin:/home/gray/study/src/new/animal.scm
;
;	History:
;


;-  Discussion 

;-  Configuration stuff 

;-  Included files 

(include "framework")

;-  Variables/constants both public and static

;--    Static data

;-  Variables/constants both public and static

(define AMINGREY 0.2)
(define AMAXGREY 0.05)
(define ACIRCGREY 0.0)

;(require 'charplot)

;-  Code 

;---- <metabolism> methods

;;(define food-attenuation 0.25)
(define food-attenuation 0.5)
;;(define food-attenuation (/ 2.0 (exp 1.0))) ;; ~1.3


;----- initialise

;; (model-method <simple-metabolism> (initialise self args)
;; 				  (set-state-variables self (list 'hunger-limit 20.0 'days-of-hunger 0.0))
;; 				  (initialise-parent)
;; 				  ;; call "parents" last to make the initialisation list work
;; 				  (set-state-variables self args)
;; 				  )



(model-body <metabolism> ;; A thing with metabolism *must* have mass
						(kdebug '(model-bodies animal-running)  (class-name-of self) (name self) "@" t "/" dt)

						(call-next-parent-body)

						(if (not (number? (my 'mass)))
							 (kdebug 'stomach (my 'name) "is dead, long live the King"))

						(if (number? (my 'mass))
							 (let* ((struct-mass (my 'structural-mass))
									  (struct-prop (my 'structural-prop))
									  (starve-level (my 'starvation-level))
									  (mass (my 'mass))
									  (mass->food-conv-rate
										(my 'mass->food-conversion-rate))
									  (food->mass-conv-rate
										(my 'food->mass-conversion-rate))

									  (condn (my 'condition))

									  (cond->food-conv-rate
										(my 'condition->food-conversion-rate))
									  (food->cond-conv-rate
										(my 'food->condition-conversion-rate))

									  (guts (* struct-mass (my 'gut-size)))
									  (stomach-cont (my 'stomach-contents))
									  (stuffed-to-the-gills
										(and (number? mass)
											  (* mass (my 'max-consumption-rate) dt)))
									  (max-dinner
										(and stuffed-to-the-gills
											  (min stuffed-to-the-gills stomach-cont)))

									  (mgr (my 'max-growth-rate))
									  (cgr (my 'max-condition-rate))
									  (met-rate (my 'metabolic-rate))
									  (dead #f)

									  (cost (and mass (* met-rate mass dt)))
									  )

								(if (not (number? cgr)) (set! cgr mgr))
								;; condition can be put on at the same rate as
								;; mass unless specified
								
								;; do calculations here....

								;; mass and cost are set to false when the
								;; organism is dead

								(kdebug 'stomach "Metabolism: cost =" cost
										 "stomach-contents =" stomach-cont)

								(if cost 
									 (begin ;; deal with metabolic costs

										(kdebug 'metabolism "D0   stomach ="
												 stomach-cont "| condition =" condn
												 "| mass =" mass "|=| cost = " cost)

										;; .... with stomach-contents
										(let ((dc (- stomach-cont cost)))
										  (set! stomach-cont (max dc 0.0))
										  (set! cost (max (- dc) 0.0))
										  )

										(kdebug 'metabolism "D1   stomach =" stomach-cont
												 "| condition =" condn "| mass =" mass
												 "|=| cost = " cost)

										;; .... with condition
										(let ((cc (* cond->food-conv-rate condn)))
										  (let ((dc (- cc cost)))
											 (set! cc (max dc 0.0))
											 (set! cost (max (- dc) 0.0))
											 
											 (set! condn (max 0.0
																	(/ cc
																		cond->food-conv-rate)))))
										
										(kdebug 'metabolism "D2   stomach =" stomach-cont
												 "| condition =" condn "| mass =" mass
												 "|=| cost = " cost)

										;; .... with body mass
										(let ((cc (* mass->food-conv-rate mass)))
										  (let ((dc (- cc cost)))
											 (set! cc (max dc 0.0))
											 (set! cost (max (- dc) 0.0))
											 
											 (set! mass (max 0.0
																  (/ cc
																	  mass->food-conv-rate)))))
										
										
										(kdebug 'metabolism "D3   stomach =" stomach-cont
												 "| condition =" condn "| mass =" mass
												 "|=| cost = " cost)

										(if (or (<= mass struct-mass)
												  (<= (/ mass struct-mass) starve-level))
											 (begin
												(set! mass #f)
												(set-my! 'mass #f)) ;; Dead, y'know
											 )
										)
									 )

								(if (and mass (> stomach-cont 0.0))
									 (begin ;; Growth and condition updates

										(kdebug 'metabolism "G0   stomach =" stomach-cont
												 "| condition =" condn "| mass =" mass
												 "|=| cost = " cost)

										;; growth first
										(let* ((mm (/ stomach-cont food->mass-conv-rate))
												 (mg (* mgr dt))
												 (delta-m (min mm mg))
												 )
										  (kdebug 'metabolism "g0a          mm =" mm
													" | mg =" mg " | delta-m =" delta-m)
										  
										  (set! mass (max 0.0 (+ mass delta-m)))
										  (set! stomach-cont
												  (max 0.0
														 (- stomach-cont
															 (* delta-m food->mass-conv-rate))))
										  (kdebug 'metabolism "g0b          mm ="
													mm" | mg =" mg " | delta-m =" delta-m)
										  )

										(kdebug 'metabolism "G1   stomach =" stomach-cont
												 "| condition =" condn "| mass =" mass
												 "|=| cost = " cost)

										;; now condition
										(let* ((mm (/ stomach-cont food->cond-conv-rate))
												 (mg (* cgr dt))
												 (delta-m (min mm mg))
												 )
										  
										  (kdebug 'metabolism "g1a          mm =" mm
													" | mg =" mg " | delta-m =" delta-m)
										  (set! condn (max 0.0 (+ condn delta-m)))
										  (set! stomach-cont
												  (max 0.0
														 (- stomach-cont
															 (* delta-m
																 food->cond-conv-rate))))
										  (kdebug 'metabolism "g1b          mm ="
													mm" | mg =" mg " | delta-m =" delta-m)
										  )

										(kdebug 'metabolism "G2   stomach =" stomach-cont
												 "| condition =" condn "| mass =" mass
												 "|=| cost = " cost)

										))
								
								(kdebug 'metabolism "X stomach =" stomach-cont
										 "| condition ="
										 condn "| mass =" mass "|=| cost = " cost)

								(set-my! 'condition condn)
								(set-my! 'stomach-contents stomach-cont)
								(set-my! 'mass mass)
								(set-my! 'structural-mass struct-mass)
								dt
								)
							 'remove)
							 )

(model-method (<metabolism> <number> <number>) (eat self available-food dt)
				  (if mass
						(let* ((struct-mass (my 'structural-mass))
								 (mass (my 'mass))
								 (guts (* struct-mass (my 'gut-size)))
								 (stomach-cont (my 'stomach-contents))
								 (stuffed-to-the-gills (* mass
																  (my 'max-consumption-rate)
																  dt))
								 (gap (- guts stomach-cont))
								 (max-dinner (min gap
														stuffed-to-the-gills
														available-food))
								 (ate 0.0)
								 )
						  ;; do calculations here....
						  
						  (begin ; eating
							 (set! ate max-dinner)
							 ;;(set! available-food (- available-food ate))
							 (set! stomach-cont (max 0.0 (+ stomach-cont ate)))
							 )

						  ;; return value 
						  (if (< (/ mass struct-mass) (my 'starvation-level))
								#f ;; false means death
								(let ()
								  ;;(if (> (* mass struct-prop) struct-mass) 
								  ;;   (set-my! self 'struct-mass struct-mass)
								  ;;   )
								  (set-my! 'stomach-contents stomach-cont)
								  ate
								  )
								)
						  )
						0)
				  )


;---- animal methods

;----- (self-assessment)
"This is a placeholder ... it almost certainly needs to change"
(model-method <simple-animal> (representation-assessment self . args) 
				  (if (null? args) ;; only passed self
						(let ((rgr (/ (my 'mass) (* (my 'age))))
								(ngr (my 'nominal-growth-rate))
								)
						  (if (< rgr (* ngr 2)) 0 (- (/ rgr ngr) 2)) ;; steps over at (> rgr (* 3 ngr))
						  )
						(let ((n (car args)) ;; mixing assumption 
								)
						  (+ (/ n (my 'population-switch)))
						  ))
				  )




;----- (age) 
(model-method <simple-animal> (age self)
				  (slot-ref self 'age))


;----- (set-age!) 
(model-method (<simple-animal> <number>) (set-age! self n)
				  (if (not (number? n))
						(aborts "thing:set-age! -- bad number")
						(slot-set! self 'age n)))


;----- (sex) 
(model-method <simple-animal> (sex self)
				  (slot-ref self 'sex))


;----- (set-sex!) 
(model-method (<simple-animal> <symbol>) (set-sex! self n)
					(if (not (member n '(female male)))
						 (aborts "thing:set-sex! -- symbol should be male or female")
						 (slot-set! self 'sex n)))

;----- (map-log-track-segment
(model-method <simple-animal> (map-log-track-segment self track wt P ps)
				  (let ((p (lambda (x) (P (local->model self x)))))
					 (if track
						(let* ((xytrack (map txyz->xy track))
								 (ptrack (map p xytrack)))
						  (if (>= (length ptrack) 2)
								(let ((startseg (list-head ptrack
																	(1- (length ptrack))))
										(finishseg (cdr ptrack)))
								  (if adjust-grey (ps 'setgray wt))
								  
								  (for-each
									(lambda (ss fs)
									  (ps 'moveto ss)
									  (ps 'moveto ss)
									  (ps 'lineto fs)
									  (ps 'stroke)
									  )
									startseg finishseg)
								  (ps 'Helvetica 4.5)
								  (ps 'moveto (p (list-head (location self) 2)))

								  (let ((m (my 'mass)))
									 (if m
										  (begin
											 (ps 'show-centered (number->string (my 'mass)))
											 (ps-circle ps
															(p (min 0.31415
																	  (* 0.25 pi
																		  (sqrt (my 'mass)))))
															(p (list-head (location self) 2))
															1.2 0.0 ))
										  (begin
											 (ps 'show-centered "Dead")
											 (ps-circle ps  (p 0.31415)
															(p (list-head (location self) 2))
															1.2 0.0 ))))
										  
								  )

								)
						  )
						)
				  #t))


(model-method (<simple-animal> <log-introspection> <symbol>) (map-log-data self logger format targets)
				  (error "Get the projections right here")
	  (if (emit-and-record-if-absent logger self (my 'subjective-time))
 			  (let ((ps (slot-ref logger 'file))
						  (p (get-model->local logger)))
					 (if (or (not p) (null? p))  (set! p (lambda (x) x)))

					 (let ((track (my 'track))
							 (tracks (my 'tracked-paths)))

						(if track (map-log-track-segment self track ACIRCGREY p ps))
						(if tracks
							 (let loop ((n (length tracks))
											(k 1.0)
											(tr tracks))
								(if (not (null? tr))
									 (begin
										(map-log-track-segment
										 self (car tr)
										 (+ AMINGREY (* (/ k n) (- AMAXGREY AMINGREY)))
										 p ps)
										(loop n (1+ k) (cdr tr))))))
						)
					 #t)
					 'boink?
				  ))
				  
(model-method (<simple-animal> <log-introspection> <symbol>) (log-data self logger format  targets)
				  (error "Get the projections right here")
				  (if (emit-and-record-if-absent logger self (my 'subjective-time))
						(let ((file (slot-ref logger 'file))
								(p (get-model->local logger)))
						  (if (or (not p) (null? p))  (set! p (lambda (x) x)))

						  (kdebug '(log-* log-animal) ":" targets)
						  (case format
							 ((ps)
							  (map-log-data self logger format  targets p file)
							  )
							 (else 
							  (log-data-parent))
							 )

						  (if (and (assoc 'track-segments
												(my 'state-flags))
									  (state-flag self 'track-segments))
								(new-track! self))
						  
						  )
						)
				  )

(model-method (<simple-animal> <number> <pair> <number> <number>)
				  (wander-around self dt point attr speed) ;;
				  (let* ((loc (location self))
							(p (unit (vector-to loc point)))
							(v (slot-ref self 'direction))
							(theta (- (nrandom pi 10) pi))
							(spd speed)
							)
					 
					 ;; This calculation ought to use random-angle and rotated-velocity
					 ;; (which applies to lists!)  I'll keep it this way for local
					 ;; clarity.

					 (let* ((new-v (unit
										 (append (list (- (* (car v) (cos theta))
																(* (cadr v) (sin theta)))
															(+ (* (cadr v) (cos theta))
																(* (car v) (sin theta))))
													(cddr v))))

							  (new-loc
								(map + loc 
									  (map *  
											 (map +
													(map * (make-list (length v) (- 1.0 attr)) new-v)
													(map * (make-list (length p) attr) p))
											 (append (make-list 2 (* spd dt))
														(make-list (length (cddr v)) 1.0)))))
							  )
						(slot-set! self 'location new-loc)
						(slot-set! self 'direction new-v)
						new-loc))  )
	 

(model-method (<simple-animal> <number> <pair> <number> <symbol>)
				  (wander-around self dt point attr speedtag) ;;
				  (wander-around self dt point attr (my speedtag)))

(model-body <simple-animal>
				;; Need to flesh this out
				(call-next-parent-body)
				dt)


(define (animal-initfunc self)
  				 (slot-set! self 'current-interest 
								(lambda args 
								  (aborts
									(string-append
									 "current-interest isn't defined for a "
									 "member of the <animal> class: ")
									(slot-ref self 'name) ":"
									(slot-ref self 'type) ":"
									(slot-ref self 'representation))))
				 (set-state-variables self (list 'age #f 'sex #f))
				 ;; call "parents" last to make the initialisation list work
				 (set-state-variables self args)
				 )


(model-body <simple-animal>
				(kdebug '(model-bodies animal-running)  (class-name-of self) (name self) "@" t "/" dt)
				(let ((dt/2 (/ dt 2.0))
						(SQRT (lambda (x) (if (>= x 0) (sqrt x) 
													 (if #t
														  (begin 
															 (kdebug 'math-error "SQRT got a value of " x)
															 0)
														  (abort "I see ... a rhinoceros ...")
														  )) ) )
						)
				  (kdebug 'animal-running "[" (my 'name)
							":" (class-name-of self) "]"
							" at " t "+" dt)
				  (kdebug "******" 'running (my 'name)
							" the " (my 'representation)
							" is running" "******")
				  (set-my! 'age (+ (my 'age) dt/2))
				  
				  (call-next-parent-body)
				  ;; should execute body code for <metabolism> and <thing>

				  (let* ((domain (kquery 'containing-agents (my 'location)))
							(foodlist (my 'foodlist))
							(homelist (my 'homelist))
							(breedlist (my 'breedlist))
							(H (my 'habitat))
							(here (my 'location))
							(food 0.0) ;; nothing unless we find some
							(struct-mass (my 'structural-mass))
							(mass (my 'mass))
							(starve-level (my 'starvation-level))
							(stomach-cont (my 'stomach-contents))
							(guts (* struct-mass (my 'gut-size)))
							(condition (my 'condition))
							(ate 0.0)
							(objective (let ((o (my 'objective))) (if (null? o) #f o)))
							(initial-age (my 'age))
							(initial-subjective-time (my 'subjective-time))
							)

					 (kdebug 'stomach "[" (my 'name) ":" (class-name-of self)
							  "]" "===> stomach =" stomach-cont "| condition ="
							  condition "| mass =" mass)

					 (if (number? mass) 
						  (begin
							 (let ((focus ((my 'current-interest) self
												(my 'age) t dt condition stomach-cont guts))
									 )
								(kdebug 'focus "[" (my 'name) ":"
										 (class-name-of self) "]" " in now focussed on " focus)

								(cond
								 ((not focus)
								  (wander-around self dt (map (lambda (x) (/ x 2.0)) domain)
													  (* 2.0 (my 'domain-attraction)) 'wanderspeed)
								  #t)
								 ;;(aborts "Dinna y' ken?"))
								 ((eq? focus 'wander)
								  (wander-around self dt (map (lambda (x) (/ x 2.0)) domain)
													  (my 'domain-attraction) 'wanderspeed)
								  )

								 ((eq? focus 'hungry)
								  (kdebug 'debugging-eating "[" (my 'name)
											":" (class-name-of self) "]" "hungry")

								  (let* ((foodsites
											 (patch-list H (lambda (x)
																  (let ((s (services x foodlist)))
																	 (and s (not (null? s)))))))
											(foodvalue
											 (map 
											  (lambda (x) 
												 (if (= food-attenuation 0.5)
													  (/ (SQRT (value x foodlist))
														  (+ (spatial-scale H)
															  (distance here (location x))))
													  (/ (pow (value x foodlist) food-attenuation)
														  (+ (spatial-scale H)
															  (distance here (location x)))))
												 ) foodsites))

											(fooddata (sort (map cons foodvalue foodsites)
																 (lambda (x y) (> (car x) (car y)))))
											)
									 (kdebug "Food ranks" "[" (my 'name) ":"
											  (class-name-of self) "]"
											  (map (lambda (x)
														(cons (car x)
																(distance here
																			 (location (cdr x)))))
													 fooddata))


									 (if (zero? (length fooddata))
										  (begin ;; No food possible ... 
											 (kdebug 'debugging-eating "[" (my 'name)
													  ":" (class-name-of self) "]" "Hunting")
											 (wander-around self dt
																 (map (lambda (x) (/ x 2.0)) domain)
																 (my 'domain-attraction) 'foragespeed)
											 )
										  (let ((target (cdar fooddata)))
											 (if (contains? target (location self))
												  (begin
													 (kdebug 'debugging-eating "[" (my 'name) ":"
															  (class-name-of self) "]"
															  "Reading the menu")
													 (let* ((TV (value target foodlist))
															  (ate #f))
														(kdebug 'debugging-eating "[" (my 'name) ":"
																 (class-name-of self) "]"
																 "ordering the lot ("
																 (value target foodlist) "/"
																 (value target (services target))
																 ")and eating it")
														(let* ((total-food (value target foodlist))
																 (prop (/ total-food
																			 (capacity target foodlist)))
																 (available-food
																  (* 2.0 total-food
																	  (/ (SQRT prop) (1+ prop)))))
														  (set! ate (eat self available-food dt))
														  )
														(kdebug 'debugging-eating "[" (my 'name) ":"
																 (class-name-of self) "]" "removing "
																 ate " from the patch ...")
														(scale! target foodlist (- 1.0 (/ ate TV)))
														(kdebug 'debugging-eating "[" (my 'name) ":"
																 (class-name-of self) "]" "done.")
														)
													 (wander-around self dt (location target)
																		 (my 'near-food-attraction)
																		 'foragespeed)
													 )
												  (begin
													 (kdebug 'debugging-eating "[" (my 'name) ":"
															  (class-name-of self) "]"
															  "move toward the food source")
													 (wander-around self dt (location target)
																		 (my 'food-attraction)
																		 'movementspeed)
													 )))
										  )
									 )
								  )
								 ((eq? focus 'flee)
								  ;; Not implemented yet
								  #f
								  )
								 ((eq? focus 'breed)
								  ;; Not implemented yet
								  #f
								  )
								 )
								)

							 (set-my! 'objective (if (null? objective) #f objective))
							 (set-my! 'age (+ (my 'age) dt/2))
							 
							 ;;(track-location self t (my 'location)) ;;
							 ;;even if they aren't moving ::
							 ;;automatically done in <thing>
							 )
						  )

					 ;; We do it this way to avoid (as much as possible) adding extra error in the times
					 (set-my! 'age (+ initial-age dt))
					 
					 ;; Ok now test condition and return
					 (if (or (not mass) (< (my 'mass) (* 1.3 (my 'structural-mass))))
						  (list 'remove self)
						  dt)
					 )
				  )
				)

(definition-comment " So we have essentially three types of animal:
adult herbivores that eat the the plants, juveniles that eat the
fruit (and deposit viable seeds), and the carnivores that eat only
juvenile herbivores.

Animals work in the following way:

They have an amount of time before they need to eat (Sated)
They will forage as soon as that time has elapsed, and will forage
	until they have eaten a particular proportion of their mass, then 
	they become sated again
They record the amount of time they have been hungry, and maintain 
	a moving window average of lengths of hunger.
They will avoid changing cells unless there is either crowding or
	they have been hungry too often.
Animals grow while they are sated, and cease to grow when hungry. If
	they are hungry for too long, they die.


Herbivores and carnivores are essentially the same beasts, code wise, 
with the following differences:
	Juvenile carnivores have a very long period when they are sated
		(they eat something that is abundant, bugs?)
   Adult carnivores hunt juvenile herbivores
   Juvenile herbivores hunt fruit
   Adult herbivores hunt plants.


****************************************************************
**********         Oversight in journal paper         **********
****************************************************************

*** NOTE: Juvenile herbivores need to have a lag between eating 
*** fruit and leaving seeds, otherwise we cannot have 
*** recolonisation by plants in a cell that has become denuded.
")


(model-method <example-animal> (die self)
				  ;;"routine corresponding to the death of an animal (calls shutdown)")
				  #t
				  )
(model-method <example-animal> (prey-present self)
				  ;;"returns a list of the animal's prey in a suitable locality")
				  #t
				  )
(model-method <example-animal> (growth self dt)
				  ;;"does the business of growing")
				  #t
				  )
(model-method <example-animal> (eat self prey domain) ;; we specify both the prey agent and the domain which contains the agent
				  ;;"ingestion....")
				  (if (isa? prey <example-plant>)
						(begin
						  'ok)
						(begin
						  'also-ok
						  ))
				  )
(model-method <example-animal> (forage self )
				  ;;"move about looking for a region with food")o
				  #t
				  )
;(model-method (<example-animal>) (crowded? self )
;				  ;;"determine if the region is too crowded for the animal")
;				  (let ((N (kernel 'members 
;				  )
(model-method <example-animal> (migrate self )
				  ;;"move to another region (change cells)")
				  #t
				  )
(model-method <example-animal> (reproduce self )
				  ;;"create offspring")
				  #t
				  )
				  

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"

;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
