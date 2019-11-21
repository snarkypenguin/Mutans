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


;-  Discussion 

;-  Configuration stuff 

;-  Included files 

(include "remodel")

;-  Variables/constants both public and static

;--    Static data

;-  Variables/constants both public and static

(define AMINGREY 0.2)
(define AMAXGREY 0.05)
(define ACIRCGREY 0.0)

;(require 'charplot)

;-  Code 

;;(define food-attenuation 0.25)
(define food-attenuation 0.5)
;;(define food-attenuation (/ 2.0 (exp 1.0))) ;; ~1.3


;----- initialise

;; (model-method <simple-metabolism> (initialise-instance self args)
;; 				  (set-state-variables self (list 'hunger-limit 20.0 'period-of-hunger 0.0))
;; 				  (initialise-parent)
;; 				  ;; call "parents" last to make the initialisation list work
;; 				  (set-state-variables self args)
;; 				  )


(model-method <simple-metabolism> (initialise-instance self)
				  (parent-initialise-instance)

				  (fail-on-uninitialised-slots self
														 '(mass decay-rate hunger-limit max-satiety
																  satiety-rate sated-quantity food-satiety-rate)))



(model-body% <simple-metabolism>
				  ;;	(no-parent-body)
				  (if (<= (my 'counter) 0) ;; counter is initialised in (create ...) and updated in (run-agent ...)
						'ok
						)

				"
satiety -- how many points we currently have
max-satiety is how much an animal can have onboard
sated-quantity is how much an animal can have before feeling full/comfortable
satiety-rate is how many units are used up each day (	how fast we lose our satiety)
food-satiety-rate is how many satiety points a mass of generic food is worth
"
				(if (not (dead? self))
					 (begin
						(set-my! 'last-reproduced (- t (* (random-real) (my 'reproduction-period))))

						(if (>= (my 'period-of-hunger) (my 'hunger-limit))
							 (begin
								(die self)
								(set-agent-state! self 'dead)
								(dnl* (name self) "starved" (agent-state self))
								'ok)
							 
							 (let ((satiety (- (my 'satiety) (my 'satiety-rate)))
									 (sated-quantity (my 'sated-quantity))
									 )
								(set-my! 'satiety satiety)
								;; this doesn't differentiate between the amount of food animals
								;; of differing sizes require.

								(cond
								 ((>= satiety sated-quantity)  ;; if we are sated, increment sated-time by dt, and grow a bit
								  (set-mass! self  (+ (my 'mass) (nominal-growth-rate self dt)))
								  (set-my! 'sated-time (+ (my 'sated-time) dt)))
								 ((> satiety 0) (set-my! 'sated-time 0))
								 ((<= satiety 0) (set-my! 'sated-time (- (my 'sated-time) dt)))
								 (#t (error "bad satiety"))
								 )
								'ok)
							 ))
					 'ok)
					 )



(model-method% <simple-metabolism> (eat self dt mymass locus foodmass foodrate #!optional update)
				  "The idea is there is an amount of food which is equivalent to foodmass*foodrate points.
   There is max-satiety-satiety space, so..."

				  (if (not (dead? self))
						(let* ((foodrate
								  (cond 
									((procedure? foodrate) (foodrate mymass))
									((number? foodrate) foodrate)
									((symbol? foodrate) 
									 (let ((fr (slot-ref self foodrate)))
										(cond ((procedure? fr)  (* (fr mymass) foodmass))
												((number? fr) (* fr foodmass))
												(#t (error "Bad food conversion rate specified indirectly")))))
									(error "Bad food conversion rate specified"))
								  )
								 (stomach-space (- (my 'max-satiety) (my 'satiety)))
								 (can-consume (max 0 (min  (+ (/ stomach-space foodrate) (- (/ (* (my 'satiety-rate) dt) day) (my 'satiety))) foodmass))) 
								 )
						  (set-my! 'satiety (+ (my 'satiety) (* foodmass foodrate)))
						  (if (> (my 'satiety) (my 'sated-quantity))
								(set-my! 'period-of-hunger 0)
								(set-my! 'period-of-hunger (+ (my 'period-of-hunger) dt)))
						  ;;(dnl* (name self) (agent-state self) "might eat" can-consume "at" (location self))
								
						  (if update
								(begin
(timing-block 'gobbles?
								  (update can-consume)
)
								  #t)
								can-consume)
						  )
						#f)
				  )

;---- animal methods

(model-method <simple-animal> (initialise-instance self)
				  (call-all-parents)
				  (fail-on-uninitialised-slots self
														 '(foodlist age-at-instantiation 
																		search-radius eat-radius endurance recovery-time
																		predator-bias prey-bias longevity
																		))

				  )


(model-method <simple-animal> (agent-prep self start end)
				  (call-parents <simple-metabolism> <living-thing>)

				  (set-uninitialised-slots self '(distance-cost) 0.001) 
				  (set-uninitialised-slots self '(chase-duration elapsed-recovery-required) 0)
				  (set-uninitialised-slots self '(homelist activity-dt current-interest breedlist domain-attraction) '())

				  (initialise-instance self)

				  (let ((ma (my 'max-age))
						  (age (my 'age))
						  (L (my 'longevity))
						  )
					 (if (< ma age) (set-my 'max-age (+ age (* 0.1 (random-real) L) (* -0.05 (random-real) L))))
					 )
				  #t
				  )



(model-method <animal> (initialise-instance self)
				  (parent-initialise-instance)

				  (fail-on-uninitialised-slots
					self '(hunt-speed search-speed forage-speed wander-speed)
					)
				  )

(model-method <animal> (agent-prep self start end)
				  (parent-agent-prep)

				  (set-uninitialised-slots self '(movement-speed) 0) 
				  (initialise-instance self)
				  #t
				  )


(model-method <example-animal> (initialise-instance self)
				  (parent-initialise-instance)
				  (fail-on-uninitialised-slots
					self '(prey-list omega-ind adult-diet-mass food-density-limit conspecific-density-limit
										  crowded-level)
					)
				  )

(model-method <example-animal> (agent-prep self start end)
				  (parent-agent-prep)

				  (set-uninitialised-slots self '(peak-mass last-reproduced) 0)
				  (set-uninitialised-slots self '(prey-list offspring) '())
				  

				  (initialise-instance self)
				  #t
				  )

(model-method <jherb> (initialise-instance self)
				  (parent-initialise)
				  (fail-on-uninitialised-slots
					self '(tree-satiety-rate fruit-satiety-rate)
					)
				  )

(model-method <jherb> (agent-prep self start end)
				  (parent-agent-prep)

				  (set-uninitialised-slots self '(peak-mass last-reproduced) 0)
				  (set-uninitialised-slots self '(prey-list) '())
				  (set-uninitialised-slots self '(seed-lag-list*) '(0 0 0 0 0 0))
				  (initialise-instance self)
				  #t
				  )

(model-method <aherb> (initialise-instance self)
				  (parent-initialise-instance)
				  (fail-on-uninitialised-slots self '(tree-satiety-rate)
														 )
				  )

(model-method <aherb> (agent-prep self start end)
				  (parent-agent-prep)

				  (set-uninitialised-slots self '(peak-mass last-reproduced) 0)
				  (set-uninitialised-slots self '(prey-list offspring) '())
				  (initialise-instance self)
				  #t
				  )

(model-method <acarn> (initialise-instance self)
				  (parent-initialise-instance)
				  (fail-on-uninitialised-slots self '(tree-satiety-rate)
														 )
				  )

(model-method <acarn> (agent-prep self start end)
				  (parent-agent-prep)

				  (set-uninitialised-slots self '(peak-mass last-reproduced) 0)
				  (set-uninitialised-slots self '(prey-list) (list <jherb>))
				  (set-uninitialised-slots self '(offspring) '())
				  (initialise-instance self)
				  #t
				  )


(model-method <simple-animal> (modal-dt self #!optional prospective-dt)
				  (let ((current-interests (my 'current-interest))
						  (cidt (my 'activity-dt))
						  (dt (my 'dt)))
					 (if (symbol? current-interests) (set! current-interests (list current-interests)))
					 
					 (cond
					  ((pair? current-interests)
						(apply min (cons dt (map cdr (filter number? (map (lambda (j) (assoc j cidt)) current-interests))))))
					  (prospective-dt prospective-dt)
					  (else dt))))


(model-method <simple-animal> (maximum-speed self)
				  (let ((er (my 'elapsed-recovery-required))
						  (ms (my 'max-speed)))
					 (if (positive? er)
						  (* (- 1 (/ er (my 'recovery-time))) ms)
						  ms)))



;----- (age) 
(model-method <simple-animal> (age self)
				  (slot-ref self 'age))


;----- (set-age!) 
(model-method (<simple-animal> <number>) (set-age! self n)
				  (if (or (not (number? n)) (negative? n))
						(aborts "animal:set-age! -- bad number")
						(slot-set! self 'age  n)))


;----- (sex) 
(model-method <simple-animal> (sex self)
				  (slot-ref self 'sex))


;----- (set-sex!) 
(model-method (<simple-animal> <symbol>) (set-sex! self n)
				  (slot-set! self 'sex n))

;----- (log-track-segment
(model-method <simple-animal> (log-track-segment self track prj ps #!rest args)
				  (let ((map-color (my 'map-color))
						  (default-color (my 'default-color)))
					 ;;				  (dnl* "In log-track-segment" (cnc self) (name self) track)
					 (ps 'comment (string-append "segment of animal track: " (name self)))
					 (ps 'comment (apply string-append (map object->string (list 'map-color " "  map-color ", " 'default-color " " default-color))))
					 (cond
					  ((and map-color (not (uninitialised? map-color)))
						(ps 'push-color (my-map-color self))
						)
					  
					  ((and default-color (not (uninitialised? default-color)))
						(ps 'push-color default-color))
					  
					  (else (ps 'push-color 0))
					  )
					 (set-my! 'track-datum (my 'mass))
					 (call-parent-methods (list <tracked-agent>) log-track-segment self prj ps)

					 (ps 'pop-color)
					 ))


(model-method (<simple-animal> <number> <point> <number> <number>) ;; point must already be in the agent's model space
				  (wander-around self dt point attr speed)
				  ;; This is not ideal, but will have to do.
				  (let ((vector-to-target  (map - point (my 'location)))
						  (displacement (stoch-walk (location self) point dt (my 'ndt) attr speed))
						  )

					 (set-my! 'direction (normalise displacement))
					 (set-my! 'location (map + (my 'location) displacement))
					 ))

(model-method (<simple-animal> <number> <point> <number> <symbol>) ;; point must already be in the agent's model space
				  (wander-around self dt point attr speedtag) ;;
				  (wander-around self dt point attr (my speedtag)))

(model-body% <simple-animal>
				 ;;(call-parents <living-thing>  <simple-metabolism>)
				 ;;(parent-model-body)
				 ;; Need to flesh this out
				 ;;				  (dnl* (cnc self) "model-body")
				 (let ((parent-returns (call-parents <living-thing> <simple-metabolism>)))
					(let ((satiety (my 'satiety))
							(sated-quantity (my 'sated-quantity))
							(max-satiety (my 'max-satiety))
							)	
					  (timing-block simple-animal-modal-dt
										 (set! dt (modal-dt self dt)))

					  ;; body for juvenile herbivores
					  
					  (cond
						((or
						  (member (my 'queue-state) '(terminated))
						  (member (my 'agent-state) '(terminated)))
						 (terminate self)
						 (list 'remove))
						((list? parent-returns)
						 (cons dt (!filter void? (!filter null? parent-returns))))
						(#t dt)
						))))

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
They will avoid changing domains unless there is either crowding or
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
*** recolonisation by plants in a domain that has become denuded.
")

(model-method <example-animal> (growth self t dt)
				  ;;"does the business of growing")
				  (void)
				  )

;; This is here, because we want a much simpler approach to metabolism.
;; Animal code will come here rather than percolate back to the more elaborate
;; routine, since this is comes first in the class precedence list (CPL)

(model-method <example-animal> (forage self )
				  (look-for* self (my 'foodlist) (my 'search-radius)))


;(model-method (<example-animal>) (crowded? self )
;				  ;;"determine if the region is too crowded for the animal")
;				  (let ((N (kernel 'members 
;				  )
(model-method <example-animal> (migrate self )
				  ;;"move to another region (change domain)")
				  (void)
				  )
(model-method <example-animal> (reproduce self )
				  ;;"create offspring")
				  '()
				  )

(model-method <example-animal> (initialise-instance self)
				  (parent-initialise-instance)
				  
				  ;; zero the following if they aren't set
				  (set-uninitialised-slots self '(period-of-hunger satiety sated-time peak-mass adult-diet-mass) 0)
				  (fail-on-uninitialised-slots self
														 '(omega-ind hunger-limit 
																		 crowded-level migrate-param max-satiety 
																		 satiety-rate sated-quantity
																		 ))
				  (let ((age (my 'age))
						  (maa (my 'mass-at-age))
						  (mass (my 'mass))
						  (max-age (my 'max-age))
						  (L (my 'longevity))
						  )

					 (if (uninitialised? (my 'max-age)) (set-my! 'max-age +inf.0))
					 (if (<= max-age age) (set-my! 'max-age (/ (+ age L) 2)))

					 (cond
					  ((not (and  mass (number? mass)))
						(set-my! 'mass (maa age))
						'set )
					  ((> (abs (- mass (maa age))) (* 0.2 mass))
;;						(dnl* "**** Resetting mass for" (taxon self) (name self) "from" mass "to" (maa age))
						(set-my! 'mass (maa age))
						'reset)
					  (else 'ok))

	;;; (if (uninitialised? (my 'mass-at-age))
	;;; 	 (error "the mass-at-age function must be defined in a parameter file, or in the create call for ") (cnc self) (taxon self))
					 
	;;; (if (uninitialised? (my 'longevity))
	;;; 	 (error "Need to define a longevity parameter for" (cnc self) (taxon self)))
					 
	;;; ;;(dnl* (my 'name) (my 'age) (my 'mass))
	;;; (set-my! 'mass ((slot-ref self 'mass-at-age) (slot-ref self 'age)))

					 (if (and (not (list? (my 'foodlist))))
						  (set-my! 'foodlist (list (my 'foodlist))))
					 )
				  )

;; This routine brings a consumer to within the 'eat-radius of the prey
;; It does *not* select prey; if target-location is specified, it must already be in the agent's coord system
(model-method (<example-animal> <thing>) (hunt self dt target #!optional target-location) 
				  (if (not target-location) (set! target-location (get-location self target)))
				  (let* ((location (my 'location))
							(D (distance location target-location))
							(H (if (has-slot? self 'hunt-speed) (my 'hunt-speed) +nan.0)) ;; pursuit
							;;(S (if (has-slot? self 'search-speed) (my 'search-speed) +nan.0)) ;;  when !foraging (moving to places)
							(F (if (has-slot? self 'forage-speed) (my 'forage-speed) +nan.0)) ;; when foraging (ambush)
							(W (if (has-slot? self 'wander-speed) (my 'wander-speed) +nan.0))
							(max-speed (maximum-speed self))
							(V (apply max (!filter nan? (list H F W))))
							(dv (/ D dt))
							(foodrate 1)
							)
					 (if (>  dv  V)
						  (set! dv V))
					 ;; We *will* move in the right direction
					 (set-direction! (normalise (map - target location))) 
					 (set-speed! V)
					 (set-my! 'movement-speed V)
					 ;;HERE!!!
					 (wander-around self dt target-location 0.85 dv) ;; the 0.9 gives us overshoot and cornering error
					 
					 (cond
					  ((and (<= (distance (location self) (location target)) (my 'capture-radius))
							  (< (random-real) (my 'predator-bias))
							  (> (random-real) (slot-ref target 'prey-bias)))
						;; we are close enough and luck is with us!
						(die target) ;; kill the target
						(set-my! 'current-interest (cons 'eat (cons 'rest (!filter (lambda (x) (member x '(hunt eat)) (my 'current-interest))))))
						(set-my! elapsed-recovery-required (* (/ (my 'chase-duration) (my 'endurance)) (my 'recovery-time)))
						(set-my! 'chase-duration 0) ;; the chase is over
						)
					  ((>= (my 'chase-duration) (my 'endurance)) ;; Exhausted
						(set-my! 'current-interest (cons 'rest (!filter (lambda (x) (member x '(hunt eat)) (my 'current-interest)))))
						(set-my! elapsed-recovery-required (my 'recovery-time))
						(set-my! 'chase-duration 0) ;; the chase is over
						)
					  (else ;; continue hunt....
						(set-my! 'chase-duration (my 'chase-duration dt))
						(if (>= (my 'chase-duration) (my 'endurance)) ;; Exhausted
							 (begin ;; do not hunt next tick
								(set-my! 'current-interest (cons 'rest (!filter (lambda (x) (member x '(hunt eat)) (my 'current-interest)))))
								(set-my! elapsed-recovery-required (my 'recovery-time))
								(set-my! 'chase-duration 0) ;; the chase is over
								)
							 )))
					 )
				  )

(model-method <example-animal> (rest-recover-check self)
				  #f)


(model-method <example-animal> (manage-offspring-check self)
				  #f)

(model-method <example-animal> (assess-local-environment self)
				  (let ((e (look-for self (my 'environmental-threats) (* 2 (my 'search-radius))))
						  (p (look-for self (my 'predatorlist)))
						  (b (look-for self (my 'breedlist) (* 1.5 (my 'search-radius)))) ;; Mates are easier to find than predators
						  )
					 #f))
(model-body% <example-animal>
;;  (dnl* (cnc self) "model-body")
  ;; Assess interests first - - - - - - - - - - - - - - - - - - - - - - -
  
    (let ((major-interest #f))
		(if (and (not (member (car (my 'current-interest)) '(flee combat mate)))
					(> t (my 'next-env-check)))
			 (set major-interest (assess-local-environment self)))
		;; water availability, fire, flood, cover from predators,
		;; food density, conspecific density, predator density, 

		(let ((envthreats (look-for self (my 'environmental-threats) (* 2 (my 'search-radius)))) ;; these are easier to perceive
				(predthreats (look-for self (my 'predatorlist)))
				(mates (if #f (look-for self (my 'breedlist) (* 1.5 (my 'search-radius))) '())) ;; Mates are easier to find than predators
		  ;;; (mates (if #f (look-for self (my 'breedlist)) '()))
		  ;;; (food (forage self)) ;; looks for food
				)

		  (if #f (rest-recover-check self))
		  (if #f (manage-offspring-check self))
		  
		  ;; Self assessment of the conditions:
		  ;; environmental threats are highest
		  ;; then predator threats
		  ;; then mating
		  ;; manage-offspring (if appropriate)
		  ;; then rest/recovery
		  ;; then food
		  
		  ;;###################################################
		  ;;## HERE
		  ;;###################################################
		  
		  ;;
		  ;; NOTES: This does not implement a notion of a home range.  It would
		  ;; be relatively simple to add and a radius of +inf.0 would indicate no
		  ;; range
		  ;;

		  ;; Adjust dt in <simple-animal> and then act on most pressing interest.
		  (let ((parent-returns (call-parents <simple-animal>)))
			 (if (not (number? (my 'mass)) )
				  (set-my! 'mass (mass-at-age self)))

			 (if (> (my 'mass) (my 'peak-mass)) (set-my! 'peak-mass (my 'mass)))

			 (if (dead? self)
				  (begin
					 ;;				(dnl* (cnc self) (name self) "is decaying...." (my 'mass))
					 (set-mass! self (* (my 'mass) (exp (* (my 'decay-rate) dt))))
					 (if (< (my 'mass) (* 0.2 (my 'peak-mass)))
						  (begin
							 (terminate self)
							 (list 'remove self))
						  'ok))
				  (begin ;; I'm not dead yet.....
					 ;; set current interest first

					 ;; mostly not implemented

					 (cond
					  (#f #f );; check for environmental threats
					  (#f #f );; check for predatory threats
					  (#f #f) ;; check for mating conditions

					  (#f #f) ;; check for rest

					  ;; Yup, foodtime 
					  ((and (not (positive? (my 'satiety))) (not (null? (forage self))))
						(set-my 'current-interest (cons 'forage (my 'current-interest)))
						)

					  (#f #f) ;; check for sleep
					  )

					 
					 (let* ((max-speed (maximum-speed self))
							  (interest (my 'current-interest)) ;; this can be a prioritised list
							  (main-interest (cond
													((pair? interest) (car interest))
													((null? interest) 'wander)
													(else interest))))

						(if (and (eq? main-interest 'hunting) (<= (min (cons +inf.0 (map (lambda (x) (distance (my 'location) (get-location self x))) (my 'prey-list))))
																				(my 'eat-radius)))
							 (set! main-interest 'eating))

						(case main-interest
						  ('flee
							;; flight is simpler than hunting
							(let ((point (map (lambda (x) (* max-speed dt x))
													(map - (my 'location) (if (agent? (my 'objective))
																					  (get-location self (my 'objective))
																					  (my 'objective)))))
									)
							  (wander-about self dt point 0.8 max-speed) ;; Without inertia, this may be a bit wonky.
							  )
							)
						  ('forage ;; this is where they look for food
							(let ((F (if (null? (my 'prey-list)) (forage self) (my 'prey-list)))) ;; sets prey-list
							  (set-my! 'prey-list F)
							  (cond
								((not (null? F))
								 (set-my! 'main-interest 'hunting)
								 (set-my! 'objective (car F))
								 (wander-about self (* 0.8 dt) (get-location self (car F)) 0.8 hunt-speed) ;; Without inertia, this may be a bit wonky.
								 )
								(else
								 (wander-about self dt (random-point (my 'environment)) 0.6 hunt-speed) ;; Without inertia, this may be a bit wonky.
								 ))
							  ))
						  ('hunting
							(hunt self dt (my 'objective)))

						  ('eating
							(eat self dt (my 'mass) (my 'location) (slot-ref target 'mass) foodrate
								  (lambda (mss)
									 ;;(dnl* "got" ate "fruit")
									 (alter target self 'adjust# 'mass (- mss))
									 ))
							)
						  ('wander
							(wander-around self dt '(0 0 0) 0 (my 'wander-speed))) ;; random walk.

						  ('seek-mate
							#f)
						  ('mate
							(set-my 'speed  0))
						  ('seek-shelter
							#f)
						  ('fight
							#f)
						  ('tend-offspring
							(set-my 'speed  0))
						  ('rest
							(set-my 'speed  0))
						  ('sleep
							(set-my 'speed  0)#t)
						  )
						(let ((rectime (my 'elapsed-recovery-required))
								)
						  
						  (if (null? (list-intersection interest '(hunting fight flee)))
								(if (positive? rectime)
									 (set-my! 'elapsed-recovery-required (max 0 (- rectime dt)))))
						  )	 
						) ;; end of interest-ing bit
					 ))
		
			 (cond
			  ((pair? (my 'offspring)) ;; Must return offspring to the kernel
				(list 'introduce (my 'offspring)) )
			  ((member (my 'agent-state) '(dead)) ;; bodies hang around: set decay-rate to +inf.0 for immediate eviction
				'ok)
			  ((or (member (my 'queue-state) '(terminated))
					 (member (my 'agent-state) '(terminated)))
				(begin (terminate self) (list 'remove self)))
			  ((not (or (number? parent-returns)  (eq? parent-returns 'ok)))
				(cons dt (!filter void? (!filter null? parent-returns))))
			  (#t
				'ok)
			  )
			 )
		  )))



;--- Juv. Herbivore methods ------------------------------------------------------------


(model-method <jherb> (initialise-instance self)
	(parent-initialise-instance)
	;; zero the following if they aren't set
	(set-uninitialised-slots self '() 0)
	(fail-on-uninitialised-slots self
										  '(tree-satiety-rate fruit-satiety-rate foodlist))
	
	(set-my! 'seedcount 0) ;;(numeric-parameter-lookup <example-plant> tree-taxon 'seeds-per-fruit))
	(set-my! 'seed-lag-list* (make-list 4 0)))
				  

;; Recall from the parameter file that the functions and parameters associated with growth
;; are likely to be inconsistent and are certainly a good start for a bad joke.

(model-body% <jherb>
	 ;; jherb does not chain back to the example animal, because it relies on
	 ;; essentially ubiquitous fruit.  Also, we hypothesise that the juveniles
	 ;; cannot effectively escape the carnivores.
	 (let ((parent-returns (call-parents <example-animal>)))
		(if (not (number? (my 'mass)) )
			 (set-my! 'mass (mass-at-age self)))

		(if (> (my 'mass) (my 'peak-mass)) (set-my! 'peak-mass (my 'mass)))

		(if (dead? self)
			 (begin
				;;				(dnl* (cnc self) (name self) "is decaying...." (my 'mass))
				(set-mass! self (* (my 'mass) (exp (* (my 'decay-rate) dt))))
				(if (< (my 'mass) (* 0.2 (my 'peak-mass)))
					 (begin
						(terminate self)
						(list 'remove self))
					 'ok)
				)
			 (begin ;; I'm not dead yet.....
				;; set current interest first
				(cond
				 ((dead? self)
				  'ok)
				 ((and (< (my 'age) (my 'max-age)) (not (member (my 'agent-state) '(dead terminated))))
				  (let ((age (my 'age))
						  (mass (my 'mass))
						  )
					 ;; body for juvenile herbivores
					 ;; Handle transition to include adult diet
					 (if (and (>= (my 'mass) (my 'adult-diet-mass)) (not (member <plant> (my 'foodlist))))
						  (set-my! 'foodlist (cons <example-plant> (my 'foodlist))))
					 
; Fruit is essentially uniformly distributed in a cell.  Nobody knows why, but there are
; dark murmurings in the back of the pub ...					 
					 (let* ((domain (my 'domain))
							  (fruitcount (value domain 'fruit))
							  (fruit-density (/ fruitcount (area domain)))
							  )
						
						;; Eat...
						(for-each
						 (lambda (type)
							;;(dnl* "Processing" type)
							(cond 
							 ((eq? type 'fruit)
							  ;;(dnl* "Fruity!")
							  (let ((tryfor
										(eat self dt mass (my 'location)
											  (value domain 'fruit)
											  (my 'fruit-satiety-rate)
											  (lambda (ate)
												 (let* ((seedcount (my 'seedcount))
														  )
													;;(dnl* "got" ate "fruit")
													(add! domain 'fruit (- ate))
													(set-my! 'seed-lag-list* (append (my 'seed-lag-list*) (list (* ate seedcount))))
													)))
										)
									  )
								 (add! domain 'seeds (car (my 'seed-lag-list*)))
								 (set-my! 'seed-lag-list* (cdr (my 'seed-lag-list*))))
							  )
							 ((member type (list <plant> <example-plant> "B.exemplarii"))
							  ;;					(dnl* "Want to eat trees")
							  #f
							  )
							 (else (error "bad mojo"))
							 )
							)
						 (force-list (my 'foodlist)))

						(if (or (> (my 'period-of-hunger) (* 1 day)) (< fruit-density (my 'food-density-limit)))
							 (let* ((loc (location self))
									  (wt (lambda (x) (/ (value x 'fruit) (+ 1 (distance loc (get-location self x)))))) ; move
									   ;;(target (let look ((p patchlist) (b (wt (my 'domain))) (cp (my 'domain)))
										;;				 (if (null? p)
										;;					  cp
										;;					  (let ((nb (wt (car p))))
										;;						 (look (cdr p) (if (> nb b) nb b) (if (> nb b) (car p) cp))))))
									  (target (let look ((p patchlist) (b (wt (my 'domain))) (cp (my 'domain)))
													(if (null? p)
														 cp
														 (let ((nb (wt (car p))))
															(look (cdr p) (if (> nb b) nb b) (if (> nb b) (car p) cp))))))
									  )
								(if #t
									 (dnl* "jherb: hungry" (my 'period-of-hunger) "fruit-density" fruit-density "/" (my 'food-density-limit)))
									
								(dnl* (name self) (location self) (location target) (get-location self target))

								(wander-around self dt ((composite-prj_src->dst self target)(random-point target)) 0.8 (my 'forage-speed))
								))

						;; This may get bumped by short timesteps, .... 
						;; if there is fruit, eat fruit until either it is gone or we are maximally sated
						;; record how much fruit was eaten and adjust satiety and fruit

			 ;;; (if (and (or (member <plant> (my 'foodlist))
			 ;;; 				  (member <example-plant> (my 'foodlist)))
			 ;;; 			 (< satiety max-satiety) (> mass (my 'adult-diet-mass)))
						
			 ;;; 	  (forage self)

						;; if there are trees, eat until either they are gone or we are maximally sated
						;; record how much was eaten and adjust satiety and adjust the damage counters for trees
						)
                )
				  dt
				  )

				 ;; *****************************************************************************************
				 ;; This is an example of preserving state information for transfer to a new representation
				 ;; [EXAMPLE of state preservation for new rep'n]
				 ;; 
				 ((and (not (member (agent-state self) '(dead terminated))) (>= (+ (my 'age) dt) (my 'max-age)))
				  (let ((slotset (filter (lambda (x)
													(member (car x)
															  '(name peak-mass
																		sex age mass period-of-hunger location speed direction
																		track tracked-paths 
																		subjective-time state-flags dont-log subsidiary-agents
																		maintenance-list projection-assoc-list))
													) (all-slots self))))
					 ;;			(dnl* "HERB GRADUATION!")
					 (list 'migrate (apply create (append (list <aherb> "adult-herbivore") slotset))))
				  )
				 (#t dt)
				 )
				)
			 ))
	 )



;--- Adult Herbivore methods ------------------------------------------------------------


(model-method <aherb> (initialise-instance self)
	(parent-initialise-instance)
			 ;; zero the following if they aren't set
			 (set-uninitialised-slots self '() 0)
			 (fail-on-uninitialised-slots self
													'(food-satiety-rate foodlist))
)

;; Recall from the parameter file that the functions and parameters associated with growth
;; are likely to be inconsistent and are certainly a good start for a bad joke.

(model-body% <aherb>
   (let ((parent-returns (call-parents <example-animal>)))
	 (cond ;; work out what to send back....
	  ((or (member (my 'agent-state) '(terminated))
			 (member (my 'queue-state) '(terminated)))
		(list 'remove))

	  ((>= (+ (my 'age) dt) (my 'longevity))
		(die self)
;;		(dnl* (name self) "died of old age")
		(terminate self)
		(list 'remove)
		)	

;;	  ;; CHECK PARENT RETURNS HERE 
;;	  ((list? parent-returns)
;;		parent-returns)
	  ((and (>= (my 'age) (my 'reproduction-age))
			  (>= (my 'mass) (my 'reproduction-mass))
			  (>= (my 'last-reproduced) (my 'reproduction-period))
			  (member (my 'sex) '(female reproducing reproducing-female ) 
			  (< (random-real) (my 'reproduction-prob))))
;;		(dnl* "HERB REPRODUCTION!")
		(set-my! 'last-reproduced 0)
		(let* ((n (max 1 (real->integer (nrnd (my 'reproduction-ct)))))
				 (offspring '(make-jh-offspring n (my 'location) (my 'domain))) ;; returns a list
				 )
		  ;; generate offspring 
		  (list 'introduce offspring) ;; the list is of the offspring
		  ))
	  (#t dt)
	  )
	 )
	 )
  

;--- Carnivore methods -------------------------------------------------------------------

;; (model-method <acarn> (reproduce self)
;;   (let ((st (my 'subjective-time))
;; 		  (mass (my 'mass))
;; 		  (sex (my 'sex))
;; 		  (age (my 'age))
;; 		  (domain (my 'domain))
;; 		  )
;; 	 (if (and (> age (my 'reproduction-age))
;; 				 (> mass (my 'reproduction-mass))
;; 				 (< (random-real) (my 'reproduction-prob))
;; 				 (member (my 'sex) '(reproducing female))
;; 		  (begin
;; 			 (let ((males (kernel 'location  (my 'location) (lambda (x) (and (isa? x <acarn>) (eq? (slot-ref x 'sex) 'male))) (my 'search-radius)))
;; 					 )
;; 				(if (pair? males)
;; 					 (set-my! offspring (create <acarn> (my 'taxon)
;;                                            'age (my 'age-at-instantiation)
;; 														 'mass (magnitude ((my 'mass-at-age) 0))
;; 														 'sex (if (odd? (random-integer 3)) 'male 'female)
;; 														 'domain (my 'domain)
;; 														 )
;; 				)

(model-method <acarn> (initialise-instance self)
				  (dnl* "NO EATING IN CARNIVORES")

	(parent-initialise-instance)
			 (set-uninitialised-slots self '() 0)
			 (fail-on-uninitialised-slots self '(food-satiety-rate foodlist))
)					  
			
(model-body% <acarn>
  (let ((parent-returns (call-parents <example-animal>)))
;;	 (dnl* (cnc self) "model-body")
	 ;;; (if (<= (my 'counter) 0) ;; counter is initialised in (create ...) and updated in (run-agent ...)
	 ;;; 	  (begin
	 ;;; 		 ))
	 
	 (if (dead? self)
		  'ok
		  (begin
			 (let ((age (my 'age))
					 (mass (my 'mass))
					 (satiety (my 'satiety))
					 (sated-quantity (my 'sated-quantity))
					 (max-satiety (my 'max-satiety))
					 (domain (my 'domain))
					 )
				;; body for ib-carnivores

				(if (>= (my 'period-of-hunger) (my 'hunger-limit))
					 (begin
						(die self)
						(dnl* (name self) "starved"))
					 (let* ((offspring (reproduce self)) ;; the reproduce call checks to see if it really ought to reproduce, and generates offspring if necessary
							  )
						(set! satiety (- satiety (my 'satiety-rate))) ;; our assumption is that a big herbivore eats as much as a little one (proportionally).
						)
					 ))
			 

			 (cond ;; work out what to send back....
			  ((or (member (my 'agent-state) '(terminated))
					 (member (my 'queue-state) '(terminated)))
				(list 'remove))

			  ((>= (+ (my 'age) dt) (my 'longevity))
				(die self)
				(dnl* (name self) "died of old age")
				(terminate self)
				(list 'remove)
				)
			  ((and (>= (my 'age) (my 'reproduction-age))
					  (>= (my 'mass) (my 'reproduction-mass))
					  (>= (my 'last-reproduced) (my 'reproduction-period))
					  (member (my 'sex) '(reproducing female reproducing-female))
					  (< (random-real) (my 'reproduction-prob)))
				(dnl* "CARN REPRODUCTION!")
				(set-my! 'last-reproduced 0)
				(let* ((n (max 1 (real->integer (nrnd (my 'reproduction-ct)))))
						 (offspring '(make-carnivores (my 'location) (my 'domain))) ;; returns a list
						 )
				  ;; generate offspring 
				  (list 'introduce offspring) ;; the list is of the offspring
				  )
				)
			  ;; CHECK PARENT RETURNS HERE 
			  ((list? parent-returns)
				parent-returns)
			  (#t dt)))
			 )
	 )
  )


(model-method <animal-array> (initialise-instance self)
  (parent-initialise-instance)

  (slot-set! self 'data-names
				 '(state mass age location domain last-reproduced
							current-interest prey-list
							)
				 )
  (if (uninitialised? (slot-ref self 'test-subject)) (slot-set! self 'test-subject (create <example-animal> (my 'taxon))))
  )

;-  The End 

;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
