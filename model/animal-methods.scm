; -*- mode: scheme; -*-
(include "framework")

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

;;(define food-attenuation 0.25)
(define food-attenuation 0.5)
;;(define food-attenuation (/ 2.0 (exp 1.0))) ;; ~1.3


;----- initialise

;; (model-method <simple-metabolism> (initialise self args)
;; 				  (set-state-variables self (list 'hunger-limit 20.0 'period-of-hunger 0.0))
;; 				  (initialise-parent)
;; 				  ;; call "parents" last to make the initialisation list work
;; 				  (set-state-variables self args)
;; 				  )

(define (nominal-growth-rate self)
  (let ((m@a (slot-ref self 'mass-at-age))
		  (age (slot-ref self 'age)))
	 (/ (- (m@a (+ age week)) (m@a age)) week)))

(model-method <simple-metabolism> (initialisation-checks self)
 		   (parent-initialisation-checks)
			;; zero the following if they aren't set
			(set-uninitialised-slots self 
											 '(period-of-hunger satiety sated-time)
											 0
											 )

			(fail-on-uninitialised-slots self
												  '(hunger-limit 
													 max-satiety 
													 satiety-rate sated-quantity
													 food-satiety-rate)))

(model-body <simple-metabolism>
;;	(no-parent-body)
	(if (<= (my 'counter) 0) ;; counter is initialised in (create ...) and updated in (run-agent ...)
		 (void))

"
satiety -- how many points we currently have
max-satiety is how much an animal can have onboard
sated-quantity is how much an animal can have before feeling full/comfortable
satiety-rate is how many units are used up each day (	how fast we lose our satiety)
food-satiety-rate is how many satiety points a mass of generic food is worth
"

	(set-my! 'last-reproduced (- t (* (random-real) (my 'reproduction-period))))

	(if (>= (my 'period-of-hunger) (my 'hunger-limit))
		 (begin
			(die self)
			(dnl* (name self) "starved"))
		 (let ((satiety (- (my 'satiety) (my 'satiety-rate)))
				 (sated-quantity (my 'sated-quantity))
				 )
			(set-my! 'satiety satiety)
			;; this doesn't differentiate between the amount of food animals
			;; of differing sizes require.

			(cond
			 ((>= satiety sated-quantity)  ;; if we are sated, increment sated-time by dt, and grow a bit
			  (set-mass! self  (+ (my 'mass) (* (nominal-growth-rate self) dt)))
			  (set-my! 'sated-time (+ (my 'sated-time) dt)))
			 ((> satiety 0) (set-my! 'sated-time 0))
			 ((<= satiety 0) (set-my! 'sated-time (- (my 'sated-time) dt)))
			 (#t (error "bad satiety"))
			 )
			))
	)
		 


(model-method <simple-metabolism> (eat self dt mymass foodmass foodrate #!optional update)
   "The idea is there is an amount of food which is equivalent to foodmass*foodrate points.
   There is max-satiety-satiety space, so..."

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
			 (can-consume (max 0 (min (/ stomach-space foodrate) foodmass)))
			 )
	  (set-my! 'satiety (+ (my 'satiety) (* foodmass foodrate)))
	  (if (> (my 'satiety) (my 'sated-quantity))
	 		(set-my! 'period-of-hunger 0)
	 		(set-my! 'period-of-hunger (+ (my 'period-of-hunger) dt)))

	  (if update
			(begin
			  (update can-consume)
			  #t)
			can-consume)))


;---- animal methods


(model-method <simple-animal> (initialisation-checks self)
				  (parent-initialisation-checks)
				  (set-uninitialised-slots self '(nominal-growth-rate) 0)
				  (fail-on-uninitialised-slots self
														 '(foodlist age-at-instantiation nominal-growth-rate
																		))

				  (let ((ma (my 'max-age))
						  (age (my 'age))
						  (L (my 'longevity))
						  )
					 (if (< ma age) (set-my 'max-age (+ age (* 0.1 (random-real) L) (* -0.05 (random-real) L))))
					 )
				  )





(model-method <animal> (initialisation-checks self)
				  (set-uninitialised-slots self '(nominal-growth-rate age-at-instantiation) 0)
				  (fail-on-uninitialised-slots self
														 '(foodlist nominal-growth-rate
																		)))

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
				  (if (or (not (number? n)) (negative? n))
						(aborts "animal:set-age! -- bad number")
						(slot-set! self 'age  n)))


;----- (sex) 
(model-method <simple-animal> (sex self)
				  (slot-ref self 'sex))


;----- (set-sex!) 
(model-method (<simple-animal> <symbol>) (set-sex! self n)
						 (slot-set! self 'sex n))

;----- (map-log-track-segment
(model-method <simple-animal> (map-log-track-segment self track wt p ps)
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
				  #t)


(model-method (<simple-animal> <log-introspection> <symbol>) (log-data self logger format  targets)
				  (if (or (my 'always-log) (emit-and-record-if-absent logger self (my 'subjective-time)))
						(let ((file (slot-ref logger 'file))
								)
						  (kdebug '(log-* log-animal) ":" targets)
						  (case format
							 ((ps)
							  (let ((track (my 'track))
									  (tracks (my 'tracked-paths))
									  (prj (composite-prj_src->dst self logger))
									  )
								 (file 'comment (string-append "tracking " (name self) " at " (number->string (subjective-time self))))
								 (if track (map-log-track-segment self track ACIRCGREY prj file))
								 (if tracks
									  (let loop ((n (length tracks))
													 (k 1.0)
													 (tr tracks))
										 (if (not (null? tr))
											  (begin
												 (map-log-track-segment
												  self (car tr)
												  (+ AMINGREY (* (/ k n) (- AMAXGREY AMINGREY)))
												  prj file)
												 (loop n (1+ k) (cdr tr))))))
								 ))
							 (else 
							  (parent-log-data))
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
							(v (direction self))
							(theta (- (* (nrnd) pi) pi)) ; [0, 2pi]
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
						(set-location! self new-loc)
						(set-direction! self new-v)
						new-loc))  )
	 

(model-method (<simple-animal> <number> <pair> <number> <symbol>)
				  (wander-around self dt point attr speedtag) ;;
				  (wander-around self dt point attr (my speedtag)))

(model-body <simple-animal>
	;; Need to flesh this out
   (let ((parent-returns (call-parents)))
	  (let ((satiety (my 'satiety))
			  (sated-quantity (my 'sated-quantity))
			  (max-satiety (my 'max-satiety))
			  )	
		 ;; body for juvenile herbivores

		 

		 (cond
		  ((member (my 'agent-state) '(terminated))
			(list 'remove dt))
		  ((list? parent-returns)
					parent-returns)
		  (#t dt)))))

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
				  ;;"move about looking for a region with food")
				  (let* ((here (my 'location))
							(results
							 (sort  (kernel 'locate (apply *provides-*? (my 'foodlist)) here (my 'search-radius))
									  (lambda (x y) (<= (distance (location x) here) (distance (location y) here)))))
							 
						  )

					 (set-my! 'prey-list results)
				  ;; Sort in order of priority, stash list in the in 'prey-list, since 'foodlist
				  ;; is categorical rather than specific agents
					 
					 results
				  ))

;(model-method (<example-animal>) (crowded? self )
;				  ;;"determine if the region is too crowded for the animal")
;				  (let ((N (kernel 'members 
 ;				  )
(model-method <example-animal> (migrate self )
				  ;;"move to another region (change domain/habitat)")
				  (void)
				  )
(model-method <example-animal> (reproduce self )
				  ;;"create offspring")
				  '()
				  )

(model-method <example-animal> (initialisation-checks self)
				  (parent-initialisation-checks)
				  
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
						(dnl* "**** Resetting mass for" (taxon self) (name self) "from" mass "to" (maa age))
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


(model-body <example-animal>
  ;;; (if (<= (my 'counter) 0) ;; counter is initialised in (create ...) and updated in (run-agent ...)
  ;;; 		(begin
  ;;; 		  ))

  (call-parents)

  (cond
	((pair? (my 'offspring)) ;; Must return offspring to the kernel
	 (list 'introduce-new-agents dt (my 'offspring)) )
	((member (my 'agent-state) '(terminated))
	 (list 'remove dt))
	((member (my 'agent-state) '(dead)) ;; bodies hang around: set decay-rate to +inf.0 for immediate eviction
	 (set-mass! self (* (my 'mass) (exp (* (my 'decay-rate) dt))))
	 (if (< (my 'mass) (* 0.2 (my 'peak-mass)))
		  (list 'remove dt)
		  dt))
	(#t
	 dt)
	)
  dt
  )
  






;--- Juv. Herbivore methods ------------------------------------------------------------


(model-method <jherb> (initialisation-checks self)
	(parent-initialisation-checks)
	;; zero the following if they aren't set
	(set-uninitialised-slots self '() 0)
	(fail-on-uninitialised-slots self
										  '(tree-satiety-rate fruit-satiety-rate foodlist))
	
	(set-my! 'seedcount (numeric-parameter-lookup <example-plant> tree-taxon 'seeds-per-fruit))
	(set-my! 'seed-lag-list* (make-list 4 0)))
				  

;; Recall from the parameter file that the functions and parameters associated with growth
;; are likely to be inconsistent and are certainly a good start for a bad joke.

(model-body <jherb>
  (let ((parent-returns (call-parents)))
	 ;;; (if (<= (my 'counter) 0) ;; counter is initialised in (create ...) and updated in (run-agent ...)
	 ;;; 	  (begin
	 ;;; 		 ))
	 
	 (cond
	  ((list? parent-returns) ;; a parent class has indicated something significant
		parent-returns)
	  ((and (< (my 'age) (my 'max-age)) (not (member (my 'agent-state) '(dead terminated))))
		(let ((age (my 'age))
				(mass (my 'mass))
				)
		  ;; body for juvenile herbivores
		  ;; Handle transition to include adult diet
		  (if (and (>= (my 'mass) (my 'adult-diet-mass)) (not (member <plant> (my 'foodlist))))
				(set-my! 'foodlist (cons <example-plant> (my 'foodlist))))
		  
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
					(let* ((tryfor
							  (eat self dt mass
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
					  (set-my! 'seed-lag-list* (cdr (my 'seed-lag-list*)))))
				  ((member type (list <plant> <example-plant> "B.exemplarii"))
					(dnl* "Want to eat trees"))
				  (else (error "bad mojo"))
				  ))
			  (force-list (my 'foodlist)))


			 (if (or (> (my 'period-of-hunger) 1) (< fruit-density (my 'food-density-limit)))
				  (let* ((loc (location self))
							(wt (lambda (x) (/ (value x 'fruit) (+ 1 (distance loc (location x)))))) ; move
							(patch-priority
							 (sort (list-copy patchlist)
									 (lambda (p q)
										(<= (wt p) (wt q)))))
							)
						(wander-around self dt (location (car patch-priority)) 0.9 (my 'foragespeed))
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
		  )

	  ;; *****************************************************************************************
	  ;; This is an example of preserving state information for transfer to a new representation
	  ;; 
		((and (not (member (agent-state self) '(dead terminated))) (>= (+ (my 'age) dt) (my 'max-age)))
		 (let ((slotset (filter (lambda (x)
										  (member (car x)
													 '(name peak-mass
															  sex age mass period-of-hunger location speed direction
															  track tracked-paths track-schedule track-epsilon
															  subjective-time state-flags dont-log subsidiary-agents
															  maintenance-list projection-assoc-list))
										  ) (all-slots self))))
			(dnl* "HERB GRADUATION!")
			(list 'migrate (apply create (append (list <aherb> "adult-herbivore") slotset)))
			))
		 (#t dt))
		)
	  )



;--- Adult Herbivore methods ------------------------------------------------------------


(model-method <aherb> (initialisation-checks self)
	(parent-initialisation-checks)
			 ;; zero the following if they aren't set
			 (set-uninitialised-slots self '() 0)
			 (fail-on-uninitialised-slots self
													'(food-satiety-rate foodlist))
)

;; Recall from the parameter file that the functions and parameters associated with growth
;; are likely to be inconsistent and are certainly a good start for a bad joke.

(model-body <aherb>
  (let ((parent-returns (call-parents)))
;	 (if (<= (my 'counter) 0) ;; counter is initialised in (create ...) and updated in (run-agent ...)
;		  (begin
;			 ))
	 
	 (cond ;; body for adult herbivores
	  ((eqv? (agent-state self) 'dead)
		dt)
	  ((not (eqv? (agent-state self) 'terminated))
		(let ((age (my 'age))
				(mass (my 'mass))
				)
		  ;; (dnl* "Just looking, officer. Moving on....")
		  (let ((flist (forage self)))
			 ;;(dnl* "kernel reports " (map name flist))
			 (if (pair? flist)
				  (let* ((here (location self))
							(nearby (filter (lambda (p) (<= (distance here (location p)) (my 'eat-radius))) flist))
							)
					 (if (null? nearby)
						  (let* ((target (location (car flist)))
									(D (distance target here))
									)
							 ;;(set-my! direction (map - (location (car nearby)) here))

							 (if (<= D  (*  (my 'foragespeed) 1-1/e 3/4 dt)) ;;  can't use more than 75% of the time and at 63% efficiency.
								  (set-location! self (location (car flist)))
								  (begin ;; have to walk there
									 (set-direction! self (map - target here))
									 (set-location! self (map + (map (lambda (x) (* x (my 'foragespeed) 1-1/e 3/4 dt)) (my 'direction)) here))))
							 )
						  (let* ((domain (my 'domain))
									(NT (length nearby))
									(mass (my 'mass))
									(treemasses (map leaf-mass nearby))
									(treemass (apply + treemasses))
									(food-satiety-rate (my 'food-satiety-rate))
									)

							 (let* ((tryfor (eat self dt mass treemass food-satiety-rate)))
								(let loop ((munch tryfor)
											  (courses nearby)
											  )
								  (if (pair? courses)
										(let* ((lmass (leaf-mass (car courses)))
												 (k (/ lmass treemass)))
										  (lose-foliage (car courses)  (* munch k lmass))
										  (loop munch (cdr courses)))))
								(set-my! 'period-of-hunger 0))
							 )
						  )
					 )
				  (set-my! 'period-of-hunger (+ (my 'period-of-hunger) 1))
				  
				  
				  )
			 )
		  ))
	  )

	 (cond ;; work out what to send back....
		  ((member (my 'agent-state) '(terminated))
			(list 'remove dt))

		  ((>= (+ (my 'age) dt) (my 'longevity))
			(die self)
			(dnl* (name self) "died of old age")
			(list 'remove dt)
			)	

		  ;; CHECK PARENT RETURNS HERE 
			((list? parent-returns)
					parent-returns)

			((and (>= (my 'age) (my 'reproduction-age))
					(>= (my 'mass) (my 'reproduction-mass))
					(>= (my 'last-reproduced) (my 'reproduction-period))
					(eq? (my 'sex) 'female)
					(< (random-real) (my 'reproduction-prob)))
			 (dnl* "HERB REPRODUCTION!")
			 (set-my! 'last-reproduced 0)
			 (let* ((n (max 1 (real->integer (nrnd (my 'reproduction-ct)))))
					  (offspring '(make-jh-offspring n (my 'location) (my 'domain) (my 'habitat))) ;; returns a list
					  )
					 ;; generate offspring 
				  (list 'introduce-new-agents dt offspring) ;; the list is of the offspring
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
;; 				 (= (my 'sex) 'female))
;; 		  (begin
;; 			 (let ((males (kernel 'location  (my 'location) (lambda (x) (and (isa? x <acarn>) (eq? (slot-ref x 'sex) 'male))) (my 'search-radius)))
;; 					 )
;; 				(if (pair? males)
;; 					 (set-my! offspring (create <acarn> (my 'taxon)
;;                                            'age (my 'age-at-instantiation)
;; 														 'mass (magnitude ((my 'mass-at-age) 0))
;; 														 'sex (if (odd? (random-integer 3)) 'male 'female)
;; 														 'domain (my 'domain)
;; 														 'habitat (my 'domain)
;; 														 )
;; 				)

(model-method <acarn> (initialisation-checks self)
				  (dnl* "NO EATING IN CARNIVORES")

	(parent-initialisation-checks)
			 (set-uninitialised-slots self '() 0)
			 (fail-on-uninitialised-slots self '(food-satiety-rate foodlist))
)					  
			
(model-body <acarn>
  (let ((parent-returns (call-parents) ))
	 ;;; (if (<= (my 'counter) 0) ;; counter is initialised in (create ...) and updated in (run-agent ...)
	 ;;; 	  (begin
	 ;;; 		 ))
	 
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
				
				;; Ecoservice based consumption
				;;; (let* ((food-per-point ((my 'food-satiety-rate) mass))
				;;; 		 (frate ((my 'food-satiety-rate) mass))
				;;; 		 (mighteat (* frate (- max-satiety satiety)))
				;;; 		 (tryfor (if (> food mighteat) (max 0 (min mighteat (- food mighteat))) (if (positive? food) food 0)))
				;;; 		 )
				;;;   (dnl* "Stomach room:" max-satiety satiety)
				;;;   (dnl* "frate = " frate ", and I would like to eat" mighteat "food")
				;;;   (dnl* "but there is " food "so I might eat" tryfor)

				  
				;;;   ;; if we are not maximally sated and we are big enough, find a tree ...
				;;;   ;; (if (and (< satiety max-satiety) (> mass (my 'adult-diet-mass)))
				;;;   ;; 	 (let ((treelist (kernel 'locate (*is-class? <example-plant>) (my 'location)  (my 'search-radius))))
				  
				;;;   ;; 		(dnl* "Still not eating trees")
				;;;   ;; 		;; if there are trees, eat until either they are gone or we are maximally sated
				;;;   ;; 		;; record how much was eaten and adjust satiety and adjust the damage counters for trees
				;;;   ;; 		(void)
				;;;   ;; 		)
				;;;   ;; )
				  
				;;;   (cond
				;;; 	((>= satiety sated-quantity)  ;; if we are sated, increment sated-time by dt, and grow a bit
				;;; 	 (set-my! 'mass  (+ mass (* (growth-rate self) dt)))
				;;; 	 (set-my! 'sated-time (+ (my 'sated-time) dt)))
				;;; 	((> satiety 0) (set-my! 'sated-time 0))
				;;; 	((<= satiety 0) (set-my! 'sated-time (- (my 'sated-time) dt)))
				;;; 	(#t (error "bad satiety"))
				;;; 	)

				;;;   (dnl* "No support for migration")
				;;;   (dnl* "No support for graduation")
				;;;   (dnl* "No carnivore bits")
				;;;   )
				)
			 ))

	 (cond ;; work out what to send back....
		  ((member (my 'agent-state) '(terminated))
			(list 'remove dt))

		  ((>= (+ (my 'age) dt) (my 'longevity))
			(die self)
			(dnl* (name self) "died of old age")
			(list 'remove dt)
			)
		  ;; CHECK PARENT RETURNS HERE 
		  ((list? parent-returns)
					parent-returns)
		  
			((and (>= (my 'age) (my 'reproduction-age))
					(>= (my 'mass) (my 'reproduction-mass))
					(>= (my 'last-reproduced) (my 'reproduction-period))
					(eq? (my 'sex) 'female)
					(< (random-real) (my 'reproduction-prob)))
			 (dnl* "CARN REPRODUCTION!")
			 (set-my! 'last-reproduced 0)
			 (let* ((n (max 1 (real->integer (nrnd (my 'reproduction-ct)))))
					  (offspring '(make-carnivores (my 'location) (my 'domain) (my 'habitat))) ;; returns a list
					  )
					 ;; generate offspring 
				  (list 'introduce-new-agents dt offspring) ;; the list is of the offspring
			 ))
			(#t dt))
		  )
  )


						

;-  The End 

;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
