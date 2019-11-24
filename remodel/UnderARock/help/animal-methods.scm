;-  Identification and Changes

;--
;	animal.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.19
;		Location: odin:/home/gray/study/src/new/animal.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2012 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

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


;----- initialize

(model-method <simple-metabolism> (initialize self args)
				  (initialise self (list 'hunger-limit 20.0 'days-of-hunger 0.0))
				  (initialize-parent)
				  ;; call "parents" last to make the initialisation list work
				  (initialise self args)
				  )



(model-body <metabolism> ;; A thing with metabolism *must* have mass
						(kdnl* 'model-bodies "In " (class-name-of self))
						(parent-body)

						(if (not (number? (my 'mass)))
							 (kdnl* 'stomach (my 'name) "is dead, long live the King"))

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

								(kdnl* 'stomach "Metabolism: cost =" cost
										 "stomach-contents =" stomach-cont)

								(if cost 
									 (begin ;; deal with metabolic costs

										(kdnl* 'metabolism "D0   stomach ="
												 stomach-cont "| condition =" condn
												 "| mass =" mass "|=| cost = " cost)

										;; .... with stomach-contents
										(let ((dc (- stomach-cont cost)))
										  (set! stomach-cont (max dc 0.0))
										  (set! cost (max (- dc) 0.0))
										  )

										(kdnl* 'metabolism "D1   stomach =" stomach-cont
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
										
										(kdnl* 'metabolism "D2   stomach =" stomach-cont
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
										
										
										(kdnl* 'metabolism "D3   stomach =" stomach-cont
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

										(kdnl* 'metabolism "G0   stomach =" stomach-cont
												 "| condition =" condn "| mass =" mass
												 "|=| cost = " cost)

										;; growth first
										(let* ((mm (/ stomach-cont food->mass-conv-rate))
												 (mg (* mgr dt))
												 (delta-m (min mm mg))
												 )
										  (kdnl* 'metabolism "g0a          mm =" mm
													" | mg =" mg " | delta-m =" delta-m)
										  
										  (set! mass (max 0.0 (+ mass delta-m)))
										  (set! stomach-cont
												  (max 0.0
														 (- stomach-cont
															 (* delta-m food->mass-conv-rate))))
										  (kdnl* 'metabolism "g0b          mm ="
													mm" | mg =" mg " | delta-m =" delta-m)
										  )

										(kdnl* 'metabolism "G1   stomach =" stomach-cont
												 "| condition =" condn "| mass =" mass
												 "|=| cost = " cost)

										;; now condition
										(let* ((mm (/ stomach-cont food->cond-conv-rate))
												 (mg (* cgr dt))
												 (delta-m (min mm mg))
												 )
										  
										  (kdnl* 'metabolism "g1a          mm =" mm
													" | mg =" mg " | delta-m =" delta-m)
										  (set! condn (max 0.0 (+ condn delta-m)))
										  (set! stomach-cont
												  (max 0.0
														 (- stomach-cont
															 (* delta-m
																 food->cond-conv-rate))))
										  (kdnl* 'metabolism "g1b          mm ="
													mm" | mg =" mg " | delta-m =" delta-m)
										  )

										(kdnl* 'metabolism "G2   stomach =" stomach-cont
												 "| condition =" condn "| mass =" mass
												 "|=| cost = " cost)

										))
								
								(kdnl* 'metabolism "X stomach =" stomach-cont
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

;----- (initialize) 
(add-method initialize
				(make-method (list <animal>)
								 (lambda (initialize-parent self args)
									;;(dnl "<animal> init")
									(slot-set! self 'current-interest 
												  (lambda args 
													 (aborts
													  (string-append
														"current-interest isn't "
														"defined for a <animal>: ")
													  (slot-ref self 'name) ":"
													  (slot-ref self 'type) ":"
													  (slot-ref self 'representation))))
									(initialise self (list 'age #f 'sex #f))
									(initialize-parent)
									;; call "parents" last to make the initialisation list work
									(initialise self args)
									)))

;----- (age) 
(add-method age
				(make-method 
				 (list <animal>)
				 (lambda (call-parent-method self)
					(slot-ref self 'age))))


;----- (set-age!) 
(add-method set-age!
				(make-method 
				 (list <animal> <number>)
				 (lambda (call-parent-method self n)
					(if (not (number? n))
						 (aborts "thing:set-age! -- bad number")
						 (slot-set! self 'age n)))))


;----- (sex) 
(add-method sex
				(make-method 
				 (list <animal>)
				 (lambda (call-parent-method self)
					(slot-ref self 'sex))))


;----- (set-sex!) 
(add-method set-sex!
				(make-method 
				 (list <animal> <symbol>)
				 (lambda (call-parent-method self n)
					(if (not (member n '(female male)))
						 (aborts "thing:set-sex! -- symbol should be male or female")
						 (slot-set! self 'sex n)))))

;----- (map-log-track-segment
(model-method <animal> (map-log-track-segment self track wt p ps)
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


(model-method <animal> (map-log-data self logger format caller targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'map-projection)))
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
				  )
				  
(model-method <animal> (log-data self logger format caller targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'map-projection)))
					 (if (or (not p) (null? p))  (set! p (lambda (x) x)))

					 (kdnl* '(log-* log-animal) ":" targets)
					 (case format
						((ps)
						 (map-log-data self logger format caller targets p ps)
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


(model-method (<animal> <number> <pair> <number> <number>)
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
		new-loc)))
	 

(model-method (<animal> <number> <pair> <number> <symbol>)
				  (wander-around self dt point attr speedtag) ;;
				  (wander-around self dt point attr (my speedtag)))

(model-body <animal>
	(kdnl* 'model-bodies "In " (class-name-of self) t)
	(let ((dt/2 (/ dt 2.0))
			(SQRT (lambda (x) (if (>= x 0) (sqrt x) 
										 (if #t
											  (begin 
												 (dnl "SQRT got a value of " x)
												 0)
											  (abort "I see ... a rhinoceros ...")
											  )) ) )
			;;(kdnl* dnl*)
			;;(kdnl* dnl)
			)
	  (kdnl* 'animal-running "[" (my 'name)
				":" (class-name-of self) "]"
				" at " t "+" dt)
	  (kdnl* "******" 'running (my 'name)
				" the " (my 'representation)
				" is running" "******")
	  (set-my! 'age (+ (my 'age) dt/2))
	  (set-my! 'subjective-time (+ (my 'subjective-time) dt/2))

	  (parent-body)
	  ;; should execute body code for <metabolism> and <thing>

	  (let* ((foodlist (my 'foodlist))
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
				)

		 (kdnl* 'stomach "[" (my 'name) ":" (class-name-of self)
				  "]" "===> stomach =" stomach-cont "| condition ="
				  condition "| mass =" mass)
		 (if (number? mass) 
			  (begin
				 (let ((focus ((my 'current-interest) self
									(my 'age) t dt condition stomach-cont guts))
						 )
					(kdnl* 'focus "[" (my 'name) ":"
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
					  (kdnl* 'debugging-eating "[" (my 'name)
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
						 (kdnl* "Food ranks" "[" (my 'name) ":"
								  (class-name-of self) "]"
								  (map (lambda (x)
											(cons (car x)
													(distance here
																 (location (cdr x)))))
										 fooddata))


						 (if (zero? (length fooddata))
							  (begin ;; No food possible ... 
								 (kdnl* 'debugging-eating "[" (my 'name)
										  ":" (class-name-of self) "]" "Hunting")
								 (wander-around self dt
													 (map (lambda (x) (/ x 2.0)) domain)
													 (my 'domain-attraction) 'foragespeed)
								 )
							  (let ((target (cdar fooddata)))
								 (if (contains? target (location self))
									  (begin
										 (kdnl* 'debugging-eating "[" (my 'name) ":"
												  (class-name-of self) "]"
												  "Reading the menu")
										 (let* ((TV (value target foodlist))
												  (ate #f))
											(kdnl* 'debugging-eating "[" (my 'name) ":"
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
											(kdnl* 'debugging-eating "[" (my 'name) ":"
													 (class-name-of self) "]" "removing "
													 ate " from the patch ...")
											(scale! target foodlist (- 1.0 (/ ate TV)))
											(kdnl* 'debugging-eating "[" (my 'name) ":"
													 (class-name-of self) "]" "done.")
											)
										 (wander-around self dt (location target)
															 (my 'near-food-attraction)
															 'foragespeed)
										 )
									  (begin
										 (kdnl* 'debugging-eating "[" (my 'name) ":"
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
				 (set-my! 'subjective-time (+ (my 'subjective-time) dt/2))
				 
				 ;;(track-locus self t (my 'location)) ;;
				 ;;even if they aren't moving ::
				 ;;automatically done in <thing>
				 )
			  )
		 
		 ;; Ok now test condition and return
		 (if (or (not mass) (< (my 'mass) (* 1.3 (my 'structural-mass))))
			  (list 'remove self)
			  dt)
		 )
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
