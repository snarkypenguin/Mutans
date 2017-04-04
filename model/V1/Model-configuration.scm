;-  Identification and Changes

;--
;	model.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2013.02.05
;		Location: odin:/home/gray/study/src/model.scm
;
;-  Code 


;(dump wally)(newline)
;(aborts "Incomplete initialisation is making things fail when it runs")



(define start 0)
;(define end 500)
;(define end 6.1)
(define end 741)


(define A4domain (list 210 294 80)) ;; (x y z) corresponds to the size of an A4 page
(define mA4domain (list 178 250 80))

(define domain mA4domain)

(set! nested-agents '(nested-habitat)) ;; No, each patch does its own thing....

;;(set! kernel-messages (append kernel-messages '(*)))
;;(set! kernel-messages (append kernel-messages '(running  log-*)))
;;(set! kernel-messages (append kernel-messages '(running focus stomach hunger-proximity eating)))
;;(set! kernel-messages (append kernel-messages '(running animal-running focus log)))
;;(set! kernel-messages (append kernel-messages '(running focus stomach hunger-proximity eating)))
;;(set! kernel-messages (append kernel-messages '(animal-running stomach hunger-proximity eating)))
;;(set! kernel-messages (append kernel-messages '(metabolism animal-running stomach hunger-proximity eating)))


;(define landscape
;  (make <landscape> (list 'name "landscape" 'type 'forest-floor 'representation 'plane 'terrain-function (plane '(12 8 4 0)) 'default-value 0 'minv '(0 0 0) 'maxv '(100 100 2000))))



;; Need to implement simple population matrices for patches.  


(define (rerun-agent agent #!optional dt)
  (set! dt (if dt dt 0.1))
  (run-model-body agent (slot-ref agent 'subjective-time) dt))



;; The ecoservlist looks like ((probability name type biomass-meanval) ...)
(define (knock-up-a-habitat namn typ default-ht domain terrain-function npatches ecoservlist)
  (let ((n (seq npatches))
		  )

	 (make <habitat> 'name namn 'type typ 'representation 'accurate 
			 'minv (car domain) 'maxv (cadr domain) 
			 'terrain-function (plane '(8 2 4 0)) 'default-value default-ht 
			 'patch-list (let loop ((nl 100))
								(let ((patches (map (lambda (x) ;; each patch
															 (let ((P (make-patch (apply random-location domain) (nrandom (/ (patchsize domain) npatches)) '())))
																(slot-set! P 'name (string-append namn "-" (pno x)))
																(let ((p (filter (lambda (t) t)
																					  (map (lambda (y) ;; each ecoservice
																								(let ((cap (nrandom (cadddr y))))
																								  (if (> (random-real) (car y))
																										#f
																										(make <ecoservice> 'patch P 
																												'name (cadr y)
																												;;'name (string-append namn "-" (cadr y))
																												'type (caddr y) 
																												'capacity cap
																												'value (min cap (/ (+ (/ cap 2.0) (nrandom (cadddr y))) 2.0))
																												;;'value (min cap (abs (- cap (nrandom (cadddr y)))))
																												'growth-rate 0.05
																												'dt 2
																												'plateau-interval 20))))
																							 ecoservlist)))
																		)
																  
																  (slot-set! P 'service-list p))
																P))
														  n))
										)
								  (let*	 ((N (map (lambda (x) (map name x)) (map service-list patches)))
											  (Nn (map length N))
											  (T (map (lambda (x) (map type x)) (map service-list patches)))
											  (Tn (map length T))
											  )
									 (if (and (> nl 0)
												 #f
											(or (< (apply max Tn) )
												 (< (apply min Tn) 3)
												 (< (apply max Nn) 6)))
										  (loop (1- nl))
										  patches))
								  )
								)
			 
			 )
	 )
)



;; The convention will be that lower case letters will be types, uppercase will be species

;; Basic update function given a growth rate predation vector δp = p (γ - b×A) + e b×p×A - c×p
;;  

(define-macro (update-function var Cap PopGrowth nmort eff ts slist var-adjustments pred-matrix prey-adj pred-adj)
  `(lambda (t ,@slist)
	  (let* (,@var-adjustments)
		 (if (zero? ,var)
			  0.0
			  (let* ((vix (let ((p (member ',var ',slist))) (if p (- (length ',slist) (length p)) #f))))
				 (if (not vix) 
					  (abort 'bad-species-in-update-function)
					  (let ((pred-consts ((,pred-matrix 'row (1+ vix))))
							  (prey-consts ((car (,pred-matrix 'col (1+ vix))))))
						 (+ (* ,var (- ,PopGrowth (apply + (map * pred-consts ,slist))))
							 (- (* ,eff  ,var (apply + (map (lambda (x) (if (< 0 x) 1 0)) (map * prey-consts ,slist))))
								 (apply + (* ,nmort ,var))))))))
		 ) ;; End of outer let*
	  ))




;; This macro make creating the update functions easier.  Ideally it would put checks in for all sorts
;; of silly mistakes.

(define-macro (first-update-function var Cap PopGrowth MaintR timescale 
										 slist 
										 var-adjustments
										 pred-matrix
										 prey-adj 
										 pred-adj 
										 )
  `(lambda (t ,@slist)
	  (let* ( ,@var-adjustments )
		 (if (zero? ,var)
			  0.0
			  (* ,var (/ t ,timescale)

				  (let* ((vix (let ((p (member ',var ,slist))) (if p (- (length ,slist) (length p)) #f))))
					 (if (not vix) 
						  (abort 'bad-species-in-update-function)
						  (let ((pred-consts ((,pred-matrix 'row (1+ vix))))
								  (prey-consts ((car (,pred-matrix 'col (1+ vix))))))
							 (- (* ,PopGrowth  
									 (- 1.0 (/ ,var ,Cap)) 
									 (if (not (zero? (* ,prey-adj ,MaintR ,var)))
										  (- 1.0 (/ (* ,prey-adj (apply + (map * prey-consts ,slist)) (* ,MaintR ,var))))
										  1)
									 (* ,pred-adj (apply + (map * pred-consts ,slist))))))))
				  )
			  )
		 )
	  )
  )

#|

dA = t A{r(1 - A/K)[1 - prey_adj ∑(prey_i prey_rate_i)]/(maint_rate * A) - pred_adj ∑ (pred_i pred_rate_i)}

where A is anything, K is the carrying capacity, r is the natural
growth rate, maint_rate is the amount it needs to consume to maintain
biomass.  It is an error to have a prey rate for things that don't
have maint_rates, and we need to bypass the division by zero in this
case.

|#



(define service-index-list '(grass "hay-crop" tree 
											  rabbit "sheep" "goat" bovine "dairy-cattle" 
											  "pig" "velociraptor"
											  ))
(define service-variables '(G h T R s g b D V p))


;;; (first-update-function var Cap PopGrowth MaintR timescale 
;;; 					  slist 
;;; 					  var-adjustments
;;; 					  prey-adj prey-consts 
;;; 					  pred-adj pred-consts)


;; The the rows refer to the predation by the organism, the columns to the pressure *on* the organism.
;; 




;; "b" in the L-V stuff 
(define predation-matrix
  (make-matrix
	;; Predation by the organism
	;;(G       h     T        R    s     g      b       D      p      V)  
	'((0       0     0        0    0     0      0       0      0      0)          ;; G    P  o  O 
	  (0       0     0        0    0     0      0       0      0      0)          ;; h    r  n  r
	  (0       0     0        0    0     0      0       0      0      0)          ;; T    e     a
	  (.0004  .0005  .0001    0    0     0      0       0      0      0)          ;; R    d  t  n
	  (.0003  .0003  .0001    0    0     0      0       0      0      0)          ;; s    a  h  i
	  (.00021 .00022 .000002  0    0     0      0       0      0      0)          ;; g    t  e  s
	  (.00018 .00014 .00003   0    0     0      0       0      0      0)          ;; b    i     m
	  (.00023 .00025 .00002   0    0     0      0       0      0      0)          ;; D    o      
	  (.00025 .00027 .000007 .0001 .0003 .0002  .00013  .00002 .00005 .00005)     ;; p    n      
	  (0      0      0       .0001 .0003 .0003  .0003   .0003  .0003  .00005))    ;; V    
	))


(define update-equations (list 
;; (update-function var Cap PopGrowth nmort eff ts slist var-adjustments pred-matrix prey-adj pred-adj)
								  (update-function G 8000 0.08 0.01 0.0 1 
														 (G h T R s g b D p V) 
														 ((G (- G h)) (b (- b D)))  ;; treat grass as everything but hay, cattle as everything but dairy cows
														 predation-matrix
														 0 1) ;; predatory pressure on it
								  (update-function h 12000 0.08 0.01 0.0 1
														 (G h T R s g b D p V)
														 ((G (- G h)) (b (- b D)))  ;; treat grass as everything but hay, cattle as everything but dairy cows
														 predation-matrix
														 0 1)

								  (update-function T 200000 0.01 0.05 0.0 1
														 (G h T R s g b D p V)
														 ((G (- G h)) (b (- b D)))  ;; treat grass as everything but hay, cattle as everything but dairy cows
														 predation-matrix
														 0 1)

								  (update-function R 1200 0.4 0.2 0.1 1
														 (G h T R s g b D p V)
														 ((G (- G h)) (b (- b D)))  ;; treat grass as everything but hay, cattle as everything but dairy cows
														 predation-matrix
														 1 1)

								  (update-function s 3000 0.3 0.15 0.25  1
														 (G h T R s g b D p V)
														 ((G (- G h)) (b (- b D)))  ;; treat grass as everything but hay, cattle as everything but dairy cows
														 predation-matrix
														 1 1)

								  (update-function g 2200 0.28 0.13 0.26  1
														 (G h T R s g b D p V)
														 ((G (- G h)) (b (- b D)))  ;; treat grass as everything but hay, cattle as everything but dairy cows
														 predation-matrix
														 1 1)
								  
								  (update-function b 60000 0.32 0.13 0.22 1
														 (G h T R s g b D p V)
														 ((G (- G h)) (b (- b D)))  ;; treat grass as everything but hay, cattle as everything but dairy cows
														 predation-matrix
														 1 1)
								  
								  (update-function D 40000 0.28 0.13 0.18 1
														 (G h T R s g b D p V)
														 ((G (- G h)) (b (- b D)))  ;; treat grass as everything but hay, cattle as everything but dairy cows
														 predation-matrix
														 1 1)

								  (update-function p 1200 0.2 0.2 0.1 1
														 (G h T R s g b D p V)
														 ((G (- G h)) (b (- b D)))  ;; treat grass as everything but hay, cattle as everything but dairy cows
														 predation-matrix
														 1 1)

								  (update-function V 1200 0.3 0.4 0.2  1
														 (G h T R s g b D p V)
														 ((G (- G h)) (b (- b D)))  ;; treat grass as everything but hay, cattle as everything but dairy cows
														 predation-matrix
														 1 1)

								  )
  )




;; we are treating dairy cattle separately since they are intensive


					 
				

;;-?;; The ecoservlist looks like ((probability name type biomass-meanval) ...)
;;-?(define H (knock-up-a-habitat "Test-domain" 'rangeland 0 (list '(0 0 10) domain ) (plane '(10 4 6 0)) 10 
;;-?										'(
;;-?										  (0.025 "myrtle" tree 3000) 
;;-?										  (0.125 "gum" tree 8000)
;;-?										  (0.8 "wattle" tree 2000) 
;;-?										  (0.9 "pasture" grass 1400) 
;;-?										  (0.3 "hay-crop" grass 2000) 
;;-?										  (0.4 "rabbit" rabbit 200) 
;;-?										  (0.5 "sheep" caprid 1000) 
;;-?										  (0.05 "goat" caprid 600) 
;;-?										  (0.25 "dairy-cattle" bovine 2375)
;;-?										  (0.25 "beef-cattle" bovine 3450)
;;-?										  (0.05 "Bos-indicus" bovine 2550)
;;-?										  (0.5 "pig" pig 1) 
;;-?										  (0.05 "Velociraptor" predator 150)
;;-?										  )))

(define H (knock-up-a-habitat "Test-domain" 'rangeland 0 (list '(0 0 10) domain ) (plane '(10 4 6 0)) 10 
										'(
										  (0.925 "vegetation" plant 12000) 
										  (0.525 "pig" herbivore 940)
;;										  (0.1 "velociraptor" predator 250)
)))

;;(define schedtimes 
(define schedtimes (append 
						 (cons 0 (seq 6))
						 (map (lambda (x) (* 10.0 (1+ x))) (seq 360)))
  ) ;; first six days, then on every tenth day from the beginning for 370 days


(define (velociraptor-brain me age t dt condn stconts gutsz)
;  (dnl "focussing with " t "@" dt " c:" condn " s:" stconts" g:" gutsz)
  (cond
	((or (< stconts (* 0.5 gutsz)) (< condn 0.3)) 'hungry)
	(#f 'flee)
	(#f 'breed)
	(#t 'wander)
	(else #f)))

(define wallymass 160.0)
(define wilmamass 180.0)
;;(set! wallymass 98.0)
;;(set! wilmamass 108.0)

(define (condition m) (* 0.2 m))

 (define wally 
  (make <animal> 
	 (list 'name "Wally" 'type 'velociraptor 'representation 'individual 'dt 0.1
			  'mass wallymass
			  'location '(100 200 40) 'direction '(1 0 0) 'dim 3 
			  'subjective-time 0.0
			  'current-interest velociraptor-brain
			  'age 4 'sex 'male 
			  'speed 0
			  'structural-mass (* 160.1 0.6)
			  'structural-prop 0.3
			  'max-consumption-rate 1.0 ;; This is essentially saying that if it had a big enough stomach, it could eat this much times its mass in a day 
			  'metabolic-rate 0.07 ;; in (kg/kg)/day
;;			  'max-condition-rate 0.15 ;; in kg/day
;;			  'max-growth-rate 0.075 ;; in kg/day
			  'max-condition-rate 0.25 ;; in kg/day
			  'max-growth-rate 0.15 ;; in kg/day
			  'starvation-level 0.5
			  'gut-size 0.5 ;; as a proportion of struct-mass

			  'stomach-contents (* 160 0.5 0.2)
			  'condition (condition wallymass)
;;			  'condition 80.1

			  'food->condition-conversion-rate 8.0 ;; N kg to make 1kg of "condition"
			  'condition->food-conversion-rate 18.0  ;; 1 kg of body mass is worth this much
			  'food->mass-conversion-rate 14.0 ;; N kg to make 1kg of body mass
			  'mass->food-conversion-rate 18.0  ;; 1 kg of body mass is worth this much

			  ;'searchspeed 20 ;; units per day
			  ;'searchspeed 40 ;; units per day
			  'searchspeed 30 ;; units per day
			  'foragespeed 15 ;; units per day
			  'wanderspeed 20 ;; units per day
			  'movementspeed 40 ;; units per day

			  'habitat H 

			  'foodlist '(cow/bull rabbit sheep pig goat)
			  'breedlist '(velociraptor)
			  'homelist '(den)

			  'domain-attraction 0.22
			  'food-attraction 0.6
			  'near-food-attraction 0.5
			  )))
 (define wilma 
  (make <animal> 
	 (list 'name "Wilma" 'type 'velociraptor 'representation 'individual 'dt 0.1
			  'mass wilmamass
			  'location '(100 200 40) 'direction '(1 0 0) 'dim 3 
			  'subjective-time 0.0
			  'current-interest velociraptor-brain
			  'age 5 'sex 'female 
			  'speed 0
			  'structural-mass (* 180.1 0.6)
			  'structural-prop 0.3
			  'max-consumption-rate 1.0 ;; This is essentially saying that if it had a big enough stomach, it could eat this much times its mass in a day 
			  'metabolic-rate 0.07 ;; in (kg/kg)/day
;;			  'max-condition-rate 0.15 ;; in kg/day
;;			  'max-growth-rate 0.075 ;; in kg/day
			  'max-condition-rate 0.25 ;; in kg/day
			  'max-growth-rate 0.15 ;; in kg/day
			  'starvation-level 0.5
			  'gut-size 0.5 ;; as a proportion of struct-mass
			  'stomach-contents (* 180 0.5 0.2)
			  'condition (condition wilmamass)
;;			  'condition 80.1

			  'food->condition-conversion-rate 8.0 ;; N kg to make 1kg of "condition"
			  'condition->food-conversion-rate 18.0  ;; 1 kg of body mass is worth this much
			  'food->mass-conversion-rate 14.0 ;; N kg to make 1kg of body mass
			  'mass->food-conversion-rate 18.0  ;; 1 kg of body mass is worth this much

			  ;'searchspeed 20 ;; units per day
			  ;'searchspeed 40 ;; units per day
			  'searchspeed 30 ;; units per day
			  'foragespeed 15 ;; units per day
			  'wanderspeed 20 ;; units per day
			  'movementspeed 40 ;; units per day
			  'habitat H 
			  'foodlist '(cow/bull rabbit sheep pig goat)
			  'breedlist '(velociraptor)
			  'homelist '(den)
			  'domain-attraction 0.22			  'food-attraction 0.6
			  'near-food-attraction 0.5
			  )))

;;; Not currently working
(define psdumper 
  (make <log-map> (list 'name "Map" 
								'format 'ps
								'introspection-schedule schedtimes 
								'filename "map-" 'filetype "0.ps"
							  )
		  ))

;; <log-data> is pretty forgiving, but at the expense of verbosity
;; <log-agent-table> insists that only one agent be logged
;; <log-table> insists that all the agents possess all the fields, 

(define logger 
  (make <log-data> (list 'name "Data" 
							  'introspection-schedule schedtimes 
							  'filename "Data"
							  'variables (list 'name 'subjective-time 'value)) ;; log-table does not automatically log the name at the front of the line
							  )
		  )


(define wilmalog
  (make <log-data> (list 'name "Wilhelmina" 
											'introspection-list (list wilma)
											'introspection-schedule schedtimes 
											'filename "Wilma"
											;;'target-agent wilma
											'variables (list 'subjective-time 'location 'mass 'stomach-contents 'condition)) ;; log-data automatically logs the name at the front of the line
		  )
  )

(define wallylog
  (make <log-data> (list 'name "Waldo" 
											'introspection-list (list wally)
											'introspection-schedule schedtimes 
											'filename "Wally"
											;;'target-agent wally
											'variables (list 'subjective-time 'location 'mass 'stomach-contents 'condition)) ;; log-data automatically logs the name at the front of the line
		  )
  )
  


(define use-psdumper #f)

;(define Q (list wally H))
;(define Q (list H))
(define Q '())

(if (not (member 'nested-habitat nested-agents))
	 (set! Q (add-habitat-to-queue Q H)) ;; this introduces all of the "sub-agents" into the run-queue
	 (set! Q (list H)))

;(set! Q (append Q (list wally psdumper))) ;; there is no guarantee that the psdumper is run in any particular order w.r.t. the others
;(set! Q (append (list psdumper) Q))

(set! Q (append Q (list wilma wally))) ;; there is no guarantee that the psdumper is run in any particular order w.r.t. the others
(set! Q (append Q (list wilmalog wallylog))) ;; there is no guarantee that the psdumper is run in any particular order w.r.t. the others

(set-introspection-list! psdumper (copy-list Q))
(set-introspection-list! logger (copy-list (service-list H)))



(for-each (lambda (x) (set-map-projection! x mm->points)) Q)

(if use-psdumper
	 (set! Q (cons psdumper Q))
	 (set! Q (cons logger Q))
	 )

(define terminating-condition-test
  (let* ((tct terminating-condition-test)
			(l (lambda (Q)
				  (and (tct Q) (number? (slot-ref wally 'mass)) (number? (slot-ref wilma 'mass)))
				  )
				))
	 l))
	 



;;(if (member 'habitat nested-agents)
;;	 (set! Q (add-habitat-to-queue Q H)) ;; this introduces all of the "sub-agents" into the run-queue
;;	 (set! Q (list H))
;;	 (set! Q (list H wally))
;;	 )


(dnl "Now run:               (run-simulation Q start end)")
(dnl "    or keep going with (continue-simulation Q end)")
(dnl "Close up shop with (shutdown-agents Q) -- this closes files and things.")








;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
