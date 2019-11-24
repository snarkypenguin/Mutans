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

;;(set! kernel-messages (append kernel-messages '(running focus stomach hunger-proximity eating)))
;;(set! kernel-messages (append kernel-messages '(animal-running stomach hunger-proximity eating)))
;;(set! kernel-messages (append kernel-messages '(metabolism animal-running stomach hunger-proximity eating)))
;;(set! kernel-messages (append kernel-messages '(model-bodies animal-running stomach hunger-proximity eating)))


;(define landscape
;  (make <landscape> (list 'name "landscape" 'type 'forest-floor 'representation 'plane 'terrain-function (plane '(12 8 4 0)) 'default-value 0 'minv '(0 0 0) 'maxv '(100 100 2000))))



;; Need to implement simple population matrices for patches.  


(define (rerun-agent agent #!optional dt)
  (set! dt (if dt dt 0.1))
  (run-model-body agent (slot-ref agent 'subjective-time) dt))



;; The ecoservlist looks like ((probability name type meanval) ...)
(define (knock-up-a-habitat namn typ default-ht domain terrain-function npatches ecoservlist)
  (let ((n (seq npatches))
		  )
	 (let ((patches (map (lambda (x) ;; each patch
								  (let ((P (make-patch (apply random-location domain) (nrandom (/ (patchsize domain) npatches)) '())))
									 (slot-set! P 'name (string-append namn "-" (pno x)))
									 (let ((p (filter (lambda (t) t)
															(map (lambda (y) ;; each ecoservice
																	 (let ((cap (nrandom (cadddr y))))
																		(if (> (random-real) (car y))
																			 #f
																			 (make <ecoservice> 'patch P 'name (cadr y) 
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
		(make <habitat> 'name namn 'type typ 'representation 'accurate 
				'minv (car domain) 'maxv (cadr domain) 
				'terrain-function (plane '(8 2 4 0)) 'default-value default-ht 'patch-list patches))))

(define H (knock-up-a-habitat "Test-domain" 'rangeland 0 (list '(0 0 10) domain ) (plane '(10 4 6 0)) 10 
										'(
										  (0.025 "myrtle" tree 2) 
										  (0.125 "gum" tree 12)
										  (0.8 "wattle" tree 2) 
										  (0.9 "pasture" grass 2) 
										  (0.3 "hay-crop" grass 2) 
										  (0.4 "rabbit" rabbit 20 0.) 
										  (0.5 "sheep" fern 8) 
										  (0.25 "dairy-cattle" cow/bull 4)
										  (0.25 "beef-cattle" cow/bull 4)
										  )))


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

(define (condition m) (* 0.6 m))

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

			  'foodlist '(cow/bull rabbit sheep)
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
			  'foodlist '(cow/bull rabbit sheep)
			  'breedlist '(velociraptor)
			  'homelist '(den)
			  'domain-attraction 0.22			  'food-attraction 0.6
			  'near-food-attraction 0.5
			  )))

;;; Not currently working
(define psdumper 
  (make <log-map> (list 'name "Map" 
								'timestep-schedule schedtimes 
							  'filename "map-" 'filetype "0.ps" 'append-time #t
							  )
		  ))

(define logger 
  (make <log-data> (list 'name "Data" 
							  'timestep-schedule schedtimes 
							  'filename "Data" 'append-time #f
							  'variables '(subjective-time location mass value)) ;; log-data automatically logs the name at the front of the line
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

(set-introspection-list! psdumper (copy-list Q))
(set-introspection-list! logger (copy-list Q))

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
	 

(define run-simulation
  (let ((rs run-simulation))
	 (lambda args 
		(dnl "Args" args)
;;;		(emit-page psdumper 0.0 (name psdumper) "0.ps")
		(apply rs args)
		;;(apply rs (append args (list (lambda () (shutdown-agents (car args))))))
		)))


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
