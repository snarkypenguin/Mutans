;-  Identification and Changes

;--
;	velociraptor-init.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2014.10.12
;		Location: pooh:/local/home/randall/study/src/velociraptor-init.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2014 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 


(load 'load-list?)
(define use-psdumper #f) ;; Not currently working

;;(define schedtimes 
(define schedtimes (append 
						 (cons 0 (seq 6))
						 (map (lambda (x) (* 10.0 (1+ x))) (seq 360)))
  ) ;; first six days, then on every tenth day from the beginning for 370 days


(define H (generate-a-habitat "TheDomain" 'rangeland 0 (list '(0 0 10) domain )  (plane '(10 4 6 0)) 10 rich-population 12))


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


(define use-psdumper #f)

;;; Not currently working
(define psdumper 
  (make <log-map> (list 'name "Map" 
								'format 'ps
								'timestep-schedule schedtimes 
								'filename "map-" 'filetype "0.ps"
							  )
		  ))

;; <log-data> is pretty forgiving, but at the expense of verbosity
;; <log-agent-table> insists that only one agent be logged
;; <log-table> insists that all the agents possess all the fields, 

(define logger 
  (make <log-data> (list 'name "Data" 
								 'dt 4
								 'missing-val missing-value
							  'timestep-schedule schedtimes 
							  'filename "Data"
							  'variables '()) ;; log-table does not automatically log the name at the front of the line
							  )
		  )




(define wilmalog
  (make <log-data> (list 'name "Wilhelmina" 
								 'dt 20
								 'missing-val missing-value
								 'introspection-list (list wilma)
								 'timestep-schedule schedtimes 
								 'filename "Wilma"
								 ;;'target-agent wilma
								 'variables (list 'subjective-time 'location 'mass 'stomach-contents 'condition)) ;; log-data automatically logs the name at the front of the line
		  )
  )

(define wallylog
  (make <log-data> (list 'name "Waldo" 
								 'dt 20
								 'missing-val missing-value
								 'introspection-list (list wally)
								 'timestep-schedule schedtimes 
								 'filename "Wally"
								 ;;'target-agent wally
								 'variables (list 'subjective-time 'location 'mass 'stomach-contents 'condition)) ;; log-data automatically logs the name at the front of the line
		  )
  )
  

;(define Q (list wally H))
;(define Q (list H))

(if (not (member 'nested-habitat nested-agents))
	 (set! Q (add-habitat-to-queue Q H)) ;; this introduces all of the "sub-agents" into the run-queue
	 (set! Q (list H)))

;(set! Q (append Q (list wally psdumper))) ;; there is no guarantee that the psdumper is run in any particular order w.r.t. the others
;(set! Q (append (list psdumper) Q))

;(set! Q (append Q (list wally))) ;; there is no guarantee that the psdumper is run in any particular order w.r.t. the others
;(set! Q (append Q (list wilma wally))) ;; there is no guarantee that the psdumper is run in any particular order w.r.t. the others
;(set! Q (append Q (list wilmalog wallylog))) ;; there is no guarantee that the psdumper is run in any particular order w.r.t. the others

;(set-introspection-list! psdumper (copy-list Q))
(set-introspection-list! logger (copy-list (patch-list H)))
;;(set-introspection-list! logger (list wilma wally))

;;(extend-variables! logger (apply append (map extra-variable-list (patch-list H))))

;;(for-each 
;; (lambda (x) 
;;	(extend-variables! logger (map string->symbol (map name (service-list x))))
;;	)
;; (patch-list H))


(for-each (lambda (x) (set-map-projection! x mm->points)) Q)

;(if use-psdumper
;	 (set! Q (cons psdumper Q))
	 (set! Q (cons logger Q))
;	 )

(define terminating-condition-test
  (let* ((tct terminating-condition-test)
			(l (lambda (Q)
				  (and (tct Q) (number? (slot-ref wally 'mass)) (number? (slot-ref wilma 'mass)))
				  )
				))
	 l))
	 

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
