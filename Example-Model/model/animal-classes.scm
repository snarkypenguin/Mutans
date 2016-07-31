;-  Identification and Changes

;--
;	animal-classes.scm -- Written by Randall Gray 

;-  Code 

(define <simple-metabolism>
  (make-class (inherits-from <object>)
				  (state-variables days-of-hunger hunger-limit)))
(register-class <simple-metabolism>)

(define <metabolism>
  (make-class
	(inherits-from <object>)
	(state-variables stomach-contents 
						  mass            ;; kg
						  structural-prop ;; structural-mass increases as
						                  ;; mass increases -- all things
						                  ;; that have metabolism must have
						                  ;; mass
						  structural-mass ;; the maximum amount it could eat
						                  ;; in a given period, assuming
						                  ;; aninfinite stomach
						  max-consumption-rate ;; the amount of "stuff" or "stuff-equivalent" 
						  ;; needed per kg per unit of time
						  metabolic-rate
						  starvation-level ;; death if
												 ;; (mass/structural-mass) goes
												 ;; below this value
						  gut-size        ;; stomach capacity as a
												;; proportion of structural-mass
						  condition       ;; "fat" reserves
						  food->condition-conversion-rate 
						  condition->food-conversion-rate
						  food->mass-conversion-rate
						  mass->food-conversion-rate
						  condition-conversion-rate
						  mass-conversion-rate
						  max-growth-rate
						  max-condition-rate
						  )
				  ))


(register-class <metabolism>)

;; structural-mass is pegged at (max (* (my 'mass) (my
;; 'structural-prop))) through time
;; structural-prop is multiplied by (my 'mass) to give a number less
;; than or equal to the structural mas
;; metabolic-rate: (* metabolic-rate (my 'mass) dt) is the mass
;; required to maintain body mass for dt
;; ... consumption above that rate is converted to "condition"
;; == _
;;   | base-rate is the amount of mass removed per dt for stomach contents
;;   |_ condition-rate is the mass removed per dt for conditon contents
;;
;; starvation-rate is the amount of body mass removed per dt
;; starvation-level is the value of (/ (my 'mass) (my
;; 'structural-mass)) at which an organism dies.  This really ought to
;; be a number like 1.3
;; stomach-contents is an absolute amount of food
;; gut-size is a scalar multiplier of the structural-mass which
;; indicates the max cap. of the stomach
;; condition is an absolute number which is equivalent to mass in the
;; stomach
;; food->condition-conversion-rate is the efficiency of conversion
;; from stomach food to condition food
;; condition->food-conversion-rate is the efficiency of conversion
;; from condition to food
;; food->mass-conversion-rate is the efficiency of conversion from
;; stomach food to mass food
;; mass->food-conversion-rate is the efficiency of conversion from
;; mass to food
;; max-consumption-rate is the rate at which things can move through
;; the system

(define <simple-animal>
  (make-class (inherits-from <simple-metabolism> <thing>)
				  (state-variables age sex habitat searchradius 
										 foodlist homelist breedlist
										 domain-attraction
										 food-attraction
										 ))
  ) ;; lists of attributes it looks for for eating, denning and breeding
(register-class <simple-animal>)


;; current-interest is a function which takes (self age t dt ...) and
;; returns a meaningful symbol
;; 

(define <animal>
  (make-class (inherits-from <metabolism> <thing>)
				  ;; lists of attributes it looks for for eating, denning
				  ;; and breeding
				  (state-variables
					current-interest age sex
					habitat searchradius foodlist homelist breedlist
					movementspeed
					searchspeed 
					foragespeed
					wanderspeed
					objective
					domain-attraction
					food-attraction
					near-food-attraction
					)
				  
				  ))
(register-class <animal>)

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
