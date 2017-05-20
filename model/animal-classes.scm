(include "framework")
;-  Identification and Changes

;--
;	animal-classes.scm -- Written by Randall Gray 

;-  Code 

(define-class <simple-metabolism>
  (inherits-from <object>)
  (state-variables days-of-hunger hunger-limit))


(define-class <metabolism>
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
						 max-condition-rate
						 max-growth-rate
						 )
  )


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

(define-class <simple-animal>
  (inherits-from <simple-metabolism> <thing>)
  (state-variables age sex habitat searchradius 
						 foodlist homelist breedlist
						 domain-attraction
						 food-attraction
						 nominal-growth-rate
						 population-switch ;; level at which it might pay to switch to analytic form
						 )
  ) ;; lists of attributes it looks for for eating, denning and breeding


;; current-interest is a function which takes (self age t dt ...) and
;; returns a meaningful symbol
;; 

(define-class <animal>
  (inherits-from <simple-animal>)
  ;; lists of attributes it looks for for eating, denning
  ;; and breeding
  (state-variables
	current-interest
	movementspeed
	searchspeed 
	foragespeed
	wanderspeed
	objective
	near-food-attraction
	))


(define-class <example-animal>
  (inherits-from <animal>)
  (state-variables
	cell
	age
	mass

	age-at-mass ;; Necessary for initialisation
	mass-at-age ;; Necessary for initialisation
	
	peak-mass
	omega-ind ;; individual mortality
	omega-hunger ;;; starvation time, $\Omega_{H}$ or $\Omega_{J}$ 

	max-satiety ;; maximum number of satiety points
	satiety     ;; current number of satiety points
	satiety-rate ;; how fast we lose our satiety
	sated-quantity  ;; the number of points that indicate satiety
	sated-time      ;; how long the organism has been sated -- negative numbers correspond to "not sated"

	time-spent-hungry

	prey-list
	forage-ct

	reproduction-age
	reproduction-mass
	reproduction-prob
	reproduction-delay
	reproduction-ct
	crowded-level
	migrate-param
	))


(define-class <jherb>
  (inherits-from <example-animal>)
  (state-variables
  	adult-diet-mass ;;  when mass is over this value, they can eat trees as well
	seedcount       ;;  just makes things a little quicker
	seed-lag-list*  ;; time triggered, first el in each element is time to deposit seeds
	tree-satiety-rate ;; how much needs to be eaten for one satiety point
	fruit-satiety-rate ;; how many need to be eaten for one satiety point
	))

(define-class <aherb>
  (inherits-from <example-animal>
	tree-satiety-rate ;; how much needs to be eaten for one satiety point
					  ))

;;; These are implemented only as an ecoservice
;(define-class <jcarn>
;  (inherits-from <example-animal>
;					  ))


(define-class <acarn>
  (inherits-from <example-animal>
					  ))



;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
