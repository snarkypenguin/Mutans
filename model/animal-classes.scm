(include "framework")
;-  Identification and Changes

;--
;	animal-classes.scm -- Written by Randall Gray 

;-  Code 

(define-class <simple-metabolism>
  (inherits-from <agent>) ;; needs to be an agent so the agent's model-body can chain to the metabolism
  (state-variables
	mass
	decay-rate
	period-of-hunger ;; how long it's been hungry
	hunger-limit ;; amount of time it can be hungry before death
	max-satiety ;; maximum number of satiety points
	satiety     ;; current number of satiety points
	satiety-rate ;; how fast we lose our satiety
	sated-quantity  ;; the number of points that indicate satiety
	sated-time      ;; how long the organism has been sated -- negative numbers correspond to "not sated"
	food-satiety-rate ;; how many satiety points a mass of generic food is worth
	))


(define-class <metabolism>
  (inherits-from <agent>) ;; needs to be an agent so the agent's model-body can chain to the metabolism
  (state-variables stomach-contents 
						 mass            ;; kg
						 decay-rate
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
  (inherits-from <simple-metabolism> <living-thing>)
  (state-variables sex  ;; we have mass here because things might eat them....
						 domain-attraction
						 homelist ;; categorical indicators of potential homes
						 foodlist ;; categorical indicators of potential prey
						          ;; implemented by calls to (*is?  ...)
						 breedlist ;; if this is null, it assumes the animal breeds with its taxon.
                             ;; Sex differences are checked only if the sex is 'female or 'male.

						 age-at-instantiation ;; the age offspring are introduced to the system
						 food-attraction
						 nominal-growth-rate
						 population-switch ;; nominal level at which it might pay to switch to analytic form
						                   ;; properly, this is handled by a monitor agent
						 search-radius
						 eat-radius    ;; the radius within which we will consider food to be accessible immediately
						 capture-radius  ;; the radius within which we will consider prey to be captured
						 endurance       ;; used in playing out chases between predator and prey
						 chase-duration  ;;
						 recovery-time
						 elapsed-recovery-required ;; the amount of time actually spent recovering. max-speed is 0.75 usual if this is positive.
						 predator-bias
						 prey-bias 
						 distance-cost
						 current-interest ;; prioritised list indicating the factors at play in decision making
						 ;; State variable indicates what "mode" of behaviour is dominant.
						 ;; Suggest '(any rest sleep forage hunt seek-mate seek-shelter drive-off-opponent)
						 activity-dt ;; an a-list of timesteps suitable for different activities
						 )
  ) ;; lists of attributes it looks for for eating, denning and breeding


;; current-interest is a function which takes (self age t dt ...) and
;; returns a meaningful symbol
;; 
;;							(H (my 'habitat))

(define-class <animal>
  (inherits-from <simple-animal>)
  ;; lists of attributes it looks for for eating, denning
  ;; and breeding
  (state-variables
	movement-speed ;; current speed to travel at

	hunt-speed     ;; (max) speed to use when "hunting" -- for a c'vore, this is probably pursuit, for h'vore it's browsing
                  ;;                                           __
	search-speed   ;; (max) speed used when searching for !food   \__  These are different so that we can use different 
	forage-speed   ;; (max) speed when looking/catching for food__/    types of searching (ambush, vs tracking for example)

	wander-speed   ;; speed when the animal is just moving about, not necessarily foraging or looking for a mate

	objective      ;; for eating, mating, driving off.....

	near-food-attraction
	))


(define-class <example-animal>
  (inherits-from <animal>)
  (state-variables
	peak-mass

	omega-ind ;; individual mortality

	adult-diet-mass

	prey-list ;; actual entities that can be used as food -- objective may be set from a value in this list

	food-density-limit ;; used to trigger migration
	last-reproduced
	reproduction-age
	reproduction-mass
	reproduction-prob
;;	reproduction-delay ;; from the start of the year
	reproduction-period
	reproduction-ct
	crowded-level
	migrate-param
	offspring ;; usually null
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
  (inherits-from <example-animal>)
  (stat-variables
	tree-satiety-rate ;; how much needs to be eaten for one satiety point
	))
  


;;; These are implemented only as an ecoservice
;(define-class <jcarn>
;  (inherits-from <example-animal>
;					  ))


(define-class <acarn>
  (inherits-from <example-animal>)
  (no-state-variables)
  )



;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
