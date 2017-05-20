; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	animal-notes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.07.14
;		Location: zero:/home/randall/Thesis/Example-Model/model/animal-notes.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2016 Randall Gray
;   All rights reserved
;

(define animal-notes "
Animals will usually be initialised like so:
   (make-agent <animal> 'name (new-animal-label) 'representation 'individual
         'subjective-time 0.0 'dt 0.1 'priority RunLast 'jiggle (random-real) 
         'mass velocimass
         'location '(100 200 40) 'direction '(1 0 0) 'dim 3 
         'current-interest velociraptor-brain
         'age 4 'sex 'male 
         'speed 0
         'structural-mass (* velocimass 0.6)
         'structural-prop 0.3
         'max-consumption-rate 1.0 
;; This is essentially saying that if it had a big enough stomach, it
;; could eat this much times its mass in a day
         'metabolic-rate 0.07 ;; in (kg/kg)/day
         'max-condition-rate 0.25 ;; in kg/day
         'max-growth-rate 0.15 ;; in kg/day
         'starvation-level 0.5
         'gut-size 0.5 ;; as a proportion of struct-mass
         'stomach-contents (* 160 0.5 0.2)
         'condition 80.1

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
         )       

This is a very general animal with very little 'speciation', and more
refined animals are likely to have pre-set state variables and
alternative methods for doing things.
")




;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:




