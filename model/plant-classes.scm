; -*- mode: scheme; -*-
(include "framework")
;-  Identification and Changes

;--
;	plant-classes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.02
;		Location: zero:/home/randall/Thesis/Example-Model/model/plant-classes.scm
;
;	History:
;

;-  Code 

(define-class <plant>
  (inherits-from <model-maintenance> <living-thing>)
  (state-variables
	decay-rate  ;; usually somethings like -2/(* 20 days)
	omega-ind ;; individual mortality
	peak-mass ;; greatest mass attained
	max-mass ;; unlikely to reach this
	lai ;; leaf area index in (0,10) -- ideal leaf area/area in drip-line
	leaf-area ;; we need a separate counter since there can be "accidents"
	leaf-mass ;; mass of a leaf
	water-stress  ;; [0,1]	level of water stress
	water-use ;; amount of water required for a square metre
	water-stress-effect ;; True if we factor water stress
	;; into fruiting
	reproduction-mass ;; how big it must be...
	reproduction-period ;; how long between reproductions
	reproduction-offset ;;
	reproduction-mechanism ;; <fruit> or (list <agent> method val)
	fruiting-probability ;; probability at each opportunity
	fruiting-mass ;; minimum mass for fruit production
	fruiting-rate      ;; relative to surface area, influenced by

	growth-rate        ;; 
	forage-damage ;; to be subtracted from the canopy when calculating leaf-area
	regrowth-rate ;; proportion of total leaf area regenerated per time period

	seeds-per-fruit
	habitat            ;; #f or a patch/landscape/habitat thing
	population-switch

	plot-circle-facets ;; number of facets -- useful for differentiating species
	plot-magnification ;; less than one contracts, 
	))


(define-class <example-plant>
  (inherits-from <plant>)
  (state-variables
	;;location from <thing> too...
	fruiting-mass
	fruiting-prob
	mort-mass
	mort-prob
	))




;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
