; -*- mode: scheme; -*-
(include "remodel")
;-  Identification and Changes

;--
;	plant-classes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.02
;		Location: zero:/home/randall/Thesis/Example-Model/model/plant-classes.scm
;
;	History:
;
"
    Copyright 2017 Randall Gray

    This file is part of Remodel.

    Remodel is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Remodel is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Remodel.  If not, see <http://www.gnu.org/licenses/>.
"

;-  Code 

(define-class <plant>
  (inherits-from <model-maintenance> <living-thing>)
  (state-variables
	stress-color ;; 
	foliage-color ;; outer circle in plots
	radius-color  ;; inner circle in plots
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
	last-reproduced
	reproduction-age ;; how big it must be...
	reproduction-mass ;; how big it must be...
	reproduction-period ;; how long between reproductions
	reproduction-offset ;;
	reproduction-mechanism ;; <fruit> or (list <agent> method val)
	fruiting-probability ;; probability at each opportunity
	fruiting-mass ;; minimum mass for fruit production
	fruit-mass ;; mass of fruit
	fruiting-rate      ;; relative to surface area, influenced by

	growth-rate        ;; 
	forage-damage ;; to be subtracted from the canopy when calculating leaf-area
	regrowth-rate-multiplier ;; proportion of total leaf area regenerated per time period

	seeds-per-fruit
	domain            ;; #f or a patch/landscape thing
	population-switch

	mass-radius ;;  scalar or function which converts a leaf mass to a
	            ;;; radius (indicative of vegetation growth or loss)

	plot-circle-facets ;; number of facets -- useful for differentiating species
	))

(define-class <plant-proxy>
  (inherits-from <proxy> <plottable>) ;; needs projection for plotting 
  (state-variables lai leaf-mass
	default-color
	dead-color
	radius-color
	foliage-color
	stress-color
	glyph
	plot-circle-facets
	circle-facets
))


(define-class <example-plant>
  (inherits-from <plant>)
  (state-variables
	;;location from <thing> too...
	mort-mass
	mort-prob
	))




;; The following class is geared to handle a non-sparse cover of
;; plants. The main savings for this representation is the impact
;; on the queue.  In principle,  this representation 
;; could be used for all of the individual-based representations
;; of plants if we could accept the necessity for synchronicity.
;; 
;; This might be typically restricted to representing a <patch>.
;; Each of
;;    state ;; equivalent to the agent state
;;		omega-ind ;; individual mortality
;;		peak-mass ;; greatest mass attained
;;		mass ;; current mass
;;		leaf-area ;; we need a separate counter since there can be "accidents"
;;		water-stress  ;; [0,1]	level of water stress
;;		reproduction-offset ;;
;;		fruiting-probability ;; probability at each opportunity (might vary amongst individuals)

;;		forage-damage ;; to be subtracted from the canopy when calculating leaf-area
;;		seeds-per-fruit
;; will be lists of values.


(define-class <plant-array>
  (inherits-from <array>)
  (state-variables
	patch-list
	test-subject
	lai
	default-color
	dead-color
	radius-color
	foliage-color
	stress-color
	glyph
	plot-circle-facets
	circle-facets

   ;; data will be  a list with entries of the form
	;; (state mass peak-mass age location leaf-area forage-damage water-stress domain)
	;; state will either be 'dead or 'alive
))

;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:




