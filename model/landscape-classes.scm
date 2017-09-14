(include "framework")
;-  Identification and Changes
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

;--
;  landscape-classes.scm -- Written by Randall Gray 

;-  Code 

;-- Environmental things

(define-class <ecoservice>
  (inherits-from <agent>)
  (state-variables
	running-externally ;; #t/#f
	external-rep-list ;; used to point to external represetations (usually IBM/ABM)
	;; The external representation list
	ext-get  ;; the method or function to use to get data from a member of the external-rep-list
	ext-set! ;; analogous to ext-get, but for setting the value of the member
	name ;; Used in output
	sym ;; symbol (possibly used in updates by other agents
	patch ;; spatial agent associated with the ecoservice
	value ;; current value of the ecoservice
	capacity ;; maximum value of the ecoservice
	r        ;; recovery rate parameter -- sharpness of  
	         ;; transition in growth curve(sigmoid),
	         ;; increase per unit of time (linear)
	
	delta-T-max ;; largest stepsize the ecoservice can accept
	do-growth   ;; #t/#f 
	growth-model ;; routine to effect growth of the form (f
	;; t dt currentvalue)
	history      ;; maintains  ((t value) ...)
	))


(Comment "An <ecoservice> typically stands for biogenic parts of the
system (ground water), or a pool of plant/animal biomass.  In
practice, it is any kind of quantity we might want to track -- numbers
of pamphlet at kiosks, the number of unit length needles 
touch parallel lines on an infinite xy-plane, each with integer x values....

The update function is of the form (lambda (self t dt) ...)  where the
args are specific things passed (like location, rainfall) and any
specific 'parameters' ought to be part of the function's closure. The
special growth functions 'sigmoidal and 'linear can be specified by
passing the appropriate symbol.

A typical construction of an ecoservice might look like
(create <ecoservice> ecosrvtaxon
  'patch P 
  'value 42000 
  'capacity +inf.0 
  'r 1 
  'delta-T-max (days 2) 
  'do-growth #t 
  'growth-model
	      ;;; 'sigmoid
	      ;;; 'linear
  (lambda (t dt v) 
	 (let* ((tssn (/ t 365.0)) 
			  (ssn (- tssn (trunc tssn)))) 
		(cond 
		 ((<= 0 0.3) (/ r 12))
		 ((<= 0 0.7) (/ r 3))
		 ((<= 0 0.8) (/ r ))
		 (#t (/ r 6)))))
  ;; 
  )
")


(define-class <population-system>
  (inherits-from <ecoservice>)
  (state-variables
	patch ;; spatial agent associated with the ecoservice
	value ;; current value of the ecoservice
	capacity ;; maximum value of the ecoservice
	r        ;; recovery rate parameter (sharpness of
	;; sigmoidal growth)
	delta-T-max ;; largest stepsize the ecoservice can
	;; accept
	do-growth   ;; #t/#f 
	growth-model ;; routine to effect growth of the form 
	;;   (f t dt currentvalue)
	history      ;; maintains  ((t value) ...)
	))


(define-class <polygon> (inherits-from <plottable>)  ;; not an agent
  (state-variables perimeter is-relative radius area)
  )

(define-class <circle> (inherits-from  <plottable>)  ;; not an agent
  (state-variables perimeter radius area)
  )

(define-class <patch> (inherits-from  <environment> <projection>)
  (state-variables service-list caretaker notepad
						 index neighbour-list
						 ))
;; This one needs a 'rep set (which is what <environment> provides
;; The caretaker variable may be a process of the form

;; (lambda (self t dt) ...)  which will be called in each of its timesteps.
;; This process cannot modify t or dt.

;; notepad is (notionally) a list which can be used as quasi-permanent
;; storage

;; neighbour-list is a list of pairs of the form (patchagent number|symbol)
;; where a number indicates the "length" of the interface between the two and
;; a symbol indicates an interface of some other sort.  This is either filled
;; in by the routine that constructs the ensemble, by some other routine, or
;; ignored utterly.


(Comment "A patch is a geographic region with a list of ecological
services.  The representation (rep) is a spatial thingie.

Adding a caretaker to a <patch> adds the ability to do
processing of some sort at each timestep.")

(define-class <patch*> (inherits-from  <patch>)
  (state-variables ))

;;
(define-class <dynamic-patch>
  (inherits-from <patch*> <diffeq-system>)
  (state-variables
	do-dynamics       ;; #t/#f whether to do the dynamics
	;; or not if this is false, state
	;; values are modified externally
	;; with calls to (set-value!

	do-growth   ;; #t/#f whether to do growth using the
	;; ecoservice growth-model or not

	subdivisions))  ;; the definitions are kept for debugging


(Comment "<dynamic-patches> are patches that have a system of
differential equations which stand in the place of the simpler
representation of patches with ecoservices.

A straightforward instantiation of a <dynamic-patch> might look like
  (define P (create <dynamic-patch> ptax 'location loc 'radius radius 
                  'representation 'patch 
                  'do-growth #f 'do-dynamics #t
                  'population-definitions 
						  (list (list \"plants\" 'plant dplant/dt) 
                         ... (list \"spiders\" spider dspider/dt)) ))
or 
  (define P (create <dynamic-patch> ptax 'location loc 'radius radius 
          'representation 'patch 
          'do-growth #f 'do-dynamics #t
          'population-names 
            '(\"plants\" \"seeds\" \"aphids\" \"ants\" \"spiders\")
          'population-symbols '(plant seed aphid ant spider)
          'dp/dt-list 
             (list dplant/dt dseed/dt daphid/dt dant/dt dspider/dt)
  ))


Definitions look like ((name1 dn1/dt) ... (nameN dnN/dt))
or (eventually) like ((name1 prey-lst pred-lst helped-by-lst
competitor-lst) ...)  but NOT a mixture of them.

Probably need to use some sort of Bayesian probability for populating
'random' patches, lest we get unlikely species mixes (like camels and
penguins)

Sticking to the service-update-map is critical for getting the right
answers, y'know.

NOTE: update values are calculated by lambdas which take the set of
values associated with the services present in the patch.  The
nominated services are listed in the service-indices list either
categorically (the symbols) and strings (the names).  The types are the
aggregate values of the names -- if you want to deal with a named
entity separately, the update equation must explicitly remove it. They
will be of the form (lambda (t ...) ...)  and *all* of the indicated
services must be there or Bad Things Happen.
")

;; species-index is an association list which pairs ecoservice names
;; to indices in the

(define-class <landscape> (inherits-from <environment>)
  (state-variables patch-list terrain-function))
;; terrain-function is a function in x and y that returns a DEM
;; patch-list is a list of patches -- the patches can be either patches,
;;   dynamic-patches or a mix -- the list is passed in at initialisation.


;;; ;; Habitats couple lists of patches to lists of services, and provide for
;;; ;; nested execution.  In practice, they don't actually accomplish much
;;; ;; that cannot be provided by landscape and ecoservices

;;; (define-class <habitat> (inherits-from <landscape>)
;;;   (state-variables dump-times scale internal-runqueue))

;;; (define-class <habitat*> (inherits-from <habitat>)
;;;   (state-variables global-patch global-update))
;;; ;; global-patch is a patch/dynamic-patch which maintains "variables" 
;;; ;;   pertinent to the whole domain (and it ought to contain the patch-list)
;;; ;; global-update is a function that updates the values in the global patch.



;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:

