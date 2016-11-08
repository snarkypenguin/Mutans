(include "framework")
;-  Identification and Changes

;--
;  landscape-classes.scm -- Written by Randall Gray 

;-  Code 

;-- Environmental things

(define-class <ecoservice>
  (inherits-from <agent>)
  (state-variables
	name ;; Used in output
	sym ;; symbol (possibly used in updates by other agents
	patch ;; spatial agent associated with the ecoservice
	value ;; current value of the ecoservice
	capacity ;; maximum value of the ecoservice
	
	r        ;; recovery rate parameter
	;; -- sharpness of
	;; transition in growth
	;; curve(sigmoid) ,
	;; increase per unit of
	;; time (linear)
	
	delta-T-max ;; largest stepsize the ecoservice can accept
	do-growth   ;; #t/#f 
	growth-model ;; routine to effect growth of the form (f
	;; t dt currentvalue)
	history      ;; maintains  ((t value) ...)
	))


(Comment "An <ecoservice> stands for biogenic parts of the system (ground
water) The update function is of the form (lambda (self t dt) ...)
where the args are specific things passed (like location, rainfall)
and any specific 'parameters' ought to be part of the function's
closure. The special growth functions 'sigmoidal and 'linear  can 
be specified by passing the appropriate symbol.

A typical construction of an ecoservice might look like
(make <ecoservice> 
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


(define-class <polygon> (inherits-from <object>)
  (state-variables locus perimeter radius)
  )

(define-class <circle> (inherits-from <object>)
  (state-variables locus perimeter radius)
  )




(define-class <patch> (inherits-from  <environment>)
  (state-variables service-list rep))

(Comment "A patch is a geographic region with a list of ecological
services.  The representation (rep) is a spatial thingie.")

;;
(define-class <dynamic-patch>
  (inherits-from <patch>)
  (state-variables
	population-names ;; list of strings associated with
	;; each population

	population-symbols ;; unique symbols for each of the
	;; pops

	population-definitions  ;; list of the form ((nameA
	;; dA/dt) ...)
	do-dynamics       ;; #t/#f whether to do the dynamics
	;; or not if this is false, state
	;; values are modified externally
	;; with calls to (set-value!

	do-growth   ;; #t/#f whether to do growth using the
	;; ecoservice growth-model or not

	dP    ;; This is generated using rk4* and d/dt-list

	d/dt-list   ;; differential equations which describe
	;; the dynamics

	subdivisions))  ;; the definitions are kept for
;; debugging


(Comment "<dynamic-patches> are patches that have a system of
differential equations which stand in the place of the simpler
representation of patches with ecoservices.

A straightforward instantiation of a <dynamic-patch> might look like
  (define P (make <dynamic-patch> 'location loc 'radius radius 'type 
                  'patch 'representation 'patch 
                  'do-growth #f 'do-dynamics #t
                  'population-definitions 
						  (list (list \"plants\" 'plant dplant/dt) 
                         ... (list \"spiders\" spider dspider/dt)) ))
or 
  (define P (make <dynamic-patch> 'location loc 'radius radius 'type 
          'patch 'representation 'patch 
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
categorically (the types) and strings (the names).  The types are the
aggregate values of the names -- if you want to deal with a named
entity separately, the update equation must explicitly remove it. They
will be of the form (lambda (t ...) ...)  and *all* of the indicated
services must be there or Bad Things Happen.
")

;; species-index is an association list which pairs ecoservice names
;; to indices in the

(define-class <landscape> (inherits-from <environment>)
  (state-variables terrain-function))
;; terrain-function is a function in x and y that returns a DEM

(define-class <habitat> (inherits-from <landscape>)
  (state-variables patch-list dump-times scale))
;; sites is a list of patches -- the patches can be either patches,
;; dynamic-patches or a mix -- the list is passed in at initialisation.



;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
