(display "Loading landscape-classes.scm\n")
;-  Identification and Changes

;--
;	landscape-classes.scm -- Written by Randall Gray 

;-  Code 

;-- Environmental things

(define <ecoservice> (make-class (inherits-from <agent>)
											(state-variables
											 patch ;; spatial agent associated with the ecoservice
											 value ;; current value of the ecoservice
											 capacity ;; maximum value of the ecoservice
											 r        ;; recovery rate parameter
											 delta-T-max ;; largest stepsize the ecoservice can accept
											 do-growth   ;; #t/#f 
											 growth-model ;; routine to effect growth of the form (f t dt currentvalue)
											 history      ;; maintains  ((t value) ...)
											 )))  

(register-class <ecoservice>) ;; this class stands for biogenic parts of the system (ground water)
;; The update function is of the form (lambda (self t dt) ...)
;; where the args are specific things passed (like location, rainfall)
;; and any specific "parameters" ought to be part of the function's closure.


(define <patch> (make-class (inherits-from <environment>)
									 (state-variables location radius service-list)))
;; a patch is a location and radius with a list of ecological services
(register-class <patch>)

(define <patch> (make-class (inherits-from <environment>)
									 (state-variables location radius service-list)))

;;;; an <explicit-patch> is a <patch> with a perimeter rather than a radius.
;;(register-class <explicit-patch>)
;;(define <patch> (make-class (inherits-from <patch>)
;;									 (state-variables perimeter)))

;;
(define <dynamic-patch> (make-class (inherits-from <patch>)
												(state-variables
												 population-names        ;; list of strings associated with each population
												 population-symbols      ;; unique symbols for each of the pops
												 population-definitions  ;; list of the form ((nameA dA/dt) ...)
												 do-dynamics             ;; #t/#f whether to do the dynamics or not
												                         ;;   if this is false, state values are modified
												                         ;;   externally with calls to (set-value! 
												 do-growth               ;; #t/#f whether to do growth using the ecoservice
												                         ;;   growth-model or not
												 dP                      ;; This is generated using rk4* and d/dt-list
												 d/dt-list               ;; differential equations which describe the dynamics
												 subdivisions)))  ;; the definitions are kept for debugging


(comment "
A straightforward instantiation of a <dynamic-patch> might look like
  (define P (make <dynamic-patch> 'location loc 'radius radius 'type 
                  'patch 'representation 'patch 
                  'do-growth #f 'do-dynamics #t
                  'population-definitions (list (list \"plants\" 'plant dplant/dt) ... (list \"spiders\" spider dspider/dt))
  ))
or 
  (define P (make <dynamic-patch> 'location loc 'radius radius 'type 
                  'patch 'representation 'patch 
                  'do-growth #f 'do-dynamics #t
                  'population-names '(\"plants\" \"seeds\" \"aphids\" \"ants\" \"spiders\")
                  'population-symbols '(plant seed aphid ant spider)
                  'dp/dt-list (list dplant/dt dseed/dt daphid/dt dant/dt dspider/dt)
  ))
")



;; definitions look like ((name1 dn1/dt) ... (nameN dnN/dt))
;; or (eventually) like ((name1 prey-lst pred-lst helped-by-lst competitor-lst) ...)
;; but NOT a mixture of them.

;; Probably need to use some sort of Bayesian probability for populating "random" patches, lest we 
;; get unlikely species mixes (like camels and penguins)

;; Sticking to the service-update-map is critical for getting the right answers, y'know.

;;*** NOTE: update equations are lambdas which take the set of values associated with the services
;;*** present in the patch.  The nominated services are listed in the service-indices list either 
;;*** categorically  (the types) and strings (the names).  The types are the aggregate values of the 
;;*** names -- if you want to deal with a named entity separately, the update equation must explicitly 
;;*** remove it. They will be of the form (lambda (t ...) ...)  and *all* of the indicated services 
;;*** must be there or Bad Things Happen.


;; species-index is an association list which pairs ecoservice names to indices in the 
(register-class <dynamic-patch>)

(define <landscape> (make-class (inherits-from <environment>) (state-variables terrain-function)))
;; terrain-function is a function in x and y that returns a DEM
(register-class <landscape>)

(define <habitat> (make-class (inherits-from <landscape>) (state-variables patch-list dump-times scale)))
;; sites is a list of patches
(register-class <habitat>)

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
