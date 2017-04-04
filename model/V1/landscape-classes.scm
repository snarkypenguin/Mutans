;-  Identification and Changes

;--
;	landscape-classes.scm -- Written by Randall Gray 

;-  Code 

;-- Environmental things

(define <ecoservice> (make-class (list <agent>) '(patch value capacity delta-T-max do-growth growth-model history)))  

(register-class <ecoservice>)
;; The update function is of the form (lambda (self t dt) ...)
;; where the args are specific things passed (like location, rainfall)
;; and any specific "parameters" ought to be part of the function's closure.


(define <patch> (make-class (list <environment>) '(location radius service-list)))
;; a patch is a location and radius with a list of ecological services
(register-class <patch>)

(define <dynamic-patch> (make-class (list <patch>) '(population-names population-symbols population-definitions do-growth dP d/dt-list subdivisions)))  ;; the definitions are kept for debugging


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



;; a patch is a location and radius with a list of ecological services

;; species-index is an association list which pairs ecoservice names to indices in the 
(register-class <dynamic-patch>)

(define <landscape> (make-class (list <environment>) '(terrain-function)))
;; terrain-function is a function in x and y that returns a DEM
(register-class <landscape>)

(define <habitat> (make-class (list <landscape>) '(patch-list dump-times scale)))
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
