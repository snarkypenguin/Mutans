;-  Identification and Changes

;--
;	habitat-support.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2014.10.12
;		Location: pooh:/local/home/randall/study/src/habitat-support.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2014 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(load 'load-list?)
(define (history-data H)
  ;; (map name (patch-list H))
  ;; (map cons (map name (patch-list H)) (map service-list (patch-list H)))
  (map (lambda (x) (cons (car x) (map (lambda (y) (list (name y) (slot-ref y 'history))) (cdr x)))) (map cons (map name (patch-list H)) (map service-list (patch-list H))))
  )


;(define landscape
;  (make <landscape> (list 'name "landscape" 'type 'forest-floor 'representation 'plane 'terrain-function (plane '(12 8 4 0)) 'default-value 0 'minv '(0 0 0) 'maxv '(100 100 2000))))


;; ecoservsetlist is a list of sets of ecological services which might be incorporated in a patch. 
;; Each of these sets has an associated "accepted" function which will return #t if the generated
;; patch is ok, otherwise it returns #f.  The format of an ecoservsetlist is
;;
;;     ((eco-serv-set-prob  (prob name type cap mean-biomass update-function) ...) ...)

(define (check-service-data-lists service-name-list service-type-list service-eqn-sym-list)
  (let* ((type-symbol-clash (intersection service-eqn-sym-list service-type-list))
			(unique-service-eqn-sym-list (unique* service-eqn-sym-list))
			(unique-service-name-list (unique* service-name-list))
			(duplicate-symbols (filter (lambda (x) (not-member x unique-service-eqn-sym-list)) service-eqn-sym-list))
			(duplicate-names (filter (lambda (x) (not-member x unique-service-name-list)) service-name-list))
			)
	 (if type-symbol-clash (dnl "The symbols " type-symbol-clash " exist in both the "
										 "type column and the symbol column in the service-data-list"))
	 (if (not (null? duplicate-symbols)) (dnl "The symbol(s) " duplicate-symbols " are duplicated in the symbol column"))
	 (if (not (null? duplicate-names)) (dnl "The symbol(s) " duplicate-names " are duplicated in the name column"))
	 (if (not (and (null? duplicate-symbols) (null? duplicate-names) (null? type-symbol-clash)))
		  (abort "Ill-defined configuration"))))
		  


(define (generate-an-ecoservice-like P prob name type cap value dt r dTmax)
  (if (or (and (boolean? prob) prob) (and (number? prob) (<= (random-real) prob)))
		(let ((esp (make <ecoservice> 'patch P 'name name 'type type 'capacity cap 'value value 'dt dt 'r r 'delta-T-max dTmax 'history (if record-ecoservice-history '() #f))))
		  ;(disable-growth! esp)
		  esp
		  )
		#f
		))

;; A dynamic patch is a system of DEs which represent different species in the indicated domain.
;;(define (generate-a-dynamic-patch-like namn domain radius ecoservlist popname-list derivatives #!optional subdivisions)
;; 
;; where namn is a string, domain is a box it is seeded into, radius is the radius of the patch, ecoservlist is 
;; a list like '((.3 hay-crop grass 20000000 2000000 3 .01) (.9 rabbit rabbit 6000 600 3 .01) (.05 goat caprid 160 16 3 .01))
;; where the first element is the probability of inclusion, the next is the name, then type, max value (or #!+inf) starting value, 
;; and finally the dt and r
;; The species-eqn-list-or-false should either be false or a list (one for each species) of functions of the form
;;
;;    (growth-func dt population-level-lst)


(define (generate-a-dynamic-patch-like namn domain radius ecoservlist populations subdivisions)
  ;; Handle optional arguments
  (let ((P (make <dynamic-patch> 'name namn 'location (apply random-location domain) 'radius radius 'population-names (populations 'species-names) 'population-symbols (populations 'species-eqn-list-or-false))))
	 (filter (lambda (t) t)
				(for-each (lambda (x)
								(let ((p (apply generate-an-ecoservice-like (cons P x))))
								  (if p (add-service P p))
								  p))
							  ecoservlist)
				)
	 
	 ;; We install the dynamics here
	 (slot-set! P 'd/dt-list (populations 'd/dt-list))
	 (disable-all-service-growth! P)
	 
	 (if subdivisions (slot-set! P 'subdivisions subdivisions))
	 P))
		 
;;(define (generate-a-dynamic-patch-from-set name domain radius ecoservsetlist derivatives #!optional subdivisions)
(define (generate-a-dynamic-patch-from-set name domain radius populations subdivisions)

  (let* ((template (populations 'template))
			(ecoservlist (random-list-ref (map cdr template) (map car template))))
	 (generate-a-dynamic-patch-like name domain radius ecoservlist populations subdivisions)))


;;(define (generate-a-habitat namn typ default-ht domain terrain-function npatches predator-prey-matrix species-data ecoservsetlist #!optional subdivisions)
(define (generate-a-habitat namn typ default-ht domain terrain-function npatches populations subdivisions)
  (let ((H 
			(make <habitat> 
			  'name namn 'type type 'representation 'intermediate 'minv (car domain) 'maxv (cadr domain)
			  'terrain-function terrain-function 'default-value default-ht
			  'patch-list 
			  (map (lambda (x) 
						(let* ((pname (string-append namn "-" (number->string x)))
								 (P (generate-a-dynamic-patch-from-set pname domain (nrandom (/ (patchsize domain) npatches)) populations subdivisions))
								 )
						  P))
					 (map 1+ (seq npatches)))
			  )
			) )
	 H)
)
#|
dA = t A{r(1 - A/K)[1 - prey_adj ∑_i(prey_i prey_rate_i)]/(maint_rate * A) - pred_adj ∑_i (pred_i pred_rate_i)}

where A is anything, K is the carrying capacity, r is the natural
growth rate, maint_rate is the amount it needs to consume to maintain
biomass.  It is an error to have a prey rate for things that don't
have maint_rates, and we need to bypass the division by zero in this
case.

|#

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
