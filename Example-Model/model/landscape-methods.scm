;-  Identification and Changes

;--
;	landscape.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.19
;		Location: odin:/home/gray/study/src/new/landscape.scm
;
;	History:
;


;-  Copyright 

;
;   (C) 2012 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

(load "postscript.scm")

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 



;-  Variables/constants both public and static

(define PATCHGREY 0.2)
(define HABITATGREY 0.1)

;- Environmental code

;-- Supporting routines

;--- Generally useful routines

;(define (I->E f) (inexact->exact (round f)))
(define (I->E f) (inexact->exact (truncate f)))

(define (repro xy res m)
  (map I->E (map (lambda (x) (/ x res)) (map - xy m))))


;;(define (logistic-growth  dt domain 0  value capacity  rvalue)

;(define service? (make-generic))
;(define add-service (make-generic))
;(define remove-service (make-generic))
;(define service-list (make-generic))
;(define service (make-generic))
;(define services (make-generic)) ;; returns value
;(define set-services! (make-generic)) ;; sets value
;(define value (make-generic))      -- defined for environment
;(define add-to-value (make-generic))      -- defined for environment
;(define set-value! (make-generic)) -- defined for environment

;--- oriented toward habitats, patches  and ecoservices
;---- ecoservice lists, nearest suppliers....

(define (locate-nearest-ecoserv habitat ecoserv loc)
  (let* ((patches (service-sites habitat ecoserv))
			(dists (map (lambda (x) (distance-to-centre x loc)) patches))
			(sdists (sort 
						(filter 
						 (lambda (x) (and (number? (car x)) 
												(not (null? (cdr x)))))
						 (map cons dists patches)) 
						(lambda (x y) (< (car x) (car y)))
						))
			)
	 (if (null? sdists) #f (cdar sdists))))

(define (sorted-ecoservices habitat ecoserv loc . weighted-by-value)
  (let* ((patches (service-sites habitat ecoserv)))
	 (let ((dists (map (lambda (x) (distance-to-centre x loc)) patches)))
		(let ((sdists (sort 
							(filter 
							 (lambda (x) (and (number? (car x)) 
													(not (null? (cdr x)))))
							 (map cons dists patches)) 
							
							(if (null? weighted-by-value)
								 (lambda (x y)
									(< (car x) (car y)))	
								 (lambda (x y)
									(< (/ (total-value (cdr x) ecoserv) (+ 1 (car x)))
										(/ (total-value (cdr y) ecoserv) (+ 1 (car y)))
										))
								 )
							)))

		  (if (null? sdists)
				#f
				(map (lambda (x) (list (car x) (cdr x))) sdists))))
	 ))


;---- Applicable to patches and patch lists

(define (patchsize domain)
  (* 0.25 (apply min (map - (list-head (cadr domain) 2)
								  (list-head (car domain) 2)))))

(define (total-patch-list-value patchlist symlist)
  (map (lambda (x) (total-value x symlist)) patchlist))


(define (total-patch-list-capacity patchlist symlist)
  (map (lambda (x) (total-capacity x symlist)) patchlist))


(define (translate-trace addend trace)
  (map
	(lambda (v)
	  (map + addend v))
	trace))

(define (scale-trace s trace)
  (if (number? s) (set! s (make-list (length (car trace)) s)))
  (if (= (length s) (length (car trace)))
		(map
		 (lambda (v)
			(map * s v))
		 trace)
		(error "Incompatible vectors" s trace)
		))
;---- For habitats

(define (def-res H)
  (let* ((m (min-bound H))
			(M (max-bound H))
			(extent (map - M m))
			)
	 (/ (apply min extent) 20.0)))


;---- Generating postscript

(define print-environment-data
  (lambda (ps p x n ns loc rad)
	 ;;(dnl (pno (value x)))
	 (ps 'moveto (map p (map + (list-head loc 2)
									 (map p (list (* 1.0 rad)
													  (* (- (/ ns 2.0) n) 1.0))))))
	 (if adjust-grey (ps 'setgray PATCHGREY)) ;; zero is white...
	 (ps 'Helvetica 7)
	 (ps 'show (string-append (slot-ref x 'name) ": "
									  (number->string (value x))))
	 ))


(define crop-caption
  (lambda (ps p x . pt)
	 (if (null? pt) (set! pt 10))
	 (let ((loc (map p (list-head (location x) 2)))
			 (rad (p (radius x))))
		(ps 'moveto (map - loc
							  (list (* 0.5 rad)  (* -1 (+ 5 (* 1 rad) )))))
		(ps 'Helvetica pt)
		(if adjust-grey (ps 'setgray PATCHGREY))
		(ps 'show-right (string-append
							  (slot-ref x 'name) " at "
							  (number->string (slot-ref x 'subjective-time))))
		)
	 )
  )

;--- (make-population-structure predation-matrix efficiency-matrix
;                   service-data-list ecoservice-template)

(define (make-population-structure predation-matrix efficiency-matrix
											  service-data-list ecoservice-template)
  ;;(pp service-data-list)
  ;;(pp ecoservice-template)
;(abort 3)
  (let* ((predation-matrix predation-matrix)
			(PM predation-matrix)
			(EM (efficiency-matrix 'transpose)) ;; orient it for easy predator use
			(ecoservice-template (deep-copy ecoservice-template))
			(service-data-list (deep-copy service-data-list))
			(service-id-list (map (lambda (x) (list-head x 3)) service-data-list))
			(service-name-list (map car service-id-list))
			(service-symbol-list (map string->symbol service-name-list))
			(service-type-list (map cadr service-id-list))
			(service-eqn-sym-list (map caddr service-id-list))

			;; This is analogous to the "(service-list-index self sym)"
			;; call in <dynamic-patch>, but it relies on globals
			(service-index (lambda (sym) 
								  (let ((m (memq sym service-eqn-sym-list))
										  (n (memq sym service-name-list)))
									 
									 (if m (- (length service-eqn-sym-list)
												 (length m))
										  (if n (- (length service-eqn-sym-list)
													  (length n))
												#f)))))

			(pd (lambda (species)
					(let* ((species-list service-eqn-sym-list)
							 (as-prey-ratio
							  (list-sym-ref ((PM 'transpose))
												 service-eqn-sym-list species))
							 (as-predator-ratio
							  (list-sym-ref (PM) service-eqn-sym-list species))							 
							 (predator-efficiency 
							  (list-sym-ref (EM) service-eqn-sym-list species)) 
							 (cap (list-ref
									 (list-ref service-data-list
												  (service-index species)) 3))
							 (gr (list-ref
									(list-ref service-data-list
												 (service-index species)) 4))
							 (mort (list-ref
									  (list-ref service-data-list
													(service-index species)) 4))
							 
							 (pop-growth-func (list-ref
													 (list-ref service-data-list
																  (service-index species)) 6))
							 )

					  (let ((dP/dt
								(lambda (t . populations)
								  (pop-growth-func t species cap gr
														 as-predator-ratio predator-efficiency
														 mort as-prey-ratio populations)))
							  )
						 dP/dt))))

			(d/dt (map (lambda (species) (pd species))
						  (map caddr service-data-list)))
			)
	 (let ((population-structure
			  (lambda args
				 (cond
				  ((null? args)
					(abort "null  passed as an argument to a population structure"))
				  ((eq? (car args) 'template) 
					ecoservice-template)
				  ((eq? (car args) 'predation-matrix) 
					predation-matrix)
				  ((eq? (car args) 'efficiency-matrix) 
					(EM 'transpose)) ;; send it back in the same form we got it
				  ((memq (car args) '(service-data species-data))
					service-data-list)
				  ((memq (car args) '(service-ids species-ids))
					service-id-list)
				  ((memq (car args) '(service-names species-names))
					service-name-list)
				  ((memq (car args) '(service-symbols species-symbols))
					service-symbol-list)
				  ((memq (car args) '(service-types species-types))
					service-type-list)
				  ((memq (car args) '(service-eqn-syms species-eqn-syms))
					service-eqn-sym-list)
				  ((eq? (car args) 'd/dt-list)
					d/dt)
				  ((eq? (car args) 'index)
					(if (null? (cdr args))
						 #f
						 (if (pair? cddr)
							  (map service-index (cdr args))
							  (service-index (cadr args)))))
				  ))))
		population-structure)
	 ))



;; the predation matrix is oriented so that if we consider
;; grass-cow-leopard across the columns, the subdiagonal will be the
;; one with non-zero entries

;; the efficiency matrix is oriented the same way as the predation
;; matrix (on input).

;; non-sigmoidal-growth must either be absent, #f, growth function
;; with a form
;;
;;    (growth-func t population-level-list)
;;
;; where domain is the "time to cap" or some such thing, where P_0 is
;; the starting value, P is the current value, K is the capacity and r
;; is the "exponent" or a list of such functions (one for each species).
;; In practice, we probably ought to never get just a function.

;--- (make-population-structure predation-matrix efficiency-matrix
;                               service-data-list ecoservice-template)

;;; For examples look at savannah-parameters....

;-- <environment> methods and bodies

;; Stops things going off the rails 

;--- (services...) returns services matching the sym or in the symlist
(default-initialization <environment>)

(model-method (<environment>) (services self syms)
				  '())

(model-body <environment>
						(parent-body)
						dt)


;-- <ecoservice> methods and bodies

;; By convention we give ecoservices names which are strings, types
;; which are symbols ... neither needs to be unique
;; 
;; value, set-value!, add! scale!


(Comment " Ecoservices are able to update their state themselves.  If
they aren't *nested*, this may have irregular interactions with any
dynamics being forced on them from a dynamic-patch since the order of
insertion in the queue is not prescribed. The best way of dealing with
this situation would be to ensure that the timestep associated with
ecoservices is half (or less) of the timestep of the patch.
")


(default-initialization <ecoservice> 'do-growth #t 'history #f)

(define (simple-ecoservice N n V C r maxdt growing? growthmodel . P)
  (if (pair? P) (set! P (car P)))
  
  (make <ecoservice>
	 'name N
	 'sym n
	 'patch P
	 'value V
	 'capacity C
	 'r r
	 'delta-T-max maxdt
	 'do-growth growing?
	 'growth-model growthmodel)
	      ;;; 'sigmoid
	      ;;; 'linear
         ;;; (lambda (t dt v) ...)
		;; 
)


(model-method <ecoservice> (dump self . count)
				  (set! count (if (null? count) 0 (car count)))
				  (display (make-string count #\space))
				  (display "<ecoservice>\n")

				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (display (make-string (+ 2 count) #\space))
									 (display x)
									 (display ": ")
									 (display y)
									 (newline))
								  slots vals)))


;---- query & set
(model-method (<ecoservice> <symbol>) (service? self sym)
				  (or (eq? (my 'type) sym) (eq? (my 'name) sym)))

(model-method (<ecoservice> <pair>) (service? self symlist)
				  (or (memq (my 'type) symlist) (memq (my 'name) symlist)))

(model-method <ecoservice> (value self)
				  (my 'value))

(model-method <ecoservice> (capacity self)
				  (my 'capacity))

(model-method (<ecoservice>) (set-value! self val)
				  ;;(dnl* "Setting" (name self) "from" (my 'value) "to" val)
				  (set-my! 'value val))

(model-method <ecoservice> (growth-model self)
				  (my 'growth-model))

;--- adjustment

(model-method (<ecoservice>) (disable-growth! self) (set-my! 'do-growth #f))
(model-method (<ecoservice>) (enable-growth! self) (set-my! 'do-growth #t))

(model-method (<ecoservice>) (add! self val)
				  (let ((v (my 'value)))
					 (if (number? v)
						  (set-my! 'value (+ v val) )
						  (aborts "ecoservice:add!: value is not a number")
						  )))

(model-method (<ecoservice>) (scale! self val)
				  (let ((v (my 'value)))
					 (if (number? v)
						  (set-my! 'value (* v val) )
						  (aborts "ecoservice:scale!: value is not a number")
						  )))


;;--- ecoservice model-body support routines (mainly about growth)


(model-body <ecoservice>

						(kdnl* "[" (my 'name) ":" (class-name-of self) "]"
								 'model-bodies "In " (class-name-of self) t)

						;;;(dnl* 'b1)
						(let ((h (slot-ref self 'history)))
						  (if h
								(slot-set! self 'history
											  (cons (cons t (my 'value)) h)))
						  )
						;;;(dnl* 'b2)
						

						(if (my 'do-growth) ;; may be suppressed in
												  ;; dynamic-patches, for example
							 (begin
								;;;(dnl "Running <ecoservice> model body for "
								;;;     (my 'name))
								(let* ((capacity (my 'capacity))
										 (value  (my 'value))
										 (domain (my 'delta-T-max))
										 (ecoserv-growth (my 'growth-model)) 
										 ;; The growth-model expects the start
										 ;; of the time-step and and an
										 ;; interval. Some services should use
										 ;; the current _value_ to calculate a
										 ;; putative "time" from it's zero point
										 ;; and then generate the value for
										 ;; t+dt. Others may have more
										 ;; straightforward ways of calculating
										 ;; the value at the end of the timestep
										 ;; (such as a table, or a regular
										 ;; increment, for example).
										 (newvalue 
										  (cond
											((procedure? ecoserv-growth)
											 ;;;(dnl 'c1)
											 (growth-model t dt value))
											((eq? ecoserv-growth 'sigmoid)
											 ;;;(dnl 'c2)
											 (let* (;; This is a logistic update
													  (ipt (/ value capacity)) 
													  (pt (inverse-sigmoid* ipt))
													  (rdt (/ dt domain))
													  )
												(* capacity (sigmoid* (+ pt rdt)))))
											((eq? ecoserv-growth 'linear)
											 ;;;(dnl 'c3)
											 (min capacity (+ value (* dt (my 'r)))))
											(#t
											 ;;;(dnl 'c4)
											 value)))
										 )
								  ;;(dnl* (name (my 'patch)) "/" (my 'name)
								  ;;      "value =" value "| newvalue =" newvalue)
								  
								  (set-my! 'value  newvalue)
								  )
								))
						(parent-body)
						dt
						)


(model-method <ecoservice> (radius self)
				  (radius (my 'patch)))

(model-method (<ecoservice> <number>)(set-radius! self r)
				  (set-radius! (my 'patch) r))

(model-method <ecoservice> (location self)
				  (location (my 'patch)))

(model-method <ecoservice>
				  (log-data self logger format caller targets . args)
				  (let ((f (if (pair? args) (car args) #f))
						  (p (if (and (pair? args)
										  (pair? (cdr args)))
									(cadr args)
									#f)))
					 (let ((file (slot-ref logger 'file)))
						(kdnl* '(log-* log-ecoservice)
								 "[" (my 'name) ":" (class-name-of self) "]"
								 "in log-data")
						(let ((leading-entry #f))
						  (for-each
							(lambda (field)
							  (kdnl* '(log-* log-ecoservice) "[" (my 'name) ":"
										(class-name-of self) "]" "checking" field)
							  (if (has-slot? self field)
									(let ((r (slot-ref self field)))
									  (case format
										 ((ps)
										  (file 'show (string-append
															(if (string? r)
																 r
																 (object->string r)) " "))
										  )
;								 ((dump)
;								  (with-output-to-port file
;										(lambda ()
;										  (dump self))))

										 ((text table dump)
										  (let ((show-field-name
													(slot-ref logger 'show-field-name))
												  (missing-val
													(slot-ref logger 'missing-val))
												  )
											 (if show-field-name
												  (begin
													 (if leading-entry 
														  (display " " file)
														  (set! leading-entry #t))
													 (display field file)))
											 
											 (let ((val (if (eq? field 'name) 
																 (if (slot-ref self 'patch)
																	  (string-append
																		(slot-ref
																		 (slot-ref self 'patch)
																		 'name) ":" (name self))
																	  (name self))
																 (if (has-slot? self field)
																	  (slot-ref self field)
																	  (slot-ref logger
																					'missing-val)))))
												(if leading-entry 
													 (display " " file)
													 (set! leading-entry #t))
												(display val file))
											 )
										  )

										 (else
										  (kdnl* '(log-* log-ecoservice)
													"[" (my 'name) ":" (class-name-of self) "]"
													"Ignoring " field " because I don't have it")
										  'ignore-unhandled-format)))
									(begin
									  (kdnl* '(log-* log-ecoservice)
												"[" (my 'name) ":" (class-name-of self) "]"
												"no service" field)
									  #f)))
							(uniq (if #t
										 targets
										 (filter (not-memq (slot-ref logger 'dont-log))
													targets)))
							)
						  (newline file)
						  )
						))
				  )


;--- <circle>
(default-object-initialization <circle>)

(define (make-circle centre radius)
  (make <circle> 'locus centre 'radius radius))

(object-method (<circle>) (dump self . count)
				  (set! count (if (null? count) 0 (car count)))

				  (display (make-string count #\space))
				  (display "<circle>\n")
				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (begin
										(display (make-string (+ 2 count) #\space))
										(display x)
										(display ": ")
										(display y)
										(newline)))
					 slots vals))
				  )
				  

(object-method (<circle> <list>) (inside? self loc)
				  (<= (distance (my 'locus) loc) (my 'radius)))

(object-method (<circle>) (centre self)
				  (my 'locus))

(object-method (<circle>) (radius self)
				  (my 'radius))

(object-method (<circle>) (min-bound self)
				  (my 'radius))

(object-method (<circle>) (max-bound self)
				  (my 'radius))

(object-method (<circle> <list>) (distance-to-boundary self loc)
				  (if (inside? self loc)
						0
						(- (distance loc (my 'locus)) (my 'radius))))

;--- <polygon>
(default-object-initialization <polygon>)

(define (make-polygon centre polygon)
  (if (not (eq? (car polygon) (car (reverse polygon))))
		(set! polygon (append polygon (list (car polygon)))))
  (make <polygon> 'locus centre 'perimeter (copy-list polygon)))

(object-method (<polygon>) (dump self . count)
				  (set! count (if (null? count) 0 (car count)))

				  (display (make-string count #\space))
				  (display "<polygon>\n")
				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (begin
										(display (make-string (+ 2 count) #\space))
										(display x)
										(display ": ")
										(display y)
										(newline)))
					 slots vals))
				  )
(object-method (<polygon> <list>) (inside? self loc)
				  (point-in-polygon loc (my 'perimeter)))


(object-method (<polygon>) (radius self)
				  (max-bound self)
				  )

(object-method (<polygon> <list>) (distance-to-boundary self loc)
				  (if (inside? self loc)
						0
						(distance-to-boundary loc (my 'perimeter))))

(object-method (<polygon>) (min-bound self)
				  (let ((c (my 'locus))
						  (p (my 'polygon)))
					 
					 (let loop ((r (distance c (car p)))
									(l (cdr p)))
						(if (null? l)
							 r
							 (loop ((min r (distance c (car p)))
									  (cdr p)))))))

(object-method (<polygon>) (max-bound self)
				  (let ((c (my 'locus))
						  (p (my 'polygon)))
					 
					 (let loop ((r (distance c (car p)))
									(l (cdr p)))
						(if (null? l)
							 r
							 (loop ((max r (distance c (car p)))
									  (cdr p)))))))


;-- <boundary> -> <circle>, <polygon>

;-- <boundary> methods and bodies

(default-object-initialization <boundary> 'rep #f)

(define (make-boundary rep centre arg)
  (let ((M (make <boundary>)))
	 (case rep
		((circle)
		 (slot-set! M 'rep (make <circle> 'locus centre 'radius arg))
		 )

		((polygon absolute-polygon)
		 (slot-set! M 'rep (make <polygon> 'locus centre
										 'perimeter (copy-list arg)))
		 )

		((relative-polygon)
		 (slot-set! M 'rep (make <polygon> 'locus centre
										 'perimeter (translate-trace centre arg)))
		 )

		(else (error "Bad representation specified for a <boundary>" rep)))))

(object-method (<boundary>) (dump self . count)
				  (set! count (if (null? count) 0 (car count)))

				  (display (make-string count #\space))
				  (display "<boundary>\n")
				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y)
									 (begin
										(display (make-string (+ 2 count) #\space))
										(display x)
										(display ": ")
										(display y)
										(newline)))
					 slots vals))
				  (display (make-string (+ 2 count) #\space))
				  (display "rep:\n")
				  (dump (my 'rep))
				  )

(object-method (<boundary>) (rep self)
				  (my 'rep))

(object-method (<boundary> <list>) (inside? self loc)
				  (inside? (my 'rep) loc))

;; 
;; min-bound, max-bound contains? services 

(object-method (<boundary>) (centre self)
				  (centre (my 'rep) ))

(object-method (<boundary> <list>) (set-centre! self c)
				  (slot-set! (my 'rep) c))

(object-method (<boundary>) (min-bound self)
				  (min-bound (my 'rep) ))

(object-method (<boundary>) (max-bound self)
				  (max-bound (my 'rep) ))

(object-method (<boundary> <list>) (distance-to-boundary self loc)
				  (distance-to-boundary (my 'rep) loc))

;-- <patch> methods and bodies
;; 
;; min-bound, max-bound contains? services 



;--- (initialize...) 
(default-initialization <patch>)

(model-method <patch> (dump self . count)
				  (set! count (if (null? count) 0 (car count)))

				  (display (make-string count #\space))
				  (display "<patch>\n")
				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (if (not (memq x '(boundary service-list)))
										  (begin
											 (display (make-string (+ 2 count) #\space))
											 (display x)
											 (display ": ")
											 (display y)
											 (newline))))
								  slots vals))
				  (display (make-string (+ 2 count) #\space))
				  (display "boundary and service lists:\n")
				  (for-each (lambda (x) (dump x (+ 4 count)))
								(cons (my 'rep) (my 'service-list)))
				  )

(model-method (<patch> <boundary> <list>) (install-boundary self bdry centre)
				  (set-my! 'rep bdry)
				  (slot-set! bdry 'locus centre)
				  )

(model-method <patch> (location self)
				  (slot-ref (my 'rep) 'locus))

;--- (service?...) queries if a service is present
(model-method (<patch> <symbol>) (service? self sym)
				  (not (null? (services self sym))))

(model-method (<patch> <list>) (service? self symlist)
				  (not (null? (services self symlist))))


;--- (set-services!...) sets the value of the services list
(model-method (<patch> <list>) (set-services! self servlist)
				  (set-my! 'service-list servlist))


;--- (add-service...) adds a service to a patch

(model-method (<patch> <ecoservice>) (add-service self new-service)
				  (set-services! self (append (my 'service-list)
														(list new-service))))


;--- (remove-service...) removes all services that match the predicate
;                        in a patch
;;                       the predicate will probably be something like
;;                       (using-name-keep? 'wobble)
(model-method (<patch> <procedure>) (remove-service self predicate)
				  (set-services! self (filter predicate (my 'service-list))))



;--- (distance-to-centre...) returns the distance to the centre of the patch
(model-method (<patch> <list>) (distance-to-centre self loc)
				  (let ((sqr (lambda (x) (* x x))))
					 (sqrt (apply + (map sqr (map - (list-head
																(slot-ref (my 'rep) 'locus) 2)
															(list-head loc 2)))))))

;--- (distance-to-interior...) returns the distance to the boundary of
;the patch (more expensive than the dist to centre)
(model-method (<patch> <list>) (distance-to-interior self loc)
				  (let* ((sqr (lambda (x) (* x x)))
							(R (- (sqrt (apply
											 + (map sqr
													  (map -
															 (list-head
															  (slot-ref (my 'rep) 'locus) 2)
															 (list-head loc 2)))))
									(my 'radius)))
							)
					 (if (< R 0) 0 R)))

;--- (contains?...) predicate to indicate if something is in the patch
(model-method <patch> (contains? self . bit)
				  (if (null? bit) 
						(abort "Missing argument to contains?")
						(set! bit (car bit)))
				  (cond
					((pair? bit)
					 (let ((R (distance-to-interior self bit)))
						(zero? R)))
					((isa? bit <thing>)
					 (let ((R (distance-to-interior self (location bit))))
						(zero? R)))
					(else #f)))

;;(model-method (<patch> <thing>) (contains? self entity)
;;				  (contains? (location entity))
;;				  )


;--- (services...) returns services matching the sym or in the symlist

(model-method (<patch>) (service-list self . ss)
				  (if (and (pair? ss) (pair? (car ss))) (set! ss (car ss)))
				  (let ((S (my 'service-list)))
					 (if (null? ss)
						  S
						  (filter
							(lambda (x) (or (member (type x) ss) (member (name x) ss)))
							S))))


(model-method (<patch>) (services self . ss)
				  (if (null? ss)
						(map (lambda (x) (type x)) (my 'service-list))
						(map type (apply service-list (cons self ss)))))


(model-method (<patch>) (specific-services self . ss)
				  (if (null? ss)
						(map (lambda (x) (name x)) (my 'service-list))
						(map type (apply service-list (cons self ss)))))


(model-method (<patch> <symbol>) (value self servlist)
				  (set! servlist (list servlist))
				  (let ((sl (if (member #t servlist)
									 (my 'service-list)
									 (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map value sl)))))

(model-method (<patch> <string>) (value self servlist)
				  (set! servlist (list servlist))
				  (let ((sl (if (member #t servlist)
									 (my 'service-list)
									 (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map value sl)))))

(model-method (<patch> <symbol>)(extra-variable self field)
				  (value self (symbol->string field)))

(model-method (<patch> <string>)(extra-variable self field)
				  (value self field))

(model-method (<patch>) (extra-variable-list self)
				  (map string->symbol (map name (my 'service-list))))

;; (add-method representation
;; 				(make-method (list <agent>)
;; 								 (lambda (representation-parent self)
;; 									(my 'representation))))



(model-method (<patch> <pair>) (value self servlist)
				  (let ((sl (if (member #t servlist)
									 (my 'service-list)
									 (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map value sl)))))

(model-method (<patch> <pair>) (capacity self servlist)
				  (let ((sl (if (member #t servlist)
									 (my 'service-list)
									 (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map capacity sl)))))

(model-method (<patch> <pair>) (mean-value self servlist)
				  (let ((sl (service-list self servlist)))
					 (if (null? sl)
						  0
						  (/ (apply + (map value sl))
							  (* 1.0 (length servlist))))))

;;;--- (set-value!...)
(model-method (<patch> <symbol>) (set-value! self sym val)
				  (let ((s (filter (lambda (a) (eq? sym (type a)))
								(my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (set-value! x val)) s)
							 #t))))

(model-method (<patch> <string>) (set-value! self sym val)
				  (let ((s (filter (lambda (a) (string=? sym  (name a)))
										 (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (set-value! x val)) s)
							 #t))))

;--- (scale!...)
(model-method (<patch> <symbol> <number>) (scale! self sym val)
				  (let ((s (filter (lambda (a) (eq? (type a) sym))
										 (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (scale! x val)) s)
							 #t))))

(model-method (<patch> <string> <number>) (scale! self sym val)
				  (let ((s (filter (lambda (a) (string=? (name a) sym))
										 (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (scale! x val)) s)
							 #t))))

;--- (add!...)
(model-method (<patch> <symbol> <number>) (add! self sym val)
				  (let ((s (filter (lambda (a) (eq? (type a) sym))
										 (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (add! x val)) s)
							 #t))))

(model-method (<patch> <string> <number>) (add! self sym val)
				  (let ((s (filter (lambda (a) (string=? (name a) sym))
										 (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (add! x val)) s)
							 #t))))

;--- (scale!...)
(model-method (<patch> <pair> <number>) (scale! self symlist val)
				  (for-each (lambda (x) (scale! self x val)) symlist))


;--- (add!...)
(model-method (<patch> <pair> <number>) (add! self sym val)
				  (for-each (lambda (x) (add! self x val)) symlist))


;--- (total-value ...) ;; needs to filter the services by membership
;in the indicated symlist
(model-method (<patch> <pair>) (total-value self symlist)
				  (let ((ss (service-list self (if (symbol? symlist)
															  (list symlist)
															  symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (value y)) ss)))))

(model-method (<patch> <pair>) (total-value self symlist)
				  (let ((ss (service-list self (if (symbol? symlist)
															  (list symlist)
															  symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (value y)) ss)))))

;--- model-method (<patch> <pair>) (total-capacity self symlist)
(model-method (<patch> <pair>) (total-capacity self symlist)
				  (let ((ss (service-list self (if (symbol? symlist)
															  (list symlist)
															  symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (capacity y)) ss)))))

(model-method (<patch> <pair>) (total-capacity self symlist)
				  (let ((ss (service-list self (if (symbol? symlist)
															  (list symlist)
															  symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (capacity y)) ss)))))

;--- model-body <patch>
(model-body <patch>
						(kdnl* 'model-bodies "In " (class-name-of self)
								 (name self) "@" t)

						;; this does the growth  and endemic mortality
						(if (member 'nested-habitat nested-agents)
							 (for-each (lambda (x)
											 (run x t (+ t dt) (my 'kernel))
											 )	
										  (service-list self)))
						(kdnl* 'nested-habitat (name self) "@" t "/"
								 (subjective-time self) ":" dt "/" (my 'dt))
						(parent-body)
						(kdnl* 'nested-habitat (name self) "@" t "/"
								 (subjective-time self) ":" dt "/" (my 'dt))
						dt
						)

;--- model-method (<patch> <agent> <symbol> <agent>) (log-data self logger format caller targets)
(model-method (<patch> <agent> <symbol> <agent>)
				  (log-data self logger format caller targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'map-projection)))
					 (if (or (not p) (null? p)) (set! p (lambda (x) x)))
					 (kdnl* '(log-* log-patch) "[" (my 'name) ":"
							  (class-name-of self) "]" "in log-data")
					 
					 (case format
						((ps)
						 (let* ((symlist (services h))
								  (name (slot-ref h 'name))
								  )
							
							(if adjust-grey (file 'setgray patchgrey))
							(ps-circle file  (p (radius self))
										  (p (list-head (location self) 2)) 0.7 0.0)
							
							
							(let* ((slist (slot-ref self 'service-list))
									 (n (+ 1 (length slist))) ;; slist is
																	  ;; becoming
																	  ;; circular??
																	  ;; ....******
									 (ns (length slist))
									 (loc (location self))
									 (rad (radius self))
									 (mm-xoffset 2)
									 (mm-yoffset 2)
									 )
							  (file 'moveto (map p (map + 
																 (list-head loc 2) 
																 (list (+ mm-xoffset
																			 (* 1.05 rad))
																		 (+ mm-yoffset
																			 (/ ns 2.0)))
																 )))
							  (file 'show-table (map
														(lambda (x) (string-append
																		 (slot-ref x 'name)
																		 ": " (pno (value x))))
														slist))
							  )
							(crop-caption file p self)
							)
						 )

						;;((text table dump)
						;; (log-data-parent)
						;; )
						(else
						 (display (my 'name) file)
						 (for-each 
						  (lambda (x)
							 (display " " file)
							 (display (value x)))
						  (map (lambda (x) (value x)) (my 'service-list))
						  (newline file))
						 ;;(log-data-parent)	
						 )
						)
					 )
				  )


;-- dynamic-patch methods and body

;--- <dynamic-patch> (initialize  (initialize-parent self args)
(add-method initialize
				(make-method (list <dynamic-patch>)
								 (lambda (initialize-parent self args)
									(initialise self (list 'population-names '()
																  'population-symbols '()
																  'd/dt-list '()
																  'do-dynamics #t
																  'population-definitions '()	
																  'subdivisions 12
																  ;; 'cause 12 is a nice number?
																  ))
									(let ((population-definitions
											 (slot-ref self 'population-definitions))
											(population-names
											 (slot-ref self 'population-names))
											(population-symbols
											 (slot-ref self 'population-symbols))
											(d/dt-list (slot-ref self 'd/dt-list)))

									  (if (or (pair? population-definitions)
												 (pair? population-names)
												 (pair? population-symbols)
												 (pair? d/dt-list))
											(begin
											  (if (and (pair? population-definitions)
														  (or (pair? d/dt-list)
																(pair? population-names)
																(pair? population-symbols)))
													(abort (string-append
															  "Dynamic patch specified a "
															  "population-definition *and* "
															  "one or more of\n"
															  "population-names "
															  "population-symbols "
															  "d/dt-list\n")))
											  
											  (if (and (null? population-definitions)
														  (not (and (pair? d/dt-list)
																		(pair? population-names)
																		(pair? population-symbols))))
													(abort (string-append
															  "Dynamic patch specified at "
															  "least one of population-names "
															  "population-symbols d/dt-list\n"
															  "but not all three."
															  )))

											  ))
									  (cond
										((pair? population-definitions)
										 (define-population-dynamics! self
											population-definitions))
										((pair? population-names)
										 (define-population-dynamics! self
											population-names
											population-symbols
											d/dt-list))
										(#t (slot-set! self 'population-definitions #f)
											 (slot-set! self 'population-names #f)
											 (slot-set! self 'population-symbols #f)
											 (slot-set! self 'd/dt-list #f)))
									  )
									(initialize-parent) ;; call "parents" last
															  ;; to make the
															  ;; initialisation list
															  ;; work
									(initialise self args)
									)))




(define (bbox ll ur)
  (list ll (list (car ur) (cadr ll)) ur (list (car ll) (cadr ur)) ll))

(define unitbox '((0 0) (1 0) (1 1) (0 1) (0 0)))

(define (%patch-initialiser clss bdry name type centre radius box . therest)
  (if (memq clss (list <patch> <dynamic-patch>))
		(append (list clss)
				  (cond
					((eq? bdry <circle>)
					 (list 'rep (make <circle> 'name name 'type type '
											locus centre 'radius radius)
							 ))
					((eq? bdry <polygon>)
					 (list 'rep (make <polygon> 'name name 'type type
											'locus centre 'point-list (copy-list box))
							 ))
					(#t
					 (error "bad boundary class specified in patch-initialiser"
							  bdry)))
				  therest)
		(error (string-append "patches may only be initialised as <patch> or "
				 "<dynamic-patch> agents")
				 clss)
		))
  


;; make-grid is geared to making patches and dynamic-patches in a regular array
;; n m is the size of the grid, ll up are the ordinates of the ll cell and the ur cell
;; bdry should be <circle> or <polygon>, clss should be <patch> or <dynamic-patch>,
;; clss-initialiser is a function that returns a fully formed initialiser list
;; (apart from the centre and radius or perimeter), and P should be either null
;; or a <habitat> like class.
(define (make-grid n m ll ur clss bdry name type clss-initialiser . therest)
  (let* ((nscale (real->integer (/ (- (car ur) (car ll)) (* 1.0 n))))
			(mscale (real->integer (/ (- (cadr ur) (cadr ll)) (* 1.0 n))))
			(radius (min nscale mscale))
			(M (make-list* n m))
			(patchlist '())
			(init-class (lambda  x
							  (apply clss-initialiser (append (list clss bdry) x))))
			)
	 (map-** (lambda (x i)
				  (let ((centre (list (+ (car ll) (* nscale (+ 0.5 (car i))))
												 (+ (cadr ll) (* mscale (+ 0.5 (cadr i))))))
						  (box (bbox (list (+ (car ll) (* nscale (car i)))
												 (+ (cadr ll) (* mscale (cadr i))))
										 (list (+ (car ll) (* nscale (+ 1 (car i))))
												 (+ (cadr ll) (* mscale (+ 1 (cadr i)))))))
						  (pname (string-append name "-" (number->string (car i)) ","
														(number->string (cadr i))))
						  )
					 (let ((np (apply make (apply init-class
															(append (list pname type centre
																			  radius box)
																	  therest)))))
						(slot-set! np 'name pname)
						np)
					 ))
				M)))


;; the services are defined by a list containing a name, a symbol, an
;; initial value, a capacity, its max dt, whether or not it grows, and
;; a growth model
(define (populate-patch p services . therest)
  (for-each (lambda (x)
				  (cond
					((agent? x)	(add-service p x))
					((list? x)	(add-service p (apply make (append (list <ecoservice>)
																				  x therest))))
					(#t (error (string-append "populate-patch should be a list of "
													  "patches, arguments  to "
													  "(make <ecoservice> ....) or both"
													  services))))
				services))


;;(define (patchsize domain) (* 0.25 (apply min (map - (list-head
;;  (cadr domain) 2) (list-head (car domain) 2)))))



;--- (make-patch services centre representation repspec . args)
(Comment "services should be a list of the form '((name type capacity) ...)
args can be  an update map or an update map and update equations 
   -- see old-model-version1/Model-configuration.scm
")

;--- model-method (<dynamic-patch> <string>) (service-list-index self service)
(model-method (<dynamic-patch> <string>) (service-list-index self service)
				  (let* ((si (my 'population-names))
							(n (length si))
							(ix (member service si))
							(i (if ix  (- n (length ix)) #f)))
					 i))

;--- model-method (<dynamic-patch> <symbol>) (service-list-index self service)
(model-method (<dynamic-patch> <symbol>) (service-list-index self service)
				  (let* ((si (my 'population-symbols))
							(n (length si))
							(ix (member service si))
							(i (if ix  (- n (length ix)) #f)))
					 i))

;--- model-method (<dynamic-patch> <pair>) (service-list-index self service)
(model-method (<dynamic-patch> <pair>) (service-list-index self service)
				  (map (lambda (x) (service-list-index self x)) service))


;; for predation matrix stuff ...
;--- model-method (<dynamic-patch> <symbol>) (service-matrix-index self service)
(model-method (<dynamic-patch> <symbol>) (service-matrix-index self service)
				  (let ((si (service-list-index self service)))
					 (if si (+ 1 si) si)))

;--- model-method (<dynamic-patch> <pair>) (service-matrix-index self service)
(model-method (<dynamic-patch> <pair>) (service-matrix-index self service)
				  (map (lambda (x) (service-matrix-index x)) service))

;--- model-method (<dynamic-patch> <pair>) (service-values self)
(model-method (<dynamic-patch> <pair>) (service-values self)
				  (map (lambda (x) (value self x)) (my 'service-update-map)))	

;--- model-method <dynamic-patch> (dump self . count)
(model-method <dynamic-patch> (dump self . count)
				  (set! count (if (null? count) 0 (car count)))

				  (display (make-string count #\space))
				  (display "<dynamic-patch>\n")
				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (if (not (eq? x 'service-list))
										  (begin
											 (display (make-string (+ 2 count) #\space))
											 (display x)
											 (display ": ")
											 (display y)
											 (newline))))
								  slots vals))
				  (display (make-string (+ 2 count) #\space))
				  (display 'service-list)
				  (display ":\n")
				  (for-each (lambda (x) (dump x (+ 4 count))) (my 'service-list))
				  )

;--- model-method (<dynamic-patch> <procedure> <symbol>
;                           <procedure>)(log-data self logger format caller targets)
(model-method (<dynamic-patch>  <procedure> <symbol> <procedure>)
				  (log-data self logger format caller targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'map-projection)))
					 (if (or (not p) (null? p))  (set! p (lambda (x) x)))
					 (kdnl* '(log-* log-patch) "[" (my 'name) ":"
							  (class-name-of self) "]" "in log-data")
					 
					 (case format
						((ps)
						 (let* ((symlist (services h))
								  (name (slot-ref h 'name))
								  )

							(if adjust-grey (file 'setgray patchgrey))
							(ps-circle file  (p (radius self))
										  (p (list-head (location self) 2)) 0.7 0.0)


							(let* ((slist (slot-ref self 'service-list))
									 (n (+ 1 (length slist))) ;; slist is becoming circular?? ....******
									 (ns (length slist))
									 (loc (location self))
									 (rad (radius self))
									 (mm-xoffset 2)
									 (mm-yoffset 2)
									 )
							  (file 'moveto (map p (map + 
																 (list-head loc 2) 
																 (list (+ mm-xoffset (* 1.05 rad))
																		 (+ mm-yoffset (/ ns 2.0)))
																 )))
							  (file 'show-table
									  (map
										(lambda (x) (string-append
														 (slot-ref x 'name)
														 ": "
														 (pno (value x))))
										slist))
							  )
							(crop-caption file p self)
							)
						 )

						;;((text table dump)
						;; (log-data-parent)
						;; )
						(else
						 (log-data-parent))
						)
					 )
				  )

;--- model-method <dynamic-patch> (enable-service-growth! self service-name)
(model-method <dynamic-patch> (enable-service-growth! self service-name)
				  (enable-growth! (service self service-name)))

;--- model-method <dynamic-patch> (disable-service-growth! self)
(model-method <dynamic-patch> (disable-service-growth! self)
				  (disable-growth! (service self service-name)))


;--- model-method <dynamic-patch> (enable-all-service-growth! self)
(model-method <dynamic-patch> (enable-all-service-growth! self)
				  (for-each enable-growth! (service-list self)))

;--- model-method <dynamic-patch> (disable-all-service-growth! self)
(model-method <dynamic-patch> (disable-all-service-growth! self)
				  (for-each disable-growth! (service-list self)))

;--- model-method <dynamic-patch> (enable-growth! self)
(model-method <dynamic-patch> (enable-growth! self) (set-my! 'do-dynamics #t))
;--- model-method <dynamic-patch> (disable-growth! self)
(model-method <dynamic-patch> (disable-growth! self) (set-my! 'do-dynamics #f))

;--- model-method <dynamic-patch> (growth-model self)
(model-method <dynamic-patch> (growth-model self) (slot-ref self 'd/dt-list))


;; this expects a list of functions which return reals
;--- model-method <dynamic-patch> (set-population-dynamics! self . d/dt-list)
(model-method <dynamic-patch> (set-population-dynamics! self . d/dt-list)
				  (slot-set! self 'population-definitions #f)
				  
				  (if (null? d/dt-list)
						(abort!))

				  (if (pair? (car d/dt-list))
						(set! d/dt-list (car d/dt-list))) ;; a *list* of
																	 ;; functions was
																	 ;; passed in the
																	 ;; "rest" part of
																	 ;; the line
				  
				  (if (andf (map procedure? d/dt-list))
						(slot-set! self 'd/dt-list d/dt-list)
						(abort!))
				  )

;--- model-method (<dynamic-patch>) (define-population-dynamics! self pn ps pf)
(model-method (<dynamic-patch>) (define-population-dynamics! self pn ps pf)
				  (if (not (and (list? pn) (list? ps) (list? pf) (= (length pn)
																					 (length ps)
																					 (length pf))))
						(abort (string-append "define-population-dynamics! was passed"
													 " lists of differing lenghth!")))
				  (let ((tpn (apply andf (map string? pn)))
						  (tps (apply andf (map symbol? ps)))
						  (tpf (apply andf (map procedure? pf))))
					 (if (not (and  tpn tps tpf))
						  (begin
							 (display (string-append "There was at least one erroneous "
															 "argument passed to define-population-dynamics!\n"))
							 (if (not tpn)
								  (let ((culprits (!filter string? pn)))
									 (dnl "The following members of the name list should be strings:")
									 (apply dnl* (cons "   " culprits)))
								  )
							 (if (not tps)
								  (let ((culprits (!filter symbol? ps)))
									 (dnl "The following members of the symbol list should be symbols:")
									 (apply dnl* (cons "   " culprits)))
								  )
							 (if (not tpf)
								  (let ((culprits (!filter procedure? pf)))
									 (dnl "The following members of the d/dt list should be functions:")
									 (apply dnl* (cons "   " culprits)))
								  )
							 )))
				  (slot-set! self 'population-names pn)
				  (slot-set! self 'population-names ps)
				  (slot-set! self 'population-names pf)

				  (slot-set! self 'population-definitions (map list n pn ps pf))

				  )



;--- model-method (<dynamic-patch>) (define-population-dynamics! self . defns )
(model-method (<dynamic-patch>) (define-population-dynamics! self . defns )
				  (if (and (pair? defns)
							  (pair? (car defns))
							  (pair? (caar defns))
							  (null? (cdr defns))) ;; passed a list
						(set! defns (car defns)))

				  
				  (if (and (apply andf (map (lambda (x) (= (length x) 3))
													 defns))
							  (>= (length defns) 1)
							  )
						(begin
						  (slot-set! self 'population-definitions defns)
						  
						  (let ((pn (map car defns))
								  (ps (map cadr defns))
								  (pf (map caddr defns)))
							 (slot-set! self 'population-names pn)
							 (slot-set! self 'population-symbols ps)
							 (slot-set! self 'd/dt-list pf))
						  )
						(abort
						 (string-append
						  " the format for defining a system of "
						  "populations is:\n"
						  "  (define-population-dynamics! self \n"
						  "     ("Grass" grass dgrass/dt)\n"
						  "     ("Rabbit" rabbit drabbit/dt)\n"
						  "     ("Fox" fox dfox/dt)\n"
						  "     ("Bear" bear dbear/dt))"
						  "where the arguments to each of the d/dt "
						  "are t grass rabbit fox bear"
						  )
						 )
						)))




;---  model-body <dynamic-patch
(model-body <dynamic-patch>
						(kdnl* 'model-bodies "In " (class-name-of self) t)
 						;; Ok, I need to be able to refer to service
						;; directly (names) and to classes (types). Type
						;; values are aggregates of the members of the
						;; service-list of that type excluding any of those
						;; members specified by name members.

						;; We can tell the difference because names are
						;; required to be strings and types are required to
						;; be symbols.

						;; Changes in a type value are implemented pro-rata.

						;;(dnl "Running <dynamic-patch> model body")

						(if (<= dt 1e-12)
							 (abort
							  "Bad dt passed to <dynamic-patch> model-body"))
						(let ((pop-values (map (lambda (x) (value self x))
													  (my 'population-names)))
								(d/dt-list (my 'd/dt-list))
								)

						  (if (not (null? pop-values))
								(let ((dP (if (not (and d/dt-list (pair? d/dt-list)))
												  (lambda args 0)
												  (rk4* d/dt-list
														  t
														  (+ t dt)
														  (/ dt (my 'subdivisions))
														  pop-values)))
										)
								  ;;(dnl "Got past the rk4* for " (my 'name) "
								  ;;and " (my 'population-names))

								  ;;(set-my! 'dP dP) ;; Don't really *need*
								  ;;this, except perhaps for debugging
								  (let ((deltas (dP (+ t dt))))
									 (for-each 
									  (lambda (x v)
										 (if (not (zero? (imag-part v)))
											  (abort "got complex number, wanted a real"))

										 ;;(dnl (class-name-of self))
										 

										 (add! self x (- v (value self x)))
										 ;; These are the adjustments due to
										 ;; consumption and predation (slot-set!
										 ;; self x v)
										 (if (< (value self x) 0.0)
											  (set-value! self x 0.0))
										 )
									  (my 'population-names) deltas))
								  )

								(aborts (symbol->string (class-name-of self))
										  " has no population names defined!"))
						  )

						(kdnl* 'nested-habitat (name self) "@" t "/"
								 (subjective-time self) ":" dt "/" (my 'dt))
						(parent-body)
						(kdnl* 'nested-habitat (name self) "@" t "/"
								 (subjective-time self) ":" dt "/" (my 'dt))
						dt)


;-- <landscape> methods and body

;;			 (filter (lambda (x) (and (contains? x loc) arg) ) (my 'patch-list)))



;; Default landscape only has the default value, oddly enough
(model-method (<landscape> <pair>) (value self loc)
				  (if (contains? self loc)
						((my 'terrain-function) loc)
						(my 'default-value)))

(model-method (<landscape> <pair>) (capacity self loc)
				  (if (contains? self loc)
						((my 'terrain-function) loc)
						(my 'default-value)))

;; This is to keep the "run" chain consistent
(model-body <landscape>
						(kdnl* 'model-bodies "In " (class-name-of self) t)
						(kdnl* 'nested-habitat (name self) "@" t "/"
								 (subjective-time self) ":" dt "/" (my 'dt))
						(parent-body)
						(kdnl* 'nested-habitat (name self) "@" t "/"
								 (subjective-time self) ":" dt "/" (my 'dt))
						dt
						)



;;(model-body <landscape> 
;;						(kdnl* 'running (my 'name) ":" (my 'representation) " is running")
;;						(for-each (lambda (x)
;;										(run-model-body x t dt))
;;									 (my 'patch-list))
;;						(parent-body)
;;						dt)



;-- <habitat> methods and body

;---- (make-habitat name default-ht domain terrain-function 
;; domain is a list  ((minx miny minz) (maxx maxy maxz))
;; patch-data is a list of  services lists for make-patch

(define (make-habitat name default-ht domain terrain-function 
							 patch-list
							 )
  (let* ((H (make <habitat> 'name name 'default-value default-ht
						'minv (car domain) 'maxv (cadr domain)
						'terrain-function terrain-function
						'patch-list patch-list))
			)
	 H)
  )


;--- Add things to the runqueue

;---- (add-habitat-to-queue Q h)
(define (add-habitat-to-queue Q h) ;; returns the queue, so us it like
											  ;; (set! Q (add-...-queue Q hab))
  (let ((p (patch-list h))
		  (s (service-list h))
		  )
	 (uniq (append Q (list h) p s))))



;--- model-method <habitat> (agent-prep self . args) Set preconditions
;                                                    for running
(model-method <habitat> (agent-prep self . args)
				  (agent-prep-parent)
				  )


;--- initialize <habitat> (initialize-parent self args) -- make a new habitat
(add-method initialize
				(make-method (list <habitat>)
								 (lambda (initialize-parent self args)
									(initialise self (list 'scale #f))
									(initialize-parent) ;; call "parents" last
															  ;; to make the
															  ;; initialisation list
															  ;; work
									(initialise self args)
									)))

;--- model-method <habitat> (dump self . count) dumps the state of the habitat
;                                               agent in a readable way
(model-method <habitat> (dump self . count)
				  (set! count (if (null? count) 0 (car count)))
				  (display (make-string count #\space))
				  (display "<habitat>\n")

				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (if (not (eq? x 'patch-list))
										  (begin
											 (display (make-string (+ 2 count) #\space))
											 (display x)
											 (display ": ")
											 (display y)
											 (newline))))
								  slots vals))
				  (display (make-string (+ 2 count) #\space))
				  (display 'patch-list)
				  (display ":\n")
				  (for-each (lambda (x) (dump x (+ 4 count))) (my 'patch-list)))


;--- model-method (<habitat> <patch>) (add-patch self patch) add a
;                                                            patch to the habitat
(model-method (<habitat> <patch>) (add-patch self patch)
				  (set-my! 'patch-list (uniq (cons patch (my 'patch-list)))))

;--- model-method (<habitat> <procedure>) (remove-patch self pfilter)
;  keep only patches which match a filter
(model-method (<habitat> <procedure>) (remove-patch self pfilter)
				  (set-my! 'patch-list (filter pfilter (my 'patch-list))))

;--- (services...) returns services matching the sym or in the symlist
(model-method <habitat>
				  (services self . ss)
				  (if (not (null? ss))
						(begin
						  (if (not (symbol? (car ss))) (set! ss (car ss)))
						  (filter (lambda (x) (member x ss)) (services self))
						  )
						(unique
						 (map string->symbol 
								(sort (map symbol->string 
											  (apply append
														(map services 
															  (patch-list self))))
										string<?)))
						))

;--- model-method (<habitat>) (service-list self . ss)
;; returns the slist of provided services: ss is an optional list of
;; patch names/symbols
(model-method (<habitat>)
				  (service-list self . ss)
				  (uniq
					(if (null? ss) 
						 (apply append (map (lambda (x)
													 (service-list x))
												  (patch-list self)))
						 (apply append 
								  (map (lambda (x) 
											(service-list x 
															  (if (symbol? (car ss))
																	ss
																	(car ss))))
										 (patch-list self))))))


;--- model-method (<habitat>) (service-list self) ret a list of all ecoservices
(model-method (<habitat>) (service-list self)
				  (let* ((P (patch-list self))
							(S (map service-list P)))
					 (apply append S)))


;--- (service?...) queries if a service is present
(model-method (<habitat> <symbol>) (service? self sym)
				  (not (null? (services self sym))))


(model-method (<habitat> <pair>) (service? self symlist)
				  (not (null? (services self symlist))))


;--- model-method (<habitat> <symbol>) (service-sites self sym)
; returns a list of patches with services
(model-method (<habitat> <symbol>) (service-sites self sym)
				  (let loop ((rslt '())
								 (pl (my 'patch-list)))
					 (cond
					  ((null? pl) rslt)
					  ((service? (car pl) sym)
						(loop (cons (car pl) rslt) (cdr pl)))
					  (else (loop rslt (cdr pl))))))


(model-method (<habitat> <pair>) (service-sites self symlist)
				  (let loop ((rslt '())
								 (pl (my 'patch-list)))
					 (cond
					  ((null? pl) rslt)
					  ((service? (car pl) symlist)
						(loop (cons (car pl) rslt) (cdr pl)))
					  (else (loop rslt (cdr pl))))))




;--- model-method (<habitat>) (patch-list self . arg) 
; returns the listof patches (possibly filtered by names, symbols,
; procedures....
(model-method (<habitat>) (patch-list self . arg)
				  (cond
					((null? arg)
					 (my 'patch-list))
					((and arg (symbol? (car arg)))
					 (let ((symlist arg))
						(filter (lambda (p) 
									 (let ((s (services p symlist)))
										(and s (not (null? s)))))
								  (my 'patch-list))))
					((and arg (pair? (car arg)))
					 (let ((symlist (car arg)))
						(filter (lambda (p) 
									 (let ((s (services p symlist)))
										(and s (not (null? s)))))
								  (my 'patch-list))))
					((and arg (procedure? (car arg)))
					 (let ((pfilter (car arg)))
						(filter pfilter (my 'patch-list))))
					(else (my 'patch-list))))


;--- (aggregate-value self location radius servicelist) 

(model-method (<habitat> <pair> <number> <pair>) (aggregate-value self location radius servicelist)
 (let* ((sl (service-sites self servicelist))
		  (lsl (filter
				  (lambda (patch) 
					 (> (intersection-of-two-circles 
							(distance-to-centre patch location)
							radius (slot-ref patch 'radius))
						 0.0))
				  sl))
		  (lslv (if (null? lsl) 
						0.0
						(apply + (map (lambda (patch)
											 (* (value patch servicelist)
												 (overlap-decay
												  0.0 ;; 1.0 gives us 1% of the
														;; pop at the radius, 0
														;; gives us a uniform dist
												  (distance (list-head (centre patch) 2)
																location)
												  radius 
												  #t
												  0.0
												  (slot-ref patch 'radius)	
												  #t)
												 ))
										  lsl))))
		  )
	lslv))


;--- model-method <habitat> (min-bound self)
(model-method <habitat> (min-bound self)
				  (let* ((v (map min-bound (slot-ref self 'patch-list)))
							(vx (apply min (map car v)))
							(vy (apply min (map cadr v)))
							)
					 (list vx vy)))

;--- model-method <habitat> (max-bound self)
(model-method <habitat> (max-bound self)
				  (let* ((v (map max-bound (slot-ref self 'patch-list)))
							(vx (apply max (map car v)))
							(vy (apply max (map cadr v)))
							)
					 (list vx vy)))

;;1.0/(1.0 + exp(-2*pi*l*(2*(x+(0.5 - off)) - 1.0)) )
;; l = 1.0, off = 0.5



;; p is usually something like mm->points

;--- (<habitat> <procedure>) (map-log-data self logger format caller targets)
(model-method (<habitat> <procedure>)
				  (map-log-data self logger format caller targets)
  (let* ((symlist (services H))
			(name (slot-ref H 'name))
			(plist (slot-ref H 'patch-list))
			(locs (centroid (map location plist)))
			)
	 (let ((file (slot-ref logger 'file))
			 (p (slot-ref self 'map-projection)))
		(if (or (not p) (null? p))  (set! p (lambda (x) x)))
		
		
		(ps 'moveto (list (p (car locs)) (p (cadr locs))))
		(if adjust-grey (ps 'setgray HABITATGREY))
		(ps 'Helvetica 12)
		(ps 'show (string-append (slot-ref H 'name)))								  
		
		(if (member 'nested-habitat nested-agents)
			 (for-each (lambda (lpch)
							 (ps 'Helvetica 7)
							 (map-log-data lpch format caller targets p ps)
							 )
						  plist))
		)))

;--- (<habitat> <procedure>...) (log-data self logger format caller targets)
(model-method (<habitat> <procedure> <symbol> <procedure>)
	  (log-data self logger format caller targets)
	  (let ((file (slot-ref logger 'file))
			  (p (slot-ref self 'map-projection)))
		 (if (or (not p) (null? p))  (set! p (lambda (x) x)))
		 (case format
			((ps)
			 (map-log-data self logger format caller targets p ps)
			 (if (member 'nested-habitat nested-agents)  
				  (for-each (lambda (lpch)
								  (log-data lpch logger format caller targets ps p)
								  )
								plist))
			 )
			((dump)
			 (with-output-to-port
				  (lambda ()
					 (dump self))))

			((text table)
			 (log-data-parent)

			 ))
		 )
	  )

;--- model-method (<habitat>) (spatial-scale self)
(model-method (<habitat>) (spatial-scale self)
				  (if (not (my 'scale))
						(let ((lscale (apply
											append
											(map
											 (lambda (x)
												(map (lambda (y)
														 (distance (location x)
																	  (location y)))
													  (patch-list self)))
											 (patch-list self)))))
						  (set-my! 'scale
									  (/ (apply + lscale)
										  (+ 1 (- (length lscale)
													 (length (my 'patch-list))))))
						  ;;  This gets rid of the "self-distances" which are zero
						  )
						)
				  (my 'scale))


;--- model-body <habitat>
(model-body <habitat>
	(kdnl* 'model-bodies "In " (class-name-of self) (name self) "@" t)

	(if (member 'nested-habitat nested-agents)
		 (for-each (lambda (x) 
						 (if (< (subjective-time x) (+ t dt))
							  (run x t (+ t dt) (my 'kernel))
							  ;;(dnl* "Skipping" (name x))
							  )
						 )
					  (my 'patch-list)
					  )
		 )
	(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self)
			 ":" dt "/" (my 'dt) 'A)
	(parent-body)
	(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self)
			 ":" dt "/" (my 'dt) 'A)
	dt
	)



;--- model-method (<habitat> <symbol>)(extra-variable self field)
(model-method (<habitat> <symbol>)(extra-variable self field)
				  (value self (symbol->string field)))


;--- model-method (<habitat>) (extra-variable-list self)
(model-method (<habitat>) (extra-variable-list self)
				  (let ((patch-vars
							(uniq (apply append
											 (map extra-variable-list (my 'patch-list)))))
						  )
					 ;;(dnl* "HABITAT PATCH VARIABLES:" patch-vars)
					 (uniq (append (list 'name 'subjective-time) patch-vars))))
;; returns a list of symbols


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:

