(include "framework")
;-  Identification and Changes

;--
;	landscape.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.19
;		Location: odin:/home/gray/study/src/new/landscape.scm
;
;	History:
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;(load "postscript.scm")

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


(define (make-box ll ur)
  (list ll (list (car ll) (cadr ur)) ur (list (cadr ll) (car ur)) ll))

;;(define (logistic-growth  dt domain 0  value capacity  rvalue)

;(define service? (make-generic))
;(define add-service! (make-generic))
;(define remove-service! (make-generic))
;(define service-list% (make-generic))
;(define service (make-generic))
;(define services (make-generic)) ;; returns value
;(define set-services! (make-generic)) ;; sets value
;(define value (make-generic))      -- defined for environment
;(define add-to-value (make-generic))      -- defined for environment
;(define set-value! (make-generic)) -- defined for environment

;--- oriented toward habitats, patches  and ecoservices
;---- ecoservice lists, nearest suppliers....

(UNFINISHED-BUSINESS "The logging of subsidiary agents is not sorted out yet")

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
	 (ps 'moveto (map p (map + (list-head loc 2)
									 (map p (list (* 1.0 rad)
													  (* (- (/ ns 2.0) n) 1.0))))))
	 (if adjust-grey (ps 'setgray PATCHGREY)) ;; zero is white...
	 (ps 'Helvetica 7)
	 (ps 'show (string-append (slot-ref x 'name) ": "
									  (number->string (value x))))
	 ))


(define (crop-caption ps p x #!rest pt)
	 (if (null? pt) (set! pt 10) (set! pt (car pt)))
	 (let ((loc ((mapf p) (list-head (location x) 2)))
			 (rad (car (p (list (radius x))))))
		(ps 'moveto (map - loc
							  (list (* 0.5 rad)  (* -1 (+ 5 (* 1 rad) )))))
		(ps 'Helvetica pt)
		(if adjust-grey (ps 'setgray PATCHGREY))
		(ps 'show-right (string-append
							  (slot-ref x 'name) " at "
							  (number->string (slot-ref x 'subjective-time))))
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

			;; This is analogous to the "(service-list%-index self sym)"
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
				  ((eqv? (car args) 'template) 
					ecoservice-template)
				  ((eqv? (car args) 'predation-matrix) 
					predation-matrix)
				  ((eqv? (car args) 'efficiency-matrix) 
					(EM 'transpose)) ;; send it back in the same form we got it
				  ((member (car args) '(service-data species-data))
					service-data-list)
				  ((member (car args) '(service-ids species-ids))
					service-id-list)
				  ((member (car args) '(service-names species-names))
					service-name-list)
				  ((member (car args) '(service-symbols species-symbols))
					service-symbol-list)
				  ((member (car args) '(service-types species-types))
					service-type-list)
				  ((member (car args) '(service-eqn-syms species-eqn-syms))
					service-eqn-sym-list)
				  ((eqv? (car args) 'd/dt-list)
					d/dt)
				  ((eqv? (car args) 'index)
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
;;(default-agent-initialisation <environment>)

(model-method (<environment>) (services% self syms)
				  '())

(model-body <environment>
				(call-next-parent-body)
				dt)

(model-method (<environment> <list>) (contains? self loc)
				  (if (member (my 'rep) '(() #f <uninitialised> <uninitialised>))
						(let ((m (map <= loc (my 'minv)))
								(M (map <= (my 'maxv) loc)))
						  (apply andf (append m M)))
						((my 'rep) (contains? self loc))))


(model-method (<environment> <list>) (contains? self loc)
				  (let* ((minv (my 'minv))(maxv (my 'maxv))
							(n (min (length loc) (length minv) (length maxv)))
							(m (list-head (min-bound self) n))
							(M (list-head (max-bound self) n))
							(l (list-head loc n)))
				  (if (member (my 'rep) '(() #f <uninitialised> <uninitialised>))
						(if (null? l)
							 #f
							 (apply andf (append (map <= m l)
														(map <= l M))))
						((my 'rep) (contains? self loc)))))

(model-method (<environment> <thing>) (contains? self entity)
				  (contains? (location entity))
				  )


(model-method (<environment>) (random-point self) ;; returns a random location in the environment
				   ;;(let ((n (my 'minv)))
				  ;;  (map + (map * (map random-real n) (map - (my 'maxv) (my 'minv))) (my 'minv)))
				  (random-location (my 'minv) (my 'maxv)))

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

When ecoservices are running externally, they maintain a list of agents which will respond to 

")


;;(default-agent-initialisation <ecoservice> '() 'do-growth #t 'history #f 'run-externally #f 'ext-get (lambda x #f) 'ext-set! (lambda x #f) 'external-rep-list '())

;;--- ecoservice model-body support routines (mainly about growth)

(UNFINISHED-BUSINESS "This should probably have bits disabled when there are active-subsidiary-agents")
(model-body <ecoservice>
						(kdnl* '(model-bodies ecoservice-running) (class-name-of self) (my 'name)  "@"  t)
						;;;(dnl* 'b1)
						(let ((h (slot-ref self 'history)))
						  (if h
								(slot-set! self 'history
											  (cons (cons t (my 'value)) h)))
						  )
						;;;(dnl* 'b2)
						

						(if (and (my 'do-growth)
									(null? (my 'active-subsidiary-agents)))
							 ;; if there are active subsidiaries, rely on them to do the calulations.
							 ;; Otherwise, it may be suppressed for other reasons, in dynamic-patches,
							 ;; for example
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
											 (growth-model t dt value))
											((eqv? ecoserv-growth 'sigmoid)
											 (let* (;; This is a logistic update
													  (ipt (/ value capacity)) 
													  (pt (inverse-sigmoid* ipt))
													  (rdt (/ dt domain))
													  )
												(* capacity (sigmoid* (+ pt rdt)))))
											((eqv? ecoserv-growth 'linear)
											 (min capacity (+ value (* dt (my 'r)))))
											(#t
											 value)))
										 )
								  ;;(dnl* (name (my 'patch)) "/" (my 'name)
								  ;;      "value =" value "| newvalue =" newvalue)
								  
								  (set-my! 'value  newvalue)
								  )
								))

						(call-next-parent-body) ;; chain to <agent>
						;;(parent-body)
						dt
						)


;; 
(define (simple-ecoservice taxon N n V C r maxdt growing? growthmodel . P) 
  (if (pair? P) (set! P (car P)))
  
  (let ((A (create <ecoservice> taxon
							  'name N
							  ;; string corresponding to its name, like "Vulpes lagopus"
							  'type n
							  ;;eg 'eqn-based
							  'sym n
							  ;; 'Vl perhaps
							  'value V
							  ;; Immediately after model-body this will be the value calculated, before hand it is the value to be used
							  'capacity C
							  ;; this corresponds to a carrying capacity ... +inf.0 and -inf.0 are reasonable values for this 
							  'r r
							  ;; r controls the growth rate per unit time, for linear values this is the slope, 
							  'delta-T-max maxdt	
							  ;; r is the rate of growth per unit time
							  'do-growth growing?
							  ;; if #f any changes must be effected by external agency
							  'growth-model growthmodel
                       ;;; 'sigmoid
                       ;;; 'linear
                       ;;; (lambda (t dt v) ...)
							  'patch P
							  )

			  ))
	 (dnl "Made ecoservice for " n)
	 (slot-set! A 'provides (list n))
	 A
	 )
)


;(model-method <ecoservice> (dump self)
;				  (dump% self 0))

(model-method <ecoservice> (dump% self count)
				  (display (make-string count #\space))
				  (display "<ecoservice>\n")

				  (let* ((slots (class-slots-of self))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (display (make-string (+ 2 count) #\space))
									 (display x)
									 (display ": ")
									 (display y)
									 (newline))
								  slots vals)))


;---- query & set

(define (ext-get-func self)
  (lambda (other)
	 (cond
	  ((eqv? (slot-ref other 'sym) (slot-ref self 'sym)) (value other))
	  (#f 0)
	  (#t (error "bad request to external ecoservice agent"))))
  )

(define (ext-set!-func self v)
  (lambda (other)
	 (cond
	  ((eqv? (slot-ref other 'sym) (slot-ref self 'sym)) (set-value! other v))
	  (#f 0)
	  (#t (error "bad request to external ecoservice agent"))))
  )

(model-method (<ecoservice>) (symbol self)
				  (slot-ref self 'sym))

(model-method (<ecoservice> <symbol>) (service? self sym)
				  (or (eqv? (my 'sym) sym) (eqv? (my 'name) sym)))

(model-method (<ecoservice> <pair>) (service? self symlist)
				  (or (member (my 'sym) symlist) (member (my 'name) symlist)))

(model-method (<ecoservice>) (value self)
				  (if (not (and (slot-ref self 'running-externally)
									 (pair? (slot-ref self 'external-rep-list))
									 (slot-ref self 'ext-get)))
						(my 'value)
						(let ((accumulator 0))
						  (apply + 
									(map
									 (lambda (x)
										((slot-ref self 'ext-get) x)
										)
									 (slot-ref self 'external-rep-list))
						  ))
						)
				  )

(model-method (<ecoservice>) (capacity self)
				  (my 'capacity))

(model-method (<ecoservice>) (set-value! self val)
				  ;;(dnl* "Setting" (name self) "from" (my 'value) "to" val)
				  (set-my! 'value val)
				  (let ((asa (my 'active-subsidiary-agents)))
					 (if (pair? asa) 
						  (let ((L (length asa)))
							 (for-each
							  (lambda (a)
								 (UNFINISHED-BUSINESS "This is primitive and clunky")
								 (slot-set! a 'value  (/ val L))) 
							  asa))
						  ))
				  )

(model-method (<ecoservice>) (growth-model self)
				  (my 'growth-model))

;--- adjustment

(model-method (<ecoservice>) (disable-growth! self) (set-my! 'do-growth #f))
(model-method (<ecoservice>) (enable-growth! self) (set-my! 'do-growth #t))

(model-method (<ecoservice>) (add! self val)
				  (let ((v (my 'value))
						  (asa (my 'active-subsidiary-agents)))
					 (if (number? v)
						  (begin
							 (set-my! 'value (+ v val) )
							 (if (pair? asa) 
								  (let ((L (length asa)))
									 (for-each
									  (lambda (a)
										 (slot-set! a 'value  (+ (slot-ref a 'value) (/ val L))) )
									  asa))
								  ))
						  (abort "ecoservice:add!: value is not a number")
						  )))

(model-method (<ecoservice>) (scale! self val)
				  (let ((v (my 'value)))
					 (if (number? v)
						  (begin
							 (set-my! 'value (* v val) )
							 (if (pair? asa) 
								  (for-each
									(lambda (a)
									  (slot-set! a 'value  (* (slot-ref a 'value) val))) 
									asa)))
						  (abort "ecoservice:scale!: value is not a number")
						  )))


(model-method <ecoservice> (radius self)
				  (radius (my 'patch)))

(model-method (<ecoservice> <number>)(set-radius! self r)
				  (set-radius! (my 'patch) r))

(model-method <ecoservice> (location self)
				  (location (my 'patch)))

(model-method (<ecoservice> <log-introspection> <symbol>) (log-data self logger format targets args)
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
											 
											 (let ((val (if (eqv? field 'name) 
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
										 (filter (not-member (slot-ref logger 'dont-log))
													targets)))
							)
						  (newline file)
						  )
						))
				  )


							  

;--- <circle>
;;(default-object-initialisation <circle>)

(define (Circle c r n)
  (let ((pi (acos -1)))
	 (translate-trace c (map (lambda (x)
										(list (cos (* (/ x n) 2.0 pi)) (sin (* (/ x n) 2.0 pi)))
										) (append (seq n) '(0)))
							)))

(define (make-crop-circle centre radius . n) ;; (-: make-patch-circle might be more appropriate, but not as much fun :-)
  (create- <circle> 'type 'area 'locus centre 'radius radius 'perimeter (Circle centre radius (if (or (null? n) (not (and (integer? (car n)) (> (car n) 2)))) 12  (car n))))
  )

;(model-method (<circle>) (dump self)
;					(dump% self 0))


(model-method (<circle>) (dump% self count)
				  (display (make-string count #\space))
				  (display "<circle>\n")
				  (let* ((slots (class-slots-of self))
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

(model-method (<circle>) (minima self)
				  (map - (my 'locus) (make-list (length (my 'locus)) (my 'radius))))

(model-method (<circle>) (maxima self)
				  (map + (my 'locus) (make-list (length (my 'locus)) (my 'radius))))

(model-method (<circle> <list>) (contains? self loc)
				  (<= (distance (my 'locus) loc) (my 'radius)))

(model-method (<circle>) (centre self)
				  (my 'locus))

(model-method (<circle>) (radius self)
				  (my 'radius))

(model-method (<circle>) (Radius self)
				  (my 'radius))

(model-method (<circle>) (min-bound self)
				  (my 'radius))

(model-method (<circle>) (max-bound self)
				  (my 'radius))

(model-method (<circle> <list>) (distance-to-boundary self loc)
				  (if (contains? self loc)
						0
						(- (distance loc (my 'locus)) (my 'radius))))

(model-method (<circle>) (random-point self)
					(let ((c (my 'locus))
							(r (my 'radius)))
					  (map (lambda (x)
								(+ x (* r
										(-
										 (* 2.0 (random-real))
										 1.0)))) c)))


;--- <polygon>

;;(default-object-initialisation <polygon>)

(define (make-polygon centre polygon)
  (if (not (eqv? (car polygon) (car (reverse polygon))))
		(set! polygon (append polygon (list (car polygon)))))
  (create- <polygon> 'type 'area 'locus centre 'perimeter (list-copy polygon)))

;(model-method (<polygon>) (dump self)
;					(dump% self 0))

(model-method (<polygon>) (dump% self count)
				  (display (make-string count #\space))
				  (display "<polygon>\n")
				  (let* ((slots (class-slots-of self))
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

(model-method (<polygon>) (minima self)
				  (extremum min (my 'perimeter)))

(model-method (<polygon>) (maxima self)
				  (extremum max (my 'perimeter)))


(model-method (<polygon> <list>) (contains? self loc)
				  (point-in-polygon loc (my 'perimeter)))


(model-method (<polygon> <list>) (distance-to-boundary self loc)
				  (if (contains? self loc)
						0
						(distance-to-boundary loc (my 'perimeter))))

(model-method (<polygon>) (min-bound self)
				  (let ((c (my 'locus)))
					 (apply min (map (lambda (p)
											 (distance c p))
										  (my 'perimeter)))))


(model-method (<polygon>) (max-bound self)
				  (let ((c (my 'locus)))
					 (apply max (map (lambda (p)
											 (distance c p))
										  (my 'perimeter)))))

(model-method (<polygon>) (Radius self)
				  (max-bound self)
				  )
(model-method (<polygon>) (radius self)
				  (min-bound self)
				  )

(model-method (<polygon>) (random-point self)
					(let* ((peri (my 'perimeter))
							 (minx (apply min (map car peri)))
							 (maxx (apply max (map car peri)))
							 (dx (- maxx minx))
							 (miny (apply min (map cadr peri)))
							 (maxy (apply max (map cadr peri)))
							 (dy (- maxy miny)))
					  (let loop ((x (+ (* (random-real) dx) minx))
									 (y (+ (* (random-real) dy) miny))
									 )
						 (if (contains? self (list x y))
							  (list x y)
							  (loop (+ (* (random-real) dx) minx)
									  (+ (* (random-real) dy) miny))))))

;-- <patch> methods and bodies
;; 
(define (make-boundary rep centre arg)
  (let ((rep-class (case rep
							((circle) <circle>)
							((polygon relative-polygon) <polygon>)
							(else 'bad))))

	 (let ((M (create- rep-class 'type 'area)))
		(case rep
		  ((circle)
			(slot-set! M 'rep (create- <circle> 'type 'area 'locus centre 'radius arg))

			(slot-set! M 'rep (create- <polygon> 'type 'area 'locus centre
											'perimeter (list-copy arg)))
			
			)

		  ((polygon absolute-polygon)
			(slot-set! M 'rep (create- <polygon> 'type 'area 'locus centre
													 'perimeter (list-copy arg)))
			(slot-set! (slot-ref M 'rep) 'radius (max-bound (slot-ref M 'rep))
			))

		  ((relative-polygon)
			(slot-set! M 'rep (create- <polygon> 'type 'area 'locus centre
											'perimeter (translate-trace centre arg)))
			(slot-set! (slot-ref M 'rep) 'radius (max-bound (slot-ref M 'rep))
			))

		  (else (error "Bad representation specified for a boundary" rep)))))
  )

;; min-bound, max-bound contains? services 

;--- (initialise...) 


;;(default-agent-initialisation <patch>
;;  'service-list '()
;;  'default-value +nan.0
;;  'minv '(-inf.0 -inf.0)
;;  'maxv '(+inf.0 +inf.0)
;;  'needs-parent-bodies #t)

;;; (model-method <patch>  (initialise self args)
;;; 				  (if (and (pair? args) (pair? (car args)) (= (length args) 1))
;;; 						(set! args (car args)))

;;; 				  (dnl* "PATCH: " initialise self args)
;;; 				  ;; call "parents" last
;;; 				  ;; to make the
;;; 				  ;; initialisation list
;;; 				  ;; work
;;; 				  (initialise-parent)

;;; 				  (set-state-variables self args)
;;; 				  )


(model-method (<patch>) (random-point self)
				  (random-point (my 'rep)))

;(model-method <patch> (dump self)
;				  (dump% self 0))

(model-method <patch> (dump% self count)
				  (display (make-string count #\space))
				  (display "<patch>\n")
				  (let* ((slots (class-slots-of self))
							(vals  (map (lambda (x)
											  (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (if (not (member x '(service-list)))
										  (begin
											 (display (make-string (+ 2 count) #\space))
											 (display x)
											 (display ": ")
											 (display y)
											 (newline))))
								  slots vals))
				  (display (make-string (+ 2 count) #\space))
				  (display "service list:\n")
				  (for-each (lambda (x) (dump% x (+ 4 count)))
								(cons (my 'rep) (my 'service-list)))
				  )

(model-method (<patch> <polygon> <list>) (install-boundary self bdry centre)
				  (set-my! 'rep bdry)
				  (slot-set! bdry 'locus centre)
				  )

(model-method (<patch> <circle> <list>) (install-boundary self bdry centre)
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


;--- (add-service!...) adds a service to a patch

(model-method (<patch> <ecoservice>) (add-service! self new-service)
				  (let ((msl (my 'service-list)))
					 (if (not (list? msl))
						  (set! msl '()))
					 (set-services! self (append msl (list new-service))))
				  )

;--- (remove-service!...) removes all services that match the predicate
;                        in a patch
;;                       the predicate will probably be something like
;;                       (using-name-keep? 'wobble)
(model-method (<patch> <procedure>) (remove-service! self predicate)
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

(model-method <patch> (contains? self bit)
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

  
(model-method (<patch>) (service-list% self ss)
				  (if (and (pair? ss) (pair? (car ss))) (set! ss (car ss)))
				  (let ((S (my 'service-list)))
					 (if (null? ss)
						  S
						  (filter
							(lambda (x) (or (member (sym x) ss) (member (name x) ss)))
							S))))


(model-method (<patch> <list>) (services% self ss)
				  (if (null? ss)
						(map (lambda (x) (symbol x)) (my 'service-list))
						(map symbol (apply service-list (cons self ss)))))

(model-method (<patch> <list>) (specific-services% self ss)
				  (if (null? ss)
						(map (lambda (x) (name x)) (my 'service-list))
						(map symbol (apply service-list (cons self ss)))))


(model-method (<patch>) (radius self)
				  (radius (my 'rep)))

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

;; (Add-method representation
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
				  (let ((s (filter (lambda (a) (eqv? sym (symbol a)))
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
				  (let ((s (filter (lambda (a) (eqv? (symbol a) sym))
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
				  (let ((s (filter (lambda (a) (eqv? (symbol a) sym))
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
(model-method (<patch> <pair> <number>) (add! self symlist val)
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
						(kdnl* '(model-bodies patch-running)"In " (class-name-of self) (name self) "@" t)

						;;(dnl* (slot-ref self 'name))

						;;; (if (not (null? (my 'active-subsidiary-agents)))
						;;; (run-subsidiary-agents self
						;;; 							   t dt (my 'active-subsidiary-agents) run (my 'kernel))

						;;; 	  (for-each (lambda (x)
						;;; 					  (run x t (+ t dt) (my 'kernel))
						;;; 					  )
						;;; 					(my 'active-subsidiary-agents))




						;;; 	  )
						(call-next-parent-body)
						dt
						)

;--- model-method (<patch> <agent> <symbol> <agent>) (log-data self logger format  targets)
(model-method (<patch> <log-introspection> <symbol> <list>) (log-data self logger format  targets)
				  (kdnl* 'log-horrible-screaming (map cnc (list self logger format  targets)) 'format: format)
				  (let ((file (slot-ref logger 'file))
						  (p (composite-projection (slot-ref self 'local->model)
															(slot-ref logger 'model->local)))
						  )
					 (if (not (procedure? p)) (set! p (lambda (x) x)))
					 (kdnl* '(log-* log-patch) "[" (my 'name) ":"
							  (class-name-of self) "]" "in log-data")
					 
					 (case format
						((ps)
						 (let* ((symlist (services self))
								  (name (slot-ref self 'name))
								  (R (car (p (list (radius self)))))
								  (L (p (list-head (location self) 2)))
								  (slist (slot-ref self 'service-list))
								  (n (+ 1 (length slist))) ;; slist is
								  ;; becoming
								  ;; circular??
								  ;; ....******
								  (ns (length slist))
								  (mm-xoffset 2)
								  (mm-yoffset 2)
								  )
							
							(if adjust-grey (file 'setgray PATCHGREY))
							(ps-circle file  R L 0.7 0.0)

							(file 'moveto (p (map + L
														 (list (+ mm-xoffset
																	 (* 1.05 R))
																 (+ mm-yoffset
																	 (/ ns 2.0)))
														 )))
							(file 'show-table (map
													 (lambda (x) (string-append
																	  (slot-ref x 'name)
																	  ": " (pno (value x))))
													 slist))

							(crop-caption file p self)
							(dnl* "past crop caption")
							))


						;;((text table dump)
						;; (log-data-parent)
						;; )
						(else
						 (display (my 'name) file)
						 (for-each 
						  (lambda (x)
							 (display " " file)
							 (if (agent? x)
								  (display (value x))
								  (display x)))
						  (map (lambda (x) (value x)) (my 'service-list)))
						 (newline file)
						 ;;(log-data-parent)	
						 )
						)
					 )
				  )


;-- dynamic-patch methods and body

(definition-comment 'dynamic-patch
  "A dynamic patch expects definitions similar to those in diffeq-systems")

;--- <dynamic-patch> (initialise  (self args)
;;; (agent-initialisation-method (<dynamic-patch> args) (no-default-variables)
;;; 				  (set-state-variables self (list 'variable-names '()
;;; 															 'variable-symbols '()
;;; 															 'd/dt-list '()
;;; 															 'do-dynamics #t
;;; 															 'variable '()	
;;; 															 'subdivisions 12
;;; 															 ;; 'cause 12 is a nice number?
;;; 															 ))
;;; 				  (let ((variable-definitions
;;; 							(slot-ref self 'variable-definitions))
;;; 						  (variable-names
;;; 							(slot-ref self 'variable-names))
;;; 						  (variable-symbols
;;; 							(slot-ref self 'variable-symbols))
;;; 						  (d/dt-list (slot-ref self 'd/dt-list)))

;;; 					 (if (or (pair? variable-definitions)
;;; 								(pair? variable-names)
;;; 								(pair? variable-symbols)
;;; 								(pair? d/dt-list))
;;; 						  (begin
;;; 							 (if (and (pair? variable-definitions)
;;; 										 (or (pair? d/dt-list)
;;; 											  (pair? variable-names)
;;; 											  (pair? variable-symbols)))
;;; 								  (abort (string-append
;;; 											 "Dynamic patch specified a "
;;; 											 "population-definition *and* "
;;; 											 "one or more of\n"
;;; 											 "variable-names "
;;; 											 "variable-symbols "
;;; 											 "d/dt-list\n")))
							 
;;; 							 (if (and (null? variable-definitions)
;;; 										 (not (and (pair? d/dt-list)
;;; 													  (pair? variable-names)
;;; 													  (pair? variable-symbols))))
;;; 								  (abort (string-append
;;; 											 "Dynamic patch specified at "
;;; 											 "least one of variable-names "
;;; 											 "variable-symbols d/dt-list\n"
;;; 											 "but not all three."
;;; 											 )))

;;; 							 ))
;;; 					 (cond
;;; 					  ((pair? variable-definitions)
;;; 						(define-system-dynamics! self
;;; 						  variable-definitions))
;;; 					  ((pair? variable-names)
;;; 						(define-system-dynamics! self
;;; 						  variable-names
;;; 						  variable-symbols
;;; 						  d/dt-list))
;;; 					  (#t (slot-set! self 'variable-definitions #f)
;;; 							(slot-set! self 'variable-names #f)
;;; 							(slot-set! self 'variable-symbols #f)
;;; 							(slot-set! self 'd/dt-list #f)))
;;; 					 )
;;; 				  (initialise-parent) ;; call "parents" last
;;; 				  ;; to make the
;;; 				  ;; initialisation list
;;; 				  ;; work
;;; 				  (set-state-variables self args)
;;; 				  )




(define (bbox ll ur)
  (list ll (list (car ur) (cadr ll)) ur (list (car ll) (cadr ur)) ll))

(define unitbox '((0 0) (1 0) (1 1) (0 1) (0 0)))

(define (%patch-initialiser clss bdry name centre radius box . therest)
  (if (member clss (list <patch> <dynamic-patch>))
		(append (list clss)
				  (cond
					((eq? bdry <circle>)
					 (list 'rep (create- <circle> 'name name 
											'locus centre 'radius radius)
							 ))
					((eq? bdry <polygon>)
					 (let ((p (list 'rep (create- <polygon> 'name name 
											'locus centre 'perimeter (list-copy box))
										 )))
						;(dump (cadr p))
						p))
					(#t
					 (error "bad boundary class specified in patch-initialiser"
							  bdry)))
				  therest)
		(error (string-append "patches may only be initialised as <patch> or "
				 "<dynamic-patch> agents")
				 clss)
		))
  


;; make-grid is geared to making patches and dynamic-patches in a regular array
;; n m is the size of the grid, ll ur are the ordinates of the ll cell and the ur cell
;; bdry should be <circle> or <polygon>, clss should be <patch> or <dynamic-patch>,
;; clss-initialiser is a function that returns a fully formed initialiser list
;; (apart from the centre and radius or perimeter), and P should be either null
;; or a <habitat> like class.


;; bounding rectangular volume for the pointset


(define (extremum op pointset)
  (map (lambda (i)
			(apply op (map (lambda (x) (list-ref x i)) pointset )))
		 (seq (length (car pointset)))))


;; Returns a list of the form (patchlist patchgrid)
(define (make-grid cell-class taxon name cell-type n m ll ur #!rest terrain)
  (let* ((nscale (real->integer (/ (- (car ur) (car ll)) (* 1.0 n))))
			(mscale (real->integer (/ (- (cadr ur) (cadr ll)) (* 1.0 n))))
			(radius (min nscale mscale))
			(patch-list '())
			(M (make-list* n m))
			)
	 
	 (map-**-ix (lambda (x i)
					  (let ((centre (list (+ (car ll) (* nscale (+ 0.5 (car i))))
												 (+ (cadr ll) (* mscale (+ 0.5 (cadr i))))))
							  (box (bbox (list (+ (car ll) (* nscale (car i)))
													 (+ (cadr ll) (* mscale (cadr i))))
											 (list (+ (car ll) (* nscale (+ 1 (car i))))
													 (+ (cadr ll) (* mscale (+ 1 (cadr i)))))))
							  (pname (string-append name "-" (number->string (car i)) ","
															(number->string (cadr i))))
							  )
						 
						 (kdnl* 'make-grid "Working " x i " --> "box)
						 (let* ((minx +nan.0)
								  (miny +nan.0)
								  (maxx +nan.0)
								  (maxy +nan.0)
								  (mB (extremum min box))
								  (MB (extremum max box))
								  (cell
									(create 
									 cell-class
									 taxon
									 'name pname
									 'type cell-type
									 'representation (class-name-of cell-type)
									 'rep
									 (create-
									  <polygon>
									  'locus centre
									  'type '(area tesselation)
									  'radius (/ (sqrt (apply + (map sqr (map - MB mB)))) 2.0)
									  'perimeter box
									  'minv mB
									  'maxv MB
									  'note "generated by make-grid"
									  'dont-log #f ;; let them be logged by default
									  ))))
							
							(set! patch-list (cons cell patch-list))
							cell))
						 )
					M)
	 ;; (map (lambda (i) (slot-ref (slot-ref (list-ref patchlist i) 'rep) 'perimeter)) (seq (length patchlist)))
	 (reverse patch-list) 
	 )
  )


;; the services are defined by a list containing a name, a symbol, an
;; initial value, a capacity, its max dt, whether or not it grows, and
;; a growth model
(define (populate-patch-with-ecoservices p services . therest)
  (for-each (lambda (x)
				  (cond
					((agent? x)	(add-service! p x))
					((list? x)	(add-service! p (apply
														 (lambda (args)
															(let ((instance (allocate-instance (car args))))
															  (apply initialise (cons <ecoservice> x therest))
															  instance
															  )))))
					(#t (error (string-append "populate-patch should be a list containing members which are"
													  "patches, arguments  or "
													  "(create <ecoservice> taxon ...)"
													  services))))
				  )
				services)
  )



;;(define (patchsize domain) (* 0.25 (apply min (map - (list-head
;;  (cadr domain) 2) (list-head (car domain) 2)))))


;--- (make-patch services centre representation repspec . args)
(Comment "services should be a list of the form '((name type capacity) ...)
args can be  an update map or an update map and update equations 
   -- see old-model-version1/Model-configuration.scm
")

;--- model-method (<dynamic-patch> <string>) (service-list-index self service)
(model-method (<dynamic-patch> <string>) (service-list-index self service)
				  (let* ((si (my 'variable-names))
							(n (length si))
							(ix (member service si))
							(i (if ix  (- n (length ix)) #f)))
					 i))

;--- model-method (<dynamic-patch> <symbol>) (service-list-index self service)
(model-method (<dynamic-patch> <symbol>) (service-list-index self service)
				  (let* ((si (my 'variable-symbols))
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

;--- model-method <dynamic-patch> (dump% self count)

;(model-method <dynamic-patch> (dump self)
;				  (dump% self 0))


(model-method <dynamic-patch> (dump% self count)
				  (display (make-string count #\space))
				  (display "<dynamic-patch>\n")
				  (let* ((slots (class-slots-of self))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (if (not (eqv? x 'service-list))
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
				  (for-each (lambda (x) (dump% x (+ 4 count))) (my 'service-list))
				  )

;--- model-method (<dynamic-patch> <procedure> <symbol> <procedure>)(log-data self logger format  targets)
(model-method (<dynamic-patch> <log-introspection> <symbol> <list>) (log-data self logger format  targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'projection-assoc-list)))
					 (if (or (not p) (null? p))  (set! p (lambda (x) x)))
					 (kdnl* '(log-* log-patch) "[" (my 'name) ":"
							  (class-name-of self) "]" "in log-data")
					 
					 (case format
						((ps)
						 (let* ((symlist (services self))
								  (name (slot-ref self 'name))
								  )

							(if adjust-grey (file 'setgray PATCHGREY))
							(ps-circle file  (p (list (radius self)))
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
(model-method <dynamic-patch> (disable-service-growth! self service-name)
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
						(kdnl* '(model-bodies landscape-running nested-habitat)  (class-name-of self) (name self) "@" t "/" dt "(dt)" (my 'subjective-time) "(subj time)" )
						(call-next-parent-body) ;; chain to <environment>
						(kdnl* '(nested-habitat)  "after parent body ->" (class-name-of self) (name self) "@" t "/" dt "(dt)" (my 'subjective-time self) "(subj time)" )
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

(define (make-habitat taxon name default-ht domain terrain-function 
							 patch-list
							 )
  ;;(dnl* "Making hay")
  (let* ((H (create <habitat*> taxon 'name name 'default-value default-ht
								'type 'habitat
						'minv (car domain) 'maxv (cadr domain)
						'terrain-function terrain-function
						'patch-list patch-list
						'type 'landscape
						)			
				)
			)
	 (dnl "Acquiring patches " patch-list)
	 (acquire-agents H #t patch-list) ;; insert the patches into the nested runqueue ... the #t means they go in the active queue too.
	 (dnl "Finished making habitat")
	 H)
  )


;;; (slot-set! H 'global-update
;;; 			  (lambda (H)
;;; 				 (let* ((services (slot-ref (slot-ref H 'global-patch) 'service-list))
;;; 						  (zero (map (lambda (x) (set-value! x 0) (value x)) services))
;;; 						  (syms (map (lambda (x) (slot-ref x 'sym)) services))
;;; 						  (agg (map
;;; 								  (lambda (s S)
;;; 									 (map (lambda (p)
;;; 											  (value-set! (value p s)
															  
					
					

;--- Add things to the runqueue

;---- (add-habitat-to-queue Q h)
(define (add-habitat-to-queue Q h) ;; returns the queue, so us it like
											  ;; (set! Q (add-...-queue Q hab))
  (let ((p (patch-list h))
		  (s (service-list h))
		  )
	 (uniq (append Q (list h) p s))))



;--- model-method <habitat> (agent-prep self start end) Set preconditions
;                                                    for running
(model-method (<habitat> <number> <number>) (agent-prep self start end)
				  (agent-prep-parent start end)
				  )


;--- initialise <habitat> (self args) -- make a new habitat
;;(default-agent-initialisation <habitat> 'scale #f)

;;; (model-method <habitat> (initialise self args)
;;; 				  (set-state-variables self (list 'scale #f))
;;; 				  (initialise-parent)
;;; 				  ;; call "parents" last
;;; 				  ;; to make the
;;; 				  ;; initialisation list
;;; 				  ;; work
;;; 				  (set-state-variables self args)
;;; 				  )

;(model-method <habitat> (dump self)
;				  (dump% self 0))

;--- model-method <habitat> (dump% self count) dumps the state of the habitat
;                                               agent in a readable way
(model-method <habitat> (dump% self count)
				  (display (make-string count #\space))
				  (display "<habitat>\n")

				  (let* ((slots (class-slots-of self))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (if (not (eqv? x 'patch-list))
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
				  (for-each (lambda (x) (dump% x (+ 4 count))) (my 'patch-list)))


;--- model-method (<habitat> <patch>) (add-patch! self patch) add a
;                                                            patch to the habitat
;; Note that there is an implicit ordering to adding patches and patchlists
(model-method (<habitat> <patch>) (add-patch! self patch)
				  (set-my! 'patch-list (uniq (cons patch (my 'patch-list))))
				  (let ((minv (apply minima (my 'patchlist)))
						  (maxv (apply maxima (my 'patchlist)))
						  )
					 (set-my 'minv minv)
					 (set-my 'maxv maxv)
				  ))

;--- model-method (<habitat> <patch>) (add-patch! self patch) add a
;                                                            patch to the habitat
;; Note that there is an implicit ordering to adding patches and patchlists
(model-method (<habitat> <list>) (add-patches! self patchlist)
				  (set-my! 'patch-list (uniq (append  patchlist (my 'patch-list))))
				  (let ((minv (apply minima (my 'patchlist)))
						  (maxv (apply maxima (my 'patchlist)))
						  )
					 (set-my 'minv minv)
					 (set-my 'maxv maxv)
				  ))

;--- model-method (<habitat> <procedure>) (remove-patch! self pfilter)
;  keep only patches which match a filter
(model-method (<habitat> <procedure>) (remove-patch! self pfilter)
				  (set-my! 'patch-list (filter pfilter (my 'patch-list))))

;--- (services...) returns services matching the sym or in the symlist
(model-method (<habitat> <list>)
				  (services% self ss)
				  (if (not (null? ss))
						(filter (lambda (x) (member x ss)) (services self))
						(unique
						 (map string->symbol 
								(sort (map symbol->string 
											  (apply append
														(map services 
															  (patch-list self))))
										string<?)))
						))



(model-method (<habitat>) (random-point self)
				  (let* ((plist (my 'patch-list))
							(llist (map random-point plist))
						   (i (random-integer (length plist)))
							)
									 
				  (list-ref llist i)))




;--- model-method (<habitat>) (service-list% self . ss)
;; returns the slist of provided services: ss is an optional list of
;; patch names/symbols
;;; (model-method (<habitat> <list>)
;;; 				  (service-list% self ss)
;;; 				  (uniq
;;; 					(if (null? ss) 
;;; 						 (apply append (map (lambda (x)
;;; 													 (service-list x))
;;; 												  (patch-list self)))
;;; 						 (apply append 
;;; 								  (map (lambda (x) 
;;; 											(service-list x 
;;; 															  (if (symbol? (car ss))
;;; 																	ss
;;; 																	(car ss))))
;;; 										 (patch-list self))))))


;--- model-method (<habitat>) (service-list% self) ret a list of all ecoservices
;; May be wrong....***
(model-method (<habitat> <list>) (service-list% self ss)
				  (let* ((S (map (lambda (s) (service-list s)) (my 'patch-list))))
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
; returns the list of patches (possibly filtered by names, symbols,
; procedures....

(model-method (<habitat>) (patch-list% self arg)
				  (cond
					((null? arg)
					 (my 'patch-list))
					((symbol? arg)
					 (let ((symlist arg))
						(filter (lambda (p) 
									 (let ((s (services p (list symlist))))
										(and s (not (null? s)))))
								  (my 'patch-list))))
					((pair? arg)
					 (let ((symlist arg))
						(filter (lambda (p) 
									 (let ((s (services p symlist)))
										(and s (not (null? s)))))
								  (my 'patch-list))))
					((procedure? arg)
					 (let ((pfilter arg))
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
				  (let ((pl (slot-ref self 'patch-list)))
					 (if (pair? pl)
						  (let* ((v (map min-bound pl))
									(vx (apply min (map car v)))
									(vy (apply min (map cadr v)))
									)
							 (list vx vy))
						  (my 'minv))))

;--- model-method <habitat> (max-bound self)
(model-method <habitat> (max-bound self)
				  (let ((pl (slot-ref self 'patch-list)))
					 (if (pair? pl)
						  (let* ((v (map max-bound (slot-ref self 'patch-list)))
									(vx (apply max (map car v)))
									(vy (apply max (map cadr v)))
									)
							 (list vx vy))
						  (my 'maxv))))

;;1.0/(1.0 + exp(-2*pi*l*(2*(x+(0.5 - off)) - 1.0)) )
;; l = 1.0, off = 0.5



;; p is usually something like mm->points

;--- (<habitat> <procedure>) (map-log-data self logger format targets)
(model-method (<habitat> <log-introspection> <symbol> <list>) (map-log-data self logger format  targets)
  (let* ((symlist (services self))
			(name (slot-ref self 'name))
			(plist (slot-ref self 'patch-list))
			(locs (centroid (map location plist)))
			)
	 (let ((ps (slot-ref logger 'file))
			 (p (slot-ref self 'projection-assoc-list)))
		(if (or (not p) (null? p))  (set! p (lambda (x) x)))
		
		(ps 'moveto (list (p (car locs)) (p (cadr locs))))
		(if adjust-grey (ps 'setgray HABITATGREY))
		(ps 'Helvetica 12)
		(ps 'show (string-append (slot-ref self 'name)))								  
		
		(let ((subs (slot-ref self 'active-subsidiary-agents)))
		  (if (pair? subs)
				 (for-each (lambda (lpch)
							 (ps 'Helvetica 7)
							 (map-log-data lpch format targets p ps)
							 )
							  subs))))
	 
		))

;--- (<habitat> <procedure>...) (log-data self logger format  targets)
(model-method (<habitat> <log-introspection> <symbol> <list>) (log-data self logger format  targets)
	  (let ((ps (slot-ref logger 'file))
			  (p (slot-ref self 'projection-assoc-list)))
		 (if (or (not p) (null? p))  (set! p (lambda (x) x)))
		 (case format
			((ps)

			 (map-log-data self logger format targets p ps)
			 (let ((subs (slot-ref self 'active-subsidiary-agents)))
				  (for-each (lambda (lpch)
								  (log-data lpch logger format targets ps p)
								  )
								(my 'patch-list)))
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


;--- model-body <habitat>
(model-body <habitat*>
	(kdnl* '(model-bodies habitat-running) (class-name-of self) (name self) "@" t)
	(call-next-parent-body)

	(let ((gp (my 'global-patch)))
	  (run-agents self t dt (list gp) run (my 'kernel)) ;; execute patch model body for global patch *after* subsidiary patches
	  dt))







;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:

