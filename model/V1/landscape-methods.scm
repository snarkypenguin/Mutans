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

;-  Support function for patches

;; the predation matrix is oriented so that if we consider grass-cow-leopard across the columns, the subdiagonal will be the one with non-zero entries
;; the efficiency matrix is oriented the same way as the predation matrix (on input).

;; non-sigmoidal-growth must either be absent, #f, growth function
;; with a form
;;
;;    (growth-func t population-level-list)
;;
;; where domain is the "time to cap" or some such thing, where P_0 is
;; the starting value, P is the current value, K is the capacity and r
;; is the "exponent" or a list of such functions (one for each species).
;; In practice, we probably ought to never get just a function.


;; For examples look at savanna-parameters....

(define (make-population-structure predation-matrix efficiency-matrix service-data-list ecoservice-template)
  (pp service-data-list)
  (pp ecoservice-template)
  (abrot)
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

;; This is analogous to the "(service-list-index self sym)" call in <dynamic-patch>, but it relies on globals
			(service-index (lambda (sym) 
								  (let ((m (member sym service-eqn-sym-list))
										  (n (member sym service-name-list)))
									 
									 (if m (- (length service-eqn-sym-list) (length m))
										  (if n (- (length service-eqn-sym-list) (length n))
												#f)))))

			(pd (lambda (species)
					(let* ((species-list service-eqn-sym-list)
							 (as-prey-ratio (list-sym-ref ((PM 'transpose)) service-eqn-sym-list species))
							 (as-predator-ratio (list-sym-ref (PM) service-eqn-sym-list species))
							 (predator-efficiency (list-sym-ref (EM) service-eqn-sym-list species)) 
							 (gr (list-ref (list-ref service-data-list (service-index species)) 3))
							 (mort (list-ref (list-ref service-data-list (service-index species)) 4))
			
							 (pop-growth-func (list-ref (list-ref service-data-list (service-index species)) 6))
							 )

					  (let ((dP/dt
								(lambda (t . populations)
								  (pop-growth-func t species (gr as-predator-ratio predator-efficiency mort as-prey-ratio populations))))
							  )
							  dP/dt))))

			(d/dt (map (lambda (species) (pd species)) (map caddr service-data-list)))
			)
	 (let ((population-structure
			  (lambda args
				 (cond
				  ((null? args)
					(abort "Nothing  passed as an argument to a population structure"))
				  ((eq? (car args) 'template) 
					ecoservice-template)
				  ((eq? (car args) 'predation-matrix) 
					predation-matrix)
				  ((eq? (car args) 'efficiency-matrix) 
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







;-- Methods and bodies

;; Stops things going off the rails 
;--- (services...) returns services matching the sym or in the symlist
(sclos-method (<environment>) (services self syms)
				  '())

(sclos-model-body <environment>
						(parent-body)
						dt)

;--- <ecoservice> methods
;; By convention we give ecoservices names which are strings, types which are symbols ... neither needs to be unique
;; 
;; value, set-value!, add! scale!


(comment " Ecoservices are able to update their state themselves.  If
they aren't *nested* This may have irregular interactions with any
dynamics being forced on them from a dynamic-patch since the order of
insertion in the queue is not prescribed. The best way of dealing with
this is to ensure that the timestep associated with ecoservices is
half (or less) of the timestep of the patch.
")
  
(add-method initialize
				(make-method (list <ecoservice>)
								 (lambda (initialize-parent self args)
									(initialise self (list 'history #f)) ;; Set history to '() to record the history
									(slot-set! self 'do-growth #t)
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))


(sclos-method <ecoservice> (dump self . count)
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
(sclos-method (<ecoservice> <symbol>) (service? self sym)
				  (or (eq? (my 'type) sym) (eq? (my 'name) sym)))

(sclos-method (<ecoservice> <pair>) (service? self symlist)
				  (or (member (my 'type) symlist) (member (my 'name) symlist)))

(sclos-method <ecoservice> (value self)
				  (my 'value))

(sclos-method <ecoservice> (capacity self)
				  (my 'capacity))

(sclos-method (<ecoservice>) (set-value! self val)
				  ;;(dnl* "Setting" (name self) "from" (my 'value) "to" val)
				  (set-my! 'value val))

(sclos-method <ecoservice> (growth-model self)
				  (my 'growth-model))

;---- adjustment

(sclos-method (<ecoservice>) (disable-growth! self) (set-my! 'do-growth #f))
(sclos-method (<ecoservice>) (enable-growth! self) (set-my! 'do-growth #t))

(sclos-method (<ecoservice>) (add! self val)
				  (let ((v (my 'value)))
					 (if (number? v)
						  (set-my! 'value (+ v val) )
						  (aborts "ecoservice:add!: value is not a number")
						  )))

(sclos-method (<ecoservice>) (scale! self val)
				  (let ((v (my 'value)))
					 (if (number? v)
						  (set-my! 'value (* v val) )
						  (aborts "ecoservice:scale!: value is not a number")
						  )))


;;--- ecoservice model-body support routines (mainly about growth)

;;(define (logistic-growth  dt domain 0  value capacity  rvalue)
  
  

(sclos-model-body <ecoservice>
						(kdnl* "[" (my 'name) ":" (class-name-of self) "]" 'model-bodies "In " (class-name-of self) t)
						(let ((h (slot-ref self 'history)))
						  (if h
								(slot-set! self 'history (cons (cons t (my 'value)) h)))
						  )
								

						(if (my 'do-growth) ;; may be suppressed in dynamic-patches, for example
							 (begin
								;;(dnl "Running <ecoservice> model body for " (my 'name))
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

										 (newvalue (growth-model t dt value))
										 )
								  ;;(dnl* (name (my 'patch)) "/" (my 'name) "value =" value "| newvalue =" newvalue)
								  
								  (set-my! 'value  newvalue)
								  )
								))
						(parent-body)
						dt
						)


;;;;--- ecoservice model-body
;;(sclos-model-body <ecoservice>
;;						(let* ((my-capacity (my 'capacity))
;;								 (ipt (/ (my 'value) my-capacity)) ;; This is a logistic update
;;								 (pt (inverse-sigmoid ipt))
;;								 (rdt (/ dt (my 'delta-T-max)))
;;								 (ov (my 'value))
;;								 )
;;						  (set-my! 'value (* my-capacity (sigmoid (+ pt rdt))))
;;						  (kdnl* 'landscape-values "[" (my 'name) ":" (class-name-of self) "]" "<ecoservice>: " == " (my 'value) "/" my-capacity))
;;						(parent-body)
;;						dt)


(sclos-method <ecoservice> (radius self)
				  (radius (my 'patch)))

(sclos-method (<ecoservice> <number>)(set-radius! self r)
				  (set-radius! (my 'patch) r))

(sclos-method <ecoservice> (location self)
				  (location (my 'patch)))

(sclos-method <ecoservice> (log-data self logger format caller targets file . p)
				  (let ((file (slot-ref logger 'file)))
					 (kdnl* '(log-* log-ecoservice) "[" (my 'name) ":" (class-name-of self) "]" "in log-data")
					 (let ((leading-entry #f))
						(for-each
						 (lambda (field)
							(kdnl* '(log-* log-ecoservice) "[" (my 'name) ":" (class-name-of self) "]" "checking" field)
							(if (has-slot? self field)
								 (let ((r (slot-ref self field)))
									(case format
									  ((ps)
										(file 'show (string-append (if (string? r) r (object->string r)) " "))
										)
;								 ((dump)
;								  (with-output-to-port file
;										(lambda ()
;										  (dump self))))

									  ((text table dump)
										(let ((show-field-name (slot-ref logger 'show-field-name))
												(missing-val (slot-ref logger 'missing-val))
												)
										  (if show-field-name
												(begin
												  (if leading-entry 
														(display " " file)
														(set! leading-entry #t))
												  (display field file)))
										  
										  (let ((val (if (eq? field 'name) 
															  (if (slot-ref self 'patch)
																	(string-append (slot-ref (slot-ref self 'patch) 'name)":" (name self))
																	(name self))
															  (if (has-slot? self field)
																	(slot-ref self field)
																	(slot-ref logger 'missing-val)))))
											 (if leading-entry 
												  (display " " file)
												  (set! leading-entry #t))
											 (display val file))
										  )
										)

									  (else
										(kdnl* '(log-* log-ecoservice) "[" (my 'name) ":" (class-name-of self) "]" "Ignoring " field " because I don't have it")
										'ignore-unhandled-format)))
								 (begin
									(kdnl* '(log-* log-ecoservice) "[" (my 'name) ":" (class-name-of self) "]" "no service" field)
									#f)))
						 (unique (if #t targets (filter (not-member (slot-ref logger 'dont-log)) targets)))
						 )
						(newline file)
						)
					 ))



;--- <patch> methods
;; 
;; min-bound, max-bound contains? services 

;(define service? (make-generic))
;(define add-service (make-generic))
;(define remove-service (make-generic))
;(define service-list (make-generic))
;(define service (make-generic))
;(define services (make-generic)) ;; returns value
;(define set-services! (make-generic)) ;; sets value
;(define value (make-generic))      -- defined for environment
;(define set-value! (make-generic)) -- defined for environment


(sclos-method <patch> (dump self . count)
				  (set! count (if (null? count) 0 (car count)))

				  (display (make-string count #\space))
				  (display "<patch>\n")
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


;---- (initialize...) 
(add-method initialize
				(make-method (list <patch>)
								 (lambda (initialize-parent self args)
									(initialise self '()) 
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))

;---- (min-bound...) & (max-bound...)
(sclos-method <patch> (min-bound self)
				  (let ((loc (my 'location))) 
					 (map - loc (make-list (length loc) (my 'radius)))))

(sclos-method <patch> (max-bound self)
				  (let ((loc (my 'location))) 
					 (map + loc (make-list (length loc) (my 'radius)))))

(sclos-method <patch> (radius self)
				  (my 'radius))

(sclos-method (<ecoservice> <number>)(set-radius! self r)
				  (my-set! 'radius r))

(sclos-method <patch> (location self)
				  (my 'location))



;---- (distance-to-centre...) returns the distance to the centre of the patch
(sclos-method (<patch> <pair>) (distance-to-centre self loc)
				  (let ((sqr (lambda (x) (* x x))))
					 (sqrt (apply + (map sqr (map - (list-head (my 'location) 2) (list-head loc 2)))))))

;---- (distance-to-interior...) returns the distance to the centre of the patch
(sclos-method (<patch> <pair>) (distance-to-interior self loc)
				  (let* ((sqr (lambda (x) (* x x)))
							(R (- (sqrt (apply + (map sqr (map - (list-head (my 'location) 2) (list-head loc 2))))) (my 'radius)))
							)
					 (if (< R 0) 0 R)))

;---- (contains?...) predicate to indicate if something is in the patch
(sclos-method <patch> (contains? self . bit)
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

;;(sclos-method (<patch> <thing>) (contains? self entity)
;;				  (contains? (location entity))
;;				  )


;--- (services...) returns services matching the sym or in the symlist

(sclos-method (<patch>) (service-list self . ss)
				  (if (and (pair? ss) (pair? (car ss))) (set! ss (car ss)))
				  (let ((S (my 'service-list)))
					 (if (null? ss)
						  S
						  (filter (lambda (x) (or (member (type x) ss) (member (name x) ss))) S))))


(sclos-method (<patch>) (services self . ss)
				  (if (null? ss)
						(map (lambda (x) (type x)) (my 'service-list))
						(map type (apply service-list (cons self ss)))))


(sclos-method (<patch>) (specific-services self . ss)
				  (if (null? ss)
						(map (lambda (x) (name x)) (my 'service-list))
						(map type (apply service-list (cons self ss)))))


;--- (service?...) queries if a service is present
(sclos-method (<patch> <symbol>) (service? self sym)
				  (not (null? (services self sym))))

(sclos-method (<patch> <pair>) (service? self symlist)
				  (not (null? (services self symlist))))


;--- (set-services!...) sets the value of the services list
(sclos-method (<patch> <pair>) (set-services! self servlist)
				  (set-my! 'service-list servlist))


;--- (add-service...) adds a service to a patch

(sclos-method (<patch> <ecoservice>) (add-service self new-service)
				  (set-services! self (append (my 'service-list) (list new-service))))


;--- (remove-service...) removes all services that match the predicate in a patch
;;                       the predicate will probably be something like (using-name-keep? 'wobble)
(sclos-method (<patch> <procedure>) (remove-service self predicate)
				  (set-services! self (filter predicate (my 'service-list))))


(sclos-method (<patch> <symbol>) (value self servlist)
				  (set! servlist (list servlist))
				  (let ((sl (if (member #t servlist) (my 'service-list) (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map value sl)))))

(sclos-method (<patch> <string>) (value self servlist)
				  (set! servlist (list servlist))
				  (let ((sl (if (member #t servlist) (my 'service-list) (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map value sl)))))

(sclos-method (<patch> <symbol>)(extra-variable self field)
				  (value self (symbol->string field)))

(sclos-method (<patch> <string>)(extra-variable self field)
				  (value self field))

(sclos-method (<patch>) (extra-variable-list self)
				  (map string->symbol (map name (my 'service-list))))

;; (add-method representation
;; 				(make-method (list <agent>)
;; 								 (lambda (representation-parent self)
;; 									(my 'representation))))



(sclos-method (<patch> <pair>) (value self servlist)
				  (let ((sl (if (member #t servlist) (my 'service-list) (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map value sl)))))

(sclos-method (<patch> <pair>) (capacity self servlist)
				  (let ((sl (if (member #t servlist) (my 'service-list) (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map capacity sl)))))

(sclos-method (<patch> <pair>) (mean-value self servlist)
				  (let ((sl (service-list self servlist)))
					 (if (null? sl)
						  0
						  (/ (apply + (map value sl)) (* 1.0 (length servlist))))))

;;;--- (set-value!...)
(sclos-method (<patch> <symbol>) (set-value! self sym val)
				  (let ((s (filter (lambda (a) (eq? sym (type a))) (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (set-value! x val)) s)
							 #t))))

(sclos-method (<patch> <string>) (set-value! self sym val)
				  (let ((s (filter (lambda (a) (string=? sym  (name a))) (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (set-value! x val)) s)
							 #t))))

;--- (scale!...)
(sclos-method (<patch> <symbol> <number>) (scale! self sym val)
				  (let ((s (filter (lambda (a) (eq? (type a) sym))  (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (scale! x val)) s)
							 #t))))

(sclos-method (<patch> <string> <number>) (scale! self sym val)
				  (let ((s (filter (lambda (a) (string=? (name a) sym))  (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (scale! x val)) s)
							 #t))))

;--- (add!...)
(sclos-method (<patch> <symbol> <number>) (add! self sym val)
				  (let ((s (filter (lambda (a) (eq? (type a) sym)) (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (add! x val)) s)
							 #t))))

(sclos-method (<patch> <string> <number>) (add! self sym val)
				  (let ((s (filter (lambda (a) (string=? (name a) sym)) (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (add! x val)) s)
							 #t))))

;--- (scale!...)
(sclos-method (<patch> <pair> <number>) (scale! self symlist val)
				  (for-each (lambda (x) (scale! self x val)) symlist))


;--- (add!...)
(sclos-method (<patch> <pair> <number>) (add! self sym val)
				  (for-each (lambda (x) (add! self x val)) symlist))


;--- (total-value ...) ;; needs to filter the services by membership in the indicated symlist


(sclos-method (<patch> <pair>) (total-value self symlist)
				  (let ((ss (service-list self (if (symbol? symlist) (list symlist) symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (value y)) ss)))))

(sclos-method (<patch> <pair>) (total-value self symlist)
				  (let ((ss (service-list self (if (symbol? symlist) (list symlist) symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (value y)) ss)))))

(define (total-patch-list-value patchlist symlist)
  (map (lambda (x) (total-value x symlist)) patchlist))

(sclos-method (<patch> <pair>) (total-capacity self symlist)
				  (let ((ss (service-list self (if (symbol? symlist) (list symlist) symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (capacity y)) ss)))))

(sclos-method (<patch> <pair>) (total-capacity self symlist)
				  (let ((ss (service-list self (if (symbol? symlist) (list symlist) symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (capacity y)) ss)))))

(define (total-patch-list-capacity patchlist symlist)
  (map (lambda (x) (total-capacity x symlist)) patchlist))

(sclos-model-body <patch>
						(kdnl* 'model-bodies "In " (class-name-of self) (name self) "@" t)

						;; this does the growth  and endemic mortality
						(if (member 'nested-habitat nested-agents)
							 (for-each (lambda (x)
											 (run x t (+ t dt) (my 'kernel))
											 )	
										  (service-list self)))
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						(parent-body)
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						dt
						)


(sclos-method (<patch> <agent> <symbol> <agent> <list>) (log-data self logger format caller targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'map-projection)))
					 (if (or (not p) (null? p)) (set! p (lambda (x) x)))
					 (kdnl* '(log-* log-patch) "[" (my 'name) ":" (class-name-of self) "]" "in log-data")
				  
					 (case format
						((ps)
						 (let* ((symlist (services h))
								  (name (slot-ref h 'name))
								  )
							
							(if adjust-grey (file 'setgray patchgrey))
							(ps-circle file  (p (radius self)) (p (list-head (location self) 2)) 0.7 0.0)
							
							
							(let* ((slist (slot-ref self 'service-list))
									 (n (1+ (length slist))) ;; slist is becoming circular?? ....******
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
							  (file 'show-table (map (lambda (x) (string-append (slot-ref x 'name) ": " (pno (value x)))) slist))
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



;-- dynamic-patch methods

(add-method initialize
				(make-method (list <dynamic-patch>)
								 (lambda (initialize-parent self args)
									(initialise self (list 'population-names '() 'population-symbols '()
																  'd/dt-list '()
																  'do-dynamics #t
																  'population-definitions '()	
																  'subdivisions 12 ;; because 12 is a nice number?
																  ))
													
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))

(sclos-method (<patch> <procedure> <symbol> <procedure>)  (log-data self logger format caller targets)
				  (display "dynamic patch log")
				  (log-data-parent))
				  

(sclos-method (<dynamic-patch> <string>) (service-list-index self service)
				  (let* ((si (my 'population-names))
							(n (length si))
							(ix (member service si))
							(i (if ix  (- n (length ix)) #f)))
						  i))

(sclos-method (<dynamic-patch> <symbol>) (service-list-index self service)
				  (let* ((si (my 'population-symbols))
							(n (length si))
							(ix (member service si))
							(i (if ix  (- n (length ix)) #f)))
						  i))

(sclos-method (<dynamic-patch> <pair>) (service-list-index self service)
				  (map (lambda (x) (service-list-index self x)) service))


;; for predation matrix stuff ...
(sclos-method (<dynamic-patch> <symbol>) (service-matrix-index self service)
				  (let ((si (service-list-index self service)))
					 (if si (1+ si) si)))

(sclos-method (<dynamic-patch> <pair>) (service-matrix-index self service)
				  (map (lambda (x) (service-matrix-index x)) service))

(sclos-method (<dynamic-patch> <pair>) (service-values self)
				  (map (lambda (x) (value self x)) (my 'service-update-map)))	

(sclos-method <patch> (dump self . count)
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


(sclos-method (<dynamic-patch>  <procedure> <symbol> <procedure>)(log-data self logger format caller targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'map-projection)))
					 (if (or (not p) (null? p))  (set! p (lambda (x) x)))
					 (kdnl* '(log-* log-patch) "[" (my 'name) ":" (class-name-of self) "]" "in log-data")
					 
					 (case format
						((ps)
						 (let* ((symlist (services h))
								  (name (slot-ref h 'name))
								  )

							(if adjust-grey (file 'setgray patchgrey))
							(ps-circle file  (p (radius self)) (p (list-head (location self) 2)) 0.7 0.0)


							(let* ((slist (slot-ref self 'service-list))
									 (n (1+ (length slist))) ;; slist is becoming circular?? ....******
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
							  (file 'show-table (map (lambda (x) (string-append (slot-ref x 'name) ": " (pno (value x)))) slist))
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





(sclos-method <dynamic-patch> (enable-service-growth! self service-name)
				  (enable-growth! (service self service-name)))

(sclos-method <dynamic-patch> (disable-service-growth! self)
				  (disable-growth! (service self service-name)))


(sclos-method <dynamic-patch> (enable-all-service-growth! self)
				  (for-each enable-growth! (service-list self)))

(sclos-method <dynamic-patch> (disable-all-service-growth! self)
				  (for-each disable-growth! (service-list self)))

(sclos-method <dynamic-patch> (enable-growth! self) (set-my! 'do-dynamics #t))
(sclos-method <dynamic-patch> (disable-growth! self) (set-my! 'do-dynamics #f))

(sclos-method <dynamic-patch> (growth-model self) (d/dt-list self))


;; this expects a list of functions which return reals
(sclos-method <dynamic-patch> (set-population-dynamics! self . d/dt-list)
				  (slot-set! self 'population-definitions #f)
				  (if (null? d/dt-list)
						(abort!))
				  (if (pair? (car d/dt-list))
						(set! d/dt-list (car d/dt-list))) ;; a list of functions was passed in the "rest" part of the line
				  
				  (if (andf (map procedure? d/dt-list))
						(slot-set! self 'd/dt-list d/dt-list)
						(abort!)))
						
(sclos-method <dynamic-patch> (define-population-dynamics! self . defns )
				  (slot-set! self 'd/dt-list #f)

				  (if (not (and (pair? (car defns)) (pair? (car defns))) (or (string? (caar defns)) (symbol? (caar defns))))
						(abort (string-append
								  " the format for defining a system of populations is:\n"
								  "(set-population-dynamics! self \n"
								  "   (grass dgrass/dt)\n"
								  "   (rabbit drabbit/dt)\n"
								  "   (fox dfox/dt)\n"
								  "   (bear dbear/dt))"
														)
								 )
						)
				  

				  (let ((pn (map caar defns))
						  (pf (map (lambda (x) 
										 (let* ((namn (list-ref x 0))
												  (dp/dt (list-ref x 1))
												  (p (make-population namn))
												  )
											(p 'register-populations pn)
											(p 'set-d/dt! dp/dt)
											p
											)
										 )
									  defns))
						  )
					 (set-my! population-names pn)
					 (set-my! population-d/dt pf)
					 )
				  )


(sclos-model-body <dynamic-patch>
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

						(if (<= dt 1e-12) (abort "Bad dt passed to <dynamic-patch> sclos-model-body"))
						(let ((pop-values (map (lambda (x) (value self x)) (my 'population-names)))
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
								  ;;(dnl "Got past the rk4* for " (my 'name) " and " (my 'population-names))

								  ;;(set-my! 'dP dP) ;; Don't really *need* this, except perhaps for debugging
								  (let ((deltas (dP (+ t dt))))
									 (for-each 
									  (lambda (x v)
										 (if (not (zero? (imag-part v)))
											  (abort "Someone has been doing recreational pharmacology"))
										 ;;(dnl (class-name-of self))
										 

										 (add! self x (- v (value self x))) ;; These are the adjustments due to consumption and predation
										 ;;(slot-set! self x v)
										 (if (< (value self x) 0.0) (set-value! self x 0.0))
										 )
									  (my 'population-names) deltas))
								  )

								(aborts (symbol->string (class-name-of self))
										  " has no population names defined!"))
						  )

						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						(parent-body)
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						dt)


;-- <landscape> methods

;;							 (filter (lambda (x) (and (contains? x loc) arg) ) (my 'patch-list)))

;; Default landscape only has the default value, oddly enough
(sclos-method (<landscape> <pair>) (value self loc)
				  (if (contains? self loc)
						((my 'terrain-function) loc)
						(my 'default-value)))

(sclos-method (<landscape> <pair>) (capacity self loc)
				  (if (contains? self loc)
						((my 'terrain-function) loc)
						(my 'default-value)))

;; This is to keep the "run" chain consistent
(sclos-model-body <landscape>
						(kdnl* 'model-bodies "In " (class-name-of self) t)
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						(parent-body)
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						dt
						)



;;(sclos-model-body <landscape> 
;;						(kdnl* 'running (my 'name) ":" (my 'representation) " is running")
;;						(for-each (lambda (x)
;;										(run-model-body x t dt))
;;									 (my 'patch-list))
;;						(parent-body)
;;						dt)



;-- <habitat> methods

(sclos-method <habitat> (agent-prep self . args)
				  (agent-prep-parent)
				  ;(slot-set! self 'variables (unique* (apply append (map extra-variable-list (slot-ref self 'patch-list)))))
)



(add-method initialize
				(make-method (list <habitat>)
								 (lambda (initialize-parent self args)
									(initialise self (list 'scale #f))
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))

(sclos-method <habitat> (dump self . count)
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


(sclos-method (<habitat> <patch>) (add-patch self patch)
				  (set-my! 'patch-list (cons patch (my 'patch-list))))

(sclos-method (<habitat> <procedure>) (remove-patch self pfilter)
				  (set-my! 'patch-list (filter pfilter (my 'patch-list))))

;--- (services...) returns services matching the sym or in the symlist

(sclos-method <habitat> (services self . ss)
				  (if (not (null? ss))
						(begin
						  (if (not (symbol? (car ss))) (set! ss (car ss)))
						  (filter (lambda (x) (member x ss)) (services self))
						  )
						(unique (map string->symbol 
									  (sort (map symbol->string 
													 (apply append (map services 
																			  (patch-list self)))) string<?)))
						))


(sclos-method (<habitat>) (service-list self . ss)
					 (unique*
					  (if (null? ss) 
							(apply append (map (lambda (x) (service-list x)) (patch-list self)))
							(apply append 
									 (map (lambda (x) 
											  (service-list x 
																 (if (symbol? (car ss)) ss (car ss))))
											(patch-list self))))))



(sclos-method (<habitat>) (service-list self)
				  (let* ((P (patch-list self))
							(S (map service-list P)))
					 (apply append S)))


;--- (service?...) queries if a service is present
(sclos-method (<habitat> <symbol>) (service? self sym)
				  (not (null? (services self sym))))


(sclos-method (<habitat> <pair>) (service? self symlist)
				  (not (null? (services self symlist))))



(sclos-method (<habitat> <symbol>) (service-sites self sym)
				  (let loop ((rslt '())
								 (pl (my 'patch-list)))
					 (cond
					  ((null? pl) rslt)
					  ((service? (car pl) sym)
						(loop (cons (car pl) rslt) (cdr pl)))
					  (else (loop rslt (cdr pl))))))


(sclos-method (<habitat> <pair>) (service-sites self symlist)
				  (let loop ((rslt '())
								 (pl (my 'patch-list)))
					 (cond
					  ((null? pl) rslt)
					  ((service? (car pl) symlist)
						(loop (cons (car pl) rslt) (cdr pl)))
					  (else (loop rslt (cdr pl))))))




(sclos-method (<habitat>) (patch-list self . arg)
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


(sclos-method (<habitat> <pair> <number> <pair>) (aggregate-value self location radius servicelist)
				  (let* ((sl (service-sites self servicelist))
							(lsl (filter (lambda (patch) 
												(>
												 (intersection-of-two-circles 
												  (distance-to-centre patch location)
												  radius (slot-ref patch 'radius))
												 0.0))
											 sl))
							(lslv (if (null? lsl) 
										 0.0
										 (apply + (map (lambda (patch)
															  (* (value patch servicelist)
																  (overlap-decay
																	0.0 ;; 1.0 gives us 1% of the pop at the radius, 0 gives us a uniform dist
																	(distance (list-head (slot-ref patch 'location) 2) location)
																	radius 
																	#t
																	0.0
																	(slot-ref patch 'radius)	
																	#t)
																  ))
															lsl))))
							)
					 lslv))


;(define (I->E f) (inexact->exact (round f)))
(define (I->E f) (inexact->exact (truncate f)))

(define (repro xy res m)
  (map I->E (map (lambda (x) (/ x res)) (map - xy m))))


(sclos-method <habitat> (min-bound self)
				  (let* ((v (map min-bound (slot-ref self 'patch-list)))
							(vx (apply min (map car v)))
							(vy (apply min (map cadr v)))
							)
					 (list vx vy)))

(sclos-method <habitat> (max-bound self)
				  (let* ((v (map max-bound (slot-ref self 'patch-list)))
							(vx (apply max (map car v)))
							(vy (apply max (map cadr v)))
							)
					 (list vx vy)))

;;1.0/(1.0 + exp(-2*pi*l*(2*(x+(0.5 - off)) - 1.0)) )
;; l = 1.0, off = 0.5



(define (def-res H)
  (let* ((m (min-bound H))
			(M (max-bound H))
			(extent (map - M m))
			)
	 (/ (apply min extent) 20.0)))

(define print-environment-data
  (lambda (ps p x n ns loc rad)
	 ;;(dnl (pno (value x)))
	 (ps 'moveto (map p (map + (list-head loc 2) (map p (list (* 1.0 rad) (* (- (/ ns 2.0) n) 1.0))))))
	 (if adjust-grey (ps 'setgray PATCHGREY)) ;; zero is white...
	 (ps 'Helvetica 7)
	 (ps 'show (string-append (slot-ref x 'name) ": "  (number->string (value x))))
	 ))


(define crop-caption
  (lambda (ps p x . pt)
	 (if (null? pt) (set! pt 10))
	 (let ((loc (map p (list-head (location x) 2)))
			 (rad (p (radius x))))
		(ps 'moveto (map - loc (list (* 0.5 rad)  (* -1 (+ 5 (* 1 rad) )))))
		(ps 'Helvetica pt)
		(if adjust-grey (ps 'setgray PATCHGREY))
		(ps 'show-right (string-append (slot-ref x 'name) " at " (number->string (slot-ref x 'subjective-time))))
		)
	 )
  )



;; p is usually something like mm->points

(sclos-method (<habitat> <procedure>) (map-log-data self logger format caller targets)
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

(sclos-method (<habitat> <procedure> <symbol> <procedure>) (log-data self logger format caller targets)
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

(sclos-method (<habitat>) (spatial-scale self)
				  (if (not (my 'scale))
						(let ((lscale (apply append (map (lambda (x) (map (lambda (y) (distance (location x) (location y))) (patch-list self))) (patch-list self)))))
						  (set-my! 'scale (/ (apply + lscale) (1+ (- (length lscale) (length (my 'patch-list)))))) ;;  This gets rid of the "self-distances" which are zero
						  )
						)
				  (my 'scale))


(sclos-model-body <habitat>
						;;(dnl* "HABITAT: model times min/mean/max/subjective = " (kernel 'min-time) "/" (kernel 'mean-time)"/" (kernel 'max-time)"/" (my 'subjective-time) " + " dt)
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
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt) 'A)
						(parent-body)
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt) 'A)
						dt
						)

(define (make-patch loc radius services . args) ;; services is a list of lists, with each inner list consisting of a name (a string), a type (a symbol) and a value (typically a number, capacity)  and another number (typically the delta-T-max
  (let* ((loc loc) 
			(radius radius)
			(service-update-map (if (null? args) '() (car args)))
			(update-equations (if (null? args) '() (cadr args)))
			(P (make <dynamic-patch> 'location loc 'radius radius 'type 'patch 'representation 'patch))
			)
	 (if (not (null? services))
		  (let ((sl (map 
						 (lambda (n t q) (make <ecoservice> 'patch P 'name n 'type t 'capacity q )
									)
						 (map car services)
						 (map cadr services)
						 (map caddr services)
						 (map cadddr services)
						 )))
			 (set-services! P sl)
			 (slot-set! P 'service-update-map service-update-map )
			 (slot-set! P 'update-equations update-equations)
			 ))
	 
	 P))



(sclos-method (<habitat> <symbol>)(extra-variable self field)
				  (value self (symbol->string field)))


(sclos-method (<habitat>) (extra-variable-list self)
				  (let ((patch-vars (unique* (apply append (map extra-variable-list (my 'patch-list)))))
						  )
					 ;;(dnl* "HABITAT PATCH VARIABLES:" patch-vars)
					 (unique* (append (list 'name 'subjective-time) patch-vars)))) ;; returns a list of symbols


;; domain is a list  ((minx miny minz) (maxx maxy maxz))
;; patch-data is a list of  services lists for make-patch

(define (make-habitat name default-ht domain terrain-function 
							 patch-data
							 )
  (let* ((PL (map (lambda (x) (apply make-patch (list (apply random-location domain) (map (lambda (y) (* 0.25 x)) (apply min (map - maxv minv))) x))) patch-data))
			(H (make <habitat> 'name name 'default-value default-ht 'minv (car domain) 'maxv (cadr domain) 'terrain-function terrain-function 
						'patch-list PL))
			)
	 H)
  )

(define (patchsize domain)
  (* 0.25 (apply min (map - (list-head (cadr domain) 2) (list-head (car domain) 2)))))



(define (add-habitat-to-queue Q h) ;; returns the queue, so us it like (set! Q (add-...-queue Q hab))
  (let ((p (patch-list h))
		  (s (service-list h))
		  )
	 (unique* (append Q (list h) p s))))


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
									(< (/ (total-value (cdr x) ecoserv) (1+ (car x)))
										(/ (total-value (cdr y) ecoserv) (1+ (car y)))
										))
								 )
							)))

		  (if (null? sdists) #f (map (lambda (x) (list (car x) (cdr x))) sdists))))
	 ))



;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
