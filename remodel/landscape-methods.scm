(include "remodel-framework")
;-  Identification and Changes

;--
;	landscape.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.19
;		Location: odin:/home/gray/study/src/new/landscape.scm
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


;;(define (logistic-growth  dt domain 0  value capacity  rvalue)

;(define service? (make-generic))
;(define add-service! (make-generic))
;(define remove-service! (make-generic))
;(define service-list% (make-generic))
;(define service (make-generic))
;(define services (make-generic)) ;; returns value
;(define set-services! (make-generic)) ;; sets value
;(define value (make-generic))      -- defined for environment
;(define set-value! (make-generic)) -- defined for environment

;--- oriented toward habitats, patches  and ecoservices
;---- ecoservice lists, nearest suppliers....

(UNFINISHED-BUSINESS "The logging of subsidiary agents is not sorted out yet")

(define (locate-nearest-ecoserv domain ecoserv loc)
  (let* ((patches (service-sites domain ecoserv))
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

(define (sorted-ecoservices domain ecoserv loc . weighted-by-value)
  (let* ((patches (service-sites domain ecoserv)))
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
;---- For domainss

(define (def-res H)
  (let* ((m (min-bound H))
			(M (max-bound H))
			(extent (map - M m))
			)
	 (/ (apply min extent) 20.0)))



;---- Generating postscript

(define print-environment-data
  (lambda (ps p x n ns loc rad)
	 (ps 'Comment "in print-environment-data")
	 (ps 'moveto (map p (map + (list-head loc 2)
									 (map p (list (* 1.0 rad)
													  (* (- (/ ns 2.0) n) 1.0))))))
	 (if adjust-grey (ps 'push-color PATCHGREY)) ;; zero is white...
	 (ps 'push-font 'Helvetica 7)
	 (ps 'show (string-append " " (slot-ref x 'name) ": "
									  (number->string (value x))))
	 (ps 'pop-font)
	 (if adjust-grey (ps 'pop-color PATCHGREY)) ;; zero is white...
	 ))


(define (crop-caption ps obj prj #!rest pt)
	 (ps 'Comment "in crop-caption")
	 (if (null? pt) (set! pt 10) (set! pt (car pt)))
	 (let ((loc (prj (list-head (location obj) 2)))
			 (rad (car (prj (make-list 2 (radius obj))))))
		(ps 'moveto (map - loc
							  (list (* 0.5 rad)  (* -1 (+ 5 (* 1 rad) )))))
		(ps 'moveto (map - loc
							  (list (* 0.5 rad)  (* -1 (+ 5 (* 1 rad) )))))

		(ps 'push-font 'Helvetica 7)
		(if adjust-grey (ps 'push-color PATCHGREY))
		(if #f
			 (ps 'show-right (string-append " "
									(slot-ref obj 'name) " at "
									(number->string (slot-ref obj 'subjective-time))))
			 (ps 'show (string-append " "
							(slot-ref obj 'name) " at "
							(number->string (slot-ref obj 'subjective-time)))))
		(ps 'pop-color)
		(ps 'pop-font)
		)
	 )

(define (caption ps obj prj loc #!rest pt)
	 (ps 'Comment "in caption")
	 (if (null? pt) (set! pt 8) (set! pt (car pt)))

	 (if loc
		  (if prj
				(ps 'moveto (prj loc))
				(ps 'moveto loc)) )
	 
	 (ps 'push-font 'Helvetica 9)
	 (if adjust-grey (ps 'setgray PATCHGREY))
	 (if #f
		  (ps 'show-right (string-append " "
								 (slot-ref obj 'name) " at "
								 (number->string (slot-ref obj 'subjective-time))))
		  (ps 'show (string-append " "
						 (slot-ref obj 'name) " at "
						 ((if #t scaled-time number->string) (slot-ref obj 'subjective-time))
						 )))
	 (ps 'pop-font)
	 )


;--- (make-population-transform-func predation-matrix efficiency-matrix
;                   service-data-list ecoservice-template)

;; This returns a *function*, NOT a class.
(define (make-population-transform-func predation-matrix efficiency-matrix
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
	 (let ((population-transform-func
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
		population-transform-func)
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

;--- (make-population-transform-func predation-matrix efficiency-matrix
;                               service-data-list ecoservice-template)

;;; For examples look at savannah-parameters....


;-- <environment> methods and bodies

;; Stops things going off the rails 

;--- (services...) returns services matching the sym or in the symlist
;;(default-agent-initialisation <environment>)

(model-method (<environment>) (services% self syms)
				  '())

(model-body% <environment>
				(call-all-parents)
				dt)

(model-method (<environment> <point>) (contains? self loc)
				  (if (member (my 'rep) '(() #f <uninitialised> <uninitialised>))
						(let ((m (map <= loc (my 'minv)))
								(M (map <= (my 'maxv) loc)))
						  (apply andf (append m M)))
						((my 'rep) (contains? self loc))))


(model-method (<environment> <point>) (contains? self loc)
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


(model-method (<environment>) (random-point self #!optional alien) ;; returns a random location in the environment
				   ;;(let ((n (my 'minv)))
				  ;;  (map + (map * (map random-real n) (map - (my 'maxv) (my 'minv))) (my 'minv)))
				  (let ((RP  (if #f
									  (random-location (my 'minv) (my 'maxv))
									  (let ((theta (* 2 (acos -1)))
											  ;;(r (random-real) (/ + (radius self) (Radius self)))
											  (r (* (random-real)  (radius self)))
											  )
										 (list (* r (cos theta)) (* r (sin theta)))))))
					 
					 (if alien ((composite-prj_src->dst self alien) RP) RP)
				  ))

(model-method (<environment>) (number-represented self)
				  1 ;; a single contributor to the sum
				  ;(apply * (map - maxv minv))
				  ) ;; area covered

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

When ecoservices are running externally, they maintain a list of
agents which will respond to kernel calls, or they will work using calls 
via their containing patch.
")


;;(default-agent-initialisation <ecoservice> '() 'do-growth #t 'history #f 'run-externally #f 'ext-get (lambda x #f) 'ext-set! (lambda x #f) 'external-rep-list '())

;;--- ecoservice model-body support routines (mainly about growth)

(UNFINISHED-BUSINESS "This should probably have bits disabled when there are active-subsidiary-agents")
(model-body% <ecoservice>
						(kdebug '(model-bodies ecoservice-running) (cnc self) (my 'name)  "@"  t)
						(let ((h (slot-ref self 'history)))
						  (if h
								(slot-set! self 'history
											  (cons (cons t (my 'value)) h)))
						  )
						
						;;(dnl* (my 'name) (my 'sym) (my 'value))

						(if (and (my 'do-growth)
									(null? (my 'active-subsidiary-agents)))
							 ;; if there are active subsidiaries, rely on them to do the calulations.
							 ;; Otherwise, it may be suppressed for other reasons, in dynamic-patches,
							 ;; for example
							 (begin
								;;;     (my 'name))
								(let* ((capacity (my 'capacity))
										 (value  (my 'value))
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
											 (ecoserv-growth t dt value))
											((eqv? ecoserv-growth 'sigmoid)
											 (let* (;; This is a logistic update
													  (domain (my 'delta-T-max))
													  (ipt (/ value capacity)) 
													  (pt (inverse-sigmoid* ipt))
													  (rdt (/ dt domain))
													  )
												(if (>= domain dt)
													 (* capacity (sigmoid* (+ pt rdt)))
													 (error "The specified timestep " dt " is greater than the 'delta-T-max value " domain " for " (my taxon)))
												))
											((eqv? ecoserv-growth 'linear)
											 (min capacity (+ value (* dt (my 'r)))))
											(#t
											 value)))
										 )
								  
								  (set-my! 'value  newvalue)
								  )
								))

						(call-all-parents) ;; chain to <agent>
						;;(parent-body)
						dt
						)


;; 
(define (simple-ecoservice taxon dt nm variable value cap r maxdt growing? growthmodel . patch)  
  (if (pair? patch)
		(set! patch (car patch)))
		
  (if (not (and (string? taxon) (string? nm) (symbol? variable ) (number? value)
					 (number? cap) (number? r) (number? maxdt) (boolean? growing?)
					 (or (symbol? growthmodel) (procedure? growthmodel))))
			  (error "Type error in call to simple-ecosystem: "
						(cond
						 ((not (string? taxon))  (string-append "taxon " (object->string taxon)))
						 ((not (string? nm))  (string-append "name " (object->string nm)))
						 ((not (symbol? variable))  (string-append "variable " (object->string variable)))
						 ((not (number? value))  (string-append "value " (object->string value)))
						 ((not (number? cap))  (string-append "cap " (object->string C)))
						 ((not (number? r))  (string-append "r " (object->string r)))
						 ((not (number? maxdt))  (string-append "maxdt " (object->string maxdt)))
						 ((not (boolean? growing?)) (string-append "growing?" (object->string growing?)))
						 ((not (or (null? growthmodel) (symbol? growthmodel) (procedure? growthmodel))) "growthmodel")
						 (#t "please check the arguments")))
			  )
    
  (let ((A (make-agent <ecoservice> taxon
						 'dt dt
						 'name (string-append (name patch) ":" (strsub nm " " "_"))
						 ;; string corresponding to its name, like "Vulpes lagopus"
						 'sym variable
						 ;; 'Vl perhaps
						 'value value
						 ;; Immediately after model-body this will be the value calculated, before hand it is the value to be used
						 'capacity cap
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
						 'patch patch
						 )

			  ))
;;	 (dnl "Made ecoservice for " name)
	 (slot-set! A 'provides (list variable))
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


(model-method <ecoservice> (number-represented self)
				  (slot-ref self 'value))



(model-method (<ecoservice>) (capacity self)
				  (my 'capacity))


(model-method (<ecoservice>) (growth-model self)
				  (my 'growth-model))

;--- adjustment

(model-method (<ecoservice>) (disable-growth! self) (set-my! 'do-growth #f))
(model-method (<ecoservice>) (enable-growth! self) (set-my! 'do-growth #t))

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
				  (abort 'you-should-call-the-patch)
				  (set-radius! (my 'patch) r))

(model-method <ecoservice> (location-needs-location@ self) #t)

(model-method <ecoservice> (location@ self)
				  (location@ (my 'patch)))

(model-method <ecoservice> (area self #!optional passing)
				  (area (my 'patch) passing))

(model-method (<ecoservice> <log> <symbol>) (log-data self logger format targets)
				  (dnl* "(model-method (<ecoservice> <log> <symbol>) (log-data self logger format targets)")
				  (let ((kdebug (if #t kdebug dnl*))
						  ;;(f (if (pair? args) (car args) #f))
						  ;;(p (if (and (pair? args)
						  ;;			  (pair? (cdr args)))
						  ;;		(cadr args)
						  ;;		#f))
						  )
					 (kdebug '(log-horrible-screaming ecoservice log-ecoservice) (cnc self) (cnc format) (cnc (my 'name)))
					 (if (or (my 'always-log) (and (member format '(ps)) (my 'always-plot)) (emit-and-record-if-absent logger self (my 'subjective-time)))
						  (let ((file (slot-ref logger 'file)))
							 (kdebug '(log-* log-ecoservice)
										"[" (my 'name) ":" (cnc self) "]"
										"in log-data")
							 (let ((leading-entry #f))
								(for-each
								 (lambda (field)
									(kdebug '(log-* log-ecoservice) "[" (my 'name) ":"
											  (cnc self) "]" "checking" field)
									(if (has-slot? self field)
										 (let ((r (slot-ref self field)))

											(case format
											  ((ps)
												(file 'push-font (my 'default-font) (my 'default-size))
												(if (and (my 'always-log) (< t (my 'subjective-time)))
													 (file 'show "["))
												(file 'show (string-append " "
																					(if (string? r)
																						 r
																						 (object->string r)) " "))
												(if (and (my 'always-log) (< t (my 'subjective-time)))
													 (file 'show "]"))
												(file 'pop-font)
												)
;								 ((dump)
;								  (with-output-to-port file
;										(lambda ()
;										  (dump self))))

											  
											  ;;; ((text table dump)
											  ;;; 	(let ((S (with-output-to-string '()
											  ;;; 											  (lambda ()
											  ;;; 												 (let ((show-field-name
											  ;;; 														  (slot-ref logger 'show-field-name))
											  ;;; 														 (missing-val
											  ;;; 														  (slot-ref logger 'missing-val))
											  ;;; 														 )
											  ;;; 													(if show-field-name
											  ;;; 														 (begin
											  ;;; 															(if leading-entry 
											  ;;; 																 (display " ")
											  ;;; 																 (set! leading-entry #t))
											  ;;; 															(display field)))
																								
											  ;;; 													(let ((val (if (eqv? field 'name) 
											  ;;; 																		(if (slot-ref self 'patch)
											  ;;; 																			 (string-append
											  ;;; 																			  (slot-ref
											  ;;; 																				(slot-ref self 'patch) 
											  ;;; 																				'name) ":" (name self))
											  ;;; 																			 (name self))
											  ;;; 																		(if (has-slot? self field)
											  ;;; 																			 (slot-ref self field)
											  ;;; 																			 (slot-ref logger
											  ;;; 																						  'missing-val)))))
											  ;;; 													  (if leading-entry 
											  ;;; 															(display " " file)
											  ;;; 															(set! leading-entry #t))
											  ;;; 													  (display val))
											  ;;; 													)
											  ;;; 												 )
											  ;;; 											  )))
												  
											  ;;; 	  (display S file)))
											  ((text table dump)
												(let ((show-field-name
														 (slot-ref logger 'show-field-name))
														(missing-val
														 (slot-ref logger 'missing-val))
														)
												  (if show-field-name
														(begin
														  (if leading-entry 
																(file 'show " ")
																(set! leading-entry #t))
														  (file 'show field)))
												  
												  (if (and (my 'always-log) (< t (my 'subjective-time)))
														(file 'show "["))
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
														  (file 'show " ")
														  (set! leading-entry #t))
													 (file 'show val))
												(if (and (my 'always-log) (< t (my 'subjective-time)))
													 (file 'show "]"))
												  
												  )
												)
											  
											  (else
												(kdebug '(log-* log-ecoservice)
														  "[" (my 'name) ":" (cnc self) "]"
														  "Ignoring " field " because I don't have it")
												'ignore-unhandled-format)))
										 (begin
											(kdebug '(log-* log-ecoservice)
													  "[" (my 'name) ":" (cnc self) "]"
													  "no service" field)
											#f)))
								 (uniq (if #t
											  targets
											  (filter (not-member (slot-ref logger 'dont-log))
														 targets)))
								 )
								(file 'newline)
								)
							 )
						  )
					 )
				  )


							  

;--- <circle>
;;(default-object-initialisation <circle>)

(define (Circle c r n) ;centre radius number of facets
  (let ((pi (acos -1)))
	 (translate-trace c (map (lambda (x)
										(list (cos (* (/ x n) 2.0 pi)) (sin (* (/ x n) 2.0 pi)))
										) (append (seq n) '(0)))
							)))

(define (make-crop-circle centre radius . n) ;; (-: make-patch-circle might be more appropriate, but not as much fun :-)
  (make-object <circle> 'location centre 'radius radius 'perimeter (Circle centre radius (if (or (null? n) (not (and (integer? (car n)) (> (car n) 2)))) 12  (car n))))
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

(model-method (<circle> <log-data>) (plot* self logger #!optional color fill scale loc)
				  (let* ((file (slot-ref logger 'file))
							(color (if color color (my 'map-color)))
							(rad (my 'radius))
							(scale (if (not scale) 1 scale))
							(loc (if (not loc) (my 'location)))
							(prj (composite-prj_src->dst self logger))
							(circ (make-circle-perimeter loc rad))
							)
					 (if (postscript? file)
						  (if fill
								(adjusted-plot-filled-polygon
								 file 0.1 (my 'default-color) color prj circ)
								(adjusted-plot-polygon
								 file 0.1 color #f prj circ))
						  (begin
							 (file 'display (string-append (object->string (prj loc) " " rad)))
							 (newline))
						  )))
				  


(model-method (<circle>) (minima self)
				  (map - (my 'location) (make-list (length (my 'location)) (my 'radius))))

(model-method (<circle>) (maxima self)
				  (map + (my 'location) (make-list (length (my 'location)) (my 'radius))))

(model-method (<circle> <point>) (contains? self loc)
				  (<= (distance (my 'location) loc) (my 'radius)))

(model-method (<circle>) (centre self)
				  (my 'location))

(model-method (<circle>) (radius self)
				  (my 'radius))

(model-method (<circle>) (Radius self)
				  (my 'radius))

(model-method (<circle>) (area self #!optional passing)
				  (* pi (sqr (radius self passing))))

(model-method (<circle>) (min-bound self)
				  (my 'radius))

(model-method (<circle>) (max-bound self)
				  (my 'radius))

(model-method (<circle> <point>) (distance-to-boundary self loc)
				  (if (contains? self loc)
						0
						(- (distance loc (my 'location)) (my 'radius))))

(model-method (<circle>) (random-point self)
					(let ((c (my 'location))
							(r (my 'radius)))
					  (map (lambda (x)
								(+ x (* r
										(-
										 (* 2.0 (random-real))
										 1.0)))) c)))

(model-method (<circle>) (perimeter self #!optional n)
				  (if (not (integer? n)) (set! n 120))
				  (make-circle-perimeter (my 'location) (my 'radius) n))

				  
;--- <polygon>

;;(default-object-initialisation <polygon>)

(define (make-polygon centre polygon #!optional is-relative)
  (if (not (eqv? (car polygon) (car (reverse polygon))))
		(set! polygon (append polygon (list (car polygon)))))
  (make-object <polygon> 'location centre 'perimeter (list-copy polygon) 'is-relative is-relative 'location #f 'area #f))

;(model-method (<polygon>) (dump self)
;					(dump% self 0))

(model-method (<polygon>) (location@ self)
				 (let ((here (my 'location)))
				 (if (my 'is-relative)
					  here
					  (if (not here)
							(let ((c (polygon-centroid (my 'perimeter))))
							  (set-my! 'location c)
							  c)
							here))))

(model-method (<polygon> <log-data>)(plot* self logger #!optional color fill scale loc)
				  (let* ((file (slot-ref logger 'file))
							(color (if color color (my 'map-color)))
							(scale (if (not scale) 1 scale))
							(loc (if (not loc) (my 'location)))
							(prj (composite-prj_src->dst self logger))
							(poly (perimeter self))
							)
					 (if (postscript? file)
						  (if fill
								(adjusted-plot-filled-polygon
								 file 0.1 (my 'default-color) color prj poly)
								(adjusted-plot-polygon
								 file 0.1 color #f prj poly))
						  (begin
							 (file 'display (centroid poly))
							 (file 'newline)))))


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

;; uses the point-in-polygon routine found in maths.scm
(model-method (<polygon> <point>) (contains? self loc)
				  (point-in-polygon loc (perimeter self)))

;; NOTE that this returns a negative number if the point is in the interior of the polygon.
(model-method (<polygon> <point>) (distance-to-boundary self point) ;; This is 2d only...
				  (distance-to-polygon point (perimeter self)))

(model-method (<polygon>) (centre self)
				  ;; This is a *very* simplistic "centre" which may lie
				  ;; outside the perimeter in convex polygons.
				  (let* ((p (perimeter self))
							(P (if (equal? (car p) (car (reverse p))) (cdr p) p))) ;; handle closed polygons.
					 (map (lambda (x) (/ x (length P))) (apply map + P))))

(model-method (<polygon>) (perimeter self #!optional drop-closing-point)
				  (let* ((c (my 'location))
							(p (if (my 'is-relative)
									 (map (lambda (ord) (map + ord c)) (my 'perimeter))
									 (my 'perimeter)))
							)
					 (if (and drop-closing-point (equal? (car p) (car (reverse p)))) (cdr p) p))) ;; handle closed polygons.

(model-method (<polygon>) (area self #!optional passing)
				  (let ((a (my 'area)))
					 (if (number? a)
						  a
						  (let ((c (polygon-area (perimeter self))))
							 (set-my! 'area c)
							 c))))

(model-method (<polygon>) (minima self)
				  (extremum min (perimeter self)))

(model-method (<polygon>) (maxima self)
				  (extremum max (perimeter self)))


(model-method (<polygon>) (min-bound self)
				  (distance-to-polygon (my 'location)  (perimeter self)))


(model-method (<polygon>) (max-bound self)
					 (apply max (map (lambda (p)
											 (distance (centre self) p))
										  (perimeter self))))


(model-method (<polygon>) (Radius self)
				  (max-bound self))

(model-method (<polygon>) (radius self)
				  (min-bound self)
				  ;;; (let* ((p (perimeter self))
				  ;;; 		  (p- (cdr p)) ;; don't want a point counted twice
				  ;;; 		  (c (centre self))
				  ;;; 		  (m (map- p- c))
				  ;;; 		  (S (apply min (map v-length m))))
				  ;;; 	 S)
				  )
(model-method (<polygon>) (random-point self)
					(let* ((peri (perimeter self))
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

(model-method <patch> (location-needs-location@ self) #t)

(model-method <patch> (location@ self #!rest args)
					  (cond ;; many loci possible
						((null? args) (perimeter self)) ;; return a list of all loci
						((number? (car args)) (perimeter self))
						((and (point? (car args)) (pair? (cdr args)) (number? (cadr args)))
						 (let ((l (car args)) (r (cadr args)))
							(map  (lambda (p) (<= (distance p l) r)) (perimeter self)))
						 ) ;; return all the loci that are within (cadr args) of the point
						((and (point? (car args)) (null? (cdr args)))
						 (nearest-point-in-list (car args) (perimeter self))
						 ) ;; return the point closest to the nominated location
						((and (list? (car args)) (apply andf (map number? (car args))))
						 (map (lambda (x) (location self x)) (car args)))
						(else (error 'bad-location-call:multiple-loci args))
				  ))

(model-method (<patch>) (perimeter self #!optional passing-argument)
				  (perimeter (my 'rep) passing-argument))

(model-method (<patch>) (area self #!optional passing-argument)
				  (area (my 'rep) passing-argument))

(model-method (<patch>) (number-represented self)
				  (area self))

(model-method (<patch>) (centre self)
				  (centre (my 'rep)))

(model-method (<patch> <point>) (distance-to-boundary self loc)
				  (distance-to-boundary (my 'rep) loc))

(model-method (<patch>) (max-bound self)
				  (max-bound (my 'rep)))


(define (make-boundary rep centre arg)
  (let ((rep-class (case rep
							((circle) <circle>)
							((polygon relative-polygon) <polygon>)
							(else 'bad))))

	 (let ((M (make-object rep-class )))
		(case rep
		  ((circle)
			(slot-set! M 'rep (make-object <circle> 'location centre 'radius arg))
			(slot-set! M 'rep (make-object <polygon> 'location centre
											'perimeter (list-copy arg)))
			)

		  ((polygon absolute-polygon)
			(slot-set! M 'rep (make-object <polygon> 'location centre
													 'perimeter (list-copy arg)))
			(slot-set! (slot-ref M 'rep) 'radius (max-bound (slot-ref M 'rep)))
			)

		  ((relative-polygon)
			(slot-set! M 'rep (make-object <polygon> 'location centre
											'perimeter (list-copy arg) 'is-relative #t))
			(slot-set! (slot-ref M 'rep) 'radius (max-bound (slot-ref M 'rep)))
			)

		  (else (error "Bad representation specified for a boundary" rep)))))
  )

;; min-bound, max-bound contains? services 

;--- (initialise...) 


;;(default-agent-initialisation <patch>
;;  'service-list '()
;;  'default-value +nan.0
;;  'minv '(-inf.0 -inf.0)
;;  'maxv '(+inf.0 +inf.0)

;;; (model-method <patch>  (initialise self args)
;;; 				  (if (and (pair? args) (pair? (car args)) (= (length args) 1))
;;; 						(set! args (car args)))

;;; 				  (dnl* "PATCH: " initialise self args)
;;; 				  ;; call "parents" last
;;; 				  ;; to make the
;;; 				  ;; initialisation list
;;; 				  ;; work
;;; 				  (parent-initialise)

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

(model-method (<patch> <polygon> <point>) (install-boundary self bdry centre)
				  (set-my! 'rep bdry)
				  (slot-set! bdry 'location centre)
				  )

(model-method (<patch> <circle> <point>) (install-boundary self bdry centre)
				  (set-my! 'rep bdry)
				  (slot-set! bdry 'location centre)
				  )

(model-method <patch> (location@ self)
				  (slot-ref (my 'rep) 'location))


(model-method (<patch> <symbol>) (provides self sym)
				  (member sym (provides self)))

(model-method (<patch> <string>) (provides self str)
				  (member str (provides self)))
				  
;; Something provided which is not a service indicates a limitless presence: a permanent lake provides water in this sense.
(model-method (<patch> <list>) (provides self slist)
				  (let ((provisions (provides self)))
					 (list-intersection slist provisions)))
						 


;; Something provided which is not a service indicates a limitless presence: a permanent lake provides water in this sense.
(model-method (<patch>) (provides self)
				  (sortless-unique
					(append
					 (list (cncs self) (my 'taxon) (my 'provides))
					 (apply append (map (lambda (s) (slot-ref s 'provides)) (my 'service-list))))))
						 

(model-method (<patch> <symbol>) (provides? self sym)
				  (member sym (provides self)))

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
(model-method (<patch> <point>) (distance-to-centre self loc)
				  (sqrt (apply + (map sqr (map - (list-head
															 (slot-ref (my 'rep) 'location) 2)
															(list-head loc 2))))))

;--- (distance-to-interior...) returns the distance to the boundary of
;the patch (more expensive than the dist to centre)
(model-method (<patch> <point>) (distance-to-interior self loc)
				  (let* ((R (- (sqrt (apply
											 + (map sqr
													  (map -
															 (list-head
															  (slot-ref (my 'rep) 'location) 2)
															 (list-head loc 2)))))
									(my 'radius)))
							)
					 (if (< R 0) 0 R)))

;--- (contains?...) predicate to indicate if something is in the patch

(model-method <patch> (contains? self bit)
;;				  (dnl* 'contains? (name self) bit)
				  (let ((rep (slot-ref self 'rep)))
					 (contains? rep bit)))

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
							(lambda (x) (or (member (symbol x) ss) (member (name x) ss)))
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


(model-method (<patch>) (Radius self)
				  (Radius (my 'rep)))


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
;; 								 (lambda (parent-representation self)
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

;--- model-method (<patch> <pair>) (total-capacity self symlist)
(model-method (<patch> <pair>) (total-capacity self symlist)
				  (let ((ss (service-list self (if (symbol? symlist)
															  (list symlist)
															  symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (capacity y)) ss)))))

;--- model-method (<patch> <agent> <symbol> <agent>) (log-data self logger format  targets)

(model-method (<patch> <log> <symbol> <list>) (log-data self logger format targets)
				  (dnl* "(model-method (<patch> <log> <symbol> <list>) (log-data self logger format targets)")
				  (if (or (my 'always-log) (and (member format '(ps)) (my 'always-plot)) (emit-and-record-if-absent logger self (my 'subjective-time)))
						(let* ((file (slot-ref logger 'file))
								 (p (composite-prj_src->dst self logger))
								 )
						  (if (not (procedure? p)) (set! p (lambda (x) x)))
						  (kdebug '(log-* log-patch) "[" (my 'name) ":"
									 (cnc self) "]" "in log-data")
						  
						  (case format
							 ((ps)
							  (file 'comment (name self) " " (taxon self) " " (subjective-time self) " " (agent-state self) )
							  (file 'push-font (my 'default-font) (my 'default-size))
							  (let* ((symlist (services self))
										(name (slot-ref self 'name))
										(R (car (p (make-list 2 (radius self)))))
										;; this will fail if the projection is not a linear-map
										(L (p (list-head (location self) 2)))
										(slist (slot-ref self 'service-list))
										(n (+ 1 (length slist))) ;; slist is
										(perim (perimeter self))
										;; becoming
										;; circular??
										;; ....******
										(ns (length slist))
										(mm-xoffset 2)
										(mm-yoffset 2)
										(p^t (list-transpose perim))
										(Left (apply min (car p^t)))
										(Right (apply max (car p^t)))
										(Up (apply max (cadr p^t)))
										(Down (apply min (cadr p^t)))
										(ploc (location self))
										(psprj (composite-prj_src->dst self logger))
										(psloc (psprj ploc))
										)
								 (file 'Comment (string-append "in log-data for <patch> " (object->string (my 'name)) (object->string (list Left Down Right Up))))
								 
								 (if adjust-grey (file 'setgray PATCHGREY))
								 (cond
								  ((or (member (class-of (my 'rep)) (list <polygon> <circle>))
										 (eq? (class-of (my 'rep)) <polygon>))
									(file 'comment (string-append "<patch> footprint for " (cnc (my 'rep)) ": " (object->string (my 'name))))
									(file 'comment (string-append "Native ordinates: " (object->string (perimeter self))))
									(file 'comment (string-append "Projected ordinates: " (object->string (map psprj (perimeter self)))))
									(file 'comment (string-append "service list: " (object->string slist)))
;									(adjusted-plot-polygon file 0.7 0.0 #f psprj (perimeter (my 'rep))))
									(map-polygon logger (perimeter self) format 'grey)
									)
								  
								  (#t (error "Bad representation for output" (cnc (my 'rep))))
								  )
								 
								 ;;; (file 'moveto (p (map + L
								 ;;; 							  (list (+ mm-xoffset
								 ;;; 										  (* 0.2 R))
								 ;;; 									  (+ mm-yoffset
								 ;;; 										  (/ ns 2.0)))
								 ;;; 							  )))

								 (file 'comment (string-append "Left Up = " (object->string (psprj (list Left Up)))))
								 (file 'moveto (psprj (list Left Up)))
								 (file 'linefeed 1)
								 (if (null? slist) (file 'linefeed 1))
								 (file 'push-color '(0.5 1.0 0.5))
								 (file 'show-table (map
														  (lambda (x) (string-append " "
																							  (slot-ref x 'name)
																							  " = " (pno (value x))))
														  slist))
								 (file 'pop-color)
								 (caption file self #f #f) ;; the last two args are prj and loc respectively
								 (file 'pop-font)
								 ))


							 ;;((text table dump)
							 ;; (parent-log-data)
							 ;; )
							 (else
							  (file 'show (my 'name))
							  (for-each 
								(lambda (x)
								  (file 'show " ")
								  (if (agent? x)
										(file 'show (value x))
										(file 'show x)))
								(map (lambda (x) (value x)) (my 'service-list)))
							  (file 'linefeed)
							  ;;(parent-log-data)	
							  )
							 )
						  )
						)
				  )


;-- patch note methods and model body

(model-method <patch> (initialise-instance self)
				  (if (uninitialised? (my 'notepad))
						(set-my! 'notepad (make-agent <blackboard> (string-append (name self) "-notepad") 'label 'not-runninge 'message-list '()))))

(model-method <patch> (query self cmd #!rest args)
				  (apply query (cons (my 'notepad) (cons cmd args))))

;--- model-body <patch>
(model-body% <patch>
				(call-all-parents)
				(kdebug '(model-bodies patch-running)"In " (cnc self) (name self) "@" t)
				(if (procedure? (my 'caretaker))
					 (let* ((caretaker (slot-ref self 'caretaker))
							  (result (if (procedure? caretaker)
											  (caretaker self t dt)
											  dt))
							  )
						(if (or (null? result) (number? result))
							 dt
							 result)
						)
					 dt)
					)


;-- dynamic-patch methods and body

(definition-comment 'dynamic-patch
  "A dynamic patch expects definitions similar to those in diffeq-systems")

;--- <dynamic-patch> (initialise-instance  (self args)
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
;;; 				  (parent-initialise-instance) ;; call "parents" last
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
					 (list 'rep (make-object <circle> 'name name 
											'location centre 'radius radius)
							 ))
					((eq? bdry <polygon>)
					 (let ((p (list 'rep (make-object <polygon> 'name name 
											'location centre 'perimeter (list-copy box))
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
;; or a <landscape> like class.


;; bounding rectangular volume for the pointset


(define (extremum op pointset)
  (map (lambda (i)
			(apply op (map (lambda (x) (list-ref x i)) pointset )))
		 (seq (length (car pointset)))))


;; Returns a list of lists, where the inner lists correspond to the rows in the grid.
;; the grids are organised like so:

;; __ __ __ __ __
;;|00|01|02|03|04|
;;|__|__|__|__|__|
;;|10|11|12|13|14|
;;|__|__|__|__|__|
;;|20|21|22|23|24|
;;|__|__|__|__|__|
;; __    __    __    __
;;/00\__/01\__/02\__/03\__
;;\__/10\__/11\__/12\__/13\
;;/20\__/21\__/22\__/23\__/
;;\__/30\__/31\__/32\__/33\
;;/40\__/41\__/42\__/43\__/
;;\__/50\__/51\__/52\__/53\
;;/60\__/61\__/62\__/63\__/
;;\__/  \__/  \__/  \__/
;; 
;; The list representations for 'grid and 'hex are consistent with
;; normal list indexing and the way we typically number things from
;; top to bottom; reversing the list gives us a more tradition "map"
;; ordering. 



;; Returns a list of lists, where the inner lists correspond to the rows in the grid.
;; (make-grid <patch> "swamp" "Sodden Lodden" <polygon> 12 36 '((0 0 0) (2400 7200))  HeronIs-DEM)
(define (make-grid cell-class taxon name cell-type n m domain #!optional (terrain #f) (statevars '()))
  (let* ((ll (car domain))
			(ur (cadr domain))
			(nscale (/ (apply - (map car domain)) (* 1.0 n)))
			(mscale (/ (apply - (map cadr domain)) (* 1.0 n)))
			(radius (min nscale mscale))
			(patch-list '())
			(M (make-list* n m))
			)
	 (if #f
		  (begin
			 (dnl* "Making a" n 'x m "grid with a bbox" domain)
			 (dnl* "stepsizes" nscale mscale)))

	 ;; The flag 'is-relative is set to false in the construction of the grid cells.
	 ;; We could specify a single box and make the polygon vertices relative to the centre,
	 ;; but this is not yet well exercised.  The distinction between the two is important when
	 ;; debugging.

	 (let ((M* (map-**-ix
						 (lambda (x i)
									(let ((box (bbox (list (+ (car ll) (* nscale (car i)))
																  (+ (cadr ll) (* mscale (cadr i))))
														  (list (+ (car ll) (* nscale (+ 1 (car i))))
																  (+ (cadr ll) (* mscale (+ 1 (cadr i)))))))
											(centre (list (+ (car ll) (* nscale (+ 0.5 (car i))))
															  (+ (cadr ll) (* mscale (+ 0.5 (cadr i))))))
											(pname (string-append name "-" (number->string (car i)) ","
																		 (number->string (cadr i))))
											)
									  (dnl* "Box " pname x i)
									  (let* ((minx +nan.0)
												(miny +nan.0)
												(maxx +nan.0)
												(maxy +nan.0)
												(mB (extremum min box))
												(MB (extremum max box))
												(PP (create-	<polygon>
																	'location centre
																	'radius (/ (sqrt (apply + (map sqr (map - MB mB)))) 2.0)
																	'perimeter box
																	'is-relative #f ;;;; (-: THIS IS IMPORTANT HERE---awkward bugs arise if this is wrong ;-)
																	'minv mB
																	'maxv MB
																	'note "generated by make-grid"
																	'dont-log #t ;; let them be logged by default
																	))
												(cell
												 (make-agent 
												  cell-class
												  taxon
												  'name pname
												  'representation cell-type
												  'rep PP
												  'index i
												  )))
;							(pp (dumpslots PP))
							(dnl* "-->"(perimeter cell))
							(dnl* "   " box)
										 (if (pair? statevars) (set-state-variables cell statevars))
										 ;; so we can adjust things like dt.

										 (set! patch-list (cons cell patch-list))
							(dnl* 'ta-taah)
										 cell))
									)
								 M)
					)
			 )
		(dnl* "Oh. I see.")

		;; connect neighbours up .... this may take some time
		(if #t (dnl "Connections not implemented yet!")
			 )
				
		;; (map (lambda (i) (slot-ref (slot-ref (list-ref patchlist i) 'rep) 'perimeter)) (seq (length patchlist)))
		M*)
	 )
  )

;; (make-grid <patch> "swamp" "Sodden Lodden" <polygon> 12 36 '((0 0 0) (2400 7200))  HeronIs-DEM)
(define (make-hex cell-class taxon name cell-type n m domain  #!optional (terrain #f) (statevars '()))
  (let* ((ll (car domain))
			(ur (cadr domain))
			(nscale (/ (- (car ur) (car ll)) (* 1.0 n)))
			(mscale (/ (- (cadr ur) (cadr ll)) (* 1.0 m)))
			(radius (min nscale mscale))
			(patch-list '())
			(M (make-list* n m))
			)
	 (if #f
		  (begin
			 (dnl* "Making a" n 'x m "hex grid with a bbox" ll ur)
			 (dnl* "stepsizes" nscale mscale)))

	 ;; The flag 'is-relative is set to false in the construction of the grid cells.
	 ;; We could specify a single box and make the polygon vertices relative to the centre,
	 ;; but this is not yet well exercised.  The distinction between the two is important when
	 ;; debugging.

	 (let ((M* ((map-**-ix
						 (lambda (x i)
									(let ((box (bbox (list (+ (car ll) (* nscale (car i)))
																  (+ (cadr ll) (* mscale (cadr i))))
														  (list (+ (car ll) (* nscale (+ 1 (car i))))
																  (+ (cadr ll) (* mscale (+ 1 (cadr i)))))))
											(centre (list (+ (car ll) (* nscale (+ 0.5 (car i))))
															  (+ (cadr ll) (* mscale (+ 0.5 (cadr i))))))
											(pname (string-append name "-" (number->string (car i)) ","
																		 (number->string (cadr i))))
											)
									  ;;(dnl* "Box " pname x i box)
									  (let* ((minx +nan.0)
												(miny +nan.0)
												(maxx +nan.0)
												(maxy +nan.0)
												(mB (extremum min box))
												(MB (extremum max box))
												(PP (create-	<polygon>
																	'location centre
																	'radius (/ (sqrt (apply + (map sqr (map - MB mB)))) 2.0)
																	'perimeter box
																	'is-relative #f ;;;; (-: THIS IS IMPORTANT HERE---awkward bugs arise if this is wrong ;-)
																	'minv mB
																	'maxv MB
																	'note "generated by make-grid"
																	'dont-log #t ;; let them be logged by default
																	))
												(cell
												 (make-agent 
												  cell-class
												  taxon
												  'name pname
												  'representation cell-type
												  'rep PP
												  )))
										 ;;							(pp (dumpslots PP))
;							(dnl* "-->"(perimeter cell))
;							(dnl* "   " box)
										 (if (pair? statevars) (set-state-variables cell statevars))
										 ;; so we can adjust things like dt.

										 (set! patch-list (cons cell patch-list))
										 cell))
									)
								 M)
					))
			 )
				  ;; (map (lambda (i) (slot-ref (slot-ref (list-ref patchlist i) 'rep) 'perimeter)) (seq (length patchlist)))
		M*)
	 )
  )


(define (patch-list-from-cover cover)
  (flatten cover))

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
															  (apply initialise-instance (cons <ecoservice> x therest))
															  instance
															  )))))
					(#t (error (string-append "populate-patch should be a list containing members which are"
													  "patches, arguments  or "
													  "(make-agent <ecoservice> taxon ...)"
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

(define (make-patch type loc radius services . args) ;; services is a list of lists, with each inner list consisting of a name (a string), a type (a symbol) and a value (typically a number, capacity)  and another number, typically the delta-T-max
  (let* ((loc loc) 
			(radius radius)
			(service-update-map (if (null? args) '() (car args)))
			(update-equations (if (null? args) '() (cadr args)))
			(P (make-agent (case type
						  ((dynamic dynamic-patch <dynamic-patch>)
							<dynamic-patch>)
						  ((simple patch <patch>)
							<patch>)
						  (else <patch>)) 'location loc 'radius radius 'type 'patch 'representation 'patch))
			)
	 (if (not (null? services))
	 	  (let ((sl (map 
	 					 (lambda (n t q) (make-agent <ecoservice> 'patch P 'name n 'type t 'capacity q )
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
				  (display ,q
							  (make-string count #\space))
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
(model-method (<dynamic-patch> <log> <symbol> <list>) (log-data self logger format  targets)
				  (dnl* "(model-method (<patch> <log> <symbol> <list>) (log-data self logger format targets)")
				  (let ((kdebug (if #t kdebug  dnl*))
						  )
					 (if (or (my 'always-log) (and (member format '(ps)) (my 'always-plot)) (emit-and-record-if-absent logger self (my 'subjective-time)))
						  (let ((file (slot-ref logger 'file))
								  (p (slot-ref self 'projection-assoc-list)))
							 (if (or (not p) (null? p))  (set! p (lambda (x) x)))
							 (kdebug '(log-* log-patch) "[" (my 'name) ":"
										(cnc self) "]" "in log-data")
							 
							 (case format
								((ps)
								 (let* ((symlist (services self))
										  (name (slot-ref self 'name))
										  )

									(if adjust-grey (file 'push-color PATCHGREY))
									(ps-circle file  (p (list (radius self)))
												  (p (list-head (location self) 2)) 0.7 0.0)


									(let* ((slist (slot-ref self 'service-list))
											 (n (+ 1 (length slist))) ;; slist is becoming circular?? ....******
											 (ns (length slist))
											 (loc (location self))
											 (rad (radius self))
											 (mm-xoffset 2)
											 (mm-yoffset 2)
											 (p^t (list-transpose perim))
											 (Left (apply min (car p^t)))
											 (Right (apply max (car p^t)))
											 (Up (apply max (cadr p^t)))
											 (Down (apply min (cadr p^t)))
											 (ploc (location self))
											 (psprj (composite-prj_src->dst self logger))
											 (psloc (psprj ploc))
											 )

									  (cond
										((or (member (class-of (my 'rep)) (list <polygon> <circle>))
											  (eq? (class-of (my 'rep)) <polygon>))
										 (file 'comment (string-append "<dynamic-patch> footprint for " (cnc (my 'rep)) ": " (object->string (my 'name))))
										 (file 'comment (string-append "Native ordinates:" (object->string (perimeter self))))
										 (file 'comment (string-append "Projected ordinates:" (object->string (map psprj (perimeter self)))))
;									(adjusted-plot-polygon file 0.7 0.0 #f psprj (perimeter (my 'rep))))
										 (map-polygon logger (perimeter self) format 'grey)
										 )
										
										(#t (error "Bad representation for output" (cnc (my 'rep))))
										)

									  (file 'moveto (psprj (list Left Up)))
									  (file 'linefeed 1)
									  (if (null? slist) (file 'linefeed 1))
									  (file 'push-color '(0.5 1.0 0.5))
									  (file 'show-table (map
																(lambda (x) (string-append " "
																									(slot-ref x 'name)
																									": "
																									(pno (value x))))
																slist))
									  )
									(caption file self #f #f) ;; the last two args are prj and loc respectively
									)
								 )

								;;((text table dump)
								;; (parent-log-data)
								;; )
								(else
								 (parent-log-data))
								)
							 )
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

;; This is a wrapper for (make-agent <landscape>, ymmv.
(define (make-landscape name default-ht domain terrain-function patch-type patch-data ;; patch-type would usually be a patch class, such as <patch> or <dynamic-patch>
							 )
  (let* ((PL (map (lambda (x) (apply make-patch (list patch-type (apply random-location domain) (map (lambda (y) (* 0.25 x)) (apply min (map - maxv minv))) x))) patch-data))
			(H (make-agent <landscape> 'name name 'default-value default-ht 'minv (car domain) 'maxv (cadr domain) 'terrain-function terrain-function 
						'patch-list PL))
			)
	 H)
  )


;; Default landscape only has the default value, oddly enough
(model-method (<landscape> <point>) (value self loc)
				  (if (contains? self loc)
						((my 'terrain-function) loc)
						(my 'default-value)))

(model-method (<landscape> <point>) (capacity self loc)
				  (if (contains? self loc)
						((my 'terrain-function) loc)
						(my 'default-value)))

;; This is to keep the "run" chain consistent
(model-body <landscape>
						(kdebug '(model-bodies landscape-running nested-domain)  (cnc self) (name self) "@" t "/" dt "(dt)" (my 'subjective-time) "(subj time)" )
						(call-all-parents) ;; chain to <environment>
						(kdebug '(nested-domain)  "after parent body ->" (cnc self) (name self) "@" t "/" dt "(dt)" (my 'subjective-time self) "(subj time)" )
						dt
						)


;--- model-method <landscape> (agent-prep self start end) Set preconditions
;                                                    for running
(model-method (<landscape> <number> <number>) (agent-prep self start end)
				  (parent-agent-prep start end)
				  #t
				  )



;(model-method <landscape> (dump self)
;				  (dump% self 0))

;--- model-method <landscape> (dump% self count) dumps the state of the landscape
;                                               agent in a readable way
(model-method <landscape> (dump% self count)
				  (display (make-string count #\space))
				  (display "<landscape>\n")

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

(model-method <landscape> (area self)
				  (apply + (map area (my 'patchlist))))

(model-method <landscape> (number-represented self)
				  (area self))
				  

;--- model-method (<landscape> <patch>) (add-patch! self patch) add a
;                                                            patch to the habitat
;; Note that there is an implicit ordering to adding patches and patchlists
(model-method (<landscape> <patch>) (add-patch! self patch)
				  (set-my! 'patch-list (uniq (cons patch (my 'patch-list))))
				  (let ((minv (apply minima (my 'patchlist)))
						  (maxv (apply maxima (my 'patchlist)))
						  )
					 (set-my! 'minv minv)
					 (set-my! 'maxv maxv)
				  ))

;--- model-method (<landscape> <patch>) (add-patch! self patch) add a
;                                                            patch to the habitat
;; Note that there is an implicit ordering to adding patches and patchlists
(model-method (<landscape> <list>) (add-patches! self patchlist)
				  (set-my! 'patch-list (uniq (append  patchlist (my 'patch-list))))
				  (let ((minv (apply minima (my 'patchlist)))
						  (maxv (apply maxima (my 'patchlist)))
						  )
					 (set-my! 'minv minv)
					 (set-my! 'maxv maxv)
				  ))

;--- model-method (<landscape> <procedure>) (remove-patch! self pfilter)
;  keep only patches which match a filter
(model-method (<landscape> <procedure>) (remove-patch! self pfilter)
				  (set-my! 'patch-list (filter pfilter (my 'patch-list))))

;--- (services...) returns services matching the sym or in the symlist
(model-method (<landscape> <list>)
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



(model-method (<landscape>) (random-point self)
				  (let* ((plist (my 'patch-list))
							(llist (map random-point plist))
						   (i (random-integer (length plist)))
							)
									 
				  (list-ref llist i)))




;--- model-method (<landscape>) (service-list% self . ss)
;; returns the slist of provided services: ss is an optional list of
;; patch names/symbols
;;; (model-method (<landscape> <list>)
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



(model-method (<landscape>) (provides self)
				  (sortless-unique
					(append
					 (cons (cnc <landscape>)
							 (cons (my taxon)
									 (apply append (map provides (my 'patch-list))))))))


;--- model-method (<landscape>) (service-list% self) ret a list of all ecoservices
;; May be wrong....***
(model-method (<landscape> <list>) (service-list% self ss)
				  (let* ((S (map (lambda (s) (service-list s)) (my 'patch-list))))
								(sortless-unique (apply append S))))
					 


;--- (service?...) queries if a service is present
(model-method (<landscape> <symbol>) (service? self sym)
				  (not (null? (services self sym))))


(model-method (<landscape> <pair>) (service? self symlist)
				  (not (null? (services self symlist))))


;--- model-method (<landscape> <symbol>) (service-sites self sym)
; returns a list of patches with services
(model-method (<landscape> <symbol>) (service-sites self sym)
				  (let loop ((rslt '())
								 (pl (my 'patch-list)))
					 (cond
					  ((null? pl) rslt)
					  ((service? (car pl) sym)
						(loop (cons (car pl) rslt) (cdr pl)))
					  (else (loop rslt (cdr pl))))))


(model-method (<landscape> <pair>) (service-sites self symlist)
				  (let loop ((rslt '())
								 (pl (my 'patch-list)))
					 (cond
					  ((null? pl) rslt)
					  ((service? (car pl) symlist)
						(loop (cons (car pl) rslt) (cdr pl)))
					  (else (loop rslt (cdr pl))))))




;--- model-method (<landscape>) (patch-list self . arg) 
; returns the list of patches (possibly filtered by names, symbols,
; procedures....

(model-method (<landscape>) (patch-list% self arg)
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

(model-method (<landscape> <point> <number> <list>) (aggregate-value self location radius servicelist)
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


;--- model-method <landscape> (min-bound self)
(model-method <landscape> (min-bound self)
				  (let ((pl (slot-ref self 'patch-list)))
					 (if (pair? pl)
						  (let* ((v (map min-bound pl))
									(vx (apply min (map car v)))
									(vy (apply min (map cadr v)))
									)
							 (list vx vy))
						  (my 'minv))))

;--- model-method <landscape> (max-bound self)
(model-method <landscape> (max-bound self)
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

;--- (<landscape> <procedure>...) (log-data self logger format  targets)
(model-method (<landscape> <log> <symbol> <list>) (log-data self logger format  targets)
				  (dnl* "(model-method (<landscape> <log> <symbol> <list>) (log-data self logger format  targets)")
				  (let ((kdebug (if #t kdebug dnl*))
						  )
					 
					 (if (or (my 'always-log) (and (member format '(ps)) (my 'always-plot)) (emit-and-record-if-absent logger self (my 'subjective-time)))
						  (let ((ps (slot-ref logger 'file))
								  (p (slot-ref self 'projection-assoc-list)))
							 (if (or (not p) (null? p))  (set! p (lambda (x) x)))
							 (case format
								((ps)

								 (let* ((symlist (services self))
										  (name (slot-ref self 'name))
										  (plist (slot-ref self 'patch-list))
										  (locs (centroid (map location plist)))
										  (ps (slot-ref logger 'file))
										  (p (slot-ref self 'projection-assoc-list)))
									(if (or (not p) (null? p))  (set! p (lambda (x) x)))
									
									(ps 'moveto (list (p (car locs)) (p (cadr locs))))
									(if adjust-grey (ps 'setgray HABITATGREY))
									(ps 'Helvetica 12)
									(ps 'show (string-append " " (slot-ref self 'name)))								  
									
									(let ((subs (slot-ref self 'active-subsidiary-agents)))
									  (if (pair? subs)
											(for-each (lambda (lpch)
															(ps 'Helvetica 7)
															(log-data lpch format targets p ps)
															)
														 subs))))
								 
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
								 (parent-log-data)

								 ))
							 )
						  )
					 )
				  )


;--- model-method (<landscape>) (spatial-scale self)
(model-method (<landscape>) (spatial-scale self)
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


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:

