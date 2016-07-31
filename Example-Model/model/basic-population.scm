

"This serves as a template for other population agents, as well as the
population machinery for the dynamic patch class."




(load "utils.scm")
(load "units.scm")
(load "sort.scm")
(load "maths.scm")
(load "maths.scm")
(load "integrate.scm")
(load "units.scm")
(load "postscript.scm")

(load "integrate.scm")
(load "matrix.scm")

;;========================================================================;;
;;
;;                      Constants and such
;;
;;========================================================================;;


(define unbounded +inf.0)

;;========================================================================;;
;;
;;                      TERMS FOR THE dP/dt expression
;;
;;========================================================================;;


;; Multiplicative cap on the growth of the species. k = "unbounded" makes it go away 
;;(define (logistic-- a k) (if (eq? k unbounded) 1.0 (- 1 (pow (/ a k) 2.4))))

(define (logistic-- a k) (if (eq? k unbounded) 1.0 (- 1 (/ a k))))

(define (logistic-growth-- a k) (if (> a k) 0.0 (- 1.0 (/ a k))))
(define (logistic-mort-- a k) (if (< a k) 0.0 (- (/ a k) 1.0)))

;; sums the contributors to population growth
(define growth-terms-- +)

;; mortality-terms should be included as a modifier within a growth-terms s-exp
(define (mortality-terms-- . args) (- 0 (apply + args)))


;;========================================================================;;


"
The usual pattern for a std-d/dt would be 

(make-basic-population 'grass '(grass rabbit fox) ...)

"


(define (make-basic-population name population-names . rest)
  (let* ((name name)
			(self #f)
			(nrest (length rest))
			(popnames population-names)
			(populations #f)
			(growth (if (>= nrest 1) 0 (list-ref rest 0)))
			(nmort (if (>= nrest 2) 0 (list-ref rest 1)))
			(cap (if (>= nrest 3) unbounded (list-ref rest 2)))
			(val (if (>= nrest 4) 0 (list-ref rest 3)))
			(logistic #f)
			(logistic-growth #f)
			(logistic-mort #f)
			(prey-rates #f) ;; attack-rate and efficiency (in that order)
			(pred-att-rate #f) ;; predator's attack-rate is obtained from the predator
			(helper-rate #f)
			(competitor-rate #f)

			(fascist-init #f)
			)
	 (let* ((preys-on (lambda (vals att-rate eff)
							  (apply + (map * vals att-rate eff))))

			  (preyed-on-by (lambda (vals att-rate)
									(apply + (map * vals att-rate))))

			  ;; These are like the predation terms, but without any symmetry
			  (helped-by (lambda (vals helper-rate)
								(apply + (map * vals helper-rate))))
			  
			  (competes-with (lambda (vals competitor-rate)
									 (apply + (map * vals competitor-rate))))


			  (d/dt 
				(if (zero? nrest)

					 (lambda args (abort
										(string-append "d/dt is uninitialised:"
															" use the 'set-d/dt! call to set it")))
					 (lambda (t . pvals)
						;; t is the first element in the list, other
						;; elements must correspond to the populations
						;; indicated in the "populations" list when the
						;; population is created
						;;
						;; vals ought to be in order
						(let ((val (list-ref pvals (- (length popnames)
																(length (member name popnames))))))
						  (if #f
								(begin
								  (dnl name "-----------------------")
								  (dnl "value ====> " val)
								  (dnl "growth = " growth)
								  (dnl "preys-on = " (preys-on pvals (map car prey-rates)
																		 (map cadr prey-rates)))
								  (dnl "total mortality =  "
										 (+ nmort (if pred-att-rate
														  (preyed-on-by pvals
																			 (map car pred-att-rate))
														  0)))
								  (dnl "natural mortality = " nmort)
								  (dnl "preyed-on-by = "
										 (preyed-on-by pvals
															(if pred-att-rate
																 (map car pred-att-rate)
																 0)))

								  (if logistic
										(dnl "logistic " logistic "*" (logistic-- val cap)))
								  (if logistic-growth
										(dnl "logistic-growth "
											  logistic-growth "*"
											  (logistic-growth-- val cap)))
								  (if logistic-mort
										(dnl "logistic-mort " logistic-mort "*"
											  (logistic-mort-- val cap)))
								  
								  (dnl)))


						  (* val 
							  (if logistic (* logistic (logistic-- val cap)) 1.0)
							  (growth-terms-- growth 
													(if prey-rates
														 (preys-on pvals (map car prey-rates)
																	  (map cadr prey-rates))
														 0)

													(if helper-rate
														 (helped-by pvals helper-rate)
														 0)

													(mortality-terms--
													 nmort 
													 (if pred-att-rate
														  (preyed-on-by pvals
																			 (map car pred-att-rate))
														  0)
													 (if competitor-rate
														  (competes-with pvals competitor-rate)
														  0)
													 )))
						  ))
					 )
				)
			  (accessor
				(letrec ((population
							 (lambda args
								(cond
								 ((null? args)
								  val)
								 
								 ((eq? (car args) 'dump)
								  (pp (list
										 'name name 
										 'self self 
										 'pops popnames populations 
										 'params val cap growth nmort
										 'prey-rates prey-rates
										 'pred-att-rate pred-att-rate 
										 'logistic/-growth/-mort
										   logistic logistic-growth logistic-mort
										 'd/dt d/dt 
										 'func population)))

								 ((eq? (car args) 'set!)
								  (set! val  (cadr args)))
												
								 ((eq? (car args) 'register-populations)
								  ;; expects (animal 'register-populations
								  ;;    (list plant animal toothy-animal....))
								  (if (not (apply andf (map procedure? (cadr args))))
										(Abort))
								  (if (null? (cdr args))
										(Abort "No populations?  What's the point?")
										(set! populations (copy-list (cadr args)))
										))
								 
								 ((eq? (car args) 'register-prey)
								  (if (not populations)
										(Abort "You must register the populations before registering the prey")
										(begin
										  (set! prey-rates (make-list (length populations) '(0 0)))
										  (if (not (null? (cdr args)))
												(begin
												  (for-each
													(lambda (x y)
													  (list2-assoc-set! populations prey-rates x y))
													(map car (cdr args)) (map cdr (cdr args)))
												  )
												)
										  )
										))
								 
								 ((eq? (car args) 'register-predators)
								  (if (not prey-rates)
										(abort (string-append
												  (symbol->string name)
												  " has been called before both the prey list has been registered")))
								  (set! pred-att-rate (map (lambda (y) (y 'attack-rate self)) populations))
								  (if (zero? (apply + (map abs (map car pred-att-rate))))
										(set! pred-att-rate #f))
								  ;;(dnl name pred-att-rate)
								  )	
								 
								 ((eq? (car args) 'logistic)
								  (if (not (or (null? (cdr args)) (boolean? (cadr args)) (number? (cadr args))))
										(abort "argument to (entity 'logistic) must be null, #f #t or a number"))
								  (if (null? (cdr args))
										(set! logistic 1.0)
										(set! logistic (cadr args)))

								  (if logistic
										(begin
										  (set! logistic-growth #f)
										  (set! logistic-mort #f)))
								  
								  )
								 
								 ((eq? (car args) 'register-helpers)
								  (if (not populations)
										(Abort "You must register the populations before registering interactions")
										(begin
										  (set! helper-rate (make-list (length populations) 0))
										  (if (not (null? (cdr args)))
												(begin
												  (for-each
													(lambda (x y)
													  (list2-assoc-set! populations helper-rate x y))
													(map car (cdr args)) (map cadr (cdr args)))
												  )
												)
										  )
										))

								 ((eq? (car args) 'register-competitors)
								  (if (not populations)
										(Abort "You must register the populations before registering interactions")
										(begin
										  (set! competitor-rate
												  (make-list (length populations) 0))
										  (if (not (null? (cdr args)))
												(begin
												  (for-each
													(lambda (x y)
													  (list2-assoc-set! populations competitor-rate x y))
													(map car (cdr args)) (map cadr (cdr args)))
												  )
												)
										  )
										))

								 ((eq? (car args) 'logistic-growth)
								  (if (not (or (null? (cdr args))
													(boolean? (cadr args))
													(number? (cadr args))))
										(abort "argument to (entity 'logistic-growth) must be null, #f #t or a number"))
								  (if (null? (cdr args))
										(set! logistic-growth 1.0)
										(set! logistic-growth (cadr args)))
								  (if logistic-growth (set! logistic #f))

								  )

								 ((eq? (car args) 'logistic-mort)
								  (if (not (or (null? (cdr args))
													(boolean? (cadr args))
													(number? (cadr args))))
										(abort
										 "argument to (entity 'logistic-mort) must be null, #f #t or a number"))
								  (if (null? (cdr args))
										(set! logistic-mort 1.0)
										(set! logistic-mort (cadr args)))
								  (if logistic-mort (set! logistic #f))
								  )

								 ((and (eq? (car args) 'attack-rate) (procedure? (cadr args)))
								  (if (not prey-rates) 	
										(if fascist-init
											 (abort
											  (string-append
												(symbol->string name)
												" has been called before both the prey list has been registered"))
											 0)
										(let ((p (list2-assoc populations prey-rates (cadr args))))
										  (if p (car p) 0))))

								 ((eq? (car args) 'update)
								  (let ((dP (apply self (cdr args))))
									 (set! value (+ value (* dP (cadr args))))
									 ))

								 ((eq? (car args) 'set-d/dt!)
								  (set! d/dt (cadr args)))

								 ((eq? (car args) 'd/dt)
								  d/dt)
								 
								 ((number? (car args))
								  (apply d/dt args))
								 
								 (else (abort
										  (string-append "The argument to "
															  (symbol->string name) ", "
															  (object->string (car args))
															  ", is not recognised")))
								 ) ;; cond
								) ;; lambda
							 ) ;; population defn
							) ;; letrec closure
				  population) ;; letrec 
				) ;; accessor definition
			  ) ;; let* closure
		(set! self accessor) ;; so we can identify ourselves to others
		accessor) ;; return accessor function for the population
	 ) ;; outer let*
  )


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
