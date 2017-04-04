
(load "basic-population.scm")


;(dnl "Making populations: args are name population-names val cap growth nmort")

;; Note: the indicated value (val) isn't used in rk4*
;;(make-population name population-names growth nmort [cap [val]])



(define Grass (make-population 'Grass '(Grass Rabbit Fox Bear) 0.8 0.0))
(define Rabbit (make-population 'Rabbit '(Grass Rabbit Fox Bear) 0.08 0.1))
(define Fox (make-population 'Fox '(Grass Rabbit Fox Bear) 0.0 0.1))
(define Bear (make-population 'Bear '(Grass Rabbit Fox Bear) 0.0 0.382))

(Grass 'register-populations (list Grass Rabbit Fox Bear)) 
(Rabbit 'register-populations (list Grass Rabbit Fox Bear))
(Fox 'register-populations (list Grass Rabbit Fox Bear))
(Bear 'register-populations (list Grass Rabbit Fox Bear))

;(Rabbit 'logistic)

(Grass 'register-prey)
(Rabbit 'register-prey (list Grass 0.0005 0.3))
(Fox 'register-prey (list Rabbit 0.00049 0.22))
;;(Bear 'register-prey (list Grass 0.0005 0.2) (list Rabbit 0.00006 0.21) (list Fox 1e-6 0.1))
(Bear 'register-prey (list Grass 0.0005 0.2) (list Rabbit 0.0001 0.2))

;;(dnl "Registering predators: no arguments required")
(Grass 'register-predators)
(Rabbit 'register-predators)
(Fox 'register-predators)
(Bear 'register-predators)

;(Fox 'register-helpers (list Bear 0.0001))
;(Bear 'register-competitors (list Fox 0.0001))


(dnl "grf")

(define grf (rk4* (list Grass Rabbit Fox Bear) 0 770 0.1 '(18000 700 80 6)))

;; If we want kerning around zero....
;;(define grf (rk4* (list Grass Rabbit Fox) 0 770 0.5 '(8000 700 80) (lambda (x) (if (> (abs x) 1e-8) x 0))))

(with-output-to-file "grf.dat" (lambda () (dump-data (grf 'dat))))

(dnl "grf done")




;;(define Grass (make-population 'Grass '(Grass Rabbit Fox Bear) 0.4 0.0 8000 80000))
;;(define Rabbit (make-population 'Rabbit '(Grass Rabbit Fox Bear) 0.04 0.4 700 2000))
;;(define Fox (make-population 'Fox '(Grass Rabbit Fox Bear) 0.0 0.1 80 2000))
;;(define Bear (make-population 'Bear '(Grass Rabbit Fox Bear) 0.0 0.1 70 1800))



;;(define Grass (make-population 'Grass '(Grass Rabbit Fox) 8000 20000 0.4 0.0))
;;(define Rabbit (make-population 'Rabbit '(Grass Rabbit Fox) 700 2000 0.04 0.4))
;;(define Fox (make-population 'Fox '(Grass Rabbit Fox) 80 2000 0.0 0.1))

;;(Grass 'logistic 1)
;;(Rabbit 'logistic #f)
;;(Fox 'logistic 1)


;;;;(grass 'logistic-growth)
;;;;(grass 'logistic-mort)
;;;;(rabbit 'logistic-mort)
;;;;(fox 'logistic-mort 2.0)

;;(dnl "Registering populations: Each needs the populations it interacts with")
;;(Grass 'register-populations (list Grass Rabbit Fox)) 
;;(Rabbit 'register-populations (list Grass Rabbit Fox))
;;(Fox 'register-populations (list Grass Rabbit Fox))

;;(dnl "Registering prey: each needs the entities it preys on with the attack rate and the efficiency")
;;(Grass 'register-prey)
;;(Rabbit 'register-prey (list Grass 0.0005 0.2))
;;(Fox 'register-prey (list Rabbit 0.0005 0.25))


;(Grass 'logistic 1)
;(Rabbit 'logistic #f)
;(Fox 'logistic 1)


;(Grass 'logistic-growth)
;(Grass 'logistic-mort)
;(Rabbit 'logistic-mort)
;(Fox 'logistic-mort 2.0)

;(dnl "Registering populations: Each needs the populations it interacts with")
;(dnl "Registering prey: each needs the entities it preys on with the attack rate and the efficiency")

;; We could also get these by
;(define dG/dt (Grass 'd/dt))
;(define dR/dt (Rabbit 'd/dt))
;(define dF/dt (Fox 'd/dt))







;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
