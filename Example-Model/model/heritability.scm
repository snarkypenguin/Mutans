; -*- mode: scheme; -*-
;; <top> <primitive-object> <object> <agent> ...

(load "preamble.scm")
(load "sort.scm")
(load "sclos.scm")
(load "kdnl.scm")
(load "utils.scm")
(include "framework") ;;Needs to come after sclos and utils, needs to be included rather than loaded.
(load "units.scm")
(load "maths.scm")
(load "integrate.scm")
(load "matrix.scm")
(load "postscript.scm")
(load "basic-population.scm")
(load "framework-classes.scm") ;; must include framework
(load "framework.scm") ;; must include framework
(load "framework-declarations.scm") ;;must include framework, loaded early so the generics are there
(load "declarations.scm") ;; must include framework
(load "log-classes.scm") ;; must include framework
(load "landscape-classes.scm") ;; must include framework
(load "plant-classes.scm") ;; must include framework
(load "animal-classes.scm") ;; must include framework
(load "kernel.scm") ;; must include framework
(load "framework-wrappers.scm") ;; must include framework, wrapper functions for dispatcing to methods
(load "framework-methods.scm") ;; must include framework
(load "log-methods.scm") ;; must include framework
(load "landscape-methods.scm") ;; must include framework
(load "plant-methods.scm") ;; must include framework
(load "animal-methods.scm") ;; must include framework


;- Inheritance and chaining test rig


(define-class <C0> (inherits-from <agent>) (state-variables testcase1  testcase2))
(define-class <D0> (inherits-from <agent>) (state-variables testcase3))

(define-class <E0> (inherits-from <C0>) (state-variables testcase4))
(define-class <F0> (inherits-from <C0> <D0>) (state-variables testcase5))


(model-method <C0> (test self #!rest args)
				  (dnl* 'CO-- self args)
				  (display "C0 ")(display args)(display ": ")
				  (display (slot-ref self 'testcase1))
				  (display " ")
				  (display (slot-ref self 'testcase2))
				  (newline)
				  'C0
				  )
(model-method <D0> (test self #!rest args)
				  (dnl* 'D0-- self args)
				  (display "D0 ")(display args)(display ": ")
				  (display (slot-ref self 'testcase3))
				  (newline)
				  (test-parent)
				  'D0
				  )

(model-method <E0> (test self #!rest args)
				  (dnl* 'E0-- self args)
				  (display "E0 ")
				  (display (slot-ref self 'testcase4))
				  (display " ")
				  (display (slot-ref self 'testcase2))
				  (display " ")
				  (display (slot-ref self 'testcase1))
				  (newline)
				  (test-parent)
				  'E0
				  )

(model-method <F0> (test self #!rest args)
				  (dnl* 'F0-- self args)
				  (dnl* "F0" (slot-ref self 'testcase5)
						  (slot-ref self 'testcase3) 
						  (slot-ref self 'testcase2) 
						  (slot-ref self 'testcase1))
				  (test-parent)
				  'F0
				  )

(model-method <C0> (initialise self #!rest args)
				  (initialise-parent)
				  (dnl* 'IC0-- self args)
				  (set-state-variables self args)
				  (slot-set! self 'testcase1 't1)
				  (slot-set! self 'testcase2 't2)
				  )

(model-method <D0> (initialise self #!rest args)
				  (initialise-parent)
				  (dnl* 'ID0-- self args)
				  (set-state-variables self args)
				  (slot-set! self 'testcase3 't3)
				  )

(model-method <E0> (initialise self #!rest args)
				  (initialise-parent)
				  (dnl* 'IE0-- self args)
				  (set-state-variables self args)
				  (slot-set! self 'testcase4 't4)
				  )

(model-method <F0> (initialise self #!rest args)
				  (initialise-parent)
				  (dnl* 'IF0-- self args)
				  (set-state-variables self args)
				  (slot-set! self 'testcase5 't5)
				  )

(define Rob (make-agent <F0> 'testcase5 'Crivens!))
(define Hamish (make-agent <E0> 'testcase3 "Nae King nor Quin!"))
(define Callan (make-agent <C0> 'testcase1 "Gie orf!"))
(define Dimo (make-agent <D0> 'testcase3 "Ho YEZ!"))

(display "Rob Hamish Callan Dimo\n")






;;; (define c ((compute-methods test) (list Rob))) ;;;
;;; ((cadr (map method-procedure c)) (lambda x x) Rob) ;;;
;;; ((method-procedure (cadr c)) (lambda x x) Rob) ;;;

;;; ((method-procedure (car ((compute-methods test) (list Rob)))) (lambda x x) Rob) ;;;
;;; ((method-procedure (cadr ((compute-methods test) (list Rob)))) (lambda x x) Rob) ;;;
;;; ((method-procedure (caddr ((compute-methods test) (list Rob)))) (lambda x x) Rob) ;;;

(define (call-all-parents methd self #!rest args)
  (let* ((ml (map method-procedure ((compute-methods methd) (cons self args)))))
	 (map (lambda (m)
			  (apply m (cons (lambda x x) (cons self args))))
			ml)))



;; (map (lambda (m) ((method-procedure m) Rob Rob)) ((compute-methods test) (list Rob)))
;; (map (lambda (m) ((method-procedure m) Rob Rob)) ((compute-methods test) (list Rob)))


;-  The End 


;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
