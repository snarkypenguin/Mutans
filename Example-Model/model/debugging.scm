;-  Code 

(require 'charplot)
(load "integrate.scm")
(load "matrix.scm")
(load "savannah-params.scm")
(load "population-support")

(define cap 200)
(define growth 1.01)
(define divisions 12)


;(plot (lambda (x) (explicit-dLP  14 x cap growth mortality)) 0 200 200)

;;(define L/dm (dLP/dm #void #void cap growth mortality))
;;(define L/dt (dLP/dt #void #void cap growth mortality))

;;(define S/dm (dLS/dm #void #void cap growth mortality))
;;(define S/dt (dLS/dt #void #void cap growth mortality))

;;(define h (differential-model dPrkm-logistic divisions cap growth mortality))
;;(define f (differential-model dPrkm-logistic #f cap growth mortality))
;;(define g (differential-model dPrkm-logistic #f cap growth mortality))
;;(g 'hard-minimum 0)
;;(g 'hard-maximum 240)
;;(set! g (g divisions))



;;(define (make-plot-array f dt end Mo)
;;  (let loop ((lst (list (list 0 Mo)))
;;				 (t 0))
;;	 (if (> (+ t dt) end)
;;		  (reverse (cons (list (+ t dt) lst)))
;;		  (loop (cons (list t (rk4 f t dt (/ dt 12.0) (cadar lst)))) (+ t dt)))))

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
