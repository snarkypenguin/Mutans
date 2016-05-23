
(define (axiom-of-choice selector lst)
  (let ((M (selector lst)))
    (filter M lst)))

(define (maxima lst)
  (axiom-of-choice
   (lambda (lst)
     (let ((M (apply max lst)))
       (lambda (x) (= M x))))
   lst))
        
(define (minima lst)
  (axiom-of-choice
   (lambda (lst)
     (let ((M (apply min lst)))
       (lambda (x) (= M x))))
   lst))
        

;; Given '((0 1 2 3 4 ... n) (10 11 ... (+10 n)) return ((0 10) (1 11) ...)
(define (make-tuples f)
  (map (lambda (x y) (list x y)) (car f) (cadr f)))


;; return the cross product of two lists (state spaces)
(define (cross2 a b)
  (apply append (map (lambda (x) (map (lambda (y) 
														  (list x y)) b)) a)))

;; return the cross product of n lists (state spaces)
(define (cross . args)
  (define (cross2 a b)
	 (apply append (map (lambda (x) (map (lambda (y) 
														(if (list? y)
															 (cons x y)
															 (list x y))) b)) a)))
  (cond
	((not (list? args)) (bad-argument))
	((null? args) '())
	((= (length args) 1)
	 (car args))
	((= (length args) 2)
		(apply cross2 args))
	(#t (cross (car args) (apply cross (cdr args))))))


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
