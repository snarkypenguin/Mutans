

(define (--1 x) (- x 1))
(define (++1 x) (+ x 1))

(define (unique s)
  (let loop ((q s)
				 (r '())
				 )
	 (if (null? q)
		  r
		  (if (member (car q) (cdr q)) ;; not unique
				(loop (cdr q) r)
				(loop (cdr q) (cons (car q) r))))))



(define (permute lst . D)
  (if (null? D) (set! D 0) (set! D (car D)))
  (if (list? lst)
		(cond	
		 ((null? lst) 
		  '())
		 ((null? (cdr lst)) 
		  (list lst))
		 (#t
		  (apply append (map {lambda (head) 
								(let [(tail (permute (filter (lambda (y) (not (equal? y head))) lst) (++1 D)) )]
								  (map {lambda (x)
											(append (cons head '()) x)
											  } tail))}
									lst))
			 ))
		#f
		))

(define (rotate-list lst n)
  (let ((list (copy-list lst))
		  (len (length lst)))
		  
	 (cond 
	  ((or (and (< n 0) (>= (+ len n) len)) (>= n len))
		#f)
	  ((zero? n) list)
	  ((< n 0) (append (list-tail list (+ len n)) (list-head list (+ len n))))
	  ((> n 0) (append (list-tail list n) (list-head list n)))
	  (#f #f))))

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
