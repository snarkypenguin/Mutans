;-  Identification and Changes

;--
;	lists.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.12.13
;		Location: odin:/home/gray/study/src/lists.scm
;
;	History:
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 


(define (map-apply proc lst)
  (map (lambda (x) (apply proc x)) lst))


;; THIS DOES NOT QUITE CONFORM TO make-list!!!: (make-list* '(1 2 3) 9) => (((9 9 9) (9 9 9)))
(define (make-list* . dims)
  (let ((defval 0))
	 (if (and (pair? dims) (pair? (car dims))) 
		  (begin
			 (if (pair? (cdr dims))
				  (set! defval (cadr dims)))

			  (set! dims (car dims))))

		  (if (null? (cdr dims))
				(make-list (car dims) defval)
				(map (lambda (x) (make-list* (cdr dims) defval)) (make-list (car dims) ))))
)

(define (n-arity-from-2-arity op identity)
  (lambda args
	 (if (not (list? args)) (error "bad list to generalised operator" args))
	 (let loop ((a args)
					(r identity))
		(cond
		 ((null? a) r)
		 ((= (length a) 1) (op (car a) r))
		 (#t (loop (cdr a) (op (car a) r)))))))


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

;; *** this is wonky ***
;; return the cross product of n lists (state spaces)  
(define (cross__ . args)
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
	(#t (cross__ (car args) (apply cross__ (cdr args))))))



(define (cross* . args)
  (define (cross2 a b)
	 (apply append (map (lambda (x) (map (lambda (y) 
														(if (list? y)
															 (cons x (list y))
															 (list x y))) b)) a)))
  (cond
	((not (list? args)) (bad-argument))
	((null? args) '())
	((= (length args) 1)
	 (car args))
	((= (length args) 2)
		(apply cross2 args))
	(#t (cross* (car args) (apply cross* (cdr args))))))


(define (listify a)
  (if (list? a) a (list a)))



;; this expects a list like '(a b c (d (e f) g (h i (j k l)) m) n o p ((q r) s)  t) and constructs an list of
;; index values that iterate through the list comprehensively -- see the commented bits for blart and blartix 


(define (generate-iteration-list L)
  (let ((S (seq (length L))))
	 (apply append (map (lambda (x s) (cond
							 ((null? (list-ref L s)) (list s))
							 ((atom? (list-ref L s)) (list (list s)))
							 ((list? (list-ref L s))
							  (let ((sl (process (list-ref L s))))
								 (map (lambda (y) (append (list s) (if (list? y) y (list y)))) sl)))
							 (#t (error "bad tackle"))))
			L S))))
						  

;;; ;;  Valid fully resolved index lists for this include
;;; (define blartix '((0)
;;; 						(1)
;;; 						(2)
;;; 						(3 0)
;;; 						(3 1 0)
;;; 						(3 1 1)
;;; 						(3 2)
;;; 						(3 3 0)
;;; 						(3 3 1)
;;; 						(3 3 2 0)
;;; 						(3 3 2 1)
;;; 						(3 3 2 2)
;;; 						(3 4)
;;; 						(4)
;;; 						(5)
;;; 						(6)
;;; 						(7 0 0)
;;; 						(7 0 1)
;;; 						(7 1)
;;; 						(8)))

;;; (define blart '(a b c
;;; 						(d
;;; 						 (e f)
;;; 						 g
;;; 						 (h i
;;; 							 (j k l))
;;; 						 m)
;;; 						n o p
;;; 						((q r)
;;; 						 s)
;;; 						t)
;;;   )



(define (depth* l)
   (let loop ((tl l) (d 0))
     (if (not (pair? tl))
         d
         (max (loop (car tl) (+ d 1)) (loop (cdr tl) d)))))

 (define (length* l)
     (if (not (pair? l))
         0
			(apply max (map length* tl))))

(define (simple-list? lst)
  (or (null? lst) 
		(and (pair? lst) 
			  (atom? (car lst)) 
			  (simple-list? (cdr lst)))))


;; (map* (lambda (v l d) (+ (random-integer 42) (* d 10) (* +1.0i l))) '((a b (c (d e) f)) (g h i) ((j k) l)))

(define (map* proc list* . d) ;; proc is of the form (lambda (listval index deepness) ...)
  (set! d (if (null? d) 0 (car d)))
  
  (if (simple-list? list*)
		(map proc list* (seq (length list*)) (make-list (length list*) d))
		(map (lambda (l* l d)
				 (if (atom? l*)
					  (proc l* (if (list? l) (car l) l)  (if (list? d) (car d) d))
					  (map* proc l* d))
				 )
			  list* (seq (length list*)) (make-list (length list*) d))
		)
  )



;; guarded
(define list-ref
  (letrec ((%list-ref list-ref))
    (lambda (l i)
      (if (or (< i 0) (>= i (length l)))
          (abort 'list-ref-index-out-of-bounds)
          (%list-ref l i)))))

; Takes sets an element in a list
(define (list-set! l i v)
   (if (zero? i)
       (set-car! l v)
       (list-set! (cdr l) (1- i) v)))


;; the index can be a list of indices
;; list-ref that accepts a list of indices
(define list-ref
	 (let ((olr list-ref))
		(lambda (lst ix)
		  (if (number? ix) (olr lst ix) (map (lambda (y) (olr lst y)) ix)))))


;; the index can be a list of indices (and if it is the value must be a corresponding list)
(define list-set!
  (letrec ((%list-set! list-set!)
			  )
	 (lambda (l i v)
		(if (list? i)
			 (for-each (lambda (x y) (%list-set! l x y)) i v)
			 (%list-set! l i v)))))


;;; (define (list-ref* lst ix)
;;;   (cond
;;; 	((and (not (list? is)) (not (number? ix)))
;;; 	 (error "Bad index passed to list-ref*" ix))
;;; 	((number? ix)
;;; 	 (list-ref lst ix))
;;; 	((and (pair? ix) (= (length ix) 1))
;;; 	 (list-ref lst (car ix)))
;;; 	(else
;;; ;;		(list-ref* (map (lambda (x) (list-ref x (car ix))) lst) (cdr ix))
;;; 		(list-ref (map (lambda (x) (list-ref* x (cdr ix))) lst) (car ix))
;;; 		)))

(define (list-ref*? lst ix)
  (cond
	((and (number? ix) (<= 0 ix) (< ix (length lst)))
	 #t)
	((and (pair? lst) (number? (car ix)) (<= 0 (car ix)) (< (car ix) (length lst)))
	 (list-ref*? (list-ref lst (car ix)) (cdr ix)))
	((and (null? lst) (not (null? ix))) #f)
	((and (null? ix) (not (null? lst))) #t)
	(#t #t)))

(define (list-ref* lst ix)
  (cond
	((and (list? ix) (= 1 (length ix))) (list-ref lst (car ix)))
	((and (number? ix) (<= 0 ix) (< ix (length lst)))
	 (list-ref lst ix))
	((and (pair? lst) (number? (car ix)) (<= 0 (car ix)) (< (car ix) (length lst)))
	 (list-ref* (list-ref lst (car ix)) (cdr ix)))
	((and (null? lst) (not (null? ix))) (error "Index list ran off the end of the list" ix))
	((and (null? ix) (not (null? lst))) lst)
	(#t (error "Should never get here, bad arguments" lst ix))))


(define (list-set* lst ix vv)
  (cond
	((number? ix)
	 (list-set! lst ix vv))
	((= (length ix) 1)
	 (dnl "unit list")
	 (list-set! lst (car ix) vv))
	(else
	 (let ((tv (list-ref* lst ix)))
		(if (atom? tv)
			 ;; indices fully resolve an element
			 (let* ((short-ix (reverse (cdr (reverse ix))))
					  (tv (list-ref* lst short-ix)))
				(list-set! tv (car (reverse ix)) vv))
			 (if (= (length tv) (length vv))
				  ;; it's ok, do it
				  (list-set* (map (lambda (x) (list-ref! x (car ix)) lst) (cdr ix)) vv)
				  (abort "The value list does not have the indicated number of elements"))))
	 )))



;; return the cross product of two lists (state spaces)
(define (*cross2* a b) 
  (apply append (map (lambda (x) (map (lambda (y) (list x y)) b)) a)))

;; return the cross product of n lists (state spaces)
(define (*cross* . args) 
  (define (*cross2* a b) 
	 (apply append (map (lambda (x) (map (lambda (y) (if (list? y) (cons x y) (list x y))) b)) a))) 
  (cond 
	((not (list? args)) 'bad-argument) 
	((null? args) '()) 
	((= (length args) 1)  (car args))	
	((= (length args) 2)	(apply *cross2* args))	
	(#t (*cross* (car args) (apply *cross* (cdr args))))))



;; The dimensions of this are 2 3 3 3
(define **doink '((((a b c) (d e f) (g h i)) 
					  ((k l m) (n o p) (q r s)) 
					  ((t u v) (w 1 2) (3 4 5)))
					 (((A B C) (D E F) (G H I)) 
					  ((K L M) (N O P) (Q R S)) 
					  ((T U V) (W 10 20) (30 40 50)))
					 )
  )




;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
