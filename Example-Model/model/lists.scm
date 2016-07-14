;-  Identification and Changes

;--
;	lists.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.12.13
;		Location: odin:/home/gray/study/src/lists.scm
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

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 


;; THIS DOES NOT CONFORM TO make-list!!!
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

(define (simple-list? lst)
  (or (null? lst) 
		(and (pair? lst) 
			  (atom? (car lst)) 
			  (simple-list? (cdr lst)))))

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


(define (list-ref* lst ix)
  (cond
	((number? ix)
	 (list-ref lst ix))
	((= (length ix) 1)
	 (list-ref lst (car ix)))
	(else
;;		(list-ref* (map (lambda (x) (list-ref x (car ix))) lst) (cdr ix))
		(list-ref (map (lambda (x) (list-ref* x (cdr ix))) lst) (car ix))
		)))

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
