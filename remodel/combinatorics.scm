;-  Identification and Changes

;--
;	combinatorics.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2015.08.25
;		Location: pooh.grayrabble.org:/local/home/randall/scm/combinatorics.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2015 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

(define-macro (define-unbound sig #!rest body)
  (let* ((name  (if (pair? sig) (car sig) sig))
			(ok (##unbound? (##global-var-ref (##make-global-var name))))
			)
	 (if ok `(define ,sig ,@body))))

;-  Code 

(define (factorial n)
  (if (or (negative? n) (not (integer? n)))
		#f
		(if (zero? n) 1 (* n (factorial (- n 1))))))

(define (nPk n k)
  (if (or (negative? n) (not (integer? n)))
		#f
		(/ (factorial n) (factorial (- n k)))))				

;; "n choose k"
(define (nCk_0 n k)
  (/ (nPk n k) (factorial k)))

;; "n choose k"
(define (nCk n k)
  (if (or (negative? n) (not (integer? n)))
		#f
		(/ (factorial n) (* (factorial k) (factorial (- n k))))))				

;;; Note:
;;;   ((n+1 : k-1)) === (n + k - 1 : k-1) === (n + k - 1 : n)



(define (pascals-triangle-line-n n)
  (map (lambda (x) (nCk n x)) (seq (+ 1 n))))


(define (pascals-triangle-n n)
  (map pascals-triangle-line-n (seq n)))

;; "n multichoose k"
;; In the context of dice, the number of sides corresponds to bins (n), and the number of dice to stars (k)
(define (nCk* n k) ;; draw k with repetitions, order irrelevant
  (nCk (+ n k -1) k))


;;; (load "combinatorics")
;;; > (nCk* 4 10) ;; From the wiki page "Combination".
;;; 286


(define (bars-and-stars str)
  (let* ((lst (string->list str))
			(starcount (length (filter (lambda (x) (not (char=? x #\|))) lst)))
			(barcount (+ 1 (length (filter (lambda (x) (char=? x #\|)) lst))))
			)
	 ;;(if (char=? (list-ref lst 0) #\|) (set! barcount (+ barcount 1)))
	 ;;(if (char=? (list-ref lst (- (length lst) 1)) #\|) (set! barcount (+ barcount 1)))
	 (list barcount starcount)))
			 
(define (bars-and-stars-count str)
  (apply nCk* (bars-and-stars str)))

(define-unbound (bit-set? n i)
  (not (zero? (bitwise n i))))

(define (high-order-bit n)
  (let loop ((i 0))
	 (if (> (power 2 i) n)
		  i 
		  (loop (+ i 1)))))


;;; (define (bitwise a b)
;;;   (display (bitwise-and a b))
;;;   (newline)
;;;   (display (bitwise-ior a b))
;;;   (newline)
;;;   (display (bitwise-xor a b))
;;;   (newline)
;;;   (display (bitwise-not a))
;;;   (newline)
;;;   (display (bitwise-arithmetic-shift-right a b))
;;;   (newline))
 
;;; (bitwise 255 5)


;;(define (bs-list sides dice)
;;  (let ((hob (high-order-bit  
;;
;;  (let loop ((

;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
















