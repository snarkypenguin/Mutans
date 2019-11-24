; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	examples.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.09.10
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/examples.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2016 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

;(load  "ptrees.scm")
(load  "pt.scm")


(define (p-n . arg)
  (if (null? arg)
		(if (even? (random-integer 3)) 1 -1)
		(* (if (even? (random-integer 3)) 1 -1) (car arg))))


;---- (random-polynomial-term L s E)  set-of-labels, scale, max exponent
(define (random-polynomial-term L s E) 
  (cond
	((zero? s) (list 0))
	((zero? E) (list (random-integer s)))
	(#t
	 (let* ((te (* (p-n (random-integer (* 2 (+ 1 E))))))
			  (e (if (zero? te) E te))
			  (coef  (* (p-n) (random-integer s)))
			  (facts (map (lambda (x)
								 (list (list-ref L (random-integer (length L)))
										 (* (p-n) (random-integer  e)))
								 ) (seq E)))
			  )
	
		(cons (if (zero? coef) 1 coef) ;; we don't want zero coefficients associated with a factor!
				facts
				))
	 ))
	 )
	


(define (poly-to-tree p)
  (list p '()))


;---- (random-polynomial L S k E)  set-of-labels, Scalar, k terms,  max Exponent
(define (random-polynomial L S k E)
  (let* ((n-terms (random-integer k))
			;;(s (* 2.0 (- (random-real) 0.5) (random-integer S))) ;; scalar part
			(s (* (p-n) (random-integer S))) ;; scalar part
			(pt (map (lambda (x) (random-polynomial-term L (+ 1 (random-integer S)) E)) (seq (+ 1 (random-integer k)))))
			)
	 (normalise-polynomial (cons s pt))))

(define (rndpoly . args)
  (random-polynomial '(w x y z) 5 6 3))


;; the polynomial (5 (7 ((x 2) (y 2))) (1 ((x 1) (y 1))) (1 ((x 1) (y 1))))
;; reads as 5 + 7x^2y^2 + xy + xy


(define pcorp1 (map rndpoly (seq 50)))
(define pcorp2 (map rndpoly (seq 50)))
(define pcorp3 (map rndpoly (seq 50)))





;---- (random-node L w D E z) Labels, (w)eight param for polynomials,  Depth, E= number param for children terms in poly
;; The weights of nodes will naturally progressively dwindle in the extension sets since at each extension the max weight is drawn from [0-W]
(define (random-node L w D E z )
  (if (not (and (list? L) (apply andf (map symbol? L)))) (error "bad label list" L))
  (if (not (apply andf (map integer? (list w D W z))))
		(error "bad numeric argument to random-node" (list w D E z)))
  
  (list (random-polynomial L w z)
		  (if (zero? D)
				'() ;; We have bottomed out depth wise
				(let ((e (random-integer E))) ;; 
				  (if (zero? e)
						'()
						(map (lambda (x) (random-node L w (- D 1) (random-integer (* 2 e)) z)) (seq e))))))
  )

(define (make-random-tree S c e tc d) ;; setofsyms const-range maxexponent termcount depth
  (let ((nS (length S))
		  (exponent (map (lambda (x) (random-integer e)) (make-list tc)))
		  )
	 (if (zero? d)
		  '()
		  (list
			(normalise-polynomial
			 (cons (random-integer c)
					 (map (lambda (x) (list (random-integer (- (* 2 c) c))
													(list (list-ref S (random-integer nS))
															x))) exponent)))
			(!filter null? (map (lambda (x) (make-random-tree S c e tc (- d 1))) (seq (random-integer d))))
			))))

(define (polys . lst)
  (let ((plylst '(
						"3 + 2 * x - 3 * x^2 + 7 * x^3"
						"1 + 2 y + 2 * x^2 + * x^3"
						"2 + * x + * x^2 - * x^3"
						"1 + 2 * x + * x^2 + * x^3"
						"-1 + 2 * x + * x^2 + 2 * x^3"
						"4 + * x + * x^2 * x + 2 * x^3"
						"3 + 2 * x - 3 * x^2 + 7 * x^3"
						"1 + 2 * x + * x^2 + * x^3"
						"1 + 2 * x + 2 * x^2 + 3 * x^3"
						"2 + * x^2 + 3 * x^3")))
	 (cond
	  ((null? lst) (list-ref plylst (random-integer (length plylst))))
	  ((equal? lst '(*)) plylst)
	  (#t (map (lambda (x) (list-ref plylst x)) lst))))
  )

(define (poly . lst)
  (let ((l (apply polys lst)))
	 (if (string? l)
		  (string->polynomial l)
		  (map string->polynomial l))))


(define Tcorpus '(((3 (2 (x 1)) (-3 (x 2)) (7 (x 3))) (((1 (2 (x 1)) (2 (x 2)) (3 (x 3))) ())))
					((1 (2 (x 1)) (1 (x 2)) (1 (x 3))) ())
					((2 (1 (x 1)) (-1 (x 3))) ())
					((1 (2 (x 1)) (1 (x 2)) (1 (x 3))) ())
					((1 (2 (x 1)) (1 (x 2)) (1 (x 3)))
					 (((3 (2 (x 1)) (-3 (x 2)) (7 (x 3))) ())
					  ((1 (2 (x 2)) (1 (x 3)) (2 (y 1))) ())
					  ((2 (1 (x 1)) (1 (x 2)) (-1 (x 3))) ())))
					((3  (-3 (x 2)) (7 (x 3))) (((4 (2 (x 1)) (2 (x 2)) (-2 (x 3))) ())))
					((1 (2 (x 1)) (1 (x 2)) (1 (x 3))) ())
					((4 (1 (x 1)) (3 (x 3))) ())
					((2 (1 (x 2)) (3 (x 3))) ())
					((3 (2 (x 1)) (-3 (x 2)) (7 (x 3))) (((2 (1 (x 1)) (1 (x 2)) (-1 (x 3))) ())))
					((1 (2 (x 1)) (1 (x 2)) (1 (x 3))) ())
					((1 (1 (x 3))) ())
					((-1 (2 (x 1)) (1 (x 2)) (2 (x 3))) ())
					((1 (2 (x 2)) (1 (x 3)) (2 (y 1))) ())
					((1 (2 (x 1))  (1 (x 3))) (((1 (2 (x 2)) (1 (x 3)) (2 (y 1))) ()) ((4 (1 (x 1)) (3 (x 3))) ())))
					((1 (2 (x 1)) ) ())
					((1 (2 (x 2)) (1 (x 3)) (2 (y 1)))
					 (((1 (2 (x 1)) (2 (x 2)) (3 (x 3))) ())
					  ((3 (2 (x 1)) (-3 (x 2)) (7 (x 3))) ())
					  ((1 (2 (x 1)) (1 (x 2)) (1 (x 3))) ())))
					((3 (2 (x 1)) (-3 (x 2)) (7 (x 3))) (((4 (1 (x 1)) (3 (x 3))) ())))
					((1 (2 (x 1)) (1 (x 2)) (1 (x 3))) ())
					((2 (1 (x 1)) (1 (x 2)) (-1 (x 3))) ())))

(define (trees . lst)
  (let ((rtl Tcorpus)
		  )
	 (if (null? lst)
		  (list-ref rtl (random-integer (length rtl)))
		  (map (lambda (x) (list-ref rtl x)) lst))))
;; children....
(define C0 '())
(define C1 (map (lambda (x)
						(list x '()))
					 (map string->polynomial
							(list "1 + 2 x + x^2"
									"7 + x + 7 x^2 x + 7 x^3"))) ;; this last is 4x+3x^3 ;-)
  )

(define C2 (map (lambda (x)
						(list x '()))
					 (map string->polynomial
							(list "1 + 2 x + x^2 + x^3"
									"-1 + 2 x + x^2 + 2 x^3"
									"4 + x + x^2 x + 2 x^3"))) ;; this last is 4x+3x^3 ;-)
  )

(define C3 (map (lambda (x)
						(list x '()))
					 (map string->polynomial
							(list "3 + 2 x - 3 x^2 + 7 x^3"
									"1 + 2 x + x^2 + x^3"
									"1 + 2 x + 2 x^2 + 3 x^3"
									"2 + x^2 + 3 x^3")))
  )

(define (random-test-tree d)
  (let ((k (random-integer 4)) ;; number of kinder
		  )
	 (if (zero? d)
		  (node (polys) '())
		  (node (polys) (map (lambda (x) (random-test-tree (- d 1))) (seq k))))))

(define (rtt . args)
  (random-test-tree 5))

(define ST0 (node 1 C0))
(define ST1 (node 2 C1))
(define ST2 (node 3 C2))
(define ST3 (node 5 C3))

(define T1 (list (string->polynomial "x + 3") (list ST1 ST2)))
(define T2 (list (string->polynomial "5 x + 7") (list ST3 ST2)))

(define X0 (list (string->polynomial "x + 3") (list (list (string->polynomial "a + 7") '()))))
(define Y0 (list (string->polynomial "y + 5") (list (list (string->polynomial "b + 5") '()))))
(define Z0 (list (string->polynomial "z + 7") (list (list (string->polynomial "a + 3") '()))))

(define X1 (list (string->polynomial "x + 3") (list (list (string->polynomial "z^2 + 7") '()))))
(define Y1 (list (string->polynomial "y + 7") (list (list (string->polynomial "x^2 + 1") '()))))
(define Z1 (list (string->polynomial "x^2 + 5") (list (list (string->polynomial "y + 2") '()))))


; Both the st and ast representations work
(define ast1 "(\"1 + 2 * x^2\" ((\"1 - * y + * y^2 + * x * y^2\" ()) (\"5 + * y^2\" ()) (\"4 + 4 * x^2\" ((\"1 + * x\" ())))))")
(define ast2 "(\"1 + 2 * x^2\" ((\"1 - * y + * y^2 + * x^2 * y^2\" ()) (\"5 - * y^2\" ()) (\"1 + 2 * x + * x^2\" ((\"-1 + * x\" ())))))")
(define ast3 "(\"1 + 2 * x^2\" ((\"1 - * y + * y^2 + * x * y^2\" ()) (\"5 + * x * y^2\" ()) (\"4 + 4 * x^2\" ((\"1 + * x\" ())))))")
(define ast4 "(\"1 + 2 * x^2\" ((\"1 - * y + * y^2 + * x * y^2\" ()) (\"5 + * x * y^2\" ()) (\"4 + 4 * x^2\" ((\"1 + * x\" ())))))")

(define st1 "(1 + 2 * x^2 ((1 - * y + * y^2 + * x * y^2 ()) (5 + * y^2 ()) (4 + 4 * x^2 ((1 + * x ())))))")
(define st2 "(1 + 2 * x^2 ((1 - * y + * y^2 + * x^2 * y^2 ()) (5 - * y^2 ()) (1 + 2 * x + * x^2 ((-1 + * x ())))))")
(define st3	"(1 + 2 * x^2 ((1 - * y + * y^2 + * x * y^2 ()) (5 + * x * y^2 ()) (4 + 4 * x^2 ((1 + * x ())))))")
(define st4 "(1 + 2 * x^2 ((1 - * y + * y^2 + * x * y^2 ()) (5 + * x * y^2 ()) (4 + 4 * x^2 ((1 + * x ())))))")

(define ct1 "(2  ((1 - * y + * y^2 + * x * y^2 ()) (5 + * y^2 ()) (4 + 4 * x^2 ((1 + * x ())))))")
(define ct2 "(3  ((1 - * y + * y^2 + * x^2 * y^2 ()) (5 - * y^2 ()) (1 + 2 * x + * x^2 ((-1 + * x ())))))")
(define ct3 "(5  ((1 - * y + * y^2 + * x * y^2 ()) (5 + * x * y^2 ()) (4 + 4 * x^2 ((1 + * x ())))))")
(define ct4 "(7  ((1 - * y + * y^2 + * x * y^2 ()) (5 + * x * y^2 ()) (4 + 4 * x^2 ((1 + * x ())))))")

(define st1+2 "(2 + 4 x^2 ((1 - y + y^2 + x y^2 ())(1 - y + y^2 + x^2 y^2 ())(5 ())(4 + 4 x^2 ((1 + x ())))(1 + 2 x + x^2 ((-1 + x ())))))")

(define t1 (string->tree st1))
(define t2 (string->tree st2))
(define t3 (string->tree st3))
(define t4 (string->tree st4))

(define c1 (string->tree ct1))
(define c2 (string->tree ct2))
(define c3 (string->tree ct3))
(define c4 (string->tree ct4))


(define U1 (list c1 c2 c3 c4 t1 t2 t3 t4 X1 Y1 X0 Y0 Z0 ST0 ST1 ST2 ST3))

(define v1 'unused)
(define v2 'unused)
(define v3 'unused)
(define v4 'unused)
(define v5 'unused)
(define v6 'unused)
(define v7 'unused)
(define v8 'unused)


(define s1 (string->tree "(2 x^2 - 2 x + 2 {(3 y + x + 1 {})})"))
(define s2 (string->tree "(2 x^2 + 2 x + 2 {(3 y + x + 1 {})})"))
(define s3 (string->tree "(2 x^2 - 2 {(3 y - x + 1 {})})"))
(define s4 (string->tree "(2 x^2 + 2 x + 2 {(3 y - x - 1 {})})"))

(define (test-associativity-+ p q r)
  (dnl "Testing additive associativity")
  (if (equal?
		 (tree+ p (tree+ q r))
		 (tree+ (tree+ p q) r))
		'associative-addition
		'non-associative-addition
		))

(define (debug-non-associative-addition p q r)
  (dnl "debugging additive associativity")
  (set! v1 p)
  (set! v2 (tree+ q r))
  (set! v3 (tree+ p (tree+ q r)))
  (set! v4 (tree+ p q))
  (set! v5 r)
  (set! v6 (tree+ (tree+ p q) r))
  (if (equal? v3 v6) 'associative-addition  'non-associative-addition )
  )

(define (test-commutativity-+ p q)
  (dnl "Testing additive commutativity")
  (if (equal? (tree+ p q) (tree+ q p))
		'commutative-addition
		'non-commutative-addition
  ))

; "4 * x^2 - 2 * x + 3"  "-4 x^2 + 2*x + 1"

(define (debug-non-associative-multiplication p q r)
  (dnl "debugging multiplicative associativity")
  (set! v1 p)
  (set! v2 (tree* q r))
  (set! v3 (tree* p (tree* q r)))
  (set! v4 (tree* p q))
  (set! v5 r)
  (set! v6 (tree* (tree* p q) r))
  (if (equal? v3 v6) 'associative-addition  'non-associative-multiplication )
  )

(define (test-associativity-* p q r)
  (dnl "Testing multiplicative associativity")
  (if (equal?
		 (tree* p (tree* q r))
		 (tree* (tree* p q) r))
		'associative-multiplication
		'non-associative-multiplication
		))

(define (test-commutativity-* p q)
  (dnl "Testing multiplicative commutativity")
  (if (equal? (tree+ p q) (tree+ q p))
		'commutative-multiplication
		'non-commutative-multiplication
		))


(define (test-distribution p q r)
  (dnl "Testing distribution of multiplication over addition")
  (if (equal? (tree+	(tree* p q) (tree* p r))
				  (tree* p (tree+ q r)))
		'multiplication-distributes-over-addition
		'multiplication-does-not-distribute-over-addition
		))

(define (debug-multiplication-does-not-distribute-over-addition p r s)
  (dnl "debugging multiplicative distribution over addition")
  (set! v1 (tree* p r))
  (set! v2 (tree* p s))
  (set! v3 (tree+ v1 v2))
  (set! v4 (tree+ r s))
  (set! v5 (tree* p v4))
  (set! v6 (tree* v4 p))

  (if (equal? (tree+
					(tree* p r) (tree* p s))
				  (tree* p (tree+ r s)))
		(tree* p (tree+ r s))
		(list 'do-not-match
				(tree+
				 (tree* p r) (tree* p s))
				(tree* p (tree+ r s)))))


(define (test-multiplication-compatibility s p)
  'compatibility-of-multiplications-not-tested)


(define (test-simple-properties p q r)
	(test-associativity-+ p q r)
	(test-commutativity-+ p q)
	(test-associativity-* p q r)
	(test-commutativity-* p q)
	)

(define (test-properties p q r )
  (let*	 ((S '(associative-addition
					 commutative-addition
					 associative-multiplication
					 commutative-multiplication
					 multiplication-distributes-over-addition))
			  (R (list
					(test-associativity-+ p q r)
					(test-commutativity-+ p q)
					(test-associativity-* p q r)
					(test-commutativity-* p q)
					(test-distribution p q r)))
			  (F (filter (lambda (x) (not (member x S))) R)))

	 (if (null? F)
		  'all-good
		  F
		  )))





(define A0 "(1 + a {})")
(define a0 (string->tree A0))
(define A1 "(1 + a^2 {(a + a^2 {}) (3 + 4 a + a^2 {})})")
(define a1 (string->tree A1))
(define A2 "(1 + a + a^2 {(2 + a^2 {}) (7 + a - 2 a^2 {})})")
(define a2 (string->tree A2))
(define B1 "(1 + b^3 {})")
(define b1 (string->tree B1))
(define B2 "(4 + b^3 {(-7 + b^2 {}) (7 + b^3 {})})")
(define b2 (string->tree B2))

(define P-tp 'ok)
(define Q-tp 'ok)
(define R-tp 'ok)

(define x1 (string->tree "(x^2 + 2 x y + y^2 {(2 x + 2 {}) (2 y - 2 x + 2 {}) (-1 y {})})"))
(define x2 (string->tree "(x^2 - 2 x y + y^2 {(2 x - 2 {}) (2 y + 2 x + 2 {}) (-1 y {})})"))
(define x3 (string->tree "(x^2 - 2 x y - y^2 {(2 x - 2 {}) (2 y + 2 x - 2 {}) (y {})})"))

  	
(define (test-properties! n d)
  (if (positive? n)
		(let ((p (random-test-tree d))
				(q (random-test-tree d))
				(r (random-test-tree d)))
		  (let ((R (test-properties p q r)))
			 (if (not (equal? R '(associative-addition
										 commutative-addition
										 associative-multiplication
										 commutative-multiplication
										 multiplication-distributes-over-addition)))
				  (begin
					 (set! P-tp p)
					 (set! Q-tp q)
					 (set! R-tp r)
					 (dnl "Failed at " n))
				  (test-properties (- n 1)))))
		(dnl "passed all trials")))

				  



(define %t1 "(x^2 - 2 x + 3 {(a + b - c {}) (a - b + c {})})")
(define %t2 "(x^2 + 2 x + 3 {(a + b - c {}) (a - b - c {})})")
(define %t3 "(x^2 + 2 x - 3 {(a - b - c {}) (a - b + c {})})")
(define %t4 "(x^2 - 2 x - 3 {(a + b - c {}) (a + b + c {})})")

(define t1*t2*t3_lbl "x^6 + 2 x^5 - x^4 + 4 x^3 + 3 x^2 + 18 x - 27")

(define tt1 (string->tree %t1))
(define tt2 (string->tree %t2))
(define tt3 (string->tree %t3))
(define tt4 (string->tree %t4))


(define e1 (string->tree "(x^2 - 2 x + 3 {(a + b - c {}) (a - b + c {})})"))
(define e2 (string->tree "(x^2 + 2 x + 3 {(a + b - c {}) (a - b - c {})})"))
(define e3 (string->tree "(x^2 + 2 x - 3 {(a - b - c {}) (a + b - c {})})"))
(define e4 (string->tree "(x^2 - 2 x - 3 {(a + b - c {}) (a + b + f {})})"))


(define (rndtree)
  (make-random-tree '(x y z) 24 3 5 4))

(define rtrees (map (lambda x (rndtree)) (seq 15)))
(define (run-trial) (apply test-properties (list-ref rtrees (map (lambda x (random-integer (length rtrees))) (seq 3)))))

(define (*+* r1 r2 r3)
  (tree+ (tree* r1 r2) (tree* r1 r3)))

(define (*+ r1 r2 r3)
  (tree* r1 (tree+ r2 r3)))

(define (*+*ii r1 r2 r3)
  (tree+ (tree*ii r1 r2) (tree*ii r1 r3)))

(define (*+ii r1 r2 r3)
  (tree*ii r1 (tree+ r2 r3)))


(define (run-iitrial)
  (let* ((tlist (list-ref rtrees (map (lambda x (random-integer (length rtrees))) (seq 3))))
			(A (tree+ (tree* (car tlist) (cadr tlist)) (tree* (car tlist) (caddr tlist))))
			(Aii (tree+ (tree*ii (car tlist) (cadr tlist)) (tree*ii (car tlist) (caddr tlist))))
			(B (tree* (car tlist) (tree+ (cadr tlist) (caddr tlist))))
			(Bii (tree* (car tlist) (tree+ (cadr tlist) (caddr tlist)))))
	 (dnl "a = b " (equal? A B))
	 (dnl "a = aii " (equal? A Aii))
	 (dnl "b = bii " (equal? B Bii))
	 ))
	 

;;; (dnl "Starting stage 1") ;;;

;;; (define tstsize 10) ;;;
;;; (define pc1 (map rndpoly (seq tstsize))) ;;;
;;; (define pc2 (map rndpoly (seq tstsize))) ;;;
;;; (define pc3 (map rndpoly (seq tstsize))) ;;;

;;; (dnl "Stage 2") ;;;

;;; (define lspc1 (n-Lambda^sigma pc1)) ;;;
;;; (define lspc2 (n-Lambda^sigma pc2)) ;;;
;;; (define lspc3 (n-Lambda^sigma pc3)) ;;;

;;; (dnl "Stage 3 " (length lspc1) ", "(length lspc2) " & " (length lspc3)) ;;;

;;; (define p1p2 (map* p* pc1 pc2)) ;;;
;;; (dnl "...") ;;;
;;; (define p2p3 (map* p* pc2 pc3)) ;;;

;;; (dnl "Stage 4 " (length p1p2) " & " (length p2p3)) ;;;

;;; (define lsp12 (n-Lambda^sigma p1p2)) ;;;
;;; (dnl "...") ;;;
;;; (define lsp23 (n-Lambda^sigma p2p3)) ;;;

;;; (dnl "Stage 5") ;;;

;;; (define lsp1p2*p3 (map* p* lsp12 pc3)) ;;;
;;; (dnl "...") ;;;
;;; (define p1*lsp2p3 (map* p* pc1 lsp23)) ;;;

;;; (dnl "Stage 6") ;;;

;;; (define llsp12*3 (n-Lambda^sigma lsp1p2*p3)) ;;;
;;; (dnl "...") ;;;
;;; (define llsp1*23 (n-Lambda^sigma p1*lsp2p3)) ;;;
;;; (dnl "Done") ;;;


;-  The End


;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
