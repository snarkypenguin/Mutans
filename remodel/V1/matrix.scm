#|

These matrices are indexed from 1.  At the moment there is no way to augment
a matrix or to overlay a small matrix onto a larger on (say for building a 
composite matrix like

                        [A  B]
                        [C  D]


(A i j)  -- i and j can either be indices, or lists of indices
	is the dereferencing operation

(A 'det) == (A 'determinant) == (determinant A)

(A 'cof) (A 'cofactor) == (cofactor A)

(A 't) == (A 'T) == (A 'tr) == (A 'transpose) == (transpose A)
	returns the transpose of A

(A 't!) == (A 'T!) == (A 'tr!) == (A 'transpose!)
	transposes A

(A 'nr) 
	the number of rows in A

(A 'nc) 
	the number of columns in A

(A 'r i) == (A 'row i) returns the row matrix associated with
	row i

(A 'c i) == (A 'col i) (A 'column i) returns the column matrix 
	associated with column i

(A 'comp-mat i j) 
	is the complementary matrix of ij (where the same rules apply to 
	ij as for dereferencing)											  

(A 'conj) == (A 'conjugate)
	returns the complex conjugate of A

(A 'conj!) == (A 'conjugate!)
	sets A to its complex conjugate

(matrix-element-map f mat1 ...)
	applies the function f element-wise to the indicated matrices
	and produces a matrix.  The matricies must all have the same
	number of rows and columns

(A 'minor i j)
	returns the minor associated with ij

(A 'adj) == (A 'adjoint) == (A 'adjugate) 
   returns the transpose of the cofactor matrix of A

(A 'inv) == (A 'inverse) 
	returns the inverse of A

Elementary operations are row!* row!/ row!+ row!- row!/-

(A 'set i j v)
	sets element ij to v (*no* lists in ij)


(@ * A ...) 
	is matrix multiplication

(@ + A ...)
	is matrix addition


|#


(load "maths.scm")

(define (deep-copy l)
  (cond
	((null? l) l)
	((pair? l) (cons (deep-copy (car l)) (deep-copy (cdr l))))
	(else  l)))

(define (one? x) (= x 1))

;; (list-nth* '((a b c) (d e f) (g h i) (j k l)) 2) => (c f i l)
(define (list-nth* lst n) (apply (lambda x (map (lambda (y) (list-ref y n)) x)) lst))

;; (!list-nth* '((a b c) (d e f) (g h i) (j k l)) 2) => ((a b) (d e) (g h) (j k))

;; cols *must* be sorted!
(define (list-ref-cols L cols)
  (pivot (list-ref-rows (pivot L) cols)))

;; rows *must* be sorted!
(define (list-ref-rows L rows)
  (cond
	((number? rows)
	 (list (list-ref L rows)))
	((null? rows) '())
	(else
	 (map (lambda (x) (list-ref L x)) rows))))

;; cols *must* be sorted!
(define (!list-ref-cols L cols)
  (if (number? cols) (set! cols (list cols)))
  (pivot (!list-ref-rows (pivot L) cols)))

;; rows *must* be sorted!
(define (!list-ref-rows L rows)
  (if (number? rows) (set! rows (list rows)))

  (let ((n (length rows)))
	 ;;(dnl "L: " L)
	 ;;(dnl "rows: " rows)
	 (cond
	  ((or (null? rows) (null? L))
		L)
	  ;;((and (= n 1)  (zero? (car rows))) '())
	  (else 
		(!list-ref-rows (append (list-head L (car rows)) 
										(list-tail L (1+ (car rows)))
										)
							 (if (null? (cdr rows)) 
								  '() 
								  (map 1- (cdr rows)))
							 )))))

(define (complementary-list-matrix LM rows cols)
  ;;(dnl "complementary-list-matrix" LM)
  ;;(dnl "rows: " rows ", columns: " cols)
  (if (not (pair? rows)) (set! rows (list rows)))
  (if (not (pair? cols)) (set! cols (list cols)))
  (!list-ref-cols (!list-ref-rows LM (map 1- rows)) (map 1- cols)))

;; This only works with lists ... this is the transpose of a list of lists.
(define (pivot x)
  (cond 
	((atom? x) x)
	((and (pair? x) (pair? (car x)))
	 (map (lambda (y) (list-nth* x y)) (seq (length (car x)))))
	((and (pair? x) (number? (car x))) ;; pivot a row vector
	 (abort "pivot-failure: not a matrix list")
	 (map list x)) 
	(else (abort "pivot failure"))))

(define (transpose A) (A 'T))

(define (complex-conjugate c)
  (let ((rp (real-part c))
		  (cp (complex-part c)))
	 
	 (if (zero? cp)
		  rp
		  (+ rp (* 0-1i cp)))))


(define (make-lists . args)
  (define (mlli r c . i)
	 (if (pair? i)
		  (make-list r (make-list c (car i)))
		  (make-list r (make-list c 0))))
  (define (mllm M)
	 (deep-copy (M)))

  (if (= (length args) 1)
		(mll (car args))
		(apply mlli args)))



;; L must be a function of the form (lambda (e) ...)
(define (matrix-element-map1 L M)
  (if (not (procedure? L))
		(abort "Missing operator in matrix-element-map1...")
		(make-matrix (map (lambda (x) (map L x)) (M)))))

;; L must be a function of the form (lambda e ...)
(define (matrix-element-mapN L . M)
  (if (not (procedure? L))
		(abort "Missing operator in matrix-element-mapN...")
		(make-matrix (apply map (lambda x (apply map L x)) (map (lambda (x) (x)) M)))))

(define matrix-element-map matrix-element-mapN)

(define (*2m A B)
  (let ((a (A)) (b ((B 'T))))
	 (make-matrix (map (lambda (r) 
			  (map (lambda (c) (apply + (map * r c))) b)
			  ) a))))
		  

(define (@ op . args)
  (let* ((n (length args))
		  (nums (filter number? args))
		  (nn (length nums)))
  (cond
	((zero? n) (op))
	((not (eq? op *)) (apply matrix-element-map args))
	((= n nn) (apply op args))
	((zero? nn) ;; all matrices....
	 (frisk-values (cond
						 ((= n 1) (make-matrix ((car args))))
						 ((= n 2) (apply *2m args))
						 (else 
						  (let ((m (*2m (car args) (cadr args))))
							 (apply @ * (cons m (cddr args))))))))
	(else ;; mix of numbers and matrices
	 (let ((N (apply op nums))
			 (M (apply @ op (filter (lambda (x) (not (number? x))) args)))
			 )
		(frisk-values (matrix-element-map1 (lambda (x) (op N x)) M))))
	)))

		




;; L must be a function of the form (lambda (M i j) ...)
(define (matrix-map L M)
  (let ((m (M)))
	 (let loop-i ((i (seq (M 'nr))))
		(if (null? i)
			 (make-matrix m)
			 (let loop-j ((j (seq (M 'nc))))
				(if (null? j)
					 (loop-i (cdr i))
					 (begin 
						(list-set! (list-ref m (car j)) (car i) (L M (1+ (car i)) (1+ (car j))))
						(loop-j (cdr j)))))))))

(define (frisk-values M)
  (matrix-element-map (lambda (e)
								(let* ((re (real-part e)) (im (imag-part e))
										(rre (round re)) (rim (round im)))
								  (if (< (abs (- rre re)) 1e-13) (set! re rre))
								  (if (< (abs (- rim im)) 1e-13) (set! im rim))
								  (if (= (inexact->exact re) re) (set! re (inexact->exact re)))
								  (if (= (inexact->exact im) im) (set! im (inexact->exact im)))

								  (cond
									((and (zero? re) (zero? im)) 0)
									((zero? im) re)
									((zero? re) im)
									(else (+ re (* im 0+i))))))
							 M))

;; sum of ((lambda (x)....) i) from i in [m, M]
(define (sum m M lmbda)
  (apply + (map lmbda (map (lambda (p) (+ m p)) (seq (1+ (- M m)))))))

;; product of ((lambda (x)....) i) from i in [m, M]
(define (prod m M lmbda)
  (apply * (map lmbda (map (lambda (p) (+ m p)) (seq (1+ (- M m)))))))


(define (general-determinant A)
  ;;(dnl "general-determinant: " (A))
  (sum 1 (A 'nc) (lambda (i) (* (if (odd? i) 1 -1) (A 1 i) (determinant (A 'comp-matrix 1 i))))))

(define (identity n)
  (let ((I (deep-copy (make-list n (make-list n 0)))))
	 (for-each (lambda (i) (list-set! (list-ref I i) i 1)) (seq n))
	 (make-matrix I)
	 ))


(define (determinant A)
	(let ((a (A))
			(nr (A 'nr))
			(nc (A 'nc)))

	  (cond
		((not (= nr nc)) (abort "determinant: non-square matrix"))
		((= nr 1) 
		 ;;(dnl "   **   1x1 matrix; returning " (caar a))
		 (caar a))
		((= nr 2)
		 (let ((r (- (* (A 1 1) (A 2 2)) (* (A 1 2) (A 2 1)))))
			;;(dnl "   **   2x2 matrix; returning " r)
			r))
		
		((= nr 3) 
		 (let ((r (- (+ (* (A 1 1) (A 2 2) (A 3 3))
							 (* (A 1 2) (A 2 3) (A 3 1))
							 (* (A 1 3) (A 2 1) (A 3 2)))
						 (+ (* (A 1 3) (A 2 2) (A 3 1))
							 (* (A 1 2) (A 2 1) (A 3 3))
							 (* (A 1 1) (A 2 3) (A 3 2))))))
			;;(dnl "   **   3x3 matrix; returning " r)
			r))
		
		(else 
		 (general-determinant A))))
  )

(define (cofactor M i j)
  (M 'cofactor i j))

(define (!span l kl)
  (if (member (car l) kl)
		(!span (cdr l) kl)
		l))

(define (span l kl)
  (let ((n (!span l kl)))
	 (list-head l (- (length l) (length n)))))



;; This is a matrix wrapped in a function.  if one does a (define A (make-matrix ...))
;; then A will reference A, while (A) will reference a *copy* of A
(define (make-matrix . args)
  (let ((M '())
		  (rows #f)
		  (columns #f)
		  )
	 (letrec ((matrix
				  (lambda x
					 (let* ((N (length x))
							  (car-is-n (and (>= N 1) (number? (car x))))
							  (car-is-l (and (>= N 1) (list? (car x))))
							  (cadr-is-n (and (> N 1) (number? (cadr x))))
							  (cadr-is-l (and (> N 1) (list? (cadr x))))
							  (caddr-is-n (and (> N 2) (number? (caddr x))))
							  (caddr-is-l (and (> N 2) (list? (caddr x))))
							  )
						(cond
						 ((null? x)
						  ;; return the matrix as a list-of-lists
						  (deep-copy M)
						  )
						 ((member (car x) '(r-length row-length nc n-cols n-columns))
						  columns
						  )
						 ((member (car x) '(c-length col-length column-length nr n-rows))
						  rows
						  )

						 ((and (pair? x) (= 1 N) 
								 (or car-is-n car-is-l))
						  (abort "Bad attempt dereferencing a matrix row: use either no indices or two"))

						 ((and (pair? x) (= 2 N) 
								 (or car-is-n car-is-l)
								 (or cadr-is-n cadr-is-l))
						  ;; dereference a value in the matrix returning either a submatrix or an element

						  (if (and car-is-n cadr-is-n)
								(list-ref* M (map 1- x))
								(cond 
								 (car-is-n
								  (make-matrix (list-ref-cols (list (list-ref M (1- (car x)))) (map 1- (cadr x)))))
								 (cadr-is-n
								  (make-matrix (list-ref-rows (list-nth* M (1- (cadr x))) (map 1- (car x)))))
								 (else
								  (make-matrix (list-ref-cols (list-ref-rows M (map 1- (car x))) (map 1- (cadr x)))))
								 )
								)
						  )

						 ((and (member (car x) '(r row)) (= 2 N) cadr-is-n)
						  ;; return the indicated row vector ** adjust for the 0-1 indexing difference **
						  (make-matrix (list (list-ref M (1- (cadr x)))))
						  )
						 ((and (member (car x) '(c col column)) (= 2 N) cadr-is-n)
						  ;; return the indicated column vector ** adjust for the 0-1 indexing difference **
						  (make-matrix (list (list-nth* M (1- (cadr x)))))
						  )

						 ((and (member (car x) '(det determinant)))
						  (determinant matrix)
						  )
						 ((member (car x) '(conj conjugate ^-))
						  ;; return the conjugate of the matrix
						  (frisk-values (make-matrix (matrix-element-map complex-conjugate M)))
						  )

						 ((member (car x) '(conj! conjugate! ^-!))
						  ;; return the conjugate of the matrix
						  (set! M ((frisk-values (make-matrix (matrix-element-map complex-conjugate M)))))
						  )

						 ((member (car x) '(T t tr transpose))
						  ;; return the transpose of the matrix
						  (make-matrix (pivot M))
						  )

						 ;; IN SITU transpose
						 ((member (car x) '(T! t! tr! transpose!))
						  ;; return the transpose of the matrix
						  (set! M (pivot M))
						  )
						 
						 ((and (member (car x) '(comp-matrix)) (= 3 N) 
								 (or 
								  (and cadr-is-l caddr-is-l)
								  (and cadr-is-n caddr-is-n))
								 )
						  ;; return the complementary matrix for the indicated rows and columns or the ijth element
						  (make-matrix (complementary-list-matrix M (cadr x) (caddr x)))
						  )
						 ((and (member (car x) '(minor)) (= 3 N) cadr-is-l caddr-is-l)
						  ;; return the minor for the indicated ij
						  (determinant (matrix 'comp-matrix (cadr x) (caddr x)))
						  )
						 ((and (member (car x) '(cof cofactor)) (= 1 N)) ;; OVERLOADED!
						  ;; return the cofactor matrix for the indicated ij (replace each element by its cofactor)
						  (frisk-values (matrix-map cofactor matrix))
						  )
						 ((and (member (car x) '(cof cofactor signed-minor))  ;; OVERLOADED!
								 (= 3 N) 
								 (or cadr-is-n cadr-is-l)
								 (or caddr-is-n caddr-is-l))
						  ;; return the cofactor (signed minor) for the indicated ij
						  (* (pow -1 (+ (cadr x) (caddr x))) 
							  (determinant (matrix 'comp-matrix (cadr x) (caddr x))))
						  )

						 ((member (car x) '(adj adjugate adjoint))
						  ;; return the adjugate
						  ((matrix 'cofactor) 'T)
						  )
						 ((member (car x) '(* H *T T* *t t*))
						  ;; return the conjugate transpose of the matrix (complex)
						  ((matrix 'conj) 'T)
						  )
						 ((member (car x) '(inv inverse))
						  ;; return the inverse
						  (frisk-values (@ * (/ 1.0 (matrix 'det)) (matrix 'cof))))
						 
						 ;; scalar multiplication of the matrix
						 ((and (= 2 N) (member (car x) '(X mult)) cadr-is-n)
						  (matrix-element-map (lambda (e) (* e (cadr x))) matrix))

						 ;;  *** Elementary row operations on a matrix
						 ;; row multiplication by a constant
						 ;; 
						 
						 ((and (eq? (car x) 'row!*) (= 3 N) cadr-is-n caddr-is-n)
						  (let ((M2 (deep-copy M))
								  )
							 (for-each
							  (lambda (i) (let ((v (list-ref (list-ref M2 (1- (cadr x))) i)))
												 (list-set! (list-ref M2 (1- (cadr x))) i (* (caddr x) v))))
							  (seq columns))
							 (make-matrix M2)))

						 ;; row division by a constant
						 ((and (eq? (car x) 'row!/) (= 3 N) cadr-is-n caddr-is-n)
						  (let ((M2 (deep-copy M))
								  )
							 (for-each
							  (lambda (i) (let ((v (list-ref (list-ref M2 (1- (cadr x))) i)))
												 (list-set! (list-ref M2 (1- (cadr x))) i (/ (caddr x) v))))
							  (seq columns))
							 (make-matrix M2)))

						 ;; row addition -- first ordinate is the row that gets modified
						 ((and (eq? (car x) 'row!+) (= 3 N) cadr-is-n caddr-is-n)
						  (let ((M2 (deep-copy M))
								  )
							 (for-each
							  (lambda (i) (let ((v (list-ref (list-ref M2 (1- (cadr x))) i))
													  (u (list-ref (list-ref M2 (1- (caddr x))) i)))
												 (list-set! (list-ref M2 (1- (cadr x))) i (+ v u))))
							  (seq columns))
							 (make-matrix M2)))

						 ;; row subtraction -- first ordinate (v) is the row that gets set to v - u, where u is the second row
						 ((and (eq? (car x) 'row!-) (= 3 N) cadr-is-n caddr-is-n)
						  (let ((M2 (deep-copy M))
								  )
							 (for-each
							  (lambda (i) (let ((v (list-ref (list-ref M2 (1- (cadr x))) i))
													  (u (list-ref (list-ref M2 (1- (caddr x))) i)))
												 (list-set! (list-ref M2 (1- (cadr x))) i (- v u))))
							  (seq columns))
							 (make-matrix M2)))


						 ;; row reduction --  v <= v - u/k 
						 ;;        where v = (cadr x), u = (caddr x) and k = (cadddr x)
						 ((and (eq? (car x) 'row!-/) (= 4 N) cadr-is-n caddr-is-n (number? (cadddr x)))
						  (let ((M2 (deep-copy M))
								  )
							 (for-each
							  (lambda (i) (let ((v (list-ref (list-ref M2 (1- (cadr x))) i))
													  (u (list-ref (list-ref M2 (1- (caddr x))) i)))
												 (list-set! (list-ref M2 (1- (cadr x))) i (- v (/ u (cadddr x))))))
							  (seq columns))
							 (make-matrix M2)))

						 ;; ASSIGN A VALUE TO AN ELEMENT

						 ((and (eq? (car x) 'set) (= 4 N) cadr-is-n caddr-is-n (number? (cadddr x)))
						  (let ((M2 (deep-copy M))
								  )
							 (make-matrix (list-set! (list-ref M (cadr x)) (caddr x) (cadddr x)))))

						 (else (abort (string-append "bad matrix operation: " (object->string x))))
						 )
						)
					 ) ;; matrix body
				  ))

		;;(dnl "Making a matrix:" args)
		(cond
		 ((null? args)
		  ;;(dnl "the void")
		  ;;(dnl 'null)
		  (set! M '())
		  (set! rows 0)
		  (set! columns 0)
		  matrix
		  )
		 ((and (pair? args) (pair? (car args)) (pair? (caar args)) 
				 ;;(number? (caaar args)) ;; Allow symbolic entries for the time being
				 ) ;; Need more tests here
		  ;;(dnl "by list representation")
		  (set! M (deep-copy (car args)))
		  (set! rows (length (car args)))
		  (set! columns (length (caar args)))
		  matrix
		  )
		 ((and (pair? args) (number? (car args)) (number? (cadr args)) (pair? (cddr args)))
		  ;; (make-matrix 3 4 '( 1 2 3 4 10 11 12 15 34 33 32 21)) => ((1 2 3 4) (10 11 12 15) (34 33 32 31))
		  ;;(dnl "with lists and numbers")
		  (let ((r (cadr args))
				  (c (car args))
				  (x (caddr args))
				  )
			 (set! rows c)
			 (set! columns r)
			 (set! M (deep-copy (let loop ((newm '()) (m x))
										 (if (null? m)
											  (reverse newm)
											  (loop (cons (list-head m c) newm) (list-tail m c))))))	
			 )
		  )
		 (else (abort 'make-matrix:aborted))
		 )
		matrix
		)
	 )
  )
