;;     This should be loaded by running 
;;
;;       (load \"maths\")

(define package-license
  "
    Copyright 2017 Randall Gray (randall@inner-product.space)


    A version of this file released under AGLPv3 is included in the
    Remodel software.  If you want to use this code, run

         git clone https://github.com/snarkypenguin/Remodel.git


    This corpus of code is currently under modification, I am 
    implementing a more comprehensive set of matrix operations 
	 to support the integration of functions over generic domains.  
    Search for HERE to find where work needs to be done.
")

;-- load dependencies

;(load "maths")

;(if #f
;	 (begin
;		(dnl  "SCHEME_LIBRARY_PATH" ":" (getenv "SCHEME_LIBRARY_PATH"))
;		(dnl  "CHIBI_IMPLEMENTATION_PATH" ":" (getenv "CHIBI_IMPLEMENTATION_PATH"))
;		(dnl  "GUILE_IMPLEMENTATION_PATH" ":" (getenv "GUILE_IMPLEMENTATION_PATH"))
;		(dnl  "GAMBIT_IMPLEMENTATION_PATH" ":" (getenv "GAMBIT_IMPLEMENTATION_PATH"))
;		(dnl  "SLIB_IMPLEMENTATION_PATH" ":" (getenv "SLIB_IMPLEMENTATION_PATH"))
;))
;(setenv "SCHEME_LIBRARY_PATH" "/usr/share/slib/")
;(setenv "SLIB_IMPLEMENTATION_PATH" "/usr/share/slib/")
;(load "/usr/share/.../slib.scm"); edit as appropriate
;(cond
; (gambit
;  (load "/usr/share/slib/gambit.init"))
; (guile
;  (use-modules (ice-9 slib)))
; ;;(chibi ;; doesn't load this as part of its normal initialisation
; ;; (load "/usr/lib/slib/chibi.init"))
; (else #f))

"
;(require 'rev3-procedures)
(require 'rev2-procedures)
(require 'pretty-print)
(require 'common-list-functions)
(require 'stdio)
(require 'sort)
(require 'string-search)

;(require 'repl)
;(require 'macro)
;(require 'macros-that-work)
;(repl:top-level macro:eval)
;(require 'strcase)
;(require 'str)
;(require 'with-file)
;(require 'line-i/o)
;(require 'random)

"

;-- Code  for building the basic operators 

;; "
;; Remember that the implementation operators can be recovered by calling 

;;     (default-implementation-arithmetic-operator '+)

;; if the file 'constants.scm' is loaded before any changes occur to the definitions
;; or by calling 

;;     (reset-primitive-arithmetic-op +

;; after 'maths.scm' if no changes have occured before it has been loaded.  The function

;;     (reset-all-primitive-arithmetic-ops) 

;; is the same as calling the reset-primitive-arithmetic-op function with +, -, * and / 
;; in turn.
;; "
;-- functions which are useful here, but not closely tied to this code

;--- (dnl . args) .... basic output routine with implicit newline
(define (dnl . args) (if (null? args) (display "") (let () (map display args) (newline))))

;--- (dnl* . args) .... basic output routine with implicit newline and spaces inserted
(define (dnl* . args)
  (cond
	((null? args) (newline))
	((null? (cdr args))
	 (display (car args))
	 (newline))
	(#t (display (car args))
		 (display " ")
		 (apply dnl* (cdr args)))))

(define (dnl% . args)
  (for-each dnl args))

(dnl%
 "Still to do:"
 "  matrix exponentials (m ^ A), which implies diagonalising code"
 )



(define (^ base exponent)
  (if (not (number? exponent))
		(error "Only numeric values are supported for exponents at the moment" ^ base exponent))
		
  (cond
	((and (number? base)(integer? exponent)) (power base exponent))
	((number? base) (exp (* exponent (log base))))
	((and (matrix? base) (integer? exponent))
	 (if (> exponent 1)
		  (* base (^ base (1- exponent)))
		  base))
	((and (matrix? exponent) (integer? base))
	 (error "Not yet implemented"))

	(else (error "bad arguments to exponentiation"))))



;---- (matrix-element-map1 L M) maps a function, L, across the individual elements of M
;; L must be a function of the form (lambda (e) ...)
(define (matrix-element-map1 L M)
  (cond
	((not (and (or (eq? L #f) (procedure? L)) (matrix? M))) (error "matrix-element-map1 requirea a function and one matrix"))
	((equal? L #f) M)
	((not (procedure? L)) (abort "Missing operator in matrix-element-map1..."))
	(#t (make-matrix (map (lambda (x) (map L x)) (M))))))

;---- (matrix-element-map2 L M1 M2) constructs a matrix by mapping an operator, L, across all elements of M1 and M2
;; (matrix-element-map2 + M1 M2) is the same as (+ M1 M2)
(define (matrix-element-map2 L M1 M2)
  (cond
	((not (and (or (eq? L #f) (procedure? L)) (matrix? M1) (matrix? M2))) (error "matrix-element-map2 requires a function and one or two matrices"))
	((equal? L #f) M1)
	((matrix? M2) (make-matrix (map (lambda (x y) (map L x y)) (M1) (M2)))
	(else (L (M1)))
	)))

;---- (matrix-element-mapN L . M*) maps an operator, L, across all elements of members of M*
;; (matrix-element-mapN + M1 ... ) generalises (matrix-element-map2 + M1 M2) 
(define (matrix-element-mapN L #!rest M*)
  (cond
	((not (and (or (eq? L #f) (procedure? L)) (matrix? M1) (matrix? M2))) (error "matrix-element-mapN requires a function and one or more matrices"))
	((= (length M*) 1) 
	 (matrix-element-map1 L (car M*)))
	((= (length M*) 2)
	 (matrix-element-map2 L (car M*) (cadr M*)))
	(#t (matrix-element-map2 L (car M*) (matrix-element-mapN L (cdr M*))))))

;---- (matrix-element-map L . matrix list)  applies an operation across a list of matrices
(define matrix-element-map matrix-element-mapN)


;; ;---- (@ op . args) wraps arithmetic operators so we may apply them to matrices
;; (define (@ op . args)
;;   ;;(dnl "Entering @")
;;   (let* ((n (length args))
;; 			(nums (filter number? args))
;; 			(mats (filter matrix? args))
;; 			(nn (length nums))
;; 			(nm (length mats))
;; ;(cdump (lambda (x) (begin (if (matrix? x) (x 'dump) (display x)) (newline))))
;; 			(N (if (null? nums) 1 (apply op nums)))
;; 			)
;; ;(for-each cdump args)

;; 	 (if (and (zero? nn) (zero? nm))
;; 		  (error "No arithmetic arguments in (@ op ...)"))

;; 	 (if (zero? nm)
;; 		  N
;; 		  (frisk-values
;; 			(matrix-element-map1
;; 			 (if (equal? op *) (lambda (x) (op N x)) #f)
;; 			 (cond
;; 			  [(equal? op +) (apply m+ mats)]
;; 			  [(equal? op -) (apply m-  mats)]
;; 			  [(equal? op *) 
;; 				(cond
;; 				 ((= nm 1) 
;; 				  (make-matrix ((car mats))))
;; 				 ((= nm 2) 
;; 				  (apply m* mats))
;; 				 ((> nm 2)
;; 				  (m* (car mats)
;; 						(apply * (cdr mats))))
				 
;; 			  )]
;; 			)
;; 		  )
;; 	  )
;; 	 )
;;   ))



;---- multiply two matrices (m2* A B)
(define m2* (lambda (A B)
				  (let ((a (A)) (b ((B ^T))))
					 (make-matrix
					  (map (lambda (r) 
								(map (lambda (c) (apply + (map * r c))) b))
							 a)))))



;---- (m* A B ...) This routine implements the matrix multiplication, prefix notation
(define (m* #!rest m)
  (letrec ((probably-ok  (and (pair? m) (matrix? (car m))))
			  (m2* (lambda (A B)
						(let ((a (A)) (b ((B ^T))))
						  (make-matrix
							(map (lambda (r) 
									 (map (lambda (c) (apply + (map * r c))) b))
								  a)))))
			  )
	 (if probably-ok
		  (let ((n (length m)))
			 (case n
				((1) (make-matrix ((car m))))
				((2) (let* ((a (car m))
								(b (cadr m))
								)
						 (m2* a b))
				 )
				(else
				 (m* (car m) (apply m* (cdr m))))
				))
		  (error "m* needs at least one matrix as an argument" m* m)
		  )))

;---- (m* A B ...) This routine implements the matrix addition, prefix notation
(define (m+ #!rest m)
  (let ((probably-ok  (and (pair? m) (matrix? (car m)))))
	 (if probably-ok
		  (let ((n (length m)))
			 (case n
				((1) (make-matrix ((car m))))
				((2) (let* ((a ((car m)))
								(b ((cadr m)))
								(S (map (lambda (c d) (map + c d)) a b)))
						 (make-matrix S))
				 )
				(else
				 (m+ (car m) (apply m+ (cdr m))))
				
				))
		  (error "m+ needs at least one matrix as an argument" m+ m)
		  )))


(define *
  (let ((*! *));; *! is the previous operator
	 (lambda args
		(let* ((n (length args))
				 (nums (filter number? args))
				 (mats (filter matrix? args))
				 (nn (length nums))
				 (nm (length mats))
				 (N (if (null? nums) 1 (apply *! nums)))
				 )

		  (cond
			[(and (zero? nn) (zero? nm))
			 (error "No arithmetic arguments in (* ...)" * args)]

			[(zero? nm)
			 N]
			[else
			 ;; return the operator applied to the numeric arguments
			 (frisk-values
			  (matrix-element-map1
				(lambda (x) (* N x)) ;; this handles any scalar multiplication in the list of arguments
				(cond
				 ((= nm 1)
				  
				  (make-matrix ((car mats))))
				 ((= nm 2) 
				  (apply m2* mats))
				 ((> nm 2)
				  (m2* (car mats)
						 (apply m* (cdr mats))))
				 )
				)
			  )]
			)))))


(define /
  (let ((/! /));; /! is the previous operator
	 (lambda args
		(if (not (= (length args) 2)) (error "Division requires two operands" / args))
		(let* ((n (length args))
				 (nums (filter number? args))
				 (mats (filter matrix? args))
				 (nn (length nums))
				 (nm (length mats))
				 (N (if (null? nums) 1 (apply /! nums)))
				 )

		  (cond
			[(and (zero? nn) (zero? nm))
			 (error "No arithmetic arguments in (/ ...)" / args)]

			[(zero? nm)
			 N];; return the operator applied to the numeric arguments

			
			[else
			 (frisk-values
			  (matrix-element-map1
				(if (number? (car args))
					 (lambda (x) (/! N x))
					 (lambda (x) (/! x N)))
				(cond
				 ((= nm 1) 
				  (make-matrix ((car mats))))
				 ((= nm 2)
				  (m2* (car mats) (inverse (cadr mats))))
				 ((> nm 2)
				  (m* (car mats (inverse (apply * (cdr mats))))) ;; in keeping with the usual scheme defn for numbers
				  )
				 )
				)
			  )]
			)))))


(define +
  (let ((+! +))
	 (lambda args
		(let* ((n (length args))
				 (nums (filter number? args))
				 (mats (filter matrix? args))
				 (nn (length nums))
				 (nm (length mats))
				 (N (if (null? nums) 0 (apply +! nums)))
				 )

		  (cond
			[(and (zero? nn) (zero? nm))
			 (error "No arithmetic arguments in (+ ...)" + args)]

			[(zero? nm)
			 N]
			[(zero? nn) ;; there must be no scalars in the addition
			 ;; return the operator applied to the numeric arguments
			 (frisk-values
			  (apply m+ mats)
			  )]
			[else (error "There seem to have been scalars in the addition, which makes no sense.")]
			)))))

(define -
  (let ((-! -))
	 (lambda args
		(let* ((n (length args))
				 (nums (filter number? args))
				 (mats (filter matrix? args))
				 (nn (length nums))
				 (nm (length mats))
				 (N (if (null? nums) 0 (apply -! nums)))
				 )

		  (cond
			[(and (zero? nn) (zero? nm))
			 (error "No arithmetic arguments in (- ...)" - args)]

			[(zero? nm)
			 N]
			[(zero? nn) ;; there must be no scalars in the addition
			 ;; return the operator applied to the numeric arguments
			 (frisk-values
			  (apply m+ (cons (car mats) (m* -1 (apply m+ (cdr mats)))))
			  )]
			[else (error "There seem to have been scalars in the addition, which makes no sense."  func args)]
			)))))


;; (define (@+ . args)
;;   (apply + args))
;; (define (@- . args)
;;   (apply - args))
;; (define (@* . args)
;;   (apply * args))
;; (define (@/ . args)
;;   (apply * args))

;; ;(define (@/ . args)
;; ;  (if (and (= (length args) 2)
;; ;			  (= ((car args) 'nr) ((car args) 'nc)) ;; first is square
;; ;			  (= ((cadr args) 'nr) ((cadr args) 'nc)) ;; second is square
;; ;			  (= ((car args) 'nr) ((cadr 'args) 'nr))) ;; they are the same size
;; ;		(@* (list (car args) ((cadr args) ^I)))
;; ;		(error "@/ only works on square matrices of the same size, where the denominator is invertable")
;; ;		)
;; ;  )

;; ;(define * @*)
;; ;(define / @/)
;; ;(define + @+)
;; ;(define - @-)


;; ;; Redefine the symbols used for ring operations

;; ; The following extend matrices so that they conform to the expectation of the operator
;; ; by either embedding them in an appropriate zero or identity matrix (for additive or
;; ; multiplicative operators, respectively).
;; (define (@@+ . args)
;;   (apply matrix-operator (cons + args)))
;; (define (@@- . args)
;;   (apply matrix-operator (cons - args)))
;; (define (@@* . args)
;;   (apply matrix-operator (cons * args)))

;; (define (@@/ . args)
;;   (if (and (= (length args) 2)
;; 			  (= ((car args) 'nr) ((car args) 'nc))
;; 			  (= ((cadr args) 'nr) ((cadr args) 'nc))
;; 			  (= ((car args) 'nr) ((cadr 'args) 'nc))
;; 			  (@@* (car args) ((cadr args) ^I))
;; 			  )
;; 		(error "Oops @@/")))

;; (define (substitute-operations lst)
;;   (map (lambda (v)
;; 			(cond
;; 			 ((list? v) (map substitute-operations v))
;; 			 ((number? v) v)
;; 			 ((matrix? v) v)
;; 			 ((null? v) v)
;; 			 ((eq? v *) @*)
;; 			 ((eq? v +) @+)
;; 			 ((eq? v -) @-)
;; 			 ((eq? v /) @/)
;; 			 (#t v)))
;; 		 lst))


;; The exponentiation operator is defined below as "(define (^ o i) ...)"

(define (assert tag . conditions)
  (if (not (m-andf conditions))
		(error "Assertion failed" tag)))

(define allow-shared-augmentations #f) ;; Setting this to #t will break some things, debugging still to do.
(define rigid-embedding #t) ;; #t if a matrix to be embedded exceeds the range of the host matrix should fail

(define dependencies
  "
This may be dependent on SLIB by Aubrey Jaffer for some of the
fancier output formatting. It does need an external sort and it may
not work well with scheme implementations other than Gambit-C, and
there may be other dependencies I haven't discovered yet.
")


;-- Notes and documentation

;--- notes
(define development-notes
  "
There really ought to be array equivalents to these; the implementation should 
probably shift to being array based as well.

In the code I occasionally use brackets rather than parentheses to
make it easy to deal with unbalanced s-expressions---gambit supports
'(...) '[...] and '{.} as valid lists, guile only the first two and
chibi only the first.  I wouldn't recommend an unattended global
search and replace, but it wouldn't be difficult to fix things should your 
version of scheme not support the alternatives.
")


;--- documentation
(define matrix-doco ;; THIS NEEDS TO BE UPDATED
  "

This gobbet of code provides basic support for using matrices in other code.
They can be used in a conventional infix style using the 'infix' command
like so:

      (infix 2 * A - B * (C ^T))

NOTE infix DOES NOT HANDLE SUBEXPRESSIONS! [NYI]

as opposed to a more schemish 

      (* 2 A (* -1 B (C ^T))).
 


These matrices are indexed from 1.  At the moment there is no way to augment
a matrix or to overlay a small matrix onto a larger on (say for building a 
composite matrix like
                        [A  B]
                        [C  D]
other than to extract the lists and manipulate them directly.

(make-matrix 3 4 '( 1 2 3 4 10 11 12 15 34 33 32 21))
   => ((1 2 3 4) (10 11 12 15) (34 33 32 31))

(make-matrix ((1 2 3) (4 5 6)) 5 5)
   => ((1 2 3 0 0) (4 5 6 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))


(M) returns the list representation of the matrix

(M i j)  -- i and j can either be indices, or lists of indices
	is the dereferencing operation

(M 'det) == (M 'determinant) == (determinant M)

(M 'minor i j) returns the i,j minor of the matrix M

(M 'cof) (M 'cofactor) == (cofactor M) returns the cofactor matrix

(M 'cof i j) (M 'cofactor i j) == (cofactor M i j) returns the cofactor 
   (the signed minor) of matrix element i,j

(M ^t) == (M ^T)
	returns the transpose of M

(M ^t!), (M ^T!)
	transposes M

(M 'nr) 
	the number of rows in M

(M 'nc) 
	the number of columns in M

(M 'r i) == (M 'row i) returns the row matrix associated with
	row i

(M 'c i) == (M 'col i) (M 'column i) returns the column matrix 
	associated with column i

(M 'comp-mat i j) 
	is the complementary matrix of ij (where the same rules apply to 
	ij as for dereferencing)											  

(M 'conj), (M 'conjugate), (M ^-) and (M ^*) 
	return the complex conjugate of M

(M 'conj!), (M 'conjugate!), (M ^-!) and (M ^*!)
	set M to its complex conjugate

(matrix-element-map f mat1 ...)
	applies the function f element-wise to the indicated matrices
	and produces a matrix.  The matricies must all have the same
	number of rows and columns

(M 'minor i j)
	returns the minor associated with ij

(M 'adj), (M 'adjoint) and (M 'adjugate) 
   return the transpose of the cofactor matrix of M

(M 'inv) == (M 'inverse) == (M ^i) == (M ^I)
	returns the inverse of M

Elementary operations are row:* row:/ row:+ row:- row:/-
and col:* col:/ col:+ col:- col:/-

(M 'set-ij i j v)
	sets element ij to v (*no* lists in ij)

(M 'set-ij! i j v)
	sets element ij to v (*no* lists in ij) in situ

Elementary operations are row:* row:/ row:+ row:- row:/-
and col:* col:/ col:+ col:- col:/-

(M 'set-row! i v)
	sets elements in row i to v (*no* lists in ij)

(M 'exact!)   coerces all elements to an exact (rational) value
(M 'inexact!) coerces all elements to an exact (floating point) 
              value
(M 'exact) and (M 'inexact) are analogous, but return a new matrix.


Augmented matrices are supported, and can be added to a matrix
like so:

   (M // A)
or
   (M //! A)

augments the matrix M with the matrix A, both must have the same
number of rows, A is notionally along the side of M.  Note that the
notation using the single letter 'a' requires a single quote to pass
the symbol.

   (M __ B)
or
   (M __! B)

augments the matrix A with the matrix B, both must have the same
number of columns, B is notionally 'below' M.  In the last example, __
is a double underline. Note that the notation using the single letter
'b' requires a single quote to pass the symbol.

The versions without an exclamation point install copies of the
indicated augmented matrix, whereas the the versions with an
exclamation point install the actual matrix.

The augmented matrices can be inspected after manipulation using

		(M //) or (M //!)

and 

		(M __) or (M __!)

where again the final example is a doubled underline

(M 'aug?) or (M 'augmented?) returns a two element list
with boolean values indicating whether there are horizontal
or vertical augmentations of the matrix.


(* 4/7 A ...) 
	demonstrates multiplication in the ring of matrices over numbers

(+ A ...)
	is matrix addition

(- A ...)
	is matrix subtraction

(/ A B) is a short-had for (* A (B ^i)), but the matrices must conform
and B must be invertible.  Similarly, (/ A 3) divides all the entries of A by 
three, and (/ 3 A) is equivalent to (* 1/3 (A ^i)).

There are a number of other functions which can be applied to matrices:

(sort-matrix< M)      reorders the rows (in any side-augmentations as well)
                      so that leading non-zero values are increasing

(sort-matrix> M)      like sort-matrix< except that the values decrease

(append-augmented-matrix-rows M)   
                      tacks the side augmentations onto the rows of the 
                      matrix and fills out the columns of the augmentation 
                      below with zeros to match. This is a convenience 
                      function for doing Gaussian reduction.

(return-augmented-matrix-rows n M)
                      reconstructs a matrix and its augmentations from a matrix
                      constructed with append-augmented-matrix-rows.
"
  )


;-- Define symbols for shorthand operations
;; THESE ARE THE MAIN WAY OF DOING THINGS!

"
  (M //!)     returns its row augmentation (beside)
  (M //)      returns a copy of its row augmentation

  (M __!)     returns its column  augmentation (below)
  (M __)      returns a copy of its column augmentation
"


(define // '//)		;; returns a copy of its row augmentation
(define //! '//!)		;; returns its row augmentation

(define __ '__)		;; returns a copy of its column augmentation
(define __! '__!)		;; returns its column augmentation



;; The following are defined (essentially as keywords for convenience

;; Superscript notation for matrices, eg (A ^t) for the transpose of A,
;; (A ^i) yields its inverse, or (A ^~) for the adjoint (complex conjugate
;; transpose) of A

;; inverse  (* (M ^i) M)) is the identity
(define ^inv '^inv)
(define ^i '^i)
(define ^I '^I)

;; transpose
(define ^T '^T)
(define ^t '^t)

;; transpose in situ (modifies matrix)
(define ^T! '^T!)
(define ^t! '^t!)

;; conjugate
(define ^- '^-)
(define ^* '^*)

;; adjoint or conjugate transpose ... there are a lot of conventions
(define ^~ '^~)
(define ^H '^H)
(define ^*t '^*t)
(define ^t* '^t*)
(define ^*T '^*T)
(define ^T* '^T*)

(define (posint? n) (and (integer? n) (positive? n)))
(define (indices? ob) (list? ob) (apply m-andf (map posint? ob)))

(define (column-matrix? M) (and (matrix? M) (= (M 'nc) 1)))
(define (row-matrix? M) (and (matrix? M) (= (M 'nr) 1)))

(define (seq:n-m n m)
  (map (lambda (x) (+ x n)) (seq (- (+ m 1) n))))

(define (!filter L lst)
  (filter (lambda (ob) (not (L ob))) lst))

(define (denull l)
  (cond
	((null? l) '())
	((not (pair? l)) l)
	(else
	 (let ((a (denull (car l)))
			 (d (denull (cdr l))))
		(cond
		 ((and (null? a) (null? d)) '())
		 ((null? a) d)
		 (else (cons a d)))))))

(define (equal*? . lst)
  (if (< (length lst) 2)
		(error equal*? "Wrong number of arguments passed to equal*?" lst)
		(apply m-andf (map (lambda (x) (equal? (car lst) x)) (cdr lst)))))

(define (matrix= M W) ;; checked
  (or (equal? M W)
		(and (matrix? M) (matrix? W) (equal? (M) (W)))))

(define (matrix=! M W) ;; checked
  (not (or (equal? M W)
			  (and (matrix? M) (matrix? W) (eq? (M) (W))))))

(define (matrix//= M W) ;; includes equality of augmented matrix to the side
  (or (equal? M W)
		(equal? (M //) (W //))
		(matrix= (M //) (W //))))

(define (matrix__= M W) ;; includes equality of augmented matrix below
  (or (equal? M W)
		(equal? (M __) (W __))
		(matrix= (M __) (W __))))

(define (matrix//!= M W) ;; includes equality of augmented matrix to the side (more stringent)
  (and (equal? M W)
		 (equal? (M //!) (W //!))))


(define (matrix__!= M W) ;; includes equality of augmented matrix below (more stringent)
  (and (equal? M W)
		 (equal? (M __!) (W __!))))

(define (matrix*= M W)
  (and (matrix= M W)
		 (matrix//= M W)
		 (matrix__= M W)))

(define (matrix*=! M W)
  (and (matrix=! M W)
		 (matrix//=! M W)
		 (matrix__=! M W)))


(define (first-match match? lst) ;; returns the first matching element or #!void
  (cond
	((not (pair? lst)) #!void)
	((match? (car lst)) (match? (cdr lst)))
	(#t (car lst))))

(define (first-non-match match? lst) ;; returns the first non-matching element or #!void
  (cond
	((not (pair? lst)) #!void)
	((not (match? (car lst))) (match? (cdr lst)))
	(#t (car lst))))

(define (first-match-ix match? lst #!optional (ix 0)) ;; returns the index of the first matching element
  (cond
	((not (pair? lst)) #!void)
	((match? (car lst)) (first-match-ix (cdr lst) match? (+ 1 ix)))
	(#t ix)
	))

(define (first-non-match-ix match? lst #!optional (ix 0)) ;; returns the index of the first non-matching element
  (cond
	((not (pair? lst)) #!void)
	((not (match? (car lst))) (first-match-ix (cdr lst) match? (+ 1 ix)))
	(#t ix)
	))


(define (eigenvectors+values M)
  "Recall, an eigenvalue lambda and its associated eigenvector v_lambda fulfill the 
  equation A v_lambda = lambda v_lambda, where lambda may be an eigenvalue for more
  than one eigenvector.  When solving for eigenvalues/vectors we need to keep this 
  in mind.
  "
  (error "not implemented yet" eigenvalues/vectors M))
   
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


(define elision '[
						;; These routines are located in utils.scm

						;; guarded
						(define list-ref
						  (letrec ((%list-ref list-ref))
							 (lambda (l i)
								(if (or (< i 0) (>= i (length l)))
									 (abort 'list-ref-index-out-of-bounds)
									 (%list-ref l i)))))


						;; the index can be a list of indices
						;; list-ref that accepts a list of indices
						(define list-ref
						  (let ((olr list-ref))
							 (lambda (lst ix)
								(if (number? ix) (olr lst ix) (map (lambda (y) (olr lst y)) ix)))))


						;; the index can be a list of indices (and if it is the value must be a corresponding list)
						(define list-set!
						  (letrec ((%list-set!
										(lambda (l i v) (if (zero? i)
																  (set-car! l v)
																  (list-set! (cdr l) (1- i) v)))
										))
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

						(define (list-set*! lst ix vv)
						  (cond
							((number? ix)
							 (list-set*! lst ix vv))
							((= (length ix) 1)
							 (dnl "unit list")
							 (list-set*! lst (car ix) vv))
							(else
							 (let ((tv (list-ref* lst ix)))
								(if (atom? tv)
									 ;; indices fully resolve an element
									 (let* ((short-ix (reverse (cdr (reverse ix))))
											  (tv (list-ref* lst short-ix)))
										(list-set*! tv (car (reverse ix)) vv))
									 (if (= (length tv) (length vv))
										  ;; it's ok, do it
										  (list-set*! (map (lambda (x) (list-ref! x (car ix)) lst) (cdr ix)) vv)
										  (abort "The value list does not have the indicated number of elements"))))
							 )))
						])

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

(define (list-set*! lst ix vv)
  (cond
	((number? ix)
	 (list-set*! lst ix vv))
	((= (length ix) 1)
	 (dnl "unit list")
	 (list-set*! lst (car ix) vv))
	(else
	 (let ((tv (list-ref* lst ix)))
		(if (atom? tv)
			 ;; indices fully resolve an element
			 (let* ((short-ix (reverse (cdr (reverse ix))))
					  (tv (list-ref* lst short-ix)))
				(list-set*! tv (car (reverse ix)) vv))
			 (if (= (length tv) (length vv))
				  ;; it's ok, do it
				  (list-set*! (map (lambda (x) (list-ref! x (car ix)) lst) (cdr ix)) vv)
				  (abort "The value list does not have the indicated number of elements"))))
	 )))


(define (first-zero lst) ;; returns the index of the first non-zero element
  (first-match zero? lst))

(define (first-non-zero lst) ;; returns the index of the first non-zero element
  (first-non-match zero? lst))

(define (first-zero-ix lst) ;; returns the index of the first non-zero element
  (first-match-ix zero? lst))

(define (first-non-zero-ix lst) ;; returns the index of the first non-zero element
  (first-non-match-ix zero? lst))


(define (simple-matrix->latex M)
  ;;  \left( \begin{array}{ccc|c} 1 & 2 & 3 & 4 \\ 5 & 6 & 7 & 8 \end{array} \right)
  (let ((preamble  (string-append
						  "\\left( \\begin{array}{"
						  (make-string (M 'nc) #\l )
						  "} "))
		  (MS (apply string-append
						 (map (lambda (row)
								  (apply string-append (cons (object->string (car row))
																	  (map (lambda (ob)
																				(string-append " & "
																									(object->string ob)
																									" "))
																			 (cdr row))) "\n"))
								(M))))
		  )
	 (string-append preamble MS "\\right)")))

(define (simple-matrix->octave M)
  (let ((preamble  (string-append "["
											 ))
		  (MS (apply string-append
						 (map (lambda (row)
								  (apply string-append (cons (object->string (car row))
																	  (map (lambda (ob)
																				(string-append "; "
																									(object->string ob)
																									" "))
																			 (cdr row))) "];\n"))
								(M))))
		  )
	 (string-append preamble MS "")))


;--- (m-andf arg ...) --  functional version of and which can be used with apply
(define (m-andf . args)
  (if (null? args)
		#t
		(and (car args) (apply m-andf (cdr args)))))

;--- construct a list which when evaluated is functionally identical to f
;; This is used to recognise matrices.  I don't know how dependent this is on implementation.
(define (function->list f)
  (if #t
		(with-input-from-string
			 (with-output-to-string
				(lambda () (pp f (current-output-port)))
				)
		  (lambda () (read)))
		(let ((op (open-string)))
		  (pp func op)
		  (force-output op)
		  (read op)
		  )
		))

;--- identify the operator: NOTE overloaded arithmetic operators *must* work on exact numbers
(define (arith-extension-type op)
  (case (op 3 5)
	 ((8) 'addition)
	 ((-2) 'subtraction)
	 ((15) 'multiplication)
	 ((3/5) 'division)
	 (else 'bad-operation)))



(define (matrix-help)
  (display matrix-doco))



;-- List routines

;--- Predicates

;--- (one? x) returns true if x == 1, probably used in other code
(define (one? x) (= x 1))

;---- The following two functions are not used, but may be used in code that loads matrix.scm
(define (!span l kl)
  (if (member (car l) kl)
		(!span (cdr l) kl)
		l))

(define (span l kl)
  (let ((n (!span l kl)))
	 (list-head l (- (length l) (length n)))))


;---- (implicit-vector? x) returns true if it is a list full of numbers
(define (implicit-vector? x) (and (list? x) (apply m-andf (map number? x))));ok

;---- (matrix-list? m) checks list for consistency with a "matrix" in list form,
(define (matrix-list? m)
  (if (null? m)
		#f
		(if (or (boolean? m) (procedure? m) (not (list? m)))
			 #f
			 (and (apply m-andf (map list? m))
					(apply = (map length m))
					(apply m-andf (map (lambda (r) (apply m-andf (map number? r))) m))
					(not (member '() m))
					)
			 )));ok


;---- (matrix? m) returns true if it is a matrix -- it also maintains a "register" of matrices

(define matrix?
  (let ((register '())
		  )
	 (lambda args
		;;(dnl "MATRIX? " args)
		;;(pp register)
		(cond
		 ((procedure? (car args)) (and (member (car args) register) #t))
		 ((and (procedure? (car args)) (eq? (cadr (function->list (car args))) 'matrix-args)) #t)
		 ((null? args) (list-copy register))
		 ((eq? (car args) 'register!) (set! register (cons (cadr args) register)))
		 ((eq? (car args) 'register)
		  (if (null? (cdr args))
				(list-copy register)
				(if (null? (cddr args))
					 (and (member (cadr args) register))
					 (map (lambda (p) (matrix? 'register p)) (cdr args))))
		  )
		 ((eq? (car args) 'flush) (set! register '()))
		 ((eq? (car args) 'help)
		  (display (string-append "(matrix?) -- return defined matrices\n"
										  "(matrix? M) -- returns #t if M is a matrix, else #f\n"
										  "(matrix? 'register M) -- identical to (matrix? M)\n"
										  "(matrix? 'register '(M0 M1 ...)) -- returns a list of #t or #f values\n"
										  "(matrix? 'register) -- return defined matrices\n"
										  "(matrix? 'flush) -- purge the list of matrices\n"
										  "(matrix? 'help) -- print this message\n"))
		  )
		 (#t #f)
		 )
		)
	 ))

(define (pad-matrix-list lst len val)
  (map (lambda (r)
			(append r (make-list (- len (length r)) val)))
		 lst))

(define (append-augmented-matrix-rows M) ;; The original number of columns MUST be maintained externally!
  (if (or (M //) (M __))
		(let* ((mtmp (if (not (M //)) (M) (map append (M) ((M //)))))
				 (btmp (if (not (M __)) #f (pad-matrix-list ((M __)) (length (car mtmp)) 0)))
				 (columns (length (car (M))))
				 (rows (length (M)))
				 )
		  (if btmp
				((make-matrix mtmp) __ (make-matrix btmp))
				(make-matrix mtmp))
		  )
		M
		))

(define (return-augmented-matrix-rows n Mdash) ;; either n is the
  ;; number of columns or
  ;; it is the original
  ;; matrix, Mdash is the
  ;; extended version
  (set! n (cond
			  ((number? n) n)
			  ((matrix-list? n) (length (car n)))
			  ((matrix? n) (n 'nc))
			  (#t (error "bad first argument: should be a number, matrix or matrix-list" return-augmented-matrix-rows n Mdash))))

  (let* ((m (map (lambda (r) (list-head r n)) (Mdash)))
			(a (map (lambda (r) (list-tail r n)) (Mdash)))
			(b (if (M __) (map (lambda (r) (list-head r n)) ((Mdash __))) #f))
			(mpa (if (M //!) ((M //!) '!) #f))
			(mpb (if (M __!) ((M __!) '!) #f))
			(M (make-matrix m))
			)
	 (if (matrix-list? a) (M // (make-matrix a)) (M //! #f))
	 (if (matrix-list? b) (M __ (make-matrix b)) (M __! #f))
	 M))



;; No idea why I wrote this... ;-)
;; (define (matrix-list-ref*? lst ix)
;;   (cond
;; 	((and (number? ix) (<= 0 ix) (< ix (length lst)))
;; 	 #t)
;; 	((and (pair? lst) (number? (car ix)) (<= 0 (car ix)) (< (car ix) (length lst)))
;; 	 (matrix-list-ref*? (list-ref lst (car ix)) (cdr ix)))
;; 	((and (null? lst) (not (null? ix))) #f)
;; 	((and (null? ix) (not (null? lst))) #t)
;; 	(#t #t)))


;-- accessors

;--- lists and list-of-lists (matrix-lists or LoL)

;---- (*list-nth* '((a b c) (d e f) (g h i) (j k l)) 2) => (c f i l)
;; (!*list-nth* '((a b c) (d e f) (g h i) (j k l)) 2) => ((a b) (d e) (g h) (j k))
(define (*list-nth* lst n) (apply (lambda x (map (lambda (y) (list-ref y n)) x)) lst))


;---- (list-ref-cols L cols) return the indicated columns
(define (list-ref-cols L cols #!optional (sortit #t)) ;; the items come out in the order specified
  (let ((cols (if sortit (sort cols <) cols)))
	 (pivot (list-ref-rows (pivot L) cols #f))))

(define (list-ref-rows L rows #!optional (sortit #t)) ;; the items come out in the order specified
  (let ((rows  (if sortit (sort rows <) rows)))
	 (cond
	  ((number? rows)
		(list (list-ref L rows)))
	  ((null? rows) '())
	  (else
		(map (lambda (x) (list-ref L x)) rows)))))

;---- (!list-ref-cols L cols) -- complement of the indicated columns
(define (!list-ref-cols L cols #!optional (sortit #t)) ;; the items come out in the order specified
  (let ((cols (if sortit (sort cols <) cols)))
	 (if (number? cols) (set! cols (list cols)))
	 (pivot (!list-ref-rows (pivot L) cols #f))))


;---- (!list-ref-rows L rows) -- complement of the indicate rows
(define (!list-ref-rows L rows #!optional (sortit #t)) ;; the items come out in the order specified
  (let* ((rows  (if sortit (sort rows <) rows))
			(n (length rows)))
	 ;;(dnl "L: " L)
	 ;;(dnl "rows: " rows)
	 (cond
	  ((or (null? rows) (null? L))
		L)
	  ;;((and (= n 1)  (zero? (car rows))) '())
	  (else 
		(!list-ref-rows (append (list-head L (car rows)) 
										(list-tail L (+ 1 (car rows)))
										)
							 (if (null? (cdr rows)) 
								  '() 
								  (map (lambda (x) (- x 1)) (cdr rows)))
							 )))))

;---- list comparisons: evaluate the relative order of two conforming lists, used in sorting

(define (list>=? l1 l2)
  (if (not (= (length l1) (length l2)))
		(error "lists of unequal length in comparison" list>=? l1 l2))
  (cond
	((and (null? l1) (null? l2)) #t) ;; if we said #f, then l2 would move ahead of l1 at the end
	((and (eq? (car l1) ':) (eq? (car l2) ':)) #t) ;; short circuits out when a colon is hit
	((> (car l1) (car l2)) #t)
	((< (car l1) (car l2)) #f)
	(#t (list>? (cdr l1) (cdr l2)))))

(define (list<=? l1 l2)
  (list>=? l2 l1))

(define (list<? l1 l2)
  (not (list?>=? l1 l2)))

(define (list>? l1 l2)
  (not (list<=? l1 l2)))


;---- (define (complementary-matrix-list LoL rows cols) constructs the complement of rows x cols
(define (complementary-matrix-list LoL rows cols)
  ;;(dnl "complementary-matrix-list" LoL)
  ;;(dnl "rows: " rows ", columns: " cols)
  (if (not (pair? rows)) (set! rows (list rows)))
  (if (not (pair? cols)) (set! cols (list cols)))
  (!list-ref-cols (!list-ref-rows LoL (map (lambda (x) (- x 1)) rows)) (map (lambda (x) (- x 1)) cols)))



;---- Extract submatrices 

;----- Extract rows from a matrix
(define (submatrix-rows M r R)
  (list-head (list-tail M (- r 1)) (- R 1)))
								  

;----- Extract columns from a matrix
(define (submatrix-cols M c C)
  (map (lambda (col) (list-head (list-tail col (- c 1)) (- C 2)))  M))



;----- (submatrix M ul lr) the indices are inclusive 

(define (submatrix M . args) ;;; (submatri M r R c C) r is min row R is max row, analogously for c
                                  ;;; (submatrix M ul lr) the indices are inclusive  
                                  ;;; (submatrix M lr) the indices are inclusive
;  (dnl "(submatrix " M " . " args ")")
  (cond
	((or (matrix? M) (matrix-list? M))
	 (if (matrix? M) (set! M (M)))

;	 (dnl "M is " M)
	 (let* ((N (length args))
			  (R (length M))
			  (C (length (car M)))
			  (ul (list 1 1))
			  (lr (list R C))
			  )

		(dnl* "N =" N "R =" R "C =" C)

		(cond
		 ((null? args)
		  #t)
		 ((matrix? (car args))
		  (set! ul '(1 1))
		  (set! lr (list ((car args) 'nr) ((car args) 'nc))))
		 ((and (= N 1) (indices? (car args))) ;; (1 1) (lr)
		  (set! lr (car args)))
		 ((and (= N 2) (indices? (car args)) (indices? (cadr args))) ;; (ul), (lr)
		  (set! ul (car args))
		  (set! lr (cadr args)))
		 ((and (= N 2) (posint? (car args)) (posint? (cadr args))) ;; 1,1 l,l (thats one,one lower,left
		  (set! lr (list (car args) (cadr args))))
		 ((and (= N 4) (apply m-andf (map posint? args))) ;; u,r l,l
		  (set! ul (list (car args) (cadr args)))
		  (set! lr (list (caddr args) (cadddr args))))
		 (#t (error "Bad arguments passed to (submatrix M ...)" submatrix M args)))

		(submatrix-cols (submatrix-rows M (car ul) (car lr)) (cadr ul) (cadr lr))
		))))

;; (list-head (list-tail rc5 (- 2 1)) (- 2 1)) => ((21 22 23 24 25))
;; (list-head (list-tail rc5 (- 2 1)) (- 3 1)) => ((21 22 23 24 25) (31 32 33 34 35))


;; (load "maths")(define I5 (identity-matrix 5))(define v '(3 2 1))(define V (make-matrix v))(define iv (I5 'embed V))(iv)(define vi (iv 'extract v))(define rc5 '((11 12 13 14 15)(21 22 23 24 25) (31 32 33 34 35)(41 42 43 44 45)(51 52 53 54 55)))


;; (list-ref* '((a b c d) (e f g h i j) (k l m)) '(1 0)) => e
;;; (define (list-ref* lst ix)
;;;   (cond
;;; 	((number? ix)
;;; 	 (list-ref lst ix))
;;; 	((= (length ix) 1)
;;; 	 (list-ref lst (car ix)))
;;; 	(else
;;; 	 ;;		(list-ref* (map (lambda (x) (list-ref x (car ix))) lst) (cdr ix))
;;; 	 (list-ref (map (lambda (x) (list-ref* x (cdr ix))) lst) (car ix))
;;; 	 )))

;---- (matrix-list-ref* lst ix) -- lst is a matrix-list, ix is a list of indices
(define (matrix-list-ref* lst ix)
  (cond
	((and (list? ix) (= 1 (length ix))) (list-ref lst (car ix)))
	((and (number? ix) (<= 0 ix) (< ix (length lst)))
	 (list-ref lst ix))
	((and (pair? lst) (number? (car ix)) (<= 0 (car ix)) (< (car ix) (length lst)))
	 (matrix-list-ref* (list-ref lst (car ix)) (cdr ix)))
	((and (null? lst) (not (null? ix))) (error "Index list ran off the end of the list" matrix-list-ref* lst ix))
	((and (null? ix) (not (null? lst))) lst)
	(#t (error "Should never get here, bad arguments" matrix-list-ref* lst ix))))


;---- (matrix-list-set*! lst ix vv) -- lst is a matrix-list, ix is a list of indices, vv is a new value
;; (define p '((a b c d) (e f g h i j) (k l m)))
;; (list-set*! p '(1 0) '(tst))
;; p  => ((a b c d) ((tst) f g h i j) (k l m))
(define (matrix-list-set*! lst ix vv)
  (cond
	((number? ix)
	 (list-set! lst ix vv))
	((= (length ix) 1)
	 ;;(dnl "unit list")
	 (list-set! lst (car ix) vv))
	(else
	 (let ((tv (matrix-list-ref* lst ix)))
		(if (atom? tv)
			 ;; indices fully resolve an element
			 (let* ((short-ix (reverse (cdr (reverse ix))))
					  (tv (matrix-list-ref* lst short-ix)))
				(list-set! tv (car (reverse ix)) vv))
			 (if (= (length tv) (length vv))
				  ;; it's ok, do it
				  (matrix-list-set*! (map (lambda (x) (matrix-list-ref* x (car ix)) lst) (cdr ix)) vv)
				  (abort "The value list does not have the indicated number of elements"))))
	 )))


;---- (pivot x) -- transpose a matrix-list/LoL
;; This only works with lists  ... this is the transpose of a list of lists.
(define (pivot x)
  (cond 
	((atom? x) x)
	((and (pair? x) (pair? (car x)))
	 (map (lambda (y) (*list-nth* x y)) (seq (length (car x)))))
	((and (pair? x) (number? (car x))) ;; pivot a row vector
	 (abort "pivot-failure: not a matrix list")
	 (map list x)) 
	(else (abort "pivot failure"))))


;;---- (make-lists . args) Construct a list of lists ... used to convert 
;; (make-lists 2 3 '(1 2)) --> '(((1 2) (1 2) (1 2)) ((1 2) (1 2) (1 2)))
;(define (make-lists . args)
;  (define (mlli r c . i)
;	 (if (pair? i)
;		  (make-list r (make-list c (car i)))
;		  (make-list r (make-list c 0))))
;  (define (mllm M)
;	 (deep-copy (M)))
;
;  (if (= (length args) 1)
;		(mlli (car args))
;		(apply mlli args)))

;-- constructors 

;--- (deep-copy l) copies a list (no common conses)

(define (deep-copy l)
  (cond
	((null? l) l)
	((pair? l) (cons (deep-copy (car l)) (deep-copy (cdr l))))
	(else  l)))

;--- other operations on list-of-lists or matrix-list

;---- (display-lol M) displays a list-of-lists or matrix-list
(define (display-lol M)
  (let ((w (lol-field-width M))
		  (m (length (car M))) ; length of rows
		  (n (length M)))   ; length of columns
	 (list (+w 3) m n)))


;-- These functions deal with matrices 

;; This is a matrix wrapped in a function.  if one does a (define A (make-matrix ...))
;; then A will stand for a matrix-object, and (A) will return the definative list-representation
;; of the elements of A.

;--- (matrix-copy M) copies the (recursive) matrix structure; there are no common pairs
(define (matrix-copy M)
  (let ((m #f)(a #f) (b #f))
	 (let ((R (cond
				  ((not M) #f)
				  ((matrix? M)
					(set! m (make-matrix (M)))

					(if #f
						 (begin ; Handle augmentations to the side
							(if (matrix? (M //!))
								 (set! a (matrix-copy (M //))))

							(if (or (matrix= a (M //!)) (equal? a (M //!))) ;; check that our copy of A is ok
								 (m //! a)
								 (error "failed to copy augA" matrix-copy M))
							)
						 (m // (M //)))

					(if #f
						 (begin ; Handle augmentations below
							(if (matrix? (M __!))
								 (set! b (matrix-copy (M __!))))
							
							(if (or (matrix= b (M __!)) (equal? b (M __!))) ;; check that our copy of B is ok
								 (m __! b)
								 (error "failed to copy augB" matrix-copy M))
							)
						 (m __ (M __)))
					m)
				  ((matrix-list? M)
					(set! m (make-matrix M))
					m)
				  (#t #f))))

;		(if (matrix*= M R) ;; check they are recursively identical in content.
		R
;			 (terrible-error "Unable to make the matrix"))
		)
	 ))

;-- Mathematical operations

;--- on numbers

(define (complex-conjugate c) ;; return the complex conjugate of a complex number
  (let ((rp (real-part c))
		  (cp (imag-part c)))
	 
	 (if (zero? cp)
		  rp
		  (+ rp (* 0-1i cp)))))

;--- on Matrices

;---- construct transposes
(define (transpose A) (A ^T)) ;; New matrix returned
(define (transpose! A) (A ^T!)) ;; A --> A^{T}

;---- construct inverses
(define (inverse A) (A 'inverse)) ;; new matrix, A^{-1} returned
(define (inverse! A) (A 'inverse!)) ;; A--> A^{-1}

(define (make-hypermatrix . indices) ;; we are lax w.r.t. whether indices is a list or a bunch of numbers
  (if (and (pair? indices) (pair? (car indices)) (null? (cdr indices)))
		(set! indices (car indices)))

  (let ((ix indices))
	 (letrec ((hms (lambda (ix*)
						  (if (null? (cdr ix*)) ;; final index
								(make-list (car ix*) 0)
								(make-list (car ix*) (hms (cdr ix*)))))))
		(hms ix))))

(define (make-random-hypermatrix . indices) ;; This might be faster if we didn't construct m first,
  (if (and (pair? indices)                      ;; but this way we can trigger the call to random-real simply 
			  (pair? (car indices))
			  (null? (cdr indices)))
		(set! indices (car indices)))
  (let ((m (apply make-hypermatrix indices)))   
	 (letrec ((rnd-hypermatrix
				  (lambda (m)                       
					 (cond
					  ((null? m) '())
					  ((atom? m) (random-real))
					  ((pair? m) (cons (rnd-hypermatrix (car m)) (rnd-hypermatrix (cdr m))))
					  (#t #f))))
				 )
		(rnd-hypermatrix m)))
  )

(define (hypermatrix-indices H)
  (if (or (atom? H) (null? H))
		'()
		(cons (length H) (hypermatrix-indices (car H)))))

(define (hypermatrix-ref m indices)
  (letrec ((ix* (hypermatrix-indices m))
			  (hr (lambda (w indices ix*)
					  (if (pair? indices)
							(let ((i (car indices)))
							  (if #t
									(cond
									 ((null? w) #f)
									 ((and (< (car indices) (car ix*)) (null? (cdr ix*)))
									  (list-ref w (car indices)))
									 ((and (< (car indices) (car ix*)) (pair? (cdr w)))
									  (hr (list-ref w (car indices)) (cdr indices) (cdr ix*)))
									 (#t
									  (dnl "w = " w "\nindices = " indices "\nix* = " ix* "\n")
									  (error "whut? [1]"))
									 )
									(cond
									 ((null? w) #f)
									 ((and (< (car indices) (car ix*)) (null? (cdr ix*)))
									  (list-ref w (car indices)))
									 ((and (< (car indices) (car ix*)) (pair? (cdr w)))
									  (hr (list-ref w (car indices)) (cdr indices) (cdr ix*)))
									 (#t (error "whut? [2]"))
									 ))
							  )
							w)
					  )))
	 (hr m indices ix*)))

(define (hypermatrix-set! m indices v)
  (letrec ((ix* (hypermatrix-indices m))
			  (hs (lambda (w indices ix*)
					  (if (pair? indices)
							(let ((i (car indices)))
							  (cond
								((null? w) #f)
								((and (< (car indices) (car ix*)) (null? (cdr ix*)))
								 (list-set! w (car indices) v))
								((and (< (car indices) (car ix*)) (pair? (cdr w)))
								 (hs (list-ref w (car indices)) (cdr indices) (cdr ix*)))
								(#t (error "whut? [3]"))
								))
							w)
					  )))
	 (hs m indices ix*)))

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


;; (define (matrices->element-lists m . atrices)
;;   (if (pair? atrices) (set! m (cons m atrices)))
;;   (let* ((H (length matrices))
;; 		   (I (length (car matrices)))
;; 		   (J (length (caar matrices)))
;; 			(R (hypermatrix H I J))

;; 		  )
;; 	 (let across-rows ((i 0))
;; 		(let across-cols ((j 0))
;; 		  (let across-matrices ((k 0))


;---- (negate-matrix M) This routine negates all entries in the matrix (including augmentations)

(define (negate-matrix M)
  (if (matrix? M)
		(let* ((m (make-matrix (map (lambda (r) (map (lambda (x) (- x)) r)) (M))))
				 )
		  (if (M //) (m // (M //)))
		  (if (M __) (m __ (M __))))
		(error "Bad matrix passed to negate-matrix" negate-matrix M)))

(define -m negate-matrix)

(define (m- A #!rest B)
  (error "Broken")
  (if (not (pair? B))
		(make-matrix (A))
		(m+ A (negate-matrix (let ((m (apply m+ B)))
									  m))
			 )
		))


;---- (frisk-values M)  maps elements of the matrix to exact representations (rational or integer)
(define frisk-epsilon 1e-12)

(define (frisk-values M) 
  (matrix-element-map (lambda (e)
								(let* ((re (real-part e)) (im (imag-part e))
										 (rre (round re)) (rim (round im)))
								  (if (< (abs (- rre re)) frisk-epsilon) (set! re rre))
								  (if (< (abs (- rim im)) frisk-epsilon) (set! im rim))
								  (if (= (inexact->exact re) re) (set! re (inexact->exact re)))
								  (if (= (inexact->exact im) im) (set! im (inexact->exact im)))

								  (cond
									((and (zero? re) (zero? im)) 0)
									((zero? im) re)
									((zero? re) im)
									(else (+ re (* im 0+i))))))
							 M))

;; General exponentiation (though the caret also gets used to indicate superscript symbols in ^i ^T, etc.)
(define (matrix^ M x)
  (let ((M^2 (lambda (M)
					(* M M))))
	 (cond
	  ((zero? x) (identity-matrix (M 'nr)))
	  ((= x 1) (matrix-copy M))
	  ((even? x) (let ((Mt (matrix^ M (/ x 2)))) (* Mt Mt)))
	  (#t (* M (matrix^ M (- x 1)))))))


(define (^ o i)
  (if (number? o)
		(power o i)
		(matrix^ o i))
  )


(define (tstfunction v)
  (letrec ((a (lambda (x) (if (<= x 0) 0 (+ (a (- x 1)) (* x x)))))
			  (b (lambda (x) (* x x (a x)))))
	 (b v)))



;The evaluation over the set of matrices and numbers is done by a recursive descent parser
;Each component of the parser takes the input stream and returns a list whose head is the return 
;value of the component and the as-yet-unconsumed input.  The top level merely returns either an 
;error condition, or the appropriate value.
;

; NOTE original elements can also play in division

(define (primitive-element? x)
		(if (or (number? x) (matrix? x))
			 1
			 #f))
  ;; This returns one to conform with the convention of the predicates returning the
  ;; number of list elements that are/would be consumed


(define Attribution-for-expression-parser
"The code for building and parsing expressions is a moderately
hacked copy of that found at 

  http://rosettacode.org/wiki/Arithmetic_evaluation#Scheme

in April 2020.  I have modified it to support variables
(rather than just numbers) and added operators associated with 
exponentiation and matrix notation (such as ^i, ^T, and ^*)
Special care is taken with parentheses

The code I started with was concise, readily modified, and a credit to
its anonymous author. Any abominable elements are my own.
")


"NOTES ON (let-values () ...)  and (values ...)

(let-values (((root rem) (exact-integer-sqrt 32))) 
  (dnl* 'root  root) (dnl* 'rem rem) (* root rem)) 
=> root 5
   rem 7
   35
Because exact-integer-sqrt returns two values, one assigned to root
the other to rem.
"


;; turn list of tokens into an AST 
;; -- using recursive descent parsing to obey laws of precedence

(define last #f)

(define (parse tokens)
 
  (define (parse-element tokens)
	 (if #f
		  (if (or (number? (car tokens)) (matrix? (car tokens)))
				(values (car tokens) (cdr tokens))
				(let-values (((expr rem) (parse-expr (cdr tokens)))) ;; parse-expr returns two values
				  (values expr (cdr rem))))
		  (cond
			((or (number? tokens) (matrix? tokens))
				(values tokens '()))
			((or (number? (car tokens)) (matrix? (car tokens)))
				(values (car tokens) (cdr tokens)))
			(else
			 (let-values (((expr rem) (parse-expr (cdr tokens))))
				(values expr (cdr rem))))
			)))
  
  (define (parse-factor tokens)
    (let-values (((left-expr rem) (parse-element tokens)))
		(if (and (not (null? rem))
					(member (car rem) (list ^)))
			 (let-values (((right-expr remr) (parse-factor (cdr rem))))
				(values (list (car rem) left-expr right-expr)
						  remr))
			 (values left-expr rem))))
  
  (define (parse-term tokens)
    (let-values (((left-expr rem) (parse-factor tokens)))
		(if (and (not (null? rem))
					(member (car rem) (list * /)))
			 (let-values (((right-expr remr) (parse-term (cdr rem))))
				(values (list (car rem) left-expr right-expr)
						  remr))
			 (values left-expr rem))))
  
  (define (parse-part tokens)
    (let-values (((left-expr rem) (parse-term tokens)))
		(if (and (not (null? rem))
					(member (car rem) (list + -)))
			 (let-values (((right-expr remr) (parse-part (cdr rem))))
				(values (list (car rem) left-expr right-expr)
						  remr))
			 (values left-expr rem))))
  
  (define (parse-expr tokens)
    (let-values (((expr rem) (parse-part tokens)))
		(values expr rem)))
  
  (let-values (((expr rem) (parse-expr tokens)))
	 (if (null? rem) 
		  expr
		  (error "Misformed expression"))))
 
;; evaluate the AST, returning a number
(define (eval-expression ast)
  (cond ((number? ast)
         ast)
		  ((matrix? ast)
			ast)
        ((member (car ast) (list + - * / ^))
         ((car ast) 
          (eval-expression (cadr ast)) 
          (eval-expression (caddr ast))))
        (else
          (error "Misformed expression"))))
 

;; convert a string into a list of tokens
;; (define (string->tokens str)
;;   (define Abc (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_%&$"))
;;   (define AbcN (append Abc (string->list "0123456789")))

;;   (define (specialop charlst)
;; 	 (let ((specials '(^t ^T ^* ^i ^I)))
;; 			 'not-there-yet
;; 			 (error "not implemented")
;; 			 ))
  
;;   (define (variablename charlst) ;; either returns variablename as a symbol or (#f and charlst)
;; 	 (error "not implemented")
;; 	 (if (not (member (car charlst) Abc))
;; 		  (values $f charlst)
;; 		  (let vloop ((namnett (list->string (list (car charlst))))
;; 						  (unprocessed (cdr charlst)))
;; 			 (cond
;; 			  ((null? unprocessed) (values namnett unprocessed))
;; 			  ((member (car unprocessed) AbcN)
;; 				(vloop (string-append namnett (list->string (list (car unprocessed))))
;; 						 (cdr unprocessed)))
;; 			  (else (values namnett unprocessed))
;; 			  ))))
  

;;   (define (next-token chars)
;; 	 (let-values ((cond ((member (car chars) (list #\+ #\- #\* #\/ #\^) char=?)
;; 								(values (cdr chars)
;; 										  (cdr (assq (car chars) ; convert char for op into op procedure, using a look up list
;; 														 (list (cons #\+ +) (cons #\- -) (cons #\* *) (cons #\/ /) (cons #\^ ^))))))
							  
;; 							  ((member (car chars) (list #\( #\)) char=?)
;; 								(values (cdr chars)
;; 										  (if (char=? (car chars) #\()
;; 												'open	
;; 												'close)))
;; 							  (else ; read 	a multi-digit positive integer
;; 								(let loop ((rem chars)
;; 											  (res 0))
;; 								  (if (and (not (null? rem)) 
;; 											  (char-numeric? (car rem)))
;; 										(loop (cdr rem)
;; 												(+ (* res 10)
;; 													(- (char->integer (car rem))
;; 														(char->integer #\0))))
;; 										(values rem
;; 												  res))))))))
;;   (let loop ((chars (remove char-whitespace? (string->list str)))
;; 				 (tokens '()))
;; 	 (if (null? chars)
;; 		  (reverse tokens)
;; 		  (let-values (((remaining-chars token) (next-token chars)))
;; 			 (loop remaining-chars
;; 					 (cons token tokens))))))


;; convert a string into a list of tokens
(define (string->tokens str)
  (define (next-token chars)
	 (cond ((member (car chars) (list #\+ #\- #\* #\/ #\^) char=?)
			  (values (cdr chars)
						 (cdr (assq (car chars) ; convert char for op into op procedure, using a look up list
										(list (cons #\+ +) (cons #\- -) (cons #\* *) (cons #\/ /) (cons #\^ ^)))))
			 ((member (car chars) (list #\( #\)) char=?)
			  (values (cdr chars)
						 (if (char=? (car chars) #\()
							  'open
							  'close)))
			 (else ; read a multi-digit positive integer
			  (let loop ((rem chars)
							 (res 0))
				 (if (and (not (null? rem)) 
							 (char-numeric? (car rem)))
					  (loop (cdr rem)
							  (+ (* res 10)
								  (- (char->integer (car rem))
									  (char->integer #\0))))
					  (values rem
								 res))))))
	 )

  (let loop ((chars (remove char-whitespace? (string->list str)))
				 (tokens '()))
	 (if (null? chars)
		  (reverse tokens)
		  (let-values (((remaining-chars token) (next-token chars)))
			 (loop remaining-chars
					 (cons token tokens))))))




;; parse and evaluate the given string -- this is pretty dodgey at the moment
(define (infix . str)
   (eval-expression (parse (if (string? (car str)) (string->tokens (car str)) str))))




;; parse and evaluate the given string
;(define (interpret str)
;  (eval-expression (parse (string->tokens str))))

 							


;; (define ring-eval
;;   (let ((multiplicative-ops (list * /))
;; 		  (additive-ops (list - +))
;; 		  (expr-first (list negation? number? matrix? subexp?))
;; 		  (expr-follow (list null?))
;; 		  (negation-first (list negation?))
;; 		  (subexpression-first (list list?))
		  
						  
;; 		  (mult-first number? matrix? 
;; 		  )
;;   (letrec ((re
;; 				(lambda (expr)
;; 				  (cond
;; 					((equal? (car expr) -) ;; negation has to be done first ...
;; 					 (negate (cadr expr)))
;; 					((or (number? expr) (matrix? expr)) expr) ;; followed by ring elements

;; 					((list? arg1) (ring-eval (cons (apply re arg1) (cdr expr)))) ;; then parenthesised expressions

;; 					((and (pair? (cdr expr)) (member (cadr expr) (list multiplicative-ops)))
;; 					 (multiplicative-expression (cadr op) (ring-eval (car expr)) (cddr expr)))

;; 					((and (pair? (cdr expr)) (member (cadr expr) (list additive-ops)))
;; 					 (arithmetic-expression (cadr op) (re (car expr)) (re (cddr expr))))
;; 					(#t (error "Oops."))
;; 					))
;; 				)
;; 			  (multiplicative-expression
;; 				(lambda (op a1 . rest
;; 						  (if (eq? op +)
								



;; 			  )




;; (define (/@/ op . args)
;;   (let* ((n (length args))
;; 			(nums (filter number? args))
;; 			(mats (filter matrix? args))
;; 			(nn (length nums))
;; 			)
;; 	 (cond
;; 	  ((zero? n) (op))
;; 	  ((= n nn) (apply op args)) ;; just a number ;; ok

;; 	  ;; so we are now dealing with matrices in some way
;; 	  ((zero? nn) ;; all matrices or vectors
;; 		(frisk-values (cond
;; 							((= n 1) (matrix ((car args))))
;; 							((= n 2) (apply m* args))
;; 							(else 
;; 							 (let ((m (m* (car args) (cadr args))))
;; 								(apply @ * (cons m (cddr args))))))))
;; 	  (else ;; mix of numbers vectors and matrices
;; 		(let* ((N (apply op nums))
;; 				 (M (apply @ op (filter matrix? (map (lambda (o) (if (and (list? o) (not (matrix? o)))
;; 																					  o))
;; 																 args))))
;; 				 )
;; 		  (frisk-values (matrix-element-map1 (lambda (x) (op N x)) M))))
;; 	  )))

;---- (rotation-matrix-2d theta) constructs a 2d rotation matrix
(define (rotation-matrix-2d theta)
  (make-matrix (list (list (cos theta) (- (sin theta)))
							(list (sin theta) (cos theta)))))

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
						(list-set! (list-ref m (car j)) (car i) (L M (+ 1 (car i)) (+ 1 (car j))))
						(loop-j (cdr j)))))))))


;---- (Sum m M lmbda) returns the sum of ((lambda (x)....) i) from i in [m, M]
(define (Sum m M lmbda)
  (apply + (map lmbda (map (lambda (p) (+ m p)) (seq (+ 1 (- M m)))))))

;---- (Prod m M lmbda) returns product of ((lambda (x)....) i) from i in [m, M]
(define (Prod m M lmbda)
  (apply * (map lmbda (map (lambda (p) (+ m p)) (seq (+ 1 (- M m)))))))


(define (leading-term-index-in-row r) ;; returns the value
  (let loop ((j 0)
				 )
;		(dnl "..")
	 (if (and (< j (length r)) (zero? (list-ref r j)))
		  (loop (+ j 1))
		  (+ j 1) ;; return!
		  ))) ;; this returns a matrix-appropriate index

(define leading-term_j leading-term-index-in-row)






;---- (unsorted-matrix? M) returns #t if the rows are not "numerically" decreasing

(define (unsorted-matrix? M)
  (and
	(not (equal? (M) (sort! (M) list>=?)))
	(not (equal? (M) (sort! (M) list<=?))))
  )

;; (define (reduce-row M i) ;; i is the index for things to be reduced to
;;   (let* ((m ((append-augmented-matrix-rows M)))
;; 			(k (list-ref m i))
;; 		  )
;; 	 (map (lambda (r)

;; (lambda (j) (/ j k)) m)))





;(define row-reduced-echelon-form gaussian-row-reduction)






;---- (minor M i j) returns the minor matrix 
(define (minor M i j) ;; ok
  (if (and (<= 1 i) (<= i (M 'nr))
			  (<= 1 j) (<= j (M 'nc)))
		(make-matrix (complementary-matrix-list (M)  i j))
		(error "Indices do not lie within the bounds of the matrix")))


;---- (cofactor M i j) returns the cofactor associated with M_{ij}
(define (cofactor M i j) ;; ok
  (if (= (M 'nr) 1)
		1
		(let ((m (minor M i j))
				(s (power -1 (+ i j))))
		  (* s (determinant m)))))


;---- (general-determinant A) returns the determinant of the matrix
(define (general-determinant A #!optional (checked #f))
  (if (not checked)
		(if (matrix? A)
			 (general-determinant A #t)
			 (error "general-determinant was passed a non-matrix"))
		(if (= (A 'nr) (A 'nc))
			 (if (= (length (A)) 1)
				  (A 1 1)
				  (begin
					 (Sum 1 (A 'nc)
							(lambda (i)
							  (* (if (odd? i) 1 -1) (A 1 i)
								  (determinant (make-matrix (minor A  1  i)) checked)))
							)
					 ))
			 (error "|A| is only possible when A is square"))
		)
  )


(define the-id 'unset)

;---- (determinant A) returns the determinant of the matrix, but more efficiently for 2x2, 3x3
(define (determinant A #!optional (force #f) (checked #f))
  (if (not checked)
		(if (matrix? A)
			 (determinant A force #t)
			 (error "determinant was passed a non-matrix"))
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
				r))
			
			(else
			 (general-determinant A checked))))
		)
  )


;---- (swap-r/c M rc1 rc2 #!optional (on-columns #f)) returns a copy of M, but with rows or columns swapped
(define (swap-r/c M rc1 rc2 #!optional (on-columns #f))
  (swap-r/c! (matrix-copy M) rc1 rc2 on-columns))

;---- (swap-r/c M rc1 rc2 #!optional (on-columns #f)) returns M, after swapping  rows or columns
(define (swap-r/c! M rc1 rc2 #!optional (on-columns #f))
  (if (not M)
		#f
		(begin
		  (if on-columns
				((swap-r/c! (M ^T!) rc1 rc2) ^T!)
				(let ((x1 (1- rc1))
						(x2 (1- rc2)))
				  (let* ((m (M '!)) ;; return the list M literally
							(r (list-ref m x1))
							(A (M //!)) ;; return the actual aux matrix
							)

					 (list-set! m x1 (list-ref m x2))
					 (list-set! m x2 r)
					 ;;(M 'set-matrix! m)
					 (if A
						  (swap-r/c! A rc1 rc2 on-columns)) ;; This ought to recurse through augmented matrices
					 )
				  M)
				)
		  )
		)
  )

;---- (r/c-operation  op M ix K #!optional (on-column #f)) performs a generic row or column operation on a copy of a matrix

(define (r/c-operation  op M ix K #!optional (on-column #f))
  (let ((Mc (matrix-copy M)))
	 (r/c-operation! op M2 ix K on-column)))

;---- (r/c-operation!  op M ix K #!optional (on-column #f)) performs a generic row or column operation on a matrix in situ

(define (r/c-operation! op M ix K #!optional (on-column #f))
  (if on-column
		(transpose! (r/c-operation! op (transpose! M) ix K #f))

		(let* ((M2 (M '!))
				 (A (M //!)) ;; we don't need to copy this
				 (lA (if A A #f))
				 (nA (if A (A 'nc) #f)) ;; A as a list-of-lists
				 )

		  (for-each 
			(lambda (i) (let ((v (list-ref (list-ref M2 (- ix 1)) i)))
							  (list-set! (list-ref M2 (- ix 1)) i (op v K))))
			(seq (M 'nc)))
		  (if A
				(for-each
				 (lambda (i) (let ((v (list-ref (list-ref lA (- ix 1)) i)))
									(list-set! (list-ref lA (- ix 1)) i (op v K))))
				 (seq nA)))
		  M

		  ))
  )


(define (matrix-already-present? M m)
  (or 
	(equal? M m)
	(equal? (M //!) m)
	(equal? (M __!) m)
	(and (M //!)
		  (matrix-already-present? (M //!) m))
	(and (M __!)
		  (matrix-already-present? (M __!) m))
	))


(define (matrix->matrix-lists M)
  (let ((m M)
		  (a (M //))
		  (b (M __)))
	 (list (M) (if a (a) a) (if b (b) b))))


(define (matrix-lists->matrix MAB)
  (let* ((n (if (list? MAB) (length MAB) #f))
			(ok (apply m-andf (map matrix-list? MAB)))
			(M (if (or (not n) (< n 1))
					 #f
					 (matrix-list->matrix (car MAB))))
			(A (if (or (not n) (< n 2))
					 #f
					 (matrix-list->matrix (cadr MAB))))
			(B (if (or (not n) (< n 3))
					 #f
					 (matrix-list->matrix (caddr MAB)))))
	 (M //! A)
	 (M __! B)
	 M
	 ))


"
An augmented matrix list takes the forms 

  ((M_1 // A_1)
   (M_2 // A_2)
   ...
   (M_k // A_k)
   ... 
   (M_n // A_n))
   

or

	((M_1 // A_1)
   (M_2 // A_2)
   ...
   (M_n // A_n)
   __
   (B_1 B_2 ..B_k))
   )

or

  ((M_1 //)
   (M_2 //)
   ...
   (M_n //)
   __
   (B_1 ... B_k))

This is a somewhat cumbersome notation to parse, but it has the advantage 
corresponding to the explicit notation we use with matrices and their augmentations.
"


;; THOUGH RECURSIVE AUGMENTATION IS CONCEIVABLY POSSIBLE, THIS DOES NOT HANDLE IT
(define (augmented-matrix-list? L)
  (if (not (list? L))
		#f
		(let* ((ma-b ((split-list-on-symbol __) L)) ;
				 (b (if (null? (cdr ma-b)) #f (cadr ma-b)))
				 (m-a (map (split-list-on-symbol //) (car ((split-list-on-symbol __) L))))
				 (m (map car m-a))
				 (a (if (< (length (car m-a)) 2) #f (map cadr m-a)))
				 )
		  (and (matrix-list? m)
				 (or (eq? b #f) (matrix-list? b))
				 (or (eq? a #f) (matrix-list? a)))
		  )
		))

;; THOUGH RECURSIVE AUGMENTATION IS CONCEIVABLY POSSIBLE, THIS DOES NOT HANDLE IT
(define (augmented-matrix-list->matrix-lists L)
  (let* ((ma-b ((split-list-on-symbol __) L)) ;
			(b (if (null? (cdr ma-b)) #f (cadr ma-b)))
			(m-a (map (split-list-on-symbol //) (car ((split-list-on-symbol __) L))))
			(m (map car m-a))
			(a (if (< (length (car m-a)) 2) #f (map cadr m-a)))
			)
	 
	 (if (and (matrix-list? m)
				 (or (eq? b #f) (matrix-list? b))
				 (or (eq? a #f) (matrix-list? a)))
		  (list m a b))))



(define (matrix-lists->augmented-matrix-list ML)
  (let* ((M-  (car ML)) ;; must be there!
			(A (if (and ML (pair? (cdr ML))) (cadr ML) #f))
			(B (if (and ML (pair? (cdr ML)) (pair? (cddr ML))) (caddr ML) #f))
			(NM (length M-))
			(N (max (if B (length B) 0) (length M-)))
			(NA (if A (length A) 0))
			(NB (if B (length B) 0))
			(M (if (< NM NB) M (append M- (make-list (- NB NM) '()))))
			)
	 (cond
	  ((and A (not B)) (map append  (map append M (make-list N '(:)) A )))
	  ((and B (not A)) (map append  (map append M (make-list N '(:)) (make-list N '(:)) B)))
	  ((and A B) (map append  (map append M (make-list N '(:)) A (make-list N '(:)) B)))
	  (#t (list (M))))
	 )
  )


(define (split-list-on-symbol symbol)
  ;; This generates a function which will map a single list onto a list of lists
  ;; split at any point the symbol occurs
  ;; use like ((split-list-on-symbol ':) lst)
  
  (let ((sym symbol))
	 (lambda (lst)
		(let loop ((L lst)
					  (A '())
					  (L* '())
					  )
		  (cond
			((null? L)
			 (reverse (cons (reverse A) L*)))
			((eq? (car L) sym)
			 (loop (cdr L) '() (cons (reverse A) L*)))
			(#t
			 (loop (cdr L) (cons (car L) A) L*)))))))

(define (augmented-matrix-list->matrix-lists MAB)
  (pivot (map (split-list-on-symbol ':) MAB)))

(define (matrix->augmented-matrix-list M)
  (matrix-lists->augmented-matrix-list (matrix->matrix-lists M)))


;we aren't generating the "below" augmentation when we map a matrix to a list

(define (augmented-matrix-list->matrix MA)
  (let* ((MAB (augmented-matrix-list->matrix-lists MA))
			(M (make-matrix (car MAB)))
			(A (if (pair? (cdr MAB)) (make-matrix (cadr MAB)) #f))
			(B (if (pair? (cddr MAB)) (make-matrix (caddr MAB)) #f)))
	 (cond
	  ((and A B) (M //! A __! B)
		M)
	  (A (M //! A)
		  M)
	  (#t M))))


;; (define matrix-list->matrix make-matrix) ;; a matrix-list defines *one* matrix without augmentation
;;    is defined immediately after make-matrix.

(define (matrix->matrix-list m) (m '%)) ;; checked


;-- (make-matrix . args)  Constructs a closure which represents a matrix.
"
While some of the functions which effect mathematical operations are defined as 
separate functions, most of the general transformations are accessed by code within 
this closure. The args should be a matrix-list, or symbols indicating operations 
followed by appropriate arguments.
"


(define (make-matrix . args)
  "This can be called with a number of different argument configurations:
     (make-matrix 3 4 '( 1 2 3 4 10 11 12 15 34 33 32 21))
        => ((1 2 3 4) (10 11 12 15) (34 33 32 31))

     (make-matrix '((1 2 3) (4 5 6)) 5 5)
        => ((1 2 3 0 0) (4 5 6 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))

     (make-matrix '(2 3 5 7))
        -> ((2) (3) (5) (7))

     (make-matrix mtx) 
        => returns a copy of mtx

     (make-matrix mtx '((4 5) (6 7) (8 9))
        => returns a new matrix with has the columns (indicated as elements in rows) appended on the RHS


  "
  (let ((frantic #f)
		  (M '())
		  (RESULT #f)
		  (rows #f)
		  (columns #f)
		  (help help)
		  ;; The augmentations are lists of subsidiary matrices (so we can do more than one at a time)
		  (augmentation-A #f) ;; operations affect augmented rows -- M/A
		  (augmentation-B #f) ;; operations affect augmented columns M
;                                                                  ---
;                                                                   A
;		                          or
;                                                                   M / A
;                                                                  ---
;                                                                   B
		  (HELP-FOR-MAKE-MATRIX #f)
		  ;; Note:  this flag affects what make-matrix does when no arguments are passed in
		  )

	 (cond
	  ((null? args)
		(dnl matrix-doco))

	  ((and (list? (car args)) (apply m-andf (map number? (car args))))
		(make-matrix (map list (car args)))) ;; make a column vector

	  ((and (null? (cdr args)) (apply m-andf (map matrix? args)))
		;; this is a clone operation

;(dnl "Cloning, I can hear the Imperial March now....")
;(dnl (car args))
		
		(if (= (length args) 1)
			 (matrix-copy (car args))
			 (map matrix-copy args)))
	  (#t
		(letrec ([matrix
					 (lambda matrix-args
						(let* ((N (length matrix-args))
								 (args (if (null? matrix-args) '() (cdr matrix-args)))
								 (car-is-n (and (>= N 1) (number? (car matrix-args))))
								 (car-is-l (and (>= N 1) (list? (car matrix-args))))
								 (cadr-is-n (and (> N 1) (number? (cadr matrix-args))))
								 (cadr-is-l (and (> N 1) (list? (cadr matrix-args))))
								 (caddr-is-n (and (> N 2) (number? (caddr matrix-args))))
								 (caddr-is-l (and (> N 2) (list? (caddr matrix-args))))
								 (cadddr-is-n (and (> N 3) (number? (cadddr matrix-args))))
								 (cadddr-is-l (and (> N 3) (list? (cadddr matrix-args))))
								 (indent (lambda (#!optional (n 0)) (make-string (* 2 n) #\space)))
								 )
						  
						  ;;(dnl "matrix arguments: " matrix-args)
						  ;;(display "M = ") (dnl M)
						  ;;(display "A = ") (pp augmentation-A)
						  ;;(display "B = ") (pp augmentation-B)

						  (letrec ((R 
										;;This cond does most of the processing, but there is a little left over at the end
										;; These are the arguments that can be passed to a matrix....

										(cond
										 ;;***************************************************	
										 ;; Return a copy of the list-of-lists (LoL) which defines the matrix

										 [(or (null? matrix-args) (eq? '% (car matrix-args))) ;; checked
										  ;; return a copy of the matrix as a list-of-lists
										  (deep-copy M)
										  ]

										 [(member (car matrix-args) '(valid?)) ;; could add ok?, but lets keep it clear
										  (and (matrix-list? M)
												 (or (not augmentation-A)
													  (and (matrix? augmentation-A)
															 (= rows (augmentation-A 'nr))))
												 (or (not augmentation-B)
													  (and (matrix? augmentation-B)
															 (= columns (augmentation-B 'nc))))
												 (apply = (cons columns (map length M))) ;; all the columns in M should be the same length
												 (= rows (length M))
												 )
										  ]

										 [(member (car matrix-args) '(last last-result)) ;; checked
										  RESULT]
										 
										 [(member (car matrix-args) '(nc cols columns )) ;;checked
										  columns
										  ]
										 [(member (car matrix-args) '(nr rows)) ;; checked
										  rows
										  ]
										 [(member (car matrix-args) '(dim dimensions rows-columns))
										  (list rows columns)]
										 [(eq? (car matrix-args) 'matrix-lists) (list (list-copy M)
																									 (if augmentation-A  (augmentation-A) #f)
																									 (if augmentation-B  (augmentation-B) #f))]
										 
										 [(member (car matrix-args) '(exact! inexact!))
										  (let ((m (map (lambda (r) (map (if (equal? (car matrix-args) 'exact) inexact->exact exact->inexact) r)) M)))
											 (set-car! M (car m))
											 (set-cdr! M (cdr m))
											 )
										  matrix
										  ]

										 [(member (car matrix-args) '(exact inexact))
										  (let ((NM (make-matrix M)))
											 (NM (if (equal? (car matrix-args) 'exact) 'exact! 'inexact!)))]



										 [(member (car matrix-args) '(aml aug-matrix-list augmented-matrix-list))
										  (let ((AML (cond
														  ((and augmentation-A augmentation-B)
															(let ((a (matrix-lists->augmented-matrix-list (list (list-copy M) (augmentation-A) (augmentation-B)))))
															  a)
															)
														  (augmentation-A
															(let ((a (matrix-lists->augmented-matrix-list (list (list-copy M) (augmentation-A) #f))))
															  a)
															)
														  (augmentation-B
															(let ((a (matrix-lists->augmented-matrix-list (list (list-copy M) #f (augmentation-B)))))
															  a)
															)
														  (#t (list (list-copy M) #f #f))
														  )))
											 AML
											 )]

										 ;; Extract a submatrix --- if there is only one pair of indices, it assumes the upper left corner is the other
										 [(and (member (car matrix-args) '(extract submatrix))
												 (or (= N 2) (= N 3))
												 )

										  (cond
											((matrix? (car args))
											 (let ((-m- (car args)))
												(submatrix '(1 1) (list (-m- 'nr) (-m- 'nc)))))
											((matrix-list? (car args))
											 (let ((-m- (car args)))
												(submatrix '(1 1) (list (length -m-)  (length (car -m-))))))
											(#t
											 (cond
											  ((and (= N 2) (indices? (car args))) (submatrix (list-copy M) '(1 1) (car args)))
											  ((= N 3) (submatrix (make-matrix M) (car args) (cadr args)))
											  (#t (error "bad indices specified in 'extract or 'submatrix" args))
											  ))
											)
										  ]

										 ;; Embed the matrix passed in the arguments in the upper left corner of the matrix
										 ;; if no row, column offsets are indicated
										 [(and (member (car matrix-args) '(embed! embed))
												 (matrix? (car args))
												 (>= N  2))
										  (let* ((A (if (equal? (car matrix-args) 'embed!)
															 matrix
															 (make-matrix (list-copy M))
															 ))
													(am (A '!))
													(ar (A 'nr))
													(ac (A 'nc))
													(vm ((car args)))
													(vr (length vm))
													(vc (length (car vm)))
													(ro (if (and (>= N 3) (posint? (cadr args))) (cadr args) 0))
													(co (if (and (>= N 4) (posint? (caddr args))) (caddr args) 0))
													)
											 (if (or (> (+ ro vr) ar) (> (+ co vc) ac) )
												  (error "matrix to be embedded spills over the rows/columns of the matrix" args))

											 (if (and rigid-embedding (or (< columns ((car args) 'nc)) (< rows ((car args) 'nr))))
												  (error "the matrix to be embedded in is too large" args)
												  (for-each
													(lambda (i)
													  (for-each
														(lambda (j)
														  (let ((ix (list  i j))
																  (dix (list (+ ro i) (+ co j)))
																  )
															 (dnl* "   Processing" ix "-->" (list-ref* vm ix))
															 (list-set*! am dix (list-ref* vm ix))
															 ))
														(seq (min columns vc)))
													  )
													(seq (min rows vr)))
												  )
											 A)]


										 ;;***************************************************	
										 ;; Return a matrix by appending the columns of another matrix on the RHS

										 [(matrix? (car matrix-args))
										  (if (= (length M) (length ((car matrix-args))))
												(let ((r (make-matrix (map append M ((car matrix-args))))))
												  r)
												(error "The matrices do not have the same number of rows" args))
										  ]

										 [(and (= N 3)
												 (posint? (car matrix-args))
												 (posint? (cadr matrix-args))
												 (matrix-list? (caddr matrix-args))
												 )
										  (let* ((r (car matrix-args))
													(c (cadr matrix-args))
													(w (caddr matrix-args))
												  )
											 (set! M (map (lambda (i) (make-list r 0)) (seq c)))
											 (set! rows r)
											 (set! columns c)
											 (for-each
											  (lambda (i)
												 (for-each
												  (lambda (j) (list-set*! M (list i j) (list-ref* w (list i j))))
												  (seq c))
												 )
											  (seq r)))
										  ]
																				
											 
										 ;;***************************************************	
										 ;; Return the list-of-lists [LoL] which defines the matrix

										 [(eq? (car matrix-args) '!)
										  ;; return the LoL that holds row/column data
										  (let ((r M))
											 ;;											 (dnl "returning reference to M" r)
											 r)

										  ]

										 ;;***************************************************	
										 ;; resets the matrix data to a copy of the argument [an LoL]
										 
										 ;; [(eq? (car matrix-args) 'set-matrix)
										 ;;  (if (matrix-list? (cadr args))
										 ;; 		  (set! M (deep-copy (cadr args)))
										 ;; 		  matrix)
										 ;;  (error "Not a list matrix" (cadr args))]


										 ;;***************************************************	
										 ;; resets the matrix data to a copy of the argument [an LoL]  ;;**NOTE** args === (cdr matrix-args)
										 
										 [(eq? (car matrix-args) 'set-matrix%)
										  (cond
											((matrix-list? (car args)) (set! M (deep-copy (car args))) matrix)
											((matrix? (car args)) (set! M ((car args))) matrix)
											(#t (error "Not a list matrix" (car args)))
											)]

										 ;;***************************************************	
										 ;; resets (aliases) the matrix data to the argument to the matrix argument

										 [(eq? (car matrix-args) 'set-matrix!)
										  (cond
											((matrix? (car args));;**NOTE** args === (cdr matrix-args)
											 (let ((m ((car args))))
												(set-car! M (car m))
												(set-cdr! M (cdr m))))

											((matrix-list? (car args))
											 (set-car! M (car (car args)))
											 (set-cdr! M (cdr (car args)))
											 matrix)
											(else	(error "Not a matrix" (car args))))
										  ]

										 ;;***************************************************	
										 ;; Sort the rows in a matrix [basic row operation]

										 ;; ;; [(member (car matrix-args) '(sort sort-matrix))
										 ;; ;;  (let ((m (matrix-copy matrix)))
										 ;; ;; 	 (m 'sort-rows!)
										 ;; ;; 	 (set! M m)
										 ;; ;; 	 )]


										 ;; [(member (car matrix-args) '(<! >! sort! sort<! sort>!)) ;; sort! defaults to <!
										 ;;  (let* ((al (matrix-lists->augmented-matrix-list
										 ;; 				 (list M
										 ;; 						 (if augmentation-A (augmentation-A '!))
										 ;; 						 (if augmentation-B (augmentation-B '!)))))
										 ;; 		  (sal (sort al list<?)) ;; NOTE this needs a conditional! 
										 ;; 		  (ml (augmented-matrix-list->matrix-lists sal))
										 ;; 		  )
										 ;; 	 (let* ((m (car ml))
										 ;; 			  (l (length ml))
										 ;; 			  (a (if (and (>= l 1) (cadr ml))
										 ;; 			  (b (if (and
										 ;; 						 (pair? (cdr ml))
										 ;; 						 (pair? (cddr ml)))
										 ;; 						(caddr ml) #f)
										 ;; 				  )
										 ;; 			  )
										 ;;  #f]
										 ;; [(member (car matrix-args) '(<! sort<! sort-matrix<!))
										 

										 
										 ;; [(member (car matrix-args) '(sort! sort-matrix!))
										 ;;  (let* ((mal (matrix-lists->augmented-matrix-list
										 ;; 					(if augmentation-A (list M (augmentation-A)) #f)))
										 ;; 			(smal (sort mal list>?))
										 ;; 			(sm (augmented-matrix-list->matrix-lists smal))
										 ;; 			(m (car sm))
										 ;; 			(a (if (pair? (cdr sm)) (cadr sm) #f))
										 ;; 			)
										 ;; 	 (dnl 'mal " -> " mal)
										 ;; 	 (dnl 'smal " -> " smal)
										 ;; 	 (dnl 'sm " -> " sm)

										 ;; 	 ;; Adjust M...
										 ;; 	 (if #t ;; preserve topmost cons
										 ;; 		  (begin
										 ;; 			 (set-car! M (car m )) ;; recall, M is the LoL that holds the elements
										 ;; 			 (set-cdr! M (cdr m )))
										 ;; 		  (set! M m) ;; ... or not
										 ;; 		  )
										 
										 ;; 	 ;; Adjust A if necessary
										 ;; 	 (if a
										 ;; 		  (begin
										 ;; 			 (if #t
										 ;; 				  (let ((a (m //!)))
										 ;; 					 (set-car! augmentation-A (car (a '!)))
										 ;; 					 (set-cdr! augmentation-A (cdr (a '!))))
										 ;; 				  (set! augmentation-A a)
										 ;; 				  )
										 ;; 			 ))
										 ;; 	 )
										 ;; matrix
										 ;; ]
										 
										 [(member (car matrix-args) '(rref reduced-row-echelon-form row-echelon-form GJF)) ;; there is no non-destructive version
										  (let ((m (matrix-copy matrix)))
											 (m 'row-echelon-form!)
											 m
											 )
										  ]

										 [(member (car matrix-args) '(rref! reduced-row-echelon-form! row-echelon-form! GJF!)) ;; there is no non-destructive version
										  (let ((m (gauss-jordan-reduction M)))
											 (set-car! M (car m))
											 (set-cdr! M (cdr m))
											 )
										  matrix
										  ]
										 

										 ;;***************************************************	
										 ;; Dump data prettily

										 [(eq? (car matrix-args) 'dump)
										  (if augmentation-A
												(for-each
												 (lambda (row row-a)
													(display (append row '(:) row-a))
													(newline))
												 M (augmentation-A))
												(for-each
												 (lambda (row)
													(display row)
													(newline))
												 M))
										  (if augmentation-B
												(begin
												  (for-each
													(lambda (x)
													  (display " - ")
													  )
													(car (augmentation-B)))
												  (newline)
												  (for-each
													(lambda (x)
													  (display x)
													  (newline))
													(augmentation-B))))
										  (newline)
										  ]

										 ;;***************************************************	
										 ;; produce a LaTeX string that represents the array.

										 [(member (car matrix-args) '(latex latex*))
										  (let ((result (let* ((matrixwidth (length (car M)))
																	  (augmatrixwidth (+ matrixwidth (if augmentation-A (augmentation-A 'nc) 0)))
																	  (preamble  (string-append
																					  "\\left(\\begin{array}{"
																					  (make-string matrixwidth (if #t #\c #\l)) 
																					  (if augmentation-A
																							(string-append "|" (make-string (augmentation-A 'nc) (if #t #\c #\l)) "}\n")
																							"}\n")))
																	  (conclusion "\\end{array}\\right)\n")
																	  (result ""))
																
																;;  \left( \begin{array}{ccc|c} 1 & 2 & 3 & 4 \\ 5 & 6 & 7 & 8 \end{array}

																
																(let ((ma-data (if augmentation-A
																						 (map append M (augmentation-A))
																						 M)))
																  (string-append
																	preamble
																	(apply string-append
																			 (map 	
																			  (lambda (row)
																				 (string-append
																				  (object->string (car row))
																				  (apply string-append 
																							(map
																							 (lambda (el)
																								(string-append " & " (object->string el)))
																							 (cdr row)))
																				  "\\\\\n"))
																			  ma-data))

																	(if augmentation-B
																		 (string-append
																		  (apply string-append
																					(map 	
																					 (lambda (row)
																						(string-append
																						 (object->string (car row))
																						 (apply string-append 
																								  (map
																									(lambda (el)
																									  (string-append " & " (object->string el)))
																									(cdr row)))
																						 (apply string-append (make-list (- augmatrixwidth (augmentation-B 'nc)) " & "))
																						 
																						 "\\\\\n"))
																					 (augmentation-B)))
																		  conclusion "\n")
																		 ))
																  ))
															 )
												  )
											 (case (car matrix-args)
												((latex) result)
												((latex*) (display result))
												))
										  ]
										 

										 ;;***************************************************	
										 ;; Simple output of matrix data
										 [(eq? (car matrix-args) 'dump)
										  (dnl "Matrix" M)
										  (if (augmentation-A) (dnl "Side aug: " augmentation-A))
										  (if (augmentation-B) (dnl "Bottom aug: " augmentation-B))
										  ]
										 
										 ;;***************************************************	
										 ;; Display help data

										 [(member (car matrix-args) '(help doco documentation))
										  (display matrix-doco)]
										 
										 
										 [(and (pair? matrix-args) (= 1 N)
												 (or car-is-n
													  car-is-l))
										  (abort "Bad attempt dereferencing a matrix: use either no indices for \neverything, or two for an element")]


										 ;;***************************************************	
										 ;; return  either a submatrix or an element

										 ;;NOTE need to test this...
										 [(and (pair? matrix-args) (= 2 N) 
												 (or car-is-n car-is-l)
												 (or cadr-is-n cadr-is-l))

										  (if (and car-is-n cadr-is-n)
												(matrix-list-ref* M (map (lambda (x) (- x 1)) matrix-args))
												(cond 
												 (car-is-n
												  (make-matrix (list-ref-cols (list (list-ref M ((lambda (x) (- x 1)) (car matrix-args)))) (map (lambda (x) (- x 1)) (cadr matrix-args)))))
												 (cadr-is-n
												  (make-matrix (list-ref-rows (*list-nth* M (- (cadr matrix-args) 1)) (map (lambda (x) (- x 1)) (car matrix-args)))))
												 (else
												  (make-matrix (list-ref-cols (list-ref-rows M (map (lambda (x) (- x 1)) (car matrix-args))) (map (lambda (x) (- x 1)) (cadr matrix-args)))))
												 )
												)
										  ]


										 ;;***************************************************	
										 ;; return a row of the matrix

										 [(and (member (car matrix-args) '(r row)) (= 2 N) cadr-is-n)
										  ;; return the indicated row vector ** adjust for the 0-1 indexing difference **
										  (make-matrix (list (list-ref M (- (cadr matrix-args) 1))))
										  ]
										 ;;***************************************************	
										 ;; return a column of the matrix

										 [(and (member (car matrix-args) '(c col column)) (= 2 N) cadr-is-n)
										  ;; return the indicated column vector ** adjust for the 0-1 indexing difference **
										  ((make-matrix (list (*list-nth* M (- (cadr matrix-args) 1)))) ^t)
										  ]

										 ;;***************************************************	
										 ;; return the determinant of the matrix

										 [(and (member (car matrix-args) '(det determinant)))
										  (determinant matrix)
										  ]

										 ;;***************************************************	
										 ;; return the conjugate of the matrix

										 [(member (car matrix-args) '(conj conjugate ^- ^*))
										  ;; return the conjugate matrix
										  (frisk-values (make-matrix (matrix-element-map complex-conjugate M)))
										  ]

										 ;;***************************************************	
										 ;; set the matrix to its complex conjugate

										 [(member (car matrix-args) '(conj! conjugate! ^-! ^*!))
										  ;; return the conjugate of the matrix
										  (set! M ((frisk-values (make-matrix (matrix-element-map complex-conjugate M)))))
										  ]

										 ;;***************************************************	
										 ;; return a copy of the transpose of the matrix

										 [(member (car matrix-args) '(^t ^T ))
										  ;; return the transpose of the matrix
										  (let* ((newB (if augmentation-A (augmentation-A ^T) #f))
													(newA (if augmentation-B (augmentation-B ^T) #f))
													(N (make-matrix (pivot M)))
													)
											 (if newA (N '//! newA))
											 (if newB (N '__! newB))
											 N
											 )]

										 ;;***************************************************	
										 ;; IN SITU transpose

										 [(member (car matrix-args) '(^t! ^T! ))
										  ;; return the transpose of the matrix
										  (let* ((newB (if augmentation-A (augmentation-A ^T) #f))
													(newA (if augmentation-B (augmentation-B ^T) #f))
													)
											 (set! M (pivot M))
											 (set! augmentation-A newA)
											 (set! augmentation-B newB)
											 matrix
											 )]
										 
										 ;;***************************************************	
										 ;; return the complementary matrix for the indicated rows and columns or the ijth element

										 [(and (member (car matrix-args) '(comp-matrix)) (= 2 N))
										  (let ((r (make-matrix (complementary-matrix-list M (caadr matrix-args) (cadadr matrix-args)))))
											 (r 'dump)
											 r)
										  ]

										 [(and (member (car matrix-args) '(comp-matrix)) (= 3 N))
										  (make-matrix (complementary-matrix-list M (cadr matrix-args) (caddr matrix-args)))
										  ]

										 ;;***************************************************	
										 ;; return a matrix minor	
										 
										 [(and (member (car matrix-args) '(minor)) (= 3 N) cadr-is-l caddr-is-l)
										  (if (= rows columns) 
												;; return the minor for the indicated ij
												(determinant (matrix 'comp-matrix (cadr matrix-args) (caddr matrix-args)))
												(error "Minors cannot be calculated in non-square matrices"))
										  ]

										 ;;***************************************************	
										 ;; return the cofactor matrix  [OVERLOADED!]
										 
										 [(and (member (car matrix-args) '(cof cofactor)) (= 1 N))
										  ;; return the cofactor matrix for the indicated ij (replace each element by its cofactor)
										  (frisk-values (matrix-map cofactor matrix))
										  ]


										 ;;***************************************************	
										 ;;  return the cofactor [signed minor] for the indicated ij  [OVERLOADED!]
										 [(and (member (car matrix-args) '(cof cofactor signed-minor))
												 (= 3 N) 
												 (or cadr-is-n cadr-is-l)
												 (or caddr-is-n caddr-is-l))
										  (cofactor matrix (cadr matrix-args) (caddr  matrix-args))
										  
										  ;;(* (power -1 (+ (cadr matrix-args) (caddr matrix-args))) 
										  ;;(determinant (matrix 'comp-matrix (cadr matrix-args) (caddr matrix-args)))
										  ;;(matrix 'minor (cadr matrix-args) (caddr matrix-args))
										  ;;)
										  ]

										 ;;***************************************************	
										 ;; return the adjugate [adjoint] matrix

										 [(member (car matrix-args) '(adj adjugate adjoint))
										  ;; return the adjugate
										  (let ((m (matrix 'cofactor)))
											 (m ^T))
										  ]

										 ;;***************************************************	
										 ;; return the conjugate transpose of the matrix

										 [(member (car matrix-args) '(^H H ^*t ^t* ^*T ^T* *T T* *t t*))
										  ;; return the conjugate transpose of the matrix (complex)
										  (let ((m (matrix 'conj)))
											 (m ^T))
										  ]

										 ;;***************************************************	
										 ;; return the inverse matrix
										 
										 [(member (car matrix-args) '(^i ^I ^inv inv inverse))
										  ;; returpn the inverse
										  (let ((d (matrix 'det)))
											 (if (zero? d)
												  #f
												  (frisk-values (* (/ 1.0 d) (matrix 'cof)))))]
										 
										 ;;***************************************************	
										 ;; return the multiplication of the matrix by a scalar

										 [(and (= 2 N) (member (car matrix-args) '(X mult)) cadr-is-n)
										  (matrix-element-map (lambda (e) (* e (cadr matrix-args))) matrix)]

										 [(and (= 3 N) (member (car matrix-args) '(set-row!))
												 (integer? (cadr matrix-args)) (< 0 (cadr matrix-args))
												 (<= (cadr matrix-args) rows) (number? (caddr matrix-args))) ;; (newA 'set-row i lst)
										  (list-set! (cadr matrix-args) (- (caddr matrix-args) 1) (cadddr matrix-args))
										  ]

										 ;; **************************************************
										 ;;      Elementary row operations on a matrix
										 ;; 
										 
										 ;;      row/column multiplication by a constant

										 [(and (eqv? (car matrix-args) 'row:*) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation * matrix (cadr matrix-args) (caddr matrix-args))]
										 [(and (eqv? (car matrix-args) 'row:/) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation / matrix (cadr matrix-args) (caddr matrix-args))]
										 [(and (eqv? (car matrix-args) 'row:+) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation + matrix (cadr matrix-args) (caddr matrix-args))]
										 [(and (eqv? (car matrix-args) 'row:-) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation - matrix (cadr matrix-args) (caddr matrix-args))]

										 [(and (eqv? (car matrix-args) 'col:*) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation * matrix (cadr matrix-args) (caddr matrix-args) #t)]
										 [(and (eqv? (car matrix-args) 'col:/) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation / matrix (cadr matrix-args) (caddr matrix-args) #t)]
										 [(and (eqv? (car matrix-args) 'col:+) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation + matrix (cadr matrix-args) (caddr matrix-args) #t)]
										 [(and (eqv? (car matrix-args) 'col:-) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation - matrix (cadr matrix-args) (caddr matrix-args) #t)]

										 ;; **************************************************
										 ;;      Elementary row operations on a matrix
										 ;; 
										 
										 ;;      row/column multiplication by a constant

										 [(and (eqv? (car matrix-args) 'row:*!) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation! * matrix (cadr matrix-args) (caddr matrix-args))]
										 [(and (eqv? (car matrix-args) 'row:/!) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation! / matrix (cadr matrix-args) (caddr matrix-args))]
										 [(and (eqv? (car matrix-args) 'row:+!) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation! + matrix (cadr matrix-args) (caddr matrix-args))]
										 [(and (eqv? (car matrix-args) 'row:-!) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation! - matrix (cadr matrix-args) (caddr matrix-args))]

										 [(and (eqv? (car matrix-args) 'col:*!) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation! * matrix (cadr matrix-args) (caddr matrix-args) #t)]
										 [(and (eqv? (car matrix-args) 'col:/!) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation! / matrix (cadr matrix-args) (caddr matrix-args) #t)]
										 [(and (eqv? (car matrix-args) 'col:+!) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation! + matrix (cadr matrix-args) (caddr matrix-args) #t)]
										 [(and (eqv? (car matrix-args) 'col:-!) (= 3 N) cadr-is-n caddr-is-n)
										  (r/c-operation! - matrix (cadr matrix-args) (caddr matrix-args) #t)]

										 [(and (eqv? (car matrix-args) 'row:-/!) (= N 4) cadr-is-n caddr-is-n cadddr-is-n)  ;;**NOTE** args === (cdr matrix-args)
										  ;; (matrix 'row:-/ i j s
										  ;; effectively
										  ;;                
										  ;;                r_i  = r_i- r_j/s
										  ;;
										  ;; the r_ik/s is usually a row with a smaller index, eg (M 1 1)...
										  ;; and a typical call might be (M 'row:-/ 2 1
										  (let* ((i (1- (car args))) ;; row to be modified ;;**NOTE** args === (cdr matrix-args)
													(j (1- (cadr args))) ;; first col
													(s (caddr args))  ;; scalar
													(ri (list-ref M i))
													(rj (list-ref M j))
													(V (map (lambda (e) (- (list-ref ri e) (/ (list-ref rj e) s))) (seq (length ri))))
													)
											 ;;(dnl "V = " V)
											 (list-set! (list-ref M i) j V)
											 ;;(dnl "M = " M)
											 ;;(dnl "Using row " j ", "  (list-ref M j)",")
											 ;;(dnl "Row/, " (list-ref M i) ", becomes " V)
											 )
										  (set-car! matrix-args 'ok)
										  matrix]

										 [(and (eqv? (car matrix-args) 'row:-*!) (= N 4) cadr-is-n caddr-is-n cadddr-is-n)
										  ;; (matrix 0'row:-/ i j s
										  ;; effectively
										  ;;                
										  ;;                r_i  = r_i- r_j * s
										  ;;
										  ;; the r_ik/s is usually a row with a smaller index, eg (M 1 1)...
										  ;; and a typical call might be (M 'row:-*! 2 1 (M 1 1));;**NOTE** args === (cdr matrix-args)
										  (let* ((i (1- (car args))) ;; row to be modified
													(j (1- (cadr args))) ;; first col
													(s (caddr args))  ;; scalar
													(ri (list-ref M i)) ;; target
													(rj (list-ref M j)) ;; modifier
													(V (map (lambda (e)
																 (- (list-ref ri e)
																	 (* (list-ref rj e) s))
																 )
															  (seq (length ri))))
													)
											 ;;(dnl "scaling value = " s)
											 ;;(dnl "Target row is " i ": " ri)
											 ;;(dnl "Row used as modifier is " j ": " (list-ref M j) " --> " (map (lambda (x) (* s x)) (list-ref M j)))
											 ;;(dnl "V = " V)
											 (list-set! M i V)
											 ;;(dnl "M = " M)
											 ;;(dnl "Using row " j ", "  (list-ref M j)",")
											 ;;(dnl "Row*, " (list-ref M i) ", becomes " V)
											 )
										  (set-car! matrix-args 'ok)
										  matrix]
										 
										 [(member (car matrix-args) '(R real)) (= 2 N)
										  (matrix-function-map exact->inexact M)]

										 [(member (car matrix-args) '(Q rational)) (= 2 N)
										  (matrix-function-map inexact->exact M)]
										 
										 [(member (car matrix-args) '(Q rational)) (= 2 N)
										  (matrix-function-map inexact->exact M)]
										 
										 ;;***************************************************	
										 ;; Swap rows and columns either generating a new matrix, or in situ

										 [(and (eqv? (car matrix-args) 'swap-rows!) (= 3 N) cadr-is-n caddr-is-n)
										  (swap-r/c! matrix (cadr matrix-args) (caddr matrix-args))]

										 [(and (member (car matrix-args) '(swap-columns! swap-cols!)) (= 3 N) cadr-is-n caddr-is-n)
										  (swap-r/c! matrix (cadr matrix-args) (caddr matrix-args) #t)]

										 [(and (eqv? (car matrix-args) 'swap-rows) (= 3 N) cadr-is-n caddr-is-n)
										  (swap-r/c matrix (cadr matrix-args) (caddr matrix-args))]

										 [(and (member (car matrix-args) '(swap-columns swap-cols)) (= 3 N) cadr-is-n caddr-is-n)
										  (swap-r/c matrix (cadr matrix-args) (caddr matrix-args) #t)]

										 ;;***************************************************	
										 ;; MAP ALL ELEMENTS WITH A FUNCTION in situ, but don't touch the augmentations

										 [(and (eq? (car matrix-args) 'map!) (= 2 N) (procedure? (cadr matrix-args)))
										  
										  (let ((f (cadr matrix-args)))
											 (for-each
											  (lambda (row)
												 (for-each
												  (lambda (i)
													 (list-set! row i (f (list-ref row i))))
												  (seq (length row)))
												 )
											  M))
										  
										  matrix]

										 ;;***************************************************	
										 ;; MAP ALL ELEMENTS WITH A FUNCTION in situ, including augmentations

										 [(and (eq? (car matrix-args) 'map!%) (= 2 N) (procedure? (car args)))
										  (let ((f (car args)))
											 (for-each
											  (lambda (row)
												 (for-each
												  (lambda (i)
													 (list-set! row i (f (list-ref row i))))
												  (seq (length row)))
												 )
											  M)
											 (if augmentation-A
												  (augmentation-A 'map!% f))
											 (if augmentation-B
												  (augmentation-B 'map!% f))
											 )
										  
										  matrix]

										 ;;***************************************************	
										 ;; MAP ALL ELEMENTS WITH A FUNCTION ON A COPY BUT NOT AUGMENTATIONS

										 [(and (eq? (car matrix-args) 'map) (= 2 N) (procedure? (car args)))
										  (let* ((MM (list-copy M))
													(f (car args)))
											 (for-each
											  (lambda (row)
												 (for-each
												  (lambda (i)
													 (list-set! row i (f (list-ref row i))))
												  (seq (length row)))
												 )
											  M))
										  
										  matrix]

										 ;;***************************************************	
										 ;; MAP ALL ELEMENTS WITH A FUNCTION ON A COPY, INCLUDING AUGMENTATIONS

										 [(and (eq? (car matrix-args) 'map%) (= 2 N) (procedure? (car args)))
										  (let* ((MM (list-copy M))
													(f (car args)))
											 (for-each
											  (lambda (row)
												 (for-each
												  (lambda (i)
													 (list-set! row i (f (list-ref row i))))
												  (seq (length row)))
												 )
											  M)
											 (if augmentation-A
												  (augmentation-A 'map% f))
											 (if augmentation-B
												  (augmentation-B 'map% f)))
										  matrix]
										 
										 ;;***************************************************	
										 ;; ASSIGN A VALUE TO AN ELEMENT **

										 [(and (= 4 N)
												 (member (car matrix-args) '(set-ij!)) 
												 cadr-is-n caddr-is-n
												 (number? (caddr args))
												 ;;(<= 1 (car args) rows)
												 ;;(<= 1 (cadr args) columns)
												 )
										  
										  (let ((i (car args)) (j (cadr args)))
											 ;;(for-each dnl M)
											 (if (and (<= 1 i rows) (<= 1 j columns))
												  (list-set*! M (list (1- i) (1- j)) (cadr args))
												  (error (string-append "Invalid indices in set-ij! " (object->string (list i j))  " should be less than " (object->string (list rows columns))))
												  ))
										  
										  ] ;;checked

										 ;;***************************************************	
										 ;; ASSIGN A VALUE TO AN ELEMENT

										 [(and (= 4 N)
												 (member (car matrix-args) '(set set-ij)) 
												 cadr-is-n caddr-is-n
												 (number? (cadddr matrix-args))
												 ;;(<= 1 (cadr matrix-args) rows)
												 ;;(<= 1 (caddr matrix-args) columns)
												 )
										  (let ((i (cadr matrix-args)) (j (caddr matrix-args))
												  (W (list-copy M)))
											 (set! M W)

											 ((caddr matrix-args) 'dump)

											 (if (and (<= 1 i rows) (<= 1 j columns))
												  (list-set*! M (list (1- i) (1- j)) (caddr matrix-args))
												  (error (string-append "Invalid indices in set-ij " (object->string (list i j))  " should be less than " (object->string (list rows columns))))
												  ))
										  ];; checked


										 ;; THIS ONLY SORTS THE ROWS OF THE MATRIX, NOT THE AUGMENTATIONS
										 ;; -- shouldn't be directly called, use sort-matrix< or sort-matrix> whic
										 ;; catches the augmentations
										 [(member (car matrix-args) '(sort-rows! sort-rows>! sort-rows<!))
										  (let* ((mtmp (if (not augmentation-A) (list-copy M) (map append M (augmentation-A))))
													(sortorder (if (equal? (car matrix-args) 'sort-rows<) list<? list>?))
													)
											 (let* ((ma (sort mtmp sortorder))
													  (m (map (lambda (r) (list-head r columns)) ma))
													  (a (denull (map (lambda (r) (list-tail r columns)) ma)))
													  (newA (if (or (null? a) (not a)) #f (make-matrix a)))
													  )
                                    (set-car! M (car m))
												(set-cdr! M (cdr m))
												(if (not (matrix? augmentation-A))
													 (set! augmentation-A #f)
													 (augmentation-A 'set-matrix! a))
												))
										  matrix
										  ]

										 [(member (car matrix-args) '(sort-rows sort-rows> sort-rows<))
										  (let* ((mtmp (if (not augmentation-A) (list-copy M) (map append M (augmentation-A))))
													(ma (sort mtmp list>?))
													(m (map (lambda (r) (list-head r columns)) ma))
													(a (map (lambda (r) (list-tail r columns)) ma))
													(newM (make-matrix m))
													(newA (if (or (null? a) (not a) (not (matrix-list? a))) #f (make-matrix a)))
													)
											 (newM // newA)
											 (newM __ augmentation-B)
											 newM)]

										 ;;***************************************************	
										 ;; test for augmented matrices

										 [(member (car matrix-args) '(augmented? aug?))
										  (list (and augmentation-A #t)(and augmentation-B #t))]
										 

										 ;;***************************************************	
										 ;; Check for errors in augmentation

										 [(and augmentation-A (eq? matrix augmentation-A) (eq matrix  augmentation-B))
										  (error "both matrix augmentations refer to the same matrix")]

										 [(and augmentation-A (eq? augmentation-A augmentation-B))
										  (error "both matrix augmentations refer to the same matrix")]

										 [(or (eq? augmentation-A matrix) (eq? augmentation-B matrix))
										  (error "circularity in matrix augmentation")]

										 [(and (= N 1) (eq? (car matrix-args) //)) ;; checked
										  ;;(dnl "WOMBAT//")
										  (matrix-copy augmentation-A)]
										 [(and (= N 1) (eq? (car matrix-args) __)) ;; checked
										  ;;(dnl "WOMBAT__")
										  (matrix-copy augmentation-B)]

										 [(and (= N 1) (eq? (car matrix-args) //!))
										  ;;(dnl "A WOMBAT//!")
										  augmentation-A]

										 [(and (= N 1) (eq? (car matrix-args) __!))
										  ;;(dnl "B WOMBAT__!")
										  augmentation-B]

										 [(and (>= 2 N) (member (car matrix-args) (if allow-shared-augmentations '(//  __) '(// __ //! __!))))
										  (let* [(op (car matrix-args))
													(is-A (and #t (member op '(// //! a A ))))
													]
											 ;;(dnl "MONK " matrix ":"  matrix-args " :: get " (if is-A "A" "B"))
											 
											 (let* ((m-ish (car args)) 
													  (m (cond
															((matrix? m-ish) (matrix-copy m-ish))
															((matrix-list? m-ish) (matrix-list->matrix m-ish))
															((augmented-matrix-list? m-ish) (augmented-matrix-list->matrix m-ish))
															((eq? m-ish #f) #f)
															(#t (error "bad argument passed to a set/get of an augmented matrix" matrix-args)))
														  ))
												(if (and m
															(or (and is-A (= rows (m 'nr)))
																 (= columns (m 'nc))))
													 (if is-A
														  (set! augmentation-A m) (set! augmentation-B m))) ;; copied in the cond clause
												matrix ;; NOTE: this does not copy matrix!  use (matrix-copy) to make an independent version
												))]

										 [(and (>= 2 N) allow-shared-augmentations (member (car matrix-args) '(//! __!)))
										  (let* [(op (car matrix-args))
										 			(is-A (and #t (member op '(//! a! A!))))
										 			(type (if (member op '(//! __!)) 'get 'set))
										 			]
											 ;;(dnl "WONKY " matrix "|"  matrix-args " :: "  type "(" op "), " (if is-A "A" "B"))
										 	 (cond
										 	  ((eq? type 'get)
										 		(if is-A
										 			 (if augmentation-A augmentation-A #f)
										 			 (if augmentation-B augmentation-B #f)))
											  ((eq? type 'set)
										 		(let* ((m-ish (if (null? (cdr matrix-args)) #f (cadr matrix-args)))
										 				 (m (cond
										 					  ((matrix? m-ish) m-ish)
										 					  ((matrix-list? m-ish) (matrix-list->matrix m-ish))
										 					  ((augmented-matrix-list? m-ish) (augmented-matrix-list->matrix m-ish))
										 					  ((eq? m-ish #f) #f)
										 					  (#t (error "bad argument passed to a set/get of an augmented matrix" matrix-args)))
										 					 ))
										 		  (if (and m (or (and is-A (= rows (m 'nr)))
										 							  (= columns (m 'nc))))
										 				(if is-A (set! augmentation-A m) (set! augmentation-B m))) ;; copied in the cond clause
										 		  matrix ;; NOTE: this does not copy matrix!  use (matrix-copy) to make an independent version
										 		  ))
										 	  (#t (error "bad mojo in matrix set/get (cond ....)"))))
										  ]

										 [#t (error (string-append "Unrecognised key, " (object->string (car matrix-args)) ", in arguments " (object->string matrix-args)))]
										 ) ;; end of the big cond
										)) ;; end of the inner letrec declarations -- R is the only thing declared
							 ;;(dnl "R = " R)
							 (set! RESULT R)
							 
							 (if (matrix? R)
								  (begin
									 (if (and (= N 4)
												 (member (cadr matrix-args)  '(// __ __! //!))
												 (member (caddr matrix-args) '(// __ __! //!)))
										  (apply R (cddr matrix-args))
										  R)
									 )) ;end of final (if (matrix? ...) ...)
							 ) ;; end of letrec 
						  ) ;; end of let
						RESULT
						);; end of the lambda
					 ] ;; end of "matrix"  [basically the "matrix"]
					) ;; end of the letrec declarations


		  ;; Body of the letrec [make-matrix]
		  (matrix? 'register! matrix) ;; register with the ... register.
		  
		  ;;(dnl "Making a matrix:" args)
		  ;; .... and  these are the arguments to make-matrix
		  
		  (cond ;;**NOTE** args === (cdr matrix-args)
			
			[(or (null? args) (eq? args '(()))) ;; make-matrix was called with no arguments
			 ;;(dnl "the void")
			 ;;(dnl 'null)
			 (if HELP-FOR-MAKE-MATRIX 
				  (display matrix-doco) 
				  (begin ;; return a degenerate matrix representation ;; this is currently the default
					 (set! M '())
					 (set! rows 0)
					 (set! columns 0)))
			 ]
			[(member (car args) '(help doco documentation))
			 (display matrix-doco)]

			[(matrix-list? (car args)) ;; ALWAYS COPIES THE LIST!
			 ;;Allow symbolic entries for the time being
			 (set! M (deep-copy (car args)))
			 (set! rows (length (car args)))
			 (set! columns (length (caar args)))
			 ]
			
			[(and (pair? args) (number? (car args)) (null? (cdr args)))
			 (car args)]
			[(and (pair? args) (posint? (car args)) (posint? (cadr args)) (pair? (cddr args)))
			 ;; (make-matrix 3 4 '( 1 2 3 4 10 11 12 15 34 33 32 21)) => ((1 2 3 4) (10 11 12 15) (34 33 32 31))
			 ;;(dnl "with lists and numbers")
			 (let ((r (cadr args))
					 (c (car args))
					 (x (caddr args))
					 )
				(cond
				 ((not (matrix-list? x))
				  (set! M (deep-copy (let loop ((newm '()) (m x))
											  (if (null? m)
													(reverse newm)
													(loop (cons (list-head m c) newm) (list-tail m c))))))
				  )
				 ((matrix-list? x)
				  (let* ((xr (length x))
							(xc (length (car x)))
							(nm (map (lambda (i) (make-list c 0)) (seq r)))
							)
					 ;;(dnl "i in [0, (min " xr " " r ")]")
					 ;;(dnl "j in [0, (min " xc " " c ")]")
					 (for-each
					  (lambda (i)
						 (for-each
						  (lambda (j)
							 ;;(dnl "setting (" i","j") in " nm)
							 ;;(dnl "x: " (list-ref x i))
							 ;;(dnl "nm: " (list-ref nm i))
							 
							 (if (and
									(<= 0 i) (< i xr) (< i r)
									(<= 0 j) (< j xc) (< j c))
								  (list-set*! nm (list i j) (list-ref* x (list i j)))))
						  (seq xc)))
					  (seq xr))
					 (make-matrix nm))
				  )
				 (#t (error "bad arguments for make-matrix with sized lists"))
				 ))
			 ]
			[else (abort 'make-matrix:aborted)])
		  matrix
		  ))
	  )
	 ))

(define matrix-list->matrix make-matrix) ;; a matrix-list defines *one* matrix without augmentation

;; (define (identity-matrix n) ;; we cannot just call this "identity" since it conflicts with sort.scm
;;   (set! the-id n)
;;   (if (and (integer? n) (positive? n))
;; 		(let ((I (deep-copy (make-list n (make-list n 0)))))
;; 		  (for-each (lambda (i) (list-set! (list-ref I i) i 1)) (seq n))
;; 		  (make-matrix I)
;; 		  )
;; 		(abort 'bad-dimensioning-in-identity n)))

(define (1-row l i)
  (map (lambda (x) (if (= x i) 1 0)) (seq l)))

;; Generates a matrix which has values of 1 down the diagonal, zero elsewhere
(define (identity-matrix n #!optional (m #f))
  (if (not m) (identity-matrix n n)
		(if (and (posint? n)(posint? m))
			 (make-matrix (map (lambda (x) (1-row m x)) (seq n)))
			 (error "inappropriate dimension for identity matrix"))))

;; Generates a matrix with zero elements
(define (zero-matrix n #!optional m)
  (if (not m) (zero-matrix n n)
		(if (and (integer? n)(positive? n))
			 (make-matrix (map (lambda (x) (make-list m 0)) (seq n)))
			 (error "inappropriate dimension for identity matrix"))))



;; (define (change-basis2 vect basis origin)
;;   (let ((n (length basis)))
;;     (if (null? origin) (set! origin (make-list (length basis) 0.0)))

;;     (if (<= n 2)
;;         (let* ((r (list-operator - basis origin))
;;                (s (list-operator - vect origin))
;;                (v (list-operator - s r))
;;                (theta '())
;;                )
;;           (if (= n 1)
;;               v
;;               (rotated-vector v (- 0 (atan (car r) (cadr r))))))
;;         'change-basis:too-many-dimensions)))


(define (change-basis-matrix b1 b2)
  "b1 and b2 are the sets of basis vectors, either using a list representation or as row ordered matrices"
  (cond
	((list? b1)	(change-basis-matrix (make-matrix b1) b2))
	((list? b2)	(change-basis-matrix b1 (make-matrix b2)))
	(#t 
	 (let ((B1 (make-matrix b1))
			 (B2 ((make-matrix b2) 't))
			 (Y (* (B1 'inv) B2))
			 )
		(lambda v
		  (if (not v)
				Y
				(let ((V (cond
							 ((and (list? v) (list? (car v)))
							  (make-matrix v) v)
							 ((list? v) (make-matrix (list v)))
							 (#t v))
							))
				  (* V Y))
				))))
	)
  )


;-- (change-transformation-basis-matrix N b1 b2) constructs a change of basis mapping
(define (change-transformation-basis-matrix N b1 b2)
  "for the transformation N, b1 and b2 are the sets of basis vectors associated with 
  the domain and codomain (either using a list representation or as row ordered 
  matrices)

  Help data needs updating too.

  Still needs testing
    Submatrices: (M (3 4 5) (1 2)) // is always a 'copy' operation
  "

  (* (b1 'inv) N b2)
  )


(define (sort-matrix< M)
  (let* ((M (matrix-copy M))
			(Ma (append-augmented-matrix-rows M))
			(Ms (Ma 'sort-rows<))
			)
	 (return-augmented-matrix-rows (M 'nc) Ms)))

(define (sort-matrix> M)
  (let ((M (matrix-copy M))
		  (Ma (append-augmented-matrix-rows M))
		  (Ms (Ma 'sort-rows<))
		  )
	 (return-augmented-matrix-rows (M 'nc) Ms)))

(define (gram-schmidt row-vectors) ;; row-vectors make up the matrix A
  ;; Ref https://en.wikipedia.org/wiki/Gram-Schmidt_process.html
  (let ((A (make-matrix row-vectors)))
	 (A 'dump)
	 (if (not (= (A 'nr) (A 'nc)))
		  (error "The number of vectors is not consistent with their dimension")
		  (let* (;(rrA (gaussian-row-reduction! ((* A (A ^T)) // A)))
					(AT (A ^T))
					(AAT (* A AT))
					(AATA (AAT //! A))
					(GSA (row-reduced-echelon-form (AATA)))
					)
			 (display "A:\n")(A 'dump)
			 (display "\nAA^T:\n")(AT 'dump)
			 (display "\nA^T:\n")(AAT 'dump)
			 (display "\nAA^T|A:\n")(AATA 'dump)
			 AATA))))




"The following body of code was previously the file 'gauss.scm'"

(define (map-func op N) (lambda (x) (op x N)))
(define (*n N) (map-func * N))
(define (/n N) (map-func / N))

;; > (map (lambda (v) (map (/n (car v)) v)) L*)
;; ((1 1/4 5/4 0) (1 3 1 1) (1 3/8 1/8 3/4))


(define L* '((4 1 5 0)
				 (1 3 1 1)
				 (8 3 1 6)))		  

;; (define (gaussian-reduction list-of-lists)
;;   (let ((N (length list-of-lists)))
;; 	 (let outerloop ((i 0)
;; 						  (L (deep-copy list-of-lists))
;; 						  )
;; 		(if (>= i N)
;; 			 L
;; 			 (let ((Ival (map (/n (list-ref* L (list i i))) (list-ref* L i))))
;; 				(list-set! L i Ival)
;; 				(let innerloop ((j (+ i 1)))
;; 				  (if (< j N)
;; 						(list-set! L j (map - (list-ref* L j)
;; 												  (map (*n (list-ref* L (list i j)))
;; 														 (list-ref* L i))))
;; 						(innerloop (+ j 1))
;; 						))
;; 				(outerloop (+ i 1) L)
;; 				)))
;; 			 ))


(define (gauss-jordan-reduction% LoL) ;; Row reduced echelon form
  "First run down the first column, eliminating variables, then step down into the next submatrix"
  (let ((lol (list-copy LoL))
		  (n (length LoL))
		  (m (length (car LoL))) ;; m should be (+ n 1) for an augmented matrix
		  )

	 (let i-loop ((i 0))
		(if (< i n)
			 (begin
				(list-set! lol i (map (/n (list-ref* lol (list i i))) (list-ref lol i)))
				(let ((mi (list-ref lol i)))
				  (let j-loop ((j (+ i 1)))
					 (if (< j n)
						  (begin
							 (list-set*! lol j
											 (map - (list-ref lol j)
													(map (*n (list-ref* lol (list j i))) mi)))
							 (j-loop (+ j 1)))
						  ))
				  )
				(if (< (+ 1 i) n)
					 (i-loop (+ i 1)))
				))
		lol)))


(define (gauss-jordan-reduction! M)
  (cond
	((matrix? M)
	 (let ((m (gauss-jordan-reduction% (M))))
		(M 'set-matrix! m))
	 )
	((list? M)
	 (gauss-jordan-reduction% M))
	(#t (error "gauss-jordan-reduction expects a matrix or a list of lists"))))

(define (gauss-jordan-reduction M)
  (let ((W (matrix-copy M)))
	 (gauss-jordan-reduction! W)))

(define (reverse-list-head lst n)
  (append (reverse (list-head lst n)) (list-tail lst n)))


(define (solve-linear-system M)
  
  (cond
	((list? M)
	 (let* ((process (lambda (y) (reverse (map (lambda (x) (reverse-list-head x (- (length x) 1))) y))))
;			  (Mgjr (process (((make-matrix (process ((M 'rref)))) 'rref))))
			  (Mgjr (process (gauss-jordan-reduction% (process (gauss-jordan-reduction% M)))))
			  )
		Mgjr
		)
	 )
	((matrix? M) (make-matrix (solve-linear-system (M))))
	(#t (error "solve-linear-system can only take matrices and LoLs"))
	))



;-- Useful matrices

;We need a class of transformation matrices:
;  rotation       DONE
;  translation
;  selective scaling
;  sheer
;  reflection
;  projection


;; > (define v ((make-matrix '((1 1 1 1))) ^t))
;; > (v)
;; ((1) (1) (1) (1))
;; > (* T v)
;; #<procedure #3 matrix>
;; > ((* T v))
;; ((5) (4) (3) (1))
;; > (T)
;; ((1 0 0 4) (0 1 0 3) (0 0 1 2) (0 0 0 1))




;; Notes on transformation matrices:
;; =================================
;; The call (transform mat lst) takes a transformation matrix, mat, and applies it to
;; the locus represented by lst, loci are typically represented as lists of numbers,

;; These really ought to be generalised for arbitrary numbers of dimensions.  I am
;; reasonably sure that four putative dimensions ought to be enough for immediate
;; work, and generalisation can happen later.


;; Translation between loci and matrices is working!

(define (locus->row-matrix locus)
  (if (apply m-andf (cons (list? locus) (map number? locus)))
		(make-matrix (list locus))
		(error "Bad locus in locus->row-matrix")))


(define (locus->col-matrix locus)
  (if (apply m-andf (cons (list? locus) (map number? locus)))
		(make-matrix locus)
		(error "Bad locus in locus->col-matrix")))

(define (locus->matrix  locus)
  (cond
	((matrix? locus) locus)
	((and (list? locus) (= 1 (length locus)))
			(locus->col-matrix locus))
	((and (list? locus) (< 1 (length locus)))
			(locus->row-matrix locus))
	(#t (error "Bad argument to locus->matrix"))))
	 

(define (col-matrix->locus m)
  (apply append (m)))

(define (row-matrix->locus m)
  (car (m)))

(define (matrix->locus m)
  (cond
	((list? m) m)
	((= (m 'nr) 1)
	 (row-matrix->locus m))
	((and (>= (m 'nc 1)) (apply = (cons 1 (map length (m)))))
	 (col-matrix->locus m))
	(#t
	 (error "matrix->locus called with a matrix which does not map to a vector"))))

;; (define (transform-locus T L) ;; L is a list of ordinates
;;   (if (list? L) (transform-locus T (locus->col-matrix L))
;; 		(if (if (or (not (matrix? L)) (> (L 'nc) 1))
;; 			 (error "Oops, you are trying to transform something that isn't a location (a column matrix)")
;; 			 (let* ((I (identity-matrix 5)) ;; 5 is the smallest size that will do all the r3 and r4
;; 					  (l (I 'embed L)) ;; 
;; 					  (T (* T l))
;; 					  (TL ((T 'extract (length L) 1)))
;; 					  )
;; 				(matrix->locus TL)))))

(define (transform-locus T L)
  ;; A locus can be a column matrix or a simple list
  (cond
	((list? L) (matrix->locus (transform-locus T (locus->col-matrix L))))
 	((and  (not (row-matrix? L)) (not (column-matrix? L)))
	 ;;(dnl "L: " L ", "(if (matrix? L) (L 'nr) (length L)))
	 (error "Oops, you are trying to transform something that isn't a location (a list or a column/row matrix)"))
	((column-matrix? L)
	 (let* ((l ((identity-matrix 5) 'embed! L))
			  (V* (* T l)))
		
		(V* 'extract (list (L 'nr) 1))
		))
	((row-matrix? L)
	 (let* ((l ((identity-matrix 5) 'embed! L))
			  (V* (* l T)))
		
		(V* 'extract (list 1 (L 'nc)))
		))
	))



(define (rotation-matrix-x alpha #!optional (dim 2))
  (case dim
	 ((2) (rotation-matrix alpha))
	 ((3) (rotation-matrix alpha 0))
	 ((4) (rotation-matrix alpha 0 0))
	 (else  (if #t (error "bad argument to rotation-matrix-?") (lambda (x) x)))))

(define (rotation-matrix-y beta #!optional (dim 3))
  (case dim
	 ((3) (rotation-matrix 0 beta))
	 ((4) (rotation-matrix 0 beta  0))
	 (else  (if #t (error "bad argument to rotation-matrix-?") (lambda (x) x)))))

(define (rotation-matrix-z gamma)
  (case dim
	 ((2) (rotation-matrix 0 0 alpha))
	 ((3) (rotation-matrix 0 0 alpha 0))
	 ((4) (rotation-matrix 0 0 alpha 0 0))
	 (else  (if #t (error "bad argument to rotation-matrix-?") (lambda (x) x)))))


(define (rotation-matrix alpha #!optional (beta #f) (gamma #f)) ;; needs to be 'one larger' than the vector
  (cond
	((not beta) ;; rotation in the plane 
	 (make-matrix (list (list (cos alpha) (- (sin alpha)))
							  (list (sin alpha) (cos alpha)))))
	((not gamma)
	 (* (rotation-x alpha) (rotation-y beta)))

	
	(#t ;; working in 3d
	 (let ((ca (cos alpha))
			 (sa (sin alpha))
			 (cb (cos beta))
			 (sb (sin beta))
			 (cg (cos gamma))
			 (sg (sin gamma))
			 )
		(make-matrix (list (list (* ca cb) (- (* ca sb sg) (* sa cg)) (+ (* ca sb cg) (* sa sg)))
								 (list (* sa cb) (+ (* sa sb sg) (* ca cg)) (- (* sa sb cg) (* ca sg)))
								 (list (- sb) (* cb sg) (* cb cg))))
		))
	(#t 'no-support-beyond-3d)
	))


;(define (intrisic-rotation alpha beta gamma)
;  (* (rotation-matrix-z alpha) (rotation-matrix-y beta) (rotation-matrix-x gamma)))

;(define (extrisic-rotation alpha beta gamma)
;  (* (rotation-matrix-z gamma) (rotation-matrix-y beta) (rotation-matrix-x alpha)))

;

(define (translation-matrix ords) ;; affine transformation matrix
  (let* ((N (length ords))
			(I ((identity-matrix (+ N 1))))
			)
	 (for-each
	  (lambda (i)
		 (list-set*! I (list i N) (list-ref ords i)))
	  (seq N))
	 (make-matrix I)
	 ))


;; scaling ... stretching, squeezing

(define (scale-matrix #!optional (alpha 1) (beta 1) (gamma 1))
  (make-matrix '`((,alpha 0 0)
						(0 ,beta 0)
						(0 0 ,gamma)
		)
))
		 
  


;; shearing
(define (stretch-matrix #!optional (sx 1) (sy 1) (sz 1) (st 1))
  (make-matrix (list (list sx 0 0 0)
							(list 0 sy 0 0)
							(list 0 0 sz 0)
							(list 0 0 0 st)
							)))

;; reflection




;; orthogonal projections




(define (rotation-matrix-x$ alpha) 
  (list-copy
	(list
	 (list 1 0 0)
	 (list 0 (cos alpha) (- (sin alpha)))
	 (list 0 (sin alpha) (cos alpha))
	 )
	))

(define (rotation-matrix-y$ beta)
  (list-copy
	(list
	 (list (cos beta) 0 (sin beta))
	 (list 0 1 0)
	 (list (- (sin beta)) 0 (cos beta))
	 )
	))

(define (rotation-matrix-z$ gamma)
  (list-copy
	(list
	 (list 0 (cos gamma) (- (sin gamma)))
	 (list 0 (sin gamma) (cos gamma))
	 (list 0 0 1)
	 )
	))

(define (translation-matrix$ #!optional (x 0) (y 0) (z 0))
  (list-copy 
	(if (list? x)
		 (apply translation-matrix$ x)
		 (list
		  (list 1 0 0 x)
		  (list 0 1 0 y)
		  (list 0 0 0 z)
		  (list 0 0 0 1)))
		))


;; For aircraft, travelling in the positive x direction with y to the
;; left and z up, roll is the rotation around the x-axiz, pitch is the
;; rotation around the y-axis and yaw is the rotation around the z-axis.
;; The resultant intrisic attitude matrix (relative to the aircrafts 
;; own coordinate system) in these terms is
;; R_z(alpha) R_y(beta) R_x(gamma)

;; or for the extrinsic (external basis)

;; R_z(gamma) R_y(beta) R_x(alpha)

;; and yes, order matters.




(define-macro (c-for i ival test inc . body)
  (let ((loopsym (string->symbol (string-append "c-for-loop-" (symbol->string i)))))
	 (display loopsym)(newline)
	 `(let ,loopsym ((i ,ival))
			 (if ,test
				  (begin
					 ,@body
					 (,loopsym ,inc))))))

    ;; (define (matrix-copy mat)
    ;;   (let* ((rows (vector-length mat))
    ;;          (result (make-vector rows)))
    ;;     (do ((row 0 (+ row 1)))
    ;;         ((= row rows) result)
    ;;       ($! result row (vector-copy ($ mat row))))))

(define AugMab '((1 0 0 // 1 1 1 1)(0 1 0 // 0 0 1 1) (0 0 1 // 1 0 1 0) __ (3 4 5) (6 2 2) (1 4 4)))
(define AugMa '((1 0 0 // 1 1 1 1)(0 1 0 // 0 0 1 1) (0 0 1 // 1 0 1 0)))
(define AugMb '((1 0 0)(0 1 0) (0 0 1) __ (3 4 5) (6 2 2) (1 4 4)))

(dnl "Define identity")
(define I (identity-matrix 3))
(dnl "Define M0")
(define M0 (make-matrix '((1 2 3 4 5)(4 2 1 0 4)(8 2 1 3 5)(1 0 1 0 1)))) ;; zero determinant
(dnl "Define M")
(define M (make-matrix '((1 2 2) (4 0 4) (2 3 5))))
(dnl "Define W")
(define W (make-matrix (M)))
(dnl "Define H")
(define H (make-matrix '((11 12 13)(21 22 23)(31 32 33))))
(dnl "Define A A0")
(define A (make-matrix '((2 1)(8 3)(6 5))))
(define A0 (make-matrix '((2 1)(8 3)(6 5)(1 3)(10 2))))
(dnl "Define B B0")
(define B (make-matrix '((5 4  1)(9 7 3)(0 6 5))))
(define B0 (make-matrix '((5 4 0 0 1)(9 2 8 7 3)(0 6 8 0 5)(1 1 0 1 1)(8 3 3 4 2))))

;(dnl 'W) 
;(define W (M ^inv))


(dnl "Define M2")
(define M2 (make-matrix '((1 2 0) (4 1 1) (0 1/2 7))))
(dnl "Define A2")
(define A2 (make-matrix '((8 2) (1 1) (0 4))))
(dnl "Define B2")
(define B2 (make-matrix '((1 2 3 4) (6 4 4 3) (8 3 6 5))))

(define MAB (matrix-copy M))
(MAB // A)
(MAB __ B)
(define M2AB (matrix-copy M2))
(M2AB // A2)
(M2AB __ B2)

  


;; basis matrix
;;(define b1 (make-matrix '((1 0 0) (0 1 0) (0 0 1))))

;; rotated basis matrix
;;(define b2 (make-matrix '((0 -1 0) (0 0 1) (1 0 0))))

;;(define v1 (make-matrix '((2 1 3))))
;;(define v2 (make-matrix '((2 1 2))))

;;(define bb (make-matrix '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
;;(define mm (M __ bb))

;;(define ww (mm // A))
;(M //! A)
;(M __! B)










			  
