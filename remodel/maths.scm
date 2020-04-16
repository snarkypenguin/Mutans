;-  Identification and Changes

;--
;	maths.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2008.05.04
;		Location: localhost:/usr/home/gray/Study/playpen/maths.scm
;


"
    Copyright 2017 Randall Gray

    This file is part of Remodel.

    Remodel is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Remodel is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Remodel.  If not, see <http://www.gnu.org/licenses/>.
"

;-  Discussion 

;; Mostly only 2d vectors.  *Some* of the routines will go to higher dimensions,
;; but still a long way to go.


;-  Configuration stuff 

;-  Included files 
"No included files, but there are some which are loaded after macro definitions"

;-  Code 

;-- support conditional inclusion (exclude multiple definitions for specific things)

"Gambit-C specific -- used to suppress definitions of already defined
 routines (usually from postscript.scm)

 This is mostly to support compilation and the avoidance of symbol clashes

 NOTE -- define-unbound will only work in *this* file -- copy the macro into
 files you want to use this in.
"

(define-macro (define-unbound sig #!rest body)
  (let* ((name  (if (pair? sig) (car sig) sig))
			(ok (##unbound? (##global-var-ref (##make-global-var name))))
			)
	 (if ok `(define ,sig ,@body))))

(define (unbound? name)
  (##unbound? (##global-var-ref (##make-global-var name))))

(define (bound? name)
  (not (unbound? name)))

;; We make protected copies of the fundamental primitives here, so we can reset or 
;; call on them if we need to.

(define error (let ((err error))
					 (lambda x
						(if (null? x)
							 (err 'here)
							 (apply abort x)))))

;-- Allow us to recover primitive maths operators

;; The nest three definitions allow us to overload the basic maths operations, but
;; let us recover them if we need to.

(define-macro (reset-primitive-arithmetic-op op)
  `(set! ,op (primitive-arithmetic-ops ',op)))

(define primitive-arithmetic-ops
  (let ((+ +)(- -)(* *) (/ /))
	 (lambda x
		(cond
		 ((null? x) (list + - * /))
		 ((eq? (car x) '+) +)
		 ((eq? (car x) '-) -)
		 ((eq? (car x) '*) *)
		 ((eq? (car x) '/) /)
		 (#t (display "(primitive-arithmetic-ops ['+ | '- | '* | '/])\n") (void))))))

(define (reset-all-primitive-arithmetic-ops)
  (reset-primitive-arithmetic-op +)
  (reset-primitive-arithmetic-op -)
  (reset-primitive-arithmetic-op *)
  (reset-primitive-arithmetic-op /))


;-- load other maths libraries


(load "constants.scm")
(load "utils.scm")
(load "combinatorics.scm")

;; Return an s-expression which, when eval'd yields an equivalent 'func'
;;*** NOTE: This does _not_ capture any closure associated with func ***
(define (function->list f)
" This function returns information about a function such as arity, ability to respond to
  particular queries and other things ov that nature.


 a ... indicates zero or more items, sexp* indicates one or more s-expressions
 [a | b] indicates either form is acceptible

 (lambda a sym sexp*)
 (lambda (a ...) sexp*)
 (lambda (a ... . b) sexp*)
 (lambda  (a ... [#!optional {b | (b v)} ...]... [#key q] ... [ . restarg]) sexp*)
 (lambda  (a ... [#!optional {b | (b v)} ...]... [#key q] ... [#!rest restarg] ) sexp*)	

'restarg' is variable containing a list of all remaining arguments to the function
"				
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


;-- logical operators m-andf and m-orf that can be applied like (apply m-andf (map pred? lst))

(define (m-andf #!rest x)
  (if (null? x)
		#t
		(if (not x)
			 #f
			 (apply m-andf (cdr x)))))

(define (m-orf #!rest x)
  (if (null? x)
		#f
		(if x
			 #t
			 (apply m-orf (cdr x)))))


;-- type interrogation (type x) returns a symbol

(define (type x)
  (cond
	((boolean? x) 'boolean)
	((character? x) 'character)
	((string? x) 'string)
	((symbol? x) 'symbol)

	((integer? x) 'integer)
	((rational? x) 'rational)
	((real? x) 'real)
	((complex? x) 'complex)
	((integer? x) 'integer)
	
	((list? x) 'list)
	((vector? x) 'vector)
	((table? x) 'table)
	((thread? x) 'thread)
	((time? x) 'time)
	((port? x) 'port)
	((box? x) 'box)
	((void? x) 'void)
	((procedure? x) 'procedure)
	(#t 'unknown)
	))

;-- predicate indicating if the arguemnt is a valid locus (number or tuple)

(define (locus? x)
  (or (and (vector? x) (apply andf (map number? (vector->list x))))
		(and (list? x) (apply andf (map number? x)))
		(number? x)))

(define !optional (void))
(define !key (void))
(define !rest (void)) ;; we never seem to see #!rest after the code is eval'd -- always a dotted notation
(let* ((kwf (cadr (function->list (lambda (a #!optional o #!key k #!rest r) #t))))
		 )
  (set! !optional (cadr kwf))
  (set! !key (cadr (cddr kwf)))
  )

"The following are used in the function interrogation code"

(define m-kw-list (list !optional !key))
(define m-lambdas# '(lambda lambda#))
(define m-lambdas '(lambda))

(define (m-atom-or-sexp x) (or (atom? x) (list? x)))


(define (m-flense-arglist args)
  (let ((skip? (lambda (x) (or (equal? x (string->keyword "#!optional")) (equal? x '#!key) (equal? x '#!rest)))))
	 (let loop ((x args)
					(result '())
					)
		(cond
		 ((null? x) (reverse result))
		 ((and (symbol? x) (skip? x)) (loop '()  result))
		 ((and (symbol? x) (not (skip? x))) (loop '()  (cons x result)))
		 ((and (pair? x) (skip? (car x))) (loop (cdr x) result))
		 (#t (loop (cdr x) (cons (car x) result))))
		))
  )


(define-macro (lambda# args #!rest body)
  "lambda# and define# provide a basic 'arity' facility for functions -- I have no idea how it plays with 
   DSSSL style optional/key/rest arguments.  This is primarily for use in functions integrating over a domain.

   The form also profices for help text and comments  
  "
  (let* ((filtered-args (m-flense-arglist args))
			(arity (length filtered-args))
			(array->displaystring (lambda (x) (if (string? x) (string-append x "\n") (apply string-append (map (lambda (y) (string-append y "\n")) x))))))
	 (help "(f 'arity) returns the number of expected arguments\n")
	 (comment "")
	 )
  
  (if (null? filtered-args)
		`(lambda formal-argument
			(if (and (pair? formal-argument) (equal? (car formal-argument) 'function-arity)))
			,arity
			(begin ,@body))
		(let ((a (car args))
				(rgs (cdr args))
				)
		  `(lambda (,a #!optional ,@rgs) ;; This breaks the innate arity check!
			  (cond
				((equal? ,a 'arity) ,arity)
				((equal? ,a 'function-arity) ,arity)
				((equal? ,a 'help) (display (array->displaystring help)))
				((equal? ,a 'flush-help!) (set! help '()))
				((equal? ,a 'set-help!) (set! help (cons help ,rgs)))
				((equal? ,a 'comments) (display (array->displaystring comments)))
				((equal? ,a 'flush-help!) (set! comments '()))
				((equal? ,a 'set-comments!) (set! comments (cons comments ,rgs)))
				(#t ((lambda# ,args ,@body) ,a ,@rgs)))
			  )
		  ))
  )

(define-macro (define# formals #!rest body)
  `(define ,(car formals) (lambda# ,(cdr formals) ,@body)))

;;(define tst (lambda# (a b #!optional c) (if c (- (+ a b) c) (+ a b))))

(define-unbound e (exp 1))
(define-unbound 1/e (/ 1 e))
(define-unbound pi (acos -1))
(define-unbound -pi (- pi))
(define-unbound pi/2 (/ pi 2))
(define-unbound -pi/2 (/ pi -2))
(define-unbound pi/4 (/ pi 4))
(define-unbound -pi/4 (/ pi -4))
(define-unbound tau (* 2 pi))
(define-unbound -tau (* -2 pi))
(define-unbound sqrt2pi (sqrt (* 2 pi)))
(define-unbound e*2 (* 2 e))
(define-unbound e*-2 (* -2 e))
(define-unbound 1-1/e (- 1 (/ 1 e))) ;; ~ .6321

(define-unbound -2pi -tau)
(define-unbound pi*2 tau)
(define-unbound sqrt-tau (sqrt tau))
(define-unbound sqrt2pi sqrt-tau)
(define-unbound 100tau 628)
(define-unbound 10000tau 62832) ;; slightly large
(define-unbound 100pi 314)
(define-unbound 10000pi 31416) ;; slightly small


;; defined in postscript.scm
;; (define (adjust operator deviant pointlist)
;;   (if (pair? deviant)
;; 		(map (lambda (pt) (map (lambda (s o) (operator s o )) deviant pt)) pointlist)
;; 		(map (lambda (pt) (map (lambda (o) (operator deviant o )) pt)) pointlist)))

;; (define (scale-point-list k pointlist)
;;   (adjust * k pointlist))

;; (define (translate-point-list k pointlist)
;;   (adjust + k pointlist))


(define-unbound (point-rotator theta)
  (lambda (point)
	 (let ((A (cos theta))
			 (B (- (sin theta)))
			 (C (sin theta))
			 (D (cos theta)))
		(list (+ (* A (car point)) (* B (cadr point)))
				(+ (* C (car point)) (* D (cadr point)))))))

(define-unbound (rotate-point theta point) ((point-rotator theta) point))
(define-unbound (rotate-pointlist theta pointlist) (map (point-rotator theta) pointlist))


;; ;; return the cross product of two lists (state spaces)
;; (define (list-cross2 a b) 
;;   (apply append (map (lambda (x) (map (lambda (y) (list x y)) b)) a)))

;; ;; return the cross product of n lists (state spaces)
;; (define (list-cross . args) 
;;   (define (*cross2* a b) 
;; 	 (apply append (map (lambda (x) (map (lambda (y) (if (list? y) (cons x y) (list x y))) b)) a))) 
;;   (cond 
;; 	((not (list? args)) 'bad-argument) 
;; 	((null? args) '()) 
;; 	((= (length args) 1)  (car args))	
;; 	((= (length args) 2)	(apply *cross2* args))	
;; 	(#t (list-cross (car args) (apply list-cross (cdr args))))))






;; (lambda sym . x) is not a valid form of a function definition

(define (lambda-form-0 lst) ;; (lambda three 3)
  (and
	(list? lst)
	(= (length lst) 3)
	(member (car lst) m-kw-list)
	(symbol? (cadr lst))
	(m-atom-or-sexp (caddr lst))))

;;  (lambda (a ...) sexp*)
(define (lambda-form-1 lst)
  (and
	(>= (length lst) 3)
	(member (car lst) m-lambdas)
	(symbol? (cadr lst))
	(not (null? (cddr lst)))
	(apply m-andf (map m-atom-or-sexp (cddr lst)))))

(define (lambda-form-2 lst)
  (and
	(>= (length lst) 3)
	(null? (filter (lambda (x) (member x (cdr (cadr lst)))) m-kw-list)) ;; must not have #!optional #!keys or #!rest
	(or
	 (if (list? (cadr lst))
		  (and
			(list? (cadr lst))
			(let ((dlst (cadr lst)))
			  (and (symbol? (car dlst))
					 (or (null? (cdr dlst)) ;(define (a)
						  (apply m-andf (map symbol? (cdr dlst))) ; (define (a b...)
						  )
					 ))
			)
		  
		  (let ((dlst (cadr lst)))
			 (and (symbol? (car dlst))
					(let L ((dl (cadr dlst)))
					  (or (symbol? dl)
							(and (pair? dl) (symbol? (car dl))
								  (L (cdr dl))))))))
	 )
	(not (null? (cddr lst)))
	(apply m-andf (map m-atom-or-sexp (cddr lst))))
  )

"
 form 0: (lambda a b)
 form 1: (lambda (a ...) sexp*) 
 form 2: (lambda (a ... . b) sexp*)  
 form 3: (lambda  (a ... [#!optional {b | (b v)} ...]... [#key q] ... [ . restarg]) sexp*)

 #<procedure #nn name> for compiled functions or procedures
 " 

;(define (simple-arity func)
;  (let ((f (function->list func)))
;	 (if (
;	 (length (!filter (lambda (x) (member x m-kw-list)) (cadr f)))))



;; (define (function-interrogation query f)
;;   (letrec ((function->list (lambda (func)
;; 								  (let ((op (open-string))
;; 										  )
;; 									 (pp func op)
;; 									 (force-output op)
;; 									 (read op)
;; 									 )))
;; 			  ;; Define the keywords used in parameter lists
;; 			  (args #f)
;; 			  (m-andf (lambda (v) ;; define m-andf
;; 						 (cond
;; 						  ((null? v) #t)
;; 						  ((atom? v) #t)
;; 						  ((pair? (car v))
;; 							(and (car v) (apply m-andf (cadr v)))	)
;; 						  (#t #f))))
;; 			  )
;; 	 ;; First classify the character of the argument list
;; 	 (let* ((frep (function->list func))
;; 			  (arguments (cadr frep)) ;; This is the argument list, (car frep) is 'lambda/lambda*/...
;; 			  )
;; 		(if (symbol? arguments) ;; single (list-)variable for the whole call
;; 			 (begin
;; 			   (if (member arguments m-kw-list)
;; 					 (begin
;; 						(display "The function:\n")(pp f)
;; 						(display "takes an argument list in ")(display arguments)
;; 						(newline)
;; 						(display "but also expects ")
;; 						(display (car (member arguments m-kw-list)))
;; 						(display ".\nThis is an error\n")
;; 						(error "bad argument list"))
;; 					 (set! args arguments)))
;; 			 ;; process a list of arguments
;; 			 (let loop ((unprocessed arguments)
;; 							(phase 0)
;; 							(normal-args '())
;; 							(dot-args '())
;; 							(unambiguous-args '())
;; 							(optional-args '())
;; 							(rest-args '())
;; 							(key-args '())
;; 							)

;; 				(cond
;; 				 ((and (= phase 0)
;; 						 (pair? unprocessed)
;; 						 (symbol? (car unprocessed))
;; 						 (not (member (car unprocessed) m-kw-list))
;; 						 )
;; 				  (loop (cdr unprocessed)
;; 						  phase
;; 						  (append normal-args (list (car unprocessed)))
;; 							(dot-args '())
;; 							(unambiguous-args '())
;; 							(optional-args '())
;; 							(rest-args '())
;; 							(key-args '())
;; 							))
;; 				 			((and (= phase 0)		
;; 						 (symbol? unprocessed)
;; 						 (not (member unprocessed m-kw-list))
;; 						 )
;; 				  (loop (cdr unprocessed)
;; 						  phase
;; 						  (append normal-args (list (car unprocessed)))
;; 							(dot-args '())
;; 							(unambiguous-args '())
;; 							(optional-args '())
;; 							(rest-args '())
;; 							(key-args '())
;; 							))

;; )))))) ;; Delete this  line!

"These functions extend the arithmetic operators to functions and lists/vectors."


(define (list-ref1 lst x) (list-ref lst (- x 1)))
(define (list-set*!1! lst x v) (list-set*! lst (- x 1) v))

(define (vector-ref1 lst x) (vector-ref lst (- x 1)))
(define (vector-set1! lst x v) (vector-set! lst (- x 1) v))

;(define (array-ref1 lst ix) (array-ref lst (- x 1)))
;(define (array-set1! lst ix v) (array-set! lst (- x 1) v))


(define (*ref ob . ix)
  (apply (cond
			 ((list? ob) list-ref)
			 ((vector? ob) vector-ref)
;			  ((array? ob) array-ref)
;			  ((string? ob) string-ref)
			 (#t (error "ref can't handle the object " ob))
			 )
			(cons ob ix)))

(define (ref1 ob . ix)
  (apply (cond
			 ((list? ob) list-ref1)
			 ((vector? ob) vector-ref1)
;			  ((array? ob) array-ref1)
;			  ((string? ob) string-ref1)
			 (#t (error "ref can't handle the object " ob))
			 )
			(cons ob ix)))

(define (*set! ob . x)
  (apply (cond
			 ((list? ob) list-set*!)
			 ((vector? ob) vector-set!)
;			  ((array? ob) array-set!)
;			  ((string? ob) string-set!)
			 (#t (error "set! can't handle the object " ob))
			 )
			(cons ob x)))

(define (set1! ob . x)
;  (display "set1! [") (display ob) (display "] ") (pp (cons ob x))
  (apply (cond
			 ((list? ob) list-set*!1!)
			 ((vector? ob) vector-set1!)
;			  ((array? ob) array-set1!)
;			  ((string? ob) string-set1!)
			 (#t (error "set1! can't handle the object " ob))
			 )
			(cons ob x)))

(define (arith-extension-type op)
  ;; overloaded operators must include the numeric tower
  (case (op 3 5)
	 ((8) 'addition)
	 ((-2) 'subtraction)
	 ((15) 'multiplication)
	 ((3/5) 'division)
	 (else 'other)))


"I really need to build in the ability to deal with vectors and arrays in the 'extend-...' 
functions.  Probably the best way to do this is to make an extension wrapper (in much
the same way as these functions already extend others).
"



(define (extend-arith-op-to-funcs op) 
  (letrec ((comment "extended to function composition")
			  (m-andf (lambda x (if (null? x) #t (and (car x) (apply m-andf (cdr x)))))))
	 (lambda args
		(if (apply m-andf (map number? args)) ;; This way numbers work as they usually do
			 (apply op args)
			 (lambda (x)
				(apply op (map (lambda (f) (if (procedure? f) (f x) f)) args)))))))


;;; (define (extend-arith-op-to-funcs op) ;; does functions and numbers, ***
;;;   "This returns a function which corresponds to a constructed func using the four basic operations" ***

;;;   (letrec ((note-- "does functions, and numbers") ***
;;; 			  (supports-functions #f) ***
;;; 			  (m-andf (lambda x (if (null? x) #t (and (car x) (apply m-andf (cdr x)))))) ***
;;; 			  ) ***
;;; 	 (let ((F (if (eq? (arith-extension-type op) 'division) ***
;;; 					  (lambda (n d) ***
;;; 						 (cond ***
;;; 						  ((and (number? n) (number? d)) ***
;;; 							(op n d)) ***

;;; 						  ((and (procedure? n) (number? d)) ***
;;; 							(op (n x) d)) ***
;;; 						  ((and (procedure? d) (number? n)) ***
;;; 							(op x (d x))) ***
;;; 						  ((and (procedure? n) (procedure? d)) ***
;;; 							(op (n x) (d x))) ***

;;; 						  ((and (list? n) (list? d) (= (length n) (length d))) ***
;;; 							(map op n d)) ***
;;; 						  ((and (list? n) (number? d)) ***
;;; 							(map (lambda (N) (op N d)))) ***
;;; 						  ((and (list? d) (number? n)) ***
;;; 							(map (lambda (D) (op n D)))) ***
;;; 						  )) ***

;;; 					  (lambda args ***
;;; 						 (cond ***
;;; 						  ((apply m-andf (map number? args)) ;; This way numbers work as they usually do ***
;;; 							(apply op args)) ***
;;; 						  (else (lambda (x) ***
;;; 									 (apply op (map (lambda (f) (if (procedure? f) (f x) f)) args))))))) ***
;;; 				 )) ***
;;; 		F) ***
;;;   )) ***


(define (extend-arith-op-to-vectors op) ;; -- this does not allow mixing vectors and scalars!
  (if (eq? (arith-extension-type op) 'division)
		(letrec ((m-andf (lambda x (if (null? x) #t (and (car x) (apply m-andf (cdr x)))))))
		  (lambda args
			 (let ((comment "arithmetic  extension of '/' to vectors ")
					 (L! (map list? args))
					 (V! (map vector? args))
					 (S! (map number? args)))
				(cond
				 ((and (> (length args) 2) (not (apply m-andf (map number? (cdr args)))))
				  (error "bad argument to a division-like operation " args))
				 ((apply m-andf (map number? args)) ;; This way numbers work as they usually do
				  (apply op args))
				 ((and (apply m-andf (map list? args)) ;; This way vectors work as they usually do
						 (apply = (map length args))
						 (apply m-andf (map (lambda (v) (map number? v)) args))
						 )
				  (apply map op args))
				 ((apply m-andf (map procedure? args))
				  (apply op (map (lambda (f) (if (procedure? f) (f x) f)) args)))
				 )
				
				
				)
			 )
		  )

		(letrec ((m-andf (lambda x (if (null? x) #t (and (car x) (apply m-andf (cdr x)))))))
		  (lambda args
			 (let ((comment "arithmetic  extension of '*,+,-' to vectors "))
				(cond
				 ((apply m-andf (map number? args)) ;; This way numbers work as they usually do
				  (apply op args))
				 ((and (apply m-andf (map list? args)) ;; This way vectors work as they usually do
						 (apply = (map length args))
						 (apply m-andf (map (lambda (v) (map number? v)) args))
						 )
				  (apply map op args))
				 ((apply m-andf (map procedure? args))
				  (apply op (map (lambda (f) (if (procedure? f) (f x) f)) args)))
				 ))
			 )
		  )
		))

"The following version collects scalars, applies the function and then deals with vectors
Implicitly assumes that adding a scalar -- say k implies '(k k k....)
IT DOES NOT DO THE FUNCTIONS"

(define (extend-to-vector-multiplication op) ;; ops *must* all work with numbers, + - / and *
  (let ((kind (if (member (arith-extension-type op) '(multiplication division)) 1 0)))
	 (lambda args
		(let* ((comment "extend multiplicative operations to a mix of scalars and vectors")
				 (nargs (filter number? args))
				 (K (if (pair? nargs)
						  (apply op nargs)
						  kind))
				 (elements (filter (lambda (x) (not (number? x))) args)))
		  (let ((result
					(if (null? elements)
						 K
						 (map (lambda (vec)
								  (map (lambda (v)
											(op v K))
										 vec))
								elements))))
			 
			 (cond
			  ((or (number? result) (and (pair? result) (number? (car result))))
				result)
			  ((and (pair? result) (pair? (car result)) (null? (cdr result)))
				(car result))
			  (#t args)
			  )))
		)
	 )
  )


;; Abbreviated names
(define (extend-arith-op op)
  (extend-arith-op-to-vectors op))

;; (+ f g)(x) -> (f x) + (g x)
(define (extend-arith-op$ op)
  (extend-arith-op-to-funcs op))

(define (extend-arith-operator op)
  (extend-to-vector-multiplication (extend-arith-op op)))

;; This one does the lot -- vectors and functions
(define (extend-arith-operator$ op)
  (extend-to-vector-multiplication (extend-arith-op$ (extend-arith-op op))))

;(define (extend-arith-op op)
;  (extend-to-vector-multiplication (extend-arith-op-to-funcs (extend-arith-op-to-vectors op))))


;; Doubled so we don't get them accidentally
(define ++ (extend-arith-op-to-funcs +))
(define -- (extend-arith-op-to-funcs -))
(define ** (extend-arith-op-to-funcs *))
(define // (extend-arith-op-to-funcs /))

;; Tripled because they are slower than just with functions
(define +++ (extend-arith-op +))
(define --- (extend-arith-op -))
(define *** (extend-arith-operator$ *))
(define /// (extend-arith-operator$ /))


(define $+ (extend-arith-operator +));; as well as normal scalar/vector operations, this will shift a point ($+ '(3 4 5) 2) -> '(5 6 7) 
(define $- (extend-arith-operator -));; as well as normal scalar/vector operations, this will shift a point ($- '(3 4 5) 2) -> '(1 2 3)
;; and (- 2 '(3 4 5) 
(define $* (extend-arith-operator *))
(define $/ (extend-arith-operator /))


;;(define %default-denominators '(2 3 4 5 6 8 10 12 16))
(define %default-denominators #f)

(define (~zero? x)
  (<= (magnitude x) 1e-15))

;; For finding fractions with a limited set of denominators that are close to a value 

(define (denominator-list n)
  (cdr (map 1+ (seq n))))

(define (all-fractions-for n)
  (map (lambda (x) (/ x n)) (seq n)))

(define (all-fractions n)

  (let* ((d* (if (list? n)
					  n
					  (denominator-list n)))
			(L (let loop ((lst '())
							  (d d*)
							  )
				  (if (null? d)
						lst
						(loop (append lst (map (lambda (x) (/ x (car d))) (seq (car d))))
								(cdr d)))))
			)
	 L)) ;; I could filter zeros and duplicate values, but that would only take more time



(define (nearest-fraction% n #!optional largest-denominator)
  (if (not largest-denominator) (set! largest-denominator 8))
  (let* ((s (if (< n 0) -1 1))  ;; this can be positive or negative
			(w (* s (truncate n))) ;; this is positive
			(f (- (* s n) w))      ;; this is the (positive) fractional part
			(denominators (if (list? largest-denominator)
									largest-denominator
									(seq largest-denominator)))
			(all-candidates (all-fractions largest-denominator))
			)
	 
	 (list (* (inexact->exact s) (inexact->exact w))
			 (inexact->exact (let winnow ((L* all-candidates)
													(best-guess-so-far 'none)
													(closest +inf.0) ;; way out
													)
									 (if (null? L*)
										  best-guess-so-far
										  (let ((try (abs (- f (car L*)))))
											 (if (and (< try closest) )
												  (winnow (cdr L*) (car L*) try)
												  (winnow (cdr L*) best-guess-so-far closest))
											 )
										  )
									 )))
	 ))

(define (nearest-fraction n #!optional largest-denominator)
  (if (not largest-denominator) (set! largest-denominator 8))
  (apply + (nearest-fraction% n largest-denominator)))

(define (nearest-fraction-error n k #!optional largest-denominator)
  (if (<= k 0) (set! k .00001))
  (let ((~n (nearest-fraction n largest-denominator)))
	 (* (truncate (/ (abs (- n ~n)) k)) k)
	 ))

(define (improper->proper-string arg #!optional denominators)
  (if (not denominators)
		(set! denominators (if %default-denominators
									  %default-denominators
									  '(1 2 4 8 16))))
  (if (number? arg)
		(set! arg (nearest-fraction% arg denominators)))

  (let ((L arg))
	 (cond
	  ((number? L) (number->string L))
	  ((= (length L) 1)
		(number->string (car L)))

	  ((= (length L) 2)
		(if (zero? (cadr L))
			 (number->string (car L))
			 (string-append (number->string (car L))
								 " "
								 (number->string (cadr L)))))
	  (else (abort 'bad-argument L)))))

(define (improper->proper-latex-string arg #!optional denominators)
  (if (not denominators)
		(set! denominators (if %default-denominators
									  %default-denominators
									  '(1 2 4 8 16))))
  (if (number? arg)
		(set! arg (nearest-fraction% arg denominators)))

  (let ((L arg))
	 (cond
	  ((number? L) (string-append "$" (number->string L) "$"))
	  ((= (length L) 1)
		(string-append "$" (number->string (car L)) "$"))

	  ((= (length L) 2)
		(if (zero? (cadr L))
			 (string-append "$"(number->string (car L)) "$")
			 (string-append "$" (number->string (car L))
								 "\\frac{" (number->string (numerator (cadr L)))
								 "}{" (number->string (denominator (cadr L))) "}$")))

	  (else (abort 'bad-argument L)))))


"There are two versions of these tesselating shapes; the first lot have the size related to"




;; This is a hexagon inscribed in a circle of radius size, with a horizontal orientation
(define (hexagon Size)
  (let ((size (/ Size 2)))
	 (list `(,size ,0.)
			 (rotate-point (/ pi 3.) `(,size ,0.))
			 (rotate-point (* 2. (/ pi 3.)) `(,size ,0.))
			 (rotate-point pi `(,size ,0.))
			 (rotate-point (* 4. (/ pi 3.)) `(,size ,0.))
			 (rotate-point (* 5. (/ pi 3.)) `(,size ,0.))
			 `(,size ,0.))))

;; This is the upper half of a (hexagon size)
(define (trapezoid size)
  (let ((size (/ Size 2)))
	 (list `(,size ,0.)
			 (rotate-point (/ pi 3.) `(,size ,0.))
			 (rotate-point (* 2. (/ pi 3.)) `(,size ,0.))
			 (rotate-point pi `(,size ,0.))
			 `(,size ,0.))))

(define (quadrilateral size) ;; cannot call it a square, that is a std scm func
  (let* ((s (/ size 2))
			(-s (- s)))
	 (list (list s s) (list -s s) (list -s -s) (list s -s) (list s s))))

(define (triangle size)
  (let ((s (/ size 2))
		  (r3 (sqrt 3))
		  )
	 (list (list s (/ (- s) r3)) (list 0 (* s r3)) (list (- s) (/ (- s) r3)))
	 ))

;; This is a hexagon inscribed in a circle of radius size, with a horizontal orientation
;; All of these geometric shapes (apart from the trapezoid) have unit area.  At least I hope so ;-)

(define (hexarea s)
  (* 6 (eqtriarea s)))

(define (eqtriarea s)
  (* (sqrt 3/16) s s))

(define (sqarea s)
  (* s s))

(define unit-hexagon-scale (hexarea 1))
(define unit-trapezoid-scale (hexarea 1))
;; this remains tied to the hexagon so it will fit in tesselations
(define unit-square-scale 1)
(define unit-triangle-scale (eqtriarea 1))

(define (hexagon* size)
  (let ((size (/ size unit-hexagon-scale)))
	 (list `(,size ,0.)
			 (rotate-point (/ pi 3.) `(,size ,0.))
			 (rotate-point (* 2. (/ pi 3.)) `(,size ,0.))
			 (rotate-point pi `(,size ,0.))
			 (rotate-point (* 4. (/ pi 3.)) `(,size ,0.))
			 (rotate-point (* 5. (/ pi 3.)) `(,size ,0.))
			 `(,size ,0.))))

;; This is the upper half of a (hexagon size)
(define (trapezoid* size)
  (let ((size (/ size unit-trapezoid-scale)))
	 (list `(,size ,0.)
			 (rotate-point (/ pi 3.) `(,size ,0.))
			 (rotate-point (* 2. (/ pi 3.)) `(,size ,0.))
			 (rotate-point pi `(,size ,0.))
			 `(,size ,0.))))

(define (square* size)
  (let* ((s (sqrt size))
			(-s (- s)))
	 (list (list s s) (list -s s) (list -s -s) (list s -s) (list s s))))

(define (triangle* size)
  (let ((size (/ size unit-triangle-scale)))
	 (triangle (* size size (/ (sqrt 3) 4)))
	 ))



(define-unbound log10 ;; defined this way so it doesn't recalculate ln10
  (let ((ln10 (log 10)))
	 (lambda (x)
		(/ (log x) ln10))))


(define (normalise vec)
  (if (vector? vec)
		(list->vector (normalise (vector->list vec)))
		(let ((len (sqrt (apply + (map sqr vec)))))
		  (if (zero? len)
				+nan.0
				(map (lambda (x) (/ x len)) vec)))))

(define maths-dnl*
  (lambda X
	 (let ((out (if (and (pair? X) (port? (car (reverse X))))
						 (car (reverse X))
						 (current-output-port)))
			 (x (if (and (pair? X) (port? (car (reverse X))))
					  (reverse (cdr (reverse X)))
					  X)))
		
		(if (and (pair? x) (null? (cdr x)))
			 (display (car x) out)
			 (begin
				(display (car x) out)
				(for-each
				 (lambda (bit)
					(display " " out)
					(display bit out)
					)
				 (cdr x))))
		(newline out)
		)))

(define unit-box '((0 0)(1 0) (1 1) (0 1) (0 0)))
(define unit-box-points  '((0 0)(1 0) (1 1) (0 1)))

(define (bounding-box point-list) ;; returns bounding box in arbitrary dimensions
  (let* ((ords (list-transpose point-list))
			(m (map-apply min ords))
			(M (map-apply max ords))
			)
	 (map list m M)))

(define (random-point-in-box ll #!optional ur)
  (if (not ur)
		(cond
		 ((= (length ll) 4)	(random-point-in-box (car ll) (caddr ll)))
		 ((= (length ll) 5)	(random-point-in-box (car ll) (caddr ll)))
		 ((= (length ll) 2)	(random-point-in-box (car ll) (cadr ll)))
		 (else (error "whut?")))
		(map +  ll (map * (map (lambda (x) (random-real)) ll) (map - ur ll)))))


(define (random-bounded-point-in-box prop ll #!optional ur)
  ;; prop is the linear proportion around the centre of the domain to use
  (if (not ur)
		(random-bounded-point-in-box prop (car ll) (caddr ll))
		(let* ((n (length ur))
				 (c (map * (map + ur ll) (make-list n 1/2)))
				 (r (map - c ll))
				 (p (make-list n (* 1/2 r prop) ))
				 )
		  (map + c (map +/- (map (lambda (x) (* p (random-real))) c)))
		  )))


(define (+/- #!optional x) (if x
										 (if (zero? (random-integer 2)) (- x) x)
										 (if (zero? (random-integer 2)) - +)))

(define (find-preimage x f m M #!optional epsilon) ;;; requires a monotonic function f
  ;; f is a monotonic function, x is a value (f a), a \in \[m, M\]
  (if (not epsilon)
		(set! epsilon 1e-6))

  (let ((c (/ (+ m M) 2)))
	 (cond
	  ((< M m) (find-preimage x f M m epsilon)) ;;; requires a monotonic function f
	  ((< x (f m)) (dnl* "(find-preimage" x f m M ") off the end on the left" x (f m)))
	  ((> x (f M)) (dnl* "(find-preimage" x f m M ") off the end on the right" x (f M)))
	  ((and (<= (f m) x) (<= x (f M)) (< (abs (- M m)) epsilon))
		(/ (+ M m) 2))
	  ((<= (f c) x)
		(find-preimage x f c M))
	  ((<= x (f c))
		(find-preimage x f m c))
	  (#t (dnl*  "Oops, we have x =" x "m =" m "f(m) =" (f m) "c = " c "f(c) = " (f c) "M =" M "f(M) =" (f M))
			(error "Klein Landscape Error")))))

(define (seq n)
  (define (rseq n) (if (<= n 0) '() (cons (- n 1) (rseq (- n 1)))))
  (if (< n 0) (map - (rseq (- n))) (reverse (rseq n))))

(define (sequence n #!optional func)
  (define (seq k)
	 (if (zero? k) '() (cons k (seq (- k 1)))))
  (if (or (not (integer? n)) (negative? n))
		(error "Argument to sequence must be a non-negative integer" n)
		(if func
			 (map func (reverse (seq n)))
			 (reverse (seq n)))))

;; Apply to a partitioned list (plist)
(define (apply-to-partitioned-list op lst)
  (apply op (map (lambda (l) (apply op l)) lst)))


(define (partitioned-list? L)
  (let ((PK (partition-list)))
	 (if (not (list? L) #f)
		  (and
			(<= (length L) PK)
			(apply m-andf (map list? L))
			(apply = (cons PK (map length (reverse (cdr L)))))
			(<= (length (car (reverse L))) PK)))))

(define (partition-list #!optional lst n) ;; cuts a list into managable pieces or leaves it alone
  ;; N is the length of the whole list if known
  (let ((biggest 10)
;(biggest 8192)
		  )
	 (if (not (list? lst))
		  biggest
		  (let ((N (if (number? n) n (length lst))))
			 (cond
			  ((or (null? lst) (<= N 0)) '())
			  ((and (number? N) (<= N biggest)) lst)
			  (else
				(let ((H (list-head lst biggest))
						(T (list-tail lst biggest)))
				  (cons H (if (<= (length T) biggest)
								  (list T)
								  (partition-list T (if (> N biggest) (- N biggest) N)))
						  ))))))))


(define (mean lst #!optional pwr)
  (if (not (or (eq? pwr #f) (number? pwr))) (error 'evil-power-specified pwr))
  (let ((k (longlist))
		  (n (length lst))
		  (P (if pwr (lambda (x) (power x pwr)) (lambda (x) x)))
		  (b (if pwr (lambda (x) (power x (/ 1 pwr))) (lambda (x) x)))
		  )
	 (/ 
	  (if (not P) 
			(if (<= n k)
				 (apply + lst)
				 (apply + (map (lambda (l) (apply + l)) (partition-list lst))))
			(if (<= n k)
				 (apply + (map P lst))
				 (if (<= n k)
					  (apply + (map P lst))
					  (apply + (map (lambda (l) (apply + (map P l))) (partition-list lst))))))
	  n))
  )






;; These are identical to filter and !filter

(define (select selector lst)
  (if (pair? lst)
		(if (selector (car lst))
			 (cons (car lst) (select selector (cdr lst)))
			 (select selector (cdr lst)))
		lst))

;; (!select selector lst) -- returns a list of those elements which fail the selector
(define (!select selector lst)
  (select (lambda x (not (apply selector x))) lst))

;;# sigmoid: x is in [0,1], l governs how sharp the transition is and off shifts it to 
;;# one side or the other of the y axis.  Organised so that if l == 1 and off = 0.0
;;# the value of the function at -0.5 is ~0.002 
;;# the range is (0,1)
;;sgmd(x,l,off) = 1.0/(1.0+exp(-2*pi*l*(2*(x+(0.5 - off))-1.0)))
;;
;;# this is set up so that the inflection point is at x=0.5 with a range of (0,1) and
;;# an operational domain of [0,1], though in practice it isn't so constrained.
;;sigmoid(x) = sgmd(x,1.0,0.5)
;;
;;# This si like sigmoid but the range is (-1,1)
;;psigmoid(x,l,off) = (sgmd(x,l,off)-sgmd(0,l,off))/(sgmd(1,l,off)-sgmd(0,l,off))
;;
;;gsigmoid(x,M,l,off) = psigmoid(x/M, l, off/M)
;;
;;
;;invsigmoid(x) =  (log(x) - log(1-x))/(4 * pi) + 0.5

;; returns the elements with "even" indices
;; (evens '(a b c d)) => (a c)
(define (even-indices* lst)
  (map (lambda (x) (list-ref lst x)) (filter even? (sequence (length lst)))))

;; returns the elements with "odd" indices
;; (odds '(a b c d e) => (b d)
(define (odd-indices* lst)
  (map (lambda (x) (list-ref lst x)) (filter odd? (sequence (length lst)))))

;; These are faster, but not robust w.r.t. line length
(define (even-indices lst) (if (null? lst) '() (cons (car lst) (if (pair? (cdr lst)) (even-indices (cddr lst)) '()))))
(define (odd-indices lst) (if (null? lst) '() (if (pair? (cdr lst)) (even-indices (cdr lst)) '())))

(define (N<= n)
  (define (rsequence n) (if (<= n 0) '() (cons (- n 1) (rsequence (- n 1)))))
  (if (< n 0) (map - (rsequence (- n))) (reverse (rsequence n))))


(define (integer-divisors n)
  (select integer? (map (lambda (x) (/ n (+ x 1))) (reverse (N<= n)))))


;; Mapf creates a function which will use a function of one variable to
;; construct a function that takes a list of values  (or a list of lists of ... values)
;; which are all mapped as though they were the sole argument of the base function

(define mapf (letrec
					  ((mapf (lambda (f)
								  (lambda (x) 
									 (cond
									  ((not (list? x)) (f x))
									  ((not (list? (car x))) (map f x))
									  ((list? x) (map (mapf f) x))
									  (#t x))))))
					mapf))







(define (default-object? x) (equal? x (void)))

;; So we can load gauss.scm 
(define-macro (define* func-template . body)
  (let* ((fname (car func-template))
			(optionalix (map (lambda (x y) (cons x y)) (cdr func-template) (sequence (length (cdr func-template)))))
			(oix (assoc '#:optional optionalix))
			(normalargs (if oix (map car (list-head optionalix (cdr oix))) optionalix))
			(optarg (if oix (list-ref optionalix (+ (cdr oix) 1)) (void))))
	 (maths-dnl* 'Func func-template)
	 (maths-dnl* 'fname fname)
	 (maths-dnl* 'optionalix optionalix )
	 (maths-dnl* 'oix oix)
	 (maths-dnl* 'normalargs normalargs)
	 (maths-dnl* 'optarg optarg)

	 (let ((txt `(define (,fname ,@normalargs #!rest ,optarg)
						(if (null? ,optarg) (set! ,optarg (void)) (set! ,optarg (car ,optarg)))
						,@body)))
		(pp txt)
		txt)
	 )
  )



;; this is a function that returns a single sawtooth the right or left
;; side may stick at the peak if the interval goes to infinity on that
;; side
(define (triangle-function peak m M)
  (if (or (not (pair? peak)) (not (= (length peak) 2))) (error "peak should be an (x y) location of the max value" peak))
  (if (>= m M) (error "M <= m!" m M))
  (if (or (infinite? (car peak)) (infinite? (cadr peak))) (error "infinite peak!"))
  (let ((x (car peak))
		  (y (cadr peak)))
	 (cond
	  ((and (infinite? m) (infinite? M))
		(lambda (p) y))
	  ((infinite? m)
		(lambda (p) (cond
						 ((< p x) y)
						 ((< M p) 0)
						 (else (* y (- 1. (/ (- p x) (- M x))))))))
	  ((infinite? M)
		(lambda (p) (cond
						 ((< x p) y)
						 ((< p m) 0)
						 (else (* y (- 1. (/ (- x p) (- x m))))))))
	  (else
		(lambda (p) (cond
						 ((< p m) 0)
						 ((< M p) 0)
						 ((<= p x) (* y (- 1. (/ (- x p) (- x m)))))
						 ((<= x p) (* y (- 1. (/ (- p x) (- M x)))))))
		))))



; Simple integration over an interval in the reals.  *Should* check to make sure that
; we don't pass one of the infinities as a bound, but ...

;; Algorithm translated from Wikipedia: "Adaptive Simpson's Method" 30/04/2009

(define (integrate% f a b #!optional (eps  1e-12) (k 5))
  (let ((eps 1e-12)
		  (INTEGRATION-MAX-DEPTH k)
		  )
	 (letrec ((inner-adaptive-integrate (lambda (f a b eps estimate fa fc fb k)
													  (if (< b a)
															(- 0 (inner-adaptive-integrate f b a eps estimate fb fc fa k))
															(let* ((h (- b a))
																	 (c (/ (+ a b) 2))
																	 (d (/ (+ a c) 2))
																	 (e (/ (+ c b) 2))
																	 (fd (f d))
																	 (fe (f e))
																	 (left-estimate (* (/ h 12) (+ fa (* 4 fd) fc)))
																	 (right-estimate (* (/ h 12) (+ fc (* 4 fe) fb)))
																	 (inner-estimate (+ left-estimate right-estimate))
																	 ;;				 (delta (/ (- inner-estimate estimate) 15))
																	 )
															  (if (or (<= k 0) (<= (magnitude (- inner-estimate estimate)) (* 15 eps)))
																	(+ inner-estimate (/ (- inner-estimate estimate) 15))
																	(+ (inner-adaptive-integrate f a c (/ eps 2) left-estimate fa fd fc (- k 1))
																		(inner-adaptive-integrate f c b (/ eps 2) right-estimate fc fe fb (- k 1))
																		))))))

				 (adaptive-integrate (lambda (f a #!optional (b #f) (eps 1e-12)(k INTEGRATION-MAX-DEPTH))
											  (if (< b a)
													(- (adaptive-integrate f b a eps k))
													(let* ((c (/ (+ a b) 2))
															 (fa (f a))
															 (fc (f c))
															 (fb (f b))
															 (estimate (+ fa (* 4 fc) fb))
															 )
													  (- (inner-adaptive-integrate f a b eps estimate fa fc fb k) (f a))))))
				 )

		(if (< b a)
			 (- (adaptive-integrate f b a eps k))
			 (let* ((c (/ (+ a b) 2))
					  (fa (f a))
					  (fc (f c))
					  (fb (f b))
					  (estimate (+ fa (* 4 fc) fb))
					  )
				(- (inner-adaptive-integrate f a b eps estimate fa fc fb k) (f a))))
		)))



;;
;;(integrate function lowerbound upperbound tolerance #!optional maxdepth)
;;(integrate% function lowerbound upperbound tolerance ignored-stepsize #!optional maxdepth)
;;(integrate* function lowerbound upperbound tolerance stepsize #!optional maxdepth)





;; returns a decay function which has a coefficient of N and a decay lambda L
;; 
(define (exp-decay-func N L)
  (lambda (t) (* N (exp (* t (/ -1 L ))))))








;; (edf x) is an exponential decay function which is for most cases defined on [0,1]->[0,1].
;; By the time x = 1---the default value---(edf x) is less than 0.0009 
;; Increasing the value of y increases the mean and the value of edf at 1, and y\in(0,+inf.0)
;; This makes a reasonable PDF for things since both axes (essentially) have a range of one.

(define (1df t #!optional y)
  (let* ((b (- pi 3))
			)
	 (if y
		  (exp (* t (/ -1. b) (/ 1 y)))
		  (exp (* t (/ -1. b))))))

(define (1cdf t #!optional y)
  (let* ((b (- pi 3)))
	 (+ (if y
			  (* (/ -1 (* b y))
				  (exp (* t (/ -1. b) (/ 1 y))))
			  (* b
				  (exp (* t (/ -1. b)))
				  )
			  )
		 )))

(define (edf t #!optional y)
  (let ((q (1cdf 1.0 y)))
	 (* q (1df t y))))

(define (ecdf t #!optional y)
  (integrate% (lambda (x) (edf x y) 0 t)))


;;# general-sigmoid: x is in [0,1], lmb governs how sharp the transition is and phi shifts it to 
;;# one side or the other of the y axis.  Organised so that if l == 1 and phi = 0.0
;;# the value of the function at -0.5 is ~0.002 
;;# the range is (0,1)

(define (modulo-real n d)
  ;; Be wary here -- it plays poorly with signed numbers
  (let* ((m (/ n d))
			(M (truncate m))
			)
	 (- m M)))

;; This is the generating function
(define (general-sigmoid-f x lmb phi)
  (exp (* 2 tau lmb (- x phi))))

;; This is exp( 2*tau*lmb * (x -phi))

(define (general-sigmoid-g v)
  (/ v (+ 1 v)))

(define (general-sigmoid  x lmb phi) 
  (general-sigmoid-g (general-sigmoid-f x lmb phi)))

;; the sigmoid* things are set up for mapping [0,1]->[0,1]

(define (sigmoid* x #!optional lmb . stuff)
  (let* ((bs (lambda (x) (general-sigmoid x (if lmb lmb 1.0) 0.5)))
			(m (bs 0.0))
			(M (bs 1.0))
			(r (- M m)))
	 (max 0.0 (min 1.0 (/ (- (bs x) m) r)))))

(define (inverse-sigmoid* x #!optional lmb . stuff)
  (let* ((bs (lambda (x) (general-sigmoid x (if lmb lmb 1.0) 0.5)))
			(m (bs 0.0))
			(M (bs 1.0))
			(r (- M m))
			(X (+ m (* r x)))
			)
	 (cond 
	  ((>= X 1) 1)
	  ((<= X 0) 0)
	  (else (max 0.0 (min 1.0 (+ (/ (- (log X) (log (- 1 X))) (* 2 tau)) 0.5)))))
	 )
  )

(define (scaled-sigmoid x y) (/ 1.0 (+ 1.0  (exp (- (* x y))))))

;; (require 'charplot) ; from slib
;; (plot (lambda (p) (general-sigmoid p 0.04)) -100.0 100.0 200)

(define (psigmoid* x) (- (* 2.0 (sigmoid* x)) 1.0))

(define (inverse-sigmoid P lmb phi)
  (cond
	((<= P 0) 0)
	((<= P 1) (+ (/ (log P) (* 2 tau lmb)) phi))
	(else 1)))


;; given a "sigmoid" value which is assumed to pertain to x = 0, the value of the offset is returned
(define (inverse-phi P_0 lmb)
  (cond
	((<= P_0 0) 0)
	((>= P_0 1) 1)
	(else (/ (log (- (/ 1 P_0) 1)) (* 4 pi lmb)))))


(define (pow e x) ;; Quick and unsophisticated.
  (exp (* x (log e))))

(define (power b e) ;; Preserves exactness if possible
  (cond
	((< e 0) (/ 1 (power b (- e))))
	((= e 1) b)
	((zero? e) 1)
	((and (integer? e) (rational? b)) ;; This will keep them as exact numbers if they are exact
	 (cond
	  ((even? e) (power (* b b) (/ e 2)))
	  (#t (* b (power b (- e 1)))))
	 )
	(else (exp (* e (log b))))))


;	sourced from https://rosettacode.org/wiki/Gamma_function#Scheme
(define gamma-lanczos
  (let ((p '(676.5203681218851 -1259.1392167224028 771.32342877765313 
										 -176.61502916214059 12.507343278686905 -0.13857109526572012
										 9.9843695780195716e-6 1.5056327351493116e-7)))
    (lambda (x)
      (if (< x 0.5)
			 (/ pi (* (sin (* pi x)) (gamma-lanczos (- 1 x))))
			 (let* ((x2 (- x 1))
					  (t (+ x2 7 0.5))
					  (a (do ((ps p (cdr ps))
								 (idx 0 (+ 1 idx))
								 (res 0.99999999999980993 (+ res 
																	  (/ (car ps)
																		  (+ x2 idx 1)))))
								((null? ps) res))))
				(* (sqrt (* 2 pi)) (expt t (+ x2 0.5)) (exp (- t)) a)))))) ;

(define gamma gamma-lanczos)


(define (count n)
  (let ((f 0))
	 (map (lambda (x) (set! f (+ 1 f)) (- f 1)) (make-list n 0))))

(define (plist? a)
  (and (pair? a) (list? a)))

(define (sign a)
  (cond
	((> a 0.0) 1.0)
	((< a 0.0) -1.0)
	(#t 0.0)))


(define (sqr x) ;; general
  (cond
	((number? x) (* x x))
	((list? x)
	 (map sqr x))
	((vector? x)
	 (vector-map sqr x))
	(#t (error 'bad-ordinate))
	))

(define-macro (sum mn mx lmbda) ;; Sum_{mn}^{mx} lmbda(x).x
  `(apply + (map ,lmbda (map (lambda (x) (+ ,mn x)) (sequence (- ,(+ 1 mx) ,mn))))))

(define-macro (prod mn mx lmbda) ;; Prod_{mn}^{mx} lmbda(x).x
  `(apply * (map ,lmbda (map (lambda (x) (+ ,mn x)) (sequence (- ,(+ 1 mx) ,mn))))))

(define (inner-product a b)
  (dnl "Calculating inner product between " a " and " b)
  (if (and (number? a) (number? b))
		(* a b)
		(apply + (map * a b))))

(define (inner-product a b)
  (cond
	((and (number? a) (number? b)) (* a b))
	((and (list? a) (list? b) (apply m-andf (map real? (append a b))))
	 (apply + (map * a b)))
	((and (list? a) (list? b) (apply m-andf (map number? (append a b))))
	 (apply + (map * a (map conjugate b))))
	(#t (error "Not recognised values" a b))))

(define dot inner-product)

(define (cross-product v1 v2)
  (list (- (* (cadr v1) (caddr v2))
			  (* (caddr v1) (cadr v2)))
		  (- (* (caddr v1) (car v2))
			  (* (car v1) (caddr v2)))
		  (- (* (car v1) (cadr v2))
			  (* (cadr v1) (car v2)))))

(define (scalar-triple-product a b c)
  (inner-product a (cross-product b c)))
;; Its value is the determinant of the matrix whose columns are the Cartesian coordinates
;; of the three vectors. It is the signed volume of the parallelepiped defined by the
;; three vectors. 

(define (vector-triple-product a b c) ;; for completeness
  (cross-product a (cross-product b c)))

;; Mechanical work is the dot product of force and displacement vectors,
;; Power is the dot product of force and velocity. Also used to simplify other things. 

(define (angle-between u v)
  (cond
	((procedure? u) (angle-between (map - (u 0) (u 1)) v))
	((procedure? v) (angle-between u (map - (v 0) (v 1))))
	(else 
	 (let* ((N (length u))
			  (O (make-list N 0)))
		
		(if (not (= N (length v)))
			 (inconsistent-vectors-in-cross-product u v))
		(let ((cos-theta (real-part (/ (apply + (map * u v)) (* (distance O u) (distance O v)))))
				)
		  (if (= 1 cos-theta) ;; they are parallel
				0
				(real-part (acos cos-theta))))))
	))



; 
; -- this is the projection of a onto b ... often found as
;    (project-vector (list-op - s o) (list-op - t o)) -- we can
;    think of b as a "basis" vector
(define (project v b)
  ($* (/ (inner-product v b) (inner-product b b)) b)
;(inner-product v ($/ b (v-length b)))
  )

;; general
(define (abeam a b scale) ; scale might be the distance covered  in a timestep
  (let* ((v (project a b))
			(w (/ v scale))
			)
	 v))

(define (norm a) ;; usual norm
  (cond ((number? a)	(* a a))
		  ((list? a) (apply + (map * a a)))
		  ((vector? a) (vector-apply + (vector-map * a a)))
		  (#t (error "bad application of norm" a))))


(define (p-norm a #!optional p) ;; a more general norm
  (if (not p)
		(norm a)
		(if (number? a)
			 (power a p)
			 (power (apply + (map (lambda (x) (power x p)) a)) (/ 1 p))

			 ))
  )

(define (test-vectors #!optional (vlen 3) (nv #f))
  (if (not nv)
		(test-vectors vlen vlen)
		(begin
		  (if (not (= vlen nv)) (dnl "This does not form a basis"))
		  (map (lambda (x) (map  (lambda (x) (random-real)) (seq vlen))) (seq nv)))))



(define (gram-schmidt l . ist)
  (let ((M (cond
				((matrix? l) l)
				((and (matrix-list? l) (null? ist)) (make-matrix l))
				((and (list? l)
						(= (length l) (length (car ist)))
						(apply m-andf (map list? ist))
						(apply = (map length ist))
						)
				 (make-matrix (cons l ist)))
				(#t (error "gram-schmidt expects either a matrix or a list-of-lists"))
				))
		  )

	 (let* ((G (@ * M (M ^t)))
			  (g (G // M))
			  (B ((g //)))
			  )
		(map (lambda (v) (normalise v)) B)
		)))


(define basis-vectors gram-schmidt)

(define (v-length a) ;; general
  (if (number? a)
		(magnitude a)
		(if (plist? a) 
			 (sqrt (norm a))
			 'bad-argument)))

;;(define (distance u v)
;;  (cond
;;	((and (number? u) (number? v)) (magnitude (- u v)))
;;	((and (= (length u) (length v)) (apply m-andf (map number? (append u v))))
;;		(let ((sqr (lambda (x) (* x x))))
;;		  (sqrt (apply + (map sqr (map - u v))))))
;;	(else (aborts "distance: bad argument")))

(define (distance p q) ;; general
  (if (and (number? p) (number? q))
		(magnitude (- p q))
		(let ()
		  (if (number? p) (set! p (list p)))
		  (if (number? q) (set! q (list q)))

        (sqrt (norm (map - p q))))))
;;(sqrt (apply + (map sqr (map - p q)))))))

(define (distance* p q) ;; general
  (if (and (number? p) (number? q))
		(magnitude (- p q))
		(let ((pp (point-list? p))
				(pq (point-list? q)))
		  (cond
			((and (point? p) (point? q)) (distance p q))
			((and (point? p) pq) (map (lambda (x) (distance p x)) q))
			((and (point? q) pp) (map (lambda (x) (distance x q)) p))
			((and pp pq) (apply append (map (lambda (x) (distance* x q)))))
			(error 'I-have-some-bad-points)
			)
		  )))

(define (list-operator op p1 p2) ;; general
  (cond
	((and (number? p1) (number? p2))
	 (op p1 p2))
	((and (number? p1) (list? p2))
	 ;;(list-operator op (make-list (length p2) p1) p2	)
	 (map (lambda (x) (list-operator op p1 x)) p2)
	 )
	((and (list? p1) (list? p2) (= (length p1) (length p2)))
	 (map op p1 p2))
	((and (number? p2) (list? p1))
;(list-operator op p1 (make-list (length p1) p2))
	 (map (lambda (x) (list-operator op x p2)) p1)
	 )
	(else 
	 (maths-dnl* "list-operator is confused!")
	 (maths-dnl* "... p1 =" p1)
	 (maths-dnl* "... p2 =" p2)
	 #f)
	)
  )

(define (point? p)
  (and (pair? p) (number? (car p))
		 (letrec ((m-andf (lambda x (if (null? x) #t (and (car x) (apply m-andf (cdr x)))))))
			(and (list? p) (apply m-andf (map number? p))))))

(define (point-list? p)
  (and (pair? p) (pair? (car p)) (number? (caar p))
		 (letrec ((m-andf (lambda x (if (null? x) #t (and (car x) (apply m-andf (cdr x)))))))
			(apply m-andf (map point? p)))))

(define (n-point? n p)
  ;;(dnl* 'n-point? n p)
  (if (not (list? p)) (error "bad list of points" p))
  (and (number? n) (list? p) (= n (length p)) (point? p)))

(define (n-point-list? n p)
  ;;(dnl* 'n-point-list? n p)
  (letrec ((m-andf (lambda x (if (null? x) #t (and (car x) (apply m-andf (cdr x)))))))
	 (apply m-andf (number? n) (map (lambda (x) (n-point? n x)) p))))



(define (distance-to-segment r segment)
  (if (eq? (car segment) (cadr segment))
		(distance r (car segment))
		(if (and (n-point? 2 r) (n-point-list? 2 segment))
			 (let* ((rx (car r))
					  (ry (cadr r))
					  (p0 (car segment))
					  (p1 (cadr segment))
					  
					  (p0x (car p0))
					  (p0y (cadr p0))
					  (p1x (car p1))		
					  (p1y (cadr p1))

					  (px (- p1x p0x))	
					  (py (- p1y p0y))
					  (t (/ (+ (* px (- rx p0x)) (* py (- ry p0y))) (+ (* px px) (* py py))))
					  )
				
				(cond
				 ((<= t 0) (distance r p0))
				 ((>= t 1) (distance r p1))
				 (#t (distance r (list (+ p0x (* t px)) (+ p0y (* t py)))))))
			 (error "bad point or point list in distance-to-segment" r segment)
			 )))



(define (proportion v #!rest interval)
  (set! interval (cond
						((null? interval) (error "Missing interval in call to proportion"))
						((and (not (pair? (car interval))) (null? (cddr interval))) interval)
						((and (null? (cddar interval))) (car interval))
						(#t (error "bad arguments to (proportion ...)" v interval))))
  (/ (- v (car interval)) (- (cadr interval) (car interval))))



(define (urnd-int m M) (+ m (random-integer (- M m))))
(define (urnd-real m M) (+ m (* (- M m) (random-real))))

(define (simple-nrnd #!rest args)
  (let* ((N (length args))
			(mean (if (>= N 1) (car args) 0))
			(stddev (if (>= N 2) (cadr args) 1))
			(m (if (>= N 3) (caddr args) -inf.0))
			(M (if (>= N 4) (cadddr args) +inf.0))
			)

	 (let* ((n (+ (* (let* ((u (random-real))
									(v (random-real))
									(w (sqrt (* -2.0 (log u)))))
							 (* w (cos (* v tau))))
						  stddev) mean)))
		(if (and (< m n) (< n M)) ;; open interval!
			 n
			 (simple-nrnd mean stddev m M)))))

(define (nrnd #!rest args)
  (let* ((N (length args))
			(mean (if (>= N 1) (car args) 0))
			(stddev (if (>= N 2) (cadr args) 1))
			(m (if (>= N 3) (caddr args) -inf.0))
			(M (if (>= N 4) (cadddr args) +inf.0))
			(n (if (list? mean) (length mean) 0)))
	 (letrec ((orf (lambda x
						  (if (null? x)
								#f
								(or (car x) (apply orf (cdr x))))))
				 (m-andf (lambda x
							  (if (null? x)
									#t
									(and (car x) (apply m-andf (cdr x))))))
				 )
		(cond
		 ((apply m-andf (map number? (list mean stddev m M))) (apply simple-nrnd args)) ;; all numbers
		 ((not (list? mean)) (error "If a tuple is pass as an arg, the mean must be a tuple", args))
		 ((and (apply m-andf (map list? (list stddev m M))) (apply = (map length (list mean stddev m M))))
		  (map simple-nrnd mean stddev m M)) ;; all consistent
		 ((number? stddev) (nrnd mean (make-list n stddev) m M))
		 ((number? m) (nrnd mean stddev (make-list n m) M))
		 ((number? M) (nrnd mean stddev m (make-list n M)))
		 (#t (error "Bad arguments to nrnd*" args)))))
  )



;; NOTE: The mean and stddev are for the related *normal* rng
(define (simple-lnrnd #!rest args)
  (let* ((N (length args))
			(mean (if (>= N 1) (car args) 0))
			(stddev (if (>= N 2) (cadr args) 1))
			(m (if (>= N 3) (caddr args) -inf.0))
			(M (if (>= N 4) (cadddr args) +inf.0))
			)
	 (let ((n (exp (nrnd mean stddev))))
		(if (and (< m n) (< n M))
			 n
			 (lnrnd mean stddev m N)))))


(define (lnrnd #!rest args)
  (let* ((N (length args))
			(mean (if (>= N 1) (car args) 0))
			(stddev (if (>= N 2) (cadr args) 1))
			(m (if (>= N 3) (caddr args) -inf.0))
			(M (if (>= N 4) (cadddr args) +inf.0))
			(n (if (list? mean) (length mean) 0)))
	 (letrec ((orf (lambda x
						  (if (null? x)
								#f
								(or (car x) (apply orf (cdr x))))))
				 (m-andf (lambda x
							  (if (null? x)
									#t
									(and (car x) (apply m-andf (cdr x))))))
				 )
		(cond
		 ((apply m-andf (map number? (list mean stddev m M))) (apply simple-lnrnd args)) ;; all numbers
		 ((not (list? mean)) (error "If a tuple is pass as an arg, the mean must be a tuple", args))
		 ((and (apply m-andf (map list? (list stddev m M))) (apply = (map length (list mean stddev m M))))
		  (map simple-lnrnd mean stddev m M)) ;; all consistent
		 ((number? stddev) (lnrnd mean (make-list n stddev) m M))
		 ((number? m) (lnrnd mean stddev (make-list n m) M))
		 ((number? M) (lnrnd mean stddev m (make-list n M)))
		 (#t (error "Bad arguments to lnrnd" args)))))
  )

(define (logtrans mean y) ;; y \in [0,1]
  (* -1 mean (log (- 1 y))))

(define (invlogtrans mean y)
  (- 1 (exp (/ y (* -1 mean)))))


(define (make-pprocess meany #!optional clip)
  (let* ((halfway-point (lambda (m M) (inexact->exact (truncate (/ (+ (max  m M) (min m M)) 2)))))
			(mean #f)
			(debug #f)
			(init (lambda (imean)
					  (set! mean imean)
					  ))
			)
	 (if (not clip) (set! clip  +inf.0))
	 (letrec ((pprng% (lambda args
							  (if debug
									(maths-dnl* "Entering pprng% with: " args))
							  (cond
								((null? args)
								 (let ((clip (if clip (invlogtrans mean clip) clip)))
									(let loop-while-zero ((y (random-real)))
									  (if (or (zero? y) (and clip (>= y (abs clip))))
											(loop-while-zero (random-real))
											(* -1 mean (log (- 1 y)))
											))
									))
								
								((eq? (car args) 'debug) (set! debug #t))
								((eq? (car args) 'no-debug) (set! debug #f))
								((eq? (car args) 'toggle-debug) (set! debug (not debug)))
								((eq? (car args) 'mean) mean)
								((and (pair? (cdr args)) (eq? (car args) 'init)) (init (cadr args)))
								((integer? (car args)) (map (lambda (x) (pprng%)) (sequence (abs (car args)))))
								(#t (error "Inappropriate arguments" args))
								)
							  ))
				 )
		(pprng% 'init meany)
		pprng%)
	 ))


(define (simple-pprnd mean #!optional clip)
; clip = (* -1 mean (log (- 1 y))))
; (/ clip (* -1 mean) = (log (- 1 y))
; (exp (/ clip (* -1 mean))) = (- 1 y)
; (- (exp (/ clip (* -1 mean))) 1) = -y
; (- 1 (exp (/ clip (* -1 mean)))) = y

  (let ((clip (if clip (invlogtrans mean clip) clip)))
	 (let loop-while-zero ((y (random-real)))
		(if (or (zero? y) (and clip (>= y clip)))
			 (loop-while-zero (random-real))
			 (* -1 mean (log (- 1 y)))
			 ))
	 ))

(define (pprnd mean #!optional M)
  (let ((lnm (if (list? mean) (length mean) #f))
		  (lnM (cond ((list? M) (length M)) (M 0) (else #f))))
	 
	 (cond
	  ((and (eq? lnm #f) (not lnM)) ;; no max, only a single mean
		(simple-pprnd mean)
		)
	  ((and (number? lnm) (not lnM))
		(map pprnd mean))
	  ((and (eq? lnm #f) (number? M)) ;; open boundary
		(let loop-till-less ((p (simple-pprnd mean)))
		  (if (>= p M)
				(loop-till-less (simple-pprnd mean M))
				p)))
	  ((and lnm (number? M)) ;; list and a single max
		(map (lambda (mn) (pprnd mn M)) mean))
	  ((and lnm M (list M) (eq? (length mean) (length M)))
		(map (lambda (mn Mx) (pprnd mn Mx)) mean M)
		)
	  (else (error "Incompatible list lengths in pprnd" mean M))
	  ))
  )


(define (random-cluster pnt rnglist count)
  (let* ((p pnt)
			(n count)
			(rnglist rnglist)
			(p* (maths-make-list n p))
			(n* (map (lambda (x) (x n)) rnglist))
			)
	 (map (lambda (a b) (map + a b)) p* (apply map list n*))
	 ))

;; this is so we can use gauss.scm for normally distributed random numbers from scmutils
(define random random-real)

(define (random-angle)
  (* tau (- (* (random-real)) 1)))

(define (random-displacement #!optional d)
  (cond
	((not d)	(random-displacement 1.0))

	(else
	 (let ((theta (random-angle))
			 (d (random-real))
			 )
		(list (* d (cos theta)) (* d sin theta))))))

(define (rotated-velocity v theta)
  (rotated-vector v theta))

(define (rotated-vector V theta #!optional axis)
  (let ((isvec (vector? V))
		  (v (or (and (list? V) V) (and (vector? V) (vector->list V))))
		  (n (length V)))
	 (cond
	  ((eq? n 1) V)
	  ((eq? n 2)
		(let ((r (list (- (* (car v) (cos theta)) (* (cadr v) (sin theta)))
							(+ (* (cadr v) (cos theta)) (* (car v) (sin theta))))))
		  (if isvec (list->vector r) r)))
	  ;;   ((and (list? axis) (eq? n (length axis)))
	  ;;    ;; rotate v around the indicated axis as though it shared a root with v
	  ;;    )
	  ;;   ((and (list? axis) (eq? 2 (length axis))
	  ;;         (list? (car axis)) (list (cadr axis))
	  ;;         (eq? n (length (car axis))) 
	  ;;         (eq? n (length (cadr axis))))
	  ;;    ;; rotate v around the indicated axis as though its base is the origin
	  (#t 'rotated-vector:too-many-dimensions)
	  )
	 ))


;;; ;; Composition operator ***
;;; (define (o . funlist)  ;; general ***
;;;   (if (= (length funlist) 1) ***
;;;       (lambda x (apply (car funlist) x)) ***
;;;       (lambda x ((car funlist) (apply (apply o (cdr funlist)) x))))) ***

;; This is the function composition operator
(define (o f . g) (lambda (x) (if (null? g) (f x) (f ((apply o g) x)))))


(define (distance-to-linear-function p F)
  (if (cartesian-linear-function? F)
		(let* ((p1x (- (car p) 10))
				 (p2x (+ (car p) 10))
				 (p1y (F p1x)) (p2y (F p2x)))
		  (distance-to-segment p (list (list p1x p1y) (list p2x p2y))))
		(if (zero? (car (F 'ds)))
			 (abs (- (car p) (car (F 0))))
			 (distance-to-linear-function p (F 'cartesian-linear-function))
			 ))
  )


(define (linear-function? f)
  (or (parametric-linear-function? f)
		(cartesian-linear-function? f)))


(define (parametric-linear-function? f)
  (and (procedure? f) (eq? (f) 'parametric-linear-function)))

(define (p-linear-function l1 #!optional l2) ;; parametric implementation
  ;; if l1 is a pair of pairs, then it is a segment and l2 must be false
  ;; otherwise both l1 and l2 ought to be points
  (let ((n (if (list? l1) (length l1) #f)))
	 (cond
	  ((and l1 (not l2) (pair? (car l1)) (pair? (cadr l1)))
		(p-linear-function (car l1) (cadr l1)))
	  ((and l1 l2 (list? l2)
			  (apply m-andf (map number? (append l1 l2)))
			  (= n (length l2)))
		(let* ((s (distance l1 l2))
				 (o (list-copy l1))
				 (i (list-copy l2))
				 (ds (map - l2 l1) ) ;; this is the displacement vector
				 )
		  (let* ((L (lambda (v) (map + o (map * ds (make-list n v)))))
					)
			 (lambda (#!optional p)
				(cond
				 ((number? p) (L p))
				 ((not p) 'parametric-linear-function)
				 ((eq? p 'n)
				  n)
				 ((eq? p 'base-segment)
				  (list o i))
				 ((eq? p 'scale)
				  s)
				 ((eq? p 'ds)
				  ds)
				 ((eq? p 'slope)
				  (if (zero? (car ds))
						+inf.0
						(/ (cadr ds) (car ds))))
				 ((eq? p 'help)
				  (display "The symbols '(help n base-segment scale) and 'ds\n")
				  (display "return help text, the dimension, the points L(0) and L(1),\n")
				  (display "|L(1)-L(0)|, and [L(1)-L(0)]\n")
				  (display "'cartesian-linear-function will return a cartesian version (if possible)\n")
				  )
				 ((eq? p 'cartesian-linear-function)
				  (cond
					((zero? (car ds))	(no-cartesian-equivalent:vertical-line))
					((not (= (length l1) 2)) (no-cartesian-equivalent:n>2-nyi))
					(#t (let* ((m (/ (cadr ds) (car ds)))
								  (c (- (cadr l1) (* m (car l1)))))
							(c-linear-function m c))))
				  )
				 (#t 'parametric-linear-function)
				 )
				))
		  ))
	  ((and (not l2) (pair? l1) (= 2 (length l1)) (list? (car l1))(list? (cadr l1)))
		(p-linear-function (car l1) (cadr l1)))
	  (#t (bad-argument))
	  )
	 )
  )

(define (cartesian-linear-function? f)
  (and (procedure? f) (eq? (f) 'cartesian-linear-function)))

(define (c-linear-function m c) ;; cartesian implementation  (c-linear-function 3/4 6) y = 3/4 x + 6
  (let* ((m m)
			(c c)
			(n 2)
			(L (lambda (x) (+ (* m x) c)))
			(bs (list (list 0 (L 0)) (list 1 (L 1))))
			)
	 (lambda (#!optional x) 
		(cond
		 ((number? x) (L x))
		 ((not x) 'cartesian-linear-function)
		 ((eq? x 'slope) m)
		 ((eq? x 'y-intercept) c)
		 ((eq? x 'base-segment) bs)
		 ((eq? x 'scale) (distance bs))
		 ((eq? x 'n) n)
		 ((eq? x 'ds) bs)
		 ((eq? x 'parametric)
		  (apply p-linear-function bs))
		 ((eq? p 'help)
		  (display "The symbols '(help n base-segment scale) and 'ds\n")
		  (display "'parametric will return a parametric version\n")
		  )
		 (#t 'cartesian-linear-function)))
	 ))

(define (add-linear-functions f g)
  (if (and
		 (linear-function? f)
		 (linear-function? g)
		 (= (f 'n) (g 'n)))
		(let ((foi (f 'base-segment))
				(goi (g 'base-segment))
				)
		  (linear-function (map (lambda (p q) (map + p q)) foi goi))
		  )
		#f))

(define (subtract-linear-functions f g)
  (if (and
		 (linear-function? f)
		 (linear-function? g)
		 (= (f 'n) (g 'n)))
		(let ((foi (f 'base-segment))
				(goi (g 'base-segment))
				)
		  (linear-function (map (lambda (p q) (map - p q)) foi goi))
		  )
		#f))


(define (linear-function-intersection p q)
  ((linear-function? p) (linear-function-intersection (list (p 0) (p 1)) q))
  ((linear-function? q) (linear-function-intersection p (list (q 0) (q 1))))
  ((and (pair? p)(pair? q)
		  (pair? (car p)) (pair? (cadr p)) (null? (cddr p))
		  (pair? (car q)) (pair? (cadr q)) (null? (cddr q))
		  )
	
	
	)
  (#t 'nyi)
  )

(define (parallel? u v) ;; u and v are linear functions taking a scalar argument
  (cond
	((cartesian-linear-function? u) (parallel? (u 'parametric) v))
	((cartesian-linear-function? v) (parallel? u (v 'parametric)))
	(#t
	 (let ((u_o (u 0))
			 (u_i (u 1))
			 (v_o (v 0))
			 (v_i (v 1))
			 )
		(~zero? (angle-between (map - u_i u_o) (map - v_i v_o))))
	 ))
  )

(define (colinear?  u v) ;; u and v are linear functions taking a scalar argument
  (let* ((ub (u 'base-segment))
			(vb (v 'base-segment))
			(uo (car ub))
			(ui (cadr ub))
			(vo (car vb))
			(vi (cadr vb))
;(uovi (linear-function uo vi)) ;; we only need one cross comparison
			(voui (p-linear-function vo ui)))
	 (and (parallel? u v)
;(parallel? uovi u)
			(parallel? voui u)))) 

(define (change-basis vect basis origin)
  (let ((n (length basis)))
    (if (null? origin) (set! origin (make-list (length basis) 0.0)))
	 
    (if (<= n 2)
        (let* ((r (list-operator - basis origin))
               (s (list-operator - vect origin))
               (v (list-operator - s r))
               (theta '())
               )
          (if (= n 1)
              v
              (rotated-vector v (- 0 (atan (car r) (cadr r))))))
        'change-basis:too-many-dimensions)))


(define (mult2 x  y)  ;; general
  (cond
	((and (plist? x) (plist? y) (map * x y)))
	((plist? x) (map (lambda (p) (* p y)) x))
	((plist? y) (map (lambda (p) (* x p)) y))
	((and (number? x) (number? y)) (* x y))
	(#t (#f 'bad-argument))
	))

(define (mult x . y)  ;; general
  (cond
	((null? y) x)
	((= (length y) 1) (mult2 x (car y)))
	(#t (mult2 x (apply mult y)))))

(define (div x y) ;; general
  (cond
	((and (plist? x) (plist? y) (map / x y)))
	((plist? x) (map (lambda (p) (/ p y)) x))
	((plist? y) (map (lambda (p) (/ x p)) y))
	((and (number? x) (number? y)) (/ x y))
	(#t (#f 'bad-argument))
	))

(define (add2 x  y) ;; general
  (cond
	((and (plist? x) (plist? y) (map + x y)))
	((plist? x) (map (lambda (p) (+ p y)) x))
	((plist? y) (map (lambda (p) (+ x p)) y))
	((and (number? x) (number? y)) (+ x y))
	(#t (#f 'bad-argument))
	))

(define (add x . y) ;; general
  (cond
	((null? y) x)
	((= (length y) 1.0) (add2 x (car y)))
	(#t (add2 x (apply add y)))))

(define (sub2 x  y) ;; general
  (cond
	((and (plist? x) (plist? y) (map - x y)))
	((plist? x) (map (lambda (p) (- p y)) x))
	((plist? y) (map (lambda (p) (- x p)) y))
	((and (number? x) (number? y)) (- x y))
	(#t (#f 'bad-argument))
	))

(define (sub x . y) ;; general
  (cond
	((null? y) (sub2 0 x))
	((= (length y) 1.0) (sub2 x (car y)))
	(#t (sub2 x (apply add y)))))

(define (make-pprnd m) ;
  (let ((table (make-vector 1024 0.0))
		  (mean m)
		  (size 0)
		  )
    (if (not (number? m))
        'make-pprnd:m-really-needs-to-be-a-number

        (let loop ((i 0))
          (if (and (= size 0) (< i 1024))
              (begin
                (vector-set! table i (- 1.0 (exp (/ (* -1.0 i) mean))))
                (if (> (vector-ref table i) 0.9999) (begin (set! size (+ 1 i)) (vector-set! table size 1.0)))
                (loop (+ 1 i))))))
	 
    (lambda mode
      (cond
       ((and (plist? mode) (eqv? (car mode) 'mean))
        mean)
       ((and (plist? mode) (eqv? (car mode) 'size))
        size)
       (#t (let (
;		   			  (r (randomo:uniform))
                 (r (random-real))
                 )
             (let loop ((i 0))
               (if (null? mode)
                   (cond
                    ((>= i size) size)
                    ((<= r 0.0) 0)
                    ((< r (vector-ref table i))
                     i)
                    (#t
                     (loop (+ 1 i)) )
                    )
                   (cond
                    ((>= i size) 1.0)
                    ((<= r 0.0) 0)
                    ((< r (vector-ref table i))
                     (/ i size))
                    (#t
                     (loop (+ 1 i)) )
                    )
                   )
               )
             )
           )
       )
      )
    )
  )




(define (nearest-point-in-list point lst)
  (let loop ((got #f)
				 (lst lst)
				 (bestd +inf.0))
	 (cond
	  ((null? lst)
		got)
	  ((or (not got) (< (distance got (car lst))  bestd))
		(loop (car lst) (cdr lst) (distance (distance got (car lst)))))
	  (else (loop got (cdr lst) bestd)))
	 ))

(define (nearest-point-in-list% func point lst)
  ;; the points in the list ares obtained by applying a function to the elements in the list
  ;; the point is just a point
  (let loop ((got #f)
				 (lst lst)
				 (bestd +inf.0))
	 (let ((fgot got)
			 (flst (func (car lst))))
		(cond
		 ((null? lst)
		  got)
		 ((or (not fgot) (< (distance fgot flst)))
		  (loop (car lst) (cdr lst) (distance (distance fgot flst))))
		 (else (loop got (cdr lst) bestd)))
		)))

(define (nearest-point-in-list* pfunc point lst)
  ;; all the points are generated as the result of a function call
  (let loop ((got #f)
				 (lst lst)
				 (bestd +inf.0))
	 (let ((fgot (if got (pfunc got) #f))
			 (flst (pfunc (car lst))))
		(cond
		 ((null? lst)
		  got)
		 ((or (not fgot) (< (distance fgot flst)))
		  (loop (car lst) (cdr lst) (distance (distance fgot flst))))
		 (else (loop got (cdr lst) bestd)))
		)))


;; This returns a piecewise linear function of one argument which is zero outside its domain
(define (pwl ptlist)
  (let* ((ptlist (list-copy ptlist))
			(weight (apply + (map cadr ptlist)))
			(nptlist (map (lambda (x) (list (car x) (/ (cadr x) weight))) ptlist))
			(normflags '(n norm normalised normalized))
			(lb (caar ptlist))
			(rb (caar (reverse ptlist)))
			(Lb (apply min (map cadr ptlist)))
			(Ub (apply max (map cadr ptlist)))
			)
	 
	 (lambda (x #!optional flag)
		(cond
		 ((member x normflags) nptlist)
		 ((eq? x 'left-bound) lb)
		 ((eq? x 'right-bound) rb)
		 ((eq? x 'lower-bound) Lb)
		 ((eq? x 'upper-bound) Ub)
		 ((symbol? x) ptlist)
		 ((or (null? ptlist)
				(not (pair? ptlist))
				(not (pair? (car ptlist))) (< x (caar ptlist)))
		  0.0)
		 (else
		  (let hunt ((p (if (member flag normflags) nptlist ptlist))
						 )
			 (cond
			  ((null? p) 0.0)
			  ((and (pair? (cdr p)) (< x (caadr p)))
				(let ((d (caar p))
						(D (caadr p))
						(n (cadar p))
						(N (cadadr p)))
				  (+ (* (/ (- N n) (- D d)) (- x d)) n)))
			  (#t (hunt (cdr p)))))
		  )
		 ))
	 ))

(define (normalised-pwl pwlf)
  (lambda (x #!optional flag)
	 (pwlf x 'n)))

(define (inverse-pwl ptlist)
  (let* ((n (- (length (car ptlist)) 1)))
	 (let ((iptlist (map (lambda (x) (cons (car (reverse x)) (list-head x n))) ptlist)))
		(pwl iptlist))))

;; This is used by rk4-* ... it does traces through many dimensional spaces
(define (interpolate pwl-point-list x)
  (cond
	((null? pwl-point-list)  #f)
	((and (not (pair? pwl-point-list)) (not (pair? (car pwl-point-list)))) #f)
	((< 2 (length (car pwl-point-list)))
	 (map 
	  (lambda (y) 
		 (interpolate 
		  (map 
			(lambda (pt)
			  (list 
				(car pt) 
				(list-ref pt y)) 
			  )
			pwl-point-list)
		  x))
	  (map (lambda (x) (+ x 1)) (sequence (- (length (car pwl-point-list)) 1)))))
	((<= x (caar pwl-point-list)) (cadar pwl-point-list))
	((null? (cdr pwl-point-list)) (cadar pwl-point-list))
	((< x (caadr pwl-point-list)) 
	 (let* ((p1 (car pwl-point-list))
			  (p2 (cadr pwl-point-list))
			  (a (car p1))
			  (m (cadr p1))
			  (b (car p2))
			  (M (cadr p2))
			  (// (lambda (n d)
					  (let* ((t1 (/ n d))
								(t2 (* t1 d)))
						 (if (= n t2)
							  t1
							  (/ (* 1.0 n) (* 1.0 d))))))
			  )

		(if (< (magnitude (- b a)) 1e-80)
			 (/ (+ m M) 2.0)
			 (+ (* (// (- x a)
						  (- b a))
					 (- M m)
					 )
				 (* 1.0 m)
				 ))
		)
	 )
	(#t (interpolate (cdr pwl-point-list) x)))
  )


;### DOMAINS -- used with some of the integrate.scm calls


"A domain is a closure which may take a number, vector of numbers or
list of numbers and return the value 1 or 0 depending on whether the
locus passed is a member of the domain or not.  In principle, we can
consider all domains as a subset of a cartesian space with an
indicated (finite) dimension.  We define a domain with a call
like (define R (make-domain name type dim infunc)), where name is a
symbol, bbox is a list consisting of two elements which define the
extrema of the hypercube, and an indicator function.

The closure R responds to the symbols
                help -- prints help
                 dim -- returns the number of axes in the cartesian space
            c|centre -- returns the centre of the domain
            r|radius -- returns the radius of a hyperspheroid, or the inner and outer radii
                        where all points are on-or-in the outer radius, and no points within
                        the inner radius lie outside the domain
            v|volume -- returns the volume of the domain (or length / area)
            a|area
            l|length
 b|bbox|bounding-box -- returns opposing bounding corners of the hypercube
     ordinate -- returns either 1 or 0 to indicate presence or absence according to
                 infunc
or an ordinate in the space.  It is an error to pass an ordinate with the wrong dimensionality.

The functions passed to make-domain should be constructed using (define# (func ...)) or 
(define (lambda# ( ...) ...)) in order to be able to get the arity of
the functions.  In principle, it means that we ought to be able to pass them a list/vector of 
ordinates, or the ordinates themselves.


Domains are specified by the dimensionality of their elements and the 
type of the elements, so a domain might be specified like so:

  (make-domain 'unit-square real 2 (lambda# (x y) (and (<= 0 x) (<= 0 y)(<= x 1) (<= y 1))))

or 

  (define globe (make-hyperspheroid centre radius)) ;; note, the centre defines the dimensionality
  (define# (inside-sphere? globe point) (<= (globe 'radius) (distance point (globe 'centre))))

though

  (globe (list x y z)) serves the same purpose -- it returns #t if (x y z) is not in the exterior.

"
(define (do-help-text objecttype)
  (string-append
	"When called with no arguments, a " (symbol->string objecttype) " returns a list with its defining parameters\n"
	"when called with a locus, the expression evaluates to #t if the locus is in the domain, or\n"
	"#f otherwise. Arguments below return the indicated values\n"
	"\n"
	"help h ? ---- print help text\n"
	"type t ----  type\n"
	
	"centre c C ---- return the notional centre of the domain\n"
	"radius r ---- return radius\n"
	"bounding b ---- bounding box\n"
	"volume v V area a A l L length ---- return the measure appropriate for the domain\n"
	))

;; ** THIS DEPENDS ON THE ABILITY TO CAPTURE A PRETTY-PRINT OF A
;; FUNCTION We could side-step this by having the constructors return
;; a record/structure with an implicitly supplied predicate function,
;; but that would mean we'd need external "calls" to use the domain.
;; This will do for the moment ... I *am* still experimenting after all.






(define (domain? d) ;; this returns the type of a domain or #f
  (let ((defn (and (procedure? d) (function->list d))))
	 (if (and (not (null? defn)) (list? defn) (eq? (cadr defn) 'domain-query-or-locus))
		  (car (d))
		  #f)))


(define (integer->bits n #!optional (N 0))
  ;; N is the minimum number of bits to generate
  (if (zero? n)
		(make-list (max 1 N) 0)
		(let* ((L (map (lambda (x) (if (bit-set? x (abs n)) 1 0))
							(reverse (seq (integer-length n)))))
				 )
		  (set! L (append (make-list (max 0 (- N (length L))) 0) L))
		  (if (< n 0)
				(cons '- L)
				L)))
  )

(define (bits->integer blist)
  (let ((s (if (eq? (car blist) '-) -1 1)))
	 (if (negative? s)
		  (* s (bits->integer (cdr blist)))
		  (let loop ((S 0) ;; this is the ongoing sum
						 (T 1) ;; T is the power of two in question
						 (B (reverse blist))
						 )
			 (if (null? B)
				  (* S s)
				  (loop
					(+ S (if (= (car B) 1) T 0))
					(* T 2)
					(cdr B)))))))

(define (bit-patterns n) ;; in n bits
  (map (lambda (x) (integer->bits x n)) (seq (power 2 n)) ))



(define (all-combinations A B)
  (if (not (and (length A) (length B)))
		(error "Vectors A and B are not the same length in (all-combinations A B)" A B)
		(let* ((n (length A))
				 (selectors (bit-patterns n))
				 )
		  (map (lambda (i* a* b*)
					(map (lambda (i a b) (if (zero? i) a b)) i* a* b*))
				 selectors
				 (make-list (length selectors) A)
				 (make-list (length selectors) B))
		  
		  )))

(define (interval a #!optional b)
  (let ((I 
			(cond
			 ((and (number? a) (number? b)) (list (min a b) (max a b))) ;; separate values
			 ((not b) (and (list? a) (= (length a)) 2) (list (apply min a) (apply max a))) 
			 ((and (list? a) (= (length a) 1) (number? b)) (list (min (list a b))(max (list a b))))
			 (#t (error "bad specification for an interval, should be a list of two numbers or just two numbers" a b))
			 )
			))

	 (orthotope (car I) (cadr I))))


;; NOTE: the axes are aligned with the ordinates!
;; To change this, a rotation needs to be applied on input ordinates

"Note for orthotopes (hyperrectangles) the outer radius is half the distance between V0 and V1, and the inner radius is the  minimum distance over all faces to the centre.""Also note that a two dimensional hypercube is a rectangle, and a one dimensional cube is a line"

(define (orthotope #!optional (V0 '(0 0)) (V1 #f)) ;; This is a hyperrectangle defined by the opposing points V0 and V1,
  (if (and (list? V0) (= (length V0) 1) (not V1))
		(begin (set! V1 (cadr V0)) (set! V0 (car V0))))
  (if (member V0 '(help h ?))
		(display help-text)
		(begin
		  (if (not (or (and (number? V0) (number? V1))
							(and (list? V0) (list? V1) (= (length V0) (length V1)))))
				(error "orthotope extrema do not conform with each other" V0 V1))
		  
		  (let ((type 'orthotope)
				  (V0 (if (number? V0) V0 (map (lambda (x y) (min x y)) V0 V1)))
				  (V1 (if (number? V1) V1 (map (lambda (x y) (max x y)) V0 V1)))
				  (n (if (number? V0) 1 (length V0)))
				  (len #f)
;				  (area (* (power (*  n) 
				  (volume (if (number? V0) (magnitude (- V1 V0)) (apply * (map - V1 V0))))
				  (help-text (do-help-text 'orthotope))
				  (rotation #f)
				  )
			 (cond
			  ((and (number? V0) (number? V1))
				(lambda domain-query-or-locus
				  (cond
					((null? domain-query-or-locus) (list typename V0 V1))
					((and (= n 1) (number? (car domain-query-or-locus)))
					 (and (<= V0 domain-query-or-locus) (<= domain-query-or-locus V1)))
					((symbol? (car domain-query-or-locus))
					 (cond
					  ((member (car domain-query-or-locus) '(help h ?)) (dnl help-text))
					  ((member (car domain-query-or-locus) '(set-typename!)) (set! typename (cadr x)))
					  ((member (car domain-query-or-locus) '(type t)) type)
					  ((member (car domain-query-or-locus) '(bounding b bbox)) (list (min V0 V1) (max V0 V1)))
					  ((member (car domain-query-or-locus) '(radius r)) (* 1/2 (abs (- V1 V0))))
					  ((member (car domain-query-or-locus) '(volume v V)) (abs volume))
					  ((member (car domain-query-or-locus) '(centre c C)) (+ V0 (* 1/2 (abs (- V1 V0)))))
;				 ((and (list? domain-query-or-locus) (apply andf (map number? domain-query-or-locus))) (apply andf (map <= (map min V0 V1) domain-query-or-locus) (map <= domain-query-or-locus (map max V0 V1))))
;				 ((and (list? domain-query-or-locus) (null? (cdr domain-query-or-locus)) (apply andf (map number? (car domain-query-or-locus)))) (apply andf (map <= (map min V0 V1) (car domain-query-or-locus)) (map <= (car domain-query-or-locus) (map max V0 V1))))
					  ((member (car domain-query-or-locus) '(help h H))
						(dnl help-text)
						)
					  (#t (display "orthotope: bad symbol, try 'help\n"))
					  ))
					(#t (error "Bad argument to orthotope" domain-query-or-locus))
					))))
			 ((= (length V0) (length V1))
			  (lambda domain-query-or-locus
				 (cond
				  ((null? domain-query-or-locus) (list 'orthotope V0 V1))
				  ((and (pair? domain-query-or-locus) (symbol? (car domain-query-or-locus)))
					(let ((SYM (car domain-query-or-locus)))
					  (case SYM
						 ((type t)
						  type)
						 ((bounding-box bounds b bbox)
						  (if (list? V0)
								(list (map min V0 V1)
										(map max V0 V1))
								(list (min V0 V1) (max V0 V1))))
						 ((radius r)
						  (list (* 1/2 (apply min (map - V1 V0))) (sqrt (apply + (map sqr (map - V1 V0)))))
						  )
						 ;; ;; (0 1) x (2 3) -> {(0, 1) (2 1) (1 3) (0 3)} 
						 ;; ;; (0 1) x (2 3) -> {(0, 1) (2 1) (1 3) (0 3)} 

						 ((volume v V)
						  (apply * (map (lambda (vo vi) (magnitude (- vi vo))) V0 V1)))
						 ((area a A)
						  #f
						  )
						 ((length l L)
						  (sqrt (apply + (map sqr (map (lambda (vo vi) (magnitude (- vi vo))) V0 V1)))) ;; this is the longest line within the object
						  )
						 ((centre c C)
						  (map (lambda (o i) (/ (+ o i) 2)) V0 V1)
						  )
						 ((help h H)
						  (dnl help-text)
						  )
						 (else (display "orthotope: bad symbol, try 'help\n"))
						 ))
					)
				  ((and (= (length domain-query-or-locus) n) (apply andf (map number? domain-query-or-locus))) ;; ordinates have been passed in
					(apply andf
							 (map (lambda (o vo vi) ;; tests good to roughly 15 places past the dp
									  (and (<= (min vo vi) o)
											 (<= o (max vo vi))))
									domain-query-or-locus V0 V1)))
				  ((and (list? (car domain-query-or-locus)) ;; a point has been passed in 
						  (= (length (car domain-query-or-locus)) n)
						  (apply andf (map number? (car domain-query-or-locus)))
						  )
					(apply andf
							 (map (lambda (o vo vi) ;; tests good to roughly 15 places past the dp
									  (and (<= (min vo vi) o)
											 (<= o (max vo vi))))
									(car domain-query-or-locus) V0 V1)))
				  ((and (vector? (car domain-query-or-locus)) ;; a vector has been passed in
						  (= (vector-length (car domain-query-or-locus)) n)
						  (apply andf (map number? (vector->list (car domain-query-or-locus)))))
					(apply andf
							 (map (lambda (o vo vi) ;; tests good to roughly 15 places past the dp
									  (and (<= (min vo vi) o)
											 (<= o (max vo vi))))
									(vector->list (car domain-query-or-locus)) V0 V1))
					)
				  (#t (error "Bad argument to orthotope" domain-query-or-locus))
				  )))
			 (#t  (error "Vertices V0 and V1 do not have the same dimension"))
			 )
		  ))
  )



;; V_n = S_{n-1}/n ==> S_n = n+1 V_{n+1}

(define (hyperspheroid-volume radius #!optional N) ;;; I haven't proven the generalisation to a hyper-ellipse
  (cond
	((and (number? radius) (not N)) (* pi radius radius))
	((and (number? radius)(integer? N)) (hyperspheroid-volume (make-list N radius)))
	((and (list? radius) (apply andf (map number? radius)))
		(let ((k (/ N 2)))
		  (if (even? N)
				(* (/ (power pi k) (factorial k))(apply * radius)) ;; N is even
				(let ((k (- k 1/2)))
				  (/ (* 2 (factorial k) (power (* 4 pi) k) (apply * radius)) (factorial N)))
				))
		)
	(#t (error "bad arguements to hyperspheroid-volume"))))

(define (hyperspheroid-area N radius) ;;; I haven't proven the generalisation to a hyper-ellipse
  (let* ((Vn-1 (hyperspheroid-volume (- N 1) radius))
			(Sn (* 2 pi Vn-1))
			)
	 Sn))
		  
;; NOTE: the axes are aligned with the ordinates!
;; To change this, a rotation needs to be applied on input ordinates
"In principle, it would make more sense to construct a 'canonical' hyperspheroid and supply translation and rotation. This would make generating graphics easier, 
but I suspect it would make use in *modelling* more computationally complex for 
many heads.  I've only generalised to they 'hypers' so I can limit the number of 
geometric objects I have to code."


(define (hyperspheroid centre radius) ;; this goes from R^2 on...
  (let* ((typename 'hyperspheroid)
			(centre centre)
			(n (length centre))
			(radius radius)
			(len (apply max (map (lambda (x) (* 2 x) radius))))
			(area (hyperspheroid-area n radius))
			(volume (hyperspheroid-volume radius (length centre)))
			(V0 (map  (lambda (x) (- x radius)) centre))
			(V1 (map  (lambda (x) (+ x radius)) centre))
			(yeah-track #f)
			(objectname 'hyperspheroid)
			(help-text (do-help-text 'hyperspheroid))
			(trackem (if yeah-track
							 (lambda (txt) (dnl txt))
							 (lambda (txt) #t))
						)
			(rotation #f)
			)
	 (lambda domain-query-or-locus
		(let ((isinlineloc (locus? domain-query-or-locus))
				(isloc (and (not (null? domain-query-or-locus)) (list? domain-query-or-locus) (null? (cdr domain-query-or-locus)) (list? (car domain-query-or-locus)) (= (length (car domain-query-or-locus)) n)
								(locus? (car domain-query-or-locus)) (apply andf (map number? (car domain-query-or-locus))) )))
		  (cond
			((null? domain-query-or-locus) (list typename centre radius))
			((symbol? (car domain-query-or-locus))
			 (case (car domain-query-or-locus)
				((help h ?) (dnl help-text))
				((type t) type)
				((set-typename!) (set! typename (cadr V0)))
				((radius r R) radius)
				((centre c C) centre)
				((length l L) len)
				((area a A) area)
				((volume v V) volume)
				((bounding-box b bbox) (list (list-copy V0) (list-copy V1)))
				(else (display "pass nothing to self-identify, a locus for containment, 'r to get radius, 'c to get centre, b to get a bounding box\n"))
				))
			(isloc
			 (trackem "list locus")
			 (<= (sqrt (apply + (map magnitude (map - (car domain-query-or-locus) centre)))) radius))
			(isinlineloc
			 (trackem "ordinate locus" domain-query-or-locus centre 
						 (<= (sqrt (apply + (map magnitude (map -  domain-query-or-locus centre)))) radius)))
			((and (vector? (car domain-query-or-locus))
					(= (vector-length (car domain-query-or-locus)) n)
					(apply andf (map number? (vector->list (car domain-query-or-locus))))
					)
			 (trackem "vector locus")
			 (<= (sqrt 1 (vector-map - domain-query-or-locus (if (vector? centre) centre (list->vector centre)))) radius))

			(#t (error "bad argument or arguments" x))
			)))
  ))


(define (domain type #!optional (bbox #f) (infunc #f))
  "NOTE: the elements of bbox must have the same dimensionality as the argument to infunc.
"  

  (let ((type type)
		  (bbox (if bbox (list (map (lambda (x y) (min x y)) (car bbox) (cadr bbox)) (map (lambda (x y) (max x y)) (car bbox) (cadr bbox))) #f))
		  (infunc infunc)
		  (help-text (do-help-text 'general-domain))
		  )

	 (cond
	  ((equal? type 'hyperspheroid)
		(hyperspheroid (car bbox) (cadr bbox)))
	  
	  ((equal? type 'orthotope)
		(orthotope (car bbox) (cadr bbox)))

	  ((and bbox
			  (or (and (number? (car bbox))
						  (number? (cadr bbox)))
					(and (pair? (car bbox))
						  (pair? (cadr bbox))
						  (apply = (map length bbox))))
			  )
		(lambda domain-query-or-locus
		  (cond
			((null? domain-query-or-locus) (list type bbox))
			((symbol? (car domain-query-or-locus))
			 (cond
			  ((eq? (car domain-query-or-locus) 'type) type)
			  ((member (car domain-query-or-locus) '(bounding-box b bbox)) bbox)
			  ((member (car domain-query-or-locus) '(radius r))
				(list (* 1/2 (apply min (map - (cadr bbox) (car bbox)))) (sqrt (apply + (map sqr (map - (cadr bbox) (car bbox))))))
				)

			  ((member (car domain-query-or-locus) '(volume v V)) (apply * (map (lambda (vo vi) (magnitude (- vi vo))) (car bbox) (cadr bbox))))
			  ((member (car domain-query-or-locus) '(centre c C)) (map (lambda (vo vi) (/ (+ vo vi) 2)) (car bbox) (cadr bbox)))
;				 ((and (list? domain-query-or-locus) (apply andf (map number? domain-query-or-locus))) (apply andf (map <= (map min V0 V1) domain-query-or-locus) (map <= domain-query-or-locus (map max V0 V1))))
;				 ((and (list? domain-query-or-locus) (null? (cdr domain-query-or-locus)) (apply andf (map number? (car domain-query-or-locus)))) (apply andf (map <= (map min V0 V1) (car domain-query-or-locus)) (map <= (car domain-query-or-locus) (map max V0 V1))))
			  ((member (car domain-query-or-locus) '(help h H))
				(dnl help-text)
				)
			  (#t (display "domain: bad symbol, try 'help\n"))
			  ))
			((and (list? (car domain-query-or-locus))
					(= (length (car domain-query-or-locus)) n)
					(apply andf (map number? (car domain-query-or-locus)))
					)
			 (apply andf
					  (map (lambda (o vo vi) ;; tests good to roughly 15 places past the dp
								(and (<= (min vo vi) o)
									  (<= o (max vo vi))))
							 (car domain-query-or-locus) (car bbox) (cadr bbox))))
			((and (= (length domain-query-or-locus) n) (apply andf (map number? domain-query-or-locus)))
			 (apply andf
					  (map (lambda (o vo vi) ;; tests good to roughly 15 places past the dp
								(and (<= (min vo vi) o)
									  (<= o (max vo vi))))
							 domain-query-or-locus (car bbox) (cadr bbox))))
			(#t (error "Bad argument to " 'general-domain  domain-query-or-locus))
			))
		)
	  (error "Vertices in the bounding box do not have the same dimension")
	  )
	 ))

;; Extension projects:

(define (domain* . subdomains) ;; This will be a domain comprised of a set of (possibly disjoint) subdomains
  "This is tricky -- getting things like radii and centroids will be awkward."
  (error 'nyi)
  )

;; and 



(define (domain% dom func-or-symbol #!optional arg)
  "This can take a function whose first argument is either 'fwd or 'inv followed by an ordinate;
   a symbol indicating an operation to be performed (such as 'bbox, or 'centre)
   
   This is tricky -- getting things like radii and centroids will be awkward."
  (error 'nyi)

  "domain% will wrap a domain or domain* with either a rotation or translation and 
   automatically remap 'global' ordinates to the local system and back as appropriate."
  )






;;   (if (infunc 'arity)
;;   ;; First work out what we are dealing with
;;   (cond
;; 	((and (pair? bbox) (number? (car bbox))) ;; simple range
	 
;; 	 )
;; 	((and (pair? bbox) (pair? (car bbox))) ;; vectors
;; 	 )	  


;;   (if (not (and (symbol? name)
;; 					 (list? bbox) (= (length bbox) 2)
;; 					 (procedure? infunc)))
;; 		(error 'domain:bad-arguments)
;; 		(let* ((N (domain-element-dimensions (car bbox)))
;; 				 (In infunc)
;; 				 (name name))
;; 		  (lambda args

;; 			 (apply infunc args) (

		



;; (define (dim:domain e)
;;   (cond
;; 	((domain? e)

;; 	((number? e) 1)
;; 	((list? e) (length e))
;; 	((vector? e) (vector-length e))
;; 	(#t #f)))






















   ;;   This is a coarse approximation of the distance covered over time for a piecewise trajectory
	;; 
   ;;   If we assume a speed of v, a directional variability == 1 (true random walk) N steps will get us sqrt(N)vt away on average.
	;;   With directional variability == 0 we get a (necessarily straight-line) distance of Nvt, this is an upper bound.  
	;;   Playing to the law of averages and grossly oversimplifying, 
	;;   we will say that r = (directional_variability * sqrt(N) + (1 - directional_variability) * N) * v * t
	;; 

	;; This is the effective radius for the given directional variability

;; (define (directed-stagger* max-angle nominal-dt )
;;   (lambda  (spd dir var dt)
;; 	 (let* ((N (/ dt nominal-dt))
;; 			  (rnd (nrnd 0 (pow (/ 1 sqrt2pi) (- N))))
;; 			  (r (* spd dt (+ (* var (sqrt N)) (* N (- 1 var)))))
;; 			  (theta (/ (* max-angle var rnd) N))
;; 			  )
;; 		(dnl* N rnd r theta)
;; 		(if (zero? N) (abort))
;; 		(list r (* spd dt) (rotated-vector dir theta)))))
		



(load "matrix.scm")


;-  The End 

;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***


