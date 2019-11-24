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

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 


;;; Gambit-C specific -- used to suppress definitions of already defined
;;; routines (usually from postscript.scm)
;; This is mostly to support compilation and the avoidance of symbol clashes


(define-macro (define-unbound sig #!rest body)
  (let* ((name  (if (pair? sig) (car sig) sig))
			(ok (##unbound? (##global-var-ref (##make-global-var name))))
			)
	 (if ok `(define ,sig ,@body))))

(define (unbound? name)
  (##unbound? (##global-var-ref (##make-global-var name))))

(define (bound? name)
  (not (unbound? name)))


;(load "utils.scm")

;(load "constants.scm")

(define-unbound e (exp 1))
(define-unbound 1/e (/ 1 e))
(define-unbound pi (acos -1))
(define-unbound tau (* 2 pi))
(define-unbound sqrt2pi (sqrt (* 2 pi)))
(define-unbound e*2 (* 2 e))
(define-unbound e*-2 (* -2 e))
(define-unbound 1-1/e (- 1 (/ 1 e))) ;; ~ .6321

(define-unbound 2pi tau)
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


"There are two versions of these tesselating shapes; the first lot have the size related to
length, the second has it related to area."


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
			(apply andf (map list? L))
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

	  

  

(define (maths-make-list n . init)
  (if (<= n 0) 
		'()
		(if (null? init)
			 (cons '() (maths-make-list (- n 1)))
			 (cons (car init) (maths-make-list (- n 1) (car init))))))
		

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
  (integrate (lambda (x) (edf x y) 0 t)))


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
  (if (list? x) (map square x) (square x))
;  (if (list? x)
;      (map * x x)
;      (* x x))
  )


(define-macro (sum mn mx lmbda)
  `(apply + (map ,lmbda (map (lambda (x) (+ ,mn x)) (sequence (- ,(+ 1 mx) ,mn))))))

(define-macro (prod mn mx lmbda)
  `(apply * (map ,lmbda (map (lambda (x) (+ ,mn x)) (sequence (- ,(+ 1 mx) ,mn))))))


(define (norm a) ;; usual norm
  (if (number? a)
		(* a a)
		(apply + (map * a a))))

(define (p-norm a #!optional p) ;; a more general norm
  (if (not p)
		(norm a)
		(if (number? a)
			 (power a p)
			 (power (apply + (map (lambda (x) (power x p)) a)) (/ 1 p))

			 ))
  )

(define (v-length a) ;; general
  (if (number? a)
		(magnitude a)
		(if (plist? a) 
			 (sqrt (norm a))
			 'bad-argument)))

;;(define (distance u v)
;;  (cond
;;	((and (number? u) (number? v)) (magnitude (- u v)))
;;	((and (= (length u) (length v)) (apply andf (map number? (append u v))))
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
		 (letrec ((andf (lambda x (if (null? x) #t (and (car x) (apply andf (cdr x)))))))
			(and (list? p) (apply andf (map number? p))))))

(define (point-list? p)
  (and (pair? p) (pair? (car p)) (number? (caar p))
		 (letrec ((andf (lambda x (if (null? x) #t (and (car x) (apply andf (cdr x)))))))
			(apply andf (map point? p)))))

(define (n-point? n p)
  ;;(dnl* 'n-point? n p)
  (if (not (list? p)) (error "bad list of points" p))
  (and (number? n) (list? p) (= n (length p)) (point? p)))

(define (n-point-list? n p)
  ;;(dnl* 'n-point-list? n p)
  (letrec ((andf (lambda x (if (null? x) #t (and (car x) (apply andf (cdr x)))))))
	 (apply andf (number? n) (map (lambda (x) (n-point? n x)) p))))



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
  
;;; (define (distance-to-polygon r poly) ***
;;;   (define (d2p r poly) ***
;;; 	 (cond ***
;;; 	  ((and (pair? poly) (pair? (cdr poly))) ***
;;; 		(apply min (map (lambda (p q) (distance-to-segment r  ***
;;; 			  (distance-to-polygon r (cdr poly)))) ***
;;; 	  ((pair? poly) (distance r (car poly))) ***
;;; 	  (#t +inf.0))) ***

;;;   (d2p r  (append  poly (if (equal? (car poly) (car (reverse poly))) '() (list (car poly)))))) ***

;; This assumes that polygons are closed by an identical point at the start and end of the trace;
;; it also assumes that polygons are simple point-lists.
(define (polygon? p)
  (and (point-list? p) (eq? (car p) (car (reverse p)))))

;; This assumes that polygons are represented (minimally) by point-lists
;; it also assumes that polygons are simple point-lists.
(define (polygon%? p)
  (point-list? p))


(define (polygon-list? l)
  (and (list? l) (apply andf (map polygon? l))))

(define (polygon-list%? l)
  (and (list? l) (apply andf (map polygon%? l))))

(define (distance-to-polygon p poly)
  (if (eq? (car poly) (car (reverse poly))) ;; prune duplicate point
		(distance-to-polygon p (cdr poly))
		(apply min (map (lambda (a b)
								(distance-to-segment p (list a b)))
							 poly (rotate-list 'cw poly)))))


(define (polygon-centroid p #!optional l)
  (if (polygon? p) ;; then the initial and final point are equal -- should be a polygon%?
		(polygon-centroid (cdr p) l)
		(map / (apply map + p) (make-list (length (car p)) (length p)))))
		

;; from http://www.larcenists.org/Twobit/benchmarksAbout.html -- http://www.larcenists.org/Twobit/src/pnpoly.scm
;; With changes 
(define (pt-in-poly2 xp yp x y) 
  (let loop ((c #f) 
				 (i (- (length xp) 1)) 
				 (j 0))
    (if (< i 0)
      c
      (if (or (and (or (> (list-ref yp i) y)
                       (>= y (list-ref yp j)))
                   (or (> (list-ref yp j) y)
                       (>= y (list-ref yp i))))
              (>= x
                       (+ (list-ref xp i)
                               (/ (*
                                        (- (list-ref xp j)
                                                (list-ref xp i))
                                        (- y (list-ref yp i)))
                                       (- (list-ref yp j)
                                               (list-ref yp i))))))
        (loop c (- i 1) i)
        (loop (not c) (- i 1) i)))))


(define (point-in-polygon p poly)
  (if (or (null? p) (null? poly) (< (length poly) 3)) 
		#f
		
		(let ((ptail (car (reverse poly))))
		  (if (not (equal? (car poly) ptail))
				(set! poly (append poly (list (car poly)))))))

  (pt-in-poly2 (map car poly) (map cadr poly) (car p) (cadr p)))


(define (signed-polygon-area poly)
  (cond
	((or (not (pair? poly)) (< (length poly) 3))
		0+1i)
	((not (equal? (car poly) (car (reverse poly))))
	 0+2i) ;; not closed
	((not (equal? (car poly) (car (reverse poly))))
	 0+3i) ;; it is a line
	(#t
	 (let* ((n (length poly))
			  (s (seq (- n 1)))
			  (m (map (lambda (ix)
							;;(dnl* ix (+ ix 1))
							;;(dnl* (* (car (list-ref poly ix))(cadr (list-ref poly (+ ix 1)))))
							;;(dnl* (* (cadr (list-ref poly ix))(car (list-ref poly (+ ix 1)))) "\n")
							(- (* (car (list-ref poly ix))
									(cadr (list-ref poly (+ ix 1))))
								(* (cadr (list-ref poly ix))
									(car (list-ref poly (+ ix 1)))))
							)
						 s))
			  )
		(/ (apply + m) 2))
	 )
	))

(define (polygon-area poly)
  (abs (signed-polygon-area poly)))


(define (dot a b) ;; general
  (apply + (list-operator * a b)))


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
				 (andf (lambda x
						  (if (null? x)
								#t
								(and (car x) (apply andf (cdr x))))))
				 )
		(cond
		 ((apply andf (map number? (list mean stddev m M))) (apply simple-nrnd args)) ;; all numbers
		 ((not (list? mean)) (error "If a tuple is pass as an arg, the mean must be a tuple", args))
		 ((and (apply andf (map list? (list stddev m M))) (apply = (map length (list mean stddev m M))))
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
				 (andf (lambda x
						  (if (null? x)
								#t
								(and (car x) (apply andf (cdr x))))))
				 )
		(cond
		 ((apply andf (map number? (list mean stddev m M))) (apply simple-lnrnd args)) ;; all numbers
		 ((not (list? mean)) (error "If a tuple is pass as an arg, the mean must be a tuple", args))
		 ((and (apply andf (map list? (list stddev m M))) (apply = (map length (list mean stddev m M))))
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



; a and b are vectors ... usually used as (project-vector (list-op - s o) (list-op - t o))
(define (project a b) ;; general
  (/ (dot a b) (v-length b)))


 ;; general
(define (abeam a b scale) ; scale might be the distance covered  in a timestep
  (let* ((v (project a b))
			(w (/ v scale))
			)
	 v))


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



(define (extend-arith-op-to-funcs op) 
  (letrec ((andf (lambda x (if (null? x) #t (and (car x) (apply andf (cdr x)))))))
	 (lambda args
		(if (apply andf (map number? args)) ;; This way numbers work as they usually do
			 (apply op args)
			 (lambda (x)
				(apply op (map (lambda (f) (if (procedure? f) (f x) f)) args)))))))

;; Doubled so we don't get them accidentally
(define ++ (extend-arith-op-to-funcs +))
(define -- (extend-arith-op-to-funcs -))
(define ** (extend-arith-op-to-funcs *))
(define // (extend-arith-op-to-funcs /))


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
  (lambda (x)
	 (cond
	  ((symbol? x) ptlist)
		  
	  ((or (null? ptlist) (not (pair? ptlist)) (not (pair? (car ptlist))) (< x (caar ptlist)))
		0.0)
	  (else (let hunt ((p ptlist))
				 (cond
				  ((null? p) 0.0)
				  ((and (pair? (cdr p)) (< x (caadr p)))
					(let ((d (caar p))
							(D (caadr p))
							(n (cadar p))
							(N (cadadr p)))
					  (+ (* (/ (- N n) (- D d)) (- x d)) n)))
				  (#t (hunt (cdr p)))))))))

(define (inverse-pwl ptlist)
  (let ((n (- (length (car ptlist)) 1)))
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
		





;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";; " 
;;; comment-end: ""
;;; End: ***
