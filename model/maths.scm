;-  Identification and Changes

;--
;	maths.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2008.05.04
;		Location: localhost:/usr/home/gray/Study/playpen/maths.scm
;
;-  Discussion 

;; Mostly only 2d vectors.  *Some* of the routines will go to higher dimensions,
;; but still a long way to go.


;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

;(load "utils.scm")

;(load "constants.scm")

(define pi (* 4 (atan 1)))
(define e (exp 1))


(define log10 ;; defined this way so it doesn't recalculate ln10
  (let ((ln10 (log 10)))
	 (lambda (x)
		(/ (log x) ln10))))


(define (normalise vec)
  (let ((len (sqrt (apply + (map sqr vec)))))
	 (if (zero? len)
		  +nan.0
		  (map (lambda (x) (/ x len)) vec))))

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
  (exp (* 4 pi lmb (- x phi))))

;; This is exp( 4*pi*lmb * (x -phi))

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
	(else (max 0.0 (min 1.0 (+ (/ (- (log X) (log (- 1 X))) (* 4 pi)) 0.5)))))
  )
)

(define (scaled-sigmoid x y) (/ 1.0 (+ 1.0  (exp (- (* x y))))))

;; (require 'charplot) ; from slib
;; (plot (lambda (p) (general-sigmoid p 0.04)) -100.0 100.0 200)

(define (psigmoid* x) (- (* 2.0 (sigmoid* x)) 1.0))

(define (inverse-sigmoid P lmb phi)
  (cond
	((<= P 0) 0)
	((<= P 1) (+ (/ (log P) (* 4 pi lmb)) phi))
	(else 1)))


;; given a "sigmoid" value which is assumed to pertain to x = 0, the value of the offset is returned
(define (inverse-phi P_0 lmb)
  (cond
	((<= P_0 0) 0)
	((>= P_0 1) 1)
	(else (/ (log (- (/ 1 P_0) 1)) (* 4 pi lmb)))))

(define (power b e)
  (cond
	((< e 0) (/ 1 (power b (- e))))
	((zero? e) 1)

	((and (integer? e) (rational? b)) ;; This will keep them as exact numbers if they are exact
	 (cond
	  ((even? e) (power (* b b) (/ e 2)))
	  (#t (* b (power b (- e 1)))))
	 )
	(else (exp (* e (log b))))))


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
  (if (list? x)
      (map * x x)
      (* x x)))

(define-macro (sum mn mx lmbda)
  `(apply + (map ,lmbda (map (lambda (x) (+ ,mn x)) (sequence (- ,(+ 1 mx) ,mn))))))

(define-macro (prod mn mx lmbda)
  `(apply * (map ,lmbda (map (lambda (x) (+ ,mn x)) (sequence (- ,(+ 1 mx) ,mn))))))



(define (norm a) ;; general
  (if (number? a)
		(* a a)
		(apply + (map * a a))))

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
  (letrec ((andf (lambda x (if (null? #t) (and (car x) (apply andf (cdr x)))))))
	 (and (list? p) (apply andf (map number? p)))))

(define (point-list? p)
  (letrec ((andf (lambda x (if (null? #t) (and (car x) (apply andf (cdr x)))))))
	 (apply andf (map point? p))))

(define (n-point? n p)
  ;;(dnl* 'n-point? n p)
  (if (not (list? p)) (error "bad list of points" p))
  (and (number? n) (list? p) (= n (length p)) (point? p)))

(define (n-point-list? n p)
  ;;(dnl* 'n-point-list? n p)
  (letrec ((andf (lambda x (if (null? #t) (and (car x) (apply andf (cdr x)))))))
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


(define (distance-to-polygon p poly)
  (if (eq? (car poly) (car (reverse poly))) ;; prune duplicate point
		(distance-to-polygon p (cdr poly))
		(apply min (map (lambda (a b)
								(distance-to-segment p (list a b)))
							 poly (rotate-list 'cw poly)))))

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

	 (let* ((n (+ (* (let* ((pi*2 (* pi 2))
									  (u (random-real))
									  (v (random-real))
									  (w (sqrt (* -2.0 (log u)))))
								(* w (cos (* v pi*2))))
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
  (* pi (- (* (random-real) 2.0) 1)))

(define (rotated-velocity v theta)
  (rotated-vector v theta))

(define (rotated-vector V theta #!optional axis)
  (let ((isvec (vector? V))
		  (v (or (and (list? V) V) (and (vector? V) (vector->list V))))
		  (n (length V)))
  (cond
   ((= n 1) V)
   ((= n 2)
	 (let ((r (list (- (* (car v) (cos theta)) (* (cadr v) (sin theta)))
						 (+ (* (cadr v) (cos theta)) (* (car v) (sin theta))))))
		(if isvec (list->vector v) v)))
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



(define (pow e x)
  (exp (* x (log e))))

(define (extend-arith-op-to-funcs op) 
  (letrec ((andf (lambda x (if (null? #t) (and (car x) (apply andf (cdr x)))))))
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
(define (interpolate pwl-pointlist x)
  (cond
	((null? pwl-pointlist)  #f)
	((and (not (pair? pwl-pointlist)) (not (pair? (car pwl-pointlist)))) #f)
	((< 2 (length (car pwl-pointlist)))
	 (map 
	  (lambda (y) 
		 (interpolate 
		  (map 
			(lambda (pt)
			  (list 
				(car pt) 
				(list-ref pt y)) 
			  )
			pwl-pointlist)
		  x))
	  (map (lambda (x) (+ x 1)) (sequence (- (length (car pwl-pointlist)) 1)))))
	((<= x (caar pwl-pointlist)) (cadar pwl-pointlist))
	((null? (cdr pwl-pointlist)) (cadar pwl-pointlist))
	((< x (caadr pwl-pointlist)) 
	 (let* ((p1 (car pwl-pointlist))
			  (p2 (cadr pwl-pointlist))
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
  (#t (interpolate (cdr pwl-pointlist) x)))
)







;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";; " 
;;; comment-end: ""
;;; End: ***

