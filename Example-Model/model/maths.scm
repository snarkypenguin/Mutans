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

(define (default-object? x) (equal? x (void)))

;; So we can load gauss.scm
(define-macro (define* func-template . body)
  (let* ((fname (car func-template))
			(optionalix (map (lambda (x y) (cons x y)) (cdr func-template) (seq (length (cdr func-template)))))
			(oix (assoc '#:optional optionalix))
			(normalargs (if oix (map car (list-head optionalix (cdr oix))) optionalix))
			(optarg (if oix (list-ref optionalix (+ (cdr oix) 1)) (void))))
	 (dnl* 'Func func-template)
	 (dnl* 'fname fname)
	 (dnl* 'optionalix optionalix )
	 (dnl* 'oix oix)
	 (dnl* 'normalargs normalargs)
	 (dnl* 'optarg optarg)

	 (let ((txt `(define (,fname ,@normalargs #!rest ,optarg)
						(if (null? ,optarg) (set! ,optarg (void)) (set! ,optarg (car ,optarg)))
						,@body)))
		(pp txt)
		txt)
	 )
)


(define-macro (pi) `,(* 4 (atan 1)))

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
  (exp (* 4 (pi) lmb (- x phi))))

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
	(else (max 0.0 (min 1.0 (+ (/ (- (log X) (log (- 1 X))) (* 4 (pi))) 0.5)))))
  )
)

(define (scaled-sigmoid x y) (/ 1.0 (+ 1.0  (exp (- (* x y))))))

;; (require 'charplot) ; from slib
;; (plot (lambda (p) (general-sigmoid p 0.04)) -100.0 100.0 200)

(define (psigmoid* x) (- (* 2.0 (sigmoid* x)) 1.0))

(define (inverse-sigmoid P lmb phi)
  (cond
	((<= P 0) 0)
	((<= P 1) (+ (/ (log P) (* 4 (pi) lmb)) phi))
	(else 1)))


;; given a "sigmoid" value which is assumed to pertain to x = 0, the value of the offset is returned
(define (inverse-phi P_0 lmb)
  (cond
	((<= P_0 0) 0)
	((>= P_0 1) 1)
	(else (/ (log (- (/ 1 P_0) 1)) (* 4 (pi) lmb)))))

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
  `(apply + (map ,lmbda (map (lambda (x) (+ ,mn x)) (seq (- ,(+ 1 mx) ,mn))))))

(define-macro (prod mn mx lmbda)
  `(apply * (map ,lmbda (map (lambda (x) (+ ,mn x)) (seq (- ,(+ 1 mx) ,mn))))))



(define (norm a) ;; general
  (if (number? a)
		(* a a)
		(apply + (map * a a))))

(define (v-length a) ;; general
  (if (number? a)
		(abs a)
		(if (plist? a) 
			 (sqrt (norm a))
			 'bad-argument)))

;;(define (distance u v)
;;  (cond
;;	((and (number? u) (number? v)) (abs (- u v)))
;;	((and (= (length u) (length v)) (apply andf (map number? (append u v))))
;;		(let ((sqr (lambda (x) (* x x))))
;;		  (sqrt (apply + (map sqr (map - u v))))))
;;	(else (aborts "distance: bad argument")))

(define (distance p q) ;; general
  (if (and (number? p) (number? q))
		(abs (- p q))
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
		(list-operator op (make-list (length p2) p1) p2))
;		(map (lambda (x) (list-operator op p1 x)) p2))
	  ((and (list? p1) (list? p2) (= (length p1) (length p2)))
		(map op p1 p2))
	  ((and (number? p2) (list? p1))
		(list-operator op p1 (make-list (length p1) p2)))
;		(map (lambda (x) (list-operator op x p2)) p1))
	  (else 
		(dnl "list-operator is confused!")
		(dnl "... p1 = " p1)
		(dnl "... p2 = " p2)
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
  (and (= n (length p)) (point? p)))

(define (n-point-list? n p)
  (letrec ((andf (lambda x (if (null? #t) (and (car x) (apply andf (cdr x)))))))
	 (apply andf (map (lambda (x) (n-point? n x)) p))))



(define (distance-to-segment r segment)
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
		))

(define (distance-to-polygon r poly)
  (define (d2p r poly)
	 (cond
	  ((and (pair? poly) (pair? (cdr poly)))
		(min (distance-to-segment r (list-head poly 2))
			  (distance-to-polygon r (cdr poly))))
	  ((pair? poly) (distance r (car poly)))
	  (#t +inf.0)))

  (d2p r  (append  poly (if (equal? (car poly) (car (reverse poly))) '() (list (car poly))))))




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


(define (dot a b) ;; general
  (apply + (list-operator * a b)))


(define (urnd-int m M) (+ m (random-integer (- M m))))
(define (urnd-real m M) (+ m (* (- M m) (randomreal))))


(define (nrnd . args)
  (let* ((N (length args))
			(mean (if (> N 0) (car args) 0))
			(stddev (if (> N 1) (cadr args) 1))
			(m (if (> N 1) (cadr args) -inf.0))
			(M (if (> N 1) (cadr args) +inf.0))
			)

	 (let* ((gaussian (lambda ()
							 (let* ((pi*2 (* (pi) 2))
									  (u (random-real))
									  (v (random-real))
									  (w (sqrt (* -2.0 (log u)))))
								(* w (cos (* v pi*2))))))
			  (p (+ (* (gaussian) stddev) mean)))
		(if (or (< p m) (< M p))
			 (apply nrnd (append (list mean var) args))
			 p))))


;; this is so we can use gauss.scm for normally distributed random numbers from scmutils
(define random random-real)

(define (random-angle)
  (* (pi) (- (* (random-real) 2.0) 1)))

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


; a and b are vectors ... usually used as (projection (list-op - s r) (list-op - t r))
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
	 (if (or (null? ptlist) (not (pair? ptlist)) (not (pair? (car ptlist))) (< x (caar ptlist))) 
		  0.0
		  (let hunt ((p ptlist))
			 (cond
			  ((null? p) 0.0)
			  ((and (pair? (cdr p)) (< x (caadr p)))
				(let ((d (caar p))
						(D (caadr p))
						(n (cadar p))
						(N (cadadr p)))
				  (+ (* (/ (- N n) (- D d)) (- x d)) n)))
			  (#t (hunt (cdr p))))))))


;; This is used by rk4-* ... it does traces through many dimensional spaces
(define (interpolate pwl x)
  (cond
	((null? pwl)  #f)
	((and (not (pair? pwl)) (not (pair? (car pwl)))) #f)
	((< 2 (length (car pwl)))
	 (map 
	  (lambda (y) 
		 (interpolate 
		  (map 
			(lambda (pt)
			  (list 
				(car pt) 
				(list-ref pt y)) 
			  )
			pwl)
		  x))
	  (map (lambda (x) (+ x 1)) (seq (- (length (car pwl)) 1)))))
	((<= x (caar pwl)) (cadar pwl))
	((null? (cdr pwl)) (cadar pwl))
	((< x (caadr pwl)) 
	 (let* ((p1 (car pwl))
			  (p2 (cadr pwl))
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

		(if (< (abs (- b a)) 1e-80)
				(/ (+ m M) 2.0)
				(+ (* (// (- x a)
							 (- b a))
						(- M m)
						)
					(* 1.0 m)
					))
			 )
	)
  (#t (interpolate (cdr pwl) x)))
)







;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
