;-  Identification and Changes

;--
;	support.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.20
;		Location: odin:/home/gray/study/src/support.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2012 CSIRO Australia
;   All rights reserved
;

;-  Discussion 
#|
    This set of files is largely interdependent.  There ought to be no issues though.
|#


;-  Configuration stuff 

;-  Included files 

(define maybe-load
  (let ((loaded-files '("support.scm"))
		  (LOAD load))
	 (lambda file
		(cond
		 ((null? file) (set! file #f))
		 ((pair? (cdr file))
		  (for-each (lambda (f) (maybe-load f)) file))
		 (else
		  (set! file (car file))
		  
		  (cond
			((not file) loaded-files)
			((not (member file loaded-files))
			 (display "loading ")(display file)(newline)
			 (LOAD file)
			 (set! loaded-files (cons file loaded-files)))
			(else file)))))))

(define (aborts . args) (abort (apply string-append args)))

;;(set! load maybe-load)







;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
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




(define pi (acos -1.))
(define 2pi (* 2.0 pi))
(define e (exp 1))
(define sqrt2pi (sqrt 2pi))



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



;;# general-sigmoid: x is in [0,1], lmb governs how sharp the transition is and phi shifts it to 
;;# one side or the other of the y axis.  Organised so that if l == 1 and phi = 0.0
;;# the value of the function at -0.5 is ~0.002 
;;# the range is (0,1)

;; This is the generating function
(define (general-sigmoid-f x lmb phi)
  (exp (* 4 pi lmb (- x phi))))

;; This is exp( 4*pi*lmb * (x -phi))

(define (general-sigmoid-g v)
  (/ v (1+ v)))

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

(define (psigmoid* x) (- (* 2.0 (sigmoid x)) 1.0))

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
	(else (/ (log (1- (/ 1 P_0))) (* 4 pi lmb)))))

(define (power b e)
  (cond
	((< e 0) (/ 1 (power b (- e))))
	((zero? e) 1)

	((and (integer? e) (rational? b)) ;; This will keep them as exact numbers if they are exact
	 (cond
	  ((even? e) (power (* b b) (/ e 2)))
	  (t (* b (power b (1- e)))))
	 )
	(else (exp (* e (log b))))))


(define (count n)
  (let ((f 0))
  (map (lambda (x) (set! f (1+ f)) (1- f)) (make-list n 0))))

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
  `(apply + (map ,lmbda (map (lambda (x) (+ ,mn x)) (seq (- ,(1+ mx) ,mn))))))

(define-macro (prod mn mx lmbda)
  `(apply * (map ,lmbda (map (lambda (x) (+ ,mn x)) (seq (- ,(1+ mx) ,mn))))))



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
	  ((and (list? p1) (list? p2) (eq? (length p1) (length p2)))
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


(define (dot a b) ;; general
  (apply + (list-operator * a b)))


(define (random-angle)
  (* pi (- (random 2.0) 1)))

(define (rotated-velocity v theta)
  (rotated-vector v theta))




(define (rotated-vector V theta #!optional axis)
  (let ((isvec (vector? V))
		  (v (or (and (list? V) v) (and (vector? V) (vector->list V))))
		  (n (length n)))
  (cond
   ((eq? n 1) V)
   ((eq? n 2)
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
    

;; Composition operator
(define (o . funlist)  ;; general
  (if (eq? (length funlist) 1)
      (lambda x (apply (car funlist) x))
      (lambda x ((car funlist) (apply (apply o (cdr funlist)) x)))))


(define (change-basis vect basis origin)
  (let ((n (length basis)))
    (if (null? origin) (set! origin (make-list (length basis) 0.0)))
        
    (if (<= n 2)
        (let* ((r (list-operator - basis origin))
               (s (list-operator - vect origin))
               (v (list-operator - s r))
               (theta '())
               )
          (if (eq? n 1)
              v
              (rotated-vector v (- 0 (atan (car r) (cadr r))))))
        'change-basis:too-many-dimensions)))


; a and b are vectors ... usually used as (projection (list-op - s r) (list-op - t r))
(define (projection a b) ;; general
  (/ (dot a b) (v-length b)))


 ;; general
(define (abeam a b scale) ; scale might be the distance covered  in a timestep
  (let* ((v (projection a b))
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
	((eq? (length y) 1) (mult2 x (car y)))
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
	((eq? (length y) 1.0) (add2 x (car y)))
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
	((eq? (length y) 1.0) (sub2 x (car y)))
	(#t (sub2 x (apply add y)))))

(define (make-pprnd m) ;
  (let ((table (make-vector 1024 0.0))
		  (mean m)
		  (size 0)
		  )
    (if (not (number? m))
        'make-pprnd:m-really-needs-to-be-a-number

        (let loop ((i 0))
          (if (and (eq? size 0) (< i 1024))
              (begin
                (vector-set! table i (- 1.0 (exp (/ (* -1.0 i) mean))))
                (if (> (vector-ref table i) 0.9999) (begin (set! size (1+ i)) (vector-set! table size 1.0)))
                (loop (1+ i))))))
	 
    (lambda mode
      (cond
       ((and (plist? mode) (eq? (car mode) 'mean))
        mean)
       ((and (plist? mode) (eq? (car mode) 'size))
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
                     (loop (1+ i)) )
                    )
                   (cond
                    ((>= i size) 1.0)
                    ((<= r 0.0) 0)
                    ((< r (vector-ref table i))
                     (/ i size))
                    (#t
                     (loop (1+ i)) )
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
  (lambda args
	 (if (apply andf (map number? args)) ;; This way numbers work as they usually do
		  (apply op args)
		  (lambda (x)
			 (apply op (map (lambda (f) (if (procedure? f) (f x) f)) args))))))

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
	  (map 1+ (seq (1- (length (car pwl)))))))
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
;-  Identification and Changes

;--
;	integrate.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2008.08.03
;		Location: trickster.100acwood.grayrabble.org:/home/gray/Study/playpen/integrate.scm
;
;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 


(define debugging-integration #f)
(define MAX-DEPTH 5)

;; Algorithm translated from Wikipedia: "Adaptive Simpson's Method" 30/04/2009

;;(adaptive-integrate function lowerbound upperbound  tolerance maxdepth)



(define (inner-adaptive-integrate f a b eps estimate fa fc fb k)
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
		  (if (or (<= k 0) (<= (abs (- inner-estimate estimate)) (* 15 eps)))
				(+ inner-estimate (/ (- inner-estimate estimate) 15))
				(+ (inner-adaptive-integrate f a c (/ eps 2) left-estimate fa fd fc (1- k))
					(inner-adaptive-integrate f c b (/ eps 2) right-estimate fc fe fb (1- k))
					)))))

(define (adaptive-integrate f a b eps . k)
  (set! k (if (null? k) MAX-DEPTH (car k)))
  (if (< b a)
		(- (adaptive-integrate f b a eps k))
		(let* ((c (/ (+ a b) 2))
				 (fa (f a))
				 (fc (f c))
				 (fb (f b))
				 (estimate (+ fa (* 4 fc) fb))
				 )
		  (inner-adaptive-integrate f a b eps estimate fa fc fb k))))


;;(integrate function lowerbound upperbound tolerance #!optional maxdepth)
;;(integrate% function lowerbound upperbound tolerance ignored-stepsize #!optional maxdepth)
;;(integrate* function lowerbound upperbound tolerance stepsize #!optional maxdepth)

(define (integrate f a b eps . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (adaptive-integrate f a b eps k)
  )

(define (integrate% f a b eps ignore-ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate f a b eps k))

(define (integrate* f a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (if (zero? ss) 
		(integrate f a b eps k)
		(let loop ((sum 0)
					  (x a))
		  (if (>= (+ x ss) b)
				(+ sum (integrate f x b eps k))
				(loop (+ sum (integrate f x (+ x ss) eps k))
						(+ x ss))))))


;;(integrate2d function lowerleft upperright tolerance #!optional maxdepth)
;;(integrate2d% function lowerleft upperright tolerance ignored-stepsize #!optional maxdepth)
;;(integrate2d* function lowerleft upperright tolerance stepsize #!optional maxdepth)
;;(integrate2d%* function lowerleft upperright tolerance stepsize #!optional maxdepth)
;;(integrate2d*% function lowerleft upperright tolerance stepsize #!optional maxdepth)
;;(integrate2d** function lowerleft upperright tolerance stepsize #!optional maxdepth)

;; a is the lower left corner of a rectangular domain, b is the upper right corner

(define (integrate2d func a b eps . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate (lambda (x) (integrate (lambda (y) (func (list x y))) (cadr a) (cadr b) eps)) (car a) (car b) eps))

(define (integrate2d* func a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate* (lambda (x) (integrate* (lambda (y) (func (list x y))) (cadr a) (cadr b) eps ss k)) (car a) (car b) eps ss k))


(define (integrate2d% f a b eps ignore-ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate2d f a b eps k))

(define (integrate2d%* f a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate% (lambda (x) 
					 (integrate* 
					  (lambda (y) (apply f x y)) 
					  (cadr a) (cadr b) eps ss k)) 
				  (car a) (car b) eps ss k) 
  )


(define (integrate2d*% f a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate* (lambda (x) 
					 (integrate%
					  (lambda (y) (apply f x y)) 
					  (cadr a) (cadr b) eps ss k)) 
				  (car a) (car b) eps ss k) 
  )


(define (integrate2d** f a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate* (lambda (x) 
					 (integrate* 
					  (lambda (y) (apply f x y)) 
					  (cadr a) (cadr b) eps ss k)) 
				  (car a) (car b) eps ss k) 
  )


(define (test-integrate2d p)
	 (if (<= (apply + (map (lambda (x) (* x x)) p)) 1) 1 0))

(define (inner-general-adaptive-integrate f a b eps estimate fa fc fb << ** // ++ -- k)
  (if (not a) (#f 'this))
  (if (not b) (#f 'that))
  
  (if (< eps 0) (set! eps (abs eps)))
  (if (<< b a) 
		(- (inner-general-adaptive-integrate f b a eps k << ** // ++ -- k))
		(let* ((h (v-length (-- b a)))
				 (c (// (++ a b) 2))
				 (d (// (++ a c) 2))
				 (e (// (++ c b) 2))
				 (fd (f d))
				 (fe (f e))
				 (left-estimate (* (/ h 12) (+ fa (* 4 fd) fc)))
				 (right-estimate (* (/ h 12) (+ fc (* 4 fe) fb)))
				 (inner-estimate (+ left-estimate right-estimate))
				 ;;				 (delta (/ (- inner-estimate estimate) 15))
				 )

;		  (mdnl "(inner-general-adaptive-integrate f" a b eps estimate fa fc fb k ")")
;		  (mdnl "inner estimate =" inner-estimate)
;		  (mdnl "k =" k)
;		  (mdnl "(abs (- inner-estimate estimate)) =" (abs (- inner-estimate estimate)))
;		  (mdnl "(* 15 eps) =" (* 15 eps))
		  (if (or (<= k 0) (<= (abs (- inner-estimate estimate)) (* 15 eps)))
				(+ inner-estimate (/ (- inner-estimate estimate) 15))
				(+ (inner-general-adaptive-integrate f a c (/ eps 2) left-estimate fa fd fc << ** // ++ -- (1- k))
					(inner-general-adaptive-integrate f c b (/ eps 2) right-estimate fc fe fb << ** // ++ -- (1- k))
					)
				)
		  )
		)
  )

(define (general-adaptive-integrate f a b eps swap-order mult div add sub . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (let (
		  (<< swap-order)
		  (** mult)
		  (// div)
		  (++ add)
		  (-- sub)
		  )

;	 (->list a)
;	 (->list b)

	 (if (< eps 0) (set! eps (abs eps)))

	 (if (<< b a)
		  (- 0 (general-adaptive-integrate f b a eps swap-order mult div add sub k))
		  (let* ((c (// (++ a b) 2))
					(fa (f a))
					(fc (f c))
					(fb (f b))
					(estimate (+ fa (* 4 fc) fb))
					)
			 (inner-general-adaptive-integrate f a b eps estimate fa fc fb swap-order mult div add sub k)
			 )
		  )
	 )
)

(define (general-integrate f a b eps swap-order mult div add sub . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (general-adaptive-integrate f a b eps swap-order mult div add sub k)
)


; This is included as a test of integrate-RV
(define (integrate-R f a b eps . k)
; function over the field of real numbers
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (general-integrate f a b eps < * / + - k))

(define (integrate-R% f a b eps ignore-ss . k)
; function over the field of real numbers
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate-R f a b eps k))

(define (no-order-swap a b)
  #f)

;;;; integrates over a path in a vector space -------------------------

(define (integrate-RV f a b eps . k)
; function over a line in a vector space

  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (if (and (number? a) (number? b))
		(integrate-R f a b eps k)
		(general-integrate f a b eps no-order-swap mult div add sub k)
		)
  )

(define (integrate-RV% f a b eps ignore-ss . k)
; function over a line in a vector space
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (integrate-RV f a b eps k))

(define (integrate-RV* f a b eps ss . k)
  (set! k (if (null? k) MAX-DEPTH (if (pair? k) (car k) k)))
  (let* ((d (list-operator - b a))
;			(x1 (dnl "d = " d))
			(HdH (v-length d))
;			(x2 (dnl "HdH = " HdH))
			(dx (list-operator * ss (list-operator / d HdH)))
;			(x3 (dnl "dx = " dx))
			)
	 (if (<= HdH eps) 
		  (integrate-RV f a b eps k)
		  (let loop ((sum 0)
						 (x a))
			 (if (>= (v-length (list-operator - (add x dx) a)) HdH)
				  (+ sum (integrate-RV f x b eps k))
				  (loop (+ sum (integrate-RV f x (add x dx) eps k))
						  (add x dx)))))))
					
#|
	Runge-Kutta 4


  dy/dt = f(t,y) with  y(t ) = y
                          0     0
  where f(t,y) is some functional form using
  y() (and possibly t)

  The Runge-Kutta process needs a t  and a y  in order to start
                                   0        0 
  and progresses thus:

  k = f(t , y )
   1     n   n

  k = f(t + h/2, y + k h/2)
   2     n        n   1	 
		     
  k = f(t + h/2, y + k h/2)
   3     n        n   2	 
		         
  k = f(t + h, y + k h)    
   4     n      n   3


              h
  y   = y  + ---(k + 2k + 2k + k )
   n+1   n    6   1    2    3   4


// this returns a piecewise linear approximation of y over [a,b] in x[], y[] which must be big enough

double simple_rk4(double stepsize, int NI, double *X, double *Y, double Y0, double (*dy)(double t, double x)) {
   double y, yy, t;
   double k1, k2, k3, k4;
   double h, h_2;
   int i;
   double a, b;

   assert(NI > 0);

#if defined(NON_POSITIVE_STEPSIZE_ABORT)
   if (stepsize <= 0) abort();
#else
   assert(stepsize > 0);
#endif

   // Initial conditions
   
   y = Y0; 
   a = X[0];
   b = a + (NI-1) * stepsize;

   h = stepsize;
   h_2 = h/2;

   for (i = 1, t = a; i < NI-1 && t < b;) {
      if (t+h > b) h = b-t; 

      k1 = dy(t, y);
      k2 = dy(t + h_2, y + k1 * h_2);
      k3 = dy(t + h_2, y + k2 * h_2);
      k4 = dy(t + h, y + k3 * h);

      yy = y + h/6 * (k1 + 2*(k2 + k3) + k4);

      i++;
      t += h;
      X[i] = t;
      Y[i] = y = yy;
   }

   return y;
}

|#

;; The Butcher tableau for RK4 is 

;;	0	|
;;	1/2|	1/2
;;	1/2|	0		1/2
;;	1	|	0		0		1
;;-----------------------------
;;		|	1/6	1/3	1/3	1/6
;;
;; A Butcher tableau is represented by 
;
;; ((c_1 ... c_s) ((a_1|1 ... a_1|s) ...(a_s|1 ... a_s|s)) (b_1 ... b_s))
;;
;; for explicit methods, or
;;
;; ((c_1 ... c_s) ((a_1|1 ... a_1|s) ...(a_s|1 ... a_s|s)) (b_1 ... b_s) (b'_1 ... b'_s))
;; 
;; for implicit methods, though I council against trying to use a single quote as a part of 
;; a symbol in scheme or lisp.


;; The R-K methods give numerical solutions for functions of the form f(t,y)
;;                                s
;; of the form y_{n+1} = y_n + h ∑ b_i k_i
;;                               i=1
;;
;;                                    i-1
;; where k_i = f(t_n + c_i h, y_n + h ∑ a_ij k_j
;;                                    j=1
;; for *explicit* methods.  

;; Implicit methods are whole different kettle of fish, since we would have to solve a system 
;; of s equations at each step to calculate k_i. In this case, we use the second form of the 
;; tableau, evaluate the sum in the expression for y_{n+1} from zero to s,  and additionally 
;; define 
;;                                s
;;            y'_{n+1} = y_n + h ∑ b'_i k_i
;;                               i=1
;;
;;                                              s
;;            e_{n+1} = y_{n+1} - y'_{n+1} = h ∑ (b_i - b'_i) k_i
;;                                             i=1
;;
;; which can be used to change the step size, h. I would usually do by dividing h into 
;; more segments.


;; We can construct a piecewise linear interpolating function with the 
;; line segments defined by the  (t_i, y_i) as is shown in rk4, below.

;; The following is the classic RK4 with a tableau of '((0 1/2 1/2 1) ((0 0 0 0) (1/2 0 0 0) (0 1/2 0 0) (0 0 1 0)) (1/6 1/3 1/3 1/6))

;;

;; We have an expression of the form dy/dx = f where y is a scalar function of t and f is a function in terms of y and t
;; and the function y is not known.
;; In this case we could write something like (define dy/dx f) (define y (rk4 dy/dx mn mx step y_0))
;; where f (and thus dy/dy) looks like "(lambda (t y) (some expression in y and t, where y is treated as a variable))"
;; and y is of the form "(lambda (t) (some expression in t))"

;; (define P (rk4 (lambda (t y)  y) 0 10 0.1 1))
;; (exp 10) => 22026.465794806718
;; (P 10) => 22026.296900876645
;;
;; compared to the librcg version in C 
;; 
;;; evaluate 'ode(dy/dt = y(t), y(0) = 1, 0.1, 10)'
;;; ode(dy/dt = y(t), y(0) = 1, 0.1, 10) = 22026.296901



;; Here f is dy/dx, [a,b] is the domain over which the 
;; returned function is defined, ss is the stepsize and 
;; Y0 is the initial value at a
(define (rk4 f a b ss Y0) 
  (if (not (procedure? f))
		(abort "rk4 expects a function as its first argument"))

  (let* ((h (* 1 ss))
			(h/2 (/ h 2)))
	 (letrec ((rk4i
				  (lambda (t y)
					 (let* ((t+h/2 (+ t h/2))
							  (t+h (+ t h))
							  (k1 (f t y))
							  (k2 (f t+h/2 (+ y (* k1 h/2))))
							  (k3 (f t+h/2 (+ y (* k2 h/2))))
							  (k4 (f t+h (+ y (* k3 h))))
							  (yy (+ y (* (/ h 6) (+ k1 (* 2 (+ k2 k3)) k4))))
							  )
						(if (> t+h b)
							 (cons (list t+h yy) '())
							 (cons (list t+h yy) (rk4i t+h yy))))))
				 )
		
		(let* ((data (cons (list a Y0) (rk4i a Y0)))
				 (Y (lambda (x)
						(if (symbol? x)
							 data
							 (interpolate data x))))
				 )
		  Y))))


;;(define sincos (rk4-2d (lambda (t x y)  y) (lambda (t x y) (- x)) 0 (* 2 pi) 0.001 0 1))  
;; where (lambda (t x y)  y) is d sin/dx and (lambda (t x y) (- x)) is d cos/dx, the domain 
;; of the resulting functions is [0,2pi] we have a step of 0.01 and the initial values are 
;; 0 and 1 for sin and cos

(define (rk4-2d f g a b ss X0 Y0) 
  (if (not (procedure? f))
		(abort "rk4 expects a function as its first argument"))
  (if (not (procedure? g))
		(abort "rk4 expects a function as its second argument"))
  (if (or (not (number? X0)) (not (number? Y0)))
		(abort "The last two of the seven arguments should be the X0 and Y0 values of the functions"))

  (let* ((h (* 1 ss))
			(h/2 (/ h 2)))
	 (letrec ((rk4i
				  (lambda (t x y)
					 (let* ((t+h/2 (+ t h/2))
							  (t+h (+ t h))

							  (k1 (f t x y))
							  (l1 (g t x y))

							  (k2 (f t+h/2 (+ x (* k1 h/2)) (+ y (* l1 h/2))))
							  (l2 (g t+h/2 (+ x (* k1 h/2)) (+ y (* l1 h/2))))

							  (k3 (f t+h/2 (+ x (* k2 h/2)) (+ y (* l2 h/2))))
							  (l3 (g t+h/2 (+ x (* k2 h/2)) (+ y (* l2 h/2))))

							  (k4 (f t+h (+ x (* k3 h)) (+ y (* l3 h))))
							  (l4 (g t+h (+ x (* k3 h)) (+ y (* l3 h))))
							  ;;;(l4 (f t+h (+ x (* k3 h)) (+ y (* l3 h)))) ;; in the paper this is l_4 = f(...) which I don't believe.

							  (xx (+ x (* (/ h 6) (+ k1 (* 2 (+ k2 k3)) k4))))
							  (yy (+ y (* (/ h 6) (+ l1 (* 2 (+ l2 l3)) l4))))
							  );; end of let* variables
						(dnl t+h " " xx " " yy)
;;						(dnl " k: " k1 " " k2 " " k3 " " k4)
;;						(dnl " l: " l1 " " l2 " " l3 " " l4)
						(if (> t+h b)
							 (cons (list t+h xx yy) '())
							 (cons (list t+h xx yy) (rk4i t+h xx yy)))
						) ;; end of let*
					 ) ;; end of lambda
				  ) ;; end of rk4i
				 ) ;; end of letrec variables
		
		(let* ((data (cons (list a X0 Y0) (rk4i a X0 Y0)))
				 (XY (lambda (t)
						(if (symbol? t)
							 data
							 (interpolate data t))))
				 )
		  XY))))


;; (define P (rk4* (list (lambda (t y)  y)) 0 10 0.1 (list 1)))
;; (exp 10) => 22026.465794806718
;; (P 10) => 22026.296900876645


;;(define sincos (rk4-2d (lambda (t x y)  y) (lambda (t x y) (- x)) 0 (* 2 pi) 0.001 0 1))  
;; where (lambda (t x y)  y) is d sin/dx and (lambda (t x y) (- x)) is d cos/dx, the domain 
;; of the resulting functions is [0,2pi] we have a step of 0.01 and the initial values are 
;; 0 and 1 for sin and cos

(define (rk4* F a b ss Xo . ZT) 
  (if (< (abs ss) 1e-12) 
		(abort "bad step size in rk4*"))

  (set! ZT (if (null? ZT) #f (car ZT)))
  (if (not (list? F))
		(abort "rk4 expects a list of functions as the first argument"))
  (if (or (not (list? Xo)) (not (eq? (length F) (length Xo))))
		(abort "rk4 expects list of the initial values of the functions as the fifth (and final) argument"))

  (let* ((h (* 1 ss))
			(h/2 (/ h 2)))
	 (letrec ((rk4i
				  (lambda args
;;					 (dnl "RK4I ËNTRY:  " args)
					 (if (< (abs h) 1e-12) 
						  (abort "bad step size in rk4*/rk4i"))
					 (let* ((t (car args))
							 (funcs (cdr args))
							 (t+h/2 (+ t h/2))
							 (t+h (+ t h))
									  
							 (q1 (let ((ordinates (cons t funcs)))
									 (map (lambda (f) (apply f ordinates)) F)))

							 (q2 (let ((ordinates (cons t+h/2 
																 (map (lambda (x v) 
																		  (+ x (* v h/2))) funcs q1))))
									 (map (lambda (f) (apply f ordinates)) F)))

							 (q3 (let ((ordinates (cons t+h/2 
																 (map (lambda (x v) 
																		  (+ x (* v h/2))) funcs q2))))
									 (map (lambda (f) (apply f ordinates)) F)))

							 (q4 (let ((ordinates (cons t+h 
																 (map (lambda (x v) 
																		  (+ x (* v h))) funcs q3))))
									 (map (lambda (f) (apply f ordinates)) F)))

							 (pV (map (lambda (x q1 q2 q3 q4) 
													 (+ x 
														 (* (/ h 6) 
															 (+ q1 (* 2 (+ q2 q3)) 
																 q4)))) 
												  funcs q1 q2 q3 q4))
							 (V (if ZT (map ZT pV) pV))
							 ) ;; end of let* variables
;;						(dnl "   Q1 " q1)
;;						(dnl "   Q2 " q2)
;;						(dnl "   Q3 " q3)
;;						(dnl "   Q4 " q4)
;;						(dnl "    V " V)
						(if (> t+h b)
							 (cons (append (list t+h) V) '())
							 (cons (append (list t+h) V) (apply rk4i (cons t+h V))))
						);; end of let*
					 ) ;; end of lambda
				  ) ;; end of rk4i
				 ) ;; end of letrec variables
						
		(let* ((data (cons (cons a Xo) (apply rk4i (cons a Xo))))
				 (XY (lambda (t)
						 (if (symbol? t)
							  data
							  (interpolate data t))))
				 )
		  XY)
		)))



#|
As an example, consider the two-stage second-order Runge–Kutta method with α = 2/3. It is given by the tableau

	0		|
	2/3	|2/3	
	---------------
			|1/4	3/4


and has the corresponding equations


k_1 = f(t_n,y_n)
k_2 = f(t_n + 2/3 h, y_n + 2/3 h k_1)
y_{n+1} = y_n + h(1/4 k_1 + 3/4 k_2);



	The Butcher tableau for Runge-Kutta-Fehlberg

	0			|
   1/4		|1/4
	3/8		|3/32	
	12/13		|1932/2197	−7200/2197	7296/2197
	1  		|439/216		−8				3680/513	   −845/4104
	1/2		|-8/27 		2				−3544/2565	1859/4104	−11/40	
-------------------------------------------------------------------------
		   	|25/216		0				1408/2565	2197/4104	−1/5		0
		   	|16/135		0				6656/12825	28561/56430	−9/50		2/55



The first row of coefficients gives the fourth-order accurate method, and the second row gives the fifth-order accurate method.

|#

#|
;; (define P (rkf* (list (lambda (t y)  y)) 0 10 0.1 (list 1)))
;; (exp 10) => 22026.465794806718
;; (P 10) =/=> 22026.296900876645

(define rkf-tableau 
  '((0 1/4 3/8 12/13 1 1/2)                       ;;c
	 ((0 0 0 0 0)                                  ;;a
	  (1/4 0 0 0 0)
	  (3/2 9/32 0 0 0)
	  (1932/2197 −7200/2197 7296/2197 0 0)
	  (439/216 −8 3680/513 −845/4104 0)
	  (-8/27 2 −3544/2565 1859/4104 −11/40))
	 (25/216 0 1408/2565 2197/4104 −1/5 0)         ;; b
	 (16/135 0 6656/12825 28561/56430 −9/50 2/55)  ;; b*
	 ))

(define (rkf* F a b ss Xo) 
  (let* ((h (* 1 ss))
			(h/2 (/ h 2))
			;;(c (car rkf-tableau))
			;;(a (cadr rkf-tableau))
			;;(b (caddr rkf-tableau))
			;;(b* (cadddr rkf-tableau))
			)

	 (letrec ((rkfi
				  (lambda args
					 (dnl "RKFI ËNTRY [" (car args) "]:  " (cdr args))
					 (let* ((step (car args))
							  (t (cadr args))
							  (funcs (cddr args))
							  
							  (q1 (let ((ordinates (cons t funcs)))
									  (map (lambda (f) (apply f ordinates)) F)))
							  
							  (q2 (let ((ordinates (cons (+ t (* h 1/4)) 
																  (map (lambda (x a) 
																			(+ x 
																				(* a 1/4))) funcs q1))))
									  (map (lambda (f) (*  (apply f ordinates))) F)))


							  (q3 (let ((ordinates (cons (+ t (* h 3/8))
																  (map (lambda (x a b) 
																			(+ x 
																				(* a 3/32) 
																				(* b 9/32))) funcs q1 q2))))
									  (map (lambda (f) (*  (apply f ordinates))) F)))

							  (q4 (let ((ordinates (cons (+ t (* h 12/13))
																  (map (lambda (x a b c) 
																			(+ x 
																				(* a 1932/2197) 
																				(* b -7200/2197)  
																				(* c 7296/2197))) funcs q1 q2 q3))))
									  (map (lambda (f) (*  (apply f ordinates))) F)))

							  (q5 (let ((ordinates (cons (+ t (* h 1))
																  (map (lambda (x a b c d)  
																			(+ x 
																				(* a 493/216) 
																				(* b -8)
																				(* c 3680/513) 
																				(* d -845/4104))) funcs q1 q2 q3 q4))))
									  (map (lambda (f) (*  (apply f ordinates))) F)))

							  (q6 (let ((ordinates (cons (+ t (* h 1/2))
																  (map (lambda (x a b c d e)  
																			(+ x 
																				(* a -8/27) 
																				(* b 2)  
																				(* c -3544/2565) 
																				(* d 1859/4104) 
																				(* e -11/40))) funcs q1 q2 q3 q4 q5))))
									  (map (lambda (f) (*  (apply f ordinates))) F)))


							  (V (map (lambda (x q1 q2 q3 q4 q5 q6) 
											(+ x 
												(* h 25/216 q1)
												;;(* h 0 q2)
												(* h 1408/2565 q3)
												(* h 2197/4104 q4)
												(* h -1/5 q5)
												;;(* h 0 q6)	
												))
										 funcs q1 q2 q3 q4 q5 q6))
							  (V* (map (lambda (x q1 q2 q3 q4 q5 q6) 
											 (+ x 
												 (* h 16/135 q1)
												 ;;(* h 0 q2)
												 (* h 6656/12825 q3)
												 (* h  28561/56430 q4)
												 (* h -9/50 q5)
												 (* h 2/55 q6)	
												 ))
										  funcs q1 q2 q3 q4 q5 q6))

							  (num (let loop ((v V) (v* V*) (worst 0) (target #f))
										(cond
										 ((null? v) target)
										 ((zero? (abs (- (car v) (car v*))))
										  (loop (cdr v) (cdr v*) worst target))
										 ((> 
											(/ (abs (- (car v) (car v*))) (/ (+ (abs (car v)) (abs (car v*))) 2))
											worst)
										  
										  (loop (cdr v) (cdr v*) (/ (abs (- (car v) (car v*))) (/ (+ (abs (car v)) (abs (car v*))) 2))  (abs (- (car v) (car v*))))) 
										 (else (loop (cdr v) (cdr v*) worst target)))))


							  (den (let loop ((v V) (v* V*) (worst 0) (target #f))
										(cond
										 ((null? v) target)
										 ((zero? (abs (- (car v) (car v*))))
										  (loop (cdr v) (cdr v*) worst target))
										 ((> 
											(/ (abs (- (car v) (car v*))) (/ (+ (abs (car v)) (abs (car v*))) 2))
											worst)
										  
										  (loop (cdr v) (cdr v*) (/ (abs (- (car v) (car v*))) (/ (+ (abs (car v)) (abs (car v*))) 2))  (/ (+ (abs (car v)) (abs (car v*))) 2)))
										 (else (loop (cdr v) (cdr v*) worst target)))))

							  (err (let loop ((v V) (v* V*) (worst 0))
										(cond
										 ((null? v) worst)
										 ((zero? (abs (- (car v) (car v*))))
										  (loop (cdr v) (cdr v*) worst))
										 ((> 
											(/ (abs (- (car v) (car v*))) (/ (+ (abs (car v)) (abs (car v*))) 2))
											worst)
										  (loop (cdr v) (cdr v*) (/ (abs (- (car v) (car v*))) (/ (+ (abs (car v)) (abs (car v*))) 2))))
										 (else (loop (cdr v) (cdr v*) worst)))))

							  (nstep (cond 
										 (#t step)
										 ((< err 1/1000000) (* step 2))
										 ((> err 1/1000) (/ step 2))
										 (else step)))
							  ) ;; end of let* variables
						;;(dnl "step " step ", nstep " nstep ", V " V ", V* " V*  ", err " err ", num " num ", den " den ", num/den " (/ num den))
						(dnl "   Q1 " q1)
						(dnl "   Q2 " q2)
						(dnl "   Q3 " q3)
						(dnl "   Q4 " q4)
						(dnl "   Q5 " q5)
						(dnl "   Q6 " q6)
						(dnl "   V* " V*)
						(dnl "    V " V)

						
						(if (> (+ t h) b)
							 (cons (append (list (+ t h)) V) '())
							 (cons (append (list (+ t h)) V) (apply rkfi (append (list nstep (+ t h)) V))))
						);; end of let*
					 ) ;; end of lambda
				  ) ;; end of rkfi
				 ) ;; end of letrec variables
		
		(let* ((data (cons (cons a Xo) (apply rkfi (append (list ss a) Xo))))
				 (XY (lambda (t)
						 (if (symbol? t)
							  data
							  (interpolate data t))))
				 )
		  XY)
		)))
|#



;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***








;-  Identification and Changes

;--
;	units.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2010.06.12
;		Location: loki:/data/study-runs/playpen/units.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2010 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

;***************** UNITS ******************

(define (exp-decay-rate prop period)
  (- (/ (log (- 1.0 prop)) period)))

(define (years . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (* n 365.0))

(define (weeks . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (* n 7.0))

(define (days . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  n)

(define (hours . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n 24.0))

(define (minutes . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n (* 24.0 60)))

(define (seconds . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n (* 24.0 60 60 )))

(define (m/s . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n (seconds 1))) 

(define (m/d . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n (days 1))) 


;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
;-  Identification and Changes

;--
;	postscript.scm -- Written by Randall Gray 

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 


(define (make-it-a-string s) 
  (or (and (string? s) s) (and (char? s) (make-string 1 s)) (object->string s)))

(define (andf . args)
  (if (null? args)
		#t
		(and (car args) (apply andf (cdr args)))))

(define (gmap l x)
  (if (pair? x)
		(map l  x)
		(l x)))

(define (rescale s x)
  (if (pair? s)
		(gmap (lambda (t) (map * s t)) x)
		(gmap (lambda (t) (* s t)) x))  )

(define (inches->points x)
  (rescale 72.0 x))

(define (points->inches x)
  (rescale (/ 1.0 72.0) x))


(define (points->mm x)
  (rescale (/ 1.0 2.83464646464646464646) x))

(define (mm->points x)
  (rescale 2.83464646464646464646 x))

(define (mm->inches x)
  (rescale (/ 1.0 24.5) x))

(define (inches->mm x)
  (rescale 24.5 x))

(define pagesize '(595 841)) ;; in 1/72 inches...

(define (scaled-by-x x pagesize)
  (list x (* x (/ (cadr pagesize) (car pagesize)))))

(define (scaled-by-y y pagesize)
  (list x (* y (/ (car pagesize) (cadr pagesize)))))

;(define target-size (inches->points (scaled-by-x 6.8 pagesize)))

(define (make-list-matrix . rows)
  (if (apply = (map length rows))
		(map copy-list rows)
		#f))


(define (list-matrix? m)
  (and (pair? m) 
		 (apply andf (map simple-list? m)) 
		 (apply = (map length m))))

(define (transpose-list-matrix A)
  (if (list-matrix? A) 
		(let* ((dima (list (length A) (length (car A))))
				 (B (make-list* (list (cadr dima) (car dima)) 0)))
		  (for-each
			(lambda (i) 
			  (for-each
				(lambda (j)
				  (list-set* B (list j i) (list-ref* A (list i j))))
				(seq (cadr dima))))
			(seq (car dima)))
		  B)
		#f))

(define (*-matrix a b)
  (cond
	((and (number? a) (number? b)) (* a b))
	((and (number? b) (list-matrix? a)) (map (lambda (x) (map (lambda (y) (* b y)) x)) a))
	((and (number? a) (list-matrix? b)) (map (lambda (x) (map (lambda (y) (* a y)) x)) b))
	(else 
	 (let* ((dima (list (length a) (length (car a))))
			  (dimb (list (length b) (length (car b))))
			  )
		(if (not (= (cadr dima) (car dimb)))
			 (abort "incompatible matrices")
			 (map (lambda (x) (map (lambda (y) (apply + (map * x y))) (transpose-list-matrix b))) a))))))

(define (*-matrix-vector m x)
  (if (list-matrix? m) 
		(*-matrix m (transpose-list-matrix (list x)))
		(*-matrix (list x)) m))

(define (rotate-point-list theta pointlist)
  (map (lambda (x) (let ((m (make-list-matrix `(,(cos theta) ,(- (sin theta))) `(,(sin theta) ,(cos theta)))))
							(*-matrix-vector m x)))
		 pointlist))

(define (adjust operator deviant pointlist)
  (if (and (pair? pointlist) (pair? (car pointlist)))
		(if (pair? deviant)
			 (map (lambda (pt) (map (lambda (s o) (operator s o )) deviant pt)) pointlist)
			 (map (lambda (pt) 
					  (map (lambda (o) 
								(operator deviant o)) pt)) pointlist)
			 )
		(if (pair? deviant)
			 (map operator deviant pointlist)
			 (map (lambda (o) (operator deviant o)) pointlist))))


(define (scale-pointlist k pointlist)
  (adjust * k pointlist))

(define (translate-pointlist offset pointlist)
  (adjust + offset pointlist))

(define (translate-pointlist* n offset lstlst) ;; This should be generalised....
  (if (zero? n)
		(adjust + offset lstlst)
		(map (lambda (lst) (translate-pointlist* (1- n) offset lst)) lstlst)))

(define pi (* 4.0 (atan 1.0)))
(define 100pi 314)
(define 10000pi 31416)
(define tau (* 2.0 pi))

(define (fold-A-series-paper aN)
  (list (/ (cadr aN) 2.0) (car aN)))


(define a4 (list (mm->points 210) (mm->points 297)))
(define a5 (fold-A-series-paper a4))
(define a6 (fold-A-series-paper a5))

;; == 595.276 841.89 points
;; == 8.26772 x 11.6929 inches

(define sl '())
(define isl '())

(define (list-tabulate len proc)
  (do ((i (- len 1) (- i 1))
       (ans '() (cons (proc i) ans)))
      ((< i 0) ans)))

(define (make-circle location radius-pts divisions)
  (translate-pointlist 
	location 
	(scale-pointlist radius-pts 
						  (map (lambda (x) 
									(list (cos (/ (* tau x) divisions)) 
											(sin (/ (* tau x) divisions))))
								 (list-tabulate divisions (lambda (x) x))
								 ))))

(define (deep-string->number lst) 
  (cond
	((null? lst) lst)
	((and (not (string? lst)) (atom? lst)) lst)
	((and (string? lst) (string->number lst)) (string->number lst))
	((pair? lst) (map deep-string->number lst))
	(#t lst)))

(define (deep-string->symbol lst) 
  (cond
	((null? lst) lst)
	((and (not (string? lst)) (atom? lst)) lst)
	((and (atom? lst) (string->number lst)) lst)
	((and (string? lst) (string->symbol lst)) (string->symbol lst))
	((pair? lst) (map deep-string->symbol lst))
	(#t lst)))

(define (load-data fname)
  (let ((data (deep-string->number (load-list-from-file fname))))
	 (display (string-append "Loaded " fname "\n"))
	 data))


(define (adjusted-plot-polygon ps width greyvalue open-path project-point-fn point-list)
  (let ((plot (if project-point-fn (map project-point-fn point-list) point-list)))
;(pp plot)
	 (plot-polygon ps width greyvalue plot open-path)))

(define (adjusted-plot-filled-polygon ps width bordervalue interiorvalue project-point-fn point-list)
  (let ((plot (if project-point-fn (map project-point-fn point-list) point-list)))
;(pp plot)
	 (plot-filled-polygon ps width bordervalue interiorvalue plot)))

;; These are support routines that get called both here and from functions in other files.....

(define ps-circle
  (lambda (ps rad x width whiteness #!optional filled)
	 ;; (adjusted-plot-polygon pshandle width whiteness openpath? projectionfn pointlist) ;; whiteness is in [0,1]
	 (if filled
		  (adjusted-plot-filled-polygon ps width whiteness #f #f 0.1 (make-circle x rad 120))
		  (adjusted-plot-polygon ps width whiteness #f #f (make-circle x rad 120))		)

	 )
  )

(define (projection loc range co-range)
  (lambda (x) (translate-pointlist loc (scale-pointlist (/ co-range range) x))))


(define (make-ps port/filename fontlist)
  (let ((file (cond
					((output-port? port/filename) port/filename)
					(else (open-output-file port/filename))))
		  (fonts fontlist)
		  (pagescale '(1.0 1.0))
		  (pageoffset '(0 0))
		  (pagecount 0)
		  )

    (define (ps-display thing)
      (display thing file)
      (display "\n" file))

    (define (ps-1-arg command arg)
      (display arg file)
      (display " " file)
      (display command file)
      (display "\n" file)
      )
    
    (define (ps-2-arg cmd x y)
      (display x file)
      (display " " file)
      (display y file)
      (display " " file)
      (display cmd file)
      (display "\n" file)
      )

    (define (ps-5-arg cmd a1 a2 a3 a4 a5)
      (display a1 file)
      (display " " file)
      (display a2 file)
      (display " " file)
      (display a3 file)
      (display " " file)
      (display a4 file)
      (display " " file)
      (display a5 file)
      (display " " file)
      (display cmd file)
      (display "\n" file))

    (define (ps-pair-or-list cmd pointlist)
      (cond 
       ((null? pointlist) #f)
       ((and (list? pointlist) (list? (car pointlist)))
		  (let loop ((p pointlist))
			 (if (not (null? p))
				  (begin
					 (ps-pair-or-list cmd (car p))
					 (loop (cdr p))))))
       ((list? pointlist)
		  (display (car pointlist) file)
		  (display " " file)
		  (display (cadr pointlist) file)
		  (display " " file)
		  (display cmd file)
		  (display "\n" file))
       (#t #f)))
	 

    (define (font nfont size)
      (if (not (null? fonts))
			 (begin
				(display "/" file)
				(display nfont file)
				(display " findfont\n" file)
				(display size file)
				(display " scalefont setfont\n" file))
			 #f))

    (define (times-roman size)
      (font "Times-Roman" size))

    (define (times-italic size)
      (font "Times-Italic" size))
    
    (define (times-bold size)
      (font "Times-Bold" size))
    
    (define (helvetica size)
      (font "Helvetica" size))
    
    (define (helvetica-italic size)
      (font "Helvetica-Italic" size))
    
    (define (helvetica-bold size)
      (font "Helvetica-Bold" size))
    
    (define (emit-header)
      (ps-display "%!PS-Adobe-1.0")

      (display "%%DocumentFonts: " file)
      (if (list? fonts)
			 (map (lambda (x) 
					  (display x file) 
					  (display " " file)) fonts)
			 (display fonts file))
      (display "\n" file)
      
      (ps-display "%%Pages: (atend)\n")
      (ps-display "%%EndProlog\n")
      )
    
    (define (define-unitnames)
      (ps-display "/inch {72 mul} def")
      (ps-display "/mm {2.8346456693 mul} def\n")
      )


    (define (showpage)
      (set! pagecount (1+ pagecount))
      (ps-display "showpage"))

    (define (select-page vert horiz) ; in units of one page length or width
      (let ((v (* (/ 297 25.4) 72))
				(h (* (/ 210 25.4) 72))
				)
		  (set! pageoffset (list (* horiz h) (* vert v)))
		  ))
	 

    (define (start-page label number)
      (display "%%Page: " file)
      (display label file)
      (display " " file)
      (display number file)
      (display "\n" file)
      (apply translate pageoffset)
      (apply scale pagescale)
      )
    
    (define (end-page)
      #t
;      (apply translate pageoffset)
;      (apply scale pagescale)
      )

    (define (trailer)
      (ps-display "%%Trailer"))
    
    (define (gsave)
      (ps-display "gsave"))

    (define (grestore)
      (ps-display "grestore"))
    
    (define (lineto x)
      (ps-pair-or-list "lineto" x))
    
    (define (rlineto x)
      (ps-pair-or-list "rlineto" x))
    
    (define (moveto x)
      (ps-pair-or-list "moveto" x))
    
    (define (rmoveto x)
      (ps-pair-or-list "rmoveto" x))
    
    (define (closepath)
      (ps-display "closepath"))

    (define (newpath)
      (ps-display "newpath"))

    (define (exch)
      (ps-display "exch"))

	 (define (currentpoint)
		(ps-display "currentpoint"))

	 (define (stringwidth s)
		(show s)
		(ps-display string-width))

	 (define (stringwidth* lst)
		(show (string-append (map object->string lst)))
		(ps-display string-width))

	 (define (lineskip #!optional specific)
		(if (not specific) 
			 (set! specific "OgHqQ")
			 (cond
			  ((string? specific) #t)
			  ((list? specific) (set! specific (string-append map object->string specific)))
			  (else (set! specific (object->string specific)))))

		(gsave)
		(show-charpath 'true specific)
		(ps-display " exch pop exch sub exch pop %%%% This should be the line height\n")
		(grestore)
		)

	 (define (lineskip* lst)
		(show (string-append (map object->string lst)))
		(lineskip))

    (define (setlinewidth weight)
      (ps-1-arg "setlinewidth" weight))
    
    (define (setgray weight)
      (ps-1-arg "setgray" weight))
    
    (define (stroke)
      (ps-display "stroke"))
    
    (define (fill)
      (ps-display "fill"))

    (define (rotate angle)
      (ps-1-arg "rotate" angle))

    (define (scale x y)
      (ps-2-arg "scale" x y))

    (define (translate x y)
      (ps-2-arg "translate" x y))

    (define (arc cx cy rad startangle endangle)
      (ps-5-arg "arc" cx cy rad startange endangle))

    (define (arcn cx cy rad startangle endangle)
      (ps-5-arg "arcn" cx cy rad startange endangle))

    (define (map-character c)
      (if (not (char? c))
			 c
			 (cond 
			  ;;((eq? c #\ht) "\\t")
			  ((eq? c #\tab) "\\t")
			  ((eq? c #\newline) "\\n")
			  ((eq? c #\return) "\\t")
			  ((eq? c #\)) "\\)")
			  ((eq? c #\() "\\(")
			  (#t c))))

    (define (show-map string)
      (map map-character (string->list string)))
	 
    (define (show tlist)
      (if (null? fonts)
			 #f
			 (begin
				(cond 
				 ((string? tlist)
				  (display (string-append "(" tlist ") show\n") file))
				 ((number? tlist)
				  (display (string-append "(" (number->string tlist) ") show\n") file))
				 ((list? tlist) (map show tlist)))
				))
      )

	 (define (show! tlist) ;; keeps the current pointer in the place it starts (at the beginning of the string)
		(ps-display " currentpoint")
		(show tlist)
		(ps-display "moveto"))


    (define (show-charpath mode tlist)
      (if (null? fonts)
			 #f
			 (begin
				(cond 
				 ((string? tlist)
				  (display (string-append "(" tlist ")" (if mode " true " " false ") "charpath pathbbox\n") file))
				 ((number? tlist)
				  (display (string-append "(" (number->string tlist) ")" (if mode " true " " false ") "charpath pathbbox\n") file))
				 ((list? tlist) (map show tlist)))
				))
      )

	 (define (make-place name)
		(if (symbol? name) (set! name (symbol->string)))
		(ps-display (string-append "currentpoint\n/place-" name "-y exch def\n/place-" name "-x exch def"))
		)

	 (define (set-place name)
		(if (symbol? name) (set! name (symbol->string)))
		(ps-display "currentpoint")
		(ps-display (string-append "/place-" name "-y exch store\n/place-" name "-x exch store"))
		)

	 (define (place name)
		(if (symbol? name) (set! name (symbol->string)))
		(ps-display (string-append "/place-" name "-x load\n/place-" name "-y load")))

	 (define (column name)
		(if (symbol? name) (set! name (symbol->string)))
		(ps-display (string-append "/place-" name " load")))

	 (define (row name)
		(if (symbol? name) (set! name (symbol->string)))
		(ps-display (string-append "/place-" name " load")))

	 (define (linefeed n)
		(if (string? n) (set! n (string->number n)))
		(ps-display " 0 ")
		(ps-display " -1.25 ")
		(ps-display n)
		(lineskip)
		(ps-display " mul mul rmoveto"))

    (define (show-centered tlist)
		;;% string x y
		;;/center {moveto dup stringwidth pop -2 div 0 rmoveto show} def

      (gsave)
      (newpath)
      (moveto '(0 0))
      (show-charpath 'true tlist)
      (grestore)
      (ps-display "/scury exch def /scurx exch def /sclly exch def /scllx exch def\n")
      (ps-display "scllx scurx sub 2 div 0 rmoveto\n")
      (show tlist)
      )

	 (define (show-centered! tlist) ;; keeps the current pointer in the place it "starts" (at the centre)
      (gsave)
      (newpath)
      (moveto '(0 0))
      (show-charpath 'true tlist)
      (grestore)
      (ps-display "/scury exch def /scurx exch def /sclly exch def /scllx exch def\n")
      (ps-display "scllx scurx sub 2 div 0 rmoveto\n")
      (show! tlist)
		)

    (define (show-right tlist)
      (gsave)
      (newpath)
      (moveto '(0 0))
      (show-charpath 'true tlist)
      (grestore)
      (ps-display "/scury exch def /scurx exch def /sclly exch def /scllx exch def\n")
      (ps-display "scllx scurx sub 0 rmoveto\n")
      (show tlist)
      )

    (define (show-right! tlist) ;; Keeps the pointer at the beginning of the string, but 
      (gsave)                   ;; writes right to left (keeping the characters in the normal
      (newpath)                 ;; order)
      (moveto '(0 0))
      (show-charpath 'true tlist)
      (grestore)
      (ps-display "/scury exch def /scurx exch def /sclly exch def /scllx exch def\n")
      (ps-display "scllx scurx sub 0 rmoveto\n")
      (show! tlist)
      )

	 (define (show-table tlist)
		(for-each 
		 (lambda (line)
			(currentpoint)
			(show line)
			(ps-display " moveto")
			(linefeed 1)
			)
		 tlist))
	 

    (emit-header)

	 (letrec ((postscript-handle 
				  (lambda x
					 (if (null? x)
						  #f
						  (let ((cmd (car x))
								  (args (cdr x)))
							 (cond
							  ((eq? cmd 'file) file)
							  ((eq? cmd 'close) 
								(trailer)
								(display "%%Pages: " file)
								(display pagecount file)
								(display "\n" file)
								(close-output-port file))

							  ((eq? cmd 'postscript) (apply ps-display args))
							  ((eq? cmd 'comment) 
								(apply ps-display (append (list "\n%%\n%% ") (map make-it-a-string args) (list "\n%%\n")))
								)

							  ((eq? cmd 'font) (apply font args))
							  ((eq? cmd 'Times-Roman) (apply font "Times-Roman" args))
							  ((eq? cmd 'Times-Italic) (apply font "Times-Italic" args))
							  ((eq? cmd 'Times-Bold) (apply font "Times-Bold" args))
							  ((eq? cmd 'Helvetica) (apply font "Helvetica" args))
							  ((eq? cmd 'Helvetica-Italic) (apply font "Helvetica-Italic" args))
							  ((eq? cmd 'Helvetica-Bold) (apply font "Helvetica-Bold" args))
							  
							  ((eq? cmd 'show) (apply show args))
							  ((eq? cmd 'show!) (apply show! args))
							  ((eq? cmd 'show-charpath) (apply show-charpath args))
							  ((eq? cmd 'show-centered) (apply show-centered args))
							  ((eq? cmd 'show-right) (apply show-right args))
							  ((eq? cmd 'show-centered!) (apply show-centered! args))
							  ((eq? cmd 'show-right!) (apply show-right! args))

							  ((eq? cmd 'show-table) (apply show-table args))

							  ((eq? cmd 'start-page) (apply start-page args))
							  ((eq? cmd 'end-page) (apply end-page args))

							  ((eq? cmd 'gsave) (gsave))
							  ((eq? cmd 'grestore) (grestore))
							  ((eq? cmd 'showpage) (showpage))

							  ((eq? cmd 'moveto) (moveto args))
							  ((eq? cmd 'rmoveto) (rmoveto args))
							  ((eq? cmd 'lineto) (lineto args))
							  ((eq? cmd 'rlineto) (rlineto args))
							  ((eq? cmd 'closepath) (closepath))
							  ((eq? cmd 'newpath) (newpath))
							  ((eq? cmd 'exch) (exch))

							  ((eq? cmd 'lineweight) (apply lineweight args))
							  ((eq? cmd 'grey) (apply grey args))
							  ((eq? cmd 'setlinewidth) (apply setlinewidth args))
							  ((eq? cmd 'setgray) (apply setgray args))
							  ((eq? cmd 'stroke) (stroke))
							  ((eq? cmd 'fill) (fill))

							  ((eq? cmd 'rotate) (apply rotate args))
							  ((eq? cmd 'translate) (apply translate args))
							  ((eq? cmd 'scale) (apply scale args))

							  ((eq? cmd 'arc) (apply arc args))
							  ((eq? cmd 'arcn) (apply arcn args))
							  ((eq? cmd 'pages) pagecount)
							  ((eq? cmd 'define-units) (define-unitnames))

							  ((eq? cmd 'make-place) (apply make-place args))
							  ((eq? cmd 'set-place) (apply set-place args))
							  ((eq? cmd 'place) (apply place args))
							  ((eq? cmd 'column) (apply column args))
							  ((eq? cmd 'row) (apply row args))
							  ((eq? cmd 'linefeed) (apply linefeed args))

							  (#t (map display cmd " is not recognised\n")))
							 ))
					 )))
		postscript-handle
		)
    ))


;; For example
(define (graph-paper ps gridsize)
  (let* ((g (* 72 (/ gridsize 25.4)))
			(W (* (/ 210 25.4) 72))
			(H (* (/ 297 25.4) 72))
			(w W)
			(h H)
			)

    (set! w (* g (- (round (/ W g)) 4)))
    (set! h (* g (- (round (/ H g)) 8)))

    (ps 'setgray 0.5)
    (ps 'setlinewidth 0.072)

    (ps 'translate (/ (- W w) 2.0) (/ (- H h) 2.0))

    (let first ((i 0))
      (if (<= (* i g) w)
			 (begin
				(ps 'moveto (* i g) 0)
				(ps 'rlineto 0 h)
				(first (1+ i))
				))
      )
    (let second ((i 0))
      (if (<= (* i g) h)
			 (begin
				(ps 'moveto 0 (* i g))
				(ps 'rlineto w 0)
				(second (1+ i))
				))
		
      )
    (ps 'stroke)
    )
  ps
  )



(define (make-graph-paper filename gridsize)
  (let* ((ps (make-ps filename '())))
    (graph-paper ps gridsize)
    (ps 'showpage)
    (ps 'close)
	 ))

(define (plot-polygon ps border weight vlist . open-polygon)
  (ps 'newpath)
  (ps 'moveto (car vlist))
  (let loop ((v (cdr vlist)))
    (if (null? v)
		  (if (or (null? open-polygon) (not (car open-polygon))) (ps 'closepath))
		  (if (and (list? v) 
					  (list? (car v)) 
					  (not (null? (car v))) 
					  (apply andf (map number? (car v))))
				(begin
				  (ps 'lineto (caar v) (cadar v))
				  (loop (cdr v))))))

  (ps 'setgray weight)  
  (ps 'setlinewidth border)
  (ps 'stroke)
  )

(define (plot-filled-polygon ps border bfill pfill vlist)
  (ps 'newpath)
  (ps 'moveto (car vlist))
  (let loop ((v (cdr vlist)))
    (if (null? v)
		  (begin
			 (ps 'closepath)
			 (ps 'gsave))
		  (if (and (list? v) 
					  (list? (car v)) 
					  (not (null? (car v))) 
					  (apply andf (map number? (car v))))
				(	begin
				  (ps 'lineto (caar v) (cadar v))
				  (loop (cdr v))))))
  (ps 'setgray pfill)
  (ps 'fill)
  (ps 'grestore)
  (ps 'setgray bfill)
  (ps 'setlinewidth border)
  (ps 'stroke)
  )

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
#|

This serves as a template for other population agents, as well as the population machinery for
the dynamic patch class.

|#




;;========================================================================;;
;;
;;                      Constants and such
;;
;;========================================================================;;


(define unbounded +inf.0)

;;========================================================================;;
;;
;;                      TERMS FOR THE dP/dt expression
;;
;;========================================================================;;


;; Multiplicative cap on the growth of the species. k = "unbounded" makes it go away 
;;(define (logistic-- a k) (if (eq? k unbounded) 1.0 (- 1 (pow (/ a k) 2.4))))

(define (logistic-- a k) (if (eq? k unbounded) 1.0 (- 1 (/ a k))))

(define (logistic-growth-- a k) (if (> a k) 0.0 (- 1.0 (/ a k))))
(define (logistic-mort-- a k) (if (< a k) 0.0 (- (/ a k) 1.0)))

;; sums the contributors to population growth
(define growth-terms-- +)

;; mortality-terms should be included as a modifier within a growth-terms s-exp
(define (mortality-terms-- . args) (- 0 (apply + args)))


;;========================================================================;;


#|
The usual pattern for a std-d/dt would be 

(make-basic-population 'grass '(grass rabbit fox) ...)

|#


(define (make-basic-population name population-names . rest)
  (let* ((name name)
			(self #f)
			(nrest (length rest))
			(popnames population-names)
			(populations #f)
			(growth (if (>= nrest 1) 0 (list-ref rest 0)))
			(nmort (if (>= nrest 2) 0 (list-ref rest 1)))
			(cap (if (>= nrest 3) unbounded (list-ref rest 2)))
			(val (if (>= nrest 4) 0 (list-ref rest 3)))
			(logistic #f)
			(logistic-growth #f)
			(logistic-mort #f)
			(prey-rates #f) ;; attack-rate and efficiency (in that order)
			(pred-att-rate #f) ;; predator's attack-rate is obtained from the predator since it may change
			(helper-rate #f)
			(competitor-rate #f)

			(fascist-init #f)
			)
	 (let* ((preys-on (lambda (vals att-rate eff)
							  (apply + (map * vals att-rate eff))))

			  (preyed-on-by (lambda (vals att-rate)
									(apply + (map * vals att-rate))))

			  ;; These are like the predation terms, but without any symmetry
			  (helped-by (lambda (vals helper-rate)
								(apply + (map * vals helper-rate))))
			  
			  (competes-with (lambda (vals competitor-rate)
									 (apply + (map * vals competitor-rate))))


			  (d/dt 
				(if (zero? nrest)
;(lambda args (abort-with-stack (string-append "d/dt is uninitialised: use the 'set-d/dt! call to set it")))
					 (lambda args (abort (string-append "d/dt is uninitialised: use the 'set-d/dt! call to set it")))
					 
					 (lambda (t . pvals)
						;; t is the first element in the list, other elements must correspond to the populations indicated in the "populations" list
						;; when the population is created
						;;
						;; vals ought to be in order
						(let ((val (list-ref pvals (- (length popnames) (length (member name popnames))))))
						  (if #f
								(begin
								  (dnl name "-----------------------")
								  (dnl "value ====> " val)
								  (dnl "growth = " growth)
								  (dnl "preys-on = " (preys-on pvals (map car prey-rates) (map cadr prey-rates)))
								  (dnl "total mortality =  " (+ nmort  (if pred-att-rate (preyed-on-by pvals (map car pred-att-rate)) 0)))
								  (dnl "natural mortality = " nmort)
								  (dnl "preyed-on-by = " (preyed-on-by pvals (if pred-att-rate (map car pred-att-rate) 0)))

								  (if logistic (dnl "logistic " logistic "*" (logistic-- val cap)))
								  (if logistic-growth (dnl "logistic-growth " logistic-growth "*" (logistic-growth-- val cap)))
								  (if logistic-mort (dnl "logistic-mort " logistic-mort "*" (logistic-mort-- val cap)))
								  
								  (dnl)))


						  (* val 
							  (if logistic (* logistic (logistic-- val cap)) 1.0)
							  (growth-terms-- growth 
													(if prey-rates
														 (preys-on pvals (map car prey-rates) (map cadr prey-rates))
														 0)

													(if helper-rate (helped-by pvals helper-rate) 0)

													(mortality-terms-- nmort 
																			 (if pred-att-rate
																				  (preyed-on-by pvals (map car pred-att-rate))
																				  0)
																			 (if competitor-rate (competes-with pvals competitor-rate) 0)
																			 )))
						  ))
					 )
				)
			  (accessor
				(letrec ((population (lambda args
											  (cond
												((null? args)
												 val)

												((eq? (car args) 'dump)
												 (pp (list
														'name name 
														'self self 
														'pops popnames populations 
														'params val cap growth nmort
														'prey-rates prey-rates
														'pred-att-rate pred-att-rate 
														'logistic/-growth/-mort logistic logistic-growth logistic-mort
														'd/dt d/dt 
														'func population)))

												((eq? (car args) 'set!)
												 (set! val  (cadr args)))
												
												((eq? (car args) 'register-populations)  ;; expects (animal 'register-populations (list plant animal toothy-animal....))
												 (if (not (apply andf (map procedure? (cadr args))))
													  (Abort))
												 (if (null? (cdr args))
													  (Abort "No populations?  What's the point?")
													  (set! populations (copy-list (cadr args)))
													  ))

												((eq? (car args) 'register-prey)
												 (if (not populations)
													  (Abort "You must register the populations before registering the prey")
													  (begin
														 (set! prey-rates (make-list (length populations) '(0 0)))
														 (if (not (null? (cdr args)))
															  (begin
																 (for-each
																  (lambda (x y)
																	 (list2-assoc-set! populations prey-rates x y))
																  (map car (cdr args)) (map cdr (cdr args)))
																 )
															  )
														 )
													  ))

												((eq? (car args) 'register-predators)
												 (if (not prey-rates) (abort (string-append (symbol->string name) " has been called before both the prey list has been registered")))
												 (set! pred-att-rate (map (lambda (y) (y 'attack-rate self)) populations))
												 (if (zero? (apply + (map abs (map car pred-att-rate))))
													  (set! pred-att-rate #f))
												 ;;(dnl name pred-att-rate)
												 )

												((eq? (car args) 'logistic)
												 (if (not (or (null? (cdr args)) (boolean? (cadr args)) (number? (cadr args))))
													  (abort "argument to (entity 'logistic) must be null, #f #t or a number"))
												 (if (null? (cdr args))
													  (set! logistic 1.0)
													  (set! logistic (cadr args)))

												 (if logistic
													  (begin
														 (set! logistic-growth #f)
														 (set! logistic-mort #f)))

												 ;;(dnl name " Logistic: " logistic logistic-growth logistic-mort)

												 )

												((eq? (car args) 'register-helpers)
												 (if (not populations)
													  (Abort "You must register the populations before registering interactions")
													  (begin
														 (set! helper-rate (make-list (length populations) 0))
														 (if (not (null? (cdr args)))
															  (begin
																 (for-each
																  (lambda (x y)
																	 (list2-assoc-set! populations helper-rate x y))
																  (map car (cdr args)) (map cadr (cdr args)))
																 )
															  )
														 )
													  ))

												((eq? (car args) 'register-competitors)
												 (if (not populations)
													  (Abort "You must register the populations before registering interactions")
													  (begin
														 (set! competitor-rate (make-list (length populations) 0))
														 (if (not (null? (cdr args)))
															  (begin
																 (for-each
																  (lambda (x y)
																	 (list2-assoc-set! populations competitor-rate x y))
																  (map car (cdr args)) (map cadr (cdr args)))
																 )
															  )
														 )
													  ))

												((eq? (car args) 'logistic-growth)
												 (if (not (or (null? (cdr args)) (boolean? (cadr args)) (number? (cadr args))))
													  (abort "argument to (entity 'logistic-growth) must be null, #f #t or a number"))
												 (if (null? (cdr args))
													  (set! logistic-growth 1.0)
													  (set! logistic-growth (cadr args)))
												 (if logistic-growth (set! logistic #f))

												 ;;(dnl name " Logistic-growth: " logistic logistic-growth logistic-mort)
												 )

												((eq? (car args) 'logistic-mort)
												 (if (not (or (null? (cdr args)) (boolean? (cadr args)) (number? (cadr args))))
													  (abort "argument to (entity 'logistic-mort) must be null, #f #t or a number"))
												 (if (null? (cdr args))
													  (set! logistic-mort 1.0)
													  (set! logistic-mort (cadr args)))
												 (if logistic-mort (set! logistic #f))

												 ;;(dnl name " Logistic-mort: " logistic logistic-growth logistic-mort)
												 )

												((and (eq? (car args) 'attack-rate) (procedure? (cadr args)))
												 (if (not prey-rates) 	
													  (if fascist-init
															(abort (string-append (symbol->string name) " has been called before both the prey list has been registered"))
															0)
													  (let ((p (list2-assoc populations prey-rates (cadr args))))
														 (if p (car p) 0))))

												((eq? (car args) 'update)
												 (let ((dP (apply self (cdr args))))
													(set! value (+ value (* dP (cadr args))))
													))

												((eq? (car args) 'set-d/dt!)
												 (set! d/dt (cadr args)))

												 ((eq? (car args) 'd/dt)
												  d/dt)
												 
												 ((number? (car args))
												  (apply d/dt args))
												 
												 (else (abort (string-append "The argument to " (symbol->string name) ", " (object->string (car args)) ", is not recognised")))
												 ) ;; cond
												) ;; lambda
											  ) ;; population defn
							) ;; letrec closure
				  population) ;; letrec 
				) ;; accessor definition
			  ) ;; let* closure
		(set! self accessor) ;; so we can identify ourselves to others
		accessor) ;; return accessor function for the population
	 ) ;; outer let*
  )


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
; Mode: Scheme
;
;
; *************************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; *************************************************************************
;
;
; Scheme is such a wonderful language, you can't program in it!
;
; This is a library of stuff I find useful.  I'll bet there's dozens
; of these out there.
;

;
; In order to make this code more easily portable, we have to be
; explicit about its implementation dependencies.  To do this, we
; have the following variable.  Please adjust it before trying to
; run this code.  See also the macro, scheme-implementation-case,
; which follows shortly.
;
; Note that some of these dependencies (i.e. gsort) are purely for
; convenience (i.e. saving me from writing sort from scratch).
; Others are more pressing, like define-macro.
;
;

(require 'sort)

;(import ../srfi/sorting)

;(declare (standard-bindings)
;	 (extended-bindings)
;	 (block)
;	 ;(not safe)
;	 )


(define gsort (lambda (predicate list) (sort list predicate)))

;; (define simple-printer (lambda () barf))

(define ??? 'unspecified-result)

(define list*
  (lambda args
    (letrec ((chase
	      (lambda (args)
		(cond ((null? args) '())
		      ((null? (cdr args)) (car args))
		      (else (cons (car args) (chase (cdr args))))))))
      (chase args))))

(define apply*
    (lambda (proc . args)
      (apply proc (apply list* args))))

(define position-of
    (lambda (x lst)
      (if (eq? x (car lst)) 0 (+ 1 (position-of x (cdr lst))))))

(define map-append
    (lambda (proc . lists)
      (apply append (apply map (cons proc lists)))))

(define last
    (lambda (l)
      (if (null? l)
	  #f
	  (if (null? (cdr l))
	      (car l)
	      (last (cdr l))))))

(define every
    (lambda (test . lists)
      (let scan ((tails lists))
	(if (member #t (map null? tails))             ;(any null? lists)
	    #t
	    (and (apply test (map car tails))
		 (scan (map cdr tails)))))))

(define remove
    (lambda (x list)
      (cond ((null? list) '())
	    ((eq? (car list) x) (cdr list))
	    (else (cons (car list) (remove x (cdr list)))))))

(define getl
    (lambda (initargs name . not-found)
      (letrec ((scan (lambda (tail)
		       (cond ((null? tail)
			      (if (pair? not-found)
				  (car not-found)
				  (error "GETL couldn't find" name)))
			     ((eq? (car tail) name) (cadr tail))
			     (else (scan (cddr tail)))))))
	(scan initargs))))

(define union
    (lambda lists
      (letrec ((clean (lambda (list result)
			(cond ((null? list) result)
			      ((memq (car list) result)
			       (clean (cdr list) result))
			      (else
			       (clean (cdr list) (cons (car list) result)))))))
	(clean (apply append lists) '()))))

(define collect-if
    (lambda (test? list)
      (cond ((null? list) '())
	    ((test? (car list)) (cons (car list) (collect-if test? (cdr list))))
	    (else (collect-if test? (cdr list))))))

;(define remove-unless
;    (lambda (test list)
;      (if (null? list)
;	  ()
;	  (let ((rest (remove-unless test (cdr list))))
;	    (if (test (car list))
;		(cons (car list) rest)
;		rest)))))

(define remove-duplicates
    (lambda (list)
      (let loop ((result-so-far '())
		 (remaining list))
	   (if (null? remaining)
	       result-so-far
	       (if (null? (memq (car remaining) result-so-far))
		   (loop (cons (car remaining) result-so-far)
			 (cdr remaining))
		   (loop result-so-far
			 (cdr remaining)))))))




;
; A simple topological sort.
;
; It's in this file so that both TinyClos and Objects can use it.
;
; This is a fairly modified version of code I originally got from Anurag
; Mendhekar <anurag@moose.cs.indiana.edu>.
;
;

(define compute-std-cpl
    (lambda (c get-direct-supers)
      (top-sort ((build-transitive-closure get-direct-supers) c)
		((build-constraints get-direct-supers) c)
		(std-tie-breaker get-direct-supers))))


(define top-sort
    (lambda (elements constraints tie-breaker)
      (let loop ((elements    elements)
		 (constraints constraints)
		 (result      '()))
	(if (null? elements)
	    result
	    (let ((can-go-in-now
		    (collect-if
		      (lambda (x)
			(every (lambda (constraint)
				 (or (not (eq? (cadr constraint) x))
				     (memq (car constraint) result)))
			       constraints))
		      elements)))
	      (if (null? can-go-in-now)
		  (error 'top-sort "Invalid constraints")
		  (let ((choice (if (null? (cdr can-go-in-now))
				    (car can-go-in-now)
				    (tie-breaker result
						 can-go-in-now))))
		    (loop
		      (collect-if (lambda (x) (not (eq? x choice)))
			          elements)
		      constraints
		      (append result (list choice))))))))))

(define std-tie-breaker
    (lambda (get-supers)
      (lambda (partial-cpl min-elts)
	(let loop ((pcpl (reverse partial-cpl)))
	     (let ((current-elt (car pcpl)))
	       (let ((ds-of-ce (get-supers current-elt)))
		 (let ((common (collect-if (lambda (x)
					     (memq x ds-of-ce))
					   min-elts)))
		   (if (null? common)
		       (if (null? (cdr pcpl))
			   (error 'std-tie-breaker "Nothing valid")
			   (loop (cdr pcpl)))
		       (car common)))))))))


(define build-transitive-closure
    (lambda (get-follow-ons)
      (lambda (x)
	(let track ((result '())
		    (pending (list x)))
	     (if (null? pending)
		 result
		 (let ((next (car pending)))
		   (if (memq next result)
		       (track result (cdr pending))
		       (track (cons next result)
			      (append (get-follow-ons next)
				      (cdr pending))))))))))

(define build-constraints
  (lambda (get-follow-ons)
    (lambda (x)
      (let loop ((elements ((build-transitive-closure get-follow-ons) x))
		 (this-one '())
		 (result '()))
	   (if (or (null? this-one) (null? (cdr this-one)))
	       (if (null? elements)
		   result
		   (loop (cdr elements)
			 (cons (car elements)
			       (get-follow-ons (car elements)))
			 result))
	       (loop elements
		     (cdr this-one)
		     (cons (list (car this-one) (cadr this-one))
			   result)))))))


;
; **********************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; **********************************************************************
;
; EDIT HISTORY:
;
;      10/**/92  Gregor  Originally Written
; 1.0  11/10/92  Gregor  Changed names of generic invocation generics.
;                        Changed compute-getters-and-setters protocol.
;                        Made comments match the code.
;                        Changed maximum line width to 72.
; 1.1  11/24/92  Gregor  Fixed bug in compute-method-more-specific?,
;                        wrt the use of for-each.
;                        Both methods on allocate instance failed to
;                        initialize fields properly.
;                        The specializers and procedure initargs are
;                        now required when creating a method, that is,
;                        they no longer default.  No working program
;                        should notice this change.
; 1.2  12/02/92  Gregor  Fix minor things that improve portability:
;                         - DEFINE needs 2 args in R4Rs
;                         - Conditionalize printer hooks.
;                         - () doesn't evaluate to ()
;
; 1.3  12/08/92  Gregor  More minor things:
;                         - () really doesn't evaluate to () damnit!
;                         - It turns out DEFINE-MACRO is never used.
;                         - Confusion over the "failure" return value
;                           of ASSQ -- ASSQ returns #f if the key is
;                           not found.
;                         - SEQUENCE   --> BEGIN
;                         - LAST-PAIR  --> last now in support
;                        Change instance rep to protect Schemes that
;                        don't detect circular structures when
;                        printing.
;                        A more reasonable error message when there
;                        are no applicable methods or next methods.
; 1.4  12/10/92  Gregor  Flush filter-in for collect-if.  Add news
;                        classes <input-port> and <output-port>.
;                        Also add 
;
; 1.5  12/17/92  Gregor  Minor changes to class of and primitive
;                        classes to try and deal with '() and #f
;                        better.
;
; 1.6   9/9/93   Gregor  Fix a monstrous bug in the bootstrap of
;                        compute-apply-generic which sometimes ran
;                        user methods on this generic function when
;                        it shouldn't.
;
; 1.7   8/9/94   Gregor  Add Scheme 48 to support.scm.
;
;
;

;;; (import support)

;;; (export
;;;  make-class
;;;  make-generic
;;;  make-method
;;;  add-method
;;;  make
;;;  initialize
;;;  slot-ref
;;;  slot-set!
;;;  class-of
;;;  class-direct-supers
;;;  class-direct-slots
;;;  class-cpl
;;;  class-slots
;;;  generic-methods
;;;  method-specializers
;;;  method-procedure
;;;  allocate-instance
;;;  initialize
;;;  compute-cpl
;;;  compute-slots
;;;  compute-getter-and-setter
;;;  compute-apply-generic
;;;  compute-methods
;;;  compute-method-more-specific?
;;;  compute-apply-methods
;;;  <pair>
;;;  <null>
;;;  <boolean>
;;;  <symbol>
;;;  <procedure>
;;;  <number>
;;;  <vector>
;;;  <char>
;;;  <string>
;;;  <input-port>
;;;  <output-port>
;;;  <class>
;;;  <top>
;;;  <object>
;;;  <procedure-class>
;;;  <entity-class>
;;;  <generic>
;;;  <method>
;;;  (re-export: support)
;;;  )

;;; (declare (standard-bindings)
;;; 	 (extended-bindings)
;;; 	 (block)
;;; 	 ;(not safe)
;;; 	 )

(define tiny-clos-version "1.7")

'(;Stuff to make emacs more reasonable.

  (put 'letrec 'lisp-indent-hook 1)

  (put 'make-method  'lisp-indent-hook 1)
  (put 'add-method   'lisp-indent-hook 'defun)

 )
;
; A very simple CLOS-like language, embedded in Scheme, with a simple
; MOP.  The features of the default base language are:
;
;   * Classes, with instance slots, but no slot options.
;   * Multiple-inheritance.
;   * Generic functions with multi-methods and class specializers only.
;   * Primary methods and call-next-method; no other method combination.
;   * Uses Scheme's lexical scoping facilities as the class and generic
;     function naming mechanism.  Another way of saying this is that
;     class, generic function and methods are first-class (meta)objects.
;
; While the MOP is simple, it is essentially equal in power to both MOPs
; in AMOP.  This implementation is not at all optimized, but the MOP is
; designed so that it can be optimized.  In fact, this MOP allows better
; optimization of slot access extenstions than those in AMOP.
; 
;
;
; In addition to calling a generic, the entry points to the default base
; language are:
;
;   (MAKE-CLASS list-of-superclasses list-of-slot-names)
;   (MAKE-GENERIC)
;   (MAKE-METHOD list-of-specializers procedure)
;   (ADD-METHOD generic method)
;
;   (MAKE class . initargs)
;   (INITIALIZE instance initargs)            ;Add methods to this,
;                                             ;don't call it directly.
;   
;   (SLOT-REF  object slot-name)
;   (SLOT-SET! object slot-name new-value)
;
;
; So, for example, one might do:
;
;   (define <position> (make-class (list <object>) (list 'x 'y)))
;   (add-method initialize
;       (make-method (list <position>)
;         (lambda (call-next-method pos initargs)
;           (for-each (lambda (initarg-name slot-name)
;                       (slot-set! pos
;                                  slot-name
;                                  (getl initargs initarg-name 0)))
;                     '(x y)
;                     '(x y)))))
;
;   (set! p1 (make <position> 'x 1 'y 3))
;
;
;
; NOTE!  Do not use EQUAL? to compare objects!  Use EQ? or some hand
;        written procedure.  Objects have a pointer to their class,
;        and classes are circular structures, and ...
;
;
;
; The introspective part of the MOP looks like the following.  Note that
; these are ordinary procedures, not generics.
;
;   CLASS-OF
;
;   CLASS-DIRECT-SUPERS
;   CLASS-DIRECT-SLOTS
;   CLASS-CPL
;   CLASS-SLOTS
;
;   GENERIC-METHODS
;
;   METHOD-SPECIALIZERS
;   METHOD-PROCEDURE
;
;
; The intercessory protocol looks like (generics in uppercase):
;
;   make                        
;     ALLOCATE-INSTANCE
;     INITIALIZE                   (really a base-level generic)
;
;   class initialization
;     COMPUTE-CPL
;     COMPUTE-SLOTS
;     COMPUTE-GETTER-AND-SETTER
;
;   add-method                     (Notice this is not a generic!)
;     COMPUTE-APPLY-GENERIC
;       COMPUTE-METHODS
;         COMPUTE-METHOD-MORE-SPECIFIC?
;       COMPUTE-APPLY-METHODS
;

;
; OK, now let's get going.  But, as usual, before we can do anything
; interesting, we have to muck around for a bit first.  First, we need  
; to load the support library.
;
; Note that there is no extension on the filename in the following load,
; in particular, it isn't "support.scm" even though that is the name of
; the file in the distribution at PARC.  The idea is that when people
; install the code at their site, they should rename all the files to
; the appropriate extension, and then not change the load.  This should
; also make things work with binary files and the like.  This comes from
; my understanding of the CL world...  I hope it is right.
;
;

;
; Then, we need to build what, in a more real implementation, would be
; the interface to the memory subsystem: instances and entities.  The
; former are used for instances of instances of <class>; the latter
; are used for instances of instances of <entity-class>.  In this MOP,
; none of this is visible to base- or MOP-level programmers.
;
; A few things to note, that have influenced the way all this is done:
;  
;   - R4RS doesn't provide a mechanism for specializing the
;     behavior of the printer for certain objects.
;     
;   - Some Scheme implementations bomb when printing circular
;     structures -- that is, arrays and/or lists that somehow
;     point back to themselves.
;
; So, the natural implementation of instances -- vectors whose first
; field point to the class -- is straight on out.  Instead, we use a
; procedure to `encapsulate' that natural representation.
;
; Having gone that far, it makes things simpler to unify the way normal
; instances and entities are handled, at least in the lower levels of
; the system.  Don't get faked out by this -- the user shouldn't think
; of normal instances as being procedures, they aren't. (At least not
; in this language.)  If you are using this to teach, you probably want
; to hide the implementation of instances and entities from people.
;
;
(define %allocate-instance
    (lambda (class nfields)
      (%allocate-instance-internal
       class
       #t
       (lambda args
	 (error "An instance isn't a procedure -- can't apply it."))
       nfields)))

(define %allocate-entity
    (lambda (class nfields)
      (%allocate-instance-internal
       class
       #f
       (lambda args
	 (error "Tried to call an entity before its proc is set."))
       nfields)))

(define %allocate-instance-internal ???)
(define %instance?                  ???)
(define %instance-class             ???)
(define %set-instance-class-to-self ???)   ;This is used only once
                                           ;as part of bootstrapping
                                           ;the braid.
(define %set-instance-proc!  ???)
(define %instance-ref        ???)
(define %instance-set!       ???)

(letrec ((instances '())
	 (get-vector
	  (lambda (closure)
	    (let ((cell (assq closure instances)))
	      (if cell (cdr cell) #f)))))

  (set! %allocate-instance-internal
	(lambda (class lock proc nfields)
	  (letrec ((vector (make-vector (+ nfields 3) #f))
		   (closure (lambda args
			      (apply (vector-ref vector 0) args))))
	    (vector-set! vector 0 proc)
	    (vector-set! vector 1 lock)
	    (vector-set! vector 2 class)
	    (set! instances (cons (cons closure vector) instances))
	    closure)))
		   
  (set! %instance?
        (lambda (x)
	  (let ((res (get-vector x)))
	    (and (not (null? res)) res))
	  ;(not (null? (get-vector x)))
	  ))

  (set! %instance-class
	(lambda (closure)
	  (let ((vector (get-vector closure)))
	    (vector-ref vector 2))))

  (set! %set-instance-class-to-self
	(lambda (closure)
	  (let ((vector (get-vector closure)))
	    (vector-set! vector 2 closure))))
		   
  (set! %set-instance-proc!
        (lambda (closure proc)
	  (let ((vector (get-vector closure)))
	    (if (vector-ref vector 1)
		(error "Can't set procedure of instance.")
		(vector-set! vector 0 proc)))))
	
  (set! %instance-ref
        (lambda (closure index)
	  (let ((vector (get-vector closure)))
	    (vector-ref vector (+ index 3)))))
		  
  (set! %instance-set!
        (lambda (closure index new-value)
	  (let ((vector (get-vector closure)))
	    (vector-set! vector (+ index 3) new-value))))
  )


;
; %allocate-instance, %allocate-entity, %instance-ref, %instance-set!
; and class-of are the normal interface, from the rest of the code, to
; the low-level memory system.  One thing to take note of is that the
; protocol does not allow the user to add low-level instance
; representations.  I have never seen a way to make that work.
;
; Note that this implementation of class-of assumes the name of a the
; primitive classes that are set up later.
; 
(define class-of
    (lambda (x)
      (cond ((%instance? x)  (%instance-class x))

	    ((pair? x)        <pair>)         ;If all Schemes were IEEE 
	    ((null? x)        <null>)         ;compliant, the order of
	    ((boolean? x)     <boolean>)      ;these wouldn't matter?
	    ((symbol? x)      <symbol>)
	    ((procedure? x)   <procedure>)
	    ((number? x)      <number>)
	    ((vector? x)      <vector>)
	    ((char? x)        <char>)
	    ((string? x)      <string>)
	    (( input-port? x)  <input-port>)
	    ((output-port? x) <output-port>)
	    

	    )))


;
; Now we can get down to business.  First, we initialize the braid.
;
; For Bootstrapping, we define an early version of MAKE.  It will be
; changed to the real version later on.  String search for ``set! make''.
;

(define make
    (lambda (class . initargs)
      (cond ((or (eq? class <class>)
		 (eq? class <entity-class>))
	     (let* ((new (%allocate-instance
			  class
			  (length the-slots-of-a-class)))
		    (dsupers (getl initargs direct-supers: '()))
		    (dslots  (map list
				  (getl initargs direct-slots:  '())))
		    (cpl     (let loop ((sups dsupers)
					(so-far (list new)))
				  (if (null? sups)
				      (reverse so-far)
				      (loop (class-direct-supers
					     (car sups))
					    (cons (car sups)
						  so-far)))))
		    (slots (apply append
				  (cons dslots
					(map class-direct-slots
					     (cdr cpl)))))
		    (nfields 0)
		    (field-initializers '())
		    (allocator
		      (lambda (init)
			(let ((f nfields))
			  (set! nfields (+ nfields 1))
			  (set! field-initializers
				(cons init field-initializers))
			  (list (lambda (o)   (%instance-ref  o f))
				(lambda (o n) (%instance-set! o f n))))))
		    (getters-n-setters
		      (map (lambda (s)
			     (cons (car s)
				   (allocator (lambda () '()))))
			   slots)))

	       (slot-set! new 'direct-supers      dsupers)
	       (slot-set! new 'direct-slots       dslots)
	       (slot-set! new 'cpl                cpl)
	       (slot-set! new 'slots              slots)
	       (slot-set! new 'nfields            nfields)
	       (slot-set! new 'field-initializers (reverse
						   field-initializers))
	       (slot-set! new 'getters-n-setters  getters-n-setters)
	       new))
	    ((eq? class <generic>)
	     (let ((new (%allocate-entity class
					  (length (class-slots class)))))
	       (slot-set! new 'methods '())
	       new))
	    ((eq? class <method>)
	     (let ((new (%allocate-instance
			 class
			 (length (class-slots class)))))
	       (slot-set! new
			  'specializers
			  (getl initargs specializers:))
	       (slot-set! new
			  'procedure
			  (getl initargs procedure:))
	       new)))))


;
; These are the real versions of slot-ref and slot-set!.  Because of the
; way the new slot access protocol works, with no generic call in line,
; they can be defined up front like this.  Cool eh?
;
;
(define slot-ref
    (lambda (object slot-name)
      (let* ((info   (lookup-slot-info (class-of object) slot-name))
	     (getter (list-ref info 0)))
	(getter object))))

(define slot-set!
    (lambda (object slot-name new-value)
      (let* ((info   (lookup-slot-info (class-of object) slot-name))
	     (setter (list-ref info 1)))
	(setter object new-value))))

(define lookup-slot-info
    (lambda (class slot-name)
      (let* ((getters-n-setters
	       (if (eq? class <class>)           ;* This grounds out
		   getters-n-setters-for-class   ;* the slot-ref tower.
		   (slot-ref class 'getters-n-setters)))
	     (entry (assq slot-name getters-n-setters)))
	(if entry
	    (cdr entry)
	    (error "No slot" slot-name "in instances of" class)))))



;
; Given that the early version of MAKE is allowed to call accessors on
; class metaobjects, the definitions for them come here, before the
; actual class definitions, which are coming up right afterwards.
;
;
(define class-direct-slots
    (lambda (class) (slot-ref class 'direct-slots)))
(define class-direct-supers
    (lambda (class) (slot-ref class 'direct-supers)))
(define class-slots
    (lambda (class) (slot-ref class 'slots)))
(define class-cpl
    (lambda (class) (slot-ref class 'cpl)))

(define generic-methods
    (lambda (generic) (slot-ref generic 'methods)))

(define method-specializers
    (lambda (method) (slot-ref method 'specializers)))
(define method-procedure
    (lambda (method) (slot-ref method 'procedure)))


;
; The next 7 clusters define the 6 initial classes.  It takes 7 to 6
; because the first and fourth both contribute to <class>.
;
(define the-slots-of-a-class     ;
    '(direct-supers              ;(class ...)        
      direct-slots               ;((name . options) ...)
      cpl                        ;(class ...) 
      slots                      ;((name . options) ...) 
      nfields                    ;an integer
      field-initializers         ;(proc ...)
      getters-n-setters))        ;((slot-name getter setter) ...)
                                 ;
(define getters-n-setters-for-class      ;see lookup-slot-info
    ;
    ; I know this seems like a silly way to write this.  The
    ; problem is that the obvious way to write it seems to
    ; tickle a bug in MIT Scheme!
    ;
    (let ((make-em (lambda (s f)
		     (list s
			   (lambda (o)   (%instance-ref  o f))
			   (lambda (o n) (%instance-set! o f n))))))
      (map (lambda (s)
	     (make-em s (position-of s the-slots-of-a-class)))
	   the-slots-of-a-class)))
(define <class> (%allocate-instance #f (length the-slots-of-a-class)))
(%set-instance-class-to-self <class>)

(define <top>          (make <class>
			     direct-supers: (list)
			     direct-slots:  (list)))

(define <object>       (make <class>
			     direct-supers: (list <top>)
			     direct-slots:  (list)))

;
; This cluster, together with the first cluster above that defines
; <class> and sets its class, have the effect of:
;
;   (define <class>
;     (make <class>
;           'direct-supers (list <object>)
;           'direct-slots  (list 'direct-supers ...)))
;
(slot-set! <class> 'direct-supers      (list <object>))
(slot-set! <class> 'direct-slots       (map list the-slots-of-a-class))
(slot-set! <class> 'cpl                (list <class> <object> <top>))
(slot-set! <class> 'slots              (map list the-slots-of-a-class))
(slot-set! <class> 'nfields            (length the-slots-of-a-class))
(slot-set! <class> 'field-initializers (map (lambda (s)
					      (lambda () '()))
					    the-slots-of-a-class))
(slot-set! <class> 'getters-n-setters  '())


(define <procedure-class> (make <class>
				direct-supers: (list <class>)
				direct-slots:  (list)))

(define <entity-class>    (make <class>
			        direct-supers: (list <procedure-class>)
			        direct-slots:  (list)))

(define <generic>         (make <entity-class>
			        direct-supers: (list <object>)
			        direct-slots:  (list 'methods)))

(define <method>          (make <class>
			        direct-supers: (list <object>)
			        direct-slots:  (list 'specializers 'procedure))) 


;
; These are the convenient syntax we expose to the base-level user.
;
;
(define make-class
    (lambda (direct-supers direct-slots)
      (make <class>
	    direct-supers: direct-supers
	    direct-slots:  direct-slots)))

(define make-generic
    (lambda ()
      (make <generic>)))

(define make-method
    (lambda (specializers procedure)
      (make <method>
	    specializers: specializers
	    procedure:    procedure)))




;
; The initialization protocol
;
(define initialize (make-generic))
	    

;
; The instance structure protocol.
;
(define allocate-instance (make-generic))
(define compute-getter-and-setter (make-generic))


;
; The class initialization protocol.
;
(define compute-cpl   (make-generic))
(define compute-slots (make-generic))

;
; The generic invocation protocol.
;
(define compute-apply-generic         (make-generic))
(define compute-methods               (make-generic))
(define compute-method-more-specific? (make-generic))
(define compute-apply-methods         (make-generic))

;
; The next thing to do is bootstrap generic functions.
; 
(define generic-invocation-generics (list compute-apply-generic
					  compute-methods
					  compute-method-more-specific?
					  compute-apply-methods))

(define add-method
    (lambda (generic method)
      (slot-set! generic
		 'methods
		 (cons method
		       (collect-if
			(lambda (m)
			  (not (every eq?
				      (method-specializers m)
				      (method-specializers method))))
			(slot-ref generic 'methods))))
      (%set-instance-proc! generic (compute-apply-generic generic))))

;
; Adding a method calls COMPUTE-APPLY-GENERIC, the result of which calls
; the other generics in the generic invocation protocol.  Two, related,
; problems come up.  A chicken and egg problem and a infinite regress
; problem.
;
; In order to add our first method to COMPUTE-APPLY-GENERIC, we need
; something sitting there, so it can be called.  The first definition
; below does that.
; 
; Then, the second definition solves both the infinite regress and the
; not having enough of the protocol around to build itself problem the
; same way: it special cases invocation of generics in the invocation
; protocol.
;
;
(%set-instance-proc! compute-apply-generic
     (lambda (generic)
       (let ((method (car (generic-methods generic))))
	 ((method-procedure method) #f generic))))

(add-method compute-apply-generic
    (make-method (list <generic>)
      (lambda (call-next-method generic)
	(lambda args
	  (if (and (memq generic generic-invocation-generics)     ;* G  c
		   (memq (car args) generic-invocation-generics)) ;* r  a
	      (apply (method-procedure                            ;* o  s
		      (last (generic-methods generic)))           ;* u  e
		     (cons #f args))                              ;* n
	                                                          ;* d
	      ((compute-apply-methods generic)
	       ((compute-methods generic) args)
	       args))))))


(add-method compute-methods
    (make-method (list <generic>)
      (lambda (call-next-method generic)
	(lambda (args)
	  (let ((applicable
		 (collect-if (lambda (method)
			       ;
			       ; Note that every only goes as far as the
			       ; shortest list!
			       ;
			       (every applicable?
				      (method-specializers method)
				      args))
			     (generic-methods generic))))
	    (gsort (lambda (m1 m2)
		     ((compute-method-more-specific? generic)
		      m1
		      m2
		      args))
		   applicable))))))


(add-method compute-method-more-specific?
    (make-method (list <generic>)
      (lambda (call-next-method generic)
	(lambda (m1 m2 args)
	  (let loop ((specls1 (method-specializers m1))
		     (specls2 (method-specializers m2))
		     (args args))
	    (cond ((and (null? specls1) (null? specls2))
                   (error
                     "Two methods are equally specific."))
                  ((or  (null? specls1) (null? specls2))
                   (error
                     "Two methods have a different number of specializers."))
		  ((null? args)
		   (error
                     "Fewer arguments than specializers."))
		  (else
		   (let ((c1  (car specls1))
			 (c2  (car specls2))
			 (arg (car args)))
		     (if (eq? c1 c2)
			 (loop (cdr specls1)
			       (cdr specls2)
			       (cdr args))
			 (more-specific? c1 c2 arg))))))))))


(add-method compute-apply-methods
    (make-method (list <generic>)
      (lambda (call-next-method generic)
	(lambda (methods args)
	  (letrec ((one-step
		     (lambda (tail)
		       (lambda ()
			 (if (null? tail)
			     (error "No applicable methods/next methods.")
			     (apply (method-procedure (car tail))
				    (cons (one-step (cdr tail)) args)))))))
	    ((one-step methods)))))))

(define applicable?
    (lambda (c arg)
      (memq c (class-cpl (class-of arg)))))

(define more-specific?
    (lambda (c1 c2 arg)
      (memq c2 (memq c1 (class-cpl (class-of arg))))))



(add-method initialize
    (make-method (list <object>)
      (lambda (call-next-method object initargs) object)))

(add-method initialize
    (make-method (list <class>)
      (lambda (call-next-method class initargs)
	(call-next-method)
	(slot-set! class
		   'direct-supers
		   (getl initargs direct-supers: '()))
	(slot-set! class
		   'direct-slots
		   (map (lambda (s)
			  (if (pair? s) s (list s)))
			(getl initargs direct-slots:  '())))
	(slot-set! class 'cpl   (compute-cpl   class))
	(slot-set! class 'slots (compute-slots class))
	(let* ((nfields 0)
	       (field-initializers '())
	       (allocator
		(lambda (init)
		  (let ((f nfields))
		    (set! nfields (+ nfields 1))
		    (set! field-initializers
			  (cons init field-initializers))
		    (list (lambda (o)   (%instance-ref  o f))
			  (lambda (o n) (%instance-set! o f n))))))
	       (getters-n-setters
		(map (lambda (slot)
		       (cons (car slot)
			     (compute-getter-and-setter class
							slot
							allocator)))
		     (slot-ref class 'slots))))
	  (slot-set! class 'nfields nfields)
	  (slot-set! class 'field-initializers field-initializers)
	  (slot-set! class 'getters-n-setters getters-n-setters)))))

(add-method initialize
    (make-method (list <generic>)
      (lambda (call-next-method generic initargs)
	(call-next-method)
	(slot-set! generic 'methods '())
	(%set-instance-proc! generic
			   (lambda args (error "Has no methods."))))))

(add-method initialize
    (make-method (list <method>)
      (lambda (call-next-method method initargs)
	(call-next-method)
	(slot-set! method 'specializers (getl initargs specializers:))
	(slot-set! method 'procedure    (getl initargs procedure:)))))



(add-method allocate-instance
    (make-method (list <class>)
      (lambda (call-next-method class)
	(let* ((field-initializers (slot-ref class 'field-initializers))
	       (new (%allocate-instance
		      class
		      (length field-initializers))))
	  (let loop ((n 0)
		     (inits field-initializers))
	    (if (pair? inits)
		(begin
		 (%instance-set! new n ((car inits)))
		 (loop (+ n 1)
		       (cdr inits)))
		new))))))

(add-method allocate-instance
    (make-method (list <entity-class>)
      (lambda (call-next-method class)
	(let* ((field-initializers (slot-ref class 'field-initializers))
	       (new (%allocate-entity
		      class
		      (length field-initializers))))
	  (let loop ((n 0)
		     (inits field-initializers))
	    (if (pair? inits)
		(begin
		 (%instance-set! new n ((car inits)))
		 (loop (+ n 1)
		       (cdr inits)))
		new))))))


(add-method compute-cpl
    (make-method (list <class>)
      (lambda (call-next-method class)
	(compute-std-cpl class class-direct-supers))))


(add-method compute-slots
    (make-method (list <class>)
      (lambda (call-next-method class)
	(let collect ((to-process (apply append
					 (map class-direct-slots
					      (class-cpl class))))
		      (result '()))
	  (if (null? to-process)
	      (reverse result)
	      (let* ((current (car to-process))
		     (name (car current))
		     (others '())
		     (remaining-to-process
		      (collect-if (lambda (o)
				    (if (eq? (car o) name)
					(begin
					 (set! others (cons o others))
					 #f)
					#t))
				  (cdr to-process))))
		(collect remaining-to-process
			 (cons (append current
				       (apply append (map cdr others)))
			       result))))))))


(add-method compute-getter-and-setter
    (make-method (list <class>)
      (lambda (call-next-method class slot allocator)
	(allocator (lambda () '())))))



;
; Now everything works, both generic functions and classes, so we can
; turn on the real MAKE.
;
;
(set! make
      (lambda (class . initargs)
	(let ((instance (allocate-instance class)))
	  (initialize instance initargs)
	  instance)))

;
; Now define what CLOS calls `built in' classes.
;
;
(define <primitive-class>
    (make <class>
	  direct-supers: (list <class>)
	  direct-slots:  (list)))

(define make-primitive-class
    (lambda class
      (make (if (null? class) <primitive-class> (car class))
	    direct-supers: (list <top>)
	    direct-slots:  (list))))


(define <pair>        (make-primitive-class))
(define <null>        (make-primitive-class))
(define <symbol>      (make-primitive-class))
(define <boolean>     (make-primitive-class))
(define <procedure>   (make-primitive-class <procedure-class>))
(define <number>      (make-primitive-class))
(define <vector>      (make-primitive-class))
(define <char>        (make-primitive-class))
(define <string>      (make-primitive-class))
(define  <input-port> (make-primitive-class))
(define <output-port> (make-primitive-class))

;-  Identification and Changes

;--
;	framework-controls.scm -- Written by Randall Gray 

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data


;; This is a list of symbols (or strings, I guess) -- kdnl* calls
;; which have a member of this list, get printed during the run.

(define kernel-messages '())
(define adjust-grey #t)
(define nested-agents '()) ;; needs this for the moment



;; If this is set to true, the kernel becomes inaccessible to the
;; agent when it is not running. Thus, it is unable to access
;; non-local information when answering queries and performing updates

(define blue-meanie #f)




;; These are not necessarily controls, but they are used in a similar fashion and need to preceed framework-classes.

(define (class-name-of x)
  (cond
	((isa? x <agent>) 
	 (let ((n (class-register 'name? (class-of x))))
		(and n
			  ;;(string->symbol (string-append "instance-of-" (symbol->string n)))
			  n
			 )
		))
	((and (sclos-object? x) (assoc x (class-register))) 
	 (let ((n (class-register 'name? x)))
		(and n
			  (string->symbol (string-append "class:" (symbol->string n)))
			  )))
	(else #f)))
	

(define class-register 
  (let ((register '())
		  )
	 (lambda args
		(if (null? args)
			 register
			 (let ((cmd (car args))
					 (opts (if (null? (cdr args)) #f (cdr args))))
				(cond
				 ((and (eq? cmd 'add) opts (not (assq (car opts) register)))
				  (set! register (acons (car opts) (cdr opts) register)) ;; save things as lists
				  )

				 ((and (eq? cmd 'name?) opts)
				  (let ((a (assq (car opts) register)))
					 (and a (cadr a))))

				 ((and (eq? cmd 'rec-by-class) opts)
				  (let ((a (assq (car opts) register)))
					 a))

				 ((and (eq? cmd 'class?) opts)
				  (let ((a (filter (lambda (x) (eq? (car opts) (cadr x))) register)))
					 (and a (car a) (caar a))))

				 ((and (eq? cmd 'rec-by-name) opts)
				  (let ((a (filter (lambda (x) (eq? (car opts) (cadr x))) register)))
					 (and a (car a))))

				 (else (abort 'badly))))))))
						

(define generic-register 
  (let ((register '())
		  )
	 (lambda args
		(if (null? args)
			 register
			 (let ((cmd (car args))
					 (opts (if (null? (cdr args)) #f (cdr args))))
				(cond
				 ((and (eq? cmd 'add) opts (not (assq (car opts) register)))
				  (set! register (acons (car opts) (cdr opts) register)) ;; save things as lists
				  )

				 ((and (eq? cmd 'name?) opts)
				  (let ((a (assq (car opts) register)))
					 (and a (cadr a))))

				 ((and (eq? cmd 'rec-by-generic) opts)
				  (let ((a (assq (car opts) register)))
					 a))

				 ((and (eq? cmd 'generic?) opts)
				  (let ((a (filter (lambda (x) (eq? (car opts) (cadr x))) register)))
					 (and a (car a) (caar a))))

				 ((and (eq? cmd 'rec-by-name) opts)
				  (let ((a (filter (lambda (x) (eq? (car opts) (cadr x))) register)))
					 (and a (car a))))

				 (else (abort 'badly))))))))
						

(define (sclos-object? a)
  (and (%instance? a) #t))

(define (agent? a)
  (and (%instance? a) (isa? a <agent>) #t))

(define (has-slot? a k)	
  (member k (map car (class-slots (class-of a)))))


;--    Public data 

;-  Code 

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
