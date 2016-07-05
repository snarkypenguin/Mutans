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

(load "utils.scm")


(define pi (acos -1.))
(define twopi (* 2.0 pi))
(define e (exp 1))
(define sqrt2pi (sqrt twopi))



(define (seq n)
  (define (s m l)
    (if (zero? m)
        (cons m l)
        (s (- m 1) (cons m l))))
  (cond
   ((zero? n) '())
   ((< n 0) (map (lambda (x) (+ n x)) (s (- (- n) 1) '())))
   (#t (s (- n 1) '()))))


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
  (/ v (+ 1 v)))

(define (general-sigmoid  x lmb phi) 
  (general-sigmoid-g (general-sigmoid-f x lmb phi)))

;; the sigmoid* things are set up for mapping [0,1]->[0,1]

;;used to be (define (sigmoid* x #!optional lmb . stuff)
(define (sigmoid* x . stuff)
  
  (let ((lmb #f))
	 (if (pair? stuff)
		  (begin
			 (set! lmb (car stuff))
			 (set! stuff (cdr stuff))))
	 
	 (let* ((bs (lambda (x) (general-sigmoid x (if lmb lmb 1.0) 0.5)))
			  (m (bs 0.0))
			  (M (bs 1.0))
			  (r (- M m)))
		(max 0.0 (min 1.0 (/ (- (bs x) m) r))))
	 ))

(define (inverse-sigmoid* x . stuff)
  (let ((lmb #f))
	 (if (pair? stuff)
		  (begin
			 (set! lmb (car stuff))
			 (set! stuff (cdr stuff))))	 
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
		))
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
	(else (/ (log (- (/ 1 P_0) 1)) (* 4 pi lmb)))))

(define (power b e)
  (cond
	((< e 0) (/ 1 (power b (- e))))
	((zero? e) 1)

	((and (integer? e) (rational? b)) ;; This will keep them as exact numbers if they are exact
	 (cond
	  ((even? e) (power (* b b) (/ e 2)))
	  (t (* b (power b (- e 1)))))
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

(define (sum mn mx lmbda)
  (apply + (map lmbda (map (lambda (x) (+ mn x)) (seq (- (+ 1 mx) mn))))))

(define (prod mn mx lmbda)
  (apply * (map lmbda (map (lambda (x) (+ mn x)) (seq (- (+ 1 mx) mn))))))



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


(define (rotated-vector V theta . axis)
  (set! axis (if (null? axis) #f (car axis)))
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


 ;; general -- 
(define (abeam a b scale) ; scale might be the distance covered  in a timestep
  (let* ((v (projection a b))
			(w (/ v scale))
			)
	 v)) ;; Is this right?  Should it be w?


(define (mult2 x  y)  ;; general
  (cond
	((and (plist? x) (plist? y) (map * x y)))
	((plist? x) (map (lambda (p) (* p y)) x))
	((plist? y) (map (lambda (p) (* x p)) y))
	((and (number? x) (number? y)) (* x y))
	(#t (report-error 'bad-argument))
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
	(#t (report-error 'bad-argument))
	))


(define (add2 x  y) ;; general
  (cond
	((and (plist? x) (plist? y) (map + x y)))
	((plist? x) (map (lambda (p) (+ p y)) x))
	((plist? y) (map (lambda (p) (+ x p)) y))
	((and (number? x) (number? y)) (+ x y))
	(#t (report-error 'bad-argument))
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
	(#t (err 'bad-argument))
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
                (if (> (vector-ref table i) 0.9999) (begin (set! size (+ 1 i)) (vector-set! table size 1.0)))
                (loop (+ 1 i))))))
	 
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
	  (map +1 (seq (- (length (car pwl)) 1)))))
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
