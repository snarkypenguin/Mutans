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

;;(load "utils.scm") ;; for cross-product of lists -- included by maths.scm
;;(load "maths.scm")


(define debugging-integration #f)
(define MAX-DEPTH 5)

;; Algorithm translated from Wikipedia: "Adaptive Simpson's Method" 30/04/2009

;;(adaptive-integrate function lowerbound upperbound  tolerance maxdepth)

(define (differential F x . eps)
  (let* ((eps (if (null? eps) 1e-8 (car eps)))
			(q (/ (- (F (+ x eps)) (F (- x eps))) (* 2 eps))))
	 q))
				


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
		  (if (or (<= k 0) (<= (magnitude (- inner-estimate estimate)) (* 15 eps)))
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
		(let loop ((the-sum 0)
					  (x a))
		  (if (>= (+ x ss) b)
				(+ the-sum (integrate f x b eps k))
				(loop (+ the-sum (integrate f x (+ x ss) eps k))
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
  
  (if (< eps 0) (set! eps (magnitude eps)))
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
;		  (mdnl "(magnitude (- inner-estimate estimate)) =" (magnitude (- inner-estimate estimate)))
;		  (mdnl "(* 15 eps) =" (* 15 eps))
		  (if (or (<= k 0) (<= (magnitude (- inner-estimate estimate)) (* 15 eps)))
				(+ inner-estimate (/ (- inner-estimate estimate) 15))
				(+ (inner-general-adaptive-integrate f a c (/ eps 2) left-estimate fa fd fc << ** // ++ -- (1- k))
					(inner-general-adaptive-integrate f c b (/ eps 2) right-estimate fc fe fb << ** // ++ -- (1- k))
					)
				)
		  )
		)
  )


;;;; integrate using supplied functions for less-than (swap-order) and basic arithmetic operations (* / + -) -----

;; This takes functions for the basic arithmetic operations, rather than assuming the standard operations.
;; swap-order is a "less-than" function

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

	 (if (< eps 0) (set! eps (magnitude eps)))

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
		  (let loop ((the-sum 0)
						 (x a))
			 (if (>= (v-length (list-operator - (add x dx) a)) HdH)
				  (+ the-sum (integrate-RV f x b eps k))
				  (loop (+ the-sum (integrate-RV f x (add x dx) eps k))
						  (add x dx)))))))
					
(define rk4-doco "
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



The Butcher tableau for RK4 is 

	0	|
	1/2|	1/2
	1/2|	0		1/2
	1	|	0		0		1
  -----------------------------
		|	1/6	1/3	1/3	1/6

A Butcher tableau is represented by 

((c_1 ... c_s) ((a_1|1 ... a_1|s) ...(a_s|1 ... a_s|s)) (b_1 ... b_s))

for explicit methods, or

((c_1 ... c_s) ((a_1|1 ... a_1|s) ...(a_s|1 ... a_s|s)) (b_1 ... b_s) (b'_1 ... b'_s))

for implicit methods, though I council against trying to use a single quote as a part of 
a symbol in scheme or lisp.


The R-K methods give numerical solutions for functions of the form f(t,y)
                               s
of the form y_{n+1} = y_n + h ∑ b_i k_i
                              i=1

                                   i-1
where k_i = f(t_n + c_i h, y_n + h ∑ a_ij k_j
                                   j=1
for *explicit* methods.  

Implicit methods are whole different kettle of fish, since we would have to solve a system 
of s equations at each step to calculate k_i. In this case, we use the second form of the 
tableau, evaluate the sum in the expression for y_{n+1} from zero to s,  and additionally 
define 
                               s
           y'_{n+1} = y_n + h ∑ b'_i k_i
                              i=1

                                             s
           e_{n+1} = y_{n+1} - y'_{n+1} = h ∑ (b_i - b'_i) k_i
                                            i=1

which can be used to change the step size, h. I would usually do by dividing h into 
more segments.


We can construct a piecewise linear interpolating function with the 
line segments defined by the  (t_i, y_i) as is shown in rk4, below.

The following is the classic RK4 with a tableau of '((0 1/2 1/2 1) ((0 0 0 0) (1/2 0 0 0) (0 1/2 0 0) (0 0 1 0)) (1/6 1/3 1/3 1/6))

We have an expression of the form dy/dt = f where y is a scalar function of t and f is a function in terms of y and t
and the function y is not known.
In this case we could write something like (define dy/dt f) (define y (rk4 dy/dt mn mt step y_0))
where f (and thus dy/dt) looks like (lambda (t y) (some expression in y and t, where y is treated as a variable))
and y is of the form (lambda (t) (some expression in t))

(define P (rk4 (lambda (t y)  y) 0 10 0.1 1))
(exp 10) => 22026.465794806718
(P 10) => 22026.296900876645

compared to the librcg version in C 

   evaluate 'ode(dy/dt = y(t), y(0) = 1, 0.1, 10)'
   ode(dy/dt = y(t), y(0) = 1, 0.1, 10) = 22026.296901

")

;; Here f is dy/dt, [a,b] is the domain over which the 
;; returned function is defined, ss is the stepsize and 
;; Y0 is the initial value at a.  dy/dt is in terms of t and y

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


;; We repeat this every time, so I don't forget!

;; We have an expression of the form dy/dt = f where y is a scalar function of t and f is a function in terms of y and t
;; and the function y is not known.
;; Here, we could write something like (define dy/dx f) (define y (rk4 dy/dt mn mt step y_0))
;; where f (and thus dy/dt) looks like "(lambda (t y) (some expression in y and t, where y is treated as a variable))"
;; and y is of the form "(lambda (t) (some expression in t))"

;; In rk4-2d, x and y are functions of t, and the two lambdas passed in are expressions in terms of t, x and y.
;; We could write (define

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
;;						(dnl t+h " " xx " " yy)
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

(define simple-rk4-example
"
;; (define P (rk4* (list (lambda (t y)  y)) 0 10 0.1 (list 1)))
;; (exp 10) => 22026.465794806718
;; (P 10) => 22026.296900876645


;; We repeat this every time, so I don't forget!

;; We have an expression of the form dy/dt = f where y is a scalar function of t and f is a function in terms of y and t
;; and the function y is not known.
;; In this case we could write something like (define dy/dt f) (define y (rk4 dy/dt mn mt step y_0))
;; where f (and thus dy/dt) looks like (lambda (t y) (some expression in y and t, where y is treated as a variable))
;; and y is of the form (lambda (t) (some expression in t))

;;(define sincos (rk4-2d (lambda (t x y)  y) (lambda (t x y) (- x)) 0 (* 2 pi) 0.001 0 1))
;; where (lambda (t x y)  y) is d sin/dt and (lambda (t x y) (- x)) is d cos/dt, the domain 
;; of the resulting functions is [0,2pi] we have a step of 0.01 and the initial values are 
;; 0 and 1 for sin and cos

;;  REMEMBER: this returns a function f:R^1->R^{(length F)}

;;-------------------

")

(define rk4-examples-with-charplot
"
;; In the example below, Q is the function which represents the trace of the system through time.
(define (rk*-example-1)
  ;;;
  ;;; (require 'charplot) ;; needs slib installed
  (let* ((dx/dt (lambda (t x y) (+ (* 1.001 (- 1  (/ x 42))) (- 1 (/ x (/ y 10))))))
			(dy/dt (lambda (t x y) (+ (* 1.003 (- 1  (/ y 500))) (- (* 0.01 x y)))))
			(Q (rk4* (list dx/dt dy/dt)	0 800 0.1 (list 10 500)))
			)
	 (plot Q 0 40)
	 
    ;;;            _________________________________________________________________ 
    ;;;        500|-+                                                               |
    ;;;           | :+                                                              |
    ;;;        450|-:                                                               |
    ;;;           | :+                                                              |
    ;;;        400|-: +                                                             |
    ;;;           | :  +                                                            |
    ;;;        350|-:  +                                                            |
    ;;;           | :                                                               |
    ;;;        300|-:   +                                                           |
    ;;;           | :   +                                                           |
    ;;;        250|-:    +                                                          |
    ;;;           | :     +                                                         |
    ;;;        200|-:     +                                                         |
    ;;;           | :      ++                                                       |
    ;;;        150|-:       +                                                       |
    ;;;           | :        ++                                                     |
    ;;;        100|-:         ++                                                    |
    ;;;           | :          +++                                                  |
    ;;;         50|-:            +++++                                              |
    ;;;           | : *************** +++++++++++++++++++++++                       |
    ;;;          0|-**---------------************************-----------------------|
    ;;;           |_:____.____:____.____:____.____:____.____:____.____:____.____:___|
    ;;;             0         10        20        30        40        50        60   
    ;;; > (Q 799)
    ;;; (4.265524500299042 22.45794983845297)
    ))

(define (rk*-example-2)
  ;;;
  ;;; (require 'charplot) ;; needs slib installed
  (let* ((dx/dt (lambda (t x y z) (+ (* 1.001 (- 1  (/ x 42))) (- 1 (/ x (/ y 10))))))
			(dy/dt (lambda (t x y z) (+ (* 1.003 (- 1  (/ y 500))) (- (* 0.01 x y)))))
			(dz/dt (lambda (t x y z) (+ (* 1.003 (- 1  (/ z 8))) (+ (* 0.01 x z)))))
			(Q (rk4* (list dx/dt dy/dt dz/dt)	0 800 0.1 (list 10 500 4)))
			)
	 (plot Q 0 40)
	 
       ;;;     _________________________________________________________________ 
       ;;; 500|-+                                                               |
       ;;;    | :+                                                              |
       ;;; 450|-:                                                               |
       ;;;    | :+                                                              |
       ;;; 400|-: +                                                             |
       ;;;    | :  +                                                            |
       ;;; 350|-:  +                                                            |
       ;;;    | :                                                               |
       ;;; 300|-:   +                                                           |
       ;;;    | :   +                                                           |
       ;;; 250|-:    +                                                          |
       ;;;    | :     +                                                         |
       ;;; 200|-:     +                                                         |
       ;;;    | :      ++                                                       |
       ;;; 150|-:       +                                                       |
       ;;;    | :        ++                                                     |
       ;;; 100|-:         ++                                                    |
       ;;;    | :          +++                                                  |
       ;;;  50|-:            +++++                                              |
       ;;;    | : *****xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                       |
       ;;;   0|-xxxxxxx----------************************-----------------------|  
       ;;;    |_:____.____:____.____:____.____:____.____:____.____:____.____:___|   
       ;;;      0         10        20        30        40        50        60      
    ))
")


(define (rk4* F a b ss Xo . ZT) ;; F is a list of functions, say dy/dt dx/dt, and dz/dt, where each is in terms of t, x, y, z
                                ;; and the initial values of x, y, and z are specified in Xo.  rk4* returns
                                ;; a vector function with the values of x, y and z over the domain t in [a,b].
  (if (< (magnitude ss) 1e-12) 
		(abort "bad step size in rk4*"))

  (set! ZT (if (null? ZT) #f (car ZT)))
  (if (not (list? F))
		(abort "rk4 expects a list of functions as the first argument"))
  (if (or (not (list? Xo)) (not (= (length F) (length Xo))))
		(abort "rk4 expects list of the initial values of the functions as the fifth (and final) argument"))

  (let* ((h (* 1 ss))
			(h/2 (/ h 2)))
	 (letrec ((rk4i
				  (lambda args
;;					 (dnl "RK4I ËNTRY:  " args)
					 (if (< (magnitude h) 1e-12) 
						  (abort "bad step size in rk4*/rk4i"))
					 (let* ((t (car args))
							 (funcs (cdr args))
							 (t+h/2 (+ t h/2))
							 (t+h (+ t h))
									  
							 (q1 (let ((ordinates (cons t funcs)))
;;									 (dnl "ordinates: " ordinates)
;;									 (dnl "F: " F)
;;									 (dnl "result: " (map (lambda (f) (apply f ordinates)) F))
									 (map (lambda (f) (apply f ordinates)) F)))
							 
							 (q2 (let ((ordinates (cons t+h/2 
																 (map (lambda (x v)
																		  (+ x (* v h/2))) funcs q1))))
;;									 (dnl "ordinates: " ordinates)
;;									 (dnl "F: " F)
;;									 (dnl "result: " (map (lambda (f) (apply f ordinates)) F))
									 (map (lambda (f) (apply f ordinates)) F)))

							 (q3 (let ((ordinates (cons t+h/2 
																 (map (lambda (x v) 
																		  (+ x (* v h/2))) funcs q2))))
;;									 (dnl "ordinates: " ordinates)
;;									 (dnl "F: " F)
;;									 (dnl "result: " (map (lambda (f) (apply f ordinates)) F))
									 (map (lambda (f) (apply f ordinates)) F)))

							 (q4 (let ((ordinates (cons t+h 
																 (map (lambda (x v) 
																		  (+ x (* v h))) funcs q3))))
;;									 (dnl "ordinates: " ordinates)
;;									 (dnl "F: " F)
;;									 (dnl "result: " (map (lambda (f) (apply f ordinates)) F))
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


;;; F is a list of functions dp/dt dq/dt ... which are in terms of t x y ... and p q r ...
;;; the bounds on x, y ... are given in var-min and var-max, and the stepsizes are either all given by a numeric ss-list
;;; or by the numbers in ss-list

(define (real->integer_ x) (inexact->exact (round x)))
(define (real->integer x) (inexact->exact (round (+ 0.5 x))))

(define (*n-cells* m M s)
  (let ((c (inexact->exact (+  (truncate (/ (- M m) s)))))
		  )
	 (+ 1 c)))

(define (make-linear-list m M ss var-min var-max var-ss)
  (let* ((counts (map *n-cells* var-min var-max var-ss))
			)
	 #f)
  )
			 
(define (grid-values m M s)
  (let* ((S (*n-cells* m M s))
			(S/2 (/ S 2.0)))
	 (map (lambda (x) (if (< x S/2) (+ m (* x s)) (- M (* (- S x) s)))) (seq (+ 1 S)))))

(define (relevant-indices x L) ;; This really sucks.
  (let* ((v (list->vector L))
			(n (vector-length v))
			(i 0)
			(j (- n 1))
			(k (let loop ((h 0))
				  (cond
					((>= h n) h)
					((<= x (vector-ref v h)) h)
					((and (< (+ h 1) n) (> x (vector-ref v h)) (< x (vector-ref v (+ h 1))))
					 (list h (+ h 1)))
					(#t (loop (+ h 1))))))
			)
	 k
	 )
  )


(define (*cross* . args)
  (define (cross2 a b)
	 (apply append (map (lambda (x) (map (lambda (y) 
														(if (list? y)
															 (cons x (list y))
															 (list x y))) b)) a)))
  (cond
	((not (list? args)) (error "bad-argument" args))
	((null? args) '())
	((= (length args) 1)
	 (car args))
	((= (length args) 2)
	 (apply cross2 args))
	(#t (*cross* (car args) (apply *cross* (cdr args))))))


(define (make-hypercube var-min var-max var-ss)
  (apply *cross* (map grid-values var-min var-max var-ss)))

  
;; This is "rk4 with parameters".  Here "mins" and "maxs" are the minima and maxima for t and the parameters, Fo is the initial value for the antiderivative of f
(define (rk4p dF/dt  mins maxs var-ss Fo) ;; 
  (let* ((t0 (car mins))
			(tM (car maxs))
			(var0 (cadr mins))
			(varM (cadr maxs))
			(t-ss (if (list? var-ss)  (car var-ss) var-ss))
			(var-ss (if (list? var-ss)  (cadr var-ss) var-ss))
			(varlist (grid-values var0 varM var-ss))
			(flist (list->vector (map (lambda (x) (rk4 (lambda (t F) (dF/dt t x F)) t0 tM t-ss Fo)) varlist)) )
			)
	 (lambda (t x)
		(let ((n (relevant-indices x varlist)))
		  (if (number? n)
				((vector-ref flist n) t)
				(let ((v1 ((vector-ref flist (car n)) t))
						(d1 (- x (vector-ref (car n))))
						(v2 ((vector-ref flist (cadr n)) t))
						(d2 (- x (vector-ref (cadr n)))))
				  (+ (* v1 (/ (- d2 x) (- d2 d1))) (* v2 (/ (- x d1) (- d2 d1))))))
		  ))
	 )
  )



"
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

"

(define rkf-exclusion
"
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
										 ((zero? (magnitude (- (car v) (car v*))))
										  (loop (cdr v) (cdr v*) worst target))
										 ((> 
											(/ (magnitude (- (car v) (car v*))) (/ (+ (magnitude (car v)) (magnitude (car v*))) 2))
											worst)
										  
										  (loop (cdr v) (cdr v*) (/ (magnitude (- (car v) (car v*))) (/ (+ (magnitude (car v)) (magnitude (car v*))) 2))  (magnitude (- (car v) (car v*))))) 
										 (else (loop (cdr v) (cdr v*) worst target)))))


							  (den (let loop ((v V) (v* V*) (worst 0) (target #f))
										(cond
										 ((null? v) target)
										 ((zero? (magnitude (- (car v) (car v*))))
										  (loop (cdr v) (cdr v*) worst target))
										 ((> 
											(/ (magnitude (- (car v) (car v*))) (/ (+ (magnitude (car v)) (magnitude (car v*))) 2))
											worst)
										  
										  (loop (cdr v) (cdr v*) (/ (magnitude (- (car v) (car v*))) (/ (+ (magnitude (car v)) (magnitude (car v*))) 2))  (/ (+ (magnitude (car v)) (magnitude (car v*))) 2)))
										 (else (loop (cdr v) (cdr v*) worst target)))))

							  (err (let loop ((v V) (v* V*) (worst 0))
										(cond
										 ((null? v) worst)
										 ((zero? (magnitude (- (car v) (car v*))))
										  (loop (cdr v) (cdr v*) worst))
										 ((> 
											(/ (magnitude (- (car v) (car v*))) (/ (+ (magnitude (car v)) (magnitude (car v*))) 2))
											worst)
										  (loop (cdr v) (cdr v*) (/ (magnitude (- (car v) (car v*))) (/ (+ (magnitude (car v)) (magnitude (car v*))) 2))))
										 (else (loop (cdr v) (cdr v*) worst)))))

							  (nstep (cond 
										 (#t step)
										 ((< err 1/1000000) (* step 2))
										 ((> err 1/1000) (/ step 2))
										 (else step)))
							  ) ;; end of let* variables

						
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
")



;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***








