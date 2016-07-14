;-  Identification and Changes

;--
;	utils.scm -- Written by Randall Gray 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 


;; This will not be a part of the rest of the code unless the code uses (include "utils.scm")
(define-macro (with-exit name . body)
  `(call-with-current-continuation
    (lambda (,name) ,@body)))


;; This allows me to put in comments which I can "articulate" if needs be.
(define (comment . args) #!void)

;;(define (guess-type ob)
;;  (let ((s (make-string 200)))
;;	 (with-output-to-string s (lambda () (display ob)))
;;	 s)
;;  )

;; This next is probably quite gambit specific w.r.t. procedures

(define (object-type ob)
  (cond 
	((eq? ob #!void) '(void))
	((null? ob) '(null))
	((list? ob) '(list))
	((pair? ob) '(pair))
	((boolean? ob) '(boolean))
	((number? ob) '(number))
	((symbol? ob) '(symbol))
	((char? ob) '(char))
	((string? ob) '(string))
	((vector? ob) '(vector))
	((procedure? ob)
	 (let* ((typestr (call-with-output-string (lambda (p) (display ob p))))
			  (tmp (cdr (reverse (string->list typestr))))
			  (lst (list-tail (reverse tmp) (length (member #\space tmp))))
			  )

		 (if (eq? (car lst) #\#)
			  (list 'procedure (list->string (cdr lst)))
			  (list 'procedure (string->symbol (list->string lst))))
		 ))
	((atom? ob) '(atom unknown))
	(else 'unknown)
  )
)

;; Selects a random member of a list when each element has a given weighting
(define (random-list-ref lst class-weights)
  (define (partial-sums lst)
	 (let loop ((n (reverse lst))
					(r '())
					)
		(cond 
		 ((null? n) r)
		 ((null? (cdr n))
		  (cons (car n) r))
		 (else (loop (cdr n) (cons (apply + n) r)))
		 )))
  (let* ((last-weight (car (reverse class-weights)))
			(total-weight (apply + class-weights))
			(k (* (random-real) total-weight))
			(ps (filter (lambda (x) (<= k x)) (partial-sums class-weights)))
			)
	 (if (eq? (length ps) (length class-weights))	
		  (car (reverse lst))
		  (list-ref lst (- (length class-weights) (length ps))))
	 )
  )

;; (list-nth* '((a b c) (d e f) (g h i) (j k l)) 2) => (c f i l)
(define (list-nth* lst n) (apply (lambda x (map (lambda (y) (list-ref y n)) x)) lst))

;; This only works with lists ... this is the transpose of a list of lists.
(define (list-transpose x)
  (cond 
	((atom? x) x)
	((and (pair? x) (pair? (car x)))
	 (map (lambda (y) (list-nth* x y)) (seq (length (car x)))))
	((and (pair? x) (number? (car x))) ;; pivot a row vector
	 (abort "list-transpose failure: not a matrix list")
	 (map list x)) 
	(else (abort "list-transpose failure"))))

(define (dump-data lst . file)
  (set! file (if (null? file) (current-output-port) (car file)))
  (for-each 
	(lambda (x)
	  (for-each 
		(lambda (y)
		  (display y file)
		  (display " " file))
		x)
	  (newline file))
	lst))
		


;; Function equivalent of "and"
(define (andf . args)
  (cond
   ((null? args) #t)
   ((null? (cdr args)) (car args))
   ((car args)
    (apply andf (cdr args)))
   (else #f)))

;; Function equivalent of "or"
(define (orf . args)
  (cond
   ((null? args) #f)
   ((null? (cdr args)) (car args))
   ((car args) (car args))
   (else (apply orf (cdr args)))))


(define (character? x) (char? x))
(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
(define (close f) (close-port f))

;;(define (make-list n . init)
;;  (if (<= n 0) 
;;		'()
;;		(if (null? init)
;;			 (cons '() (make-list (1- n)))
;;			 (cons (car init) (make-list (1- n) (car init))))))


;;; (define (Display arg . port)
;;;   (if (pair? arg)
;;; 		(apply write (append (list arg) port))
;;; 		(apply display (append (list arg) port))))


(define (dnl . args)
  (let* ((n-- (1- (length args)))
			(lt (if (null? args) #f (car (reverse args))))
			(lh (if (null? args) '() (list-head args n--))))

	 (let ((dsp (if (port? lt) (lambda (x) (display x lt) #!void) display)))
			 (if (port? lt)
				  (and (map dsp lh) (newline lt))
				  (and (map dsp args) (newline))))
			 #!void))


(define (dnl* . args)
  (let* ((n (length args))
			(n-- (1- n)))
  (if (and (pair? args) (port? (list-tail args n--)))
		(apply dnl (append (cdr (apply append (map (lambda (x) (list " " x)) (list-head args n--)) (list-tail args n--)))))
		(apply dnl (cdr (apply append (map (lambda (x) (list " " x)) args)))))))
		  

(define (kdnl* message . args)
  (if (member message kernel-messages)
		(apply dnl* args)))

;;(define (dnl* . args)
;;	 (if (null? args) 
;;		  (newline)
;;		  (begin
;;			 (display (car args)) 
;;			 (for-each (lambda (x) 
;;							 (display " ") 
;;							 (display x)) 
;;						  (cdr args)) 
;;			 (newline))))


;; Used as the placeholder for uninitialised things in classes
(define uninitialised (lambda args 'uninitialised))


;; surjections onto subspaces for lists representing vectors
(define (txyz->xy x) (list-head (cdr x) 2))
(define (txyz->xyz x) (cdr x))

(define (txyz->t x) (car x))
(define (txyz->x x) (cadr x))
(define (txyz->y x) (caddr x))
(define (txyz->z x) (cadddr x))


;; This is the function composition operator
(define (o f . g) (lambda (x) (if (null? g) (f x) (f ((apply o g) x)))))


(define pi (* 4.0 (atan 1.0)))

;;; #|
;;; (define (general-sigmoid x y) (/ 1.0 (+ 1.0  (exp (- (* x y))))))

;;; ;;(define sigmoid (lambda (x) (general-sigmoid x 1.0)))

;;; ;; (require 'charplot) ; from slib
;;; ;; (plot (lambda (p) (general-sigmoid p 0.04)) -100.0 100.0 200)

;;; (define sigmoid*
;;;   (let* ((bs (lambda (x) (/ 1.0 (+ 1.0 (exp (* -2 pi (- (* 2.0 x) 1.0)))))))
;;; 			(m (bs 0.0))
;;; 			(M (bs 1.0))
;;; 			(r (- M m)))
;;; 	 (lambda (x)
;;; 		(max 0.0 (min 1.0 (/ (- (bs x) m) r))))))

;;; ;; maps R->[0,1]
;;; (define (inverse-sigmoid x)
;;;   (cond 
;;; 	((>= x 1) 1)
;;; 	((<= x 0) 0)
;;; 	(else (max 0.0 (min 1.0 (+ (/ (- (log x) (log (- 1 x))) (* 4 pi)) 0.5))))))
;;; |#


(define (o->s x) (if (string? x) x (object->string x)))


(define (random-location minv maxv)
  (map + (map * (map - maxv minv) (map (lambda (x) (random-real)) (make-list (length minv)))) minv))

(define (nrandom mean . its) ;; very dodgey ... but bog simple
  (let ((N (if (null? its) 100 (car its))))
	 (let loop ((i 0)
					(sum 0))
		(if (>= i N)
			 (* (/ mean (/ N 2.0)) sum)
			 (loop (1+ i) (+ sum (random-real)))))))


(define (general-biomass-growth-rate x peak width scale y) 
  (* scale 
          (- 
                (general-sigmoid (+ (- x peak) width) y)
                (general-sigmoid (- (- x peak) width) y))))

(define (dP/dt P dt r K) (* r P (- 1.0 (/ P K))))0


(define (aborts . args)
  (abort (apply string-append (map o->s args))))

;; A sequence of integers
;; (seq 2) => (0 1) 
;; (seq 0) => ()
;; (seq -2) => (-1 0)
(define (seq n)
  (define (rseq n) (if (zero? n) '() (cons (1- n) (rseq (1- n)))))
  (if (< n 0) (map - (rseq (- n))) (reverse (rseq n))))



;; returns the elements with "even" indices
;; (evens '(a b c d)) => (a c)
(define (evens lst)
  (map (lambda (x) (list-ref lst x)) (filter even? (seq (length lst)))))

;; returns the elements with "odd" indices
;; (odds '(a b c d e) => (b d)
(define (odds lst)
  (map (lambda (x) (list-ref lst x)) (filter odd? (seq (length lst)))))

;; true if the list l is comprised of atoms
(define (simple-list? l)	
  (apply andf (map atom? l)))

;; true if l is either an atom or a simple list (as above)
(define (atom-or-simple-list? l)	
  (or (atom? l)
		(apply andf (map atom? l))))

;; True if the list contains a list
(define (nested-list? l)
	 (and (pair? l) (pair? (car l)) (null? (cdr l))))

;; The name says it all -- selector is a function which takes the whole
;; list, lst, and returns a predicate which is used in a filter call over lst.
(define (axiom-of-choice selector lst)
  (let ((M (selector lst)))
    (filter M lst)))

(define (maxima lst)
  (axiom-of-choice
   (lambda (lst)
     (let ((M (apply max lst)))
       (lambda (x) (= M x))))
   lst))

(define (minima lst)
  (axiom-of-choice
   (lambda (lst)
     (let ((M (apply min lst)))
       (lambda (x) (= M x))))
   lst))


;; (make-tuples '((0 1 2 3 4 ... n) (10 11 ... (+10 n))) => ((0 10) (1 11) ...(n n))
;;(define (make-tuples f)
;;  (map (lambda (x y) (list x y)) (car f) (cadr f)))

(define (make-tuples f)  ;;; This version is much better.   
	 (apply map (lambda x x) f))

;; return the cross product of two lists (state spaces)
(define (cross2 a b)
  (if (atom? b) (cross2 a (list b))
		(apply append (map (lambda (x) (map (lambda (y) 
														  (list x y)) b)) a)))
  )

;; return the cross product of n lists (state spaces)
(define (cross . args)
  (define (cross2 a b)
	 (if (atom? b) 
		  (cross2 a (list b))
		  (apply append 
					(map 
					 (lambda (x) 
						(map (lambda (y) 
								 (if (list? y)
									  (cons x y)
									  (list x y))) b)) a))))
  (cond
	((null? args) '())
   ((atom? args) (list args))
	((not (list? args)) (bad-argument))
	((= (length args) 1)
	 (car args))
	((= (length args) 2)
    (apply cross2 args))
	(#t (cross (car args) (apply cross (cdr args))))))


;; (acons a b c) => ((a . b) c)
(define (acons key val alist)
  (cons (cons key val) alist))

(define (list-sym-ref l sl s)
  (let* ((Lsl (length sl))
			(ss (member s sl))
			(Ls (if ss (length ss) #f))
			)
	 (if Ls
		  (list-ref l (- Lsl Ls))
		  (abort 'list-sym-ref:bad-index))))

;; Like list-set! but for a-lists
;; This is a mutator -- the list needs to exist first for it to work, though.
(define (assoc-set! alist key val)
  (let loop ((l alist))
    (if (null? l) 
        (append alist (cons (cons key val) '()))
        (if (equal? (caar l) key)
            (set-cdr! (car l) val)
            (loop (cdr l))))))

;; This should be called like (set! lst (assoc-append lst ...))
(define (assoc-append alist key value)
  (if (or (null? alist) (not (assoc key alist)))
      (acons key (list value) alist)
      (map (lambda (x)
             (if (and (pair? x) (equal? (car x) key))
                 (append x (list value))
                 x))
           alist)
      ))

;; This should be called like (set! lst (assoc-delete lst ...))
(define (assoc-delete alist key)
  (reverse (let loop ((a alist)(r '()))
             (if (null? a)
                 r
                 (if (and (pair? a) (pair? (car a)) (equal? (caar a) key))
                     (loop (cdr a) r)
                     (loop (cdr a) (cons (car a) r)))))))

(define (member-assoc k lst)
  (let ((n (length lst))
		  (m (member k (map car lst))))
	 (if m
		  (list-tail lst (- n (length m)))
		  #f)))

(define (list2-assoc-set! k v k2 v2) 
	 (let loop ((kl k) (vl v))
		(cond ((or (null? kl) (null? vl)) #f)
				((eq? (car kl) k2) (set-car! vl v2))
				(else (loop (cdr kl) (cdr vl))))))

(define (list2-assoc k v k2) 
	 (let loop ((kl k) (vl v))
		(cond ((or (null? kl) (null? vl)) #f)
				((eq? (car kl) k2) vl)
				(else (loop (cdr kl) (cdr vl))))))

(define-macro (++ i) `(let ((j ,i)) (set! ,i (1+ ,i)) j))

;; removes completely null branches
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

;; flattens a list and preserves depth first parsing order
(define (flatten l)
  (if (not (pair? l)) 
		l
		(let loop ((l l))
		  (let* ((f (apply append (map (lambda (x) (if (pair? x) x (list x))) l))))
			 (if (apply orf (map pair? f))
				  (loop f)
				  f)))))


(define (denull-and-flatten l)
  (denull (flatten l)))

;; (define (denull-and-flatten l)
;;   (let ((nl '()))
;;     (letrec ((daf
;;               (lambda (l)
;;                 (cond
;;                  ((null? l) '())
;;                  ((not (pair? l))
;;                   (if (not (member l nl))
;;                       (set! nl (cons l nl))
;;                       ))
;;                  ((pair? l) (daf (car l)) (daf (cdr l)))))))
;;       (daf l))
;;     nl))



;; *** Note, the "sort" included expects a function like < rather than <= ***

;; True if all the elements of the list are unique w.r.t. "equal?"
(define (unique? l)
  (if (<= (length l) 1)
      #t  
      (if (equal? (car l) (cadr l))
          #f
          (uniq? (cdr l)))))

;; True if all the elements of the list are unique w.r.t. "eq?"
(define (uniq? l)
  (if (<= (length l) 1)
      #t  
      (if (eq? (car l) (cadr l))
          #f
          (uniq? (cdr l)))))

(define (uniq lst) ;; returns the unique elements w.r.t. eq?: should be sorted
  (cond
   ((null? lst) '())
   ((null? (cdr lst)) lst)
   ((not (eq? (car lst) (cadr lst))) (cons (car lst) (uniq (cdr lst))))
   (else (uniq (cdr lst)))))

(define (unique lst) ;; returns the unique elements w.r.t. equal?: should be sorted
  (cond
   ((null? lst) '())
   ((null? (cdr lst)) lst)
   ((not (equal? (car lst) (cadr lst))) (cons (car lst) (unique (cdr lst))))
   (else (unique (cdr lst)))))

(define (unique* lst) ;; returns the unique elements w.r.t. "member": need not be sorted
  (cond               ;; Note that the *last* instance is the one that is preserved!
	((null? lst) lst)
	((not (member (car lst) (cdr lst))) (cons (car lst) (unique* (cdr lst))))
	(else (unique* (cdr lst)))))

;; This is used to generate temporary symbols and their string versions
(define (symbol->stringN k)
  (cond
   ((symbol? k) (symbol->string k))
   ((eq? k '%%_null_%%) '()) ;; This is a hack ... not really right, but it will work for the purpose
   ((not k) #f)
   (else #f)))

;; This is used to generate temporary symbols and their string versions
(define (string->symbolN k)
  (cond
   ((string? k) (string->symbol k))
   ((null? k) '%%_null_%%)  ;; This is a hack ... not really right, but it will work for the purpose
   ((not k) #f)
   (else #f)))

(define (not-member lst) (lambda (x) (not (member x  lst))))



;;---      (string->symbol-list str) -- Converts a string to a list of symbols 
;;                                      does no error checking

(define (string->symbol-list str)
  (call-with-input-string 
	str 
	(lambda (f)
	  (call-with-current-continuation
		(lambda (return)
		  (let loop ((result '()))
			 (let ((value '()))
				(set! value (read f))
				(if (not (eof-object? value))
					 (loop (cons value result))
					 (return (reverse result))))))))))


;;; (define (range min max step)
;;;   (cond
;;; 	((eq? min max) (list min))
;;; 	((and (< min max) (< step 0))
;;; 	 (range min max (- step)))

;;; ((and (> min max) (> step 0))
;;;  (range max min step))

;;; ((and (> min max) (< step 0))
;;;  (let loop ((l (list min))
;;; 				(x (+ min step)))
;;; 	(if (< x max)
;;; 		 l
;;; 		 (loop (append l (list x)) (+ x step)))))
;;; ((and (< min max) (> step 0))
;;;  (let loop ((l (list min))
;;; 				(x (+ min step)))
;;; 	(if (> x max)
;;; 		 l
;;; 		 (loop (append l (list x)) (+ x step)))))
;;; (else 'bad-range)))


;; guarded list reference
(define list-ref
  (letrec ((%list-ref list-ref))
    (lambda (l i)
      (if (or (< i 0) (>= i (length l)))
          (abort 'list-ref-index-out-of-bounds)
          (%list-ref l i)))))

; Takes sets an element in a list
(define (list-set! l i v)
   (if (zero? i)
       (set-car! l v)
       (list-set! (cdr l) (1- i) v)))


 (define list-set-car! list-set!)

 (define (list-set-cdr! l i v)
   (if (zero? i)
       (set-cdr! l v)
       (list-set-cdr! (cdr l) (1- i) v)))
 ;; set the value associated with key in a-list
 ;; (assoc-set! list key value)



;; list-ref that accepts a list of indices
;; (list-ref '(1 2 3 4 5) '(0 3)) => (1 4)
(define list-ref
	 (let ((olr list-ref))
		(lambda (lst ix)
		  (if (number? ix) (olr lst ix) (map (lambda (y) (olr lst y)) ix)))))


;; the index can be a list of indices 
;;     (and if it is the value must be a corresponding list)
(define list-set!
  (letrec ((%list-set! list-set!)
			  )
	 (lambda (l i v)
		(if (list? i)
			 (for-each (lambda (x y) (%list-set! l x y)) i v)
			 (%list-set! l i v)))))


;; (list-ref* '((a b c d) (e f g h i j) (k l m)) '(1 0)) => e
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


;; (define p '((a b c d) (e f g h i j) (k l m)))
;; (list-set* p '(1 0) '(tst))
;; p  => ((a b c d) ((tst) f g h i j) (k l m))
(define (list-set* lst ix vv)
  (cond
	((number? ix)
	 (list-set! lst ix vv))
	((= (length ix) 1)
	 (dnl "unit list")
	 (list-set! lst (car ix) vv))
	(else
	 (let ((tv (list-ref* lst ix)))
		(if (atom? tv)
			 ;; indices fully resolve an element
			 (let* ((short-ix (reverse (cdr (reverse ix))))
					  (tv (list-ref* lst short-ix)))
				(list-set! tv (car (reverse ix)) vv))
			 (if (= (length tv) (length vv))
				  ;; it's ok, do it
				  (list-set* (map (lambda (x) (list-ref! x (car ix)) lst) (cdr ix)) vv)
				  (abort "The value list does not have the indicated number of elements"))))
	 )))



;;
;; return the first k elements of a list (analogous to list-tail)
;;
(define (list-head the-list k)
  (if (and (> k 0) (not (null? the-list)))
		(cons (car the-list) (list-head (cdr the-list) (1- k)))
		'()))


;; remove an object from a list
(define (remove obj lst)
  (letrec ((head (list '*head*)))
    (letrec ((remove
               (lambda (lst tail)
                 (cond ((null? lst)
								lst)
							  ((not (pair? lst))
								#f)
                       ((eqv? obj (car lst)) (remove (cdr lst) tail))
                       (#t
                        (set-cdr! tail (list (car lst)))
                        (remove (cdr lst) (cdr tail)))))))
      (remove lst head))
    (cdr head)))



;; Calculate the usual norm for the list of numbers, v
(define (@norm v)
  (apply + (map * v v)))

;; map v to a unit length list representing a vector
(define (unit v)
  (let ((n (sqrt (@norm v))))
	 (if (zero? n)
		  (map (lambda (x) 0) v)
		  (map (lambda (x) (/ x n)) v))))

;; the list representation of a vector from s to d, but smart about <agent>s
(define (vector-to s d)
  (let ((src (if (isa? s <thing>) (slot-ref s 'location) s))
		  (dst (if (isa? d <thing>) (slot-ref d 'location) d)))
	 (map - dst src)))

;; returns the centroid of a list of vectors or numbers (the average)
(define (centroid lst)
  (cond 
	((null? lst) #f)
	((not (pair? lst)) #f)

	((= (length lst) 1) (car lst))
	(else 
	 (let* ((n (length lst))
			  (div (lambda (x) (/ x n))))
		(map div (apply map + lst)))))) ;; This is a nice idiom
		
		


;;; #|
;;;   Given two circles of radii r and R respectively, and a distance
;;;   between them of d, we get the area of intersection, A, to be the value calculated below.
;;;   Ref:  Weisstein, Eric W., "Circle-circle intersection." from 
;;;         Mathworld, http://MathWorld.wolfram.com/Circle-CircleIntersection.html 2010-06-30
;;; |#

(define pi (* 4.0 (atan 1.0)))
(define (intersection-of-two-circles d R r) ;; R and r are the radii, d is the distance
  (if (< R r)
		(intersection-of-two-circles d r R)
		(begin
		  (cond
			((<= (+ r d) R) (* pi r r)) ;; complete containment
			((or (<= r 0.0) (> (- d r) R)) 0.0) ;; no intersection
			(else 
			 (let ((A (- 
						  (+ (* r r 
								  (acos (/ (- (+ (* d d) (* r r)) (* R R))
											  (* 2.0 d r))))
							  (* R R 
								  (acos (/ (- (+ (* d d) (* R R)) (* r r))
											  (* 2.0 d R)))))
						  (* 0.5 (sqrt (* (+ (- d) r R) (+ d r (- R)) (+ d (- r) R) (+ d r R)))))))
				A))))))
;;; #|
  
;;;   The decay function determines what proportion of a population is available to predation: a value of
;;;   0.01 indicates that only one percent of the prey population is accessible to the predator.  The
;;;   behaviour of the function is governed by the radius of the population and a distance_decay which
;;;   controls how quickly the proportion decreases with distance.  When the distance_decay is 1.0 the
;;;   proportion available is about 0.01 at a distance equal to the radius of the population.  If the
;;;   distance_decay is in (0,...], then the distance at which we get an available proportion of 0.01 is
;;;   linearly related to the distance_decay (a distance_decay of 2 gives us a proportion of 1% at r/2 if
;;;   r is the radius.  Clearly setting the distance_decay to a small enough value will make the
;;;   spatially explicit version approximate the non-spatial version. If the distance_decay is equal to
;;;   0, then a uniform distribution over the population's disk is assumed.

;;;   For gnuplot:
;;;   pi2 = pi*pi
;;;   sqr(x) = (x*x)
;;;   f(d, dd,r) = ((sqr(r/pi2) * sqr(1.0/dd) < 0.0005) ? (d > r ? 0.0 : (1.0/(pi * sqr(r/pi2)))) : (sqr(1.0/dd) * sqr(r/pi2) / (sqr(1.0/dd) * sqr(r/pi2) + sqr(d))))

;;;   Note: the integral over this disk is 
;;;    Integral(0,2pi, 
;;; 		Integral(0, R, sqrt(distance_decay*notional_radius) * atan(r/sqrt(distance_decay*notional_radius)), d r), d theta)

;;; |#


(define (decay distance distance-decay notional_radius clip)
  (set! notional_radius (/ notional_radius (* pi pi))) ;; divide by pi to normalise it

  (let* ((sqr (lambda (x) (* x x)))
			(dd2 (sqr distance_decay))
			(nr2 (sqr notional_radius))
			(d2 (sqr distance))
			(r 0.0)
			;; "density"
			;;(s (if (> dd2 1.0) (/ 1.0 ( * pi nr2)) (+ (- 1.0 (sqrt (abs distance_decay))) (/ (sqrt (abs distance_decay)) (* pi nr2)))))
			;; "biomass"
			(s 1.0)
			)
	 
	 (if (or (< dd2 0.0001) (< (abs (/ nr2 dd2)) 0.0005))
		  (if (or (<= distance notional_radius) (not clip)) (set! r 1.0))
		  (if (and clip (> distance (* notional_radius 1.1)))
				(set! r 0.0)
				(let ((dd (sqr (/ notional_radius distance))))
				  (set! r (/ dd (+ dd d2))))))
	 (* s r)))
				

(define (symmetric-decay dist dd1 nr1 clip1 dd2 nr2 clip2)
  (* (decay d dd1 nr1 clip1) (decay d dd2 nr2 clip2)))

(define (overlap-decay dist dd1 nr1 clip1 dd2 nr2 clip2)
  (let ((i2c (intersection-of-two-circles dist nr1 nr2)))
	 (* (/ i2c (* nr1 nr1 pi)) (/ i2c (* nr2 nr2 pi)))))

(define (weighted-symmetric-decay dist dd1 nr1 clip1 dd2 nr2 clip2)
  (let ((i2c_decay (overlap-decay dist dd1 nr1 clip1 dd2 nr2 clip2)))
	 (* i2c_decay (symmetric-decay dist dd1 nr1 clip1 dd2 nr2 clip2))))

(define (prj-z=0 loc)
  (let ((l (length loc)))
	 (cond 
	  ((< l 2) (aborts "bad length in prj-xy"))
	  ((= l 2) loc)
	  (else (append (list (car loc) (cadr loc)) (make-list (- l 2) 0))))))

;; (length coeff) should be (1+ (length loc))
(define (plane coeff)
  (lambda (loc)
	 (+ (car coeff) (apply + (map * (prj-z=0 loc) (prj-z=0 (cdr coeff)))))))




(define (pno n . k)
  (let ((n (number->string (/ (truncate (* 100.0 n)) 100.0))
			  ))
	 (if (null? k)
		  n
		  (string-append (make-string (- (car k) (string-length n)) #\0) n))))



(define (do-map-conversion pfn gfn)
  (let ((cmd (string-append "gs -q -dQUIET -dNOPAUSE -r300x300 -sDEVICE=pnggray -sOutputFile=" gfn " - " pfn " < /dev/null &")))
	 (kdnl* '(log-* do-map-conversion) "[" (my 'name) ":" (class-name-of self) "]" "Running" cmd (class-name-of self))
	 (shell-command cmd)))







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

(define pagesize '(595 841))

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
;-  Identification and Changes

;--
;	lists.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.12.13
;		Location: odin:/home/gray/study/src/lists.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2012 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 


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

;; guarded
(define list-ref
  (letrec ((%list-ref list-ref))
    (lambda (l i)
      (if (or (< i 0) (>= i (length l)))
          (abort 'list-ref-index-out-of-bounds)
          (%list-ref l i)))))

; Takes sets an element in a list
(define (list-set! l i v)
   (if (zero? i)
       (set-car! l v)
       (list-set! (cdr l) (1- i) v)))


;; the index can be a list of indices
;; list-ref that accepts a list of indices
(define list-ref
	 (let ((olr list-ref))
		(lambda (lst ix)
		  (if (number? ix) (olr lst ix) (map (lambda (y) (olr lst y)) ix)))))


;; the index can be a list of indices (and if it is the value must be a corresponding list)
(define list-set!
  (letrec ((%list-set! list-set!)
			  )
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

(define (list-set* lst ix vv)
  (cond
	((number? ix)
	 (list-set! lst ix vv))
	((= (length ix) 1)
	 (dnl "unit list")
	 (list-set! lst (car ix) vv))
	(else
	 (let ((tv (list-ref* lst ix)))
		(if (atom? tv)
			 ;; indices fully resolve an element
			 (let* ((short-ix (reverse (cdr (reverse ix))))
					  (tv (list-ref* lst short-ix)))
				(list-set! tv (car (reverse ix)) vv))
			 (if (= (length tv) (length vv))
				  ;; it's ok, do it
				  (list-set* (map (lambda (x) (list-ref! x (car ix)) lst) (cdr ix)) vv)
				  (abort "The value list does not have the indicated number of elements"))))
	 )))






;; The dimensions of this are 2 3 3 3
(define doink '((((a b c) (d e f) (g h i)) 
					  ((k l m) (n o p) (q r s)) 
					  ((t u v) (w 1 2) (3 4 5)))
					 (((A B C) (D E F) (G H I)) 
					  ((K L M) (N O P) (Q R S)) 
					  ((T U V) (W 10 20) (30 40 50)))
					 )
  )




;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
;;; File   : sort.scm
;;; Author : Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
;;; Updated: 11 June 1991
;;; Defines: sorted?, merge, merge!, sort, sort!

;;; --------------------------------------------------------------------
;   Many Scheme systems provide some kind of sorting functions.  They do
;   not, however, always provide the _same_ sorting functions, and those
;   that I have had the opportunity to test provided inefficient ones (a
;   common blunder is to use quicksort which does not perform well).
;   Because sort and sort! are not in the standard, there is very little
;   agreement about what these functions look like.  For example, Dybvig
;   says that Chez Scheme provides
;	(merge predicate list1 list2)
;	(merge! predicate list1 list2)
;	(sort predicate list)
;	(sort! predicate list),
;   while the MIT Scheme 7.1 manual, following Common Lisp, offers
;	(sort list predicate),
;   TI PC Scheme offers
;	(sort! list/vector predicate?)
;   and Elk offers
;	(sort list/vector predicate?)
;	(sort! list/vector predicate?)
;   Here is a comprehensive catalogue of the variations I have found.
;   (1) Both sort and sort! may be provided.
;   (2) sort may be provided without sort!
;   (3) sort! may be provided without sort
;   (4) Neither may be provided
;   ---
;   (5) The sequence argument may be either a list or a vector.
;   (6) The sequence argument may only be a list.
;   (7) The sequence argument may only be a vector.
;   ---
;   (8) The comparison function may be expected to behave like <
;   (9) or it may be expected to behave like <=
;   ---
;   (10) The interface may be (sort predicate? sequence)
;   (11) or (sort sequence predicate?)
;   (12) or (sort sequence &optional (predicate? <))
;   ---
;   (13) The sort may be stable
;   (14) or it may be unstable.
;   ---
;   All of this variation really does not help anybody.  A nice simple
;   merge sort is both stable and fast (quite a lot faster than `quick'
;   sort).
;   I am providing this source code with no restrictions at all on its
;   use (but please retain D.H.D.Warren's credit for the original idea).
;   You may have to rename some of these functions in order to use them
;   in a system which already provides incompatible or inferior sorts.
;   For each of the functions, only the top-level define needs to be
;   edited to do that.
;   I could have given these functions names which would not clash with
;   any Scheme that I know of, but I would like to encourage implementors
;   to converge on a single interface, and this may serve as a hint.
;   The argument order for all functions has been chosen to be as close
;   to Common Lisp as made sense, in order to avoid NIH-itis.
;
;   Each of the five functions has a required *last* parameter which is
;   a comparison function.  A comparison function f is a function of 2
;   arguments which acts like <.  For example,
;	(not (f x x))
;	(and (f x y) (f y z)) => (f x z)
;   The standard functions <, >, char<?, char>?, char-ci<?, char-ci>?,
;   string<?, string>?, string-ci<?, and string-ci>? are suitable for
;   use as comparison functions.  Think of (less? x y) as saying when
;   x must *not* precede y.
;
;   (sorted? sequence less?)
;	returns #t when the sequence argument is in non-decreasing order
;	according to less? (that is, there is no adjacent pair ... x y ...
;	for which (less? y x))
;	returns #f when the sequence contains at least one out-of-order pair.
;	It is an error if the sequence is neither a list nor a vector.
;
;   (merge list1 list2 less?)
;	This merges two lists, producing a completely new list as result.
;	I gave serious consideration to producing a Common-Lisp-compatible
;	version.  However, Common Lisp's `sort' is our `sort!' (well, in
;	fact Common Lisp's `stable-sort' is our `sort!', merge sort is
;	*fast* as well as stable!) so adapting CL code to Scheme takes a
;	bit of work anyway.  I did, however, appeal to CL to determine
;	the *order* of the arguments.
;
;   (merge! list1 list2 less?)
;	merges two lists, re-using the pairs of list1 and list2 to build
;	the result.  If the code is compiled, and less? constructs no new
;	pairs, no pairs at all will be allocated.  The first pair of the
;	result will be either the first pair of list1 or the first pair
;	of list2, but you can't predict which.
;	
;	The code of merge and merge! could have been quite a bit simpler,
;	but they have been coded to reduce the amount of work done per
;	iteration.  (For example, we only have one null? test per iteration.)
;
;   (sort sequence less?)
;	accepts either a list or a vector, and returns a new sequence which
;	is sorted.  The new sequence is the same type as the input.  Always
;	(sorted? (sort sequence less?) less?).
;	The original sequence is not altered in any way.  The new sequence
;	shares its _elements_ with the old one; no elements are copied.
;
;   (sort! sequence less?)
;	returns its sorted result in the original boxes.  If the original
;	sequence is a list, no new storage is allocated at all.  If the
;	original sequence is a vector, the sorted elements are put back
;	in the same vector.
;
;   Note that these functions do NOT accept a CL-style ":key" argument.
;   A simple device for obtaining the same expressiveness is to define
;   (define (keyed less? key) (lambda (x y) (less? (key x) (key y))))
;   and then, when you would have written
;	(sort a-sequence #'my-less :key #'my-key)
;   in Common Lisp, just write
;	(sort! a-sequence (keyed my-less? my-key))
;   in Scheme.
;;; --------------------------------------------------------------------


;;; (sorted? sequence less?)
;;; is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
;;; such that for all 1 <= i <= m,
;;;	(not (less? (list-ref list i) (list-ref list (- i 1)))).

(define (sorted? seq less?)
    (cond
	((null? seq)
	    #t)
	((vector? seq)
	    (let ((n (vector-length seq)))
		(if (<= n 1)
		    #t
		    (do ((i 1 (+ i 1)))
			((or (= i n)
			     (less? (vector-ref seq (- i 1))
			     	    (vector-ref seq i)))
			    (= i n)) )) ))
	(else
	    (let loop ((last (car seq)) (next (cdr seq)))
		(or (null? next)
		    (and (not (less? (car next) last))
			 (loop (car next) (cdr next)) )) )) ))


;;; (merge a b less?)
;;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;;; and returns a new list in which the elements of a and b have been stably
;;; interleaved so that (sorted? (merge a b less?) less?).
;;; Note:  this does _not_ accept vectors.  See below.

(define (merge a b less?)
    (cond
	((null? a) b)
	((null? b) a)
	(else (let loop ((x (car a)) (a (cdr a)) (y (car b)) (b (cdr b)))
	    ;; The loop handles the merging of non-empty lists.  It has
	    ;; been written this way to save testing and car/cdring.
	    (if (less? y x)
		(if (null? b)
		    (cons y (cons x a))
		    (cons y (loop x a (car b) (cdr b)) ))
		;; x <= y
		(if (null? a)
		    (cons x (cons y b))
		    (cons x (loop (car a) (cdr a) y b)) )) )) ))


;;; (merge! a b less?)
;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept vectors.

(define (merge! a b less?)
    (define (loop r a b)
	(if (less? (car b) (car a))
	    (begin
		(set-cdr! r b)
		(if (null? (cdr b))
		    (set-cdr! b a)
		    (loop b a (cdr b)) ))
	    ;; (car a) <= (car b)
	    (begin
		(set-cdr! r a)
		(if (null? (cdr a))
		    (set-cdr! a b)
		    (loop a (cdr a) b)) )) )
    (cond
	((null? a) b)
	((null? b) a)
	((less? (car b) (car a))
	    (if (null? (cdr b))
		(set-cdr! b a)
		(loop b a (cdr b)))
	    b)
	(else ; (car a) <= (car b)
	    (if (null? (cdr a))
		(set-cdr! a b)
		(loop a (cdr a) b))
	    a)))



;;; (sort! sequence less?)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(define (sort! seq less?)
    (define (step n)
	(cond
	    ((> n 2)
		(let* ((j (quotient n 2))
		       (a (step j))
		       (k (- n j))
		       (b (step k)))
		    (merge! a b less?)))
	    ((= n 2)
		(let ((x (car seq))
		      (y (cadr seq))
		      (p seq))
		    (set! seq (cddr seq))
		    (if (less? y x) (begin
			(set-car! p y)
			(set-car! (cdr p) x)))
		    (set-cdr! (cdr p) '())
		    p))
	    ((= n 1)
		(let ((p seq))
		    (set! seq (cdr seq))
		    (set-cdr! p '())
		    p))
	    (else
		'()) ))
    (if (vector? seq)
	(let ((n (vector-length seq))
	      (vector seq))			; save original vector
	    (set! seq (vector->list seq))	; convert to list
	    (do ((p (step n) (cdr p))		; sort list destructively
		 (i 0 (+ i 1)))			; and store elements back
		((null? p) vector)		; in original vector
		(vector-set! vector i (car p)) ))
	;; otherwise, assume it is a list
	(step (length seq)) ))


;;; (sort sequence less?)
;;; sorts a vector or list non-destructively.  It does this by sorting a
;;; copy of the sequence.  My understanding is that the Standard says
;;; that the result of append is always "newly allocated" except for
;;; sharing structure with "the last argument", so (append x '()) ought
;;; to be a standard way of copying a list x.

(define (sort seq less?)
    (if (vector? seq)
	(list->vector (sort! (vector->list seq) less?))
	(sort! (append seq '()) less?)))

;;; eof


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
;-  Identification and Changes

;--
;	class.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.30



;-  Code 


(include "framework-declarations.scm")
(include "model-declarations.scm")


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
;-  Identification and Changes

(declare-method introspection-list "return the introspection list")
(declare-method set-introspection-list! "set the list of agents to be examined")
(declare-method introspection-times "return the introspection list")
(declare-method set-introspection-times! "set the list of agents to be examined")
(declare-method page-preamble "open files & such")
(declare-method log-this-agent "a lambda that produces output")
(declare-method page-epilogue "make things tidy")
(declare-method set-variables! "set the list of variables")
(declare-method extend-variables! "extend the list of variables")


(declare-method schedule-times "return a schedule")
(declare-method schedule-epsilon "return a schedule's epsilon")
(declare-method set-schedule-times! "set the times the agents is scheduled to do something")
(declare-method set-schedule-epsilon! "set the epsilon for the schedule")
(declare-method insert-schedule-time! "insert a time into a schedule")
;;(declare-method flush-stale-schedule-entries! "name says it all, really...")
;;(declare-method scheduled-now? "name says it all, really...")

(declare-method log-data "err, ...log data to an open output") 
(declare-method emit-page "set the list of agents to be examined")
(declare-method open-p/n "open a port for logging")
(declare-method close-p/n "close a logging port")


(declare-method map-log-track-segment "description needed")
(declare-method map-projection "description needed")
(declare-method set-map-projection! "description needed")
(declare-method map-log-data "specific for postscript output")
(declare-method map-emit-page "specific for postscript output")

(declare-method data-log-track-segment "description needed")
(declare-method data-log-data "specific for data output")
(declare-method data-emit-page "specific for data output")

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
;- Identification and Changes

;- Load initial libraries 

;; support.scm *must* be loaded before this file

;;-------------------------------------------;;
;; This is the order things must happen in   ;;
;; any file defining methods or model bodies ;;
;;---------------------------------------------
;; (include "framework-preamble%.scm")
;; (load-model-framework)
;;---------------------------------------------


;-- Define/allocate new classes

;--- substrate

;(define <pair>        (make-primitive-class))
(define <list>        (make-primitive-class)) ;; add a "list" primitive ... different from "pair"

;(define <null>        (make-primitive-class))
;(define <symbol>      (make-primitive-class))
;(define <boolean>     (make-primitive-class))
;(define <procedure>   (make-primitive-class <procedure-class>))
;(define <number>      (make-primitive-class))
;(define <vector>      (make-primitive-class))
;(define <char>        (make-primitive-class))
;(define <string>      (make-primitive-class))
;(define  <input-port> (make-primitive-class))
;(define <output-port> (make-primitive-class))


;--- helpers/warts



;; sclos classes
(register-class <pair>)
(register-class <null>)
(register-class <boolean>)
(register-class <symbol>)
(register-class <procedure>)
(register-class <number>)
(register-class <vector>)
(register-class <char>)
(register-class <string>)
(register-class <input-port>)
(register-class <output-port>)
(register-class <class>)
(register-class <top>)
(register-class <object>)
(register-class <procedure-class>)
(register-class <entity-class>)
(register-class <generic>)
(register-class <method>)





;--- agent based classes


(define <agent> (make-class (list <object>) 
									 '(name type representation agent-state subjective-time dt 
											  migration-test timestep-schedule kernel counter
											  map-projection
											  state-flags
											  agent-epsilon
											  agent-schedule
											  dont-log
											  agent-body-ran

											  ;; as a parent of other agents
											  subsidiary-agents active-subsidiary-agents parent-nesting-state

											  ;; as a child of other agents
											  nest-parent child-nesting-state)
											  ))

;; subsidiary-agents are agents which may be embedded in a larger dynamic agent. Agents know 
;; what their parent agent is (if they have one) and may indicate to the parent that they should be
;; added to the active list. The parent agent is the one that actually decides if a agent is to move 
;; into the active queue or out of the active queue.  Whe things get moved, "value" from the parent is 
;; moved into the relevant sub-agents.  The set of ecoservices of the parent contains all of the types 
;; represented in its sub-agents.






(register-class <agent>)

;; name is a string
;; representation is a symbol
;; subjective-time, and dt are numbers representing time and an interval, respectively
;; body is a function  (lambda (self t df . args) ...)
;; migration-test is a function  (lambda (self t df . args) ...)
;; timestep-schedule is a list of times at which the agent needs to run (a list of monotonically increasing numbers)
;; kernel is a function that can be used to interact with the kernel of the simulation




(define <tracked-agent> (make-class (list <agent>) '(track tracked-paths track-schedule track-epsilon)))
;; "track" will either be a list like (... (t_k x_k y_k) ...) or false
;; "tracked-paths" is a list of non-false traces or false
(register-class <tracked-agent>)

(define <thing> (make-class (list <tracked-agent>) '(mass dim location direction speed)))
(register-class <thing>)
;;(class-register 'add <thing> "<thing>")

(define <environment> (make-class (list <agent>) '(default-value minv maxv))) ;; bounding volume

(register-class <environment>)

;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:


;-  Identification and Changes

#|

The way this works is like so: 

	An agent is created which has a list of entities which are polled
when it runs (it calls "log-data") to cause the entities to generate
appropriate output which is sent to a port.  Before it processes the
list of entities, it calls "prepare-to-dump", and after it has
finished it calls "finished-dumping" -- these two routines handle
pagination page wrapping.

|#








(define <introspection> (make-class (list <agent>)
									  '(file filename filetype format variables variables-may-be-set 
												missing-val show-field-name preamble-state
												introspection-list 
												;;introspection-schedule 
												timestep-epsilon)))
;; file is the output handle
;; if filename is not a string, things go to stdout
;; if append-time is not false it must either be true or the size of the number (digit count)
;; if filetype is defined it must be a string and it will be appended to the filename 
;; the introspection list is the list of agent to be looked at
;; the introspection schedule is the set of times to run at (this is really probably better done with timestep-schedule)

(register-class <introspection>)

(define <logfile> (make-class (list <introspection>)
									  '()))
;; file is the output handle
;; if filename is not a string, things go to stdout
;; if append-time is not false it must either be true or the size of the number (digit count)
;; if filetype is defined it must be a string and it will be appended to the filename 
(register-class <logfile>)

(define <snapshot> (make-class (list <introspection>)
									  '(lastfile currentfile)))
;; file is the output handle
;; if filename is not a string, things go to stdout
;; if append-time is not false it must either be true or the size of the number (digit count)
;; if filetype is defined it must be a string and it will be appended to the filename 
(register-class <snapshot>)


;; This is the basis for all the logging things like the log-map agent and the log-data


(define <log-map> (make-class (list <snapshot>)
										'(ps png)))
;; ps and png indicate whether the files should be left on disk at the end of the step

(register-class <log-map>)

(define <log-data> (make-class (list <logfile>)
									  '())) 
;; projections is an association list of slot-names and the projection functions to apply before output
;; Data is generated using 

(register-class <log-data>)

(define <log-agent-table> (make-class (list <log-data>)
									  '(target-agent))) 
;; projections is an association list of slot-names and the projection functions to apply before output
;; Data is generated using 

(register-class <log-agent-table>)

(define <log-table> (make-class (list <log-data>)
									  '())) 
;; projections is an association list of slot-names and the projection functions to apply before output
;; Data is generated using 

(register-class <log-table>)

(define <log-agent-table*> (make-class (list <log-agent-table>)
									  '())) 
;; <log-agent-table*> hands off most of the output generation to the class entity being output
;; projections is an association list of slot-names and the projection functions to apply before output
;; Data is generated using 

(register-class <log-agent-table*>)

(define <log-table*> (make-class (list <log-table>)
									  '())) 
;; <log-table*> hands off most of the output generation to the class entity being output
;; projections is an association list of slot-names and the projection functions to apply before output
;; Data is generated using 

(register-class <log-table*>)


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
o;- Identification and Changes

;- Load initial libraries 

;; these must be loaded before this is
;;(include


;;-------------------------------------------;;
;; This is the order things must happen in   ;;
;; any file defining methods or model bodies ;;
;;---------------------------------------------
;; (include "%framework.scm")
;; (load-model-framework)
;;---------------------------------------------


;;---------------------------------------------------
;; Important routines which I Really Ought to Know ;;
;;---------------------------------------------------


;(define wally (make <animal> '(location (0 0 0) dim 3 direction (1 0 0) mass 3.2 age 1 dt 0.1 representation wombat name "wally" )))

;- Utility functions

(define show-warnings #f)
(define kernel-messages '())
(define temporal-fascist #f) ;; This can make things quite picky.

(define (kdnl* msg . args)
  (if (or (member msg kernel-messages) (member '* kernel-messages)
			 (and (list? msg) (not (null? (intersection msg kernel-messages)))))
		(begin
		  (display msg)(display " ==> ")
		  (apply dnl* args))))

(define (warning . args)
  (if show-warnings
		(begin
		  (display "*** warning: ")
		  (apply dnl* args)
		  )))





;-- Define/allocate new classes

;--- substrate

;--- helpers/warts

;--- agent based classes

;-- handy macros that need class definitions

;-- Define/allocate new generic methods

;; This is defined all in one place so we don't get rogue redefinitions of a generic method clobbering 
;; the definitions which occur earlier in the file. I don't know if tiny-clos can trap such things, but
;; it seems simple enough to split things so that it doesn't happen, though I prefer to keep a class all 
;; in one place.  Ditto for the "make-class" calls.


;--- Helper classes (wart classes)
; none yet

;--- Agent classes

;---- <agent>

;--- Helper classes (wart classes)

;----- (initialise) ;; All subclasses can use this to  initialise things (or make mass assignments 
(sclos-method <object> (initialise self args) ;; args should be a list of the form ('tag value 'tag value ...), slotlist is a list of valid slotnames
				  (kdnl* 'initialise "[" (my 'name) ":" (class-name-of self) "]" "running initialise with " args)
				  (if (and (pair? args) (pair? (car args)) (= (length args) 1))
						(set! args (car args)))
				  (let ((slots  (map car (class-slots (class-of self)))))
					 (kdnl* 'initialise-slots "[" (my 'name) ":" (class-name-of self) "]" "Slots available are: " slots)
					 (for-each 
					  (lambda (slotname argval)
						 (if (member slotname slots)
							  (set-my! slotname argval)
							  (display (string-append "Use of undeclared class variable: "
															  (if (string? slotname) slotname (object->string slotname)) " in " (symbol->string (class-name-of self)) "\n"))
							  )
						 )
					  (evens args) (odds args)))
				  )

(sclos-method <class> (run self pt pstop pkernel)
				  (kdnl* self "[" (my 'name) ":" (class-name-of self) "]" " is not an agent, so we cannot run it.!"))
	
(sclos-method <object> (run self pt pstop pkernel)
				  (kdnl* self "[" (my 'name) ":" (class-name-of self) "]" " is not an agent, so we cannot run it.!"))
	

;--- Agent classes
;---- <agent> methods


;----- (initialize) 

(sclos-method <agent> (initialize self args)
				  (initialise self (list 'state-flags '() 'subjective-time 0.0 'dt 1.0 
												 'migration-test uninitialised 
												 'counter 0 'map-projection (lambda (x) x)
												 'agent-schedule '() 'agent-epsilon 1e-6
												 'agent-state 'ready-for-prep 
												 'agent-body-ran #f

												 'dont-log '(ready-for-prep agent-body-ran agent-schedule agent-epsilon map-projection ;; agent things
																					 counter migration-test state-flags dont-log
																					 timestep-schedule kernel 
																					 introspection-list introspection-schedule timestep-epsilon ;; log agent things
																					 dims ;; thing things
																					 default-value minv maxv ;; environment things
																					 plateau-interval growth-rate ;; ecoservice things
																					 service-list service-update-map update-equations terrain-function dump-times scale ;; landscape things
																					 log-services-from-patch log-patches-from-habitat
																					 domain-attraction food-attraction ;; animal things
																					 near-food-attraction searchspeed wanderspeed foragespeed	
																					 movementspeed foodlist homelist breedlist habitat
																					 )
												 ))
				  (initialize-parent) ;; call "parents" last to make the initialisation list work
				  (initialise self args))


(sclos-method <agent> (agent-prep self . args)
				  (slot-set! self 'timestep-schedule (unique (sort (slot-ref self 'timestep-schedule) <))) ;; ensures no duplicate entries
				  (if (eq? (slot-ref self 'agent-state) 'ready-for-prep)
						(slot-set! self 'agent-state 'ready-to-run)
						(abort (string-append (name self) " has been instructed to prep but it's state is " (my 'agent-state)))
))


(sclos-method <agent> (agent-shutdown self . args) ;; Termination can happen from any state
				  (slot-set! self 'agent-state 'terminated))

;; (add-method initialize
;; 			(make-method (list <agent>)
;; 								 (lambda (initialize-parent self args)
;; 									;;(dnl "<agent> init")
;; 									(initialise self (list 'subjective-time 0.0 'dt 1.0 'migration-test uninitialised))
;; 									(initialize-parent) ;; call "parents" last to make the initialisation list work
;; 									(initialise self args)
;; 									)))

;----- (dump) ;; This dumps all the slots from agent up.  The class-slots of <class> aren't included 

(sclos-method <agent> (dump self . count)
				  (set! count (if (null? count) 0 (car count)))
				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) (display (make-string count #\space)) (display x) (display ": ") (display y)(newline))
								  slots vals)))

;; This is the default log method in the absence of more specific code.

(sclos-method (<agent>) (log-data self logger format caller targets)
				  (kdnl* '(log-* log-data) (name self) "[" (my 'name) ":" (class-name-of self) "]" "in <agent>:log-data")
				  (let ((file (slot-ref logger 'file))
						  (show-field-name (slot-ref logger 'show-field-name))
						  (missing-val (slot-ref logger 'missing-val))
						  (spaced-out #f)
						  (cntr 0)
						  )

					 (for-each ;; field in the variable list
					  (lambda (field)
						 (if show-field-name
							  (begin
								 (if (not spaced-out)
									  (set! spaced-out #t)
									  (display " " file))
								 (display field file)))
						 
						 (cond
						  ((has-slot? self field)
							(kdnl* '(log-* log-data logging-debug) "     " (name self) (class-name-of self) "Dumping " field "=" (if (has-slot? self field) (slot-ref self field) "missing!"))
							(if (not spaced-out)
								 (set! spaced-out #t)
								 (display " " file))
							(display (slot-ref self field) file)
							)
						  ((member field (extra-variable-list self))
							(kdnl* '(log-* log-data logging-debug) "     " (name self) (class-name-of self) "Dumping extra " field "=" (extra-variable self field))
							(if (not spaced-out)
								 (set! spaced-out #t)
								 (display " " file))
							(display (extra-variable self field) file)
							)
						  (missing-val
							(if (not spaced-out)
								 (set! spaced-out #t)
								 (display " " file))
							(display missing-val file))
						  (else
							(if (not spaced-out)
								 (set! spaced-out #t)
								 (display " " file))
							)	
						  )
						 )
					  (unique (if #t targets (filter (not-member (my 'dont-log)) targets))))
					 (newline file)
					 )
				  )

(sclos-method (<agent>) (run-agents self t dt agentlist run)
							 (for-each (lambda (x) 
											 (if (< (subjective-time x) (+ t dt))
														(run x t (+ t dt) (my 'kernel))
														(dnl* "Skipping" (name x)))
											 )
										  agentlist
										  )
							 )

(sclos-method (<agent>) (run-nested-agents self t dt run)
				  (let ((al (my 'subsidiary-agents)))
					 (if (not (null? al))
						  (run-agents self t dt al run))
					 ))
					 


;----- (name) 

(sclos-method <agent> (name self)
				  (if (not (or (string? (my 'name)) (eq? (my 'name) #f)))
						(abort "agent:name -- not a string")
						(my 'name)))

;(sclos-method <agent> (name self)
;				  (my 'name))

;; (add-method name
;; 				(make-method (list <agent>)
;; 								 (lambda (name-parent self)
;; 									(slot-ref self 'name))))

;----- (set-name!) 

(sclos-method (<agent> <string>) (set-name! self n)
				  (if (string? n)
						(set-my! self 'name n)
						(abort "agent:set-name! -- arg is not a string")))

;(sclos-method (<agent> <string>) (set-name! self n)
;				(set-my! self 'name n))


;; (add-method set-name!
;; 				(make-method (list <agent> <string>)
;; 								 (lambda (set-name!-parent self n)
;; 									(if (string? n)
;; 										 (slot-set! self 'name n)
;; 										 (abort "agent:set-name! -- arg is not a string"))
;; 										 )))

(define undefined (lambda x 'undefined))
(define undefined-state-flag (lambda x 'undefined-state-flag))

(sclos-method <agent> (type self)
				  (my 'type))

;;

(sclos-method <agent> (set-type! self newtype)
				  (set-my! 'type newtype))

;;

(sclos-method (<agent> <symbol>) (set-state-flag! self sym val)
				  (let ((v (assoc sym (my 'state-flags))))
					 (if v
						(set-cdr! v val)
						(set-my! 'state-flags (cons (cons sym val)  (my 'state-flags)))
						)
					 ))

(sclos-method (<agent> <symbol>) (add-state-flag self sym val)
				  (if (assoc sym (my 'state-flags))
						(set-state-flag! self sym val)
						(set-my! 'state-flags (cons (cons sym val)  (my 'state-flags)))
						))


(sclos-method (<agent> <symbol>) (state-flag self sym)
				  (let ((r (assoc sym (my 'state-flags))))
				  (if r 
						(cdr r)
						undefined-state-flag)))
						

;----- (representation) 
(sclos-method <agent> (representation self)
				  (let ((rep (my 'representation)))
					 (if (symbol? rep)
						  rep
						  (abort "agent:representation --  not a symbol"))))


;; (add-method representation
;; 				(make-method (list <agent>)
;; 								 (lambda (representation-parent self)
;; 									(my 'representation))))

;----- (set-representation!) 
(sclos-method (<agent> <string>) (set-representation! self n)
				  (if (symbol? n)
						(set-my! self 'representation n)
						(abort "agent:set-representation! -- arg is not a symbol"))
				  )

;; (add-method set-representation!
;; 				(make-method (list <agent> <string>)
;; 								 (lambda (set-representation!-parent self n)
;; 									(if (symbol? n)
;; 										 (slot-set! self 'representation n)
;; 										 (abort "agent:set-representation! -- arg is not a symbol"))
;; 										 )))

;----- (subjective-time) 
(sclos-method <agent> (subjective-time self)
				  (my 'subjective-time))

;; (add-method subjective-time
;; 				(make-method (list <agent>)
;; 								 (lambda (subjective-time-parent self)
;; 									(my 'subjective-time))))

;----- (set-subjective-time!) 
(sclos-method (<agent> <number>) (set-subjective-time! self n)
				  (if (number? n)
						(slot-set! self 'subjective-time n)
						(abort "agent:set-subjective-time! -- arg is not a number")))

;; (add-method set-subjective-time!
;; 				(make-method (list <agent> <number>)
;; 								 (lambda (set-subjective-time!-parent self n)
;; 									(if (number? n)
;; 										 (slot-set! self 'subjective-time n)
;; 										 (abort "agent:set-subjective-time! -- arg is not a number"))
;; 										 )))

;----- (migration-test) 
(add-method migration-test
				(make-method (list <agent>)
								 (lambda (migration-test self)
									(slot-ref self 'migration-test))))

;----- (set-migration-test!) 
(add-method set-migration-test!
				(make-method (list <agent> <number>)
								 (lambda (set-migration-test!-parent self ntest)
									(if (procedure? ntest)
										 (slot-set! self 'migration-test ntest)
										 (abort "agent:set-migration-test! -- arg is not a procedure"))
									)))

;----- (timestep-schedule) 
(add-method timestep-schedule
				(make-method (list <agent>)
								 (lambda (timestep-schedule self)
									(slot-ref self 'timestep-schedule))))


;----- (set-timestep-schedule!) 
(add-method set-timestep-schedule!
				(make-method (list <agent> <number>)
								 (lambda (set-timestep-schedule!-parent self nbody)
									(if (list? nbody)
										 (slot-set! self 'timestep-schedule nbody)
										 (abort "agent:set-timestep-schedule! -- arg is not a procedure"))
									(slot-set! self 'timestep-schedule nbody)
									)))

;----- (kernel) 
(add-method kernel
				(make-method (list <agent>)
								 (lambda (kernel self)
									(slot-ref self 'kernel))))

;----- (set-kernel!) 
(add-method set-kernel!
				(make-method (list <agent> <number>)
								 (lambda (set-kernel! self n)
									(if (number? n)
										 (slot-set! self 'kernel n)
										 (abort "agent:set-kernel! -- arg is not a number"))
									)))



(sclos-method (<agent>) (snapshot self)
				  (map (lambda (x) (list x (slot-ref self x))) (class-slots (class-of self))))


(sclos-method <agent> (i-am self) (my 'representation))
(sclos-method (<agent>) (is-a self list-of-kinds) (member (my 'representation) list-of-kinds))
(sclos-method <agent> (parameter-names self) (map car (class-slots (class-of self))))
(sclos-method <agent> (parameters self) (map (lambda (x) (slot-ref self x)) (map car (class-slots (class-of self)))))
;;(sclos-method <agent> (set-parameters! newparams) (for-each (lambda (x y) (slot-set! self x y)) (parameter-names self) (parameters self)))
(sclos-method (<agent> <pair>) (set-parameters! self newparams) (for-each (lambda (x y) (slot-set! self x y)) (parameter-names self) newparams))

(sclos-method (<agent> <symbol>) (extra-variable self field) #!void)
(sclos-method (<agent> <symbol>) (extra-variable-list self) '())


(sclos-method <agent> (query self kernel . args) (apply (my 'kernel) (append (list 'query) args)))
(sclos-method <agent> (run-at self x) 
				  (let ((tq (cons x (my 'timestep-schedule))))
					 (set-my! 'timestep-schedule (sort tq <=))))

;; This gets called by the kernel, and in turn calls "run-model-body"
;; which calls "model-body" ...  The main purpose of this layer is to
;; rectify time (e.g. ensure that the upcoming tick starts when it is
;; supposed to) and to update the subjective time.

#|
(sclos-method <agent> (run self T pstop pkernel)
				  (kdnl* 'run-model-body "<agent>" (class-name-of self))
				  (let* ((t T)         ;; T is the time to start, 
							(stop pstop)     ;; pstop is the upper limit of the step
							(kernel pkernel) ;; this is used to get non-local information	
							(ttr (let ()     ;; reset the times to run list
									 (set-my! 'timestep-schedule (prune-local-run-queue (my 'subjective-time) (my 'timestep-schedule)))
									 (my 'timestep-schedule)))
							(dt (interval t (slot-ref self 'dt) stop ttr))  ;; work out the dt for this step
							(subj-time (my 'subjective-time)) ;; get the subjective time for the agent
							(DT 0) ;; this will be the total elapsed time relative to subj-time
							)
					 (set-my! 'kernel kernel)   ;; This is the function that handles kernel queries

					 (cond
					  ((< subj-time t)  ;; This will be the usual case
						(if temporal-fascist
							 (begin
								(kdnl* "[" (my 'name) ":" (class-name-of self) "]" "a/an" (my 'representation) "is lost in time at" subj-time "or" t) 
								'missing-time)

							 (let loop-through-time ((st subj-time)
															 (ddt (min (- t subj-time) (my 'dt)))
															 )
								(kdnl* 'passing-control-to-model "[" (my 'name) ":" (class-name-of self) "]" "Passing control to the model at" t "for" (if (< st t) ddt  (- (+ t dt) subj-time)))
								(let ((m (run-model-body self subj-time (if (< st t) ddt  (- (+ t dt) subj-time)) )))
								  (cond
									((eq? m #!void) (abort (string-append "The model body for " (class-name-of self) " returned #!void which is an error")))
									((eq? dt #!void) (abort (string-append "dt for " (class-name-of self) " is somehow #!void which is an error")))
									((eq? DT #!void) (abort (string-append "DT for " (class-name-of self) " is somehow #!void which is an error")))
									((number? m) 
									 (set! DT (+ DT m))
									 (set! st (+ st ddt))
									 (set! subj_time st)

									 (cond
									  ((< st t) (loop-through-time st (min (- t subj-time) (my 'dt))))
									  ((< st (+ t dt)) (loop-through-time st (min (- t subj-time) (my 'dt) dt)))
									  )
									 ((or (symbol? m) (list? m))
									  (kdnl* "BORK!!!" m))
									 (else
									  (kdnl* "BORK!!!" m))
									 ))
								  m)
								))
						)
						
						((and (> dt 0.0) ;; 
								(>= subj-time (+ t  dt)))
						 (kdnl* "[" (my 'name) ":" (class-name-of self) "]" "a/an" (my 'representation) "is driving a DeLorian.  Expected subjective-time to be" t "but it was" subj-time "and dt =" dt) 
						 'back-to-the-future)
						(#t
						 (kdnl* 'passing-control-to-model "[" (my 'name) ":" (class-name-of self) "]" "Passing control to the model at" t "for" dt)
						 (let ((m (run-model-body self t dt)))
							(set! DT (+ DT m))
							m)
						 )
						)

					 (if (zero? DT) (and (dnl "*******************************") (abort "BAD TICK")))

					 (set-subjective-time! self (+ DT (my 'subjective-time)))
					 (kdnl* '(nesting run-model-body) (class-name-of self) (name self) "Leaving run with " DT " @ " (my 'subjective-time) "[" (my 'dt) "]")
					 'ok
					 )
				  )
|#

(add-method
   run
   (make-method
    (list <agent>)
    (lambda (run-parent self T pstop pkernel)
      (let ((my (lambda (x) (slot-ref self x)))
            (set-my! (lambda (x y) (slot-set! self x y)))
            (kernel (slot-ref self 'kernel)))
        (kdnl* 'run-model-body "<agent>" (class-name-of self))
        (let ((t T))
          (let ((stop pstop))
            (let ((kernel pkernel))
              (let ((ttr (begin
                           (set-my! 'timestep-schedule
                                    (prune-local-run-queue
                                     (my 'subjective-time)
                                     (my 'timestep-schedule)))
                           (my 'timestep-schedule))))
                (let ((dt (interval t (slot-ref self 'dt) stop ttr)))
                  (let ((subj-time (my 'subjective-time)))
                    (let ((DT 0))
                      (set-my! 'kernel kernel)
                      (cond ((< subj-time t)
                             (if temporal-fascist
                                 (begin
                                   (kdnl* "["
                                          (my 'name)
                                          ":"
                                          (class-name-of self)
                                          "]"
                                          "a/an"
                                          (my 'representation)
                                          "is lost in time at"
                                          subj-time
                                          "or"
                                          t)
                                   'missing-time)
                                 ((letrec ((loop-through-time
                                            (lambda (st ddt)
                                              (kdnl* 'passing-control-to-model
                                                     "["
                                                     (my 'name)
                                                     ":"
                                                     (class-name-of self)
                                                     "]"
                                                     "Passing control to the model at"
                                                     t
                                                     "for"
                                                     (if (< st t)
                                                         ddt
                                                         (- (+ t dt)
                                                            subj-time)))
                                              (let ((m (run-model-body
                                                        self
                                                        subj-time
                                                        (if (< st t)
                                                            ddt
                                                            (- (+ t dt)
                                                               subj-time)))))
                                                (cond ((eq? m #!void)
                                                       (abort (string-append
                                                               "The model body for "
                                                               (class-name-of
                                                                self)
                                                               " returned #!void which is an error")))
                                                      ((eq? dt #!void)
                                                       (abort (string-append
                                                               "dt for "
                                                               (class-name-of
                                                                self)
                                                               " is somehow #!void which is an error")))
                                                      ((eq? DT #!void)
                                                       (abort (string-append
                                                               "DT for "
                                                               (class-name-of
                                                                self)
                                                               " is somehow #!void which is an error")))
                                                      ((number? m)
                                                       (set! DT (+ DT m))
                                                       (set! st (+ st ddt))
                                                       (set! subj_time st)
                                                       (cond ((< st t)
                                                              (loop-through-time
                                                               st
                                                               (min (- t
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                               subj-time)
                            (my 'dt))))
                     ((< st (+ t dt))
                      (loop-through-time st (min (- t subj-time) (my 'dt) dt)))
                     (else #!void))
               ((or (symbol? m) (list? m)) (kdnl* "BORK!!!" m))
               (else (kdnl* "BORK!!!" m)))
              (else #!void))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                m))))
                                    loop-through-time)
                                  subj-time
                                  (min (- t subj-time) (my 'dt)))))
                            ((and (> dt 0.) (>= subj-time (+ t dt)))
                             (kdnl* "["
                                    (my 'name)
                                    ":"
                                    (class-name-of self)
                                    "]"
                                    "a/an"
                                    (my 'representation)
                                    "is driving a DeLorian.  Expected subjective-time to be"
                                    t
                                    "but it was"
                                    subj-time
                                    "and dt ="
                                    dt)
                             'back-to-the-future)
                            (#t
                             (kdnl* 'passing-control-to-model
                                    "["
                                    (my 'name)
                                    ":"
                                    (class-name-of self)
                                    "]"
                                    "Passing control to the model at"
                                    t
                                    "for"
                                    dt)
                             (let ((m (run-model-body self t dt)))
                               (set! DT (+ DT m))
                               m))
                            (else #!void))
                      (if (zero? DT)
                          (and (dnl "*******************************")
                               (abort "BAD TICK")))
                      (set-subjective-time! self (+ DT (my 'subjective-time)))
                      (kdnl* '(nesting run-model-body)
                             (class-name-of self)
                             (name self)
                             "Leaving run with "
                             DT
                             " @ "
                             (my 'subjective-time)
                             "["
                             (my 'dt)
                             "]")
                      'ok)))))))))))


(define blue-meanie #f)


;; This routine does the running since "run" has fixed up the ticks
(sclos-method <agent> (run-model-body self t ldt) ;looks like (run-model-body me t dt) in code
				  ;;  The model returns the amount of time it actually ran for
				  (kdnl* '(nesting run-model-body) (class-name-of self) "Running at " t "+" ldt "[" (my 'dt) "]")
				  (if (< dt 0.0) (abort 'bad-dt))

				  (let* ((return (model-body self t ldt)))
					 
					  ;; The model's tick is done now, adjust the subjective_time to reflect how much time it took for the tick
					 (if (number? return) 
					  	  ;;(set-my! 'subjective-time (+ t return)) 
					  	  ;; Any non-numeric return value indicate a "condition"
					  	  ;; which by definition means that no time should have been used
					  	  ;;else ... Huston, we have a problem....
					  	  (begin
					  		 (dnl (my 'name) " returned from its model body with " return)
					  		 return
					  		 ;; Just deal with it.
					  		 ))

					 ;; deal with any changes in the entity's representation, or the general configuration of the model as a whole
					 ;; prefix a symbol ('migrate, for example) to the return value if it needs to change, last bit should be "return"
					 
					 (let ((mtrb ((my 'migration-test) self t ldt return)))
						(if blue-meanie (set-my! 'kernel #f))  ;; we don't want it to make calls to a closure that has vanished

						(if mtrb
							 (if (pair? return) (cons mtrb return) (cons mtrb (list return)))
							 return))
					 )
				  )  ;; returns the amount of time it ran in the last tick, some request to the scheduler or an error condition



;; model-body knows "self" "t" "dt" and all its state variables.  
;; This particular version of the routine should not call parent-body.
(sclos-model-body <agent>
						(if #t
							 (begin
								(kdnl* 'track-subjective-times "[" (my 'name) ":" (class-name-of self) "]" " running at " (my 'subjective-time) ":" t)
								(skip-parent-body)
								))
						dt)



;---- <tracked-agent> methods

;----- (initialize) 
(add-method initialize
				(make-method (list <tracked-agent>)
								 (lambda (initialize-parent self args)
									;;(dnl "<thing> init")
									(initialise self '(track #f tracked-paths #f track-schedule '() track-epsilon 1e-6))
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))


(sclos-method (<tracked-agent> <number> <pair>) (track-locus! self t loc)
				  (let ((tr (my 'track)))
					 (set-my! 'track 
								 (if tr 
									  (append (my 'track) (list (cons t loc)))
									  (list (cons t loc))
								 ))
				  )
)

(sclos-method (<tracked-agent>) (track self)
				  (my 'track))


(sclos-method (<tracked-agent> <number> <pair>) (set-track! self t)
				  (set-my! 'track (deep-copy t))) ;; we copy it so that we aren't subject to the track changing under our feet


(sclos-method (<tracked-agent>) (new-track! self)
				  (let ((p (my 'tracked-paths))
						  (t (my 'track)))
					 (cond 
					  ((and p t) (set-my! 'tracked-paths (cons p t)))
					  (t (set-my! 'tracked-paths (list t))))
					 (set-my! 'track #f)))

(sclos-method (<tracked-agent>) (tracks self)
				  (my 'tracked-paths))

						  
(sclos-model-body <tracked-agent>
						(track-locus! self t (my 'location)) ;; even if they aren't moving
						(parent-body)
						dt
						)
						



;---- <thing> methods

;----- (initialize) 
(add-method initialize
				(make-method (list <thing>)
								 (lambda (initialize-parent self args)
									;;(dnl "<thing> init")
									(initialise self '(dim #f location #f direction #f speed #f mass #f track #f tracked-paths #f))
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))

;----- (mass) 
(add-method mass
				(make-method 
				 (list <thing>)
				 (lambda (call-parent-method self)
					(slot-ref self 'mass))))


;----- (set-mass!) 
(add-method set-mass!
				(make-method 
				 (list <thing> <number>)
				 (lambda (call-parent-method self n)
					(if (not (number? n))
						 (abort (string-append "thing:set-mass! -- bad number"))
						 (slot-set! self 'mass n)))))

;----- (dim) 
(add-method dim
				(make-method 
				 (list <thing>)
				 (lambda (call-parent-method self)
					(slot-ref self 'dim))))


;----- (set-dim!) 
(add-method set-dim!
				(make-method 
				 (list <thing> <number>)
				 (lambda (call-parent-method self n)
					(if (not (integer? n))
						 (abort (string-append "thing:set-dim! -- bad integer"))
						 (slot-set! self 'dim n)))))

;----- (speed) 
(add-method speed
				(make-method 
				 (list <thing>)
				 (lambda (call-parent-method self)
					(slot-ref self 'speed))))


;----- (set-speed!) 
(add-method set-speed!
				(make-method 
				 (list <thing> <number>)
				 (lambda (call-parent-method self n)
					(if (not (number? n))
						 (abort (string-append "thing:set-speed! -- bad number"))
						 (slot-set! self 'speed n)))))


;----- (location) 
(add-method location
				(make-method 
				 (list <thing>)
				 (lambda (call-parent-method self)
					(slot-ref self 'location))))


;----- (set-location!) 
(add-method set-location!
				(make-method 
				 (list <thing>)
				 (lambda (call-parent-method self vec)
					(if (not (= (length vec) (slot-ref self 'dim)))
						 (abort (string-append "thing:set-location! -- bad list length"))
						 (slot-set! self 'location vec)))))


;----- (direction) 
(add-method direction
				(make-method 
				 (list <pair>)
				 (lambda (call-parent-method self)
					(slot-ref self 'location))))


;----- (set-direction!) 
(add-method set-direction!
				(make-method 
				 (list <thing> <pair>)
				 (lambda (call-parent-method self vec)
					(if (not (= (length vec) (slot-ref self 'dim)))
						 (abort (string-append "thing:set-direction! -- bad list length"))
						 (slot-set! self 'direction vec)))))





;---- environment methods

(sclos-method <environment> (min-bound self)
				  (copy-list (my 'minv)))

(sclos-method <environment> (max-bound self)
				  (copy-list (my 'maxv)))

(sclos-method (<environment> <pair>) (contains? self loc)
				  (let ((mbounds (min-bound self))
						  (Mbounds (max-bound self))
						  )
					 (apply andf (append (map < mbounds eloc) (map < eloc Mbounds)))))


;; Default environment only has the default value, oddly enough
(sclos-method (<environment> <pair>) (value self loc)
				  (my 'default-value))

(sclos-method (<environment> <pair>) (set-value! self loc val)
				  (set-my! 'default-value val))

(sclos-method (<environment> <thing>) (contains? self entity)
				  (contains? (location entity))
				  )

(sclos-method (<environment> <symbol> <pair>) (value self tag loc . args)
				  (my 'default-value))

(sclos-method (<environment> <symbol> <pair>) (set-value! self tag loc val)
				  (set-my! 'default-value val))

(sclos-method (<environment> <thing>) (contains? self entity)
				  (contains? (location entity))
				  )












;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:

;-  Identification and Changes


#| 
In the logging code, a "page" is notionally a pass through the target agent list.  The page-preamble may open a file and write some
preliminary stuff, or it may just assume that the file is open already and do nothing at all.  Similarly the page-epilogue does things 
like close pages and emit "showpage" for postscript stuff.
|#





(define (schedule-times self class-sym)
  (if (eq? class-sym 'introspection) (schedule-times self T 'timestep)
		(slot-ref self (string->symbol (string-append (symbol->string class-sym) "schedule")))))

(define (schedule-epsilon self class-sym)
  (if (eq? class-sym 'introspection) (schedule-epsilon self T 'timestep)
  (slot-ref self (string->symbol (string-append (symbol->string class-sym) "epsilon")))))

(define (set-schedule-times! self class-sym lst)
  (if (eq? class-sym 'introspection) (set-schedule-times! self T 'timestep)
  (slot-set! self (string->symbol (string-append (symbol->string class-sym) "schedule")) lst)))

(define (insert-schedule-time! self class-sym t)
  (if (eq? class-sym 'introspection) (insert-schedule-time! self T 'timestep)
  (slot-set! self (string->symbol (string-append (symbol->string class-sym) "schedule")) (uniq (sort (cons t (schedule-times self class-sym)) <=)))))

(define (set-schedule-epsilon! self class-sym val)
  (if (eq? class-sym 'introspection) (set-schedule-epsilon! self T 'timestep)
		(slot-set! self (string->symbol (string-append (symbol->string class-sym) "epsilon")) val)))


(define (flush-stale-schedule-entries! self class-sym)
  (if (eq? class-sym 'introspection) (flush-stale-schedule-entries! self T 'timestep)
		(let ((cs-sym (string->symbol (string-append (symbol->string class-sym) "-schedule")))
				(ce-sym (string->symbol (string-append (symbol->string class-sym) "-epsilon"))))
		  (slot-set! self cs-sym (filter (lambda (x) (> (+ x (slot-ref self ce-sym)) (slot-ref self 'subjective-time))) (slot-ref self cs-sym))))) )


(define (scheduled-now? self T class-sym)	
  (if (eq? class-sym 'introspection) (scheduled-now? self T 'timestep)
		(let ((cs-sym (string->symbol (string-append (symbol->string class-sym) "-schedule")))
				(ce-sym (string->symbol (string-append (symbol->string class-sym) "-epsilon"))))
		  (kdnl* '(log-sched-error) "[Getting" cs-sym " = " (slot-ref self cs-sym) " and " ce-sym " = " (slot-ref self ce-sym))
		  (let ((sched (slot-ref self cs-sym)))
			 (let ((v (and (not (null? sched)) 
								(not (zero? (length (filter 
															(lambda (x) (<= (abs (- T x)) (slot-ref self ce-sym))) 
															sched)))))))
				(flush-stale-schedule-entries! self class-sym)
				v)))))

(define (introspection-filename filename filetype #!optional t)
  (if (string? filename)
		(if t
			 (string-append filename "-" (pno t 6) "0" filetype)
			 (string-append filename filetype))
		#f))





(sclos-method <agent> (map-projection self)
 				  (my 'map-projection))

(sclos-method (<agent> <procedure>) (set-map-projection! self p)
				  (set-my! 'map-projection p))


(add-method initialize
				(make-method (list <introspection>)
								 (lambda (initialize-parent self args)
									;;(dnl "<thing> init")
									(initialise self (list 'type 'introspection 
																  'introspection-list '() 
																  'timestep-epsilon 1e-6 
																  'file #f 'filename #f 'filetype #f 'format 'text 'missing-val "NoData" 'show-field-name #f
																  'preamble-state '() 'variables-may-be-set #t
																  ))
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))

(sclos-method <introspection> (agent-prep self start end kernel . args)
				  (agent-prep-parent)
				  )

(sclos-method <introspection> (agent-shutdown self . args)
				  (let ((file (my 'file)))
					 (if (and (my 'file) (output-port? (my 'file)) (not (member (my 'file) (list (current-output-port) (current-error-port)))))
						  (close-output-port file))
					 (set-my! 'file #f)
					 (agent-shutdown-parent)
					 ))



(sclos-method (<introspection> <agent>) (insert-agent! self target)
				  (set-my! 'introspection-list (cons target (my 'introspection-list))))

(sclos-method (<introspection> <agent>) (append-agent! self target)
				  (set-my! 'introspection-list (append (my 'introspection-list) (list target))))

(sclos-method <introspection> (introspection-list self) (my 'introspection-list))
(sclos-method <introspection> (introspection-times self) (my 'timestep-schedule))

(sclos-method (<introspection> <pair>) (set-introspection-list! self lst) (set-my! 'introspection-list lst))				  
(sclos-method (<introspection> <pair>) (set-introspection-times! self lst) (set-my! 'timestep-schedule lst))

(sclos-method (<introspection> <pair>) (set-variables! self lst)
				  (if (and (my 'variables-may-be-set) (list? lst))
						(set-my! 'variables lst)
						(abort "cannot extend variables after it starts running")
						))

(sclos-method (<introspection> <pair>) (extend-variables! self lst)
				  (if (and (my 'variables-may-be-set) (list? lst))
						(set-my! 'variables (unique* (append (my 'variables) lst)))
						(abort "cannot extend variables after it starts running")
						))


(sclos-method (<introspection>) (emit-page self)
				  (kdnl* '(log-* introspection-trace) "[" (my 'name) ":" (class-name-of self) "]" "Introspection: emit-page")
				  (let ((format (my 'format)))
					 (page-preamble self self format) ;; for snapshots, this will be "opening", for logfiles, it will only open the first time

					 (for-each 
					  (lambda (ila)
						 (log-data ila self format self (my 'variables))
						 #f
						 )
					  (my 'introspection-list))
					 )
				  (page-epilogue self self (slot-ref self 'format))
				  )


(sclos-model-body <introspection>
						(kdnl* '(log-* introspection-trace) "[" (my 'name) ":" (class-name-of self) "]" "Introspection: model-body")
						(let ((sched (my 'timestep-schedule))
								)

						  (set! dt (if (and (pair? sched) (< (car sched) (+ t dt))) (- (car sched) t) dt))

						  (kdnl* '(log-* introspection-trace) "      list:     " (my 'introspection-list))
						  (kdnl* '(log-* introspection-trace) "      schedule: " (list-head (my 'timestep-schedule) 3) (if (> (length (my 'timestep-schedule)) 3) '... ""))
						  
						  (set-my! 'variables-may-be-set #f)
						  (emit-page self)

						  ;(skip-parent-body)
						  (parent-body)
						  ;;(max dt (* 2.0 dt))
						  dt
						))




;---- snapshot methods
(sclos-method <snapshot> (initialize self args)
				  (initialize-parent) ;; call "parents" last to make the initialisation list work
				  (initialise self (list 'type snapshot 'lastfile #f 'currentfile #f))
				  )

(sclos-method <snapshot> (page-preamble self logger format)
				  (kdnl* '(introspection snapshot) "[" (my 'name) ":" (class-name-of self) "]" "is preparing to dump")
				  (let ((filename (my 'filename))
						  (filetype (my 'filetype))
						  (file (my 'file))
						  (t (my 'subjective-time))
						  )

					 (cond
					  ((not (or (not filename) (string? filename)))
						(abort (my 'name) " has a filename which is neither false, nor a string.  This is a fatal error."))

					  ((not (or (not filetype) (string? filetype)))
						(abort (my 'name) " has a filetype which is neither false, nor a string.  This is a fatal error."))

					  ((not (number? t))
						(abort (my 'name) " has a subjective time which is not a number. This is a fatal error."))
					  )

					 ;; Open a new file
					 (cond
					  ((not file)
						(let ((fn (introspection-filename (my 'filename) (my 'filetype) t))) 
						  (kdnl* '(introspection snapshot) "[" (my 'name) ":" (class-name-of self) "]" "opening" fn)
						  (set-my! 'lastfile (my 'currentfile))
						  (set-my! 'currentfile fn)
						  (if (zero? (string-length fn))
								(set! file (current-output-port))
								(set! file (open-output-file fn)))
						  ))
					  ((member file (list (current-output-port) (current-error-port)))
						;; do nothing really
						(kdnl* '(introspection snapshot) "[" (my 'name) ":" (class-name-of self) "]" "is writing to stdout or stderr")
						#!void
						)
					  (else 
						(kdnl* '(introspection  snapshot) "[" (my 'name) ":" (class-name-of self) "]" " has hit page-preamble with a file that is still open.  This is an error.\nClosing the file (" (my 'lastfile) ") and continuing.")
						(close-output-port file)
						(set-my! 'file #f)
						(let ((fn (introspection-filename (my 'filename) (my 'filetype) t)))
						  (set-my! 'lastfile (my 'currentfile))
						  (set-my! 'currentfile fn)
						  (if (zero? (string-length fn))
								(set! file (current-output-port))
								(set! file (open-output-file fn)))
						  )	
						)
					  )
					 (set-my! 'file file)))

(sclos-method <snapshot> (page-epilogue self logger format)
				  (let ((file (my 'file)))
					 (if (and file (not (member file (list (current-output-port) (current-error-port)))))
						  (begin
							 (kdnl* '(introspection snapshot) "[" (my 'name) ":" (class-name-of self) "]" "is closing the output port")
							 (close-output-port file)
							 (set-my! 'file #f)))))




;---- logfile methods

(sclos-method <logfile> (page-preamble self logger format)
				  (kdnl* '(introspection logfile) "[" (my 'name) ":" (class-name-of self) "]" "is preparing to dump")
				  (let ((filename (my 'filename))
						  (file (my 'file))
						  )
					 
					 (if (not (or (not filename) (string? filename)))
						  (abort (my 'name) " has a filename which is neither false, nor a string.  This is a fatal error."))

					 ;; Open a new file
					 (if (not file)
						  (begin
							 (kdnl* '(introspection logfile) "[" (my 'name) ":" (class-name-of self) "]" "is opening a log file")
							 (if (zero? (string-length filename))
								  (set! file (current-output-port))
								  (set! file (open-output-file filename)))
							 )

						  (set-my! 'file file)))
				  )

(sclos-method <logfile> (page-epilogue self logger format)
				  (kdnl* '(introspection logfile) "[" (my 'name) ":" (class-name-of self) "]" "has finished a dump")
				  #!void)



;---- log-map methods
;----- (initialize) 
(add-method initialize
				(make-method (list <log-map>)
								 (lambda (initialize-parent self args)
									;;(dnl "<thing> init")
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									(initialise self '(type log-map format ps)) ;; keep all files
									)))



(sclos-method <log-map> (page-preamble self logger format)
				  ;; This *must* replace it's parent from <snapshot> since it doesn't work with a traditional port
				  (kdnl* '(log-* log-map) (name self) "[" (my 'name) ":" (class-name-of self) "]" "in page-preamble")
				  (let ((filename (my 'filename))
						  (filetype (my 'filetype))
						  (file (my 'file))
						  (t (my 'subjective-time))
						  )

					 (cond
					  ((not (or (not filename) (string? filename)))
						(abort (my 'name) " has a filename which is neither false, nor a string.  This is a fatal error."))

					  ((not (or (not filetype) (string? filetype)))
						(abort (my 'name) " has a filetype which is neither false, nor a string.  This is a fatal error."))

					  ((not (number? t))
						(abort (my 'name) " has a subjective time which is not a number. This is a fatal error."))
					  )

					 ;; Open a new file
					 (cond
					  ((not file)
						(kdnl* '(introspection log-map) "[" (my 'name) ":" (class-name-of self) "]" "is preparing to dump")
						
						(let ((fn (introspection-filename (my 'filename) (my 'filetype) t)))
						  (set-my! 'lastfile (my 'currentfile))
						  (set-my! 'currentfile fn)
						  (if (zero? (string-length fn))
								(abort "Oh. Bother.")
								(set! file (make-ps fn '(Helvetica))))
						  ))
					  ((member file (list (current-output-port) (current-error-port)))
						;; do nothing really
						(kdnl* '(introspection log-map) "[" (my 'name) ":" (class-name-of self) "]" "has nothing to do")
						#!void
						)
					  (else 
						(kdnl* '(introspection log-map) "[" (my 'name) ":" (class-name-of self) "]" " Good, we've hit page-preamble with a file that is still open.\nClosing the file (" 
								 (my 'lastfile) ") and opening a new one.")
						(close-output-port file)
						(set-my! 'file #f)
						(let ((fn (introspection-filename (my 'filename) (my 'filetype) t)))
						  (set-my! 'lastfile (my 'currentfile))
						  (set-my! 'currentfile fn)
						  (if (zero? (string-length fn))
								(abort "Oh. Bother.")
								(set! file (make-ps fn '(Helvetica))))
						  )
						)
					  )
					 (set-my! 'file file)))

(sclos-method <log-map> (page-epilogue self logger format)
				  ;; This *must* replace it's parent from <snapshot> since it doesn't work with a traditional port
				  (kdnl* '(log-* log-map) (name self) "[" (my 'name) ":" (class-name-of self) "]" "has page-epilogue")
				  (let ((file (my 'file))
						  (name (my 'currentfile)))
					 (if file
						  (begin
							 (file 'close)
							 (set-my! 'file #f)))

					 ;;					 (if (my 'png)
					 ;;						  (let* ((pfn (substring name 0 (- (string-length name) 2)))
					 ;;									(gfn (string-append pfn "png")))
					 ;;							 (do-map-conversion name gfn)))
					 )
				  )


;; This logs to an open file
(sclos-method (<log-map> <procedure>) (log-data self logger format caller targets)
				  (lambda (target)	
					 (kdnl* '(log-* log-map) (name self) "[" (my 'name) ":" (class-name-of self) "]" "in log-data" (class-name-of target) (slot-ref target 'name))
					 (let* ((name (slot-ref target 'name))
							  (p (slot-ref self 'map-projection))
							  (ps (slot-ref self 'file))
							  )
						(ps 'comment "logging data for " name "*************************")
						(ps 'moveto (list (p '(20 20))))
						(ps 'setgray 0.0)
						(ps 'Helvetica 14)
						(ps 'show (string-append (slot-ref self 'name)))
						(ps 'comment "finished logging data for " name)
						)))


;---- log-data methods
;----- (initialize) 
(add-method initialize
				(make-method (list <log-data>)
								 (lambda (initialize-parent self args)
									;;(dnl "<thing> init")
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									(initialise self '(type log-data)) ;; keep all files
									)))


(sclos-method <log-data> (agent-prep self start end kernel . args)
				  ;; This opens the output file on initialisation.
				  (agent-prep-parent) ;; parents should prep first
				  (kdnl* '(log-* log-data) (name self) "[" (my 'name) ":" (class-name-of self) "]" "in agent-prep")
				  
				  (let ((filename (my 'filename))
						  (filetype (my 'filetype)))
					 (if (string? (my 'filename))
						  (begin
							 (kdnl* '(log-* log-data) (name self) "[" (my 'name) ":" (class-name-of self) "]" "opening " (introspection-filename filename (if filetype filetype "")))
							 (set-my! 'file  (open-output-file (introspection-filename filename (if filetype filetype "")))) (current-output-port))
						  (begin
							 (kdnl* '(log-* log-data) (name self) "[" (my 'name) ":" (class-name-of self) "]" "using stdout as the output file " )
							 (set-my! 'file (current-output-port))
							 )
						  )
					 )
				  (if (null? (my 'variables))
						(let ((vars (reverse (unique* (reverse (append '(name subjective-time) (apply append (map extra-variable-list (my 'introspection-list)))))))))
						  (slot-set! self 'variables vars)))
				  )


(sclos-method <log-data> (agent-shutdown self . args)
				  (kdnl* '(log-* log-data) (name self) "[" (my 'name) ":" (class-name-of self) "]" "in agent-shutdown")
				  (if (and (my 'file) (output-port? (my 'file)) (not (member (my 'file) (list (current-output-port) (current-error-port)))))
						(begin
						  (close-output-port (my 'file))
						  (set-my! 'file #f) ;; leave it the way it should be left
						  ))
				  (agent-shutdown-parent) ;; Parents should shutdown last
				  )

(sclos-method <log-data> (page-preamble self logger format)
				  (page-preamble-parent) ;; opens the file

				  (if (not (output-port? (my 'file)))
						(abort "Serious problems getting an output port for " (my 'name)))

				  (let ((il (my 'introspection-list))
						  (file (my 'file))
						  (show-field-name (my 'show-field-name))
						  (missing-val (my 'missing-val))
						  )
					 (case format
						((ps)
						 #f)
						(else
						 
						 
						 (if (not (member 'header (my 'preamble-state)))
							  (begin
								 (if (and (pair? il) (null? (cdr il))) ;; agent name comes first since it is easy to prune the first line
									  (begin
										 (display (string-append "# " (name (car il))) (my 'file))
										 (newline file)))
							  
								 (let ((header 
										  (if missing-val
												(my 'variables)
												(let loop ((all-variables '())
															  (all-entities il))
												  (if (null? all-entities)
														(intersection (uniq (map string->symbol (sort (map symbol->string all-variables) string<?))) (my 'variables))
														(loop (append
																 (map car (class-slots (class-of (car all-entities)))) 
																 (extra-variable-list (car all-entities))
																 all-variables) (cdr all-entities))))
												)
										  ))
									(display "# " file)
									(for-each (lambda (x) (display " " file) (display x file)) header)
									(newline file))
								 (set-my! 'preamble-state (cons 'header (my 'preamble-state)))
								 )
							  )
						 )
						)
					 )
				  )
						

;; This is typically never called since it "logs" the logfile.  Mostly here as an example.
(sclos-method (<log-data> <procedure> <symbol> <procedure>) (log-data self logger format caller targets . file)
				  (kdnl* '(log-* log-data) (name self) "[" (my 'name) ":" (class-name-of self) "]" "in log-data")
				  (let ((file (my 'file))
						  (show-field-name (my 'show-field-name))
						  (subjects (my 'introspection-list))
						  (targets (my 'variables))
						  )
					 (for-each (lambda (target) 
									 (display "**" file)
									 (for-each ;; field in the variable list
									  (lambda (field)
										 (if show-field-name
											  (begin
												 (display " " file)
												 (display field file)))

										 (cond
										  ((member field (map car (class-slots (class-of target))))
											(kdnl* '(log-* log-data logging-debug) "     Dumping " field "=" (if (has-slot? self t) (slot-ref self t) "missing!"))
												 
											(display " " file)
											(display (slot-ref target field) file)
											)
										  ((member field (extra-variable-list target))
											(display " " file)
											(display (extra-variable target field) file)
											)
										  (missing-val
											(display " " file)
											(display missing-val file)))
										 )
									  (if #t targets (filter (not-member (my 'dont-log)) targets)))
									 (newline file)
									 )
								  subjects)
					 )
				  )


(sclos-method (<log-data>) (page-epilogue self logger format)
				  (kdnl* '(log-* log-data) (name self) "[" (my 'name) ":" (class-name-of self) "]" "in page-epilogue")
				  (if (and (pair? (my 'introspection-list)) (pair? (cdr (my 'introspection-list))))
						(or #t (newline (my 'file))) ;; We don't want a blank line between each record! -- change #t to #f to get lines between "pages"
						)
				  )



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
;	classes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.19
;		Location: odin:/home/gray/study/src/new/classes.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2012 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 



;-  Code 




;-- Animals of all sorts....



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
;	landscape-classes.scm -- Written by Randall Gray 

;-  Code 

;-- Environmental things

(define <ecoservice> (make-class (list <agent>) '(patch value capacity delta-T-max r history)))  

(register-class <ecoservice>)
;; The update function is of the form (lambda (self t dt) ...)
;; where the args are specific things passed (like location, rainfall)
;; and any specific "parameters" ought to be part of the function's closure.


(define <patch> (make-class (list <environment>) '(location radius service-list)))
;; a patch is a location and radius with a list of ecological services
(register-class <patch>)

(define <dynamic-patch> (make-class (list <patch>) '(population-names population-symbols population-definitions dP d/dt-list subdivisions)))  ;; the definitions are kept for debugging 
;; 

;; definitions look like ((name1 dn1/dt) ... (nameN dnN/dt))
;; or (eventually) like ((name1 prey-lst pred-lst helped-by-lst competitor-lst) ...)
;; but NOT a mixture of them.

;; Probably need to use some sort of Bayesian probability for populating "random" patches, lest we 
;; get unlikely species mixes (like camels and penguins)

;; Sticking to the service-update-map is critical for getting the right answers, y'know.

;;*** NOTE: update equations are lambdas which take the set of values associated with the services
;;*** present in the patch.  The nominated services are listed in the service-indices list either 
;;*** categorically  (the types) and strings (the names).  The types are the aggregate values of the 
;;*** names -- if you want to deal with a named entity separately, the update equation must explicitly 
;;*** remove it. They will be of the form (lambda (t ...) ...)  and *all* of the indicated services 
;;*** must be there or Bad Things Happen.



;; a patch is a location and radius with a list of ecological services

;; species-index is an association list which pairs ecoservice names to indices in the 
(register-class <dynamic-patch>)

(define <landscape> (make-class (list <environment>) '(terrain-function)))
;; terrain-function is a function in x and y that returns a DEM
(register-class <landscape>)

(define <habitat> (make-class (list <landscape>) '(patch-list dump-times scale)))
;; sites is a list of patches
(register-class <habitat>)

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
;	animal-classes.scm -- Written by Randall Gray 

;-  Code 

(define <simple-metabolism> (make-class (list <object>) '(days-of-hunger hunger-limit)))
(register-class <simple-metabolism>)

(define <metabolism> (make-class (list <object>) '(stomach-contents 
																	mass
																	structural-prop             ;; structural-mass increases as mass increases -- all things that have metabolism must have mass
																	structural-mass             ;; kg
																	max-consumption-rate        ;; the maximum amount it could eat in a given period, assuming an infinite stomach
																	metabolic-rate              ;; the amount of "stuff" or "stuff-equivalent" needed per kg per unit of time
																	starvation-level            ;; the animal dies if mass/structural-mass goes below this value 
																	gut-size                    ;; stomach capacity as a proportion of structural-mass
																	condition                   ;; "fat" reserves
																	food->condition-conversion-rate   ;; exchange rate between stomach-contents and condition (* condition condition-conversion-rate) ~ stomach-contents
																	condition->food-conversion-rate   ;; exchange rate between stomach-contents and condition (* condition condition-conversion-rate) ~ stomach-contents
																	food->mass-conversion-rate        ;; exchange rate between stomach-contents and body mass (* delta-mass mass-conversion-rate) ~ stomach-contents
																	mass->food-conversion-rate        ;; exchange rate between stomach-contents and body mass (* delta-mass mass-conversion-rate) ~ stomach-contents
																	condition-conversion-rate   ;; exchange rate between stomach-contents and condition (* condition condition-conversion-rate) ~ stomach-contents
																	mass-conversion-rate        ;; exchange rate between stomach-contents and body mass (* delta-mass mass-conversion-rate) ~ stomach-contents
																	max-growth-rate             ;; the most it will grow in a time period (uses mass-conversion-rate
																	max-condition-rate          ;; the most it will increase its condition in a time period (uses condition-conversion-rate -- defaults to mgr
																	)
											))
(register-class <metabolism>)

;; structural-mass is pegged at (max (* (my 'mass) (my 'structural-prop))) through time
;; structural-prop is multiplied by (my 'mass) to give a number less than or equal to the structural mas
;; metabolic-rate: (* metabolic-rate (my 'mass) dt) is the mass required to maintain body mass for dt
;; ... consumption above that rate is converted to "condition"
;; == _ 
;;   |  base-rate is the amount of mass removed per dt for stomach contents
;;   |_ condition-rate is the amount of mass removed per dt for conditon contents
;;
;; starvation-rate is the amount of body mass removed per dt
;; starvation-level is the value of (/ (my 'mass) (my 'structural-mass)) at which an organism dies.  This  really ought to be a number like 1.3
;; stomach-contents is an absolute amount of food
;; gut-size is a scalar multiplier of the structural-mass which indicates the max cap. of the stomach
;; condition is an absolute number which is equivalent to mass in the stomach
;; food->condition-conversion-rate is the efficiency of conversion from stomach food to condition food
;; condition->food-conversion-rate is the efficiency of conversion from condition to food
;; food->mass-conversion-rate is the efficiency of conversion from stomach food to mass food
;; mass->food-conversion-rate is the efficiency of conversion from mass to food
;; max-consumption-rate is the rate at which things can move through the system

(define <simple-animal> (make-class (list <simple-metabolism> <thing>) '(age sex habitat searchradius 
																									  foodlist homelist breedlist
																									  domain-attraction
																									  food-attraction
																									  ))
  ) ;; lists of attributes it looks for for eating, denning and breeding
(register-class <simple-animal>)


;; current-interest is a function which takes (self age t dt ...) and returns a meaningful symbol
;; 

(define <animal> (make-class (list <metabolism> <thing>) ;; lists of attributes it looks for for eating, denning and breeding
									  '(current-interest age sex
																habitat searchradius foodlist homelist breedlist
																movementspeed
																searchspeed 
																foragespeed
																wanderspeed
																objective
																domain-attraction
																food-attraction
																near-food-attraction
																)
				  
				  ))
(register-class <animal>)

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
;	wally.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.16
;		Location: odin:/home/gray/study/src/new/wally.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2012 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

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
;-  Identification and Changes

;--
;	animal.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.19
;		Location: odin:/home/gray/study/src/new/animal.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2012 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data



;-  Variables/constants both public and static

(define AMINGREY 0.2)
(define AMAXGREY 0.05)
(define ACIRCGREY 0.0)

;(require 'charplot)

;-  Code 

;---- <metabolism> methods

;;(define food-attenuation 0.25)
(define food-attenuation 0.5)
;;(define food-attenuation (/ 2.0 (exp 1.0))) ;; ~1.3


;----- initialize

(sclos-method <simple-metabolism> (initialize self args)
				  (initialise self (list 'hunger-limit 20.0 'days-of-hunger 0.0))
				  (initialize-parent) ;; call "parents" last to make the initialisation list work
				  )



(sclos-model-body <metabolism> ;; A thing with metabolism *must* have mass
						(kdnl* 'model-bodies "In " (class-name-of self))
						(parent-body)

						(if (not (number? (my 'mass)))
							 (kdnl* 'stomach (my 'name) "is dead, long live the King"))

						(if (number? (my 'mass))
							 (let* ((struct-mass (my 'structural-mass))
									  (struct-prop (my 'structural-prop))
									  (starve-level (my 'starvation-level))
									  (mass (my 'mass))
									  (mass->food-conv-rate (my 'mass->food-conversion-rate))
									  (food->mass-conv-rate (my 'food->mass-conversion-rate))

									  (condn (my 'condition))

									  (cond->food-conv-rate (my 'condition->food-conversion-rate))
									  (food->cond-conv-rate (my 'food->condition-conversion-rate))

									  (guts (* struct-mass (my 'gut-size)))
									  (stomach-cont (my 'stomach-contents))
									  (stuffed-to-the-gills (and (number? mass) (* mass (my 'max-consumption-rate) dt)))
									  (max-dinner (and stuffed-to-the-gills (min stuffed-to-the-gills stomach-cont)))

									  (mgr (my 'max-growth-rate))
									  (cgr (my 'max-condition-rate))
									  (met-rate (my 'metabolic-rate))
									  (dead #f)

									  (cost (and mass (* met-rate mass dt)))
									  )

								(if (not (number? cgr)) (set! cgr mgr)) ;; condition can be put on at the same rate as mass unless specified
								
								;; do calculations here....

								;; mass and cost are set to false when the organism is dead
								(kdnl* 'stomach "Metabolism: cost =" cost "stomach-contents ="  stomach-cont)

								(if cost 
									 (begin ;; deal with metabolic costs

										(kdnl* 'metabolism "D0   stomach =" stomach-cont "| condition =" condn "| mass =" mass "|=| cost = " cost)

										;; .... with stomach-contents
										(let ((dc (- stomach-cont cost)))
										  (set! stomach-cont (max dc 0.0))
										  (set! cost (max (- dc) 0.0))
										  )

										(kdnl* 'metabolism "D1   stomach =" stomach-cont "| condition =" condn "| mass =" mass "|=| cost = " cost)

										;; .... with condition
										(let ((cc (* cond->food-conv-rate condn)))
										  (let ((dc (- cc cost)))
											 (set! cc (max dc 0.0))
											 (set! cost (max (- dc) 0.0))
											 
											 (set! condn (max 0.0 (/ cc cond->food-conv-rate)))))
										
										(kdnl* 'metabolism "D2   stomach =" stomach-cont "| condition =" condn "| mass =" mass "|=| cost = " cost)

										;; .... with body mass
										(let ((cc (* mass->food-conv-rate mass)))
										  (let ((dc (- cc cost)))
											 (set! cc (max dc 0.0))
											 (set! cost (max (- dc) 0.0))
											 
											 (set! mass (max 0.0 (/ cc mass->food-conv-rate)))))
										
										
										(kdnl* 'metabolism "D3   stomach =" stomach-cont "| condition =" condn "| mass =" mass "|=| cost = " cost)

										(if (or (<= mass struct-mass)
												  (<= (/ mass struct-mass) starve-level))
											 (begin
												(set! mass #f)
												(set-my! 'mass #f)) ;; Dead, y'know
											 )
										)
									 )

								(if (and mass (> stomach-cont 0.0))
									 (begin ;; Growth and condition updates

										(kdnl* 'metabolism "G0   stomach =" stomach-cont "| condition =" condn "| mass =" mass "|=| cost = " cost)

										;; growth first
										(let* ((mm (/ stomach-cont food->mass-conv-rate))
												 (mg (* mgr dt))
												 (delta-m (min mm mg))
												 )
										  (kdnl* 'metabolism "g0a          mm =" mm" | mg =" mg " | delta-m =" delta-m)
										  
										  (set! mass (max 0.0 (+ mass delta-m)))
										  (set! stomach-cont (max 0.0 (- stomach-cont (* delta-m food->mass-conv-rate))))
										  (kdnl* 'metabolism "g0b          mm =" mm" | mg =" mg " | delta-m =" delta-m)
										  )

										(kdnl* 'metabolism "G1   stomach =" stomach-cont "| condition =" condn "| mass =" mass "|=| cost = " cost)

										;; now condition
										(let* ((mm (/ stomach-cont food->cond-conv-rate))
												 (mg (* cgr dt))
												 (delta-m (min mm mg))
												 )
										  
										  (kdnl* 'metabolism "g1a          mm =" mm" | mg =" mg " | delta-m =" delta-m)
										  (set! condn (max 0.0 (+ condn delta-m)))
										  (set! stomach-cont (max 0.0 (- stomach-cont (* delta-m food->cond-conv-rate))))
										  (kdnl* 'metabolism "g1b          mm =" mm" | mg =" mg " | delta-m =" delta-m)
										  )

										(kdnl* 'metabolism "G2   stomach =" stomach-cont "| condition =" condn "| mass =" mass "|=| cost = " cost)

										))
								
								(kdnl* 'metabolism "X    stomach =" stomach-cont "| condition =" condn "| mass =" mass "|=| cost = " cost)

								(set-my! 'condition condn)
								(set-my! 'stomach-contents stomach-cont)
								(set-my! 'mass mass)
								(set-my! 'structural-mass struct-mass)
								dt
								)
							 'remove)
							 )







;(add-method initialize
;				(make-method (list <metabolism>)
;								 (lambda (initialise-parent self args)
;									;;(dnl "<metabolism> init")
;									(initialise self '(condition 0.0 metabolic-rate 0.0))
;									(initialise-parent) ;; call "parents" last to make the initialisation list work
;									)))


;(sclos-getter <metabolism> structural-mass)  ;; This would be a call something like "(structural-mass self)"
;(sclos-setter <metabolism> structural-mass)  ;; This would be a call something like "(set-structural-mass! self val)"
;(sclos-getter <metabolism> stomach-contents)
;(sclos-setter <metabolism> stomach-contents)
;(sclos-getter <metabolism> gut-size)
;(sclos-setter <metabolism> gut-size)
;(sclos-getter <metabolism> condition)
;(sclos-setter <metabolism> condition)

#|
(sclos-getter <metabolism> structural-prop)
(sclos-setter <metabolism> structural-prop)
(sclos-getter <metabolism> metabolic-rate)
(sclos-setter <metabolism> metabolic-rate)
(sclos-getter <metabolism> condition-rate)
(sclos-setter <metabolism> condition-rate)
(sclos-getter <metabolism> starvation-rate)
(sclos-setter <metabolism> starvation-rate)
(sclos-getter <metabolism> condition-conversion-rate)
(sclos-setter <metabolism> condition-conversion-rate)
|#


(sclos-method (<metabolism> <number> <number>) (eat self available-food dt)
				  (if mass
						(let* ((struct-mass (my 'structural-mass))
								 (mass (my 'mass))
								 (guts (* struct-mass (my 'gut-size)))
								 (stomach-cont (my 'stomach-contents))
								 (stuffed-to-the-gills (* mass (my 'max-consumption-rate) dt))
								 (gap (- guts stomach-cont))
								 (max-dinner (min gap stuffed-to-the-gills available-food))
								 (ate 0.0)
								 )
						  ;; do calculations here....
						  
						  (begin ; eating
							 (set! ate max-dinner)
							 ;;(set! available-food (- available-food ate))
							 (set! stomach-cont (max 0.0 (+ stomach-cont ate)))
							 )

						  ;; return value 
						  (if (< (/ mass struct-mass) (my 'starvation-level))
								#f ;; false means death
								(let ()
								  ;;(if (> (* mass struct-prop) struct-mass) 
								  ;;   (set-my! self 'struct-mass struct-mass)
								  ;;   )
								  (set-my! 'stomach-contents stomach-cont)
								  ate
								  )
								)
						  )
						0)
				  )

;---- animal methods

;----- (initialize) 
(add-method initialize
				(make-method (list <animal>)
								 (lambda (initialise-parent self args)
									;;(dnl "<animal> init")
									(slot-set! self 'current-interest 
												  (lambda args 
													 (aborts "current-interest isn't defined for a <animal>: " 	
																(slot-ref self 'name) ":" (slot-ref self 'type) ":" (slot-ref self 'representation))))
									(initialise self (list 'age #f 'sex #f))
									(initialise-parent) ;; call "parents" last to make the initialisation list work
									)))

;----- (age) 
(add-method age
				(make-method 
				 (list <animal>)
				 (lambda (call-parent-method self)
					(slot-ref self 'age))))


;----- (set-age!) 
(add-method set-age!
				(make-method 
				 (list <animal> <number>)
				 (lambda (call-parent-method self n)
					(if (not (number? n))
						 (aborts "thing:set-age! -- bad number")
						 (slot-set! self 'age n)))))


;----- (sex) 
(add-method sex
				(make-method 
				 (list <animal>)
				 (lambda (call-parent-method self)
					(slot-ref self 'sex))))


;----- (set-sex!) 
(add-method set-sex!
				(make-method 
				 (list <animal> <symbol>)
				 (lambda (call-parent-method self n)
					(if (not (member n '(female male)))
						 (aborts "thing:set-sex! -- bad symbol, should be male or female")
						 (slot-set! self 'sex n)))))

;----- (map-log-track-segment
(sclos-method <animal> (map-log-track-segment self track wt p ps)
				  (if track
						(let* ((xytrack (map txyz->xy track))
								 (ptrack (map p xytrack)))
						  (if (>= (length ptrack) 2)
								(let ((startseg (list-head ptrack (1- (length ptrack))))
										(finishseg (cdr ptrack)))
								  (if adjust-grey (ps 'setgray wt))
								  
								  (for-each
									(lambda (ss fs)
									  (ps 'moveto ss)
									  (ps 'moveto ss)
									  (ps 'lineto fs)
									  (ps 'stroke)
									  )
									startseg finishseg)
								  (ps 'Helvetica 4.5)
								  (ps 'moveto (p (list-head (location self) 2)))
								  ;;(ps 'show-centered (string-append "[" (number->string (my 'mass)) "]"))
								  (let ((m (my 'mass)))
									 (if m
										  (begin
											 (ps 'show-centered (number->string (my 'mass)))
											 (ps-circle ps  (p (min 0.31415 (* 0.25 pi (sqrt (my 'mass))))) (p (list-head (location self) 2)) 1.2 0.0 ))
										  (begin
											 (ps 'show-centered "Dead")
											 (ps-circle ps  (p 0.31415) (p (list-head (location self) 2)) 1.2 0.0 ))))
										  
								  )

								)
						  )
						)
				  #t)


(sclos-method <animal> (map-log-data self logger format caller targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'map-projection)))
					 (if (or (not p) (null? p))  (set! p (lambda (x) x)))

					 (let ((track (my 'track))
							 (tracks (my 'tracked-paths)))

						(if track (map-log-track-segment self track ACIRCGREY p ps))
						(if tracks
							 (let loop ((n (length tracks))
											(k 1.0)
											(tr tracks))
								(if (not (null? tr))
									 (begin
										(map-log-track-segment self (car tr) (+ AMINGREY (* (/ k n) (- AMAXGREY AMINGREY))) p ps)
										(loop n (1+ k) (cdr tr))))))
						)
					 #t)
				  )
				  
(sclos-method <animal> (log-data self logger format caller targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'map-projection)))
					 (if (or (not p) (null? p))  (set! p (lambda (x) x)))

					 (kdnl* '(log-* log-animal) ":" targets)
					 (case format
						((ps)
						 (map-log-data self logger format caller targets p ps)
						 )
						(else 
						 (log-data-parent))
						)

					 (if (and (assoc 'track-segments (my 'state-flags)) (state-flag self 'track-segments))
						  (new-track! self))
					 
					 )
				  )


(sclos-method (<animal> <number> <pair> <number> <number>) (wander-around self dt point attr speed) ;;
  (let* ((loc (location self))
			(p (unit (vector-to loc point)))
			(v (slot-ref self 'direction))
			(theta (- (nrandom pi 10) pi))
			(spd speed)
		  )
	 
	 ;; This calculation ought to use random-angle and rotated-velocity (which applies to lists!)
	 ;; I'll keep it this way for local clarity.
	 (let* ((new-v (unit (append (list (- (* (car v) (cos theta)) (* (cadr v) (sin theta)))
												  (+ (* (cadr v) (cos theta)) (* (car v) (sin theta))))
										  (cddr v))))

			  (new-loc (map + loc 
								 (map *  
										(map + (map * (make-list (length v) (- 1.0 attr)) new-v)
											  (map * (make-list (length p) attr) p))
										(append (make-list 2 (* spd dt))  (make-list (length (cddr v)) 1.0)))))
			  )
		(slot-set! self 'location new-loc)
		(slot-set! self 'direction new-v)
		new-loc)))
	 

(sclos-method (<animal> <number> <pair> <number> <symbol>) (wander-around self dt point attr speedtag) ;;
				  (wander-around self dt point attr (my speedtag)))

(sclos-model-body <animal>
						(kdnl* 'model-bodies "In " (class-name-of self) t)
						(let ((dt/2 (/ dt 2.0))
								(SQRT (lambda (x) (if (>= x 0) (sqrt x) 
															 (if #t
																  (begin 
																	 (dnl "SQRT got a value of " x)
																	 0)
																  (abort "I see ... a rhinoceros ...")
															 )) ) )
								;;(kdnl* dnl*)
								;;(kdnl* dnl)
								)
						  (kdnl* 'animal-running "[" (my 'name) ":" (class-name-of self) "]" " at " t "+" dt)
						  (kdnl* "******" 'running (my 'name) " the " (my 'representation) " is running" "******")
						  ;;(dnl "Vombatus vombatus " t ":" (my 'subjective-time) "+" dt " " (my 'age) " " (my 'location))
						  (set-my! 'age (+ (my 'age) dt/2))
						  (set-my! 'subjective-time (+ (my 'subjective-time) dt/2))

						  (parent-body) ;; should execute body code for <metabolism> and <thing>

						  (let* ((foodlist (my 'foodlist))
									(homelist (my 'homelist))
									(breedlist (my 'breedlist))
									(H (my 'habitat))
									(here (my 'location))
									(food 0.0) ;; nothing unless we find some
									(struct-mass (my 'structural-mass))
									(mass (my 'mass))
									(starve-level (my 'starvation-level))
									(stomach-cont (my 'stomach-contents))
									(guts (* struct-mass (my 'gut-size)))
									(condition (my 'condition))
									(ate 0.0)
									(objective (let ((o (my 'objective))) (if (null? o) #f o)))
									)

							 (kdnl* 'stomach "[" (my 'name) ":" (class-name-of self) "]" "===> stomach =" stomach-cont "| condition =" condition "| mass =" mass)
 
							 (if (number? mass) 
								  (begin
									 (let ((focus ((my 'current-interest) self (my 'age) t dt condition stomach-cont guts))
											 )
										(kdnl* 'focus "[" (my 'name) ":" (class-name-of self) "]" " in now focussed on " focus)

										(cond
										 ((not focus)
										  (wander-around self dt (map (lambda (x) (/ x 2.0)) domain) (* 2.0 (my 'domain-attraction)) 'wanderspeed)
										  #t)
										 ;;(aborts "Dinna y' ken?"))
										 ((eq? focus 'wander)
										  (wander-around self dt (map (lambda (x) (/ x 2.0)) domain) (my 'domain-attraction) 'wanderspeed)
										  )

										 ((eq? focus 'hungry)
										  (kdnl* 'debugging-eating "[" (my 'name) ":" (class-name-of self) "]" "hungry")

										  (let* (
													(foodsites (patch-list H (lambda (x) (let ((s (services x foodlist)))
																										(and s (not (null? s)))))))
													(foodvalue (map 
																	(lambda (x) 
																	  (if (= food-attenuation 0.5)
																			(/ (SQRT (value x foodlist)) (+ (spatial-scale H) (distance here (location x))))
																			(/ (pow (value x foodlist) food-attenuation) (+ (spatial-scale H) (distance here (location x)))))
																	  ) foodsites))

													(fooddata (sort (map cons foodvalue foodsites) (lambda (x y) (> (car x) (car y)))))
													)
											 (kdnl* "Food ranks" "[" (my 'name) ":" (class-name-of self) "]" (map (lambda (x) (cons (car x) (distance here (location (cdr x)))))  fooddata))
											 ;;(plot (map (lambda (x) (list (car x) (cdr x))) '((.030477192023144497 . 96.5099300051329) (.02838833958702164 . 116.00897402071166) (.025996860113632107 . 118.06461386706091) (.018331938492922764 . 166.74242294209597) (.0176501058990574 . 189.26489278564608) (.010190290595720903 . 125.28278510462805))) "appeal"  "distance")

											 (if (zero? (length fooddata))
												  (begin ;; No food possible ... 
													 (kdnl* 'debugging-eating "[" (my 'name) ":" (class-name-of self) "]" "Hunting")
													 (wander-around self dt (map (lambda (x) (/ x 2.0)) domain) (my 'domain-attraction) 'foragespeed)
													 )
												  (let ((target (cdar fooddata)))
													 (if (contains? target (location self))
														  (begin
															 (kdnl* 'debugging-eating "[" (my 'name) ":" (class-name-of self) "]" "Reading the menu")
															 (let* ((TV (value target foodlist))
																	  (ate #f))
																(kdnl* 'debugging-eating "[" (my 'name) ":" (class-name-of self) "]" "ordering the lot (" (value target foodlist) "/" (value target (services target)) ")and eating it")
																(let* ((total-food (value target foodlist))
																		 (prop (/ total-food (capacity target foodlist)))
																		 (available-food (* 2.0 total-food (/ (SQRT prop) (1+ prop)))))
																  (set! ate (eat self available-food dt))
																  )
																(kdnl* 'debugging-eating "[" (my 'name) ":" (class-name-of self) "]" "removing " ate " from the patch ...")
																(scale! target foodlist (- 1.0 (/ ate TV)))
																(kdnl* 'debugging-eating "[" (my 'name) ":" (class-name-of self) "]" "done.")
																)
															 (wander-around self dt (location target) (my 'near-food-attraction) 'foragespeed)
															 ;;(wander-around self dt (location target) 0.75) ;; force a reasonable bias
															 )
														  (begin
															 (kdnl* 'debugging-eating "[" (my 'name) ":" (class-name-of self) "]" "move toward the food source")
															 (wander-around self dt (location target) (my 'food-attraction) 'movementspeed)
															 ;;(wander-around self dt (location target) 0.85) ;; force a fairly strong bias
															 )))
												  )
											 )
										  )
										 ((eq? focus 'flee)
										  ;; Not implemented yet
										  #f
										  )
										 ((eq? focus 'breed)
										  ;; Not implemented yet
										  #f
										  )
										 )
										)

									 (set-my! 'objective (if (null? objective) #f objective))
									 (set-my! 'age (+ (my 'age) dt/2))
									 (set-my! 'subjective-time (+ (my 'subjective-time) dt/2))
									 
									 ;;(track-locus self t (my 'location)) ;; even if they aren't moving :: automatically done in <thing>
									 )
								  )
							 
							 ;; Ok now test condition and return
							 (if (or (not mass) (< (my 'mass) (* 1.3 (my 'structural-mass))))
								  (list 'remove self)
								  dt)
							 )
						  )
						)






;------ <animal> methods

;(sclos-method (<animal> <habitat>) (forage self habitat)
;				  (let* ((R (my 'searchradius))
;							(P (my 'location))
;							(candidates (filter (lambda (x) (< (car x) R))
;													  (sorted-ecoservices (my 'foodlist) habitat (my 'location))))
;							(DC (map (lambda (x) (let ((p (location x)))	(cons (distance P p) x))) candidates))
;							(scandidate (sort DC (lambda (x y) (<  (/ (value x)
;																					(< (car x) (car y)))))
;													))
;							(ccandidate (filter contains (map cdr scandidate)))
;							)
;
;					 ;; forage or move here
;
;					 
;
;
;					 #t) 
;				  )



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
;	landscape.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.19
;		Location: odin:/home/gray/study/src/new/landscape.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2012 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 


;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 



;-  Variables/constants both public and static

(define PATCHGREY 0.2)
(define HABITATGREY 0.1)

;-  Support function for patches

(define (make-population-structure predation-matrix efficiency-matrix service-data-list ecoservice-template #!optional non-sigmoidal-growth)
  (let* ((predation-matrix predation-matrix)
			(PM predation-matrix)
			(EM (efficiency-matrix 'transpose)) ;; orient it for easy predator use
			(ecoservice-template (deep-copy ecoservice-template))
			(service-data-list (deep-copy service-data-list))
			(service-id-list (map (lambda (x) (list-head x 3)) service-data-list))
			(service-name-list (map car service-id-list))
			(service-symbol-list (map string->symbol service-name-list))
			(service-type-list (map cadr service-id-list))
			(service-eqn-sym-list (map caddr service-id-list))

;; This is analogous to the "(service-list-index self sym)" call in <dynamic-patch>, but it relies on globals
			(service-index (lambda (sym) 
								  (let ((m (member sym service-eqn-sym-list))
										  (n (member sym service-name-list)))
									 
									 (if m (- (length service-eqn-sym-list) (length m))
										  (if n (- (length service-eqn-sym-list) (length n))
												#f)))))

			(pd (lambda (species)
					(let* ((species-list service-eqn-sym-list)
							 (as-prey-ratio (list-sym-ref ((PM 'transpose)) service-eqn-sym-list species))
							 (as-predator-ratio (list-sym-ref (PM) service-eqn-sym-list species))
							 (predator-efficiency (list-sym-ref (EM) service-eqn-sym-list species))
							 (gr (list-ref (list-ref service-data-list (service-index species)) 3))
							 (mort (list-ref (list-ref service-data-list (service-index species)) 4))
							 )

					  (let ((dP/dt	
								(if non-sigmoidal-growth
									 (lambda (t . args)
										(growth-terms--  
										 gr ;; this would be an exponential growth term if we weren't using the sigmoidal stuff
										 (apply + (map * as-predator-ratio predator-efficiency args))
										 (mortality-terms-- mort 
																  (apply + (map * as-prey-ratio args)))))
									 
									 (lambda (t . args)
										(growth-terms--  ;; Explicitly excludes a constant growth term....
										 (apply + (map * as-predator-ratio predator-efficiency args)) ;; this is growth associated with predation
										 (mortality-terms-- mort
																  (apply + (map * as-prey-ratio args))))) ;; decline associated with predation
									 
									 ) ))
						 dP/dt))))

			(d/dt (map (lambda (species) (pd species)) (map caddr service-data-list))) 

			)
	 (let ((population-structure
			  (lambda args
				 (cond
				  ((null? args)
					(abort "Nothing  passed as an argument to a population structure"))
				  ((eq? (car args) 'template) 
					ecoservice-template)
				  ((eq? (car args) 'predation-matrix) 
					predation-matrix)
				  ((eq? (car args) 'efficiency-matrix) 
					(EM 'transpose)) ;; send it back in the same form we got it
				  ((member (car args) '(service-data species-data))
					service-data-list)
				  ((member (car args) '(service-ids species-ids))
					service-id-list)
				  ((member (car args) '(service-names species-names))
					service-name-list)
				  ((member (car args) '(service-symbols species-symbols))
					service-symbol-list)
				  ((member (car args) '(service-types species-types))
					service-type-list)
				  ((member (car args) '(service-eqn-syms species-eqn-syms))
					service-eqn-sym-list)
				  ((eq? (car args) 'd/dt-list)
					d/dt)
				  ((eq? (car args) 'index)
					(if (null? (cdr args))
						 #f
						 (if (pair? cddr)
							  (map service-index (cdr args))
							  (service-index (cadr args)))))
				  ))))
		population-structure)
	 ))







;-- Methods and bodies

;; Stops things going off the rails 
;--- (services...) returns services matching the sym or in the symlist
(sclos-method (<environment>) (services self syms)
				  '())

(sclos-model-body <environment>
						(parent-body)
						dt)

;--- <ecoservice> methods
;; By convention we give ecoservices names which are strings, types which are symbols ... neither needs to be unique
;; 
;; value, set-value!, add! scale!


(comment "
Ecoservices are able to update their state themselves.  This may have irregular interactions with 
any dynamics being forced on them from a dynamic-patch since the order of insertion in the queue 
is not prescribed. The best way of dealing with this is to ensure that the timestep associated with 
ecoservices is half (or less) of the timestep of the patch.
")
  
(add-method initialize
				(make-method (list <ecoservice>)
								 (lambda (initialize-parent self args)
									(initialise self (list 'history #f)) ;; Set history to '() to record the history
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))


(sclos-method <ecoservice> (dump self . count)
				  (set! count (if (null? count) 0 (car count)))
				  (display (make-string count #\space))
				  (display "<ecoservice>\n")

				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (display (make-string (+ 2 count) #\space))
									 (display x)
									 (display ": ")
									 (display y)
									 (newline))
								  slots vals)))


;---- query & set
(sclos-method (<ecoservice> <symbol>) (service? self sym)
				  (or (eq? (my 'type) sym) (eq? (my 'name) sym)))

(sclos-method (<ecoservice> <pair>) (service? self symlist)
				  (or (member (my 'type) symlist) (member (my 'name) symlist)))

(sclos-method <ecoservice> (value self)
				  (my 'value))

(sclos-method <ecoservice> (capacity self)
				  (my 'capacity))

(sclos-method (<ecoservice>) (set-value! self val)
				  (dnl* "Setting" (name self) "from" (my 'value) "to" val)
				  (set-my! 'value val))

;---- adjustment
(sclos-method (<ecoservice>) (add! self val)
				  (let ((v (my 'value)))
					 (if (number? v)
						  (set-my! 'value (+ v val) )
						  (aborts "ecoservice:add!: value is not a number")
						  )))

(sclos-method (<ecoservice>) (scale! self val)
				  (let ((v (my 'value)))
					 (if (number? v)
						  (set-my! 'value (* v val) )
						  (aborts "ecoservice:scale!: value is not a number")
						  )))


;;--- ecoservice model-body support routines (mainly about growth)

(define (test-population the-pop-model #!optional the-growth-model)
  (let ((t-domain 1000)
		  (Po 0.0)
		  (K 20000)
		  (lmb 1)
		  )

	 (let ((x (seq (1+ t-domain)))
			 (values '())
			 (last-t 0)
			 )
		(for-each (lambda (t)
						(set! values (cons (the-pop-model t t-domain Po K lmb) values))
						(set! last-t t)
						)
					 x)
		(map cons x (reverse values))
		))
  )
  
(define (test-growth population grower)
  (let ((t-domain 1000)
		  (dt 50)
		  (Po 0.0)
		  (K 20000)
		  (lmb 1)
		  )

a	 (let ((x (seq (1+ t-domain)))
			 )

		(let loop ((t 0)
					  (P Po)
					  (result '()))
		  (if (> t t-domain)
				(reverse result)
				(let ((p (grower dt t-domain Po P K lmb)))
				  (loop (+ t dt)
						  (+ p P)
						  (cons (cons t p) result))
				  ) ) )))
  )

(define (exponential-population t domain P_0 K lmb)
  ;;(set! lmb (* 3 pi lmb)) ;; This makes it reasonably compatible with the sigmoid
  ;;(set! lmb (* 2 pi lmb)) ;; This makes it reasonably compatible with the sigmoid
  ;;(set! lmb (* 3/2 pi lmb)) ;; This makes it reasonably compatible with the sigmoid (with lmb 0.5)
  ;(set! lmb (* e pi lmb)) ;; This makes it reasonably compatible with the sigmoid (a)
  ;;(set! lmb (* 1/2 pi lmb)) ;; This makes it reasonably compatible with the sigmoid

  ;;(+ P_0 (/ (- K P_0) (1+ (exp (* (- lmb) t))))) ;; formulation a
  (if (> t domain)
		K
		(+ P_0 (* (- K P_0) (- 1 (exp (* (- (/ lmb domain)) t)))))  ;; formulation b
		)
  )

(define (sigmoidal-population t domain P_0 K lmb)
  (if (> t domain)
		K
		(* K (sigmoid* (/ t domain) lmb))))

(define (inverse-exp-growth dt domain P_0 P K lmb)
  ;(set! lmb (* e pi lmb)) ;; This makes it reasonably compatible with the sigmoid (a)
  (if (< P K)
		(/ (* (/ (- domain) lmb) (log (- 1.0 (/ (- P P_0) (- K P_0))))) domain)
		1.0))


(define (exponential-growth dt domain P_0 P K lmb) 
;; This *does not* diddle lmb ... any didd'es need to be in inverse-exp-growth and exponential-population
  (if (>= P K)
		0
		(let* ((t (* domain (inverse-exp-growth dt domain P_0 P K lmb)))
				 (NP (exponential-population (+  dt t) domain P_0 K lmb))
				 )
		  (-  NP P))))


(define (sigmoidal-growth dt domain P_0 P K lmb)
  (if (>= P K)
		0
		(- (sigmoidal-population (* domain (+ (/ dt domain) (inverse-sigmoid* (/ P K) lmb))) domain P_0 K lmb) P)))


(sclos-model-body <ecoservice>
						(kdnl* "[" (my 'name) ":" (class-name-of self) "]" 'model-bodies "In " (class-name-of self) t)
						(let ((h (slot-ref self 'history)))
						  (if h
								(slot-set! self 'history (cons (cons t (my 'value)) h)))
						  )
								

						(let ((rvalue (my 'r)))
						  (if (> rvalue 0.0)
								(let* ((capacity (my 'capacity))
										 (value  (my 'value))
										 (domain (my 'delta-T-max))
										 (delta (sigmoidal-growth dt domain 0 value capacity 1.0))
										 )
								  ;;(dnl* (name (my 'patch)) "/" (my 'name) "value =" value "| delta =" delta)
								  
								  (warning "We've just hacked in a sigmoidal growth function with no option for exponential growth or anything else yet")
								  
								  (set-my! 'value  (+ value delta))
								  )
								))

						(parent-body)
						dt
						)


;;;;--- ecoservice model-body
;;(sclos-model-body <ecoservice>
;;						(let* ((my-capacity (my 'capacity))
;;								 (ipt (/ (my 'value) my-capacity)) ;; This is a logistic update
;;								 (pt (inverse-sigmoid ipt))
;;								 (rdt (/ dt (my 'delta-T-max)))
;;								 (ov (my 'value))
;;								 )
;;						  (set-my! 'value (* my-capacity (sigmoid (+ pt rdt))))
;;						  (kdnl* 'landscape-values "[" (my 'name) ":" (class-name-of self) "]" "<ecoservice>: " == " (my 'value) "/" my-capacity))
;;						(parent-body)
;;						dt)


(sclos-method <ecoservice> (radius self)
				  (radius (my 'patch)))

(sclos-method (<ecoservice> <number>)(set-radius! self r)
				  (set-radius! (my 'patch) r))

(sclos-method <ecoservice> (location self)
				  (location (my 'patch)))

(sclos-method <ecoservice> (log-data self logger format caller targets file . p)
				  (let ((file (slot-ref logger 'file)))
					 (kdnl* '(log-* log-ecoservice) "[" (my 'name) ":" (class-name-of self) "]" "in log-data")
					 (let ((leading-entry #f))
						(for-each
						 (lambda (field)
							(kdnl* '(log-* log-ecoservice) "[" (my 'name) ":" (class-name-of self) "]" "checking" field)
							(if (has-slot? self field)
								 (let ((r (slot-ref self field)))
									(case format
									  ((ps)
										(file 'show (string-append (if (string? r) r (object->string r)) " "))
										)
;								 ((dump)
;								  (with-output-to-port file
;										(lambda ()
;										  (dump self))))

									  ((text table dump)
										(let ((show-field-name (slot-ref logger 'show-field-name))
												(missing-val (slot-ref logger 'missing-val))
												)
										  (if show-field-name
												(begin
												  (if leading-entry 
														(display " " file)
														(set! leading-entry #t))
												  (display field file)))
										  
										  (let ((val (if (eq? field 'name) 
															  (if (slot-ref self 'patch)
																	(string-append (slot-ref (slot-ref self 'patch) 'name)":" (name self))
																	(name self))
															  (if (has-slot? self field)
																	(slot-ref self field)
																	(slot-ref logger 'missing-val)))))
											 (if leading-entry 
												  (display " " file)
												  (set! leading-entry #t))
											 (display val file))
										  )
										)

									  (else
										(kdnl* '(log-* log-ecoservice) "[" (my 'name) ":" (class-name-of self) "]" "Ignoring " field " because I don't have it")
										'ignore-unhandled-format)))
								 (begin
									(kdnl* '(log-* log-ecoservice) "[" (my 'name) ":" (class-name-of self) "]" "no service" field)
									#f)))
						 (unique (if #t targets (filter (not-member (slot-ref logger 'dont-log)) targets)))
						 )
						(newline file)
						)
					 ))



;--- <patch> methods
;; 
;; min-bound, max-bound contains? services 

;(define service? (make-generic))
;(define add-service (make-generic))
;(define remove-service (make-generic))
;(define service-list (make-generic))
;(define service (make-generic))
;(define services (make-generic)) ;; returns value
;(define set-services! (make-generic)) ;; sets value
;(define value (make-generic))      -- defined for environment
;(define set-value! (make-generic)) -- defined for environment


(sclos-method <patch> (dump self . count)
				  (set! count (if (null? count) 0 (car count)))

				  (display (make-string count #\space))
				  (display "<patch>\n")
				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (if (not (eq? x 'service-list))
										  (begin
											 (display (make-string (+ 2 count) #\space))
											 (display x)
											 (display ": ")
											 (display y)
											 (newline))))
								  slots vals))
				  (display (make-string (+ 2 count) #\space))
				  (display 'service-list)
				  (display ":\n")
				  (for-each (lambda (x) (dump x (+ 4 count))) (my 'service-list))
				  )


;---- (initialize...) 
(add-method initialize
				(make-method (list <patch>)
								 (lambda (initialize-parent self args)
									(initialise self '()) 
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))

;---- (min-bound...) & (max-bound...)
(sclos-method <patch> (min-bound self)
				  (let ((loc (my 'location))) 
					 (map - loc (make-list (length loc) (my 'radius)))))

(sclos-method <patch> (max-bound self)
				  (let ((loc (my 'location))) 
					 (map + loc (make-list (length loc) (my 'radius)))))

(sclos-method <patch> (radius self)
				  (my 'radius))

(sclos-method (<ecoservice> <number>)(set-radius! self r)
				  (my-set! 'radius r))

(sclos-method <patch> (location self)
				  (my 'location))



;---- (distance-to-centre...) returns the distance to the centre of the patch
(sclos-method (<patch> <pair>) (distance-to-centre self loc)
				  (let ((sqr (lambda (x) (* x x))))
					 (sqrt (apply + (map sqr (map - (list-head (my 'location) 2) (list-head loc 2)))))))

;---- (distance-to-interior...) returns the distance to the centre of the patch
(sclos-method (<patch> <pair>) (distance-to-interior self loc)
				  (let* ((sqr (lambda (x) (* x x)))
							(R (- (sqrt (apply + (map sqr (map - (list-head (my 'location) 2) (list-head loc 2))))) (my 'radius)))
							)
					 (if (< R 0) 0 R)))

;---- (contains?...) predicate to indicate if something is in the patch
(sclos-method <patch> (contains? self . bit)
				  (if (null? bit) 
						(abort "Missing argument to contains?")
						(set! bit (car bit)))
				  (cond
					((pair? bit)
					 (let ((R (distance-to-interior self bit)))
						(zero? R)))
					((isa? bit <thing>)
					 (let ((R (distance-to-interior self (location bit))))
						(zero? R)))
					(else #f)))

;;(sclos-method (<patch> <thing>) (contains? self entity)
;;				  (contains? (location entity))
;;				  )


;--- (services...) returns services matching the sym or in the symlist

(sclos-method (<patch>) (service-list self . ss)
				  (if (and (pair? ss) (pair? (car ss))) (set! ss (car ss)))
				  (let ((S (my 'service-list)))
					 (if (null? ss)
						  S
						  (filter (lambda (x) (or (member (type x) ss) (member (name x) ss))) S))))


(sclos-method (<patch>) (services self . ss)
				  (if (null? ss)
						(map (lambda (x) (type x)) (my 'service-list))
						(map type (apply service-list (cons self ss)))))


(sclos-method (<patch>) (specific-services self . ss)
				  (if (null? ss)
						(map (lambda (x) (name x)) (my 'service-list))
						(map type (apply service-list (cons self ss)))))


;--- (service?...) queries if a service is present
(sclos-method (<patch> <symbol>) (service? self sym)
				  (not (null? (services self sym))))

(sclos-method (<patch> <pair>) (service? self symlist)
				  (not (null? (services self symlist))))


;--- (set-services!...) sets the value of the services list
(sclos-method (<patch> <pair>) (set-services! self servlist)
				  (set-my! 'service-list servlist))


;--- (add-service...) adds a service to a patch

(sclos-method (<patch> <ecoservice>) (add-service self new-service)
				  (set-services! self (append (my 'service-list) (list new-service))))


;--- (remove-service...) removes all services that match the predicate in a patch
;;                       the predicate will probably be something like (using-name-keep? 'wobble)
(sclos-method (<patch> <procedure>) (remove-service self predicate)
				  (set-services! self (filter predicate (my 'service-list))))


(sclos-method (<patch> <symbol>) (value self servlist)
				  (set! servlist (list servlist))
				  (let ((sl (if (member #t servlist) (my 'service-list) (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map value sl)))))

(sclos-method (<patch> <string>) (value self servlist)
				  (set! servlist (list servlist))
				  (let ((sl (if (member #t servlist) (my 'service-list) (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map value sl)))))

(sclos-method (<patch> <symbol>)(extra-variable self field)
				  (value self (symbol->string field)))

(sclos-method (<patch> <string>)(extra-variable self field)
				  (value self field))

(sclos-method (<patch>) (extra-variable-list self)
				  (map string->symbol (map name (my 'service-list))))

;; (add-method representation
;; 				(make-method (list <agent>)
;; 								 (lambda (representation-parent self)
;; 									(my 'representation))))



(sclos-method (<patch> <pair>) (value self servlist)
				  (let ((sl (if (member #t servlist) (my 'service-list) (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map value sl)))))

(sclos-method (<patch> <pair>) (capacity self servlist)
				  (let ((sl (if (member #t servlist) (my 'service-list) (service-list self servlist))))
					 (if (null? sl)
						  0
						  (apply + (map capacity sl)))))

(sclos-method (<patch> <pair>) (mean-value self servlist)
				  (let ((sl (service-list self servlist)))
					 (if (null? sl)
						  0
						  (/ (apply + (map value sl)) (* 1.0 (length servlist))))))

;;;--- (set-value!...)
(sclos-method (<patch> <symbol>) (set-value! self sym val)
				  (let ((s (filter (lambda (a) (eq? sym (type a))) (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (set-value! x val)) s)
							 #t))))

(sclos-method (<patch> <string>) (set-value! self sym val)
				  (let ((s (filter (lambda (a) (string=? sym  (name a))) (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (set-value! x val)) s)
							 #t))))

;--- (scale!...)
(sclos-method (<patch> <symbol> <number>) (scale! self sym val)
				  (let ((s (filter (lambda (a) (eq? (type a) sym))  (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (scale! x val)) s)
							 #t))))

(sclos-method (<patch> <string> <number>) (scale! self sym val)
				  (let ((s (filter (lambda (a) (string=? (name a) sym))  (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (scale! x val)) s)
							 #t))))

;--- (add!...)
(sclos-method (<patch> <symbol> <number>) (add! self sym val)
				  (let ((s (filter (lambda (a) (eq? (type a) sym)) (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (add! x val)) s)
							 #t))))

(sclos-method (<patch> <string> <number>) (add! self sym val)
				  (let ((s (filter (lambda (a) (string=? (name a) sym)) (my 'service-list))))
					 (if (null? s)
						  #f
						  (begin 
							 (for-each (lambda (x) (add! x val)) s)
							 #t))))

;--- (scale!...)
(sclos-method (<patch> <pair> <number>) (scale! self symlist val)
				  (for-each (lambda (x) (scale! self x val)) symlist))


;--- (add!...)
(sclos-method (<patch> <pair> <number>) (add! self sym val)
				  (for-each (lambda (x) (add! self x val)) symlist))


;--- (total-value ...) ;; needs to filter the services by membership in the indicated symlist


(sclos-method (<patch> <pair>) (total-value self symlist)
				  (let ((ss (service-list self (if (symbol? symlist) (list symlist) symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (value y)) ss)))))

(sclos-method (<patch> <pair>) (total-value self symlist)
				  (let ((ss (service-list self (if (symbol? symlist) (list symlist) symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (value y)) ss)))))

(define (total-patch-list-value patchlist symlist)
  (map (lambda (x) (total-value x symlist)) patchlist))

(sclos-method (<patch> <pair>) (total-capacity self symlist)
				  (let ((ss (service-list self (if (symbol? symlist) (list symlist) symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (capacity y)) ss)))))

(sclos-method (<patch> <pair>) (total-capacity self symlist)
				  (let ((ss (service-list self (if (symbol? symlist) (list symlist) symlist))))
					 (if (or (not ss) (null? ss) )
						  0.0
						  (apply + (map (lambda (y) (capacity y)) ss)))))

(define (total-patch-list-capacity patchlist symlist)
  (map (lambda (x) (total-capacity x symlist)) patchlist))

(sclos-model-body <patch>
						(kdnl* 'model-bodies "In " (class-name-of self) (name self) "@" t)

						;; this does the growth  and endemic mortality
						(if (member 'nested-habitat nested-agents)
							 (for-each (lambda (x)
											 (run x t (+ t dt) (my 'kernel))
											 )	
										  (service-list self)))
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						(parent-body)
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						dt
						)


(sclos-method (<patch> <agent> <symbol> <agent> <list>) (log-data self logger format caller targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'map-projection)))
					 (if (or (not p) (null? p)) (set! p (lambda (x) x)))
					 (kdnl* '(log-* log-patch) "[" (my 'name) ":" (class-name-of self) "]" "in log-data")
				  
					 (case format
						((ps)
						 (let* ((symlist (services h))
								  (name (slot-ref h 'name))
								  )
							
							(if adjust-grey (file 'setgray patchgrey))
							(ps-circle file  (p (radius self)) (p (list-head (location self) 2)) 0.7 0.0)
							
							
							(let* ((slist (slot-ref self 'service-list))
									 (n (1+ (length slist))) ;; slist is becoming circular?? ....******
									 (ns (length slist))
									 (loc (location self))
									 (rad (radius self))
									 (mm-xoffset 2)
									 (mm-yoffset 2)
									 )
							  (file 'moveto (map p (map + 
																 (list-head loc 2) 
																 (list (+ mm-xoffset (* 1.05 rad))
																		 (+ mm-yoffset (/ ns 2.0)))
																 )))
							  (file 'show-table (map (lambda (x) (string-append (slot-ref x 'name) ": " (pno (value x)))) slist))
							  )
							(crop-caption file p self)
							)
						 )

						;;((text table dump)
						;; (log-data-parent)
						;; )
						(else
						 (display (my 'name) file)
						 (for-each 
						  (lambda (x)
							 (display " " file)
							 (display (value x)))
						  (map (lambda (x) (value x)) (my 'service-list))
						  (newline file))
						 ;;(log-data-parent)	
						 )
						)
					 )
				  )



;-- dynamic-patch methods

(add-method initialize
				(make-method (list <dynamic-patch>)
								 (lambda (initialize-parent self args)
									(initialise self (list 'population-names '() 'population-symbols '()
																  'd/dt-list '()
																  'population-definitions '()	
																  'subdivisions 12 ;; because 12 is a nice number?
																  ))
													
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))

(sclos-method (<patch> <procedure> <symbol> <procedure>)  (log-data self logger format caller targets)
				  (display "dynamic patch log")
				  (log-data-parent))
				  

(sclos-method (<dynamic-patch> <string>) (service-list-index self service)
				  (let* ((si (my 'population-names))
							(n (length si))
							(ix (member service si))
							(i (if ix  (- n (length ix)) #f)))
						  i))

(sclos-method (<dynamic-patch> <symbol>) (service-list-index self service)
				  (let* ((si (my 'population-symbols))
							(n (length si))
							(ix (member service si))
							(i (if ix  (- n (length ix)) #f)))
						  i))

(sclos-method (<dynamic-patch> <pair>) (service-list-index self service)
				  (map (lambda (x) (service-list-index self x)) service))


;; for predation matrix stuff ...
(sclos-method (<dynamic-patch> <symbol>) (service-matrix-index self service)
				  (let ((si (service-list-index self service)))
					 (if si (1+ si) si)))

(sclos-method (<dynamic-patch> <pair>) (service-matrix-index self service)
				  (map (lambda (x) (service-matrix-index x)) service))

(sclos-method (<dynamic-patch> <pair>) (service-values self)
				  (map (lambda (x) (value self x)) (my 'service-update-map)))	

(sclos-method <patch> (dump self . count)
				  (set! count (if (null? count) 0 (car count)))

				  (display (make-string count #\space))
				  (display "<dynamic-patch>\n")
				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (if (not (eq? x 'service-list))
										  (begin
											 (display (make-string (+ 2 count) #\space))
											 (display x)
											 (display ": ")
											 (display y)
											 (newline))))
								  slots vals))
				  (display (make-string (+ 2 count) #\space))
				  (display 'service-list)
				  (display ":\n")
				  (for-each (lambda (x) (dump x (+ 4 count))) (my 'service-list))
				  )


(sclos-method (<dynamic-patch>  <procedure> <symbol> <procedure>)(log-data self logger format caller targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'map-projection)))
					 (if (or (not p) (null? p))  (set! p (lambda (x) x)))
					 (kdnl* '(log-* log-patch) "[" (my 'name) ":" (class-name-of self) "]" "in log-data")
					 
					 (case format
						((ps)
						 (let* ((symlist (services h))
								  (name (slot-ref h 'name))
								  )

							(if adjust-grey (file 'setgray patchgrey))
							(ps-circle file  (p (radius self)) (p (list-head (location self) 2)) 0.7 0.0)


							(let* ((slist (slot-ref self 'service-list))
									 (n (1+ (length slist))) ;; slist is becoming circular?? ....******
									 (ns (length slist))
									 (loc (location self))
									 (rad (radius self))
									 (mm-xoffset 2)
									 (mm-yoffset 2)
									 )
							  (file 'moveto (map p (map + 
																 (list-head loc 2) 
																 (list (+ mm-xoffset (* 1.05 rad))
																		 (+ mm-yoffset (/ ns 2.0)))
																 )))
							  (file 'show-table (map (lambda (x) (string-append (slot-ref x 'name) ": " (pno (value x)))) slist))
							  )
							(crop-caption file p self)
							)
						 )

						;;((text table dump)
						;; (log-data-parent)
						;; )
						(else
						 (log-data-parent))
						)
					 )
				  )

 ;;; format is:
 ;;; 	(set-population-dynamics self
 ;;; 									 (grass ggr gnmort 'no-prey 'no-benefactors 'no-competitors)
 ;;; 									 (rabbit rgr rnmort '(grass 0.0005 0.2) 'no-benefactors 'no-competitors)
 ;;; 									 (fox fgr fnmort '(rabbit 0.0005 0.25) 'no-benefactors 'no-competitors)
 ;;; 									 )


;; this expects a list of functions which return reals
(sclos-method <dynamic-patch> (set-population-dynamics! self . d/dt-list)
				  
				  (slot-set! self 'population-definitions #f)
				  (if (null? d/dt-list)
						(abort!))
				  (if (pair? (car d/dt-list))
						(set! d/dt-list (car d/dt-list))) ;; a list of functions was passed in the "rest" part of the line
				  
				  (if (andf (map procedure? d/dt-list))
						(slot-set! self 'd/dt-list d/dt-list)
						(abort!)))
						
(sclos-method <dynamic-patch> (define-population-dynamics! self . defns )
				  (slot-set! self 'd/dt-list #f)

				  (if (not (and (pair? (car defns)) (pair? (car defns))) (or (string? (caar defns)) (symbol? (caar defns))))
						(abort (string-append	" the format for defining a system of populations is:\n"
														"
         (set-population-dynamics! self 
                (grass dgrass/dt)
                (rabbit drabbit/dt)
                (fox dfox/dt)
                (bear dbear/dt))"
														;;									"
														;;         (set-population-dynamics! agent 
														;;                (grass ggr gnmort 'no-prey 'no-benefactors 'no-competitors)
														;;                (rabbit rgr rnmort '(grass 0.0005 0.2) 'no-benefactors 'no-competitors)
														;;                (fox fgr fnmort '(rabbit 0.0005 0.25) 'no-benefactors 'no-competitors)
														;;                (bear bgr bnmort '((grass 0.0003 0.18) (rabbit 0.0004 0.2) (fox 0.00001 0.2) (bear 0.00001 0.2))))"
														)
								 )
						)
				  

				  (let ((pn (map caar defns))
						  (pf (map (lambda (x) 
										 (let* ((namn (list-ref x 0))
												  (dp/dt (list-ref x 1))
												  (p (make-population namn))
												  )
											(p 'register-populations pn)
											(p 'set-d/dt! dp/dt)
											p
											)
										 )
									  defns))
						  )
					 (set-my! population-names pn)
					 (set-my! population-d/dt pf)
					 )
				  )


(sclos-model-body <dynamic-patch>
						(kdnl* 'model-bodies "In " (class-name-of self) t)
 						;; Ok, I need to be able to refer to service
						;; directly (names) and to classes (types). Type
						;; values are aggregates of the members of the
						;; service-list of that type excluding any of those
						;; members specified by name members.

						;; We can tell the difference because names are
						;; required to be strings and types are required to
						;; be symbols.

						;; Changes in a type value are implemented pro-rata.

;; glenn carter varney -- nice piano -- chopinesque ABC FM 

						(if (<= dt 1e-12) (abort "Bad dt passed to <dynamic-patch> sclos-model-body"))
						(let ((pop-values (map (lambda (x) (value self x)) (my 'population-names)))
								(d/dt-list (my 'd/dt-list))
								)
						  (if (not (null? pop-values))
								(let ((dP (if (not (and d/dt-list (pair? d/dt-list)))
												  (lambda args 0)
												  (rk4* d/dt-list
														  t
														  (+ t dt)
														  (/ dt (my 'subdivisions))
														  pop-values)))
										)

								  ;;(set-my! 'dP dP) ;; Don't really *need* this, except perhaps for debugging
								  (let ((deltas (dP (+ t dt))))
									 (for-each 
									  (lambda (x v)
										 (if (not (zero? (imag-part v)))
											  (abort "Someone has been doing recreational pharmacology"))
										 ;;(dnl (class-name-of self))
										 

										 (add! self x (- v (value self x))) ;; These are the adjustments due to consumption and predation
										 ;;(slot-set! self x v)
										 (if (< (value self x) 0.0) (set-value! self x 0.0))
										 )
									  (my 'population-names) deltas))
								  )

								(aborts (symbol->string (class-name-of self))
										  " has no population names defined!"))
						  )

						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						(parent-body)
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						dt)


;-- <landscape> methods

;;							 (filter (lambda (x) (and (contains? x loc) arg) ) (my 'patch-list)))

;; Default landscape only has the default value, oddly enough
(sclos-method (<landscape> <pair>) (value self loc)
				  (if (contains? self loc)
						((my 'terrain-function) loc)
						(my 'default-value)))

(sclos-method (<landscape> <pair>) (capacity self loc)
				  (if (contains? self loc)
						((my 'terrain-function) loc)
						(my 'default-value)))

;; This is to keep the "run" chain consistent
(sclos-model-body <landscape>
						(kdnl* 'model-bodies "In " (class-name-of self) t)
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						(parent-body)
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt))
						dt
						)



;;(sclos-model-body <landscape> 
;;						(kdnl* 'running (my 'name) ":" (my 'representation) " is running")
;;						(for-each (lambda (x)
;;										(run-model-body x t dt))
;;									 (my 'patch-list))
;;						(parent-body)
;;						dt)



;-- <habitat> methods

(sclos-method <habitat> (agent-prep self . args)
				  (agent-prep-parent)
				  ;(slot-set! self 'variables (unique* (apply append (map extra-variable-list (slot-ref self 'patch-list)))))
)



(add-method initialize
				(make-method (list <habitat>)
								 (lambda (initialize-parent self args)
									(initialise self (list 'scale #f))
									(initialize-parent) ;; call "parents" last to make the initialisation list work
									)))

(sclos-method <habitat> (dump self . count)
				  (set! count (if (null? count) 0 (car count)))
				  (display (make-string count #\space))
				  (display "<habitat>\n")

				  (let* ((slots (map car (class-slots (class-of self))))
							(vals  (map (lambda (x) (slot-ref self x)) slots)))
					 (for-each (lambda (x y) 
									 (if (not (eq? x 'patch-list))
										  (begin
											 (display (make-string (+ 2 count) #\space))
											 (display x)
											 (display ": ")
											 (display y)
											 (newline))))
								  slots vals))
				  (display (make-string (+ 2 count) #\space))
				  (display 'patch-list)
				  (display ":\n")
				  (for-each (lambda (x) (dump x (+ 4 count))) (my 'patch-list)))


(sclos-method (<habitat> <patch>) (add-patch self patch)
				  (set-my! 'patch-list (cons patch (my 'patch-list))))

(sclos-method (<habitat> <procedure>) (remove-patch self pfilter)
				  (set-my! 'patch-list (filter pfilter (my 'patch-list))))

;--- (services...) returns services matching the sym or in the symlist

(sclos-method <habitat> (services self . ss)
				  (if (not (null? ss))
						(begin
						  (if (not (symbol? (car ss))) (set! ss (car ss)))
						  (filter (lambda (x) (member x ss)) (services self))
						  )
						(unique (map string->symbol 
									  (sort (map symbol->string 
													 (apply append (map services 
																			  (patch-list self)))) string<?)))
						))


(sclos-method (<habitat>) (service-list self . ss)
					 (unique*
					  (if (null? ss) 
							(apply append (map (lambda (x) (service-list x)) (patch-list self)))
							(apply append 
									 (map (lambda (x) 
											  (service-list x 
																 (if (symbol? (car ss)) ss (car ss))))
											(patch-list self))))))



(sclos-method (<habitat>) (service-list self)
				  (let* ((P (patch-list self))
							(S (map service-list P)))
					 (apply append S)))


;--- (service?...) queries if a service is present
(sclos-method (<habitat> <symbol>) (service? self sym)
				  (not (null? (services self sym))))


(sclos-method (<habitat> <pair>) (service? self symlist)
				  (not (null? (services self symlist))))



(sclos-method (<habitat> <symbol>) (service-sites self sym)
				  (let loop ((rslt '())
								 (pl (my 'patch-list)))
					 (cond
					  ((null? pl) rslt)
					  ((service? (car pl) sym)
						(loop (cons (car pl) rslt) (cdr pl)))
					  (else (loop rslt (cdr pl))))))


(sclos-method (<habitat> <pair>) (service-sites self symlist)
				  (let loop ((rslt '())
								 (pl (my 'patch-list)))
					 (cond
					  ((null? pl) rslt)
					  ((service? (car pl) symlist)
						(loop (cons (car pl) rslt) (cdr pl)))
					  (else (loop rslt (cdr pl))))))




(sclos-method (<habitat>) (patch-list self . arg)
				  (cond
					((null? arg)
					 (my 'patch-list))
					((and arg (symbol? (car arg)))
					 (let ((symlist arg))
						(filter (lambda (p) 
									 (let ((s (services p symlist)))
										(and s (not (null? s)))))
								  (my 'patch-list))))
					((and arg (pair? (car arg)))
					 (let ((symlist (car arg)))
						(filter (lambda (p) 
									 (let ((s (services p symlist)))
										(and s (not (null? s)))))
								  (my 'patch-list))))
					((and arg (procedure? (car arg)))
					 (let ((pfilter (car arg)))
						(filter pfilter (my 'patch-list))))
					(else (my 'patch-list))))


(sclos-method (<habitat> <pair> <number> <pair>) (aggregate-value self location radius servicelist)
				  (let* ((sl (service-sites self servicelist))
							(lsl (filter (lambda (patch) 
												(>
												 (intersection-of-two-circles 
												  (distance-to-centre patch location)
												  radius (slot-ref patch 'radius))
												 0.0))
											 sl))
							(lslv (if (null? lsl) 
										 0.0
										 (apply + (map (lambda (patch)
															  (* (value patch servicelist)
																  (overlap-decay
																	0.0 ;; 1.0 gives us 1% of the pop at the radius, 0 gives us a uniform dist
																	(distance (list-head (slot-ref patch 'location) 2) location)
																	radius 
																	#t
																	0.0
																	(slot-ref patch 'radius)	
																	#t)
																  ))
															lsl))))
							)
					 lslv))


;(define (I->E f) (inexact->exact (round f)))
(define (I->E f) (inexact->exact (truncate f)))

(define (repro xy res m)
  (map I->E (map (lambda (x) (/ x res)) (map - xy m))))


(sclos-method <habitat> (min-bound self)
				  (let* ((v (map min-bound (slot-ref self 'patch-list)))
							(vx (apply min (map car v)))
							(vy (apply min (map cadr v)))
							)
					 (list vx vy)))

(sclos-method <habitat> (max-bound self)
				  (let* ((v (map max-bound (slot-ref self 'patch-list)))
							(vx (apply max (map car v)))
							(vy (apply max (map cadr v)))
							)
					 (list vx vy)))

;;1.0/(1.0 + exp(-2*pi*l*(2*(x+(0.5 - off)) - 1.0)) )
;; l = 1.0, off = 0.5



(define (def-res H)
  (let* ((m (min-bound H))
			(M (max-bound H))
			(extent (map - M m))
			)
	 (/ (apply min extent) 20.0)))

(define print-environment-data
  (lambda (ps p x n ns loc rad)
	 ;;(dnl (pno (value x)))
	 (ps 'moveto (map p (map + (list-head loc 2) (map p (list (* 1.0 rad) (* (- (/ ns 2.0) n) 1.0))))))
	 (if adjust-grey (ps 'setgray PATCHGREY)) ;; zero is white...
	 (ps 'Helvetica 7)
	 (ps 'show (string-append (slot-ref x 'name) ": "  (number->string (value x))))
	 ))


(define crop-caption
  (lambda (ps p x . pt)
	 (if (null? pt) (set! pt 10))
	 (let ((loc (map p (list-head (location x) 2)))
			 (rad (p (radius x))))
		(ps 'moveto (map - loc (list (* 0.5 rad)  (* -1 (+ 5 (* 1 rad) )))))
		(ps 'Helvetica pt)
		(if adjust-grey (ps 'setgray PATCHGREY))
		(ps 'show-right (string-append (slot-ref x 'name) " at " (number->string (slot-ref x 'subjective-time))))
		)
	 )
  )



;; p is usually something like mm->points

(sclos-method (<habitat> <procedure>) (map-log-data self logger format caller targets)
				  (let* ((symlist (services H))
							(name (slot-ref H 'name))
							(plist (slot-ref H 'patch-list))
							(locs (centroid (map location plist)))
							)
					 (let ((file (slot-ref logger 'file))
							 (p (slot-ref self 'map-projection)))
						(if (or (not p) (null? p))  (set! p (lambda (x) x)))


						(ps 'moveto (list (p (car locs)) (p (cadr locs))))
						(if adjust-grey (ps 'setgray HABITATGREY))
						(ps 'Helvetica 12)
						(ps 'show (string-append (slot-ref H 'name)))								  

						(if (member 'nested-habitat nested-agents)
							 (for-each (lambda (lpch)
											 (ps 'Helvetica 7)
											 (map-log-data lpch format caller targets p ps)
											 )
										  plist))
						)))

(sclos-method (<habitat> <procedure> <symbol> <procedure>) (log-data self logger format caller targets)
				  (let ((file (slot-ref logger 'file))
						  (p (slot-ref self 'map-projection)))
					 (if (or (not p) (null? p))  (set! p (lambda (x) x)))
					 (case format
						((ps)
						 (map-log-data self logger format caller targets p ps)
						 (if (member 'nested-habitat nested-agents)  
							  (for-each (lambda (lpch)
											  (log-data lpch logger format caller targets ps p)
											  )
											plist))
						 )
						((dump)
						 (with-output-to-port
							  (lambda ()
								 (dump self))))

						((text table)
						 (log-data-parent)

						 ))
					 )
				  )

(sclos-method (<habitat>) (spatial-scale self)
				  (if (not (my 'scale))
						(let ((lscale (apply append (map (lambda (x) (map (lambda (y) (distance (location x) (location y))) (patch-list self))) (patch-list self)))))
						  (set-my! 'scale (/ (apply + lscale) (1+ (- (length lscale) (length (my 'patch-list)))))) ;;  This gets rid of the "self-distances" which are zero
						  )
						)
				  (my 'scale))


(sclos-model-body <habitat>
						(dnl* "HABITAT: model times min/mean/max/subjective = " (kernel 'min-time) "/" (kernel 'mean-time)"/" (kernel 'max-time)"/" (my 'subjective-time) " + " dt)
						(kdnl* 'model-bodies "In " (class-name-of self) (name self) "@" t)

						(if (member 'nested-habitat nested-agents)
							 (for-each (lambda (x) 
											 (if (< (subjective-time x) (+ t dt))
												  (run x t (+ t dt) (my 'kernel))
												  (dnl* "Skipping" (name x)))
											 )
										  (my 'patch-list)
							 	)
							 )
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt) 'A)
						(parent-body)
						(kdnl* 'nested-habitat (name self) "@" t "/"(subjective-time self) ":" dt "/" (my 'dt) 'A)
						dt
						)

(define (make-patch loc radius services . args) ;; services is a list of lists, with each inner list consisting of a name (a string), a type (a symbol) and a value (typically a number, capacity)  and another number (typically the delta-T-max
  (let* ((loc loc) 
			(radius radius)
			(service-update-map (if (null? args) '() (car args)))
			(update-equations (if (null? args) '() (cadr args)))
			(P (make <dynamic-patch> 'location loc 'radius radius 'type 'patch 'representation 'patch))
			)
	 (if (not (null? services))
		  (let ((sl (map 
						 (lambda (n t q) (make <ecoservice> 'patch P 'name n 'type t 'capacity q )
									)
						 (map car services)
						 (map cadr services)
						 (map caddr services)
						 (map cadddr services)
						 )))
			 (set-services! P sl)
			 (slot-set! P 'service-update-map service-update-map )
			 (slot-set! P 'update-equations update-equations)
			 ))
	 
	 P))



(sclos-method (<habitat> <symbol>)(extra-variable self field)
				  (value self (symbol->string field)))


(sclos-method (<habitat>) (extra-variable-list self)
				  (let ((patch-vars (unique* (apply append (map extra-variable-list (my 'patch-list)))))
						  )
					 (dnl* "HABITAT PATCH VARIABLES:" patch-vars)
					 (unique* (append (list 'name 'subjective-time) patch-vars)))) ;; returns a list of symbols


;; domain is a list  ((minx miny minz) (maxx maxy maxz))
;; patch-data is a list of  services lists for make-patch

(define (make-habitat name default-ht domain terrain-function 
							 patch-data
							 )
  (let* ((PL (map (lambda (x) (apply make-patch (list (apply random-location domain) (map (lambda (y) (* 0.25 x)) (apply min (map - maxv minv))) x))) patch-data))
			(H (make <habitat> 'name name 'default-value default-ht 'minv (car domain) 'maxv (cadr domain) 'terrain-function terrain-function 
						'patch-list PL))
			)
	 H)
  )

(define (patchsize domain)
  (* 0.25 (apply min (map - (list-head (cadr domain) 2) (list-head (car domain) 2)))))



(define (add-habitat-to-queue Q h) ;; returns the queue, so us it like (set! Q (add-...-queue Q hab))
  (let ((p (patch-list h))
		  (s (service-list h))
		  )
	 (unique* (append Q (list h) p s))))


(define (locate-nearest-ecoserv habitat ecoserv loc)
  (let* ((patches (service-sites habitat ecoserv))
			(dists (map (lambda (x) (distance-to-centre x loc)) patches))
			(sdists (sort 
						(filter 
						 (lambda (x) (and (number? (car x)) 
												(not (null? (cdr x)))))
						 (map cons dists patches)) 
						(lambda (x y) (< (car x) (car y)))
						))
			)
	 (if (null? sdists) #f (cdar sdists))))

(define (sorted-ecoservices habitat ecoserv loc . weighted-by-value)
  (let* ((patches (service-sites habitat ecoserv)))
	 (let ((dists (map (lambda (x) (distance-to-centre x loc)) patches)))
		(let ((sdists (sort 
							(filter 
							 (lambda (x) (and (number? (car x)) 
													(not (null? (cdr x)))))
							 (map cons dists patches)) 
							
							(if (null? weighted-by-value)
								 (lambda (x y)
									(< (car x) (car y)))	
								 (lambda (x y)
									(< (/ (total-value (cdr x) ecoserv) (1+ (car x)))
										(/ (total-value (cdr y) ecoserv) (1+ (car y)))
										))
								 )
							)))

		  (if (null? sdists) #f (map (lambda (x) (list (car x) (cdr x))) sdists))))
	 ))



;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
;-   Identification and Changes

;--

;	kernel.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2008.04.07
;		Location: localhost:/usr/home/gray/Study/playpen/kernel.scm.scm
;
;-  Code 


;;(require 'sort)
;;(require 'pretty-print)
;;(require 'common-list-functions)


;(include "method%.scm")
;(include "model-body%.scm")

;;*** The kernel must be loaded *after* the agents are  all created -- this is so
;;*** it has access to the "run" "migrate" ... generic methods.


;;---------------------------------------------------
;; Important routines which I Really Ought to Know ;;
;;---------------------------------------------------


;; (queue t stop runqueue . N)
;; t is model time, stop is when the whole model ought to stop 
;; runqueue is a list of run-records

;; (q-insert Q rec reccmp)
;; inserts the record "rec" into the queue "Q" sorting records with "reccmp"
;; Call: (set! rq (q-insert rq (make <whatever> ...) Qcmp)

(define lookit-running-names #f) ;; emits the name of the running agent if true

                    ;;; snapshot data spanning agents 


(define developing #t)
(define current-page #f)
(define current-page-number 0)


(define terminating-condition-test 
			(lambda rq #f))


;---------------------------------------------------
;               Kernel support
;---------------------------------------------------

;; Compares the subjective time of two agents
(define (Qcmp r1 r2)
  (< (subjective-time r1 ) (subjective-time r2)))

(define (map-q arg q)
  (map (lambda (s) (arg s)) q))


(define (running-queue rq stop)
  (cond
	((null? rq) #f)
	((not (pair? rq)) #f)
	((not (list? rq)) #f)
	((not (agent? (car rq))) #f)
	(#t (let ((f (car rq)))
			(< (subjective-time f) stop)))))

;; returns the "time" of the agent at the head of the queue (unused)
(define (model-time rq stop)
  (if (running-queue rq stop)
		(if (or (null? rq) (not (list? rq)))
			 rq
			 (let ((f (car rq)))
				(subjective-time f))
			 )
		'end-of-run
		)
  )

;; returns an interval (tick-length) based on the current time, the desired tick length, 
;; the nominated end of the run and a list of target times
(define (interval t ddt stopat tlist)
  ;; tlist is a sorted queue of times to run
  (if (< (- stopat t) ddt)
		(set! ddt (- stopat t)))

  (cond
	((null? tlist)	
	 ddt)
	((and (list? tlist) 
			(number? (car tlist))
			(eq? (car tlist) t)
			)
	 ddt)
	((and (list? tlist) 
			(number? (car tlist))
			)
	 (- (car tlist) t))
	(else 'bad-time-to-run)))

;; remove stale times in the time-to-run queue
(define (prune-local-run-queue tm ttr)
  (let (
;		  (call-starts (cpu-time))
		  (r '())
		  )
	 (set! r (let loop ((l ttr))
				  (if (or (null? l)
							 (> tm (car l))
							 )
						l
						(loop (cdr l)))))
;	 (set! kernel-time (+ kernel-time (- (cpu-time) call-starts)))
	 r )
  )

;; insert a run record in the queue Q
(define (q-insert Q rec reccmp)
  (let (
;		  (call-starts (cpu-time))
		  )
	 (set! Q (remove rec Q))
	 (let* ((f (append Q (list rec)))
			  (sf (sort f reccmp))
			  )
;		(set! kernel-time (+ kernel-time (- (cpu-time) call-starts)))
		sf)
	 )
  )

;---------------------------------------------------
;           Kernel -- the main loop
;---------------------------------------------------

;; this is so we catch runaway growth
(define (test-queue-size rq N)
  (if (and (number? N) (> (length rq) N))
		(begin
		  (dnl "There are " (length rq) " entries in the runqueue when we expect at most " N);
		  (dnl "These entities total " (apply + (map (lambda (x) (members x)) rq)) " members");
		  
		  (map (lambda (me) 
					(dnl "   " (representation me) ":" (name me) " (" me ")  @ " (subjective-time me) " with " (members me))) 
				 rq)
		  (abort "Failed queue size test")
		  ))
  )


;; This is the main loop which runs agents and reinserts them after they've executed.
;; There is extra support for inter-agent communication and migration.
;;
;;     The queue doesn't (and *shouldn't*) care at all about how much time the agents used.
;;

(define (queue t stop runqueue . N)
  (set! N (if (null? N) #f (car N)))

  (let loop ((rq runqueue))
	 (cond
	  ((terminating-condition-test rq)
		(list 'terminated rq))
	  ((null? rq)
		'empty-queue)
	  ((and (list? rq) (symbol? (car rq)))
		rq)
	  ((file-exists? "halt")
		(delete-file "halt")
		(shutdown-agents rq)
		rq)
	  ((and (< t stop) (running-queue rq stop))
		(set! t (apply min (map-q subjective-time rq)))
		(test-queue-size rq N)
		(set! rq (run-agent t stop rq))
		(test-queue-size rq N)
			 (loop rq))
	  )
	 )
  )

;; converts the parameter vector "params" to reflect a new representation
(define (convert-params params rep)
  (let* ((newparams params)
			)
	 (list-set! newparams 3 rep)
	 (list-set! newparams 5 (if (eq? rep 'individual) 1 0))
	 newparams
	 ))


(define (distances-to what agentlist loc)
  (map (lambda (agent) 
			(if (and (procedure? agent)
						(eq? (representation agent) what))
				 (distance loc (location agent ))
				 2e308)
			)
		 agentlist)
  )

(define (distances-to-agents agentlist loc)
  (map (lambda (agent) 
			(if (procedure? agent)
				 (distance loc (location agent))
				 1e308)
			)
		 agentlist)
  )

(define (distances-to-populations agentlist loc)
  (distances-to 'population agentlist loc))


;; returns the index of the left-most value
(define (min-index n-list)
  (cond 
	((not (list? n-list)) 'not-a-list)
	((null? n-list) #f)
	((not (apply andf (map number? n-list)))
	 'Non-number-entry)
	(#t
	 (let* ((k (length n-list))
			  (result (let loop ((ix 0)
										(best #f)
										)
							(if (>= ix k)
								 best
								 (let ((n (list-ref n-list ix)))
									
									(cond 
									 ((infinite? n) ;; skip invalid entries
									  (loop (1+ ix) best))
									 ((and (number? n)
											 (or 
											  (not best)
											  (let ((b (list-ref n-list best)))
												 (or (infinite? b)
													  (and (number? best) (<= n b)))))
											 )
									  (loop (1+ ix) ix))
									 (#t (loop (1+ ix) best)))
									))) ))
		(if (or (not result) (infinite? result) (>= result 1e308))
			 #f
			 result)
		))
	)
  )

(define (kernel-call Q client query #!optional args)
  (cond
   ((procedure? query) (filter (lambda (x) (and (query x) (not (eq? x client)))) Q)) ;; Do not return the client
   ((symbol? query)
    (cond
     ((eq? query 'time) (model-time Q +inf.0))
	  ((eq? query 'agent-count) (length *agent-list*))
     ((eq? query 'agent-Q-count) (length Q))
     ((eq? query 'next-agent) (if (null? Q) Q (car Q)))
	  ((eq? query 'min-time) (if (null? Q) 0 (apply min (map subjective-time Q))))
	  ((eq? query 'max-time) (if (null? Q) 0 (apply max (map subjective-time Q))))
	  ((eq? query 'mean-time) (if (null? Q) 0 (/ (apply + (map subjective-time Q)) (length Q))))
     (#t (abort 'kernel-call:not-defined)))
	 )
   (#t (abort 'kernel-call:bad-argument))
   )
  )



;; The agent function, "process",  must respond to the following things
;;    (snapshot agent)

;;    (i-am agent)
;;    (is-a agent)
;;    (representation agent)
;;    (name agent)
;;    (subjective-time agent)
;;    (dt agent)
;;    (parameters agent)

;;    (run-at agent t2)
;;    (run agent currenttime stoptime kernel)



;; The return values from agents fall into the following categories:

;; 	a symbol
;; 		is automatically inserted at the head of the queue and execution is terminated (for debugging)

;; 	dt 
;; 		normal execution

;; 	(list 'introduce-new-agents dt list-of-new-agents)
;; 	   indicates that the agents in list-of-new-agents should be added to the simulation
;; 	(list 'remove-me dt)
;; 	   indicates that an agent should be removed from the simulation
;; 	(list 'migrate dt list-of-suggestions)
;; 	   the list-of-suggestions is so that an external assessment routine 
;; 	(list 'domain dt message-concering-domain-problem)
;; 		usually something like requests for greater resolution...
;; 	(list 'migrated dt)
;;      indicates that a model has changed its representation for some reason


(define (prep-agents Q start end . args)
  (kdnl* 'prep "Prepping from" start "to" end "    with" Q)
  (for-each
	(lambda (A)
	  (kdnl* 'prep "Prepping in lambda" (name A))
	  (let ((kernel (lambda x (apply kernel-call (append (list rq A) x ))))
				)
		 (kdnl* 'prep "Prepping in apply" (name A))
		 (apply agent-prep (append (list A start end kernel) args))
		 )
	  )
	Q)
  )

(define (shutdown-agents Q . args)
  (for-each
	(lambda (A)
	  (let* ((kernel (lambda x (apply kernel-call (append (list rq process) x ))))
				)
		 (apply agent-shutdown (append (list A kernel) args))
		 )
	  )
	Q)
	)


;; Dispatches a call to the agent through the "run" routine. It also
;; handles special requests from the agent like mutation and spawning.
;; subjective time is set in  (run ...)
(define run-agent
  (let ((populist '())) ;; Remember the population list across
								;; invocations ... (equiv to a "static" in C,
								;; folks.)


	 (lambda (t stop rq . N) ;; This is the function "run-agent"
      (set! N (if (null? N) #f (car N)))

		(let* ((rq rq)
				 (process (if (and (not (null? rq)) (list? rq)) (car rq) #f)) 
				 ;; ... either false or the lambda to run
				 (agent-state (slot-ref process 'agent-state))
				 )

		  (or (eq? agent-state 'running)
				(eq? agent-state 'ready-to-run)
				(and (eq? agent-state 'ready-for-prep)
					  (abort (string-append "Attempted to run " 
													(symbol->string (class-name-of process)) ":"(name process)
													" before it has been prepped")))
				(abort (string-append "Attempted to run " 
											 (symbol->string (class-name-of process)) ":"(name process)
											 " when it is in the state " (object->string agent-state))))

		  (if process (kdnl* 'running "running" (name process) "at" t))

		  (test-queue-size rq N)
		  ;; remove the agent's run request from the top of the queue
		  (set! rq (remove process rq))
		  (test-queue-size rq N)
		  
		  (kdnl* 'run-agent "In run-agent")

		  (slot-set! process 'agent-body-ran #f) ;; Mark things as not having run through the body list
		  
		  ;; Here result should be a complex return value, not the number of ticks used.
		  (let* ((kernel (lambda x (apply kernel-call (append (list rq process) x ))))
					(result (if (symbol? process) 
									'bad-runqueue
									(if (eq? agent-state 'suspended) ;; A suspended agent "consumes" its time without doing anything, except update its subj. time.
										 (let ((dt (interval 
														t
														(slot-ref process 'dt) 
														stop
														(slot-ref process 'timestep-schedule))
													  )
												 (st (slot-ref process 'subjective-time)))
											'ok)
										 (let ((r (run process t stop kernel)))
											(if lookit-running-names (dnl (slot-ref process 'name) " " (subjective-time process) " " r))
											(kdnl* 'run-agent "finished with run" (name process) "@" (subjective-time process) "+" r)
											r
											)
										 )
									))
					)
			 (if (not (slot-ref process 'agent-body-ran))
				  (begin
					 (dnl "The agent " (class-name-of process) ":" (name process) " failed to chain back to the base <agent> sclos-model-body.")
					 (dnl "This suggest that things have gone very wrong; either call (parent-body) or (skip-parent-body).")
					 (abort 'missed-model-body)))

			 (let (
					 ;;(call-starts (cpu-time))
					 )

				(cond
				  ((eq? result 'ok)
					(set! rq (q-insert rq process Qcmp))
				  rq)

				 ((number? result) 
				  ;; The result (in this case) is the amount of time used
				  ;; measured from the subjective-time of the agent.
				  ;; q-insert knows how to find out "when" the agent is,
				  ;; and will re-insert it correctly.  subjective-time is
				  ;; updated
				  (abort "(run ...) returned a number rather than a state")
				  (set! rq (q-insert rq process Qcmp)))

				 ((symbol? result) ;;---------------------------------------------------------------------------------------------------------
				  (let ()
					 (dnl "Got " result)

					  (cond
					  ((eq? result 'remove)
						rq)
					  (else 
						(cons result rq)))
					 ))
				 ((eq? result void)
				  (let ((s (string-append "A " (symbol->string (class-name-of process)) " tried to return a void from its model-body.  This is an error")))
					 ;;(Abort s)
					 (abort s)
					 ))

				 (#t
				  (set! rq (q-insert rq process Qcmp)))
				 ((list? result)
				  (case (car result)
					 
					 ;; Remove ===========================================================================
					 ('remove
					  rq
					  ) ;; end of the migration clause

					 ;; Migrate to a different model representation ===========================================================================
					 ('migrate
					  #f
					  ) ;; end of the migration clause

					 ;; insert spawned offspring into the system ** not implemented in make-entity
					 ('spawnlist ;;------------------------------------------------------------------------------------------------------------
					  #f
					  )
					 (else 'boink)
					 ) ; case
				  ) ; cond clause
				 )
				)
			 (test-queue-size rq N)
			 (kdnl* 'run-agent "Finished with run-agent" (name process) "@" (subjective-time process))
			 ;; *********** THIS MAY NOT BE NECESSARY OR WHAT I WANT **************
			 ;; I may need to have a method that will take a kernelcall procedure from
			 ;; another agent if there is out-of-band activity that requires a kernelcall.
			 ;; Best to play conservative for the moment.
			 rq)
		  )
		)
	 )
  )


(define nested-agents '())

;; Q is the preloaded run-queue
;; Start and End are numbers s.t. Start < End
(define (run-simulation Q Start End . close-up-shop) 
  (prep-agents Q Start End)

  (set! Q (queue Start End Q))

  ;; We don't shut down just now, we are still developing
  (if (not developing) (shutdown-agents Q))

  (if (and (not (null? close-up-shop)) (procedure? (car close-up-shop)))
		((car close-up-shop)))
  )

(define (continue-simulation Q End . close-up-shop) 
  (set! Q (queue (subjective-time (car Q)) End Q))
  (if (and (not (null? close-up-shop)) (procedure? (car close-up-shop)))
		((car close-up-shop)))
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
;	model.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2013.02.05
;		Location: odin:/home/gray/study/src/model.scm
;
;-  Code 


;(dump wally)(newline)
;(aborts "Incomplete initialisation is making things fail when it runs")



(define start 0)
;(define end 500)
;(define end 6.1)
(define end 741)


(define A4domain (list 210 294 80)) ;; (x y z) corresponds to the size of an A4 page
(define mA4domain (list 178 250 80))

(define domain mA4domain)

(set! nested-agents '(nested-habitat)) ;; No, each patch does its own thing....

;;(set! kernel-messages (append kernel-messages '(*)))
;;(set! kernel-messages (append kernel-messages '(running  log-*)))
;;(set! kernel-messages (append kernel-messages '(running focus stomach hunger-proximity eating)))
;;(set! kernel-messages (append kernel-messages '(running animal-running focus log)))
;;(set! kernel-messages (append kernel-messages '(running focus stomach hunger-proximity eating)))
;;(set! kernel-messages (append kernel-messages '(animal-running stomach hunger-proximity eating)))
;;(set! kernel-messages (append kernel-messages '(metabolism animal-running stomach hunger-proximity eating)))


;(define landscape
;  (make <landscape> (list 'name "landscape" 'type 'forest-floor 'representation 'plane 'terrain-function (plane '(12 8 4 0)) 'default-value 0 'minv '(0 0 0) 'maxv '(100 100 2000))))


(define (rerun-agent agent #!optional dt)
  (set! dt (if dt dt 0.1))
  (run-model-body agent (slot-ref agent 'subjective-time) dt))




;; ecoservsetlist is a list of sets of ecological services which might be incorporated in a patch. 
;; Each of these sets has an associated "accepted" function which will return #t if the generated
;; patch is ok, otherwise it returns #f.  The format of an ecoservsetlist is
;;
;;     ((eco-serv-set-prob  (prob name type cap mean-biomass update-function) ...) ...)

(define (caddddr x) (car (cddddr x)))
 
(define record-ecoservice-history #t)
(define (filter-hist-by-patch s d) (filter (lambda (x) (string=? (car x) s)) d))


(define (history-data H)
  ;; (map name (patch-list H))
  ;; (map cons (map name (patch-list H)) (map service-list (patch-list H)))
  (map (lambda (x) (cons (car x) (map (lambda (y) (list (name y) (slot-ref y 'history))) (cdr x)))) (map cons (map name (patch-list H)) (map service-list (patch-list H))))
  )


(define (generate-an-ecoservice-like P prob name type cap value dt r dTmax)
  (if (or (and (boolean? prob) prob) (and (number? prob) (<= (random-real) prob)))
		(make <ecoservice> 'patch P 'name name 'type type 'capacity cap 'value value 'dt dt 'r r 'delta-T-max dTmax 'history (if record-ecoservice-history '() #f))
		#f
		))

;;(define (generate-a-dynamic-patch-like namn domain radius ecoservlist popname-list derivatives #!optional subdivisions)
;; 
;; where name is a string, domain is a box it is seeded into, radius is the radius of the patch, ecoservlist is 
;; a list like '((.3 hay-crop grass 20000000 2000000 3 .01) (.9 rabbit rabbit 6000 600 3 .01) (.05 goat caprid 160 16 3 .01))
;; where the first element is the probability of inclusion, the next is the name, then type, max value (or #!+inf) starting value, 
;; and finally the dt and r

(define (generate-a-dynamic-patch-like namn domain radius ecoservlist populations subdivisions)
  ;; Handle optional arguments
  (let ((P (make <dynamic-patch> 'name namn 'location (apply random-location domain) 'radius radius 'population-names (populations 'species-names) 'population-symbols (populations 'species-eqn-syms))))
	 (filter (lambda (t) t)
				(for-each (lambda (x)
								(let ((p (apply generate-an-ecoservice-like (cons P x))))
								  (if p (add-service P p))
								  p))
							  ecoservlist)
				)
	 
	 ;; We install the dynamics here
	 (slot-set! P 'd/dt-list (populations 'd/dt-list))
	 (if subdivisions (slot-set! P 'subdivisions subdivisions))
	 P))
		 
;;(define (generate-a-dynamic-patch-from-set name domain radius ecoservsetlist derivatives #!optional subdivisions)
(define (generate-a-dynamic-patch-from-set name domain radius populations subdivisions)

  (let* ((template (populations 'template))
			(ecoservlist (random-list-ref (map cdr template) (map car template))))
	 (generate-a-dynamic-patch-like name domain radius ecoservlist populations subdivisions)))


;;(define (generate-a-habitat namn typ default-ht domain terrain-function npatches predator-prey-matrix species-data ecoservsetlist #!optional subdivisions)
(define (generate-a-habitat namn typ default-ht domain terrain-function npatches populations subdivisions)
  (let ((H 
			(make <habitat> 
			  'name namn 'type type 'representation 'intermediate 'minv (car domain) 'maxv (cadr domain)
			  'terrain-function terrain-function 'default-value default-ht
			  'patch-list 
			  (map (lambda (x) 
						(let* ((pname (string-append namn "-" (number->string x)))
								 (P (generate-a-dynamic-patch-from-set pname domain (nrandom (/ (patchsize domain) npatches)) populations subdivisions))
								 )
						  P))
					 (map 1+ (seq npatches)))
			  )
			) )
	 H)
)
#|
dA = t A{r(1 - A/K)[1 - prey_adj ∑(prey_i prey_rate_i)]/(maint_rate * A) - pred_adj ∑ (pred_i pred_rate_i)}

where A is anything, K is the carrying capacity, r is the natural
growth rate, maint_rate is the amount it needs to consume to maintain
biomass.  It is an error to have a prey rate for things that don't
have maint_rates, and we need to bypass the division by zero in this
case.

|#




;
(define H (generate-a-habitat "Seabed" 'coastal 0 (list '(0 0 10) domain )  (plane '(10 4 6 0)) 10 gsr-population 12))


;;(define schedtimes 
(define schedtimes (append 
						 (cons 0 (seq 6))
						 (map (lambda (x) (* 10.0 (1+ x))) (seq 360)))
  ) ;; first six days, then on every tenth day from the beginning for 370 days



(define missing-value 0)  ;; things arrived at by mortality are likely to be inexact

(define logger 
  (make <log-data> (list 'name "Data" 
								 'dt 4
								 'missing-val missing-value
							  'timestep-schedule schedtimes 
							  'filename "Data"
							  'variables '()) ;; log-table does not automatically log the name at the front of the line
							  )
		  )


(define Q '())

(if (not (member 'nested-habitat nested-agents))
	 (set! Q (add-habitat-to-queue Q H)) ;; this introduces all of the "sub-agents" into the run-queue
	 (set! Q (list H)))

(set-introspection-list! logger (copy-list (patch-list H)))
(for-each (lambda (x) (set-map-projection! x mm->points)) Q)

(set! Q (cons logger Q)) ;; insert the logger at the head of the list


(define terminating-condition-test
  (let* ((tct terminating-condition-test)
			(l (lambda (Q)
				  (and (tct Q) (number? (slot-ref wally 'mass)) (number? (slot-ref wilma 'mass)))
				  )
				))
	 l))
	 
(dnl "Run with (doit Q n) to use a step of n days, as many times as you like....")
(dnl "Close up shop with (shutdown-agents Q) -- this closes files and things.")



;(define test-G (generate-an-ecoservice-like test-P 1.0 "pasture" 'grass 1400000 14000000 3 0.1))
;(define test-g (generate-an-ecoservice-like test-P 1.0 "pasture" 'grass 1400000 140000 3 0.1))
;(define test-r (generate-an-ecoservice-like test-P 1.0 "rabbit" 'rabbit 2000 200 3 0.01))
;(define testq (list test-G test-g test-r))

;;(dnl "*** Need to ensure that there are no common symbols between the type list and")
;;(dnl "the symbol list, also that all the symbols in the symbol list are unique")

(define (check-service-data-lists service-name-list service-type-list service-eqn-sym-list)
  (let* ((type-symbol-clash (intersection service-eqn-sym-list service-type-list))
			(unique-service-eqn-sym-list (unique* service-eqn-sym-list))
			(unique-service-name-list (unique* service-name-list))
			(duplicate-symbols (filter (lambda (x) (not-member x unique-service-eqn-sym-list)) service-eqn-sym-list))
			(duplicate-names (filter (lambda (x) (not-member x unique-service-name-list)) service-name-list))
			)
	 (if type-symbol-clash (dnl "The symbols " type-symbol-clash " exist in both the "
										 "type column and the symbol column in the service-data-list"))
	 (if (not (null? duplicate-symbols)) (dnl "The symbol(s) " duplicate-symbols " are duplicated in the symbol column"))
	 (if (not (null? duplicate-names)) (dnl "The symbol(s) " duplicate-names " are duplicated in the name column"))
	 (if (not (and (null? duplicate-symbols) (null? duplicate-names) (null? type-symbol-clash)))
		  (abort "Ill-defined configuration"))))
		  

(define (Doit q)
  (check-service-data-lists service-name-list service-type-list service-eqn-sym-list)
  (prep-agents testq 0 30)
  (set! testq (queue start end q))
  )

(define Dunnit #f)

(define *dunnit* #f)

(define (doit q . n)
  (set! Dunnit (lambda () (shutdown-agents q)))
  (set! n (if (pair? n) (car n) 1))
  (if (not *dunnit*) (begin (prep-agents q start end) (set! *dunnit* 0)))
  (set! q (queue *dunnit* (+ *dunnit* n) q))
  (set! *dunnit* (+ *dunnit* n))
  )





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
;	model-parameters.scm -- Written by Randall Gray 


(define gsr-species-individuals ;; used in the rk4* equations
  (let ((grscale 0.0)
		  (mrscale 0.0)
		  )
	;; mort is the natural mortality, growth is the non-sigmoidal growth rate (exponential)
	;; Basic species data (name, type growth rate, mortality rate, age at "max mass"
	(list (list "seagrass" 'grass 'G (* grscale 0.04) (* mrscale 0.0005) (years 0.25))
			(list "sponge" 'sponge 's (* grscale 0.008) (* mrscale 0.0005) (years 0.25))
			(list "reef" 'reef 'r (* grscale 0.0023) (* mrscale 0.0005) (years 50.))
			)
		)
	)


(define gsr-predation-matrix ;; competition matrix, really ... the mortality/growth associated with an interaction
  (make-matrix 
	;; (g s r)
	'((0      0.0004 0.0002) ;; g
	  (0.0003 0      0.0005) ;; s 
	  (0.0005 0.0005 0)      ;; r
	 )))


(define gsr-efficiency-matrix ;; says it all really ... efficiency convert from the victim mass to victor mass
  (make-matrix 
	;; (g s r)
	'((0 0.3 0.3) ;; g
	  (0.2 0 0.2) ;; s 
	  (0.2 0.2 0) ;; r
	 )))



(define gsr-population
  (make-population-structure
	
	;; Defines the basic predatory interaction rates between species
	gsr-predation-matrix

	;; Defines the basic predatory interaction rates between species
	gsr-efficiency-matrix

	;; Basic species data (name, type growth rate, mortality rate, age at "max mass"
	gsr-species-individuals

	;; Patch template for population level data.  "r" is the "sharpness" parameter for sigmoidal growth
	(list
	 (list 0.5  ;prob  name   class   MaxCap   CurVal dt r dTmax
			 (list 0.99 "sponge" 'sponge 41000 4100 3 1 (years 20))
			 (list 0.99 "seagrass" 'grass 41000 4100   3  1 (years 5))
			 (list 0.99 "reef" 'reef 41000 6100 3 1 (years 275))
			 )
	 (list 0.5
			 (list 0.99 "sponge" 'sponge 42000 6200 3 1 (years 20))
			 (list 0.99 "seagrass" 'grass 42000 4200   3  1 (years 5))
			 (list 0.99 "reef" 'reef 42000 4200 3 1 (years 275))
			 )
	 (list 0.5
			 (list 0.99 "sponge" 'sponge 43000 4300 3 1 (years 20))
			 (list 0.99 "seagrass" 'grass 43000 6300   3  1 (years 5))
			 (list 0.99 "reef" 'reef 43000 4300 3 1 (years 275))
			 )
	 (list 0.5
			 (list 0.99 "sponge" 'sponge 43000 4300 3 1 (years 20))
			 (list 0.99 "seagrass" 'grass 43000 4300   3  1 (years 5))
			 (list 0.99 "reef" 'reef 43000 4300 3 1 (years 275))
			 )
	 ))
  )




(define V-species-individuals
  (let ((grscale 1.0)
		  (mrscale 1.0)
		  )
	;; Basic species data (name, type growth rate, mortality rate, age at "max mass"
	(list (list "grass" 'grass 'G (* grscale 0.008) (* mrscale 0.0) (years 0.25))
			(list "hay" 'grass 'h (* grscale 0.008) (* mrscale 0.0) (years 0.25))
			(list "myrtle" 'tree 'm (* grscale 0.0023) (* mrscale 0.0) (years 50.))
			(list "eucalypt" 'tree 'e (* grscale 0.0019) (* mrscale 0.0) (years 120))
			(list "wattle" 'tree 'w (* grscale 0.0029) (* mrscale 0.0) (years 10))
			(list "rabbit" 'rabbit 'r (* grscale 0.0008) (* mrscale 0.1) (years 0.3))
			(list "sheep" 'caprid 's (* grscale 0.00075) (* mrscale 0.1) (years 4))
			(list "goat" 'caprid 'g (* grscale 0.00075) (* mrscale 0.1) (years 4))
			(list "beef-cattle" 'bovine 'b (* grscale 0.0007) (* mrscale 0.1) (years 5))
			(list "dairy-cattle" 'bovine 'd (* grscale 0.0007) (* mrscale 0.1) (years 4))
			(list "bos-indicus" 'bovine 'B (* grscale 0.0007) (* mrscale 0.1) (years 6))
			(list "pig" 'pig 'p (* grscale 0.07) (* mrscale 0.003) (years 12))
			(list "velociraptor" 'predator 'V (* grscale 0.0006) (* mrscale 0.3) (years 14))
			)
		)
	)

(define V-predation-matrix 
  (if #f
		;; Defines the basic predatory interaction rates between species
		(make-matrix
		 ;;                       PREY
		 ;; Predation by the organism
		 ;;(G     h    m     e    w     r   s  g  b    d   B   p   V)    ;;   PREDATORS
		 '((0     0    0     0    0     0   0  0  0    0   0   0   0)    ;; G    P  o  O 
			(0     0    0     0    0     0   0  0  0    0   0   0   0)    ;; h    r  n  r
			(0     0    0     0    0     0   0  0  0    0   0   0   0)    ;; m    e     a
			(0     0    0     0    0     0   0  0  0    0   0   0   0)    ;; e    d  t  n
			(0     0    0     0    0     0   0  0  0    0   0   0   0)    ;; w    a  h  i
			(.5   .5   .01   .01   .01   0   0  0  0    0   0   0   0)    ;; r    t  e  s
			(.5  .5  .1   .01   .01   0   0  0  0    0   0   0   0)    ;; s    i     m
			(.21 .22 .02   .02   .02   0   0  0  0    0   0   0   0)    ;; g    o      
			(.18 .14 .03   .03   .03   0   0  0  0    0   0   0   0)    ;; b    i
			(.23 .25 .02   .02   .02   0   0  0  0    0   0   0   0)    ;; d    n
			(.18 .14 .03   .03   .03   0   0  0  0    0   0   0   0)    ;; B    
			(.25 .27 .007  .007 .007 .01   .3 .2 .13  .02 .02 .05 .05)  ;; p          
			(0    0    0     0    .15  .3    .3 .3 .3   .3  .3  .3  .05)  ;; V    
			)
		 )
		(make-matrix 
		 (copy-list (make-list 13 (make-list 13 0)))
		 )
		)
  )

(define V-efficiency-matrix
  (make-matrix 
	(copy-list (make-list 13 (make-list 13 0.3)))))


(define (pasture-or-hay p) (if (< (random-real) p) "pasture" "hay-crop"))

(define V-rich-population
  (make-population-structure 

	;; Defines the basic predatory interaction rates between species
	 V-predation-matrix

	;; Defines the basic predatory interaction rates between species
	 V-efficiency-matrix

	;; Basic species data (name, type growth rate, mortality rate, age at "max mass"
	V-species-individuals

	;; Patch template for population level data.  "r" is the "sharpness" parameter for sigmoidal growth
	(list 
	 (list 0.2   ;prob  name   class   MaxCap   CurVal dt r dTmax
			 (list 0.025 "myrtle" 'tree 300100 30100 3 (years 180))
			 (list 0.8 "wattle" 'tree 200100 20100 3 1 (years 30))
			 (list 0.9 (pasture-or-hay 0.3) 'grass 1400100 140100 3 1 (years 0.3))
			 (list 0.3 "rabbit" 'rabbit 2010 210 3 1 (years 0.8))
			 (list 0.5 "sheep" 'caprid 10100 1100 3 1 (years 13))
			 (list 0.5 "goat" 'caprid 6010 610 3 1 (years 14))
			 )
	 (list 0.2
			 (list 0.8 "wattle" 'tree 300200 30100 3 1 (years 160))
			 (list 0.9 (pasture-or-hay 0.7) 'grass 1400200 140200 3 1 (years 0.28))
			 (list 0.45 "rabbit" 'rabbit 2200 220 3 1 (years 0.78))
			 (list 0.5 "sheep" 'caprid 10200 1200 3 1 (years 12))
			 (list 0.5 "goat" 'caprid 6020 620 3 1 (years 13))
			 )
	 (list 0.2
			 (list 0.3 "wattle" 'tree 200300 20300 3 1 (years 170))
			 (list 0.5 "gum" 'tree 200300 20300 3 1 (years 280))
			 (list 0.9 (pasture-or-hay 0.5) 'grass 1400300 140300 3 1 (years 0.3))
			 (list 0.3 "rabbit" 'rabbit 2030 230 3 1 (years 0.8))
			 (list 0.85 "dairy-cattle" 'bovine 2030 230 3 1 (years 4))
			 )
	 (list 0.2
			 (list 0.8 "wattle" 'tree 200400 20400 3 1 (years 0.160))
			 (list 0.9 (pasture-or-hay 0.9) 'grass 1400400 140400 3 1 (years 0.8))
			 (list 0.1 "rabbit" 'rabbit 2040 240 3 1 (years 0.3))
			 (list 0.825 "beef-cattle" 'bovine 3040 340 3 1 (years 5))
			 (list 0.5 "Bos-indicus" 'bovine 2040 240 3 1 (years 6))
			 )
	 (list 0.2
			 (list 0.8 "wattle" 'tree 200500 20500 3 1 (years 150))
			 (list 0.9 (pasture-or-hay 0.9) 'grass 1400500 140500 3 1 (years 0.4))
			 (list 0.7 "rabbit" 'rabbit 2050 250 3 1 (years 0.28))
			 (list 0.8 "Bos-indicus" 'bovine 2050 250 3 1 (years 6))
			 )
	 (list 0.2
			 (list 0.9 (pasture-or-hay 0.05) 'grass 20006000 2006000 3 1 (years 0.4))
			 (list 0.8 "rabbit" 'rabbit 6060 660 3 1 (years 0.3))
			 (list 0.8 "goat" 'caprid 166 16 3 1 (years 12))
			 )
	 (list 0.2
			 (list 0.025 "myrtle" 'tree 30007000 3007000 3 1 (years 180))
			 (list 0.125 "gum" 'tree 8070 870 3 1 (years 220))
			 (list 0.8 "wattle" 'tree 20007000 2007000 3 1 (years 80))
			 (list 0.9 (pasture-or-hay 0.95) 'grass 140700 14700 3 1 (years 0.3))
			 (list 0.9 "rabbit" 'rabbit 2070 270 3 1 (years 0.3))
			 (list 0.5 "pig" 'pig 1070 170  3 1 (years 20))
			 )
	 (list 0.2
			 (list 0.025 "myrtle" 'tree 30008000 3008000 3 1 (years 150))
			 (list 0.125 "gum" 'tree 8080 880 3 1 (years 200))
			 (list 0.8 "wattle" 'tree 20008000 2008000 3 1 (years 70))
			 (list 0.8 "pig" 'pig 1080 180 3 1 (years 15))
			 )
	 ))
  )


(define V-sparse-population
  (make-population-structure
	
	;; Defines the basic predatory interaction rates between species
	V-predation-matrix

	;; Defines the basic predatory interaction rates between species
	 V-efficiency-matrix

	;; Basic species data (name, type growth rate, mortality rate, age at "max mass"
	V-species-individuals

	;; Patch template for population level data
	(list
	 (list 0.5  ;prob  name   class   MaxCap   CurVal dt r dTmax
			 (list 0.025 "myrtle" 'tree 30000 3000 3 1 (years 180))
			 (list 0.8 "wattle" 'tree 200000 20000 3 1 (years 50))
			 (list 0.9 (pasture-or-hay 0.5) 'grass 1400000 140000 3 1 (years 0.3))
			 (list 0.3 "rabbit" 'rabbit 2000 200 3 1 (years 0.8))
			 (list 0.5 "sheep" 'caprid 10000 1000 3 1 (years 12))
			 )
	 (list 0.5
			 (list 0.8 "wattle" 'tree 310000 31000 3 1 (years 75))
			 (list 0.9 (pasture-or-hay 0.5) 'grass 1410000 141000 3 1 (years 0.4))
			 (list 0.45 "rabbit" 'rabbit 6100 610 3 1 (years 0.85))
			 (list 0.5 "goat" 'caprid 6100 610 3 1 (years 14))
			 )
	 (list 0.5
			 (list 0.3 "wattle" 'tree 220000 22000 3 1 (years 40))
			 (list 0.5 "gum" 'tree 220000 22000 3 1 (years 220))
			 (list 0.9 (pasture-or-hay 0.5) 'grass 1420000 142000 3 1 (years 0.3))
			 (list 0.1 "rabbit" 'rabbit 2200 220 3 1 (years 0.8))
			 (list 0.25 "dairy-cattle" 'bovine 23000 2300 3 1 (years 6))
			 )
	 ))
  )

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
