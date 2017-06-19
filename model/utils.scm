;-  Identification and Changes

;--
;	utils.scm -- Written by Randall Gray 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 
;;(load "constants.scm")

;;(define (guess-type ob)
;;  (let ((s (make-string 200)))
;;	 (with-output-to-string s (lambda () (display ob)))
;;	 s)
;;  )

(define (scaleT T)
  (* 0.1 
  (cond
	((< T 600) T)
	((< T hour) (/ T 600))
	((< T day) (/ T hours)) 
	((< T week) (/ T days))
	((< T year) (/ T weeks))
	(else (/ T years))
	)))

(define (scaleTS T)
  (cond
	((< T 600) "secs")
	((< T hour) "mins")
	((< T day) "hours")
	((< T week) "days")
	((< T year) "weeks")
	(else "years")
  ))

(define (scaled-time T)
  (string-append ((if SLIB (lambda (x) (sprintf "%.2f" x)) number->string) (scaleT T)) " " (scaleTS T)))

(define (scaled-time-ratio T E)
  (string-append ((if SLIB (lambda (x) (sprintf "%.2f" x)) number->string) (/ (scaleT T) (scaleT E))) " " (scaleTS T)"/"(scaleTS E)))

(define (pp-to-string p)
  (with-output-to-string '()
								 (lambda ()
									(pp p))))

(define (atom? x)
  (not (pair? x)))

;;(define (cddddr x) (cdr (cdddr x)))
;;(define (cadddr x) (car (cdddr x)))
;(define (caddddr x) (car (cdddr x)))
(define (caddddr x) (list-ref x 4))
;(define (cdddddr x) (cdr (cdddr x)))
(define (cdddddr x) (list-tail x 5))
;(define (cadddddr x) (car (cddddddr x)))
(define (cadddddr x) (list-ref x 5))
;(define (cddddddr x) (cdr (cddddddr x)))
(define (cddddddr x) (list-tail x 6))
;(define (caddddddr x) (car (cdddddddr x)))
(define (caddddddr x) (list-ref x 6))
;(define (cdddddddr x) (cdr (cdddddddr x)))
(define (cdddddddr x) (list-tail x 7))
;(define (cadddddddr x) (car (cddddddddr x)))
(define (cadddddddr x) (list-ref x 7))
;(define (cddddddddr x) (cdr (cddddddddr x)))
(define (cddddddddr x) (list-tail x 8))
;(define (caddddddddr x) (car (cddddddddr x)))
(define (caddddddddr x) (list-ref x 8))
;(define (cdddddddddr x) (cdr (cdddddddddr x)))
(define (cdddddddddr x) (list-ref x 9))

(define (listify a)
  (if (list? a) a (list a)))

;; This next is probably quite gambit specific w.r.t. procedures

(define (force-list l)
  (if (not (list? l))
		(list l)
		l))

(define (object-type ob)
  (cond 
	((equal? ob #!void) '(void))
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

		(if (equal? (car lst) #\#)
			 (list 'procedure (list->string (cdr lst)))
			 (list 'procedure (string->symbol (list->string lst))))
		))
	((atom? ob) '(atom unknown))
	(else 'unknown)
	)
  )

;---- (list-intersection a b) intersection of lists a and b
(define (list-intersection A B)
  (if (not (pair? A)) (set! A (list A)))
  (if (not (pair? B)) (set! A (list B)))
  (let ((f (filter (lambda (x) (member x B)) A)))
	 (if (null? f)
		  '()
		  f)))

;---- (list-intersection* a b) intersection 
(define (list-intersection* . args )
  (case (length args)
	 ((0) '())
	 ((1) (car args))
	 ((2) (list-intersection (car args) (cadr args)))
	 (else (list-intersection (car args) (apply list-intersection* (cdr args))))))

;---- (list-intersection? op a b) intersection of lists a and b
(define (list-intersection? op selector A B)
  (if (not (pair? A)) (set! A (list A)))
  (if (not (pair? B)) (set! A (list B)))
  (let ((f (filter (lambda (x) (op (selector x) (map selector B))) A))
		  (g (filter (lambda (x) (op (selector x) (map selector A))) B)))
	 (union+ f g)))

;---- (list-intersection* a b) intersection of lists 
(define (list-intersection?* op selector . args )
  (let ((li?* (lambda x (apply list-intersection?* (cons op (cons selector x))))))
	 (case (length args)
		((0) '())
		((1) (car args))
		((2) (list-intersection? op selector (car args) (cadr args)))
		(else (list-intersection? op selector (car args) (apply li?* (cdr args)))))))




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
	 (if (= (length ps) (length class-weights))	
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
(define (close f) (close-port f))

(define (make-list% n . init)
  (if (<= n 0) 
		'()
		(if (null? init)
			 (cons '() (make-list% (- n 1)))
			 (cons (car init) (make-list% (- n 1) (car init))))))


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



;; surjections onto subspaces for lists representing vectors
(define (txyz->xy x) (list-head (cdr x) 2))
(define (txyz->xyz x) (cdr x))

(define (txyz->t x) (car x))
(define (txyz->x x) (cadr x))
(define (txyz->y x) (caddr x))
(define (txyz->z x) (cadddr x))

(define (o->s x) (if (string? x) x (object->string x)))

(define (random-location minv maxv)
  (map + (map * (map - maxv minv) (map (lambda (x) (random-real)) (make-list% (length minv)))) minv))

(define (nrandom mean . its) ;; very dodgey ... but bog simple
  (let ((N (if (null? its) 100 (car its))))
	 (let loop ((i 0) (the-sum 0))
		(if (>= i N)
			 (* (/ mean (/ N 2.0)) the-sum)
			 (loop ((+ 1  i) (+ the-sum (random-real))))))))


(define (simple-general-sigmoid x y) (/ 1.0 (+ 1.0  (exp (- (* x y))))))
;
;;(define sigmoid (lambda (x) (general-sigmoid x 1.0)))

;; (require 'charplot) ; from slib
;; (plot (lambda (p) (general-sigmoid p 0.04)) -100.0 100.0 200)

;(define sigmoid*
;  (let* ((bs (lambda (x) (/ 1.0 (+ 1.0 (exp (* -2 pi (- (* 2.0 x) 1.0)))))))
; 			(m (bs 0.0))
; 			(M (bs 1.0))
;			(r (- M m)))
; 	 (lambda (x)
; 		(max 0.0 (min 1.0 (/ (- (bs x) m) r))))))
;
;;; maps R->[0,1]
;(define (inverse-sigmoid x)
;  (cond 
; 	((>= x 1) 1)
; 	((<= x 0) 0)
; 	(else (max 0.0 (min 1.0 (+ (/ (- (log x) (log (- 1 x))) (* 4 pi)) 0.5))))))




(define (general-biomass-growth-rate x peak width scale y) 
  (* scale 
	  (- 
		(simple-general-sigmoid (+ (- x peak) width) y)
		(simple-general-sigmoid (- (- x peak) width) y))))

(define (dP/dt P dt r K) (* r P (- 1.0 (/ P K))))0


(define (aborts . args)
  (abort (apply string-append (map o->s args))))

;; A sequence of integers
;; (seq 2) => (0 1) 
;; (seq 0) => ()
;; (seq -2) => (-1 0)
(define (seq n)
  (define (rseq n) (if (<= n 0) '() (cons (- n 1) (rseq (- n 1)))))
  (if (< n 0) (map - (rseq (- n))) (reverse (rseq n))))



;; returns the elements with "even" indices
;; (evens '(a b c d)) => (a c)
(define (evens* lst)
  (map (lambda (x) (list-ref lst x)) (filter even? (seq (length lst)))))

;; returns the elements with "odd" indices
;; (odds '(a b c d e) => (b d)
(define (odds* lst)
  (map (lambda (x) (list-ref lst x)) (filter odd? (seq (length lst)))))

;; These are faster, but not robust w.r.t. line length
(define (evens lst) (if (null? lst) '() (cons (car lst) (if (pair? (cdr lst)) (evens (cddr lst)) '()))))
(define (odds lst) (if (null? lst) '() (if (pair? (cdr lst)) (evens (cdr lst)) '())))

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

(define (n-arity-from-2-arity op identity)
  (lambda args
	 (if (not (list? args)) (error "bad list to generalised operator" args))
	 (let loop ((a args)
					(r identity))
		(cond
		 ((null? a) r)
		 ((= (length a) 1) (op (car a) r))
		 (#t (loop (cdr a) (op (car a) r)))))))

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

;; *** this is wonky ***
;; return the cross product of n lists (state spaces)  
(define (cross__ . args)
  (define (cross2 a b)
	 (apply append (map (lambda (x) (map (lambda (y) 
														(if (list? y)
															 (cons x y)
															 (list x y))) b)) a)))
  (cond
	((not (list? args)) (error "bad argument" args))
	((null? args) '())
	((= (length args) 1)
	 (car args))
	((= (length args) 2)
	 (apply cross2 args))
	(#t (cross__ (car args) (apply cross__ (cdr args))))))



(define (cross* . args)
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
	(#t (cross* (car args) (apply cross* (cdr args))))))


; ( a b (c d e) f g)
; needs to (conceptually) map to
; a b c f g
; a b d f g
; a b e f g


(define (iteration-list lst)
  (seq (length lst)))

(define (iteration-list* lst)
  (generate-iteration-list lst))


(define (rotate-list direction l)
  (case direction
	 ((cw clockwise) (append (cdr l) (cons (car l) '())))
	 ((ccw counter-clockwise) (let ((n (- (length l) 1))) (append (list-tail l n) (list-head l n))))
	 (else (error "Unrecognised direction, should be cw or ccw" direction))))
	 
  
		

;-- (**-map (lambda (oblst/ob) ...) ll*ist) -- preserves list structure
(define (**-map L ll*ist)
  (if (null? ll*ist)
		'()
		(cons 
		 (if (atom? (car ll*ist))
			  (L (car ll*ist))
			  (**-map L (car ll*ist)))
		 (**-map L (cdr ll*ist)))))


;-- (map-**-ix (lambda (oblst itlst) ..) objlst) 
;; returns a list (with a depth of one) where the function has been applied to each element
;; and the indices for each element are available to the processing function, l.

(define (map-**-ix l oblst)
  (let* ((itl (iteration-list* oblst)) ; generates indices for stepping through objlst
			(obl (map (lambda (x) (list-ref* oblst x)) itl)))
	 (map l obl itl)))

;;; (map-** (lambda (x i) (dnl* "x" x "--" "i" i)) '((a b c d) (e f g h) (i j k l)))
;;; x a -- i (0 0)
;;; x b -- i (0 1)
;;; x c -- i (0 2)
;;; x d -- i (0 3)
;;; x e -- i (1 0)
;;; x f -- i (1 1)
;;; x g -- i (1 2)
;;; x h -- i (1 3)
;;; x i -- i (2 0)
;;; x j -- i (2 1)
;;; x k -- i (2 2)
;;; x l -- i (2 3)
;;; (#!void #!void #!void #!void #!void #!void #!void #!void #!void #!void #!void #!void)

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

;; Like list-set! but for a-lists
;; This is a mutator -- the list needs to exist (have at least one entry) for it to work, though.
(define (assq-set! alist key val)
  (let loop ((l alist))
	 (if (null? l) 
		  (append alist (cons (cons key val) '()))
		  (if (eqv? (caar l) key)
				(set-cdr! (car l) val)
				(loop (cdr l))))))

;; This should be called like (set! lst (assq-append lst ...))
(define (assq-append alist key value)
  (if (or (null? alist) (not (assq key alist)))
		(acons key (list value) alist)
		(map (lambda (x)
				 (if (and (pair? x) (eqv? (car x) key))
					  (append x (list value))
					  x))
			  alist)
		))

;; This should be called like (set! lst (assq-delete lst ...))
(define (assq-delete alist key)
  (reverse (let loop ((a alist)(r '()))
				 (if (null? a)
					  r
					  (if (and (pair? a) (pair? (car a)) (eqv? (caar a) key))
							(loop (cdr a) r)
							(loop (cdr a) (cons (car a) r)))))))

(define (member-assq k lst)
  (let ((n (length lst))
		  (m (member k (map car lst))))
	 (if m
		  (list-tail lst (- n (length m)))
		  #f)))

;; Commented out only to avoid multiple definitions
;;; (define (list2-assq-set! k v k2 v2) 
;;;   (let loop ((kl k) (vl v))
;;; 	 (cond ((or (null? kl) (null? vl)) #f)
;;; 			 ((eqv? (car kl) k2) (set-car! vl v2))
;;; 			 (else (loop (cdr kl) (cdr vl))))))

;;; (define (list2-assq k v k2) 
;;;   (let loop ((kl k) (vl v))
;;; 	 (cond ((or (null? kl) (null? vl)) #f)
;;; 			 ((eqv? (car kl) k2) vl)
;;; 			 (else (loop (cdr kl) (cdr vl))))))

;;(define-macro (++ i) `(let ((j ,i)) (set! ,i (+ 1 j))))
;;(define-macro (-- i) `(let ((j ,i)) (set! ,i (+ -1 j))))

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


(define (map-apply proc lst)
  (map (lambda (x) (apply proc x)) lst))


;; THIS DOES NOT QUITE CONFORM TO make-list!!!: (make-list* '(1 2 3) 9) => (((9 9 9) (9 9 9)))
(define (make-list* . dims)
  (let ((defval 0))
	 (if (and (pair? dims) (pair? (car dims))) 
		  (begin
			 (if (pair? (cdr dims))
				  (set! defval (cadr dims)))

			  (set! dims (car dims))))

		  (if (null? (cdr dims))
				(make-list% (car dims) defval)
				(map (lambda (x) (make-list* (cdr dims) defval)) (make-list% (car dims) ))))
)
(define (make-list** dims defval) ;; This expects defval to either be a "thing" or a procedure of no arguments
  (if (null? (cdr dims))
		(map (if (procedure? defval) (lambda x (defval)) defval) (make-list% (car dims)))
		(map (lambda (x) (make-list** (cdr dims) defval)) (make-list% (car dims))))
)

(define (depth* l)
   (let loop ((tl l) (d 0))
     (if (not (pair? tl))
         d
         (max (loop (car tl) (+ d 1)) (loop (cdr tl) d)))))

(define (length* l)
  (if (not (pair? l))
		0
		(apply max (map length* l))))

;;(define (simple-list? lst)
;;  (or (null? lst) 
;;		(and (pair? lst) 
;;			  (atom? (car lst)) 
;;			  (simple-list? (cdr lst)))))


;; (map% (lambda (v l d) (+ (random-integer 42) (* d 10) (* +1.0i l))) '((a b (c (d e) f)) (g h i) ((j k) l)))

(define (map% proc list* . d) ;; proc is of the form (lambda (listval index deepness) ...)
  (set! d (if (null? d) 0 (car d)))
  
  (if (simple-list? list*)
		(map proc list* (seq (length list*)) (make-list% (length list*) d))
		(map (lambda (l* l d)
				 (if (atom? l*)
					  (proc l* (if (list? l) (car l) l)  (if (list? d) (car d) d))
					  (map% proc l* d))
				 )
			  list* (seq (length list*)) (make-list% (length list*) d))
		)
  )



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
		(if (equal? (car l) (cadr l))
			 #f
			 (uniq? (cdr l)))))

(define (uniq lst) ;; returns the unique elements w.r.t. eq?: should be sorted
  (cond
	((null? lst) '())
	((null? (cdr lst)) lst)
	((not (equal? (car lst) (cadr lst))) (cons (car lst) (uniq (cdr lst))))
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
(define (not-memq lst) (lambda (x) (not (memq x  lst))))



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
		(list-set! (cdr l) (- i 1) v)))


(define list-set-car! list-set!)

(define (list-set-cdr! l i v)
  (if (zero? i)
		(set-cdr! l v)
		(list-set-cdr! (cdr l) (- i 1) v)))
;; set the value associated with key in a-list
;; (assoc-set! list key value)



;; list-ref that accepts a list of indices
;; (list-ref '(1 2 3 4 5) '(0 3)) => (1 4)
(define list-ref%
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


(define (list-ref*? lst ix)
  (cond
	((and (number? ix) (<= 0 ix) (< ix (length lst)))
	 #t)
	((and (pair? lst) (number? (car ix)) (<= 0 (car ix)) (< (car ix) (length lst)))
	 (list-ref*? (list-ref lst (car ix)) (cdr ix)))
	((and (null? lst) (not (null? ix))) #f)
	((and (null? ix) (not (null? lst))) #t)
	(#t #t)))

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

(define (list-ref* lst ix)
  (cond
	((and (list? ix) (= 1 (length ix))) (list-ref lst (car ix)))
	((and (number? ix) (<= 0 ix) (< ix (length lst)))
	 (list-ref lst ix))
	((and (pair? lst) (number? (car ix)) (<= 0 (car ix)) (< (car ix) (length lst)))
	 (list-ref* (list-ref lst (car ix)) (cdr ix)))
	((and (null? lst) (not (null? ix))) (error "Index list ran off the end of the list" ix))
	((and (null? ix) (not (null? lst))) lst)
	(#t (error "Should never get here, bad arguments" lst ix))))


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
				  (list-set* (map (lambda (x) (list-ref* x (car ix)) lst) (cdr ix)) vv)
				  (abort "The value list does not have the indicated number of elements"))))
	 )))



;; this expects a list like '(a b c (d (e f) g (h i (j k l)) m) n o p ((q r) s)  t) and constructs an list of
;; index values that iterate through the list comprehensively -- see the commented bits for blart and blartix 


(define (generate-iteration-list L)
  (let ((S (seq (length L))))
	 (apply append (map (lambda (x s)
								 (cond
								  ((null? (list-ref L s)) (list s))
								  ((atom? (list-ref L s)) (list (list s)))
								  ((list? (list-ref L s))
									(let ((sl (generate-iteration-list (list-ref L s))))
									  (map (lambda (y) (append (list s) (if (list? y) y (list y)))) sl)))
								  (#t (error "bad mojo in generate-iteration-list" L))))
							  L S))))
						  

;;; ;;  Valid fully resolved index lists for this include
;;; (define blartix '((0)
;;; 						(1)
;;; 						(2)
;;; 						(3 0)
;;; 						(3 1 0)
;;; 						(3 1 1)
;;; 						(3 2)
;;; 						(3 3 0)
;;; 						(3 3 1)
;;; 						(3 3 2 0)
;;; 						(3 3 2 1)
;;; 						(3 3 2 2)
;;; 						(3 4)
;;; 						(4)
;;; 						(5)
;;; 						(6)
;;; 						(7 0 0)
;;; 						(7 0 1)
;;; 						(7 1)
;;; 						(8)))

;;; (define blart '(a b c
;;; 						(d
;;; 						 (e f)
;;; 						 g
;;; 						 (h i
;;; 							 (j k l))
;;; 						 m)
;;; 						n o p
;;; 						((q r)
;;; 						 s)
;;; 						t)
;;;   )




;;
;; return the first k elements of a list (analogous to list-tail)
;;
(define (list-head the-list k)
  (if (and (> k 0) (not (null? the-list)))
		(cons (car the-list) (list-head (cdr the-list) (- k 1)))
		'()))


;; remove an object from a list
(define (remove-element obj lst)
  (letrec ((head (list '*head*)))
	 (letrec ((inner-remove
				  (lambda (lst tail)
					 (cond ((null? lst)
							  lst)
							 ((not (pair? lst))
							  #f)
							 ((eqv? obj (car lst)) (inner-remove (cdr lst) tail))
							 (#t
							  (set-cdr! tail (list (car lst)))
							  (inner-remove (cdr lst) (cdr tail)))))))
		(inner-remove lst head))
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





"
Given two circles of radii r and R respectively, and a distance
between them of d, we get the area of intersection, A, to be the value calculated below.
Ref:  Weisstein, Eric W., 'Circle-circle intersection.' from 
Mathworld, http://MathWorld.wolfram.com/Circle-CircleIntersection.html 2010-06-30
"

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

"
The decay function determines what proportion of a population is available to predation: a value of
0.01 indicates that only one percent of the prey population is accessible to the predator.  The
behaviour of the function is governed by the radius of the population and a distance_decay which
controls how quickly the proportion decreases with distance.  When the distance_decay is 1.0 the
proportion available is about 0.01 at a distance equal to the radius of the population.  If the
distance_decay is in (0,...], then the distance at which we get an available proportion of 0.01 is
linearly related to the distance_decay (a distance_decay of 2 gives us a proportion of 1% at r/2 if
														r is the radius.  Clearly setting the distance_decay to a small enough value will make the
														spatially explicit version approximate the non-spatial version. If the distance_decay is equal to
														0, then a uniform distribution over the population's disk is assumed.

														For gnuplot:
														pi2 = pi*pi
														sqr(x) = (x*x)
														f(d, dd,r) = ((sqr(r/pi2) * sqr(1.0/dd) < 0.0005) ? (d > r ? 0.0 : (1.0/(pi * sqr(r/pi2)))) : (sqr(1.0/dd) * sqr(r/pi2) / (sqr(1.0/dd) * sqr(r/pi2) + sqr(d))))

														Note: the integral over this disk is 
														Integral(0,2pi, 
																	Integral(0, R, sqrt(distance_decay*notional_radius) * atan(r/sqrt(distance_decay*notional_radius)), d r), d theta)

														|#
"

(define (decay distance distance_decay notional_radius clip)
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
  (* (decay dist dd1 nr1 clip1) (decay dist dd2 nr2 clip2)))

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
	  (else (append (list (car loc) (cadr loc)) (make-list% (- l 2) 0))))))

;; First ordinate is the "base" altitude the second is the slope along "x" the third the slope along "y"
;; the value returned is the altitude.
;; (length coeff) should be (+ 1  (length loc))
(define (plane coeff)
  (lambda (loc)
	 (+ (car coeff) (apply + (map * (prj-z=0 loc) (prj-z=0 (cdr coeff)))))))




;; used in constructing filenames with sequence numbers
(define (pno n . k)
  (let* ((ns (number->string (/ (truncate (* 100.0 n)) 100.0)))
			(rs (reverse (string->list ns)))
			(n (list->string (reverse (if (eq? (car rs) #\.) (cdr rs) rs))))
			)

	 (if (null? k)
		  n
		  (string-append (make-string (- (car k) (string-length n)) #\0) n))))


(define (string-contains? str . targets) ;; (string-contains? "The quick brown fox" "ox" "hen") ==> #t (string-contains? "The quick brown fox" "oxo" "hen") ==> #f
  (if (= (length targets) 1)
      (let* ((st (car targets))
	     (n (string-length st))
	     )
	(cond
	 ((< (string-length str) n) #f)
	 ((string=? (substring str 0 n) (substring st 0 n)) #t)
	 (#t (string-contains? (substring 1 (- (string-length str) 1)) st))))
      (let loop ((st targets))
	(cond
	 ((null? st) #f)
	 ((string-contains? str (car st)) #t)
	 (#t (loop (cdr st)))))))

 ;;
 ;; (strspn str set) returns index of first char not in set
 ;; (strcspn str set) returns index of first char in set
 ;;

 (define (strspn str set)
   (let loop ((s str))
     (if (zero? (string-length s))
         (string-length str)
         (if (let inner-loop ((chset set))
               (if (zero? (string-length chset))
                   #f
                   (if (equal? (string-ref s 0)
                            (string-ref chset 0))
                       #t
                       (inner-loop (substring chset 1 (string-length chset))))))
             (loop (substring s 1 (string-length s)))
             (- (string-length str) (string-length s))))))

 (define (strcspn str set)
   (let loop ((s str))
     (if (zero? (string-length s))
         (string-length str)
         (if (let inner-loop ((chset set))
               (if (zero? (string-length chset))
                   #t
                   (if (equal? (string-ref s 0)
                            (string-ref chset 0))
                       #f
                       (inner-loop (substring chset 1 (string-length chset))))))
             (loop (substring s 1 (string-length s)))
             (- (string-length str) (string-length s))))))



;; This silently collapses multiple instances of either spaces or the indicated separator
 (define (collapsing-strtok str . separator)
   (if (null? separator)
       (set! separator " ")
       (set! separator (car separator)))

   (if (string? str)
       (let loop ((results '())
                  (sstr str))
         (if (zero? (string-length sstr))
             results
             (if (zero? (strspn sstr separator))
                 (loop (append results (list (substring sstr 0 (strcspn sstr separator) )))
                       (substring sstr (strcspn sstr separator) (string-length sstr)))
                 (loop results
                       (substring sstr (strspn sstr separator) (string-length sstr)))))))
   )

;; This does not collapse multiple instances of either spaces or the indicated separator 
 (define (strtok str . separator)
   (if (null? separator)
       (set! separator " ")
       (set! separator (car separator)))

   (if (string? str)
       (let loop ((results '())
                  (sstr str))
         (if (zero? (string-length sstr))
             results
             (if (zero? (strspn sstr separator))
                 (loop (append results (list (substring sstr 0 (strcspn sstr separator) )))
                       (substring sstr (strcspn sstr separator) (string-length sstr)))

					  (loop (if (and (> (string-length sstr) 1) (zero? (strspn (substring sstr 1 (string-length sstr)) separator))) results (append results (list "")))
							  (substring sstr 1 (string-length sstr)))))))
   )



;; reconstructs the string either with spaces or the indicated separator

 (define (reconstruct-string strarray . separator)
   (if (null? separator)
       (set! separator " ")
       (set! separator (car separator)))

   (if (null? strarray)
       ""
       (let loop ((sa (cdr strarray))
                  (ns (car strarray)))
         (if (null? sa)
             ns
             (loop (cdr sa) (string-append ns separator (car sa)))))))

 ;;
 ;; Searching
 ;;


 ;; Depth first search
 (define (dfs node target nodemap carnode cdrnode)
   (if (null? node)
       #f
       (if (equal? (nodemap node) target)
           node
           (if (pair? node)
               (let ((l (dfs (carnode node) target nodemap carnode cdrnode)))
                 (if l
                     l
                     (dfs (cdrnode node) target nodemap carnode cdrnode)))
               #f))))


 (define (dfs-path node target nodemap carnode cdrnode)
   (if (or (null? node) (not node))
       #f
       (if (equal? (nodemap node) target)
           (list node)
           (if (not (pair? node))
               #f
               (letrec ((l (dfs-path (carnode node) target nodemap carnode cdrnode)))
                 (if l
                     (if (pair? l) 
                         (cons (car l) (cons 'car (cdr l)))
                         (cons l 'car))

                     (letrec ((r (dfs-path (cdrnode node) target nodemap carnode cdrnode)))
                       (if r
                           (if (pair? r)
                               (cons (car r) (cons 'cdr (cdr r)))
                               (cons r 'cdr))
                           #f))))
               ))))


 ;; breadth first search
 (define (bfs-list key list-of-lists)
   (let bsof ((lol (list-copy list-of-lists)))
     (if (or (not (pair? lol)) (null? lol))
         #f
         (if (equal? key (car lol))
             (car lol)
             (let ()
               (if (and (list? (car lol)) (list? lol))
                   (bsof (append (cdr lol) (car lol))) ; strip off a level of nesting
                   (bsof (cdr lol)) 
                   ) 
               )
             )
         )
     )
   )


 (define (bfs key list-of-lists unwrapper)
   (let ((uw #f))
     
     (if (not (procedure? unwrapper))
         (set! uw (lambda (x) x))
         (set! uw (lambda (x) (unwrapper x)))
         )
     
     (if (null? list-of-lists)
         #f
         (let bsof ((lol (list-copy list-of-lists)))
           (if (or (not (pair? lol)) (null? lol))
               #f
               (if (pair? (car lol)) 
                   (if (and (pair? lol) (equal? key (uw (car lol))))
                       (car lol)
                       (let ()
                         (if (and (list? (car lol)) (list? lol))
                             (bsof (append (cdr lol) (car lol))) ; strip off a level of nesting
                             (bsof (cdr lol)) 
                             ) 
                         )
                       )
                   (bsof (cdr lol)))
               )
           )
         )
     )
   )


 ;; String/number mappings
 (define (hms->h t)
   (let loop ((h 0)
              (d 1)
              (s (strtok t ":")))
     (if (null? s)
         h
         (loop (+ h (/ (string->number (car s)) d)) (* d 60) (cdr s)))))


 (define (glog b e) (/ (log e) (log b)))

;;;  (define (power b e)
;;;    (cond
;;;     ((< e 0) (if t '()))
;;;     ((zero? e) 1)
;;;     ((even? e) (power (* b b) (/ e 2)))
;;;     (t (* b (power b (- e 1))))))

;;; (define power
;;;   (let ((p power))
;;; 	 (lambda (x e)
;;; 		(cond
;;; 		 ((and (zero? x) (zero? e))
;;; 		  1) ;; because it is what it is, not a bug.
;;; 		 ((and (not (zero? x)) (zero? e))
;;; 		  1)
;;; 		 ((and (integer? e) (not (negative? e)))
;;; 		  (p x e))
;;; 		 ((and (integer? e) (negative? e))
;;; 		  (/ 1 (p x (* -1 e))))
;;; 		 ((complex? e) (exp (* e (log x))))
;;; 		 ))))


 (define (gexp b e) 
   (exp (* (log b) e)))


 ;; This creates an escape using a most unlikely environment variable name 
 ;; to see if we have finished loading
 (define getenv
   (let ((ge getenv))
     (lambda (str . thing)
       (if (string=? str " Is scheme.rc loaded? ")
           #t
           (if (null? thing) (ge str) (ge str (car thing)))))))
;)


;; (make-sequence (lambda (x) (* x x)) 1 20 2)
(define (make-sequence gen m Mx . step)
  (set! step (if (null? step) 1 (car step)))
  (if (< (* step Mx) (* step m)) 
      '() 
      (cons (gen m) (make-sequence gen (+ m step) Mx step))))


;-  The End ;;; ;;; 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";; "
;;; comment-end:"" 
;;; End:
