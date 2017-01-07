; -*- mode: scheme; -*-
;-  Identification and Changes

; Tree ring code.

;	History:

;  HERE: so string->node, string->children, children->string and node->string  all work.
;  node addition works (I think)
;  working on multiplication
;  need to test things like associativity, distribution, commutativity and the effect of
;  order of arguments
;  


;--  Copyright 

;--  Discussion 

"
So I need to preserve terms with a factor of zero -- they still carry information
which ought to be preserved.
"

"
This implements the arithmetic operations on the ring of trees.
Implemented assuming chibi-scheme/R7RS or a similar implementation.

Test data is kept at the end of the file, since we might use
constructors to generate it.
"

;-  Configuration stuff 

;; This is ugly, but works in Gambit-c

(define booboo 'all-good)
(define ERROR error)
(define (error str arg)
  (set! booboo arg)
  (bad str arg))

(define time-register '())
(define collecting-times #f)
(define tr-serial-number 0)

(define-macro (time-probe name . body)
  `(let* ((x_x_name ,name)
			(x_x_s (process-times))
			(x_x_n (let () ,@body))
			(x_x_f (process-times)))
	  (if collecting-times
			(let ((sn tr-serial-number))
			  (set! tr-serial-number (+ 1 tr-serial-number))
			  (if (null? time-register)
					(let ((r (real-time)))
					  (set! time-register (cons (list "origin" (list 0 0 r 'start) (list 0 0 r 'finish) (list 0 0 0 'difference sn)) time-register))))
			  (set! time-register (cons (list x_x_name ;; times columns are user system wallclock
														 ;; the rows are start, finish, difference
														 (append (f64vector->list x_x_s) (list 'start sn))
														 (append (f64vector->list x_x_f) (list 'finish sn))
														 (append (map - (f64vector->list x_x_f) (f64vector->list x_x_s)) (list 'difference sn))
														 )
												 time-register))))
	  x_x_n))

(define-macro (time-probe* name . body)
  `(begin
	  (if collecting-times
			(let ((sn tr-serial-number))
			  (set! tr-serial-number (+ tr-serial-number 1))
			  
			  (if (null? time-register)
					(let ((r (real-time)))
					  (set! time-register (cons (list "origin" (list 0 0 r 'origin sn)) time-register))))
			  
			  
			  (let* ((x_x_name ,name)
						(x_x_s (process-times))
						)
				 (set! time-register (cons (list "origin" (list 0 0 r 'origin sn)) time-register))
				 (let* ((x_x_n (let () ,@body))
						  (x_x_f (process-times)))
					(set! time-register (cons (list x_x_name ;; times columns are user system wallclock
															  ;; the rows are start, finish, difference
															  (append (map - (f64vector->list x_x_f) (f64vector->list x_x_s)) (list difference sn))
															  )
													  time-register))))
			  x_x_n))))

(define (reset-timer . arg)
  (if (pair? arg) (set! collecting-times (car arg)))
  (set! time-register '())
  (set! tr-serial-number 0)
  )

(define (only tag) (lambda (x) (string=? tag (car x))))

(define (cross** . args)
  (define (c2 a b)
	 (cross2 (car args) (cadr args)))

  (cond
	((null? args) '())
	((= (length args) 1)
	 (copy-list (car args)))
	((= (length args) 2)
	 (cross2 (car args) (cadr args)))
	(#t
	 (map (lambda (x) (cons (car x) (cadr x))) (cross2 (car args) (apply cross** (cdr args)))))
	))

(define (map* mapping . args)
  (map (lambda (args) (apply mapping args)) (apply cross** args)))


;; This converts a string to a list of symbols, numbers and lists.
(define (read-string str)
  (let* ((P (open-input-string str))
			(R (read P)))
	 (close-port P)
	 R))

(define (write-string lst)
  (with-output-to-string '() (lambda () (write lst))))



(define (timer-counts register)
  (let* ((n (length register))
			(bits (unique (map car register)))
			(split-bits (map (lambda (tag) (filter (only tag) register)) bits))
			)
	 
	 (dnl "Total number of records: " (length register))
	 (dnl "Total elapsed time: " (- (caddr (caddr (car register))) (caddr (cadr (list-ref register (- n 1))))))
	 (newline)
	 (for-each (lambda (i)
					 (dnl (list-ref bits i) " used "
							(apply + (map (lambda (x) (car (list-ref x 3)))
											  (list-ref split-bits i)))))
				  (seq (length bits)))
	 (dnl "\nNote that there is multiple counting going on with the times.")
	 ))


(define (timer-counts* register)
  (let* ((n (length register))
			(bits (unique (map car register)))
			(split-bits (map (lambda (tag) (filter (only tag) register)) bits))
			)
	 
	 (dnl "Total number of records: " (length register))
	 (dnl "Total elapsed time: " (- (caddr (caddr (car register))) (caddr (cadr (list-ref register (- n 1))))))
	 (newline)
	 (for-each (lambda (i)
					 (dnl (list-ref bits i) " used "
							(apply + (map (lambda (x) (car (list-ref x 3)))
											  (list-ref split-bits i)))))
				  (seq (length bits)))
	 (dnl "\nNote that there is multiple counting going on with the times.")
	 ))


;--  Included files 


(define tr-debugging #f)

;-  Code 

(load "maths.scm") ;; Includes utils.scm
(load "crossproduct.scm")

(define (power b e) ;; also defined in maths.scm
  (cond
	((< e 0) (/ 1 (power b (- e))))
	((zero? e) 1)

	((and (integer? e) (rational? b)) ;; This will keep them as exact numbers if they are exact
	 (cond
	  ((even? e) (power (* b b) (/ e 2)))
	  (t (* b (power b (- e 1)))))
	 )
	(else (exp (* e (log b))))))

(define (type? x)
  (cond
	((boolean? x) 'boolean)
	((null? x) 'null)
	((symbol? x) 'symbol)
	((number? x) 'number)
	((polynomial? x) 'polynomial)
	((unique-node? x) 'unique-node)
	((node? x) 'node)
	((children? x) 'children)
	((list? x) 'list)
	((pair? x) 'pair)
	((procedure? x) 'procedure)
	((char? x) 'char)))
	
(define (Type? x)
  (let ((t (type? x)))
	 (if (eq? t 'node) 'tree t)))

;-- Supporting code: string and list utilities, simple polynomial arithmetic
;--- Mostly convenience functions, utilities, and mathematical functions
;---- display with implicit newline
(define (dnl . args) (if (null? args) (display "") (let () (map display args) (newline))))
(define (DNL . args) (if debugging (apply dnl args)))

;---- (ddnl . args) a version of dnl that only works if tr-debugging is true
(define (ddnl . args)
  (if tr-debugging
		(apply dnl args)))

;---- Define a random-integer that accepts a "0" argument

;---- Identity function
(define I (lambda (x) x))
(define !I (lambda (x) (not x)))

(let ((ri random-integer))
  (set! random-integer
		  (lambda (x)
			 (if (and (integer? x) (positive? x)) (ri x)
				  0))))

(define (andf . args)
  (if (null? args)
		#t
		(and (car args) (apply andf (cdr args)))))

(define (orf . args)
  (if (null? args)
		#f
		(or (car args) (apply orf (cdr args)))))

;--- list routines, filter and map routines 

;---- List utilities

(define (n-arity-from-2-arity op identity)
  (lambda args
	 (if (not (list? args)) (error "bad list to generalised operator" args))
	 (let arity-loop ((a args)
					(r identity))
		(cond
		 ((null? a) r)
		 ((= (length a) 1) (op (car a) r))
		 (#t
		  ;; (dnl 'n-arity-from-2-arity " (" op " " (car a) " " r ") and " (cdr a))
		  (arity-loop (cdr a) (op (car a) r)))))))



;----- merge-sort from RosettaCode.org
(define (merge-sort l gt?) ;; From Rosetta code
  (define (merge left right)
    (cond
     ((null? left)
      right)
     ((null? right)
      left)
     ((gt? (car left) (car right))
      (cons (car right)
            (merge left (cdr right))))
     (else
      (cons (car left)
            (merge (cdr left) right)))))
  (define (take l n)
    (if (zero? n)
		  (list)
		  (cons (car l)
				  (take (cdr l) (- n 1)))))
  (let ((half (quotient (length l) 2)))
    (if (zero? half)
		  l
		  (merge (merge-sort (take      l half) gt?)
					(merge-sort (list-tail l half) gt?)))))

;----- (flatten-list l) removes all nesting
(define (flatten-list lst)
  (let flatten-loop ((l lst)
				 (r '()))
	 (cond
	  ((null? l) (reverse r))
	  ((not (pair? (car l)))
		(flatten-loop (cdr l)
				(cons (car l) r)))
	  ((and (pair? l) (pair? (car l)))
		(flatten-loop (cdr l)
				(append (reverse (flatten-list (car l))) r)))
	  ((pair? l)
		(flatten-loop (cdr l) (cons (car l) r)))
	  (#t 'boink)
	  )
	 ))


;----- (flatten-singletons lst) converts (flatten-singletons '(a)) -> '(a) ; (flatten-singletons '((a) (b) (c d) e)) -> (a b (c d) e)
(define (flatten-singletons lst)
  (cond
	((not (pair? lst)) lst)
	((and (pair? lst) (pair? (car lst)) (null? (cdr lst))) ;; This may be wrong for what I want....
	 (car lst))
	((and (pair? lst) (pair? (car lst)) (null? (cdar lst)))
	 (cons (caar lst) (flatten-singletons (cdr lst))))
	(#t (cons (flatten-singletons (car lst)) (flatten-singletons (cdr lst))))))

;----- (unique lst) returns a list containing only the unique elements

(define (unique lst)
  (let uniq-loop ((l lst)
				 (r '()))
	 (if (null? l)
		  (reverse r)
		  (if (not (member (car l) r))
				(uniq-loop (cdr l) (cons (car l) r))
				(uniq-loop (cdr l) r)))))

;----- (!filter selector lst) -- returns a list of those elements which fail the selector

(define (!filter selector lst)
  (filter (lambda x (not (apply selector x))) lst))


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



;---- String utilities
;----- (string-head s n (string? integer?)) returns the first part of a string
(define (string-head s n)
  (let ((N (string-length s)))
	 (cond
	  ((zero? n) "")
	  ((< n N) (substring s 0 n))
	  (#t s))))

;----- (string-tail s n (string? integer?)) returns the last part of a string
(define (string-tail s n)
  (let* ((N (string-length s))
			(k (- N n)))
	 (cond
	  ((zero? n) "")

	  ((and (>= k 0) < k n)(substring s k N))
	  (#t s))
	 ))

;----- (string-car s) like car, but for strings
(define (string-car s)
  (string-head s 1))

;----- (string-cdr s) like cdr, but for strings
(define (string-cdr s)
  (string-tail s (- (string-length s) 1)))

(define (string-contains? str . targets) ;; (string-contains? "The quick brown fox" "ox" "hen") ==> #t (string-contains? "The quick brown fox" "oxo" "hen") ==> #f
  (if (= (length targets) 1)
      (let* ((st (car targets))
				 (n (string-length st))
				 )
		  (cond
			((< (string-length str) n) #f)
			((string=? (substring str 0 n) (substring st 0 n)) #t)
			(#t (string-contains? (substring str 1 (- (string-length str) 1)) st))))
      (let strcont-loop ((st targets))
		  (cond
			((null? st) #f)
			((string-contains? str (car st)) #t)
			(#t (strcont-loop (cdr st)))))))

(define (locate-substring str substr)
  (let ((n (string-length substr))
		  (N (string-length str))
		  )
	 (let locatestr-loop ((k 0))
		(cond
		 ((< N (+ k n) ) #f)
		 ((string=? (substring str k (+ k n)) substr) k)
		 (#t (locatestr-loop (+ k 1)))))))



(define (string-replace s t r) ;string target replacement
  (let ((k (locate-substring s t)))
	 (if (not k)
		  s
		  (string-append (substring s 0 k)
							  r
							  (substring s (+ k (string-length t)) (string-length s))))))

(define (string-replace* s t r)
  (let ((x (string-replace s t r)))
	 (if (string=? x s)
		  s
		  (string-replace* x t r)
		  )))


;;
;; (strspn str set) returns index of first char not in set
;; (strcspn str set) returns index of first char in set
;;

(define (strspn str set)
  (let strspan-loop ((s str))
	 (if (zero? (string-length s))
		  (string-length str)
		  (if (let inner-strspan-loop ((chset set))
				  (if (zero? (string-length chset))
						#f
						(if (eq? (string-ref s 0)
									(string-ref chset 0))
							 #t
							 (inner-strspan-loop (substring chset 1 (string-length chset))))))
				(strspan-loop (substring s 1 (string-length s)))
				(- (string-length str) (string-length s))))))

(define (strcspn str set)
  (let strcspan-loop ((s str))
	 (if (zero? (string-length s))
		  (string-length str)
		  (if (let inner-strcspan-loop ((chset set))
				  (if (zero? (string-length chset))
						#t
						(if (eq? (string-ref s 0)
									(string-ref chset 0))
							 #f
							 (inner-strcspan-loop (substring chset 1 (string-length chset))))))
				(strcspan-loop (substring s 1 (string-length s)))
				(- (string-length str) (string-length s))))))



;; This silently collapses multiple instances of either spaces or the indicated separator
(define (collapsing-strtok str . separator)
  (if (null? separator)
		(set! separator " ")
		(set! separator (car separator)))

  (if (string? str)
		(let colstrtok-loop ((results '())
					  (sstr str))
		  (if (zero? (string-length sstr))
				results
				(if (zero? (strspn sstr separator))
					 (colstrtok-loop (append results (list (substring sstr 0 (strcspn sstr separator) )))
							 (substring sstr (strcspn sstr separator) (string-length sstr)))
					 (colstrtok-loop results
							 (substring sstr (strspn sstr separator) (string-length sstr)))))))
  )

;; This does not collapse multiple instances of either spaces or the indicated separator 
(define (strtok str . separator)
  (if (null? separator)
		(set! separator " ")
		(set! separator (car separator)))

  (if (string? str)
		(let strtok-loop ((results '())
					  (sstr str))
		  (if (zero? (string-length sstr))
				results
				(if (zero? (strspn sstr separator))
					 (strtok-loop (append results (list (substring sstr 0 (strcspn sstr separator) )))
							 (substring sstr (strcspn sstr separator) (string-length sstr)))

					 (strtok-loop (if (and (> (string-length sstr) 1) (zero? (strspn (substring sstr 1 (string-length sstr)) separator))) results (append results (list "")))
							 (substring sstr 1 (string-length sstr)))))))
  )



;; reconstructs the string either with spaces or the indicated separator

(define (reconstruct-string strarray . separator)
  (if (null? separator)
		(set! separator " ")
		(set! separator (car separator)))

  (if (null? strarray)
		""
		(let restring-loop ((sa (cdr strarray))
					  (ns (car strarray)))
		  (if (null? sa)
				ns
				(restring-loop (cdr sa) (string-append ns separator (car sa)))))))


;; Like strtok, but it separates with a string rather than a set of characters
;; defaults to ","
(define (simple-split-string-at-separator str . separatorstring)
  (let ((sep (if (null? separatorstring)
					  ","	(car separatorstring))))
	 (let ((ns (string-length sep)))
		(let ssplits-loop ((s str)
					  (r '()))
		  (let ((n (locate-substring s sep)))
			 (if n
				  (ssplits-loop
				 	(substring s (+ n ns) (string-length s))
					(cons (substring s 0 n) r))
				  (reverse (cons s r))))))))

;; Like strtok, but it separates with a strings rather than a set of characters
;; defaults to ","
(define (simple-split-string-at-separator+ str . separatorstring)
  (let ((sep (if (null? separatorstring)
					  ","	(car separatorstring))))
	 (let ((ns (string-length sep)))
		(let ssplitssep-loop ((s str)
					  (r '()))
		  (let ((n (locate-substring s sep)))
			 (if n
				  (ssplitssep-loop
				 	(substring s (+ n ns) (string-length s))
					(cons sep (cons (substring s 0 n) r)))
				  (reverse (cons s r))))))))


(define (next-split str separatorstrings)
  (let* ((d (map (lambda (x) (locate-substring str x)) separatorstrings))
			(pairings (map cons d separatorstrings))
			(dl (filter number? d))
			(k (if (null? dl) #f (assoc (apply min (filter number? d)) pairings)))
			)
	 k))


;; Like strtok, but it separates with a set of strings rather than a set of characters
(define (split-string-at-separators str separatorstrings)
  (let splitssep-loop ((s str)
				 (r '()))
	 (let* ((n-s (next-split s separatorstrings))
			  (n (if n-s (car n-s) #f))
			  (sep (if n-s (cdr n-s) #f))
			  (ns (if n-s (string-length sep) #f)))
		(if n
			 (splitssep-loop
			  (substring s (+ n ns) (string-length s))
			  (cons (substring s 0 n) r))
			 (reverse (cons s r))))))

;; Like strtok, but it separates with a set of strings rather than a set of characters
(define (split-string-at-separators+ str separatorstrings)
  (let splitssep+loop ((s str)
				 (r '()))
	 (let* ((n-s (next-split s separatorstrings))
			  (n (if n-s (car n-s) #f))
			  (sep (if n-s (cdr n-s) #f))
			  (ns (if n-s (string-length sep) #f)))
		(if n
			 (splitssep+loop
			  (substring s (+ n ns) (string-length s))
			  (cons sep (cons (substring s 0 n) r)))
			 (reverse (cons s r))))))

;----- (erode-whitespace string) erode spaces at leading and trailing ends of  strings in lists
(define (erode-whitespace string)
  (let* ((N (string-length string))
			)
	 (if (<= N 0)
		  ""
		  (let ((h (string-ref string 0))
				  (t (string-ref string (- N 1))))
			 (cond
			  ((member h '(#\space #\tab))
				(erode-whitespace (string-cdr string)))
			  ((member t '(#\space #\tab))
				(erode-whitespace (substring string 0 (max 0 (- N 1)))))
			  (#t string))))))

;--- Code for polynomials

"Polynomials are encoded as a list of terms.  Terms are either
constants (numbers), or factors.  Factors are either constants or
lists of length two which are comprised of a symbol and an integer
which represents an exponent. "


;---- Minor predicates: constant? variable? factor? term? polynomial?

(define (constant? x) (or (number? x) (and (pair? x) (number? (car x)) (null? (cdr x)))))
(define (Constant? x) (or (constant? x)
								  (and (pair? x)
										 (or (and (pair? (car x) (zero? (cadar x))))
											  (and (number? (car x))
													 (pair? (cadr x))
													 (symbol? (caadr x))
													 (zero? (cadadr x)))))))
															
(define (remap-constant x)
  (cond
	((not (Constant? x)) x)
	((number? x) x)
	((and (pair? x) (number? (car x))) (car x))
	((and (pair? (car x) (zero? (cadar x)))) 1)
	((and (number? (car x))
			(pair? (cadr x))
			(symbol? (caadr x))
			(zero? (cadadr x)))
	 (car x))
	(#t x)))


(define (variable? x) (and (list? x) (= 2 (length x)) (symbol? (car x)) (number? (cadr x))))

(define (factor? x)
  (and (not (or (procedure? x) (vector? x) (boolean? x)))
		 (or (constant? x)	(variable? x))))

(define (term? x)
  (and (not (or (procedure? x) (vector? x) (boolean? x)))
		 (or (constant? x)
			  (and (pair? x) (apply andf (map (lambda (f) (factor? f)) x))))))

(define (polynomial? x)
  (and (not (or (procedure? x) (vector? x) (boolean? x)))
		 (or (number? x)
			  (and (pair? x) (apply andf (map term? x))))))


(define plus " + ")
(define minus " - ")

;----- Utilities for manipulating parts of polynomials

(define (rectify x)
  (if (number? x) (list x) x))

(define (rectify-numbers lst)
  (if (number? lst)
		(list lst)
		(if (list? lst)
			 (map (lambda (x)
					  (if (and (pair? x)
								  (null? (cdr x))
								  (number? (car x)))
							(car x)
							x))
					lst)
			 lst
			 )
		))


;---- Comparison operators

;----- Symbols

;------ symbols: (symbol<? a b)
(define (symbol<? a b)
  (cond
	((and (symbol? a) (symbol? b)) (string<? (symbol->string a) (symbol->string b)))
	(#t (error "Something bad happened in symbol<?" a b))
	))

;------ symbols: (symbol=? a b)
(define (symbol=? a b)
  (equal? a b))

;------ symbols: (symbol<=? a b)
(define (symbol<=? a b)
  (string<=? (symbol->string a) (symbol->string b)))

;------ symbols: (symbol<=? a b)
(define (symbol>? a b)
  (string>? (symbol->string a) (symbol->string b)))


;----- Factors

(define (factor-ordering a b)
  (cond
	((equal? a b) '=)
	((and (null? a) (not (null? b))) '<)
	((and (not (null? a)) (null? b)) '>)
	((and (pair? a) (pair? b))
	 (cond
	  ((symbol<? (car a) (car b)) '<)
	  ((symbol>? (car a) (car b)) '>)
	  ((equal? (car a) (car b))
		(cond
		 ((< (cadr a) (cadr b)) '<)
		 ((> (cadr a) (cadr b)) '>)
		 ((= (cadr a) (cadr b)) '=)
		 ))))
	((and (number? a) (number? b))
	 (cond
	  ((> a b) '>)
	  ((= a b) '=)
	  ((< a b) '<)))
	((and (number? a) (pair? b)) '>)
	((and (pair? a) (number? b)) '<)
	(#t 'factor-fail)
	))
		

(define (factor<? a b)
  (and (factor? a) (factor? b)
		 (equal? '< (factor-ordering a b))))

;------ (factor>? a b)  is the "greater than" comparison operator for sorting factors within a term: good for merge-sort
(define (factor>? a b)
  (and (factor? a) (factor? b)
		 (equal? '> (factor-ordering a b))))

;------ (factor=? a b) is an equality test 
(define (factor=? a b)
  (and (factor? a) (factor? b)
		 (equal? '= (factor-ordering a b))))


;------ (sort-factor! t) side effect: sorts the factors within a term
(define (sort-factor! t)
  (sort! t factor<?))


;----- terms
;------ (fix-factors! t)  sorts (in situ) the factors within the term

(define (fix-factors! t)
  (for-each sort-factor! t))

;------ (term<? a b)  possible side effects! the "less than" comparison operator may also sort the factors in the terms
;; this is not reliable if the factors in the term are not sorted


(define (term-ordering a b) ;; totally excludes constants
  (let ((a (if (not (list? a)) a (sort a factor<?)))
		  (b (if (not (list? b)) b (sort b factor<?))))
	 (cond
	  ((and (null? a) (null? b))	'=)
	  ((and (null? a) (not (null? b))) '<)
	  ((and (not (null? a)) (null? b)) '>)
	  ((equal? a b) '=) ;; cheat
	  ((and (pair? a) (pair? b))
		(cond
		 ((factor<? (car a) (car b)) '>)
		 ((factor>? (car a) (car b)) '<)
		 ((factor=? (car a) (car b))
		  (term-ordering (cdr a) (cdr b)))))
	  ((and (number? a) (number? b))
		(cond
		 ((< a b) '<)
		 ((= a b) '=)
		 ((> a b) '>)))
	  ((and (number? a) (pair? b)) '>)
	  ((and (pair? a) (number? b)) '<)
	  (#t 'fail))
	  ))

(define (term<? a b)
  (and (term? a) (term? b)
		 (equal? (term-ordering a b) '<)))


;------ (term>? a b)  is the "greater than" comparison operator for sorting terms within a term: good for merge-sort
(define (term>? a b)
  (and (term? a) (term? b)
		 (equal? (term-ordering a b) '>)))

;------ (term=? a b) is an equality test 
(define (term=? a b)
  (and (term? a) (term? b)
		 (equal? (term-ordering a b) '=)))


;------ (normalise-term t) --- returns a normalised term (in proper factor order with multiplicities condensed)
(define (index-in lst key)
  (letrec ((ll (length lst))
			  (ii (lambda (n) 
					  (cond
						((>= n ll) #f)
						((equal? (list-ref lst n) key) n)
						(#t (ii (+ n 1)))))))
	 (ii 0)))
		
(define (remap-term f)
	 (cond
	  ((number? f) f)
	  ((and (pair? f) (number? (car f))) (car f))
	  ((and (pair? f) (symbol? (car f)) (number? (cadr f)) (zero? (cadr f))) 1)
	  ((pair? f) f)
	  (#t f)
	  ))

(define (normalise-term t) ;; fixes the order of the factors in the term and collects factors (adjust exponents)
  (if (number? t)
		t
		(let* ((t (if filter-constant-factors
						  (map remap-term t)
						  t))
				 (C (filter number? t))
				 (F (!filter number? t))
				 )
		  (let* ((cC (apply * C))
					(ci (sort F factor<?)) ;; the symbols and exponents 
					(cn (unique (sort (map car ci) symbol<?))) ;; the unique signatures
					(ce (map (lambda (x) (apply + (map cadr x))) (map (lambda (i) (filter (lambda (k) (equal? (car k) i)) ci)) cn)))
					(cf (map list cn ce))
					(s " ")
					)
			 ;;(dnl "normalise-term: " C s F ": "cC s ci s cn s ce s cf)
			 (cons cC cf)
			 ))))

;----- (normalise-terms x) collects common terms and puts the terms in a canonical order.
(define (normalise-terms x)
  (set! x (map (lambda (n) (if (and (pair? n) (number? (car n)) (null? (cdr n))) (car n) n)) (map flatten-constant x)))
  (let* ((x  (sort x term<?))  ;; collects terms
			(C (filter number? x))
			(F (!filter null? (sort (!filter number? x) term<?)))
			(se (map car F))
			(ss (map cdr F))
			(Su (unique ss))
			(Ss (!filter null?
							 (map (lambda (x)
									  (map cdr (filter
													(lambda (y)
													  (equal? (car y) x))
													(map cons ss se))))
									Su)))
			(Sr (!filter null? (map (lambda (x y) (cons (apply + y) x)) Su Ss)))
			)

	 (if (null? C)
		  Sr
		  (append Sr (list (apply + C))))
	 )
  )



;----- polynomials -- most polynomial routines (esp. the comparisons) expect the polynomial to be normalised

(define (zero-term? t)
  (cond
	((null? t) #t)
	((and (number? t) (zero? t)) #t)
	((number? t) #f)
	((and (pair? t) (zero? (car t))) #t)
	(#t #f)))
			
(define (normalise-polynomial-factors p) p)
(define (normalise-polynomial p)
  (let* ((p2 (normalise-terms (map normalise-term (rectify-numbers p))))
			(sorted 	 (sort p2 term<?))
			(R (if filter-zero-terms
					 (!filter zero-term? sorted)
					 sorted)))
	 (if (null? R) '(0) R)))

	 
(define (normalise-label t)
  (list (normalise-polynomial (label t)) (children t)))

(define (normalise-labels t)
  (list (normalise-polynomial (label t)) (children t)))


;------ (polynomial=? p q) 
(define (polynomial=? p q)
  (string=? (polynomial->string p) (polynomial->string q)))

;------ (polynomial<? p q)
(define (polynomial<? p q)
  (if #t
		(string<? (polynomial->string p) (polynomial->string q))
		(begin
		  (display "polynomial? p -- ")(pp p)
		  (display "polynomial? q -- ")(pp q)

		  (let ((p (if (string? p) (string->polynomial p) p))
				  (q (if (string? q) (string->polynomial q) q)))

			 (if (not (polynomial? p))
				  (begin
					 (display "polynomial<?: p is not a polynomial: ")(pp p)
					 (error "The first argument is not a polynomial in a call to polynomial<?" p)))

			 (if (not (polynomial? q))
				  (begin
					 (display "polynomial<?: q is not a polynomial: ") (pp q)
					 (error "The second argument is not a polynomial in a call to polynomial<?" q)))

			 (let (;;(a (reverse (normalise-polynomial p)))
					 ;;(b (reverse (normalise-polynomial q)))
					 (a (normalise-polynomial p))
					 (b (normalise-polynomial q))
					 )
				(cond
				 ((and (null? a) (null? b))  #f)
				 ((and (null? a) (not (null? b))) #t)
				 ((and (not (null? a))  (null? b))  #f)

				 ((term<? (car a) (car b)) #t)
				 ;;((term=? (car a) (car b)) (polynomial<? (reverse (cdr a)) (reverse (cdr b))))
				 ((term=? (car a) (car b)) (polynomial<? (cdr a) (cdr b)))
				 ((term<? (car b) (car a)) #f)

				 ((and (number? (car a)) (number? (car b)))
				  (< (car a) (car b)))
				 ((and (number? (car a)) (not (number? (car b))))
				  #t)
				 ((and (not (number? (car a))) (number? (car b)))
				  #f)
				 (#t (error "ran off the rails in polynomial comparison" p q)))))
		  )
		))



;------ (polynomial=? p q) -- not fast, but robust
(define (polynomial=? p q)
  (string=? (polynomial->string p) (polynomial->string q)))

(define (polynomial<? p q)
  (if (not (polynomial? p))
		(begin
		  (dnl "polynomial<?: p is buggered\n" p)
		  (error "The first argument is not a polynomial in a call to polynomial<?" p)))
  (if (not (polynomial? q))
		(begin
		  (dnl "polynomial<?: q is buggered\n" q)
		  (error "The second argument is not a polynomial in a call to polynomial<?" q)))

  (let ((a (reverse (normalise-polynomial p)))
		  (b (reverse (normalise-polynomial q))))
	 (cond
	  ((and (null? a) (null? b))  #f)
	  ((and (null? a) (not (null? b))) #t)
	  ((and (not (null? a))  (null? b))  #f)

	  ((and (number? (car a)) (number? (car b)))
		(< (car a) (car b)))
	  ((and (number? (car a)) (not (number? (car b))))
		#t)
	  ((and (not (number? (car a))) (number? (car b)))
		#f)
	  ((term<? (car a) (car b)) #t)
	  ((term=? (car a) (car b)) (polynomial<? (reverse (cdr a)) (reverse (cdr b))))
	  ((term<? (car b) (car a)) #f)
	  (#t (error "ran off the rails in polynomial comparison" p q)))))


(define (node<? n m)
  (cond
	((not (and (node? n) (node? m)))
	 (error "Passed a non-node to node<?" n m))
	((and (null? n) (null? m)) #f)
	((and (null? n) (not (null? m))) #t)
	((and (not (null? n))  (null? m)) #f)

	((polynomial<? (label n) (label m)) #t)
	((polynomial<? (label m) (label n)) #f)

	;; must have equal labels
	((< (weight n) (weight m)) #t)
	((> (weight n) (weight m)) #f)
	;; must have equal weights
	((< (absolute-value n) (absolute-value m)) #t)
	((> (absolute-value n) (absolute-value m)) #f)
	;; must have the same magnitude
	((< (card n) (card m)) #t)
	((> (card n) (card m)) #f)
	;; must have the same cardinality
	((< (depth n) (depth m)) #t)
	((> (depth n) (depth m)) #f)
	;; must have the same depth
	(#t #t)))



(define (node=? n m)
  (if #t ;; canonical or "improved"
		(cond
		 ((not (and (node? n) (node? m)))
		  (error "Passed a non-node to node<?" n m))
		 ((and (null? n) (null? m)) #t)
		 ((and (null? n) (not (null? m))) #f)
		 ((and (not (null? n))  (null? m)) #f)
		 
		 ((polynomial=? (label n) (label m)) #t)
		 ((not (polynomial=? (label m) (label n))) #f)
		 ;; must have equal labels
		 ((= (weight n) (weight m)) #t)
		 ((not (= (weight n) (weight m))) #f)
		 ;; must have equal weights
		 ((= (absolute-value n) (absolute-value m)) #t)
		 ((not (= (absolute-value n) (absolute-value m))) #f)
		 ;; must have the same magnitude
		 ((= (card n) (card m)) #t)
		 ((not (= (card n) (card m))) #f)
		 ;; must have the same cardinality
		 ((= (depth n) (depth m)) #t)
		 ((not (= (depth n) (depth m))) #f)
		 ;; must have the same depth
		 (#t #t))
		(if #t ;; Quick or rubbery
			 (equal? n m)
			 (or ;; rubbery version
			  (and (null? n) (null? m))
			  (and (node? n) (node? m))
			  (and
				(polynomial=? (label n) (label m))
				(= (weight n) (weight m))
				(= (absolute-value n) (absolute-value m))
				(= (card n) (card m))
				(= (depth n) (depth m))))))
		)
  
(define tree=? node=?)



;----- various predicates for filtering

(define (label-constants p)
  (filter number? (label p)))

(define (label-ind-factors p)
  (!filter number? (label p)))

(define (constant-label? p)
  (cond
	((number? p) #t)
	((and (list? p) (number? (label p))) #t)
	((list? p) (= (length (label-constants p))
					  (length (label p))))
	(#t #f)))


(define (the-non-zero-constant-terms x)  ;; Applies to *terms*, not *factors*
  (cond
	((and (pair? x) (null? (cdr x)) (number? (car x)) (not (zero? (car x))))
	 (car x))
	((and (number? x) (not (zero? x))) x)
	(#t #f)))

(define (the-non-zero-indeterminant-terms x)  ;; Applies to *terms*, not *factors*
  (if (and (pair? x)
			  (number? (car x))
			  (not (zero? (car x)))
			  (car x))
		x
		#f))

(define (the-indeterminant-terms x)  ;; Applies to *terms*, not *factors*
  (if (and (pair? x)
			  (number? (car x))
			  (car x))
		x
		#f))

(define (the-zero-terms x)  ;; Applies to *terms*, not *factors*
  (or	(and (number? x) (zero? x))
		(and (pair? x) (number? (car x)) (zero? (car x)) (car x))
		))


(define (the-constant-terms x)  ;; Applies to *terms*, not *factors*
  (or (and (number? x) x)
		(and (pair? x)
			  (and (number? (car x))
					 (pair? (cdr x))
					 (zero? (cadr (cadr x))))
			  (car x)
			  )))


(define (the-non-zero-indeterminant-factors x)  ;; Applies to *factors*, not *terms*
  (if (and (pair? x)
			  (number? (car x))
			  (not (zero? (cadr x)))
			  (car x)
			  )
		x
		#f))

(define (the-indeterminant-factors x)  ;; Applies to *factors*, not *terms*
  (if (and (pair? x)
			  (number? (car x))
			  (car x)
			  )
		x
		#f))

(define (the-constant-factors x)  ;; Applies to *factors*, not *terms*
  (or (and (number? x) x)
		(and (pair? x)
			  (and (number? (car x))
					 (zero? (cadr x)))
			  (car x)
			  )))

(define (pick-only type x)
  (if (not (list? x))
		(if (type x)
			 '())
		(filter I (map type x))))

;----- (trees-with-labels trees labels)
(define (trees-with-labels trees labels)
  (filter (lambda (x) (member (label x) labels)) trees))

;----- (trees-with-labels trees labels)
(define (trees-with-similar-labels trees labels)
  (filter (lambda (t)
				(not (null? (filter (lambda (x) (sim? (label t) x)) labels))))
			 trees))

(define (rewrite-constant-indeterminant x)
  ;;  (dnl "---> " x)
  (cond
	((number? x) x)
	((and (pair? x) (number? (car x))(zero? (car x)))
	 (car x))
	((and (pair? x) (number? (car x)) (pair? (cdr x)) (pair? (cadr x))
			(apply andf (map zero? (map cadr (cdr x)))))
	 (car x))
	(#t x))
  )

;---- manipulation routines for polynomials and their kin
;----- (normalise-factors x) collects common factors in a term
;; (define (normalise-factors x)
;;   ;;  (dnl "** " x " " (term? x))
;;   ;;  (set! x (map (lambda (y) (if (and (pair? y) (null? (cdr y))) (car y) y)) x))

;; ;;;  (set! x (!filter the-zero-terms (map rewrite-constant-indeterminant x)))

;;   (if #t ;; Stable version
;; 		(let* ((x  x)
;; 				 (C (filter number? x))
;; 				 (F (!filter null? (sort (!filter number? x) factor<?)))
;; ;	(NF (filter (lambda (u) (pair? u) (equal? u (filter number? u))) x))
;; 				 (ss (map car F))
;; 				 (se (map cadr F))
;; 				 (Su (unique ss))
;; 				 (Ss (!filter null? (map (lambda (x) (map cdr (filter (lambda (y) (equal? (car y) x)) (map cons ss se)))) Su)))
;; 				 (Sr (!filter null? (map (lambda (x y) (list x (apply + y))) Su Ss)))
;; 				 )
;; 		  ;;   (dnl "Input = " x)
;; 		  ;;	 (dnl "Su: " Su)
;; 		  ;;	 (dnl "Ss" Ss)
;; 		  ;;	 (dnl "Sr" Sr)

;; 		  (if (null? C)
;; 				Sr
;; 				(cons (apply * C) Sr))
;; 		  )

;; 		;; Experimental version
;; 		(let* ((C (pick-only the-constant-factors x))
;; 				 (F (pick-only the-indeterminant-factors x))
;; ;	(NF (filter (lambda (u) (pair? u) (equal? u (filter number? u))) x))
;; 				 (ss (map car F))
;; 				 (se (map cadr F))
;; 				 (Su (unique ss))
;; 				 (Ss (!filter null?
;; 								  (map (lambda (x)
;; 											(map cdr
;; 												  (filter (lambda (y) (equal? (car y) x))
;; 															 (map cons ss se))))
;; 										 Su)))
;; 				 (Sr (!filter null? (map (lambda (x y) (list x (apply + y))) Su Ss)))
;; 				 )
;; 		  ;;   (dnl "input = " x)
;; 		  ;;   (dnl "Su: " Su)
;; 		  ;;	 (dnl "Ss" Ss)
;; 		  ;;	 (dnl "Sr" Sr)

;; 		  (let* ((R (if (null? C)
;; 							 Sr
;; 							 (cons (apply * C) Sr)))
;; 					)
;; 			 R)
;; 		  )
;; 		)
;;   )

;; ;------ (normalise-term x) alias for normalise-factors
;; (define normalise-term normalise-factors)


;; ;------ (normalise-terms x) collects compatible 

;; (define (normalise-terms x)
;;   (set! x (map rewrite-constant-indeterminant x))
;;   (if #t ;; Stable version
;; 		(let* ((C (filter number? x))
;; 				 (T (!filter null? (map normalise-factors (!filter number? x))))
;; 				 )
;; 		  (set! C (if (not (null? C)) (apply + C)))

;; 		  (let* ((C (filter number? x))
;; 					(T (!filter number? x)))
;; 			 (sort
;; 			  (if (null? C)
;; 					(!filter null? (map normalise-factors T))
;; 					(cons (apply + C) (!filter null? (map normalise-factors T))))
;; 			  term<?)
;; 			 ))

;; 		;; Experimental version
;; 		(let* ((C (pick-only the-non-zero-constant-terms x))
;; 				 (T (pick-only the-indeterminant-terms x))
;; 				 )
;; 		  (set! C (if (not (null? C)) (apply + C)))
;; 		  (let ((R (sort
;; 						(if (null? C)
;; 							 (!filter null? (map normalise-factors T))
;; 							 (cons (if (number? C) C (apply + C)) (!filter null? (map normalise-terms T))))
;; 						term<?)))
;; 			 R)
;; 		  )
;; 		)
;;   )




;------ (normalise-terms x) collects compatible factors


;----- (symbolic-sigs t) generates a signature/signatures for a term or terms  so we can collect compatible terms in polys

(define (symbolic-sigs x)
  (cond
	((number? x) x)
	((constant? x) x)
	((polynomial? x)
	 (let* ((X (normalise-terms x))
			  (C (filter number? X))
			  (S (!filter null? (!filter number? X)))
			  (S (map (lambda (y) (!filter number? y)) (!filter null? (!filter number? X))))
			  (V (map (lambda (x) (map car x)) S)) ;; just get the symbolic bit
			  ;;(E (map cadr S))
			  )
		(sort S term<?))) ;; need the exponent too
	((term? x)
	 (car (symbolic-sigs (list x))))
	(#t (error "bad argument to symbolic-sigs" x))
	)	
  )



;----- (matches-sym-sig sig) generates a "filter" function

(define (matches-sym-sig sig)
  (lambda (t)
	 (cond
	  ((term? t) (equal? (symbolic-sigs t) sig))
	  (#t (error "matches-sym-sig only works for terms!" x)))))

(define (flatten-constant P)
  (if (and (pair? P) (null? (cdr P))
			  (pair? (car P)) (null? (cdar P))
			  (number? (caar P)))
		(car P)
		P)
;;  P
  )

;----- (normalise-polynomial p) adds collecting terms to  normalise-terms

(define (normalise-polynomial P)
  (normalise-terms 
	(let ((P (flatten-constant P)))
	  (let ((np (cond
					 ((string? P) (string->polynomial P))
					 ((null? P) (boogaboogabooga))
					 ((number? P) (list P))
					 ((and (pair? P) (null? (cdr P)) (number? (car P))) P)
					 ((and (pair? P) (null? (cdr P)) (number? (car P))) P)
					 ((and (list? P) (polynomial? P))
					  (set! P (map rewrite-constant-indeterminant P))
					  (let* ((p (normalise-terms P))
								(C (apply + (filter number? p)))
								(T (!filter number? p))
								(Ct (map (lambda (y) (if (null? y) 1 y)) (map (lambda (x) (filter number? x)) T)))
								(ss (map flatten-constant (if (null? T) '() (unique (symbolic-sigs T)))))
								(nc-terms (map (lambda (sig)								  
													  (let* ((fl (filter (matches-sym-sig sig) p))
																(fc (apply + (map (lambda (x) (if (pair? (car x)) 1 (car x))) fl)))
																)
														 (cons fc sig)
														 )
													  )
													ss)))
						 (cons C nc-terms))
					  )
					 (#t (error "normalise-polynomial: Not a polynomial, cannot normalise " P))
					 )
					))
		 (if (null? np) (Boogaboogabooga))
		 np))
	))


(define (normalise-label t)
  (list (normalise-polynomial (label t))
		  (children t)))

(define (normalise-labels t)
  (list (normalise-polynomial (label t))
		  (map normalise-labels (children t))))

;---- arithmetic ops for polynomials

;----- (p+ . ) adds polynomials
(define (p+ . args)
  (normalise-polynomial (apply append (map polynomial (map rectify args)))))

;----- (_t* a b) multiplies terms in polynomials


;; (define (_t* a b)
;;   (define (__t* . args)
;; 	 ;;  (if (not (apply andf (map term? args))) (error "non-term passed to _t*" (list args (map term? args))))
;; 	 (rectify-numbers
;; 	  (normalise-factors (apply append (map (lambda (x) (if (pair? x) x (list x))) args)))))
;;   (__t* a b))

(define (ensure-list lst)
  (map (lambda (x) (if (list? x) x (cons x '()))) lst))


;----- (t* a b) multiplies terms in polynomials
(define (t* t1 t2)
  (define (fix x) (if (not (pair? x)) (list x) x))
  (normalise-term (append (fix t1) (fix t2))))

(define (@t* x)
  (apply t* x))

(define (bad-p2* a b)
  (let ((a (if (number? a) (list a) a))
		  (b (if (number? b) (list b) b)))
	 (normalise-polynomial (map (lambda (x) (apply t* x)) (map* (lambda x x) a b)))))

(define (p2* a b)
  (let ((a (if (pair? a) a (list a)))
		  (b (if (pair? b) b (list b))))
	 (normalise-polynomial
	  (map* (lambda (x y)
				 (t* x y)) a b))))

(define (p* . args)
  (let ((args (map polynomial args)))
	 (case (length args)
		((0) '(1))
		((1) (car args))
		((2) (apply p2* args))
		(else (normalise-polynomial (p2* (car args) (apply p* (cdr args)))))))
  )

(define (sp* . args) (apply p* (map string->polynomial args)))


;---- (evaluate-polynomial P codex)

(define (substitute-factor f codex)
  (if (number? f)
		f
		(let ((s (assq (car f) codex)))
		  (if s (list (cdr s) (cadr f)) f))))

(define (substitute-term t codex)
  (if (number? t)
		t 
		(map (lambda (x) (substitute-factor x codex)) t)))

(define (substitute-polynomial P codex)
  (map (lambda (x) (substitute-term x codex)) P))


(define (evaluate-factor f)
  (if (number? f)
		f
		(apply power f)))

(define (evaluate-term t)
  (if (number? t)
		t
		(apply * (map evaluate-factor t))))

(define (evaluate-polynomial P codex)
  (let ((p (substitute-polynomial P codex)))
	 (if (number? p)
		  p
		  (apply + (map evaluate-term p)))))
				  

;---- polynomial->string & string->polynomial routines

;----- (string->term str) maps a term in string form to a polynomial
(define (string->term str . neg)
  "4 a b^3 c^2 d"

  (set! neg (if (null? neg) '(1) '(-1)))
  (let ((str (tidy-string-ends str)))
	 (if (not (string? str))
		  str
		  (begin
			 (if (or (string-contains? str plus) (string-contains? str minus)) (error "expected term, got polynomial" str))
			 (let* ((l (strtok str))
					  (nl (map string->number l))
					  (sl (map cadr (filter (lambda (x) (if (not (car x)) (cadr x) #f)) (map list nl l))))
					  (st (map (lambda (x)
									 (let ((v (strtok x "^")))
										(if (= 1 (length v))
											 (list (string->symbol (car v)) 1)
											 (list (string->symbol (car v)) (string->number (cadr v)))
											 ))) sl))
					  )
				(set! nl (append neg (filter I nl)))
				(cond
				 ((null? nl) st)
				 ((null? st) (apply * nl))
				 (#t (cons (apply * nl) st))))))))

;----- (polynomial->string p) and (string->polynomial pstr) convert between strings like 4 + 2 x + 2 x^2 y^3 and (4 (2 (x 1)) (2 (x 2)(y 3)))

(define (string->polynomial pstr)
  (let ((pstr (tidy-string-ends (string-replace* pstr "*" " "))))
	 (let ((P (if (not (string? pstr))
					  pstr
					  (begin
						 (if (or (string-contains? pstr " ^")  (string-contains? pstr "^ "))
							  (error " There should be no spaces around the caret (^) operator " pstr))
						 
						 (let* ((str (split-string-at-separators+ pstr (list plus minus))))
							;;(apply append str-)

							(let s->poly-loop ((s str)
										  (r '())
										  )
							  (cond
								((null? s) (normalise-polynomial (reverse r)))
								((string=? (car s) minus)
								 (let ((t (string->term (cadr s))))
									(s->poly-loop (cddr s)
										 (cons (string->term (cadr s) -1) r))
									))
								((string=? (car s) plus)
								 (s->poly-loop (cddr s)
										 (cons (string->term (cadr s)) r)
										 ))
								(#t
								 (s->poly-loop (cdr s)
										 (cons (string->term (car s)) r)
										 ))
								)
							  )
							)
						 )
					  ))
			 )
		(if (null? P)
			 '(0)
			 P))))



;----- (poly-string-term t form) generate a string for a term
(define (poly-string-term t form)
  (let* ((c (filter number? t))
			(cn (if (null? c) 1 (apply * c)))
			(cs (n->s cn form))
			(s (filter pair? t))
			)
	 (if (null? s)
		  (string-append cs " ") ;; Just a number, no polynomial factors
		  (string-append (if (= 1 cn) "" (string-append "" cs))
							  (apply string-append
										(cons (if (= cn 1) "" " ")
												(map (lambda (f)
														 (string-append
														  (symbol->string (car f))
														  (if (= 1 (cadr f))
																" "
																(string-append "^" (n->s (cadr f) form) " ")
																)))
													  s)))
							  ))
	 ))

(define (n->s n form)
  (cond
	((integer? n) (number->string n))
	((flonum? n) (number->string n))
	((rational? n)
	 (case form
		(('() #f)
		 (number->string n))
		((latex inline latex-inline)
		 (string-append "\\frac{" (number->string (numerator n)) "}{" (number->string (denominator n)) "}"))
		((display latex-display)
		 (string-append "\\dfrac{" (number->string (numerator n)) "}{" (number->string (denominator n)) "}"))
		(else 
		 (number->string n))
		))
	))


;----- (poly-string-polynomial p op cp) generate a string for a polynomial (with appropriate bracketting)
(define (poly-string-polynomial p op cp . latex-fracform)
  (set! latex-fracform (if (pair? latex-fracform) (car latex-fracform) #f))

  (erode-whitespace
	(let* ((p (normalise-polynomial (if (number? p) (list p) p)))
			 (C (apply + (filter number? p)))
			 (t* (map (lambda (x) (poly-string-term x latex-fracform)) (!filter number? p)))
			 )
	  (if (null? t*)
			(n->s C latex-fracform)
			;;(string-append op (number->string C) cp)

			(let poly-string-loop ((t (cdr t*))
						  (s (string-append op (car t*)))
						  )
			  (cond
				((null? t)
				 (if (zero? C)
					  (string-append s cp)
					  (string-append  s "+ " (n->s C latex-fracform)  cp)))

				;; non-negative term
				((not (char=? (string-ref (car t) 0) #\-))
				 (poly-string-loop (cdr t)
						 (string-append s "+ " (car t))))

				;; negative term
				((char=? (string-ref (car t) 0) #\-)
				 (poly-string-loop (cdr t)
						 (string-append
						  s "- "
						  (substring (car t) 1 (string-length (car t)))))
				 )
				(#t (error "bad polynomial" p))))))))

;----- (polynomial->string p) emit a string corresponding to the polynomial
(define (polynomial->string p)
  (cond
	((string? p) p) ;; we assume it knows what it is doing....
	((polynomial? p) (poly-string-polynomial p "" ""))
	((polynomial? p) (poly-string-polynomial p "" ""))
	(#t "")))

;----- (polynomial->inline-string p)  emit a latex inline string corresponding to the polynomial
(define (polynomial->latex-inline p)
  (if (polynomial? p)
		(poly-string-polynomial p "\\(" "\\)" 'latex)
		""))

;----- (polynomial->display-string p)  emit a latex display string corresponding to the polynomial
(define (polynomial->latex-display p)
  (if (polynomial? p)
		(poly-string-polynomial p "\\[" "\\]" 'display)
		""))

;----- (term trm)
(define (term trm)
  (cond
	((string? trm)
	 (term (string->polynomial trm)))
	((term? trm) (normalise-factors term))
	((polynomial? trm)
	 (if (not (= (length trm) 1))
		  #f
		  (car (normalise-polynomial trm))))
	(#t #f)))

;----- (polynomial ply) construct/normalise a polynomial
(define (polynomial ply)
  (cond
	((string? ply) (string->polynomial ply))
	((term? ply) (normalise-polynomial (list ply)))
	((polynomial? ply) (normalise-polynomial ply))
	(#t #f)))



;-- Code for rings of tree elements (nodes)
"
Nodes are lists of the form 

      (polynomial set-of-child-nodes)

and trees are really just collections of related nodes where there is
a root (the node which is (uniquely) the child of no other node in the set).
"

(define use-caution #t)


;--- Important "constants": emptyset, zerotree

(define emptyset '{})
(define zerotree (list 0 emptyset))  ;; [Definition 1]
(define onetree (list 1 emptyset))  ;; [Definition 1]

;--- Accessors: weight label extension extenson-set child 

;----  label and children

(define (label v)
  (if (null? v)
		0
		(car v)))

(define (scalar-term n)
  (apply * (filter number? n)))

(define (weight n)
  (scalar-term (label n)))

(define (content n) ;; scores 1 for each non-zero label
  (cond
	((null? n) 0)
	((node? n)
		(let* ((l (label n))
				 )
		  (apply +
					(cons
					 (+ (if (or (null? l) (member l '(0 (0) ((0)) 0.0 (0.0) ((0.0))))) 0 1)
						 )
					 (map content (children n))))))
	((children? n)
	 (apply + (map content n)))
	(#t
	 (dnl "The object passed to content is not a tree or treelike object!" (tree->string n))
	 #f)))
						 

;---- (children-> . args) construct a set of children
(define (children-> . kids)
  (if (apply andf (map node? kids))
		(copy-list kids)
		(error "Bad element(s) in children" kids)))

;---- (children v) returns the set of children of v
(define (children v)
  (cond
	((null? v) '())
	((and (= (length v) 2) (polynomial? (label v)))  (cadr v))
	((node? v) (cadr v))
	(#t (error "requested a set of children from the wrong kind of object" v))))



;---- (child v l) the element of the set of children of v with a label l, or the empty set
(define (child V l)
  (let (
		  (v V
			  ;;(if (not (unique-children? oV))
			  ;;	(normalise-children V)
			  ;;  	V))
		  ))
	 (let* ((s (cond
					((null? v) zerotree)
					((children? v) v)
					((node? v) (children v))
					(#t (error "the 'v' argument is not a node or set of children" v))))
			  (r (filter (lambda (x) (equal? l (label x))) s))
			  )
		(if (= (length r) 1) ;; if it is exactly a single element just return that otherwise either a null or a list
			 (car r)
			 r))))

(define (union . args)  (unique (apply append args)))  ;; makes it nice and readable for set operations
(define (union+ . args)  (apply append args))          ;; like union, but preserves multiplicity


;---- (intersection op sel . args)
;; op is a function of the form (pred key matchlist) and returns #t if key is in matchlist
(define (intersection op sel . args)
  (let* ((s (map (lambda  (x) (map sel x)) args))
			(S (apply list-intersection* s))
			(candidates (apply union+ args))
			)
	 (filter (lambda (x) (op x S)) candidates)))
  

(define (intersection-l*= . args) ;; the intersection of the sets is the set of elements which have labels which are identical
  (apply intersection (cons trees-with-labels (cons label args))))

(define (intersection-l*~ . args) ;; the intersection of the sets is the set of elements which have labels which are identical
  (apply intersection (cons trees-with-similar-labels (cons label args))))




;--- Predicates: zerotree? node? tree? simple-node? children?

;---- (zerotree? t) returns #t if t is the zerotree.  We need this because "(equal? '(0 0 '()) zerotree)" may return false
(define (zerotree? t)
  (if (string? t) (set! t (string->node t)))

  (if (not (node? t)) (error "bad call to zerotree?" t))
  (and (pair? t)
		 (= (length t) 2)
		 (polynomial=? (label t) "(0)")
		 (or (null? (children t))
			  (apply zerotree? (children t)))))


;---- (node? n) and (tree? n) 
(define (node? n)
  (cond
	;;((null? n) #t) ;; No, a null list is not equivalent to the zerotree.
	((not (pair? n)) #f)
	(#t (and (= (length n) 2)
				(polynomial? (label n))
				(or (null? (children n))
					 (apply andf (map node? (children n))))
				))
	))

(define tree? node?)

;---- (simple-node? n)  node with empty set of children
(define (simple-node? n)
  (and (= (length n) 1) (polynomial? (label n)) (null? (children n))))


;---- (unique-children? E)
(define (unique-children? E)
  (cond
	((null? E) #t)
	((and (apply andf (map unique-node? E))
			(let ((lbls (map polynomial->string (map label E))))
			  (= (length lbls) (length (unique lbls)))))
	 #t)
	(#t #f)) )

;---- (unique-node? n) and (tree? n) 
(define (unique-node? n)
  (and (node? n)
		 (unique-children? (children n))))


;---- (children? E) 
;; They may have several identical children, for example.
(define (children? E)
  (cond
	((null? E) #t)
	(#t (apply andf (map node? E)))
	))


;--- Basic constructors: node random-node make-children
(define (make-unique-children . args)
  (if (apply andf (map node? args))
		(copy-list args)
		(let ((X (!filter node? args)))
		  (error "One of the children is from a Stephen King novel"  X))))


(define (make-zerotree D) ;; generates a member of the equivalence class of (0 {})
  (if (not (and (not (negative? D)) (integer? D)))
		(error "bad argument to random-zerotree" D))
  (list 0
		  (if (<= D 0)	'() ;; We have bottomed out depth wise
				(list (make-zerotree (- D 1))))))


(define node list) ;; makes it easier to read


;---- (n-lambda poly) generates a list of the indeterminant factors in a polynomial
(define (n-lambda n) ;; the relation used for collecting terms
  (let ((fl (factor-list (if (polynomial? n) n (label n))))
		  (cl (filter number? (if (polynomial? n) n (label n))))
		  ;;(cl '())
		  )
	 ;;(unique (sort (map (lambda (x) (!filter number? x)) fl) factor<?))
	 (append (unique (map (lambda (x) (!filter number? x)) (sort fl term<?))) (if (null? cl) '() '(1)))
	 ))

  ;; (let ((fl (factor-list (!filter number? (if (polynomial? n) n (label n))))))
  ;; 	 (if (null? fl)
  ;; 		  (list (apply * (filter number? (if (polynomial? n) n (label n)))))
  ;; 		  fl)))


(define (sim? u v) ;; similar nodes have polynomials that are related (nodes or polys)
  (equal? (n-lambda u) (n-lambda v)))

(define compatible? sim?)

(define (eq-class? s)
  (cond
	((null? s) #t)
	((not (pair? s)) #f)
	((polynomial? (car s)) (apply andf (map polynomial? s)))
	(#t (children? s))))

(define (eq-class-set? S)
  (apply andf (map eq-class? S)))

  

(define (eq-class r U) ; equiv class in U corresponding to s
  (let* ((c (n-lambda r))
			(C (filter (lambda (x) (sim? x r)) U)))
	 (if (eq-class-set? C)
		  C
		  (error "bad result in eq-class"C))))

(define (eq-class^sigma s U) ; sum of equiv class in U corresponding to s
  (let ((r (apply (if (polynomial? s) p+ tree+) (eq-class s U))))
	 (cond
	  ((or (polynomial? r) (tree? r)) r)
	  (#t (error"bad result in eq-class^sigma" r))
	  )))


(define (n-Lambda U) ;; set of all equivalence classes in U (nodes)
  (cond
	((null? U)	'())
	((not (pair? U)) (error "bad argument in n-Lambda" U))
	(#t
	 (let* ((ispoly (polynomial? (car U)))
			  (U (sort U (if ispoly polynomial<? node<?)))
			  (c (filter (if ispoly constant? constant-label?) U))
			  (i (!filter (if ispoly constant? constant-label?) U))
			
			  (I* (unique (map (lambda (r) (filter (lambda (x) (sim? x r)) i)) i)))
			  ;; If we try in the other order, (sim? r x), you get pathological behaviour :-)
			  )
		(if (null? c)
			 I*
			 (append (cons c '()) I*))))
	))
		 

(define (n-Lambda^sigma U) ; members of all equiv classes in U summed
  (if #t
		U
		 
		(let* ((U (sort U node<?))
				 (R  (cond
						((null? U) '())
						;;	((zero? (apply + (map (if (children? U) absolute-value (lambda (x) 1)) U))) '()) ;; null trees are collapsed
						;;	(#t (map (lambda (x) (if (children? x) (apply tree+ x) (apply p+ x))) (n-Lambda U)))))
						(#t (map (lambda (x)
									  (cond
										((polynomial? (car x)) (apply p+ x))
										((children? x) (apply tree+ x ))
										(#t x))) 
									(!filter null? (n-Lambda U)))))))
		  (if (apply andf (map tree? R))
				(sort R node<?)
				(error "bad value for return from n-Lambda^sigma" R)))))



;; ;---- (node p c) ; polynomial-label children 

;; (define (node p c) ; polynomial-label children
;;   ;;  (ddnl "\nnode: " w " " p " " c)
;;   (cond
;; 	((string? p) (node (string->polynomial p) c))
;; 	(#t
;; 	 (if (not (polynomial? p)) (error "Label must be a polynomial" p))
	 
;; 	 (if (not (or (null? c)  (apply fand (map node? c))))
;; 		  (begin
;; 			 (set! booboo c)
;; 			 (ERROR "Bad set of children" 'ook)))
;; 	 (list (polynomial p) c))))


;---- (random-polynomial-term L s E)  set-of-labels, scale, max exponent
(define (random-polynomial-term L s E) 
  (cond
	((zero? s) (list 0))
	((zero? E) (list (random-integer s)))
	(#t
	 (let ((e (random-integer E)))
		(list (+ 1 (random-integer s)) ;; we don't want zero coefficients associated with a factor!
				(normalise-polynomial-factors (map (lambda (x) (list (list-ref L (random-integer (length L)))  (+ 1 (random-integer e)))) (seq E))))
		))))


;---- (random-polynomial L S k E)  set-of-labels, Scalar, k terms,  max Exponent
(define (random-polynomial L S k E)
  (let* ((n-terms (random-integer k))
			;;(s (* 2.0 (- (random-real) 0.5) (random-integer S))) ;; scalar part
			(s (random-integer S)) ;; scalar part
			(pt (map (lambda (x) (random-polynomial-term L (+ 1 (random-integer S)) E)) (seq (random-integer k))))
			)
	 (normalise-polynomial (cons s pt))))

;; the polynomial (5 (7 ((x 2) (y 2))) (1 ((x 1) (y 1))) (1 ((x 1) (y 1))))
;; reads as 5 + 7x^2y^2 + xy + xy

;---- (random-node L w D E z) Labels, (w)eight param for polynomials,  Depth, E= number param for children terms in poly
;; The weights of nodes will naturally progressively dwindle in the extension sets since at each extension the max weight is drawn from [0-W]
(define (random-node L w D E z )
  (if (not (and (list? L) (apply andf (map symbol? L)))) (error "bad label list" L))
  (if (not (apply andf (map integer? (list w D W z))))
		(error "bad numeric argument to random-node" (list w D E z)))
  
  (list (random-polynomial L w z)
		  (if (zero? D)
				'() ;; We have bottomed out depth wise
				(let ((e (random-integer E))) ;; 
				  (if (zero? e)
						'()
						(map (lambda (x) (random-node L w (- D 1) (random-integer (* 2 e)) z)) (seq e))))))
  )



;---- Functions from Definitions 2,3 and 4

;----- (depth tree)  returns the depth of the tree [Definition 2]
(define (depth tree)
  (cond
	((not (tree? tree)) #f)
	((or (null? tree) (zerotree? tree)) 0)
	((null? (children tree)) 1)
	(#t (+ 1 (apply max (map depth (children tree)))))) )

;----- (trim tree) returns a tree with all simple nodes excised [Definition 3]
(define (trim tree)
  (if (simple-node? tree)
		tree
		(list (label tree) (!filter null? (map trim (children tree))))))

;----- (card tree) returns the cardinality of the tree (the number of non-zero nodes [Definition 4]
(define (card tree)
  (if (zerotree? tree)
		0
		(apply + (cons 1 (map card (children tree))))))

;----- (overlap u v) the overlap between two trees [Definition 5]
(define (overlap u v)
  (if (or (null? u) (null? v) (not (equal? (label u) (label v))))
		0
		(+ 1
			(apply + (map (lambda (x) (apply overlap x)) (cross* (children u) (children v)))))))

;------ (shadow u v) returns the part of v in the "shadow" cast by u
(define (shadow u v)
  'nyi)





;---- string->tree routines

;----- (filter-formatting s)

(define (reverse-string s)
  (list->string (reverse (string->list s))))


(define (filter-formatting S)
  (let ((excisions
			'(#\newline #\linefeed #\return
			  #\" #\'
			))
		  (replacements '((#\{ . #\() (#\} . #\)))))
	 (let* ((s0 (string->list S))
			  (s1 (!filter (lambda (x) (member x excisions)) s0))
			  (s2 (map (lambda (x) (let ((k (assq x replacements)))
											 (if k (cdr k) x)))
						  s1))
			  )
		(list->string s2))))

(define (strcar s) ;; expects a char, returns a char
  (if (string=? s "")
		#f
		(string-ref s 0)))

(define (strcdr s) ;; expects a string, returns a string
  (if (string=? s "")
		#f
		(let ((n (string-length s)))
		  (substring s 1 n))))

(define (strindex S ch)
  (cond
	((char? ch)
	 (let strindex-loop ((s S)
					(i 0))
		(cond
		 ((string=? s "") #f)
		 ((char=? (strcar s) ch) i)
		 (#t (strindex-loop (strcdr s) (+ i 1))))))
	((and (string? ch) (= (string-length ch) 1))
	 (strindex S (strcar ch)))
	((string? ch)
	 (map (lambda (c) (strindex S c)) (string->list)))))
	
	
(define (!strindex S ch)
  (cond
	((char? ch)
	 (let !strindex-loop ((s S)
					(i 0))
		(cond
		 ((string=? s "") #f)
		 ((not (char=? (strcar s) ch)) i)
		 (#t (!strindex-loop (strcdr s) (+ i 1))))))
	((and (string? ch) (= (string-length ch) 1))
	 (!strindex S (strcar ch)))
	(#t 'bad-character)
	((string? ch)
	 (map (lambda (c) (!strindex S c)) (string->list)))))
	
		
(define (strsub string old new)
  (let ((lold (string-length old))
		  (chold (string-ref old 0))
		  (lnew (string-length new))
		  (lstr (string-length string))
		  )
	 (let strsub-loop ((s "")
					(i 0))
		(if (>= i lstr)
			 s
			 (let ((si (string-ref string i)))
				(cond
				 ((not (char=? (string-ref string i) chold))
				  (strsub-loop (string-append s (list->string (list (string-ref string i))))
						  (+ i 1)))

				 ((> (+ i lold) lstr)
				  (string-append s (substring string i lstr)))

		 
				 ((string=? (substring string i (+ i lold)) old)
				  (strsub-loop (string-append s new) (+ i lold)))

				 (#t (strsub-loop (string-append s (list->string (list (string-ref string i))))
							  (+ i 1)))
				 )
				)))))
(define (strsub* string old new)
  (let strsub*-loop ((os string)
				 (s (strsub string old new))
				 )
	 (if (string=? os s)
		  s
		  (strsub*-loop s (strsub s old new)))))

(define (tidy-string-ends s)
  
  (let* ((s (strsub* s "  " " "))
			(z (substring s (!strindex s " ") (string-length s)))
			(d (reverse-string z))
			(b (substring d (!strindex d " ") (string-length d)))
			)
	 (reverse-string b)))

(define (rectify-tree-string S)
	(let ((S (tidy-string-ends
				 (strsub*
				  (strsub*
					(strsub* (filter-formatting (tidy-string-ends S))
								"  " " ")
					"( " "(")
				  " )" ")")
				 ))
			)
	  (strsub* S "( )" "()")))
  
(define (string->children C)
  (map string->node (map write-string (read-string C))))

(define (string->node S)
  (let ((s (rectify-tree-string S)))
	 (if (not (char=? (strcar s) #\())
		  'bad-node-string:lparen
		  (let* ((rs (strcdr s))
					(i (strindex rs "("))
					(poly (string->polynomial (substring rs 0 i)))
					(kinder (substring rs i (- (string-length rs) 1))))
			 (list poly (string->children kinder))
		))))
	 

(define string->tree string->node)

;---- Set restrictions from Definitions 7 and 8

;----- (L u) returns the set of labels associated with the elements of the set u [Definitions 7 & 8]
(define (L u)
  (cond
	((null? u) emptyset)
	((children? u) (map label u))
	((node? u) (map label (children u)))
	(#t '())))


;; ;----- (U_v_op f u v) is a generic routine for U_v and U_!v 
;; (define  (U_v_op f u v)
;;   (let* ((nu (node? u))
;; 			(nv (node? v))
;; 			(esu (if nu #f (children? u)))
;; 			(esv (if nv #f (children? v)))
;; 			)
;; 	 (cond
;; 	  ((or (null? u) (null? v))
;; 		'())

;; 	  ((and nu nv)
;; 		(U_v_op f (children u) (children v)))

;; 	  ((node? u) (U_v_op f (children u) v))
;; 	  ((node? v) (U_v_op u f (children v)))

;; 	  ((and esu esv)
;; 		(let* ((lu (map polynomial->string (L u)))
;; 				 (lv (map polynomial->string (L v)))
;; 				 (r (map cdr (f (lambda (x) (member (car x) lv)) (map cons lu u)))))
;; 		  ;;;(dnl "U labels: " lu)
;; 		  ;;;(dnl "V labels: " lv)
;;         ;;;(dnl r)
;; 		  r))
;; 	  (#t '()))))

;----- (L* tree) construct a recursive (label)
(define (L* t)
  (cond
	((null? t) '())
	((node? t) (cons (polynomial->string (label t)) (unique (map L* (children t) ))))
	(#t #f)))


;----- (U_v u v) the set of elements in the extension set of u which have a label in (L v) [Definitions 7 * 8]
;              (the labels in the extn set of v)
(define  (U_v u v)
  (U_v_op filter u v))


;----- (U_!v u v) the set of elements in the extension set of u which do not have a label in (L v) [Definitions 7 & 8]
(define  (U_!v u v)
  (U_v_op !filter u v))

;----- (UandV u v) the set of elements in the extension sets which have labels in both sets 

(define (UandV u v)
  (let ((UnV (list-intersection (map label (children u)) (map label (children v)))))
	 (map (lambda (x)
			  (let ((ux (child u x))
					  (vx (child v x)))

				 (filter (lambda (x y) (pair? x) (pair? y)) ux vx)))
			UnV)))



;--- Mathematical operators on trees from Definitions 6,9
;; ;---- (tree* u v)  Multiplication of trees by scalars  [Definition 6]
;; (define (tree* u v)
;;   (cond
;; 	((and (number? u) (number? v)) (* u v))
;; 	((and (number? u) (node? v))
;; 	 (node
;; 	  (p* u (label v)) (map (lambda (x) (tree* u x)) (children v))))
;; 	((and (number? v) (node? u))
;; 	 (tree* v u))
;; 	((and (node? u) (node? v))
;; 	 (error "Called scalar tree multiplication with two nodes" u v))
;; 	))

;; (define (T** u v)
;;   (cond
;; 	((and (number? u) (number? v)) (* u v))
;; 	((and (polynomial? u) (number? v)) (p* (list u) v))
;; 	((and (polynomial? v) (number? u)) (p* (list v) u))
;; 	((and (number? u) (node? v))
;; 	 (node
;; 	  (p* (list u) (label v))
;; 	  (map (lambda (x) (T** u x)) (children v))))
;; 	((and (number? v) (node? u))
;; 	 (T** v u))

;; 	((and (node? u) (node? v))
;; 	 (error "Called scalar tree multiplication with two nodes" u v))
;; 	))






(define (nodes-with-label L es)
  (let ((l (cond
				((string? L) L)
				((polynomial? L) (polynomial->string L))
				(#t (error "bad label passed to nodes-with-label" L)))))
	 (filter (lambda (x) (string=? (polynomial->string (label x)) l)) es)))




;---- (normalise-children S) returns an extension set which has all nodes with common labels added together
;; (define (normalise-children S)
;;   (map normalise-label
;; 		 (filter (lambda (x) (not (zero? (content x)))) 
;; 					(sort
;; 					 (cond
;; 					  ;; ((not (unique-children? S)
;; 					  ;;  S)
;; 					  ;;  (let ((ll (unique (map polynomial->string (!filter null? (map label S))))))
;; 					  ;; 	(if (equal? (length ll) (length S))
;; 					  ;; 		 S
;; 					  ;; 		 (let ((lst (map (lambda (x)
;; 					  ;; 								 ;;(dnl x ": " (nodes-with-label x S))
;; 					  ;; 								 (nodes-with-label x S)) ll)))
;; 					  ;; 			(sort (map (lambda (x) (apply tree+ x)) lst) node<?) ;; collect compatible trees, sort result
;; 					  ;; 			)
;; 					  ;; 		 )))
;; 					  ((children? S) S)
;; 					  (#t (error "Not a set of children!" S)))
;; 					 node<?)
;; 					)
;; 		 )
;;   )


;---- (T+ u v) add two trees together [Definition 9] 
;; 
;; We might consider treating two trees as weakly compatible when one or both polynomial labels 
;; are zero
;;

(define (factor-list p)
  (if (string? p)
		(factor-list (string->polynomial p))
		;;(map (lambda (x) (!filter number? x)) (!filter number? p)))
		(!filter number? p))
		
  )

(define (related? p1 p2)
  (let ((r (equal? (factor-list p1) (factor-list p2))))
;;	 (dnl p1 " R " p2 " --> " (factor-list p1) " R " (factor-list p2) " = " r)
	 r))

(define (related-children? c1 c2)
  (related? (label c1) (label c2)))


(define (p-related-s r p set)
  (apply orf (map (lambda (x) (r p x)) set)))

(define (partition-sets r u v . verbose)
  "In the returned value the car is the list of matches for u+v the cdr are the non-matches"
  (let ((L (list
				(append 	(filter (lambda (x) (p-related-s r (label x) (label v))) u)
							(filter (lambda (x) (p-related-s r (label x) (label u))) v))
				(append (!filter (lambda (x) (p-related-s r (label x) (label v))) u)
						  (!filter (lambda (x) (p-related-s r (label x) (label u))) v)))
			  ))
	 L
  ))


(define (partition-set* r u)
;;  (dnl "U: " u)
  (cond
	((null? u) u)
	((null? (cdr u))
	 u)
	(#	
		(let ((R (partition-sets r (list (car u)) u)))
		  (let ((rin (if (null? R) R (car R)))
				  (rout (if (null? R) R (cadr R))))
			 (if (pair? rin)
				  (set! rin (cdr rin)) ;; skip the doubled first entry
				  'bad-karma)

;;			 (dnl "RIN  " rin)
;;			 (dnl "ROUT " rout)
			 (cons rin (partition-set* r rout))))))
  )


;; This does not work.  It probably only normalises the polynomials

;; (define (normalise-set r op U) ;; r is usually related-polynomials? or related-children?, op is usually p+ or tree+
;;   (let ((s (!filter null? (map (lambda (x) (if (null? x) x (apply op x))) (partition-set* r U)))))
;; 	 (if (zero? (content s))
;; 		  emptyset
;; 		  s))
;;   )


;; ;; Add the children appropriately
;; (define (TSc+ . args) ;; This handles the addition of sets of polynomials according to the rule that we only really 
;;   (time-probe "TSc+"
;; 				  (if (null? args)  ;; add polynomials with the same indeterminant bits (like x^4)
;; 						'()
;; 						(!filter null? (normalise-set related-children? tree+ (!filter null? (apply append args)))))
;; 				  )
;;   )


;; (define (TSc+ . args)
;;   (let ((eqclist (eq-class^sigma (apply append args))))
;; 	 (map p+ eqclist))
;;   ) 'WRONG


;; (define (normalise-tree t)
;;   (let ((T	(cons (normalise-polynomial (label t))
;; 						(cons (TSc+ (map normalise-tree (children t))) '())))
;; 		  )
;; 	 (if (zero? (content T))
;; 		  zerotree
;; 		  T)))

(define (normalise-tree t)
  (if (not (tree? t))
		(begin
		  (set! booboo t)
		  (dnl "Bad tree\n" t))
		(let ((T (cons (normalise-polynomial (label t))
							(cons (map normalise-tree (sort (children t) node<?)) '())))
				)
		  (if (null? (car T))
				(set-car! T '(0)))
		  
		  (if #f
				(if (zero? (content T))
					 zerotree
					 T)
				T)
		  )))


(define (operator-curry op lst)
  (let ((n (length lst)))
	 (cond
	  ((null? lst) (op zerotree zerotree))
	  ((= n 1) (op (car lst) zerotree))
	  ((= n 2) (apply op lst))
	  (#t (op (car lst) (operator-curry op (cdr lst)))))))

(define (normalise-tree t)
  (if (not (tree? t))
		(begin
		  (set! booboo t)
		  (dnl "Bad tree\n" t))
		(let ((T (cons (normalise-polynomial (label t))
							(cons (map normalise-tree (sort (children t) node<?)) '())))
				)
		  (if (null? (car T))
				(set-car! T '(0)))
		  
		  (if #f
				(if (zero? (content T))
					 zerotree
					 T)
				T))))


(define (apply-chain op lst)
  (let ((n (length lst)))
	 (cond
	  ((null? lst) (op zerotree zerotree))
	  ((= n 1) (op (car lst) zerotree))
	  ((= n 2) (apply op lst))
	  (#t (op (car lst) (apply-chain op (cdr lst)))))))


;; (define (tree+ x y)
;; 	 (normalise-label
;; 	  (cond
;; 		((or (and (null? x) (null? y))
;; 			  (and (zerotree? x) (zerotree? y))) zerotree)
;; 		((or (null? x) (zerotree? x)) y)
;; 		((or (null? y) (zerotree? y)) x)
;; 		(#t
;; 		 ;;(dnl "SOP, (" (polynomial->string (label x)) ")(" (polynomial->string (label y)) ")")
;; 		 (list (p+ (label x) (label y))
;; 				 (let ((U (n-Lambda (append (children x) (children y)))))
;; 					(map (lambda (eqclass)
;; 							 (apply-chain tree+ eqclass))
;; 						  U))))))
;; 	 )

(define (tree2+ x y)
  (list (p+ (label x) (label y))
		  (sort (n-Lambda^sigma (union+ (children x) (children y))) node<?)))
;; Because the addition of trees incorporates the addition of children, adding a tree and its negative
;; yields a tree with an absolute magnitude of zero.

(define (tree+ . args)
  (case (length args)
	 ((0) zerotree)
	 ((1) (car args))
	 ((2) (apply tree2+ args))
	 (else (normalise-tree (tree2+ (car args) (apply tree+ (cdr args)))))))

(define (tree- . args)
  (case (length args)
	 ((0) zerotree)
	 ((1) (car args))
	 (else (apply tree+ (cons (car args) (map (lambda (x) (tree* -1 x)) (cdr args)))))))

			
(define (T/Z t)
  (let ((childs (sort (!filter (lambda (x) (or (null? x) (zero? (absolute-value x)))) (map T/Z (children t))) node<?)))
	 (dnl "label t:   " (label t))
	 (dnl "Chilluns:  " childs)
	 (dnl "Sum c:     " (apply + (map absolute-value childs)))
	 (dnl "Fchildren: " (!filter null? childs))
	 (if (zero? (apply + (map absolute-value childs)))
		  (and (display "Z ") (list (label t) emptyset))
		  (and (display "P ") (list (label t) (!filter null? childs)))
		  )))


;---- (absolute-value t)  returns the absolute magnitude of a tree [Definition 11]
(define (absolute-value t)
  (cond
	((null? t) 0)
	((children? t) (apply + (map absolute-value  t)))
	((zerotree? t) 0)
	((null? (children t))
	 (abs (weight t)))
	(#t (+ (abs (weight t)) (absolute-value (children t))))))

(define (type-tests x)
  (list 'number?  (number? x)
		  'polynomial? (polynomial? x)
		  'node?  (node?  x)
		  'children? (children? x)))

(define type-of-multiplication-for-children
  ;;  '*simple-children*
  ;;  'first-variant
    'experimental
  )

;---- (tree* . args) two argument multiplication of trees
(define (tree* . args)
  (define (ddnl . arg)
	 #!void
;;	 (apply dnl (cons "tree* " arg))
	 )

  (case (length args)
	 ((0) '())
	 ((1) (car args))
	 ((2) (let ((B (car args)) (C (cadr args)))
			  (cond
				((and (null? B)(null? C)) '())
				((null? C) B)
				((null? B) C)
				((and (number? B) (number? C))
				 (ddnl "numbers")
				 (* B C))
				
				((or (and (polynomial? B) (number? C))
					  (and (polynomial? B) (polynomial? C)))
				 (ddnl "polynomial and number, polynomial and polynomial")
				 (p* B C))
				
				((and (number? B) (node? C))
				 (ddnl "number and node")
				 (list
				  (p* B (label C))
				  (map (lambda (x) (tree* B x)) (children C))))
				
				((and (polynomial? B) (node? C))
				 (ddnl "polynomial and node")
				 (list
				  (p* B (label C))
				  (map (lambda (x) (tree* B) x) (children C))))

				((and (polynomial? B) (children? C))
				 (ddnl "polynomial and children")
				 (sort (map (lambda (x) (tree* B x)) C) node<? ))

				((and (number? B) (children? C))
				 (ddnl "polynomial and children")
				 (sort (map (lambda (x) (tree* (list B) x)) C) node<?)) ;; because we use multiplication by -1 to do subtraction

				((and (tree? B) (tree? C))
				 ;; (dnl "Two trees")
				 (let ((B B)
						 (C C))

					;;((or (zerotree? B) (zerotree? C))
					;; zerotree)
					(let* ((L (p* (label B) (label C)))
							 (CB (children B))
							 (CC (children C))
							 ;; (CB (sort (children B) node<?))
							 ;; (CC (sort (children C) node<?))
							 )
					  (normalise-tree (list L (sort
														(cond
														 ((and (null? B) (null? C)) '())
														 ((null? B) C)
														 ((null? C) B)
														 (else 
														  (case (if (symbol? multiplier) multiplier (car multiplier))
															 ((intersection+)
															  (n-Lambda^sigma (intersection-l*~ CB CC))
															  )
															 ((intersection)
															  (intersection-l*~ CB CC)
															  )
															 ((pairwise-*)
															  (map* tree* CB CC)
															  )
															 ((pairwise-multiplication)
															  (n-Lambda^sigma (map* tree* CB CC))
															  )
															 (else (error "bad multiplication selector" multiplier))
															 )))
														node<?)))
					  )))

				((or
				  (and (polynomial? C) (number? B))
				  (and (number? C) (node? B))
				  (and (polynomial? C) (node? B))
				  (and (number? C) (children? B) (not (null? (children B))))
				  (and (polynomial? C) (children? B) (not (null? (children B))))
				  )
				 (ddnl "swapping B and C")
				 (tree* C B))

				(#f (error "bad argument to tree*"))
				)))
	  (else (error "too many arguments to tree*" args))
	  )
	 )



;-- Routines for use by models

(define (tree-distance p q)
  (let ((tree (tree+ (tree* -1 p) q)))
	 (/ (absolute-value tree)
		 (card tree))))


;--- (tree->wordies t)
(define (tree->wordies t)
  (cons (polynomial->string (label t))
		  (list (map tree->wordies (children t)))))


;--- (tree->-string t . args) -- no formatting

;---
(define (tree->string t . indent)
			(string-append
			 "(" 
			 (cond
			  ((null? (label t)) "0 ")
			  ((string? (label t)) (label t))
			  (#t (polynomial->string (label t))))
			 (if (null? (children t))
				  (begin
					 " {}")
				  (children->string (sort (children t) node<?)))
			 ")"
	 ))


;--- (children->string es . indent) maps an extension set to a string
(define (children->string es . indent)
  (string-append
	(if (null? es)
		 " {}"		 
		 (string-append
		  " {"
		  (tree->string (car es))
		  (apply string-append
					(map (lambda (x)
							 (string-append " "
												 (tree->string x)))
						  (cdr es)))
		  " "
		  "}"))))

;--- (tree->display-string t . args) -- with formatting (but not latex)

;---
(define (tree->display-string t . indent)
  (set! indent (if (null? indent) 0 (car indent)))
  (string-append
	(tree->display-string% t indent)
	"\n"))

;---- (make-space n) makes a string n spaces long for indents
(define (make-space n)
  (make-string n #\space))

;---- (tree->display-string% t indent) This does the heavy lifting
(define (tree->display-string% t indent)
  (let ((emit-space #t))
	 (string-append
	  (if (null? t)
			""
			(string-append
			 "(" 
			 " \"" (if (string? (label t)) (label t) (polynomial->string (label t))) "\""
			 (if (null? (children t))
				  (begin
					 (set! emit-space #f)
					 " {}")
				  (children->display-string (children t) (+ 2 indent)))
			 (if emit-space (make-space (+ indent 1)) "")
			 ")"
			 )))
	 ))



;--- (children->display-string es . indent) maps an extension set to a string
(define (children->display-string es . indent)
  (set! indent (if (null? indent) 0 (car indent)))
  (string-append
	(if (null? es)
		 " {}"		 
		 (string-append
		  "\n"
		  (make-space indent)
		  "{"
		  (tree->display-string (car es) (+ 2 indent))
		  (apply string-append
					(map (lambda (x)
							 (string-append " "
												 (make-space (+ 2 indent))
												 (tree->display-string x (+ 2 indent))))
						  (cdr es)))
		  (make-space (+ 2 indent))
		  " "
		  "}\n"))))

;--- 

;; New trees (S)et of syms max-(c)onst max-(e)xp (tc)termcount (d)epth

;; (define (S* u v)
;;   (dnl "(car u) = " (car u))
;;   (dnl "(cadr u) = " (cadr u))
;;   (dnl "(car v) = " (car v))
;;   (dnl "(cadr v) = " (cadr v))

;;   (list (p* (car u) (car v))
;; 		  (S+	(map (lambda (x)
;; 						 (dnl "1st: " x " " (car u) " = "  (p* x (car u)))
;; 						 (p* x (car u))) (cadr v))
;; 				(map (lambda (x)
;; 						 (dnl "1st: " x " " (car v) " = "  (p* x (car v)))
;; 						 (p* x (car v))) (cadr u)))))




(define (random-dice n s)
  (if (or (zero? n) (< s 2))
		0
		(ceiling (/ (apply + (map (lambda (x) (random-integer s)) (seq n))) n))))




(define (dnlt t)
  (display (tree->display-string t)))

(define (dnlc t)
  (display (children->string t)))



(define (tdnl . args)
  (apply dnl 
			(map (lambda (x)
					 (cond
					  ((children? x) (children->string x))
					  ((node? x) (tree->display-string x))
					  ((polynomial? x) (polynomial->string x))
					  (#t x)))
				  args)))
			 




;-- Appendix: Miscellaneous routines

(define (tstdl rcg)
  (if (eq? (car rcg) 'ok)
		20160707 ;; success
		#f))



;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; " ***
;;; comment-end:""  ***

;;; End:

