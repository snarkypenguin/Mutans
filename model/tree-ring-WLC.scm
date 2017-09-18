;-*- mode: scheme; -*-
;-  Identification and Changes


;	pt.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.09.28
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/pt.scm
;
;	History:
;

;-  Discussion 

"
This implements the arithmetic operations on the ring of trees.
Implemented assuming chibi-scheme/R7RS or a similar implementation.

Test data is kept at the end of the file, since we might use
constructors to generate it.
"
;-  Configuration stuff 

;--  Variables/constants both public and static
(define tr-debugging #f)

(define (dnl* . args)
	 (apply dnl (apply append (map (lambda (x) (list x " ")) args))))

(define plus " + ") ;; used to denote polynomial addition 
(define minus " - ") ;; used to denote polynomial subtraction

(define multiplier ;; head of the list does the job
  '(
	 pairwise-multiplication
	 pairwise-*
	 intersection+
	 intersection
	 )
  )

(define filter-zero-terms #t)
(define filter-constant-factors #t)

;--- Error bits
;; This is ugly, but works in Gambit-c
(define booboo 'all-good)
(define ERROR error)
;(define (error str arg)
;  (set! booboo arg)
;  (err str arg))

;--- codepath tracking/timing [Comes first because it has macros]
(define time-register '())
(define collecting-times #f)
(define tr-serial-number 0)

;--- (time-probe name . body) macro
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

;--- (time-probe* name . body) macro
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

;--- Timer code instrumentation
;---- (reset-timer . arg)
(define (reset-timer . arg)
  (if (pair? arg) (set! collecting-times (car arg)))
  (set! time-register '())
  (set! tr-serial-number 0)
  )

;---- (only tag) filters list by tag string
(define (only tag) (lambda (x) (string=? tag (car x))))

;---- (timer-counts register) print counts in a timer register
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

;---- (timer-counts* register) print counts in a timer register
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
							(apply + (map (lambda (x) (car <(list-ref x 3)))
											  (list-ref split-bits i)))))
				  (seq (length bits)))
	 (dnl "\nNote that there is multiple counting going on with the times.")
	 ))




;--  Included files 
(load "maths.scm") ;; Includes utils.scm
;;(load "crossproduct.scm")

;-  Infrastructure and polynomial code

(define (equal*? x y . z)
  (and (equal? x y)
		 (or (null? z)
			  (apply equal*? (cons x z)))))

;-- direct product functions (op is list concatenation)
;--- (%cross2 a b) basic two element cross product
(define (%cross2 a b)
  (if (atom? b) (%cross2 a (list b))
		(apply append (map (lambda (x) (map (lambda (y) 
														  (list x y)) b)) a)))
  )

;--- (cross** . args) basic cross product using %cross2
(define (cross** . args)
  (define (c2 a b)
	 (%cross2 (car args) (cadr args)))

  (cond
	((null? args) '())
	((= (length args) 1)
	 (list-copy (car args)))
	((= (length args) 2)
	 (%cross2 (car args) (cadr args)))
	(#t
	 (map (lambda (x) (cons (car x) (cadr x))) (%cross2 (car args) (apply cross** (cdr args)))))
	))

;--- (map* mapping . args) Map with implicit cross product to construct args
(define (map* mapping . args)
  (map (lambda (args) (apply mapping args)) (apply cross** args)))
			  
;-- Extra list modifiers

;--- (set-cadr! lst bit)
(define (set-cadr! lst bit)
  (if (pair? lst)
		(set-car! (cdr lst) bit)
		(error "List must be a pair in set-cadr!")))

;--- (set-caddr! lst bit)
(define (set-caddr! lst bit)
  (if (and (pair? lst) (pair? (cdr lst)))
		(set-cadr! (cdr lst) bit)
		(error "List must have at least two elements in set-caddr!")))

;--- (list-insert! lst ix el ) ;;; just appends element if (< (length lst) ix)
(define (list-insert! lst ix el ) 
  (cond
	((null? lst) (error "List must be a pair in list-insert!"))
	((and (null? (cdr lst)) (>= ix 1))
	 (set-cdr! lst (list el)))
	((zero? ix)
	 (set-cdr! lst (cons (car lst) (cdr lst)))
	 (set-car! lst el))
	((positive? ix)
	 (list-insert! (cdr lst) (- ix 1) el))
	(#t (error "bad index passed to list-insert!" ix)))
  (void)
  )

;--- (list-delete! lst ix) deletes an element in a list. Ignores deletions past the end.
(define (list-delete! lst ix)
  (if (< ix (length lst))
		(cond
		 ((null? lst) (error "List must be a pair in list-insert!"))
		 ((and (zero? ix) (null? (cdr lst)))
		  '())
		 ((= 1 ix) 
		  (set-cdr! lst (cddr lst)))
		 ((zero? ix)
		  (set-car! lst (cadr lst))
		  (set-cdr! lst (cddr lst)))
		 (#t (list-delete! (cdr lst) (- ix 1))))
		lst)) ;; Probably ought to return an error... but we are trying to delete a phantom element.


(define (set-weight! t w)
  (cond
	((= 3 (length t))	(set-car! t w))
	((= 2 (length t))	(error "not yet implemented for node2"))
	(#t #f)
	))
	 
(define (set-label! t w)
  (cond
	((= 3 (length t))	(set-cadr! t w))
	((= 2 (length t))	(set-car! t w))
	(#t #f)
	))

(define (set-children! t w)
  (cond
	((= 3 (length t))	(set-caddr! t w))
	((= 2 (length t))	(set-cadr! t w))
	(#t #f)
	))




;-- String conversions
;--- (read-string str) converts a string to a list of symbols, numbers and lists.
(define (read-string str)
  (let* ((P (open-input-string str))
			(R (read P)))
	 (close-port P)
	 R))

;--- (write-string str) converts a list of symbols, numbers and lists to a string.
(define (write-string lst)
  (with-output-to-string '() (lambda () (write lst))))

;-- Arithmetic routines
;--- (power b e) a version that preserves exactness
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

;--- (sqr x) useful for map calls
;(define (sqr x) (* x x))
(define sqr square)

;--- Identity function
(define I (lambda (x) x))

;--- Boolean complement of the Identity function
(define !I (lambda (x) (not x)))

;--- random-integer wrapper that accepts a "0" argument silently accept non-positive values and return 0
(let ((ri random-integer))
  (set! random-integer
		  (lambda (x)
			 (if (and (integer? x) (positive? x))
				  (ri x)
				  0))))

;--- (n-arity-from-2-arity op identity) constructs analogs of "sum" from a "plus"
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

;-- type predicates and query functions
(define (Zero? x)
  (cond
	((or (number? x) (string? x)) (Zero? (canonical-label x)))
	((polynomial? x) (and (= (length x) 1) (= (car x) 0)))
	(#t #f)))

;--- (type%? x) returns the type (node2 and node3  are distinct) or #f
(define (type%? x)
  (cond
	((boolean? x) 'boolean)
	((null? x) 'null)
	((symbol? x) 'symbol)
	((number? x) 'number)
	((polynomial? x) 'polynomial)
	((node3? x) 'node3)
	((unique-node? x) 'unique-node)
	((node2? x) 'node2)
	((children? x) 'children)
	((list? x) 'list)
	((pair? x) 'pair)
	((procedure? x) 'procedure)
	((char? x) 'char)
	(#t #f))
  )

;--- (type? x) returns the type (all types of nodes are returned as 'node) or #f
(define (type? x)
  (cond
	((boolean? x) 'boolean)
	((vector? x) 'vector)
	((null? x) 'null)
	((symbol? x) 'symbol)
	((number? x) 'number)
	((and (list? x) (apply andf (map number? x))) 'Vector)
	((polynomial? x) 'polynomial)
	((node3? x) 'node)
	((unique-node? x) 'unique-node)
	((node2? x) 'node)
	((children? x) 'children)
	((list? x) 'list)
	((pair? x) 'pair)
	((procedure? x) 'procedure)
	((char? x) 'char)
	(#t #f)))

;--- (Type? x) Wrapper that returns 'tree
(define (Type? x)
  (cond
	((node3? x)
	 (if (Zero? (label x)) 'tree3 'node3))
	((node2? x) 'tree2)
	(#t (type? x))))

;-- Supporting code: string and list utilities, simple polynomial arithmetic
;--- Mostly convenience functions, utilities
;---- display with implicit newline
(define (dnl . args) (if (null? args) (display "") (let () (map display args) (newline))))
(define (DNL . args) (if debugging (apply dnl args)))

;---- (ddnl . args) a version of dnl that only works if tr-debugging is true
(define (ddnl . args)
  (if tr-debugging
		(apply dnl args)))

;---- functional versions for and and or
;----- (andf . args) can be used with apply 
(define (andf . args)
  (if (null? args)
		#t
		(and (car args) (apply andf (cdr args)))))

;----- (orf . args) can be used with apply 
(define (orf . args)
  (if (null? args)
		#f
		(or (car args) (apply orf (cdr args)))))

;--- list routines, filter and map routines 
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

;----- (pick-only type x) picks particular types of object from a list
(define (pick-only type x)
  (if (not (list? x))
		(if (type x)
			 '())
		(filter I (map type x))))

;---- (list-intersection a b) intersection of lists a and b
; functionally identical to (list-intersection% = (lambda (x) (x) A B))
(define (list-intersection A B)
  (if (not (pair? A)) (set! A (list A)))
  (if (not (pair? B)) (set! A (list B)))
  (let ((f (filter (lambda (x) (member x B)) A)))
	 (if (null? f)
		  '()
		  f)))

;---- (list-intersection* a b) intersection 
;; functionally identical to (list-intersection%* = (lambda (x) (x)) ....)
(define (list-intersection* . args )
  (case (length args)
	 ((0) '())
	 ((1) (car args))
	 ((2) (list-intersection (car args) (cadr args)))
	 (else (list-intersection (car args) (apply list-intersection* (cdr args))))))

;---- (list-intersection% op selector a b) key-based intersection of lists a and b
; op is a predicate, selector extracts things from a and b
(define (list-intersection% op selector A B)
  (if (not (pair? A)) (set! A (list A)))
  (if (not (pair? B)) (set! A (list B)))
  (let ((f (filter (lambda (x) (op (selector x) (map selector B))) A))
		  (g (filter (lambda (x) (op (selector x) (map selector A))) B)))
	 (union+ f g)))

;---- (list-intersection%* a b) intersection of lists 
; op is a predicate, selector extracts things from a and b
(define (list-intersection%* op selector . args )
  (let ((li?* (lambda x (apply list-intersection%* (cons op (cons selector x))))))
	 (case (length args)
		((0) '())
		((1) (car args))
		((2) (list-intersection% op selector (car args) (cadr args)))
		(else (list-intersection% op selector (car args) (apply li?* (cdr args)))))))

;--- String utilities
;---- (string-head s n (string? integer?)) returns the first part of a string
(define (string-head s n)
  (let ((N (string-length s)))
	 (cond
	  ((zero? n) "")
	  ((< n N) (substring s 0 n))
	  (#t s))))

;---- (string-tail s n (string? integer?)) returns the last part of a string
(define (string-tail s n)
  (let* ((N (string-length s))
			(k (- N n)))
	 (cond
	  ((zero? n) "")

	  ((and (>= k 0) < k n)(substring s k N))
	  (#t s))
	 ))

;---- (string-car s) like car, but for strings
(define (string-car s)
  (string-head s 1))

;---- (string-cdr s) like cdr, but for strings
(define (string-cdr s)
  (string-tail s (- (string-length s) 1)))

;---- (string-contains? str . targets) 
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

;---- (locate-substring str substr)
(define (locate-substring str substr)
  (let ((n (string-length substr))
		  (N (string-length str))
		  )
	 (let locatestr-loop ((k 0))
		(cond
		 ((< N (+ k n) ) #f)
		 ((string=? (substring str k (+ k n)) substr) k)
		 (#t (locatestr-loop (+ k 1)))))))

;---- (string-replace string target replacement) replaces firstinstance
(define (string-replace s t r) ;string target replacement
  (let ((k (locate-substring s t)))
	 (if (not k)
		  s
		  (string-append (substring s 0 k)
							  r
							  (substring s (+ k (string-length t)) (string-length s))))))

;---- (string-replace* string target replacement) replaces all instances
(define (string-replace* s t r)
  (let ((x (string-replace s t r)))
	 (if (string=? x s)
		  s
		  (string-replace* x t r)
		  )))

;---- (strcspn str set) returns index of first char not in set
(define (strspn str set)
  (let strspan-loop ((s str))
	 (if (zero? (string-length s))
		  (string-length str)
		  (if (let inner-strspan-loop ((chset set))
				  (if (zero? (string-length chset))
						#f
						(if (equal? (string-ref s 0)
									(string-ref chset 0))
							 #t
							 (inner-strspan-loop (substring chset 1 (string-length chset))))))
				(strspan-loop (substring s 1 (string-length s)))
				(- (string-length str) (string-length s))))))


;---- (strcspn str set) returns index of first char in set
(define (strcspn str set)
  (let strcspan-loop ((s str))
	 (if (zero? (string-length s))
		  (string-length str)
		  (if (let inner-strcspan-loop ((chset set))
				  (if (zero? (string-length chset))
						#t
						(if (equal? (string-ref s 0)
									(string-ref chset 0))
							 #f
							 (inner-strcspan-loop (substring chset 1 (string-length chset))))))
				(strcspan-loop (substring s 1 (string-length s)))
				(- (string-length str) (string-length s))))))

;---- (collapsing-strtok str . separator)  analogous to strtok(3)
;; This silently collapses multiple instances of either spaces or the indicated separator
;; eats extra separators
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

;---- (strtok str . separator)  analogous to strtok(3)
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


;---- (define (reconstruct-string strarray . separator) 
;; reconstructs the string either with spaces or the indicated separator  (the obverse of strtok)
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

;---- (simple-split-string-at-separator str . separatorstring) similar to strtok, but with a "substring"
;; Like strtok, but it separates with a string rather than a set of characters
;; separator defaults to ","
(define (simple-split-string-at-separator str . separatorstring)
  (let ((sep (if (null? separatorstring)
					  ","	(car separatorstring))))
	 (let ((ns (string-length sep)))
		(let split-loop ((s str)
					  (r '()))
		  (let ((n (locate-substring s sep)))
			 (if n
				  (split-loop
				 	(substring s (+ n ns) (string-length s))
					(cons (substring s 0 n) r))
				  (reverse (cons s r))))))))

;---- (simple-split-string-at-separator+ str . separatorstring) similar to simple-split-string-at-separator
;; Like simple-split-string-at-separator, but includes the instances of the separatorstring in the list
;; separator defaults to ","
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

;---- (next-split str separatorstrings) used by split-string-at-separators and split-string-at-separators+
(define (next-split str separatorstrings)
  (let* ((d (map (lambda (x) (locate-substring str x)) separatorstrings))
			(pairings (map cons d separatorstrings))
			(dl (filter number? d))
			(k (if (null? dl) #f (assoc (apply min (filter number? d)) pairings)))
			)
	 k))

;---- (split-string-at-separators str separatorstrings) like simple-split-string-at-separator but takes more than one possible separator string
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

;---- (split-string-at-separators+ str separatorstrings) completes the pattern
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

;---- (erode-whitespace string) erode spaces at leading and trailing ends of  strings in lists
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

(define (poly-zero? x)
  (or (and (number? x) (zero? x))
		(and (polynomial? x)
			  (zero? (scalar-term x))
			  (apply andf (map zero? (map scalar-term (cdr x)))))))
		

;---- (polynomial item) make an object into a polynomial
(define (polynomial item)
  (cond
	((symbol? item) (symbol->polynomial item))
	((string? item) (string->polynomial item))
	((polynomial? item) item)
	((term? item) (list item))
	((factor? item) (list (list item)))
	(#t item)))



;--- Predicates: constant? variable? factor? term? polynomial?
;---- (constant? x) boolean which is true if the normalised polynomial argument is constant
(define (constant? x) (or (number? x) (and (pair? x) (number? (car x)) (null? (cdr x)))))

;---- (Constant? x) boolean which is true if the polynomial argument is constant, 
(define (Constant? x) (or (constant? x)
								  (and (pair? x)
										 (or (and (pair? (car x) (zero? (cadar x))))
											  (and (number? (car x))
													 (pair? (cadr x))
													 (symbol? (caadr x))
													 (zero? (cadadr x)))))))
															
;---- (variable? x) returns true if the argument is a symbol with an exponent
(define (variable? x) (and (list? x) (= 2 (length x)) (symbol? (car x)) (number? (cadr x))))

;---- (factor? x) returns true if the argument is a constant or a variable
(define (factor? x)
  (and (not (or (procedure? x) (vector? x) (boolean? x)))
		 (or (constant? x)	(variable? x))))

;---- (term? x) returns true if the argument is a list of factors
(define (term? x)
  (and (not (or (procedure? x) (vector? x) (boolean? x)))
		 (or (constant? x)
			  (and (pair? x) (apply andf (map (lambda (f) (factor? f)) x))))))

;---- (polynomial? x) returns true if the argument is a list of terms
(define (polynomial? x)
  (and (not (or (procedure? x) (vector? x) (boolean? x)))
		 (or (number? x)
			  (and (pair? x) (apply andf (map term? x))))))

;---- Utilities for manipulating parts of polynomials

;-----  (flatten-constant P)
(define (flatten-constant P)
  (if (and (pair? P) (null? (cdr P))
			  (pair? (car P)) (null? (cdar P))
			  (number? (caar P)))
		(car P)
		P)
;;  P
  )


;----- (remap-constant x) maps a complicated constant term to a simpler form
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

;----- (rectify x) wraps a bare number in a cons 
(define (rectify x)
  (if (number? x) (list x) x))

;----- (rectify-numbers lst) fixes all bare numbers in a list (there may be more than one constant factor)
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

;---- Comparison operators and sorting
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
;------ (factor-ordering a b) returns a symbol indicating the relative ordering
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
		
;------ (factor<? a b)  is the "less than" comparison operator for sorting factors within a term
;  This is a single multiplicative element like "4" or (x 2) === "x^2"
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

;------ (term-ordering a b) returns a symbol indicating relative ordering (totally excludes constants)
(define (term-ordering a b) 
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

;------ (term<? a b)  possible side effects! the "less than" comparison operator may also sort the factors in the terms
;; this is not reliable if the factors in the term are not sorted
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

;------ (index-in lst key)
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

;------  (normalise-term t) fixes the order of the factors in the term and collects factors (adjust exponents)
(define (normalise-term t) 
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

;------ (normalise-terms x) collects common terms and puts the terms in a canonical order.
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

;----- node comparisons
;------  (sort-children! t) This is an in situ sort!
(define (sort-children! t)
  (if (not (null? children))
		(sort! (children t) node<?)))

;------ (probe . args)  tracking code
(define (probe . args)
  ;;(dnl args)
  #t)

;------ (node<? n m)
;------  (node<? n m)
(define (node<? n m)
  (define (ndl . arg)
	 (if #f
		  (begin
			 (apply dnl (cons '--> arg))
			 (display "n: ")(pp n)
			 (display "  label = ") (label n)
			 (display "m: ")(pp m)
			 (display "  label = ") (label m)))
	 #!void
	 )

  (if #t
		(string<? (tree->string n) (tree->string m))
		(begin
		  (ndl '------------------------------------------------<--)
		  (cond
			((not (and (node? n) (node? m)))
			 (error "Passed a non-node to node<?" n m))
			((and (probe 'A) (null? n) (null? m)) (ndl 1) #f)
			((and (probe 'B) (null? n) (not (null? m))) (ndl 2) #t)
			((and (probe 'C) (not (null? n))  (null? m)) (ndl 3) #f)
			((and (probe 'D " " (label n) " || " (label m)) (polynomial<? (label n) (label m))) (ndl 4) #t)
			((and (probe 'E) (polynomial<? (label m) (label n))) (ndl 5) #f)
			;; ... so labels must be equal

			((not (and (null? (children n)) (null? (children m)))) ;; not empty 
			 (ndl 6) 
			 (cond
			  ((and (probe 'F)  #f) #f)
			  ((null? (children n)) (ndl 7) #t)
			  ((null? (children m)) (ndl 8) #f)
			  (#t
				(sort-children! n)
				(sort-children! m)

				(let loop ((cn (children n))
							  (cm (children m))
							  )
				  (ndl "loop")
				  (cond
					((and (probe 'G) #f) #f)
					((and (null? cn) (null? cm)) (ndl 9) #f)
					((null? cn) (ndl 10) #t)
					((null? cm) (ndl 11) #f)
					((node<? (car cn) (car cm)) (ndl 12) #t)
					((node<? (car cm) (car cn)) (ndl 13) #f)
					(#t (loop (cdr cn) (cdr cm))))))))

			;; must have equal weights
			((< (absolute-value n) (absolute-value m)) (ndl 14) #t)
			((> (absolute-value n) (absolute-value m)) (ndl 15) #f)
			;; must have the same magnitude
			((< (card n) (card m)) (ndl 16) #t)
			((> (card n) (card m)) (ndl 17) #f)
			;; must have the same cardinality
			((< (depth n) (depth m)) (ndl 18) #t)
			((> (depth n) (depth m)) (ndl 19) #f)
			;; must have the same depth
			(#t (ndl 20) #f)))
		))

;------- (node=? n m)
(define (node=? n m)
  (if #t
		(equal? n m)
		(or ;; rubbery version
		 (if #t
			  (and (not (node<? n m)) (not (node<? m n)))
			  (and
				(or (and (null? n) (null? m))
					 (and (node? n) (node? m)))
				(polynomial=? (label n) (label m))
				(= (weight n) (weight m))
				(= (absolute-value n) (absolute-value m))
				(= (card n) (card m))
				(= (depth n) (depth m))
				))
		 ))
  )
  
;----- tree=? is an alias for node=?
(define tree=? node=?)


;----- various predicates for filtering
;------  (label-constants p)
(define (label-constants p)
  (filter number? (label p)))

;------  (label-ind-factors p)
(define (label-ind-factors p)
  (!filter number? (label p)))

;------  (constant-label? p)
(define (constant-label? p)
  (cond
	((number? p) #t)
	((and (list? p) (number? (label p))) #t)
	((list? p) (= (length (label-constants p))
					  (length (label p))))
	(#t #f)))

;------ (zero-term? t) predicate indicating if a term contributes nothing
(define (zero-term? t)
  (cond
	((null? t) #t)
	((and (number? t) (zero? t)) #t)
	((number? t) #f)
	((and (pair? t) (zero? (car t))) #t)
	(#t #f)))

;------  (the-non-zero-constant-terms x)
(define (the-non-zero-constant-terms x)  ;; Applies to *terms*, not *factors*
  (cond
	((and (pair? x) (null? (cdr x)) (number? (car x)) (not (zero? (car x))))
	 (car x))
	((and (number? x) (not (zero? x))) x)
	(#t #f)))

;------  (the-non-zero-indeterminate-terms x)
(define (the-non-zero-indeterminate-terms x)  ;; Applies to *terms*, not *factors*
  (if (and (pair? x)
			  (number? (car x))
			  (not (zero? (car x)))
			  (car x))
		x
		#f))

;------  (the-indeterminate-terms x)
(define (the-indeterminate-terms x)  ;; Applies to *terms*, not *factors*
  (if (and (pair? x)
			  (number? (car x))
			  (car x))
		x
		#f))

;------  (the-zero-terms x)
(define (the-zero-terms x)  ;; Applies to *terms*, not *factors*
  (or	(and (number? x) (zero? x))
		(and (pair? x) (number? (car x)) (zero? (car x)) (car x))
		))

;------  (the-constant-terms x)
(define (the-constant-terms x)  ;; Applies to *terms*, not *factors*
  (or (and (number? x) x)
		(and (pair? x)
			  (and (number? (car x))
					 (pair? (cdr x))
					 (zero? (cadr (cadr x))))
			  (car x)
			  )))

;------  (the-non-zero-indeterminate-factors x)
(define (the-non-zero-indeterminate-factors x)  ;; Applies to *factors*, not *terms*
  (if (and (pair? x)
			  (number? (car x))
			  (not (zero? (cadr x)))
			  (car x)
			  )
		x
		#f))

;------  (the-indeterminate-factors x)
(define (the-indeterminate-factors x)  ;; Applies to *factors*, not *terms*
  (if (and (pair? x)
			  (number? (car x))
			  (car x)
			  )
		x
		#f))

;------  (the-constant-factors x)
(define (the-constant-factors x)  ;; Applies to *factors*, not *terms*
  (or (and (number? x) x)
		(and (pair? x)
			  (and (number? (car x))
					 (zero? (cadr x)))
			  (car x)
			  )))

;----- (trees-with-labels trees labels)
(define (trees-with-labels trees labels)
  (filter (lambda (x) (member (label x) labels)) trees))

;----- (trees-with-similar-labels trees labels)
(define (trees-with-similar-labels trees labels)
  (filter (lambda (t)
				(not (null? (filter (lambda (x) (sim? (label t) x)) labels))))
			 trees))

;----- (rewrite-constant-indeterminate x)
(define (rewrite-constant-indeterminate x)
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

;---- manipulation routeins for polynomials and their kin
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


;---- arithmetic ops for polynomials
;----- (p+ . args)  polynomial addition ... too simple for words
(define (p+ . args)
  (normalise-polynomial (apply append (map polynomial (map rectify args)))))

;----- (_t* a b) multiplies terms in polynomials
;;; (define (_t* a b) ;;;
;;;   (define (__t* . args) ;;;
;;; 	 ;;  (if (not (apply andf (map term? args))) (error "non-term passed to _t*" (list args (map term? args)))) ;;;
;;; 	 (rectify-numbers ;;;
;;; 	  (normalise-terms (apply append (map (lambda (x) (if (pair? x) x (list x))) args))))) ;;;
;;;   (__t* a b)) ;;;

;-----  (ensure-list lst)
(define (ensure-list lst)
  (map (lambda (x) (if (list? x) x (cons x '()))) lst))

;-----  (t* t1 t2)
(define (t* t1 t2)
  (define (fix x) (if (not (pair? x)) (list x) x))
  (normalise-term (append (fix t1) (fix t2))))

;-----  (@t* x)
(define (@t* x)
  (apply t* x))

;-----  (bad-p2* a b)
(define (bad-p2* a b)
  (let ((a (if (number? a) (list a) a))
		  (b (if (number? b) (list b) b)))
	 (normalise-polynomial (map (lambda (x) (apply t* x)) (map* (lambda x x) a b)))))

;-----  (p2* a b)
(define (p2* a b)
  (let ((a (if (pair? a) a (list a)))
		  (b (if (pair? b) b (list b))))
	 (normalise-polynomial
	  (map* (lambda (x y)
				 (t* x y)) a b))))

;-----  (p* . args)
(define (p* . args)
  (let ((args (map polynomial args)))
	 (case (length args)
		((0) '(1))
		((1) (car args))
		((2) (apply p2* args))
		(else (normalise-polynomial (p2* (car args) (apply p* (cdr args)))))))
  )

;-----  (sp* . args) (apply p* (map string->polynomial args)))
(define (sp* . args) (apply p* (map string->polynomial args)))

;---- (evaluate-polynomial P codex)

(define (substitute-factor f codex)
  (if (number? f)
		f
		(let ((s (assq (car f) codex)))
		  (if s (list (cdr s) (cadr f)) f))))

;-----  (substitute-term t codex)
(define (substitute-term t codex)
  (if (number? t)
		t 
		(map (lambda (x) (substitute-factor x codex)) t)))

;-----  (substitute-polynomial P codex)
(define (substitute-polynomial P codex)
  (map (lambda (x) (substitute-term x codex)) P))

;-----  (evaluate-factor f)
(define (evaluate-factor f)
  (if (number? f)
		f
		(apply power f)))

;-----  (evaluate-term t)
(define (evaluate-term t)
  (if (number? t)
		t
		(apply * (map evaluate-factor t))))

;-----  (evaluate-polynomial P codex)
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


;----- (symbol->polynomial pstr) 

(define (symbol->polynomial sym)
  (list (list 1 (list sym 1))))


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

;-----  (n->s n form)
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

;- *** Code for rings of tree elements (nodes) ***
"
Nodes are lists of the form 

      (weight polynomial set-of-child-nodes) [node3]

or 

      (polynomial set-of-child-nodes)        [node2]

and trees are really just collections of related nodes where there is
a root (the node which is (uniquely) the child of no other node in the set).
"

(define use-caution #t)
(define node list) ;; makes it easier to read

;-- Important "constants": emptyset, zerotree

(define emptyset '{})
(define zerotree (node 0 0 emptyset))  ;; [Definition 1]
(define onetree (node 1 0 emptyset))  ;; [Definition 1]


;-- Accessors: weight label extension extension-set child children

;---  label and children

;----  (weight n) 
(define (weight v)
  (cond
	((null? v) 0)
	((= (length v) 2) (scalar-term (label v)))
	((= (length v) 3) (car v))
	(#t (bad argument))))
  
;----  (label v) 
(define (label v)
  (cond
	((null? v) 0)
	((= (length v) 2) (car v))
	((= (length v) 3) (cadr v))
	(#t (bad argument))))

;----  (scalar-term lbl) 
(define (scalar-term lbl)
  (apply * (filter number? lbl)))

(define (content n)
  (let ((N (length n)))
	 (cond
	  ((null? n) 0)
	  ((zero? N) 0)
	  ((= 2 N) (content2 n))
	  ((= 3 N) (content3 n))
	  (#t #f))))

;----  (content2 n) 
(define (content2 n) ;; scores 1 for each non-zero label
  (cond
	((null? n) 0)
	((node? n)
		(let* ((l (label n))
				 )
		  (apply +
					(cons
					 (+ (if (or (null? l) (member l '(0 (0) ((0)) 0.0 (0.0) ((0.0))))) 0 1)
						 )
					 (map content2 (children n))))))
	((children? n)
	 (apply + (map content2 n)))
	(#t
	 (dnl "The object passed to content is not a tree or treelike object!" (tree->string n))
	 #f)))
						 

;----  (content3 n) 
(define (content3 n) ;; scores 1 for each non-zero weight
  (cond
	((null? n) 0)
	((node? n)
		(let* ((l (weight n))
				 )
		  (apply +
					(cons
					 (+ (if (or (null? l) (member l '(0 (0) ((0)) 0.0 (0.0) ((0.0))))) 0 1)
						 )
					 (map content3 (children n))))))
	((children? n)
	 (apply + (map content3 n)))
	(#t
	 (dnl "The object passed to content is not a tree or treelike object!" (tree->string n))
	 #f)))
						 

;--- (children<- . args) construct a set of children
(define (children<- . kids)
  (if (apply andf (map node? kids))
		(list-copy kids)
		(error "Bad element(s) in children" kids)))

;--- (children v) returns the set of children of v
(define (children v)
  (cond
	((null? v) '())
	((and (= (length v) 2) (polynomial? (label v)))  (cadr v))
	((and (= (length v) 3) (polynomial? (label v)))  (caddr v))
	(#t (error "requested a set of children from the wrong kind of object" v))))

;--- (child v l) the element of the set of children of v with a label l, or the empty set
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

;----  (union . args) 
(define (union . args)  (unique (apply append args)))  ;; makes it nice and readable for set operations

;----  (union+ . args) 
(define (union+ . args)  (apply append args))          ;; like union, but preserves multiplicity

;--- (intersection op sel . args)
;; op is a function of the form (pred key matchlist) and returns #t if key is in matchlist
(define (intersection op sel . args)
  (let* ((s (map (lambda  (x) (map sel x)) args))
			(S (apply list-intersection* s))
			(candidates (apply union+ args))
			)
	 (filter (lambda (x) (op x S)) candidates)))
  

;----  (intersection-l*= . args) 
(define (intersection-l*= . args) ;; the intersection of the sets is the set of elements which have labels which are identical
  (apply intersection (cons trees-with-labels (cons label args))))

;----  (intersection-l*~ . args) 
(define (intersection-l*~ . args) ;; the intersection of the sets is the set of elements which have labels which are similar
  (apply intersection (cons trees-with-similar-labels (cons label args))))

;-- Predicates: zerotree? node? tree? simple-node? children?

;--- (zerotree? t) returns #t if t is the zerotree.  We need this because "(equal? '(0 0 '()) zerotree)" may return false
(define (zerotree? t)
  (if (string? t) (set! t (string->node t)))
  (cond
	((not (node? t)) (error "bad call to zerotree?" t))
	((null? t) #f)
	((node3? t) (and 
					 (zero? (weight t)) (poly-zero? (label t))
					 (null? (children t))))
	((node2? t) (and 
					 (poly-zero? (label t))
					 (null? (children t))))
	))


(define (null0? x) (or (null? x) (equal? zerotree x)))

(define (node? n)
  (or (node2? n)
		(node3? n)))

;--- (node2? n) and (tree? n) 
(define (node2? n)
  (cond
	;;((null? n) #t) ;; No, a null list is not equivalent to the zerotree.
	((not (pair? n)) #f)
	(#t (and (= (length n) 2)
				(polynomial? (label n))
				(or (null? (children n))
					 (and (pair? (children n))
							(apply andf (map node2? (children n)))))
				))
	))

;--- (node2? n) and (tree? n) 
(define (node3? n)
  (cond
	;;((null? n) #t) ;; No, a null list is not equivalent to the zerotree.
	((not (pair? n)) #f)
	(#t (and (= (length n) 3)
				(number? (weight n))
				(polynomial? (label n))
				(or (null? (children n))
					 (and (pair? (children n))
							(apply andf (map node3? (children n)))))
				))
	))

(define tree? node?)

;--- (simple-node? n)  node with empty set of children
(define (simple-node? n)
  ;;(and (= (length n) 1) (polynomial? (label n)) (null? (children n)))
  (and (node? n) (null? (children n)))
  )

;--- (unique-children? E)
(define (unique-children? E)
  (cond
	((null? E) #t)
	((and (apply andf (map unique-node? E))
			(let ((lbls (map polynomial->string (map label E))))
			  (= (length lbls) (length (unique lbls)))))
	 #t)
	(#t #f)) )

;--- (unique-node? n)
(define (unique-node? n)
  (and (node? n)
		 (unique-children? (children n))))

;--- (children? E) 
;; They may have several identical children, for example.
(define (children? E)
  (cond
	((null? E) #t)
	(#t (apply andf (map node? E)))
	))

;----  (make2-zerotree D) 
(define (make2-zerotree D) ;; generates a member of the equivalence class of (0 {})
  (if (not (and (not (negative? D)) (integer? D)))
		(error "bad argument to random-zerotree" D))
  
  (list 0
		  (if (<= D 0)	'() ;; We have bottomed out depth wise
				(list (make-zerotree (- D 1))))))

;----  (make3-zerotree D) 
(define (make3-zerotree D) ;; generates a member of the equivalence class of (0 {})
  (if (not (and (not (negative? D)) (integer? D)))
		(error "bad argument to random-zerotree" D))
  
  (list 0 0
		  (if (<= D 0)	'() ;; We have bottomed out depth wise
				(list (make-zerotree (- D 1))))))

;-- Support for node3 forms
;--- (UrestrictedtoV U V)
(define (U-restricted-to-V U V) ;; U\V_L
  (let ((u (if (node? U) (children U) U))
		  (v (if (node? V) (children V) V)))
	 (let ((vl (map label v)))
		(filter (lambda (x) (member (label x) vl)) u))))

;----  (U-restricted-to-NOT-V U V) 
(define (U-restricted-to-NOT-V U V) ;; U\V_L
  (let ((u (if (node? U) (children U) U))
		  (v (if (node? V) (children V) V)))
	 (let ((vl (map label v)))
		(filter (lambda (x) (not (member (label x) vl))) u)))
  )

;----  (U-and-V U V) 
(define (U-and-V U V)
  (let ((u (if (node? U) (children U) U))
		  (v (if (node? V) (children V) V)))
	 (let ((L (intersection (map label u) (map label v))))
		(list (filter (lambda (x) (member (label x) L)) u) 
				(filter (lambda (x) (member (label x) L)) v))))
  )

;-- Simple mathematical functions  

;--- (n-lambda poly) generates a list of the indeterminate factors in a polynomial
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

;----  (sim? u v) 
(define (sim? u v) ;; similar nodes have polynomials that are related (nodes or polys)
  (if (and (node2? u) (node2? v))
		(equal? (n-lambda u) (n-lambda v))
		(and (node3? u) (node3? v) (equal? (label u) (label v)))))

;----  compatible? 
(define compatible? sim?)

;----  (eq-class? s) 
(define (eq-class? s)
  (cond
	((null? s) #t)
	((not (pair? s)) #f)
	((polynomial? (car s)) (apply andf (map polynomial? s)))
	(#t (children? s))))

;----  (eq-class-set? S) 
(define (eq-class-set? S)
  (apply andf (map eq-class? S)))

  

;----  (eq-class r U) 
(define (eq-class r U) ; equiv class in U corresponding to s
  (let* ((c (n-lambda r))
			(C (filter (lambda (x) (sim? x r)) U)))
	 (if (eq-class-set? C)
		  C
		  (error "bad result in eq-class"C))))

;----  (eq-class^sigma s U) 
(define (eq-class^sigma s U) ; sum of equiv class in U corresponding to s
  (let ((r (apply (if (polynomial? s) p+ tree+) (eq-class s U))))
	 (cond
	  ((or (polynomial? r) (tree? r)) r)
	  (#t (error"bad result in eq-class^sigma" r))
	  )))

;----  (n-Lambda U) 
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
		 

;----  (n-Lambda^sigma U) 
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

;--- Functions from Definitions 2,3 and 4

;---- (depth tree)  returns the depth of the tree [Definition 2]
(define (depth tree)
  (cond
	((not (tree? tree)) #f)
	((or (null? tree) (zerotree? tree)) 0)
	((null? (children tree)) 1)
	(#t (+ 1 (apply max (map depth (children tree)))))) )

;---- (trim tree) returns a tree with all simple nodes excised [Definition 3]
(define (trim tree)
  (if (simple-node? tree)
		tree
		(list (label tree) (!filter null? (map trim (children tree))))))

;---- (card tree) returns the cardinality of the tree (the number of non-zero nodes [Definition 4]
(define (card tree)
  (if (zerotree? tree)
		0
		(apply + (cons 1 (map card (children tree))))))

;---- (overlap u v) calculates the overlap between two trees [Definition 5]
(define (overlap u v)
  (if (or (null? u) (null? v) (not (equal? (label u) (label v))))
		0
		(+ 1
			(apply + (map (lambda (x) (apply overlap x)) (cross** (children u) (children v)))))))

;----- (shadow u v) returns the part of u in the "shadow" cast by v
(define (shadow u v)
  (cond
	((null0? v)
	 u)
	((null0? u)
	 u)
	((= 3 (length u) (length v))
		(if (equal? (label u) (label v))
			 (list (weight u) (label u) (!filter null0? (map (lambda (x) (apply shadow x)) (cross** (children u) (children v)))))
			 '())
		)
	(#t (error "Bad arguments passed to shadow" u v))))
		


;--- tree mutators (and a supporting routine)

;---- (canonical-label L) supports a string specification of the labels in code
(define (canonical-label L)
  (cond
	((number? L) (list L))
	((string? L) (string->polynomial L))
	((symbol? L) (list (list 1 (list L 1))))
	((polynomial L) L)
	(#t (error "Bad label"))))

;---- (change-weight! tree list-of-labels value) changes the weight of a node
(define (change-weight! tree list-of-labels  value)
  (cond
	((not (pair? tree)) #f)
	((and (pair? tree) (= (length tree) 3))
	 (cond
	  ((and (polynomial? (canonical-label list-of-labels)) (equal? (canonical-label list-of-labels) (label tree)))
		(set-weight! tree value)
		#t)
	  ((polynomial? (canonical-label list-of-labels)) (result #f 1))

	  ((and (list? list-of-labels)
			  (polynomial? (canonical-label (car list-of-labels)))
			  (equal? (canonical-label (car list-of-labels)) (label tree))
			  (null? (cdr list-of-labels))
			  )
		(set-weight! tree value)
		#t)
	  ((and (list? list-of-labels)
			  (polynomial? (canonical-label (car list-of-labels)))
			  (equal? (canonical-label (car list-of-labels)) (label tree))
			  )
		(let* ((lbllist (map label (children tree)))
				 (ix (index-in lbllist (canonical-label (cadr list-of-labels)))))
		  (set! piglet lbllist)
		  (set! pooh (canonical-label (car list-of-labels)))
		  (if ix
				(change-weight (list-ref (children tree) ix) (cdr list-of-labels) value)
				(result #f 2))))
	  (#t (result #f 3))))
	((and (pair? tree) (= (length tree) 2))
	 (error "Not yet implemented for two element trees"))
	(#t (error "Bad mojo in change-weight"))))

;---- (change-label! tree list-of-labels value) changes the label of a node
(define (change-label! tree list-of-labels  value)
  (cond
	((not (pair? tree)) #f)
	((and (pair? tree) (= (length tree) 3))
	 (cond
	  ((and (polynomial? (canonical-label list-of-labels)) (equal? (canonical-label list-of-labels) (label tree)))
		(set-label! tree value)
		#t)
	  ((polynomial? (canonical-label list-of-labels)) (result #f 1))

	  ((and (list? list-of-labels)
			  (polynomial? (canonical-label (car list-of-labels)))
			  (equal? (canonical-label (car list-of-labels)) (label tree))
			  (null? (cdr list-of-labels))
			  )
		(set-label! tree value)
		#t)
	  ((and (list? list-of-labels)
			  (polynomial? (canonical-label (car list-of-labels)))
			  (equal? (canonical-label (car list-of-labels)) (label tree))
			  )
		(let* ((lbllist (map label (children tree)))
				 (ix (index-in lbllist (canonical-label (cadr list-of-labels)))))
		  (set! piglet lbllist)
		  (set! pooh (canonical-label (car list-of-labels)))
		  (if ix
				(change-label (list-ref (children tree) ix) (cdr list-of-labels) value)
				(result #f 2))))
	  (#t (result #f 3))))
	((and (pair? tree) (= (length tree) 2))
	 (error "Not yet implemented for two element trees"))
	(#t (error "Bad mojo in change-label"))))

;---- (change-children! tree list-of-childrens value) changes the children of a node
(define (change-children! tree list-of-labels value)
  (cond
	((not (pair? tree)) #f)
	((and (pair? tree) (= (length tree) 3))
	 (cond
	  ((and (polynomial? (canonical-children list-of-labels)) (equal? (canonical-label list-of-labels) (label tree)))
		(set-children! tree value)
		#t)
	  ((polynomial? (canonical-children list-of-labels)) (result #f 1))

	  ((and (list? list-of-labels)
			  (polynomial? (canonical-children (car list-of-labels)))
			  (equal? (canonical-label (car list-of-labels)) (label tree))
			  (null? (cdr list-of-labels))
			  )
		(set-children! tree value)
		#t)
	  ((and (list? list-of-labels)
			  (polynomial? (canonical-children (car list-of-labels)))
			  (equal? (canonical-label (car list-of-labels)) (label tree))
			  )
		(let* ((lbllist (map children (children tree)))
				 (ix (index-in lbllist (canonical-children (cadr list-of-labels)))))
		  (set! piglet lbllist)
		  (set! pooh (canonical-children (car list-of-labels)))
		  (if ix
				(change-children (list-ref (children tree) ix) (cdr list-of-labels) value)
				(result #f 2))))
	  (#t (result #f 3))))
	((and (pair? tree) (= (length tree) 2))
	 (error "Not yet implemented for two element trees"))
	(#t (error "Bad mojo in change-children"))))

;---- (replace-child! tree list-of-labels  new-child) replaces a child node with a substitute.
(define (replace-child! tree list-of-labels  new-child) ;; the new child need not have the same label as the old one
  (cond
	((not (pair? tree)) #f)
	((and (pair? tree) (= (length tree) 3))
	 (cond
	  ((and (polynomial? (canonical-label list-of-labels)) (equal? (canonical-label list-of-labels) (label tree)))

		#t)
	  ((polynomial? (canonical-label list-of-labels)) #f)
	  ((and (list? list-of-labels)
			  (polynomial? (canonical-label (car list-of-labels)))
			  (equal? (canonical-label (car list-of-labels)) (label tree))
			  (null? (cdr list-of-labels))
			  )
		(set-label! tree (weight new-child))
		(set-weight! tree (label new-child))
		(set-children! tree (children new-child))
		#t)
	  ((and (list? list-of-labels)
			  (polynomial? (canonical-label (car list-of-labels)))
			  (equal? (canonical-label (car list-of-labels)) (label tree))
			  )
		(let* ((lbllist (map label (children tree)))
				 (ix (index-in lbllist (canonical-label (cadr list-of-labels)))))
		  (set! piglet lbllist)
		  (set! pooh (canonical-label (car list-of-labels)))
		  (if ix
				(replace-child! (list-ref (children tree) ix) (cdr list-of-labels) new-child)
				#f)))
	  (#t #f)))
	((and (pair? tree) (= (length tree) 2))
	 (error "Not yet implemented for two element trees"))
	(#t (error "Bad mojo in replace-child!"))))





;--- string->tree routines

;----  (reverse-string s) 
(define (reverse-string s)
  (list->string (reverse (string->list s))))


;---- (filter-formatting s)
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

;----  (strcar s) 
(define (strcar s) ;; expects a char, returns a char
  (if (string=? s "")
		#f
		(string-ref s 0)))

;----  (strcdr s) 
(define (strcdr s) ;; expects a string, returns a string
  (if (string=? s "")
		#f
		(let ((n (string-length s)))
		  (substring s 1 n))))

;----  (strindex S ch) 
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
	
	
;----  (!strindex S ch) 
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
	
		
;----  (strsub string old new) 
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

;----  (strsub* string old new) 
(define (strsub* string old new)
  (let strsub*-loop ((os string)
				 (s (strsub string old new))
				 )
	 (if (string=? os s)
		  s
		  (strsub*-loop s (strsub s old new)))))

;----  (tidy-string-ends s) 
(define (tidy-string-ends s)
  
  (let* ((s (strsub* s "  " " "))
			(z (substring s (!strindex s " ") (string-length s)))
			(d (reverse-string z))
			(b (substring d (!strindex d " ") (string-length d)))
			)
	 (reverse-string b)))

;----  (rectify-tree-string S) 
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
  
;----  (string->children C) 
(define (string->children C)
  (map string->node (map write-string (read-string C))))

;----  (string->node S)  ;; tree2: (ply (children...)), tree3: (wt: ply (children...))
(define (string->node S)
  (if (strindex S ":")
		(string->node3 S)
		(string->node2 S)))

;----  (string->node2 S) 
(define (string->node2 S)
  (let ((s (rectify-tree-string S)))
	 (if (not (char=? (strcar s) #\())
		  'bad-node-string:lparen
		  (let* ((rs (strcdr s))
					(i (strindex rs "("))
					(poly (string->polynomial (substring rs 0 i)))
					(kinder (substring rs i (- (string-length rs) 1))))
			 (list poly (string->children kinder))
		))))
	 

;----  (string->node3 S) 
(define (string->node3 S)
  (let ((s (rectify-tree-string S)))
	 (if (not (char=? (strcar s) #\())
		  'bad-node-string:lparen
		  (let* ((rs (strcdr s))
					(j (strindex rs ":"))
					(i (strindex rs "("))
					(wt (string->number (substring rs 0 j )))
					(poly (string->polynomial (substring rs (+ j 1) i)))
					(kinder (substring rs i (- (string-length rs) 1))))
			 ;;; (dnl* 'rs rs) ;;;
			 ;;; (dnl* 'j j (substring rs 0 j )) ;;;
			 ;;; (dnl* 'wt wt (substring rs 0 j)) ;;;
			 ;;; (dnl* 'i i (substring rs i (string-length rs))) ;;;
			 ;;; (dnl* 'poly poly (substring rs (+ j 1) i)) ;;;
			 ;;; (dnl* 'kinder kinder) ;;;
			 ;;; (dnl* 'result (list wt poly (string->children kinder))) ;;;
			 (list wt poly (string->children kinder))
		))))
	 
(define (string->node3-take1 S)
;;  (dnl "Node3")
  (let ((s (rectify-tree-string S)))
	 (if (not (char=? (strcar s) #\())
		  'bad-node-string:lparen
		  (let* ((rs (strcdr s))
					(j (strindex rs ":"))
					(i (strindex rs "("))
					(wt (string->number (substring rs 0 j )))
					(poly (string->polynomial (substring rs j i)))
					(kinder (substring rs i (- (string-length rs) 1))))
			 ;;; (dnl* 'rs rs) ;;;
			 ;;; (dnl* 'j j) ;;;
			 ;;; (dnl* 'i i) ;;;
			 ;;; (dnl* 'wt wt (substring rs 0 j)) ;;;
			 ;;; (dnl* 'poly poly) ;;;
			 ;;; (dnl* 'result (list wt poly (string->children kinder))) ;;;
			 (list wt poly (string->children kinder))
		))))
	 

;----  string->tree 
(define string->tree string->node)

;---- (L u) returns the set of labels associated with the elements of the set u [Definitions 7 & 8]
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

;---- (L* tree) construct a recursive (label)
(define (L* t)
  (cond
	((null? t) '())
	((node? t) (cons (polynomial->string (label t)) (unique (map L* (children t) ))))
	(#t #f)))

;---- (U_v u v) the set of elements in the extension set of u which have a label in (L v) [Definitions 7 * 8]
;;               (the labels in the extn set of v)
;; (define  (U_v u v)
;;   (U_v_op filter u v))

;---- (U_!v u v) the set of elements in the extension set of u which do not have a label in (L v) [Definitions 7 & 8]
;; (define  (U_!v u v)
;;   (U_v_op !filter u v))

;---- (UandV u v) the set of elements in the extension sets which have labels in both sets 

;; (define (UandV u v)
;;   (let ((UnV (list-intersection (map label (children u)) (map label (children v)))))
;; 	 (map (lambda (x)
;; 			  (let ((ux (child u x))
;; 					  (vx (child v x)))

;; 				 (filter (lambda (x y) (pair? x) (pair? y)) ux vx)))
;; 			UnV)))

;-- Mathematical operators on trees from Definitions 6,9
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

;--- (normalise-children S) returns an extension set which has all nodes with common labels added together
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

;--- (T+ u v) add two trees together [Definition 9] 
;; 
;; We might consider treating two trees as weakly compatible when one or both polynomial labels 
;; are zero
;;

;----  (factor-list p) 
(define (factor-list p)
  (if (string? p)
		(factor-list (string->polynomial p))
		;;(map (lambda (x) (!filter number? x)) (!filter number? p)))
		(!filter number? p))
		
  )

;----  (related? p1 p2) 
(define (related? p1 p2)
  (let ((r (equal? (factor-list p1) (factor-list p2))))
;;	 (dnl p1 " R " p2 " --> " (factor-list p1) " R " (factor-list p2) " = " r)
	 r))

;----  (related-children? c1 c2) 
(define (related-children? c1 c2)
  (related? (label c1) (label c2)))

;----  (p-related-s r p set) 
(define (p-related-s r p set) ;; r is the relation, p is the thing that we are looking for relatives of
  (apply orf (map (lambda (x) (r p x)) set)))

;----  (partition-sets r u v . verbose) 
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

;----  (partition-set* r u) 
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

			 (cons rin (partition-set* r rout))))))
  )

;----  (normalise-tree t) 
(define (normalise-tree t)
  (cond
	((= 2 (length t))	(normalise-tree2 t))
	((= 3 (length t))	(normalise-tree3 t))
	(#t (another-epic-failure))))

;----  (normalise-tree2 t) 
(define (normalise-tree2 t)
  (if (not (tree? t))
		(begin
		  (set! booboo t)
		  (dnl "Bad tree\n" t))
		(let ((T (cons (normalise-polynomial (label t))
							(cons (map normalise-tree2 (sort (children t) node<?)) '())))
				)
		  (if (null? (car T))
				(set-car! T '(0)))
		  
		  (if #f
				(if (zero? (content T))
					 zerotree
					 T)
				T)
		  )))

;----  (normalise-tree3 t) 
(define (normalise-tree3 t)
  (let ((w (weight t))
		  (l (label t))
		  (c (filter (lambda (x) (not (and
												 (zero? (weight x))
												 (poly-zero? (label x))
												 (null? (children x))))
									)
						  (map normalise-tree3 (children t)))
						 )
		  )
	 (list w l c)))


(define (normalise-tree3--ok-as-far-as-it-goes t)
  (if (not (tree? t))
		(begin
		  (set! booboo t)
		  (dnl "Bad tree\n" t))
		(let ((T (cons (weight t) (cons (normalise-polynomial (label t))
												  (cons (!filter (lambda (x)
																		 (zero? (content3 x)))
																	  (map normalise-tree3
																			 (sort (children t) node<?))) '())))
				))
		  (if (not (number? (weight T)))
				(set-car! T 0))

		  (if (null? (label T))
				(set-cadr! T '(0)))
		  
		  (if #f
				(if (zero? (content T))
					 zerotree
					 T)
				T)
		  )))

;----  (operator-curry op lst) 
(define (operator-curry op lst)
  (let ((n (length lst)))
	 (cond
	  ((null? lst) (op zerotree zerotree))
	  ((= n 1) (op (car lst) zerotree))
	  ((= n 2) (apply op lst))
	  (#t (op (car lst) (operator-curry op (cdr lst)))))))

;----  (x-tree+ x y) 
(define (x-tree+ x y) 
  (normalise-label
	(cond
	 ((or (and (null? x) (null? y))
			(and (zerotree? x) (zerotree? y))) zerotree)
	 ((or (null? x) (zerotree? x)) y)
	 ((or (null? y) (zerotree? y)) x)
	 (#t
	  ;;(dnl "SOP, (" (polynomial->string (label x)) ")(" (polynomial->string (label y)) ")")
	  (list (p+ (label x) (label y))
			  (let ((U (n-Lambda (append (children x) (children y)))))
				 (map (lambda (eqclass)
						  (apply-chain tree+ eqclass))
						U))))))
  )

;---- (make-pairings c1 c2) this maps a pair of lists into a list of pairs which have members with the same label
(define (make-pairings c1 c2) ;; It expects the inputs to be compatible 
  (let* ((klst (map label c2))
			(R (map cons
					  c1
					  (map (lambda (x) (list-ref c2 (index-in klst (label x)))) c1))))
	 R))

;----  (boxplus X Y) the arguements are sets of children

(define (boxplus X Y)
  (let*((X+y (U-restricted-to-V X Y))
		  (X-y (U-restricted-to-NOT-V X Y)) ;; X+y \cupdot X-y is clearly X
		  (Y+x (U-restricted-to-V Y X))
		  (Y-x (U-restricted-to-NOT-V Y X))
		  )
	 (if (not (= (length X+y) (length Y+x)))
		  (error "Inconsistent intersection in boxplus" X+y Y+x)
		  (append X-y Y-x  (filter (lambda (x) (not (equal? x '(0 0 '()))))
											(map (lambda (x) (apply tree+ x) )
												  (filter (lambda (x) (equal? (label (car x)) ;; this bit excludes incompatible
																						(label (cadr x))));; nodes
															 (cross** X+y Y+x))))))
	 ))

(define (less-zerotree A)
  (!filter
	(lambda (x)
	  (and (zero? (weight x))
			 (or (poly-zero? (label x))
				  (equal? '(0) (label x)))
			 (equal '() (children x))))
	A))

(define (boxcross p C)
	(less-zerotree (map (lambda (c) (list (weight c) (p* p (label c)) (children c))) C))
  )

;----  (tree3+ x y) 
(define (tree3+ x y)
  (let* ((E (equal? (label x) (label y)))
			(T (normalise-tree3 (if E
											(begin
											  (list (+ (weight x) (weight y))
													  (label x)
													  (boxplus x y))
											  )
											(or (list 0 0 '())
												 (error "Attempt to add nodes which are not compatible\n  " x 'AND y)))
									  ))
			)
	 T))
			

;----  (tree2+ x y) 
(define (tree2+ x y)
  (list (p+ (label x) (label y))
		  (sort (n-Lambda^sigma (union+ (children x) (children y))) node<?)))

;---- (tree+ . args)
;; Because the addition of trees incorporates the addition of children, adding a tree and its negative
;; yields a tree with an absolute magnitude of zero.

(define (tree+ . args)
	 (case (length args)
		((0) zerotree)
		((1) (car args))
		((2)
		 (cond ((node2? (car args)) (apply tree2+ args))
				 ((node3? (car args)) (apply tree3+ args))
				 (A very bad thing)))
		(else
		 (cond ((node2? (car args)) (tree2+ (car args) (apply tree2+ (cdr args))))
				 ((node3? (car args)) (tree3+ (car args) (apply tree3+ (cdr args))))
				 (A very bad thing)))
		))

;----  (tree- . args) 
(define (tree- . args)
	 (case (length args)
		((0) zerotree)
		((1) (tree* -1 (car args)))
		((2) 
		 (tree+ (car args) (tree* -1 (cadr args)))
		 )
		 (else 
		  (tree+ (car args) (tree -1 (apply tree+ (cdr args))))
		  )
		;;	 (else (apply tree+ (cons (car args) (map (lambda (x) (tree* -1 x)) (cdr args)))))
		))
			
;----  (T/Z t)  
(define (T/Z t)
  (let ((childs (sort (!filter (lambda (x)
											(or (null? x)
												 (zero? (absolute-value x)))
											)
										 (map T/Z (children t))) node<?)))
	 (if (zero? (apply + (map absolute-value childs)))
		  (list (weight t) (label t) emptyset)
		  (list (weight t) (label t) (!filter null? childs))
		  )))

(define (norm t)
  (+ (sqr (weight t))
	  (if (pair? (children t))
			(apply + (map norm (children t)))
			0)))

(define (distance s t)
  (if (equal? (Type? s) (Type? t))
		(case (Type? s)
		  ((number) (abs (- s t)))
		  ((Vector) (sqrt (apply + (map sqr (map - s t)))))
		  ((tree3) (sqrt (norm (tree- s t))))
		  (else #f))
		#f))

;--- (absolute-value t)  returns the absolute magnitude of a tree [Definition 11]
(define (absolute-value t)
  (if (null? t)
		0
		(let ((w (weight t))
				(c (children t)))
		  (cond
			((null? t) 0)
			((zerotree? t) 0)
			((null? c) (abs (weight t)))
			(#t (apply + (cons (abs (weight t)) (map absolute-value (children t)))))))))

;----  (type-tests x) 
(define (type-tests x)
  (list 'number?  (number? x)
		  'polynomial? (polynomial? x)
		  'node?  (node?  x)
		  'children? (children? x)))

;----  type-of-multiplication-for-children  (tree2 only)
(define type-of-multiplication-for-children
  ;;  '*simple-children*
  ;;  'first-variant
    'experimental
  )

;--- (* . args)
(define (tree* . args)
  (let ((t3 (apply orf (map node3? args)))
		  (t2 (apply orf (map node2? args)))
		  )
	 (if (and t2 t3)
		(error "Cannot mix tree types" args))
	 (cond
	  (t3 (apply tree3* args))
	  (t2 (apply tree2* args))
	  (#t (apply p* args)))))

;--- (* . args) two argument multiplication of trees
(define (tree2* . args)
  ;; (dnl "B " (type-tests B))
  ;; (dnl "C " (type-tests C))
  (case (length args)
	 ((0) '())
	 ((1) (car args))
	 ((2) (let ((B (car args)) (C (cadr args)))
			  (cond
				((and (null? B)(null? C)) '())
				((null? C) B)
				((null? B) C)
				((and (number? B) (number? C))
				 ;; (dnl "numbers")
				 (* B C))
				
				((or (and (polynomial? B) (number? C))
					  (and (polynomial? B) (polynomial? C)))
				 ;; (dnl "polynomial and number, polynomial and polynomial")
				 (p* B C))
				
				((and (number? B) (node? C))
				 ;; (dnl "number and node")
				 (list
				  (p* B (label C))
				  (map (lambda (x) (tree* B x)) (children C))))
				
				((and (polynomial? B) (node? C))
				 ;; (dnl "polynomial and node")
				 (list
				  (p* B (label C))
				  (tree2* B (children C))))
				;;(map (lambda (x) (tree2* B x)) (children C))))

				((and (polynomial? B) (children? C))
				 ;; (dnl "polynomial and children")
				 (sort (map (lambda (x) (tree2* B x)) C) node<? ))

				((and (number? B) (children? C))
				 ;; (dnl "polynomial and children")
				 (sort (map (lambda (x) (tree2* (list B) x)) C) node<?)) ;; because we use multiplication by -1 to do subtraction

				((and (tree2? B) (tree2? C))
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
															 ((intersection)
															  (intersection%* sim? label CB CC))
															 ((pairwise-multiplication)
															  (map* tree2* CB CC)
															  )
															 ((pairwise-multiplication-with-consolidation)
															  (n-Lambda^sigma (map* tree2* CB CC))
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
;;				 (dnl "swapping B and C")
				 (tree2* C B))

				(#f (error "bad argument to tree2*"))
				)))
	  (else (error "too many arguments to tree2*" args))
	  )
	 )

(define (tree3* . args)
  (let* ((tP (filter polynomial? args))
			(P (let ((Q (apply p* tP)))
				  (cond
					((null? Q) 1)
					((number? Q) Q)
					((and (list? Q) (= 1 (length Q)) (number? (car Q)))	(car Q))
					(#t Q))))
			(T (filter node? (!filter polynomial? args)))
			(result #f)
			)
	 (let ((R
			  (cond
				((null? T) zerotree)
				((null? (cdr T))
				 (car T))
				((= (length T) 2)
				 (let ((A (car T))(B (cadr T)))
					(list (* (weight A) (weight B))
							(p* (label A) (label B))
							(boxplus (boxcross (label B) (children A))
										(boxcross (label A) (children B))))))
				(#t (let ((B (apply tree3* (cdr T))))
						(tree3* (car T) B)))))
			 )
		(if (and (polynomial? P) (not (number? P)))
			 (error "Multiplication by non-scalar polynomials is not supported" P args))
		(list (* P (weight R)) (label R) (map (lambda (x) (tree3* P x)) (children R))))))

;- Routines for use by models

;----  (tree-distance- p q) 
(define (tree-distance- p q)
  (let ((tree (tree+ (tree* -1 p) q)))
	 (/ (absolute-value tree)
		 (card tree))))

;----  (tree-distance p q) 
(define (tree-distance p q)
  (distance p q)
  )

;-- (tree->wordies t)
(define (tree->wordies t)
  (cons (polynomial->string (label t))
		  (list (map tree->wordies (children t)))))

;-- (tree->string t . indent)
(define (tree->string t . indent)
			(string-append
			 "("
			 (if (node3? t)
				  (string-append (number->string (weight t)) ": ")
				  "")
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

;-- (children->string es . indent) maps an extension set to a string
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

;-- (tree->display-string t . args) -- with formatting (but not latex)
(define (tree->display-string t . indent)
  (set! indent (if (null? indent) 0 (car indent)))
  (string-append
	(tree->display-string% t indent)
	"\n"))

;--- (make-space n) makes a string n spaces long for indents
(define (make-space n)
  (make-string n #\space))

;--- (tree->display-string% t indent) This does the heavy lifting
(define (tree->display-string% t indent)
  (let ((emit-space #t))
	 (string-append
	  (if (null? t)
			""
			(string-append
			 "(" 
			 (if (= (length t) 3)
						  (string-append (number->string (weight t)) ": ")
						  "")
			 (if (string? (label t)) (label t) (polynomial->string (label t))) 
			 (if (null? (children t))
				  (begin
					 (set! emit-space #f)
					 " {}")
				  (children->display-string (children t) (+ 2 indent)))
			 (if emit-space (make-space (+ indent 1)) "")
			 ")"
			 )))
	 ))

;--- (tree->display-string%% t indent) This does the heavy lifting
(define (tree->display-string%% t indent)
  (let ((emit-space #t))
	 (string-append
	  (if (null? t)
			""
			(string-append
			 "(" 
			 (if (= (length t) 3)
						  (string-append "\"" (number->string (weight t)) ": \" ")
						  "")
			 "\""
			 (if (string? (label t)) (label t) (polynomial->string (label t))) "\""
			 (if (null? (children t))
				  (begin
					 (set! emit-space #f)
					 " {}")
				  (children->display-string (children t) (+ 2 indent)))
			 (if emit-space (make-space (+ indent 1)) "")
			 ")"
			 )))
	 ))

;-- (children->display-string es . indent) maps an extension set to a string
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

;-- 

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

;----  (random-dice n s) 
(define (random-dice n s)
  (if (or (zero? n) (< s 2))
		0
		(ceiling (/ (apply + (map (lambda (x) (random-integer s)) (seq n))) n))))

;----  (dnlt t) 
(define (dnlt t)
  (display (tree->display-string t)))

;----  (dnlc t) 
(define (dnlc t)
  (display (children->string t)))

;----  (tdnl . args) 
(define (tdnl . args)
  (apply dnl 
			(map (lambda (x)
					 (cond
					  ((children? x) (children->string x))
					  ((node? x) (tree->display-string x))
					  ((polynomial? x) (polynomial->string x))
					  (#t x)))
				  args)))
			 

;- Appendix: Miscellaneous routines


(define (tree-interpolate t1 t2 pivot)
  (if (and (<= 0 pivot) (<= pivot 1))
		(tree+ (tree* (- 1 pivot) t1) (tree* pivot t2))
		(error "the pivot must be in [0,1]" pivot)))

(define (p-n . arg)
  (if (null? arg)
		(if (even? (random-integer 3)) 1 -1)
		(* (if (even? (random-integer 3)) 1 -1) (car arg))))

;---- (random-polynomial-term L s E)  set-of-labels, scale, max exponent
(define (random-polynomial-term L s E) 
  (cond
	((zero? s) (list 0))
	((zero? E) (list (random-integer s)))
	(#t
	 (let* ((te (* (p-n (random-integer (* 2 (+ 1 E))))))
			  (e (if (zero? te) E te))
			  (coef  (* (p-n) (random-integer s)))
			  (facts (map (lambda (x)
								 (list (list-ref L (random-integer (length L)))
										 (* (p-n) (random-integer  e)))
								 ) (seq E)))
			  )
	
		(cons (if (zero? coef) 1 coef) ;; we don't want zero coefficients associated with a factor!
				facts
				))
	 ))
	 )
	

;---- (random-polynomial L S k E)  set-of-labels, Scalar, k terms,  max Exponent
(define (random-polynomial L S k E)
  (let* ((n-terms (random-integer k))
			;;(s (* 2.0 (- (random-real) 0.5) (random-integer S))) ;; scalar part
			(s (* (p-n) (random-integer S))) ;; scalar part
			(pt (map (lambda (x) (random-polynomial-term L (+ 1 (random-integer S)) E)) (seq (+ 1 (random-integer k)))))
			)
	 (normalise-polynomial (cons s pt))))

(define (rand-poly . args)
  (random-polynomial '(w x y z) 3 6 3))

;;; This is wrong now.... ;;;
;;; (define (random-tree3 depth n-terms nweight symbols maxexp toomanykids) ;;;
;;;   (let ((mknode (lambda (w) ;;;
;;; 						(list (if (number? w) w (* (rndsgn) (random-integer (+ 1 nweight))))) ;;;
;;; 						(rand-poly (+ 1 (random-integer n-terms)) ;;;
;;; 									  nweight ;;;
;;; 									  symbols ;;;
;;; 									  maxexp) ;;;
;;; 								(if (or (not (positive? toomanykids)) (<= 1 depth)) ;;;
;;; 									 '() ;;;
;;; 									 (map (lambda (x) ;;;
;;; 											  (random-tree3 (- depth 1) n-terms nweight symbols maxexp (- toomanykids 1))) ;;;
;;; 											(seq (random-integer toomanykids)))))) ;;;
;;; 		  ) ;;;
;;; 	 (mknode 0))) ;;;
					 

(define (rndel lst)
  (let ((n (length lst)))
	 (list-ref lst (random-integer n))))

(define (mkrnd-node3% . kinder)
  (list 
	(rndel '(-3 -2 -1 0 0 0 1 2 3 5 7 11))
	(rand-poly rand-poly 3 12 '(x y z) 3)
	kinder))

(define coefficient-lst '(-4 -3 -1 0 1 3 5 7 11 13))

(define (mkrnd-node3 depth nkids . val)
  (let ((n (if (< depth 2)
					(mkrnd-node3%)
					(let* ((k (seq (+ 1 (random-integer nkids))))
							 (c (map (lambda (x) (mkrnd-node3 (- depth 1) nkids #t)) (seq (rndel k))))
							 )
					  (list (rndel coefficient-lst)
							  (if (null? val) '(0) (rand-poly 3 12 '(x y z) 3))
							  c))))
		  )
	 n))

(define tst1 (string->tree "(0: 0  {(5: c +  b + a {(4: 2 m c + 3 m b - m a {}) 
                                            (1/2:  n c - n b + 2 n a {})
                                            (7/2:  n c - n b + n a {})
                                            (1/3:-1 + p c + 2 p b - 2 p a {})})})"))

(define tst2 (string->tree "(0: 0 {(3: c + b + a {(1/2: 2 m c + 3 m b - m a {}) 
                                            (1/5:  n c - n b + 2 n a {})
                                            (1/7:-1 + p c + 2 p b + 3 p a {})})})"))

						  
(define tst3 (string->tree "(0: 0 {(3: c + b + a {(1/2: 2 m c + 3 m b - m a {}) 
                                            (1/5:  n c - n b + 2 n a {})
                                            (6: n + 3 m {})		
                                            (1/7:-1 + p c + 2 p b + 3 p a {})})})"))
;; model status format

;;    model --> domain* --> niche* --> agent* --> sub-agents
;;                                            --> maintained alternatives
;;
;; In principle, the agent could be treated and structured entirely like a model.
;; In \it{practice}, this might just make things a bit more awkward \it{at this stage\}.
;;

;; Recall that ALL valid trees have a root node with a weight of zero.
;; In the schema below, "gn" denotes the \it{goodness} of a niche's current
;; aggregate representation, "" is a number of 

(define *model (string->tree "(0: model {
  (0: A {(0: plant {(0: tree {(1: individuals {})} )} )
         (0: aherbivore {(0: Hanimal {(1: individuals {})} )} )
         (0: jherbivore {(0: jHanimal {(1: individuals {})} )} )
         (0: acarnivore {(0: Canimal {(1: individuals {})} )} )
         (0: jcarnivore {(0: jCanimal {(1: individuals {})} )} )
   } )
  (0: B {(0: plant {(0: eqntree {(1: biomass {})} )} )
         (0: aherbivore {(0: eqnaHerb {(1: biomass {})} )} )
         (0: jherbivore {(0: eqnjHerb {(1: biomass {})} )} )
         (0: acarnivore {(0: eqnCarn {(1: biomass {})} )} )
         (0: jcarnivore {(0: eqnjCarn {(1: biomass {})} )} ) })
  (0: C {})
  (0: D {})
  (0: E {})
  (0: F {})
  (0: G {})
  (0: H {})
  (0: I {})})"))

;;; (0: model-lbl {set-of-domains}) ;;;
;;; (nD: domain-lbl {set-of-niches}) ;;;
;;; (nN: niche-lbl {set-of-representations-for-niche}) ;;;
;;; (nR: rep-lbl {set-of-agents}) ;;;
;;; (nA: agent-lbl {?}) ;;;

;;; The weights are probably "fitness" indicators. ;;;

;;; This sort of tree can be compared with the known-good/known-bad ;;;
;;; templates, and can be masked with an (overlay ...)  ;;;


;----  (tstdl rcg) 
(define (tstdl rcg)
  (if (eq? (car rcg) 'ok)
		20160707 ;; success
		#f))

;-  The End 

;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
