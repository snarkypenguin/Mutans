;;(include "/etc/scheme.d/implementation-macros.scm")

(define (dnl . args) (if (null? args) (display "") (let () (map display args) (newline))))
(define (DNL . args) (if debugging (apply dnl args)))


(define (string-contains? str . targets) ;; (string-contains? "The quick brown fox" "ox" "hen") ==> #t (string-contains? "The quick brown fox" "oxo" "hen") ==> #f
  (if (= (length targets) 1)
      (let* ((st (car targets))
	     (n (string-length st))
	     )
	(cond
	 ((< (string-length str) n) #f)
	 ((string=? (substring str 0 n) (substring st 0 n)) #t)
	 (#t (string-contains (substring 1 (- (string-length str) 1)) st))))
      (let loop ((st targets))
	(cond
	 ((null? st) #f)
	 ((string-contains? str (car st)) #t)
	 (#t (loop (cdr st)))))))

(define (locate-substring str substr)
  (let ((n (string-length substr))
		  (N (string-length str))
		  )
	 (let loop ((k 0))
		(cond
		 ((< N (+ k n) ) #f)
		 ((string=? (substring str k (+ k n)) substr) k)
		 (#t (loop (+ k 1)))))))
		  			
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
                   (if (eq? (string-ref s 0)
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
                   (if (eq? (string-ref s 0)
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


;; Like strtok, but it separates with a string rather than a set of characters
;; defaults to ","
(define (simple-split-string-at-separator str . separatorstring)
  (let ((sep (if (null? separatorstring)
					  ","	(car separatorstring))))
	 (let ((ns (string-length sep)))
		(let loop ((s str)
					  (r '()))
		  (let ((n (locate-substring s sep)))
			 (if n
				  (loop
				 	(substring s (+ n ns) (string-length s))
					(cons (substring s 0 n) r))
				  (reverse (cons s r))))))))

;; Like strtok, but it separates with a strings rather than a set of characters
;; defaults to ","
(define (simple-split-string-at-separator+ str . separatorstring)
  (let ((sep (if (null? separatorstring)
					  ","	(car separatorstring))))
	 (let ((ns (string-length sep)))
		(let loop ((s str)
					  (r '()))
		  (let ((n (locate-substring s sep)))
			 (if n
				  (loop
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
  (let loop ((s str)
				 (r '()))
	 (let* ((n-s (next-split s separatorstrings))
			  (n (if n-s (car n-s) #f))
			  (sep (if n-s (cdr n-s) #f))
			  (ns (if n-s (string-length sep) #f)))
		(if n
			 (loop
			  (substring s (+ n ns) (string-length s))
			  (cons (substring s 0 n) r))
			 (reverse (cons s r))))))

;; Like strtok, but it separates with a set of strings rather than a set of characters
(define (split-string-at-separators+ str separatorstrings)
  (let loop ((s str)
				 (r '()))
	 (let* ((n-s (next-split s separatorstrings))
			  (n (if n-s (car n-s) #f))
			  (sep (if n-s (cdr n-s) #f))
			  (ns (if n-s (string-length sep) #f)))
		(if n
			 (loop
			  (substring s (+ n ns) (string-length s))
			  (cons sep (cons (substring s 0 n) r)))
			 (reverse (cons s r))))))



