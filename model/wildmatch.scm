;-  Identification and Changes

;--
;	wildmatch.scm -- Written by Randall Gray 

;


;-  Discussion 

;; * matches any string  -- this will attempt to match as much of the string as possible
;; ? matches any single character
;; 
;; @ matches anhy alphabetic (^ is specifically an uppercase and   is a lowercase)
;; # matches any decimal digit
;; ~ matches any space (space tab  newline cr)
;; \ escapes the next symbol
;; ! is a "not" which applies to the rest of the pattern  -- "!*" is guaranteed to return false ;-)

;; if any of "@", "#" or "~" are followed by a "+" it matches [1...,∞) things
;; if they are followed by a "*" then it matches [0...,∞) things



;-  Configuration stuff 

;;; (define wildesc #\\) ***
;;; (define wildnot #\!) ***

;;; (define wildstar #\*) ***
;;; (define wildplus #\+) ***

;;; (define wildspace #\_) ***
;;; (define wildchar #\%) ***
;;; (define wildabc #\?) ***
;;; (define wilddig #\#) ***


;;; (define wildlist (list wildstar wildplus wildspace wildchar wildabc wildesc wilddig)) ***

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

;;; (define (dnl . lst)  ***
;;;   (if (pair? lst) ***
;;; 		(begin ***
;;; 		  (display (car lst)) ***
;;; 		  (if (pair? (cdr lst))  ***
;;; 				(for-each display (cdr lst))))) ***
;;;   (newline)) ***

;;; (define (dnl* . lst)  ***
;;;   (if (pair? lst) ***
;;; 		(begin ***
;;; 		  (display (car lst)) ***
;;; 		  (if (pair? (cdr lst))  ***
;;; 				(display (string-append (map (lambda (x) (string-append " " (object->string x))) (cdr lst))))))) ***
;;;   (newline)) ***

(define (tail l n)
  (list-tail l (- (length l) n)))


(define wildcard-digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\7 #\8 #\9))
(define wildcard-lowercase '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
											#\k #\l #\m #\n #\o #\p #\q #\r #\s 
											#\t #\u #\v #\w #\x #\y #\z))

(define wildcard-uppercase '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
											#\K #\L #\M #\N #\O #\P #\Q #\R #\S 
											#\T #\U #\V #\W #\X #\Y #\Z))
(define wildcard-alphabetics (append wildcard-lowercase wildcard-uppercase))

(define wildcard-spaces '(#\space #\tab #\newline #\return))
(define wildcard-punctuation '(#\, #\. #\; #\: #\! #\? #\&))

(define (letter? z)
  (member z wildcard-alphabetics))

(define (lowercase? z)
  (member z wildcard-lowercase))

(define (uppercase? z)
  (member z wildcard-uppercase))

(define (digit? z)
  (member z wildcard-digits))

(define (space? z)
  (member z wildcard-spaces))


(define (punctuation? z)
  (member z wildcard-punctuation))



;;; NOTE WELL: passing char-ci=? for char= will make the "constant" parts  case insensitive, but
;;; not the parts being matched by wildcards: if you use a ^ or a   they will *still* only match
;;; members of their set!


;;; Currently:

;;; The 1-∞ operator doesn't work, expects * and ? for strings and characters.



(define (make-string-wildcard-match . char=)
  (if (null?  char=)
		(set! char= char=?)
		(set! char= (car char=)))

  (let* ((wildcard-sets (list  (list 'chars #\? char?)
										 (list 'letters #\: letter?)
										 (list 'lowercase #\% lowercase?)
										 (list 'uppercase #\$ uppercase?)
										 (list 'digits #\$ digit?)
										 (list 'spaces #\_ space?)
										 ))
			(wildesc #\\)
			(wildstar #\*) ;; 0-∞ operator
			(wildplus #\+) ;; 1-∞ operator
			(rslt '())										 
			(collecting '())
			(simple-charmatch (lambda (pch sch)
									  (let* ((testalist (map cdr wildcard-sets))
												(test (assq pch testalist))
												)
										 (and test ((cadr test) sch)))))
			
			) ;; end of let* variables

	 (letrec ((simple-wildmatch (lambda (pattern string)
											(if (string? pattern) (set! pattern (string->list pattern)))
											(if (string? string) (set! string (string->list string)))

											(if (and (null? pattern) (null? string))
												 #t
												 (let ((pn (length pattern)) (sn (length string)))
													(cond 
													 ;;if ((pattern && !string) || (!pattern && string)) return 6;
													 ((and (zero? pn) (not (zero? sn)))
													  #f)

													 ;; consume superfluous wildcard -- we do this before the next one so that "**" matches an empty string....
													 ((and (>= (length pattern) 2) (equal? (car pattern) wildstar) (equal? (cadr pattern) wildstar))
													  (simple-wildmatch (cdr pattern) string))

													 ;; An empty string matches any of the "star" type wildcards
													 ((and (null? string) (pair? pattern) (or (and (null? (cdr pattern)) (equal? (car pattern) wildstar))
																					  (and (= 2 (length pattern)) (equal? (cadr pattern) wildstar) (member (car pattern) (map cadr wildcard-sets))
																							 )))
													  #t)

													 ;; terminal wildcard
													 ((and (pair? pattern) (equal? (car pattern) wildstar) (null? (cdr pattern)))
													  #t) 

													 ;; handle escaped wildcard characters and escape things (well, escaped *anything* really)
													 ((and (pair? pattern) (equal? (car pattern) wildesc))
													  (if (not (char= (cadr pattern) (car string)))
															#f
															(simple-wildmatch (cddr pattern) (cdr string))))
													 
													 
													 ;; now we handle single instances of things like @, #, and ~
													 ((member (car pattern) (map cadr wildcard-sets))
													  ;; if it is bare (pn < 2) we just handle it
													  (cond
														((or (< pn 2) (not (member (cadr pattern) (list wildplus wildstar))))
														 (if (pair? string)
															  (and (simple-charmatch (car pattern) (car string))
																	 (simple-wildmatch (cdr pattern) (cdr string)))
															  #f)
														 )
														;; This is the 1-∞ operator
														((and (> (length pattern) 1) (equal? (cadr pattern) wildplus))
														 (if (pair? string)
															  (and (simple-charmatch (car pattern) (car string))
																	 (simple-wildmatch (append (list (car pattern) wildstar) (cddr pattern)) (cdr string)))
															  #f)
														 )

														;; This is the 0-∞ operator
														((and (> (length pattern) 1) (equal? (cadr pattern) wildstar) )
														 ;; This really ought to consume as much as possible temporarily and 
														 ;; use this bond as a to find a less greedy consumption if possible
														 (if (pair? string)
															  (or
																(simple-wildmatch (cddr pattern) string)
																(and (simple-charmatch (car pattern) (car string))
                                                     (simple-wildmatch pattern (cdr string)))
																)
															  (and (= 2 (length pattern)) (simple-charmatch (car pattern) (car string))))
														 )
														(else #f)
														)
													  )
													 ((and (pair? pattern) (equal? (car pattern) wildstar))
													  (let *-loop ((t (- (length string) 1)))
														 (if (< t 0 )
															  #f
															  (if (simple-wildmatch (cdr pattern) (list-tail string t))
																	#t
																	(*-loop (- t 1))))))
													 (#t
													  (if (and (pair? string) (pair? pattern) (not (char= (car pattern) (car string))))
															#f
															(if (pair? string)
																 (simple-wildmatch (cdr pattern) (cdr string))
																 (null? (cdr pattern)))
															))
													 
													 ))
												 )
											) ;; end of lambda
										 ) ;; end of definition of simple-wildmatch
				 )  ;; end of letrec() variables

		;; This is the wrapper function which is returned by make-string-wildcard-match
		(lambda  args

		  (if (and (= (length args) 2) (string? (car args)) (string? (cadr args)))	
				(begin
				  (set! collecting '())
				  (set! rslt '())
				  (let ((retn (apply simple-wildmatch args)))
					 (if retn
						  (if (null? rslt)
								#t
								rslt)
						  #f))
				  )
				(cond
				 ((null? args) (map (lambda (x) (list-head x 2)) wildcard-sets)) ;; returns the sets and the characters used to indicate them
				 ;; '0-∞?
				 ;; '1-∞?
				 ;; 'substitute-type-wildcard
				 ;; 'substitute-0-∞
				 ;; 'substitute-1-∞
				 ;; add-set! 
				 ;; remove-set!
				 (else (abort (string-append "unrecognised arguments to string-wildcard=? function: "  (object->string args))))
				 )
				))
		)  ;; end of letrec
	 ) ;; end of let*
  )

(define simple-wildmatch (make-string-wildcard-match))
(define simple-wildmatch-ci (make-string-wildcard-match char-ci=?))

  
;; THIS IS NOT THERE YET!
;; This is like simple-wildmatch, but returns a list of the "wild" parts that were matched, or just true if there were 
;; no wild parts in the pattern.  It returns false if the match does not succeed


(define (wildmatch pattern string)
  (simple-wildmatch pattern string)
  )


(define (wildmatch-ci pattern string)
  (simple-wildmatch-ci pattern string)
  )





;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
