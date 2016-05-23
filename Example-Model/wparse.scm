;;; The copyright to the closures 
;;; 
;;;;   (define (wmatch pattern instance) ...)
;;;;   (define (wnmatch pattern instance) ...)
;;;;   (define (hard-wmatch pattern instance . hmv) ...)
;;;;   (define (hard-wnmatch pattern instance . hmv) ...)
;;;;   (define (wparse pattern instance) ...)
;;;;   (define (wnparse pattern instance) ...)
;;;;   (define (string->symbol-list str) ...)
;;;;
;;;; is held by Randall Gray (C) 1990, and the work may be used under the GPL version 3.
;;;; 
;;---      (wmatch pattern instance) -- pattern matcher, returns #t/#f

;;(require 'string-port)
;; (wparse '((a (b c) ...) ... a) '(((0) (1 2) (3 4)) (5 (6 7)) 8))

;; Basic pattern match on the structure
(define (wmatch pattern instance)
  (cond
   ((or (null? pattern) (null? instance))
    (and (null? pattern) (null? instance)))

   ((symbol? pattern)
    (not (null? instance)) )

   ((and (list? pattern) (equal? (car pattern) 'quote))
    (equal? (cadr pattern) instance))

   ((list? pattern)
    (if (not (list? instance))
		  #f
		  (if (and (> (length pattern) 1) (equal? (cadr pattern) '...))
				(or (and (wmatch (cddr pattern) (cdr instance))
							(wmatch (car pattern) (car instance)))
					 (and (wmatch pattern (cdr instance))
							(wmatch (car pattern) (car instance))))

				(and (wmatch (cdr pattern) (cdr instance))
					  (wmatch (car pattern) (car instance)))
				
				)))))

;; Basic pattern match on the structure, but a null in the instance will match a symbol in the pattern
(define (wnmatch pattern instance)
  (cond
   ((and (null? instance) (symbol? pattern))
    #t)

   ((or (null? pattern) (null? instance))
    (and (null? pattern) (null? instance)))

   ((symbol? pattern)
    (not (null? instance)) )

   ((and (list? pattern) (equal? (car pattern) 'quote))
    (equal? (cadr pattern) instance))

   ((list? pattern)
    (if (not (list? instance))
		  #f
		  (if (and (> (length pattern) 1) (equal? (cadr pattern) '...))
				(or (and (wnmatch (cddr pattern) (cdr instance))
							(wnmatch (car pattern) (car instance)))
					 (and (wnmatch pattern (cdr instance))
							(wnmatch (car pattern) (car instance))))

				(and (wnmatch (cdr pattern) (cdr instance))
					  (wnmatch (car pattern) (car instance)))
				
				)))))

;; Hard-wmatch is like wmatch, but the associated values of the pattern symbols have to match all the way.

(define (hard-wmatch pattern instance . hmv)
  (define (internal-wmatch pattern instance)
	 (cond
	  ((or (null? pattern) (null? instance))
		(and (null? pattern) (null? instance)))
	  
	  ((symbol? pattern)
		(if (not (null? instance)) 
			 (let ((cv (assoc pattern hmv)))
				(if (or (null? cv) (not cv))
					 (begin
						(set! hmv (acons pattern instance hmv))
						#t)
					 (equal? (cdr cv) instance)))
			 #f))
     
	  ((and (list? pattern) (equal? (car pattern) 'quote))
		(equal? (cadr pattern) instance))
	  
	  ((list? pattern)
		(if (not (list? instance))
			 #f
			 (if (and (> (length pattern) 1) (equal? (cadr pattern) '...))
				  (or (and (internal-wmatch (cddr pattern) (cdr instance))
							  (internal-wmatch (car pattern) (car instance)))
						(and (internal-wmatch pattern (cdr instance))
							  (internal-wmatch (car pattern) (car instance))))
				  
				  (and (internal-wmatch (cdr pattern) (cdr instance))
						 (internal-wmatch (car pattern) (car instance)))
				  
				  )))))
  
  (set! hmv '())
  (internal-wmatch pattern instance)
  )


;; like hard-wmatch, but allows symbols to match '()


(define (hard-wnmatch pattern instance . hmv)
  (define (internal-wnmatch pattern instance)
	 (cond
	  ((and (null? instance) (symbol? pattern))
		#t)

	  ((or (null? pattern) (null? instance))
		(and (null? pattern) (null? instance)))
	  
	  ((symbol? pattern)
		(if (not (null? instance)) 
			 (let ((cv (assoc pattern hmv)))
				(if (or (null? cv) (not cv))
					 (begin
						(set! hmv (acons pattern instance hmv))
						#t)
					 (equal? (cdr cv) instance)))
			 #f))
     
	  ((and (list? pattern) (equal? (car pattern) 'quote))
		(equal? (cadr pattern) instance))
	  
	  ((list? pattern)
		(if (not (list? instance))
			 #f
			 (if (and (> (length pattern) 1) (equal? (cadr pattern) '...))
				  (or (and (internal-wnmatch (cddr pattern) (cdr instance))
							  (internal-wnmatch (car pattern) (car instance)))
						(and (internal-wnmatch pattern (cdr instance))
							  (internal-wnmatch (car pattern) (car instance))))
				  
				  (and (internal-wnmatch (cdr pattern) (cdr instance))
						 (internal-wnmatch (car pattern) (car instance)))
				  
				  )))))
  
  (set! hmv '())
  (internal-wnmatch pattern instance)
  )



;;---      (wparse pattern instance) -- pattern parser, returns a-list of values
;; Parse constructs lists of symbols (from the instance) which are associated with the symbol in the pattern

(define (wparse pattern instance)
  (define the-list '())

  (define (add-to-the-list pattern instance)
    (if (<= (length the-list) 0)
		  (set! the-list (cons (cons pattern instance) '()))
		  (set-cdr! (list-tail the-list (1- (length the-list))) (cons (cons pattern instance) '())))
    #t)

  (define (inner-parser inner-pattern inner-instance)
    (cond
     ((or (null? inner-pattern) (null? inner-instance))
      (and (null? inner-pattern) (null? inner-instance)))
     
     ((symbol? inner-pattern)
      (if (not (null? inner-instance))
			 (add-to-the-list inner-pattern inner-instance)
			 #f))
     
     ((and (list? inner-pattern) (equal? (car inner-pattern) 'quote))
      (equal? (cadr inner-pattern) inner-instance))
     
     ((list? inner-pattern)
      (if (not (list? inner-instance))
			 #f
			 (if (and (> (length inner-pattern) 1) (equal? (cadr inner-pattern) '...))
				  (or (and 
						 (wmatch (car inner-pattern) (car inner-instance))
						 (wmatch (cddr inner-pattern) (cdr inner-instance))
						 (inner-parser (car inner-pattern) (car inner-instance))
						 (inner-parser (cddr inner-pattern) (cdr inner-instance)))
						(and 
						 (wmatch (car inner-pattern) (car inner-instance))
						 (wmatch inner-pattern (cdr inner-instance))
						 (inner-parser (car inner-pattern) (car inner-instance))
						 (inner-parser inner-pattern (cdr inner-instance))))
				  
				  (and 
					(wmatch (car inner-pattern) (car inner-instance))
					(wmatch (cdr inner-pattern) (cdr inner-instance))
					(inner-parser (car inner-pattern) (car inner-instance))
					(inner-parser (cdr inner-pattern) (cdr inner-instance))))))))

  ;; begin wparse

  (set! the-list '())

  (let ((result (inner-parser pattern instance)))
    (if result
		  the-list
		  #f)))


;;;;;;;;;;    wnparse is like wparse, but allows symbols to match '()

(define (wnparse pattern instance)
  (define the-list '())

  (define (add-to-the-list pattern instance)
    (if (<= (length the-list) 0)
		  (set! the-list (cons (cons pattern instance) '()))
		  (set-cdr! (list-tail the-list (1- (length the-list))) (cons (cons pattern instance) '())))
    #t)

  (define (inner-parser inner-pattern inner-instance)
    (cond
     ((and (null? inner-instance) (symbol? inner-pattern))
      (add-to-the-list inner-pattern inner-instance))

     ((or (null? inner-pattern) (null? inner-instance))
      (and (null? inner-pattern) (null? inner-instance)))
     
     ((symbol? inner-pattern)
      (if (not (null? inner-instance))
			 (add-to-the-list inner-pattern inner-instance)
			 #f))
     
     ((and (list? inner-pattern) (equal? (car inner-pattern) 'quote))
      (equal? (cadr inner-pattern) inner-instance))
     
     ((list? inner-pattern)
      (if (not (list? inner-instance))
			 #f
			 (if (and (> (length inner-pattern) 1) (equal? (cadr inner-pattern) '...))
				  (or (and 
						 (wnmatch (car inner-pattern) (car inner-instance))
						 (wnmatch (cddr inner-pattern) (cdr inner-instance))
						 (inner-parser (car inner-pattern) (car inner-instance))
						 (inner-parser (cddr inner-pattern) (cdr inner-instance)))
						(and 
						 (wnmatch (car inner-pattern) (car inner-instance))
						 (wnmatch inner-pattern (cdr inner-instance))
						 (inner-parser (car inner-pattern) (car inner-instance))
						 (inner-parser inner-pattern (cdr inner-instance))))
				  
				  (and 
					(wnmatch (car inner-pattern) (car inner-instance))
					(wnmatch (cdr inner-pattern) (cdr inner-instance))
					(inner-parser (car inner-pattern) (car inner-instance))
					(inner-parser (cdr inner-pattern) (cdr inner-instance))))))))

  ;; begin wparse

  (set! the-list '())

  (let ((result (inner-parser pattern instance)))
    (if result
		  the-list
		  #f)))

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

(define Wmatching #t)

