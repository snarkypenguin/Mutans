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
