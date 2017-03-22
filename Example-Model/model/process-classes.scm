; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	process-classes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2017.03.14
;		Location: zero:/home/randall/Thesis/Example-Model/model/process-classes.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2017 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(define-macro (define-class cname #!rest specs)
  (let* ((filename (string-append paramdir (symbol->string cname)))
			(class-init (string-append (symbol->string cname) "-parameters"))
			(inherits '())
			(states '()))
	 (cond
	  ((and (equal? (caar specs) 'inherits-from)
			  (equal? (caadr specs) 'state-variables))
		(set! inherits (cdar specs))
		(set! states (cdadr specs)))
	  ((and (equal? (caar specs) 'state-variables)
			  (equal? (caadr specs) 'inherits-from))
		(set! states (cdar specs))
		(set! inherits (cdadr specs)))
	  ((and (equal? (caar specs) 'inherits-from)
			  (null? (cdr specs)))
		(set! inherits (cdar specs)))
	  ((and (equal? (caar specs) 'state-variables)
			  (null? (dr specs)))
		(set! states (cdar specs)))
	  )
	 
	 ;(if (odd? (length states)) (error "The state variable list must have an even number of entries!" cname states))

	 `(with-output-to-file ,filename
		(lambda ()
		  (display "'Parameters")(newline)
		  (display "(define ")
		  (display ',class-init)(newline)
		  (display "  (list\n")
		  (for-each
			(lambda (x y)
			  (display "    (list ")
			  (display "'")
			  (display x)
			  (display " ")
			  (write y)
			  (display ")\n"))
			;;(evens ',states) (odds ',states)
			',states (make-list% (length ',states) '<uninitialised>)
			)
		  (display "  )\n)\n")
		  (display (string-append "(set! global-parameter-alist (cons (cons " ,(symbol->string cname) " " ,class-init ") global-parameter-alist))\n"))
		  ))
	 
	 ))

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
