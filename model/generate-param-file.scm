; -*- mode: scheme; -*-
"
  (include \"generate-param-file.scm\")
  (include \"bugrit\")
"


(load "preamble.scm")
(dnl "Loaded preamble")

(define paramdir "params/")
(define input-files '("animal-classes.scm" "diffeq-classes.scm"
							 "remodel-classes.scm" "introspection-classes.scm"
							 "landscape-classes.scm" "log-classes.scm"
							 "model-classes.scm" "monitor-classes.scm"
							 "plant-classes.scm"))
(dnl "Considering " input-files)

(define input '())


(define (make-list% n . init)
  (if (<= n 0) 
		'()
		(if (null? init)
			 (cons '() (make-list% (- n 1)))
			 (cons (car init) (make-list% (- n 1) (car init))))))

(define (make-class #!rest args) (void))
;;(define (evens lst) (if (null? lst) '() (cons (car lst) (if (pair? (cdr lst)) (evens (cddr lst)) '()))))
;;(define (odds lst) (if (pair? (cdr lst)) (evens (cdr lst)) '()))

(include "process-classes.scm")

(define (GO)
  (let ((f (open-output-file "./bugrit")))

	 (display "Looping through the data")

	 (for-each
	  (lambda (x)
		 (display "Processing file ")(display x)(newline)
		 (set! input (append input (filter
											 (lambda (y)										  
												(and (pair? y)
													  (symbol? (car y))
													  (not (member (car y) '(include load Comment)))))
											 (with-input-from-file x read-all))))
		 (display "input now contains ")(display (length input))(display " entries\n")
		 )
	  input-files)

	 (display "bugrit = ")
	 (display f) (newline)

	 (for-each
	  (lambda (x)
		 (display "Loading entry\n")
		 (display x f)
		 (newline f)
		 )
	  input)

	 (close-output-port f)
	 )
;(delete-file "bugrit")
)

(GO)

(dnl "Try running (include \"bugrit\")")

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
