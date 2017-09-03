

(define model-timing-data '())

(define (timing-report)
  (for-each
	(lambda (x)
	  (dnl* (let* ((s (object->string (car x))) (S (string-append (make-string (max 0 (- 40 (string-length s))) #\space) s))) S)
;			  "| cpu:"  (cadr x) ": sys" (caddr x) ": real-time" (cadddr x)
			  "| cpu:"  (cadr x) ": real-time" (cadddr x)
			  ))
	model-timing-data))

(define (aggregate-model-timing-data)
  (dnl* 'Oink)
  (for-each
	(lambda (x)
	  (let ((dat (apply add (cdr x))))
		 (set-cdr! x dat) ;; dat is a list of lists of numbers, we want the colum sums
		 ))
	model-timing-data))

