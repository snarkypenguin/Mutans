;-  Identification and Changes

;--
;	threadtest.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2013.10.23
;		Location: pooh:/local/home/randall/study/src/threadtest.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2013 CSIRO Australia
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(define (runner tag)
						(lambda ()
						  (let loop ((n (random-integer 10)))
							 (display n)
							 (display " ")
							 (display (make-string n tag))
							 (newline)
							 (thread-yield!)
							 (if (zero? n)
								  "done"
								  (loop (random-integer 10))))))


(define threads (list 
					  (make-thread (runner #\a) 'A)
					  (make-thread (runner #\b) 'B)))

(define (go)
  (for-each thread-start! threads))

(define (stop)
  (for-each thread-terminatte! threads))


						




;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
