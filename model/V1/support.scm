;-  Identification and Changes

;--
;	support.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2012.11.20
;		Location: odin:/home/gray/study/src/support.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2012 CSIRO Australia
;   All rights reserved
;

;-  Discussion 
#|
    This set of files is largely interdependent.  There ought to be no issues though.
|#


;-  Configuration stuff 

;-  Included files 

(define maybe-load
  (let ((loaded-files '("support.scm"))
		  (LOAD load))
	 (lambda file
		(cond
		 ((null? file) (set! file #f))
		 ((pair? (cdr file))
		  (for-each (lambda (f) (maybe-load f)) file))
		 (else
		  (set! file (car file))
		  
		  (cond
			((not file) loaded-files)
			((not (member file loaded-files))
			 (display "loading ")(display file)(newline)
			 (LOAD file)
			 (set! loaded-files (cons file loaded-files)))
			(else file)))))))

(define (aborts . args) (abort (apply string-append args)))

;;(set! load maybe-load)

(load "utils.scm")
(load "units.scm")
(load "postscript.scm")
(load "sort.scm")
(load "maths.scm")
(load "integrate.scm")
(load "matrix.scm")






;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
