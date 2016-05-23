

;***************** UNITS ******************

(define (exp-decay-rate prop period)
  (- (/ (log (- 1.0 prop)) period)))

(define (years . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (* n 365.0))

(define (weeks . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (* n 7.0))

(define (days . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  n)

(define (hours . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n 24.0))

(define (minutes . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n (* 24.0 60)))

(define (seconds . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n (* 24.0 60 60 )))

(define (m/s . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n (seconds 1))) 

(define (m/d . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n (days 1))) 


;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
