


;***************** UNITS ******************
;; The following are for use in multiplicative expressions



;; time
(define second 1)
(define seconds second)
(define minute (* 60 seconds))
(define minutes minute)
(define hour (* 60 minutes))
(define hours hour)
(define day (* 24 hours))
(define days day)
(define week (* 7 days))
(define weeks week)
(define year (* (+ 365 1/4) day))
(define years year)

;; length
(define mm 1/1000)
(define cm 1/100)
(define dm 1/10)
(define m 1)
(define km 1000)

;; area
(define are (* 10 m 10 m))
(define ares are)
(define ha (* 100 are))
(define hectare ha)
(define hectares ha)
(define acre (* 0.4047 ha))
(define acres acre)

;; volume
(define l 1)
(define ml 1/1000)
(define cl 1/100)
(define dl 1/10)
(define kl 1000)
(define Ml 1000000)

;; mass
(define mg 1/1000000)
(define gm 1/1000)
(define kg 1)
(define Mg 1000)


;; other units
(define pound 0.45359237)
(define pounds pound)

(define foot 0.3048)
(define feet foot)
(define yard 0.9144)
(define yards yard)
(define mile 1609.344)
(define miles mile)

;; Times are based on the astronomical julian year

(define (years% . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (* n  31557600))

(define (weeks% . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (* n (days% 7)))

(define (days% . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (* n 86400))

(define (hours% . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (* n 3600))

(define (minutes% . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (* n  60))

(define (seconds% . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  n)

(define (m/s% . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n (seconds% 1))) 

(define (m/d% . n)
  (if (null? n) 
		(set! n 1)
		(set! n (car n)))
  (/ n (days% 1))) 


;--  This is here for historical reasons.

(define (exp-decay-rate prop period)
  (- (/ (log (- 1.0 prop)) period)))

;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
