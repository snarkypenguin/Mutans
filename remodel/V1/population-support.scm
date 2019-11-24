;-  Identification and Changes

;--
;	population-support.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2014.11.13
;		Location: pooh:/local/home/randall/study/src/population-support.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2014 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

(load "maths")
(load "integrate")
(load "matrix")


;-  Variables/constants both public and static

;; This ought to be defined in maths.scm, but we do it again Just Because.
(define %#undefined '%#undefined)

(define not-fussy #f)


;-  Code 

(define (!list? x) (not (list? x)))
(define (!pair? x) (not (pair? x)))


;--- Build  "structures"

(define (var x y)
  (if (symbol? x)
		(cons x y)
		'bad-var))

(define (define-parameters . lst)
  (if (alist? lst)
		lst
		'bad-parameters))

(define (define-species name class symbol parameters)
  (if (and (string? name)
			  (symbol? class)
			  (symbol? symbol)
			  (alist? parameters))
		(list name class symbol parameters)
		'bad-species))
  
(define (define-species-parameters-list . lst)
		(copy-list lst)
		)

;--- Access to species parameters

(define (alist? A)
  (or (null? A)
		(and (list? A) (pair? (car A)) (alist? (cdr A)))))

(define (species-parameters parameter-block species . parameters)
  (let* ((species (assoc species parameter-block))
			(params (and species (copy-list (list-ref species 3))))
			)
	 (cond
	  ((not params) #f)
	  ((null? parameters) params)
	  (#t (filter pair? (map (lambda (x) (let ((p (assoc x params))) p))  parameters))))))

;; returns a list of the parameters not present in in the parameter block -- I'm sure there is a nicer way ....
(define (!species-parameters parameter-block species . parameters)
  (let* ((species (assoc species parameter-block))
			(params (or (and species (copy-list (list-ref species 4)))
							#f)))

	 (cond
	  ((not params) #f)
	  ((null? parameters) '())
	  (#t (filter !pair? (map (lambda (x) (let ((p (assoc x params))) (if p p x)))  parameters))))))
		  

(define (parameter key lst)
  (let ((p (assoc key lst)))
	 (if (list? p) (cdr p) %#undefined)))


;--- Access to patch parameters







;--- functions of historical or educational interest

;; Growth models (such as exponential or sigmoidal) need to be implemented with a *-population function,
;; a *-growth function, and an entry in the case statement for calculating the delta in the model body.
;; ... 

;; returns population at time t, t <= 0 <= domain
(define (exponential-population t domain P_0 K lmb)
  ;;(set! lmb (* 3 pi lmb)) ;; This makes it reasonably compatible with the sigmoid
  ;;(set! lmb (* 2 pi lmb)) ;; This makes it reasonably compatible with the sigmoid
  ;;(set! lmb (* 3/2 pi lmb)) ;; This makes it reasonably compatible with the sigmoid (with lmb 0.5)
  ;(set! lmb (* e pi lmb)) ;; This makes it reasonably compatible with the sigmoid (a)
  ;;(set! lmb (* 1/2 pi lmb)) ;; This makes it reasonably compatible with the sigmoid

  ;;(+ P_0 (/ (- K P_0) (1+ (exp (* (- lmb) t))))) ;; formulation a
  (if (> t domain)
		K
		(+ P_0 (* (- K P_0) (- 1 (exp (* (- (/ lmb domain)) t)))))  ;; formulation b
		)
  )



;; returns population at time t, t <= 0 <= domain
(define (sigmoidal-population t domain P_0 K lmb)
  (if (> t domain)
		K
		(* K (sigmoid* (/ t domain) lmb))))

;; returns the time t, t <= 0 <= domain, which is appropriate for a population level of P 
(define (inverse-exp-growth dt domain P_0 P K lmb)
  ;(set! lmb (* e pi lmb)) ;; This makes it reasonably compatible with the sigmoid (a)
  (cond
	((<= P 0.0 0)
	((<= P P_0) 0.0)
	((< P K) (/ (* (/ (- domain) lmb) (log (- 1.0 (/ (- P P_0) (- K P_0))))) domain))
	(#t 1.0))))


(define (general-growth-function forward backward domain P_0 K lmb)
  (let ((domain domain) (P_0 P_0) (K K) (lmb lmb))
	 (lambda (dt P)
		(if (>= P K)
			 0.0
			 (- (forward (+ dt (backward dt domain P_0 P K lmb)) domain P_0 P K lmb)))
		)))  

;(define exponential-growth
;  (general-growth-function domain P_0 K lmb))

;(define sigmoidal-growth
;  (general-growth-function domain P_0 K lmb))


(define (exponential-growth dt domain P_0 P K lmb) 
;; This *does not* diddle lmb ... any diddles need to be in inverse-exp-growth and exponential-population
  (if (>= P K)
		0
		(let* ((t (* domain (inverse-exp-growth dt domain P_0 P K lmb)))
				 (NP (exponential-population (+  dt t) domain P_0 K lmb))
				 )
		  (-  NP P))))


(define (sigmoidal-growth dt domain P_0 P K lmb) ;; This will abort since we don't have an inverse-sigmoid* function
  (if (>= P K)
		0
		(- (sigmoidal-population (* domain (+ (/ dt domain) (inverse-sigmoid* (/ P K) lmb))) domain P_0 K lmb) P)))


(define (no-growth  dt domain P_0  value capacity  rvalue)
  0)



;; These will be run through the rk4* routine in a dynamic-patch; the
;; intrinsic growth happens at the patch/ecoservice level.  I know
;; that it really ought to do both at once (we could, but at the
;; expense of "easy playing").

;; These two functions are only concerned with *interactions* that change biomas (predation, mutualism, commensalism, amensalism) and metabiosis;
(define (plant-interactions t populations species predation-vector conversion-efficiency-vector prey-vector interactions)
  (growth-terms-- (mortality-terms--  (apply + (map * prey-vector populations))))
  )

(define (animal-interactions t populations species predation-vector conversion-efficiency-vector prey-vector interactions)
  (growth-terms--  (apply + (map * predation-vector conversion-efficiency-vector populations))
						 (mortality-terms--  (apply + (map * prey-vector populations))))
  )



;--- Useful in general

;; intrinsic growth (positive or negative) is handled in the ecoservice rather than the patch
;; These functions get called once we know the capacity of the patch -- it will *not* cope with changing capacity!

(define (logistic*-growth-model capacity mean-time-to-cap)
  (let ((c capacity)
		  (m mean-time-to-cap) )
	 (lambda (t)(if (> t m) c (* c (logistic* (/ t m)))))))


(define (inverse-logistic*-growth-model	capacity mean-time-to-cap)
  (let ((c capacity)
		  (m mean-time-to-cap) )
	 (lambda (M)(if (> t m) m (* m (invlogistic (/ M c)))))))


;; traditional population dynamics growth model as the RHS of a  D.E.
(define (Logistic t M r K) ;; time, mass growth capacity
  (let ((r r)(K K))
	 (lambda (t M)
			(* r M  (- 1.0 (/ M K))))))


;; This gets called when an ecoservice is created.  In the context of patches, it must happen when the size of the
;; patch is known, since that governs the the capacity and possibly the growth rate and  rate.
;; 
;;
;;      (differential-model dP-sinusoid 'incomplete 3300.0 30.0  0.5)
;;
;; which indicates that we will be reinitialising and that the base carrying capacity for the grass is 3.3Mg/ha
;; and a  of half a kilo per hectare per day.  When we call this in the patch initialisation, the area of
;; the patch is 
;; 


(define (interval-list To Tmax Tn)
  (map (lambda (x) (+ To (* x (- Tmax To)))) (seq (+ Tn 1))))










;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
