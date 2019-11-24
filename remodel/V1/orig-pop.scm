;-  Identification and Changes

;--
;	pop.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2013.03.13
;		Location: Odin:/home/gray/study/src/pop.scm
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

(load "support.scm")
(load "maths.scm")
(load "integrate.scm")
(load "units.scm")
(load "postscript.scm")

(load "integrate.scm")
(load "matrix.scm")

(define (o1 f) (lambda (x) (car (f x))))
(define (o2 f) (lambda (x) (cadr (f x))))


;;(define dN1 (lambda (t N1 N2) (* r1 (/ N1 K1) (+ (- K1 N1) (* a12 N2)))))
;;(define dN2 (lambda (t N2 N1) (* r2 (/ N2 K2) (+ (- K2 N2) (* a21 N1)))))

;;(define N1N2 (rk4* (list dN1 dN2) 0 365 0.001 (list 15000 400)))

;;(define p (N1N2 's))
;;(define q (map (lambda (x) (list-ref p x)) (map (lambda (x) (* 1000 x)) (seq 365))))

(define Kg 8000000)
(define Kr 8000000)
(define Kf 8000000)

(define (make-bunny a b)
  (lambda (t R F) (* R (- 1 (/ R Kr)) (- a (* b F)))))

(define (make-cat b c e) 
  (lambda (t R F) (- (* b e R F (- 1 (/ F Kf))) (* c F))))

;; dR/dt = a*R - b*R*F
;; dF/dt = e*b*R*F - c*F

;; where the parameters are defined by: 
;;  a is the natural growth rate of bunnies in the absence of predation, 
;;  b is the death rate per encounter of bunnies due to predation, 
;;  c is the natural death rate of pussycats in the absence of food (bunnies), 
;;  e is the efficiency of turning predated bunnies into pussycats. 

(define a 0.04) ;; growth
(define b 0.0005) ;; death due to predation
(define c 0.2) ;; death due to natural causes
(define e 0.1) ;; efficiency of coverting prey to biomass

(define dB/dt (make-bunny a b))
(define dC/dt (make-cat b c e))

(define BCo '(2000 12))

(define bc-system (list dB/dt dC/dt))
(define bc (rk4* bc-system 0 36500 1 BCo))

(define bcdata (bc 's))
;;(define q (map (lambda (x) (list-ref p x)) (map (lambda (x) (* (truncate (/ (length p) 365)) x)) (seq 365))))

(define KG #f)
(define KR #f)
(define KF #t)

(define unbounded +inf.0)
		  
;; Multiplicative cap on the growth of the species. k = "unbounded" makes it go away 
(define (logistic-- a k) (if (eq? k unbounded) 1.0 (- 1 (/ a k))))

;; sums the contributors to population growth
(define growth-terms-- +)

;; mortality-terms should be included as a modifier within a growth-terms s-exp
(define (mortality-terms-- . args) (- 0 (apply + args)))


;;; ;; These functions should all return positive values and be wrapped
;;; ;;  by an appropriate form like "(mortality-terms-- ...)
;;; (define (preys-on victims att eff)
;;;   (if (not (or (null? victims) (pair? victims)))
;;; 		(set! victims (list victims)))
;;;   (if (not (or (null? att) (pair? att)))
;;; 		(set! att (list att)))
;;;   (if (not (or (null? eff) (pair? eff)))
;;; 		(set! eff (list eff)))

;;;   (if (or (not (eq? (length att) (length victims))) (not (eq? (length victims) (length eff))))
;;; 		(abort (string-append "The lengths of the victims, efficiency  and success rate lists do not match")))

;;;   (cond 
;;; 	((and (number? eff) (number? att))
;;; 		(* eff att (apply + victims)))
;;; 	((number? eff)
;;; 		(* eff (apply + (map * victims att))))
;;; 	((number? att)
;;; 		(* att (apply + (map * victims eff))))
;;; 	(else
;;; 		(apply + (map * victims eff att)))))

;;; (define (preyed-on-by attacker att)
;;;   (if (not (or (null? attacker) (pair? attacker)))
;;; 		(set! attacker (list attacker)))
;;;   (if (not (or (null? attacker) (pair? att)))
;;; 		(set! att (list att)))
;;;   (if (not (eq? (length att) (length attacker)))
;;; 		(abort (string-append "The length of the attacker list and the success rate list do not match: " (object->string attacker) ", " (object->string att))))
;;;   (if (number? att)
;;; 		(* att (apply + attacker))
;;; 		(apply + (map * attacker att))))



(define (make-rate-matrix poplist interactions)
	 (cons (cons 'RM poplist) (map (lambda (x y) (cons x y)) poplist interactions)))

(define (transpose-rate-matrix m)
  (if (not (member (caar m) '(mr RM)))
		(abort "This isn't a predation matrix (PM) or a prey matrix (mp)")
		(let ((M (list-transpose m)))
		  (if (eq? (caar M) 'RM)
				(set-car! (car M) 'mr)
				(set-car! (car M) 'RM))
		  M)))

(define (rate-matrix m primary)
  (lambda (x)
	 (list-ref (assoc primary m) (- (length (car m)) (length (member x (car m)))))))


(define (preys-on popvals preylist)
  (apply + (map * popvals (map car preylist) (map cadr preylist))))


(define (preyed-on-by popvals predator-list)
  (apply + (map * popvals (map car predator-list))))


(define grf-pm (make-rate-matrix '(grass rabbit fox) '((0 0 0) (0.0005 0 0) (0 0.0005 0))))
(define grf-ef (make-rate-matrix '(grass rabbit fox) '((0 0 0) (0.2 0 0) (0 0.25 0))))


;;;=====================================================================
;; Producer

;; gg is the growth rate of grass
(define gg 0.4)
;; Kg is the cap for grass
(define Kg 20000)
;; ar is the attack rate for rabbits
(define ar 0.0005)

;; natural mortality
(define dg 0.0)

;; *** Function ***
(define dg/dt 
  (lambda (t G R F) 
	 (if #f
		  (begin
			 (dnl "value = " G)
			 (dnl "growth = " gg)
			 (dnl "preys-on = "(preys-on (list G R F) '((0 0) (0 0) (0 0))) )
			 (dnl "total mortality = " (mortality-terms-- 
								 (preyed-on-by (list G R F) (list '(0 0) (list ar er) '(0 0)))))
			 (dnl "natural mortality = " dg)
			 (dnl "preyed-on-by = " (preyed-on-by (list G R F) (list '(0 0) (list ar er) '(0 0))))
			 ))
	 (* G 
		 (logistic-- G Kg) 
		 (growth-terms-- gg 
							  (preys-on (list G R F) '((0 0) (0 0) (0 0))) 
							  (mortality-terms-- 
								(preyed-on-by (list G R F) (list '(0 0) (list ar er) '(0 0))))) )))
;; *** -------- ***

;;;=====================================================================
;; Intermediate predator -- preys on something, and is preyed on

;; Kr is the cap for rabbits
(define Kr 2000)
;; gr is the growth rate for rabbits
(define gr 0.04)
;; af is the attack rate for foxes
(define af 0.0005)
;; er is the efficiency for rabbits
(define er 0.2)
;; ar is the attack rate for rabbits
(define ar 0.0005)
;; dr is the background mortality for rabbits
(define dr 0.4)

;; *** Function ***
(define dr/dt 
  (lambda (t G R F) 
	 (if #f
		  (begin
			 (dnl "value = " R)
			 (dnl "growth = " gr)
			 (dnl "preys-on = "(preys-on (list G R F) (list (list ar er) '(0 0) '(0 0))) )
			 (dnl "total mortality = " (mortality-terms-- 
								 (mortality-terms-- (preyed-on-by (list G R F) (list '(0 0) '(0 0) (list af ef))) 
														dr)))
			 (dnl "natural mortality = " dr)
			 (dnl "preyed-on-by = " (preyed-on-by (list G R F) (list '(0 0) '(0 0) (list af ef))))
			 ))
	 (* R 
		 ;;(logistic-- R Kr) 
		 (growth-terms-- gr 
							  (preys-on (list G R F) (list (list ar er) '(0 0) '(0 0)))
							  (mortality-terms-- (preyed-on-by (list G R F) (list '(0 0) '(0 0) (list af ef))) 
														dr)))))
;; *** -------- ***


;;;=====================================================================
;; Apex predator

;; Kf is the cap for foxes
(define Kf 2000)
;; ef is the efficiency for foxes
(define ef 0.25)
;; df is the background mortality for foxes
(define df 0.1)

(define gf 0.0)

;; *** Function ***
(define df/dt 
  (lambda (t G R F) 
	 (if #f
		  (begin
			 (dnl "value = " F)
			 (dnl "growth = " gf)
			 (dnl "preys-on = " (preys-on (list G R F) (list '(0 0) (list af ef) '(0 0))))
			 (dnl "total mortality = " (mortality-terms-- (preyed-on-by (list G R F) '((0 0) (0 0) (0 0)))
														df))
			 (dnl "natural mortality = " df)
			 (dnl "preyed-on-by = " (preyed-on-by (list G R F) '((0 0) (0 0) (0 0))))
			 ))
	 (* F 
		 (logistic-- F Kf) 
		 (growth-terms-- (preys-on (list G R F) (list '(0 0) (list af ef) '(0 0)))
							  (mortality-terms-- (preyed-on-by (list G R F) '((0 0) (0 0) (0 0)))
														df)))))

;; *** -------- ***




(dnl "grf")
;(define  grf (rk4* (list dg/dt dr/dt df/dt) 0 1770 0.5 '(8000 800 80)))

(define  grf (rk4* (list dg/dt dr/dt df/dt) 0 770 0.5 '(8000 700 80)))
(with-output-to-file "grf.dat" (lambda () (dump-data (grf 'dat))))

;;(dnl "grf2")
;;(define  grf2 (rkf* (list dG/dt dR/dt dF/dt) 0 770 0.5 '(8000 80000 8000)))
(dnl "grf done")


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
