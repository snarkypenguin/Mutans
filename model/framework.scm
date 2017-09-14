(include "framework")
; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;  framework.scm -- Written by Randall Gray 
;  Initial coding: 
;     Date: 2016.07.26
;     Location: zero:/home/randall/Thesis/Example-Model/model/framework.scm
;
;  History:
;
;  This has the "defined" bits, not just the macros.
"
    Copyright 2017 Randall Gray

    This file is part of Remodel.

    Remodel is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Remodel is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Remodel.  If not, see <http://www.gnu.org/licenses/>.
"

;-  Code 


(define (lookit-q a) (map (lambda (x) (x a)) (list taxon cnc subjective-time agent-state queue-state)))

;; The functions returned by mass-at-age-function and age-at-mass-function will
;; return an imaginary value for a mass or age which lies outside the defined
;; domain. The absolute value of the returned value is the minimum or maximum
;; mass or age.


;; These are (mainly) used for attenuating colors
(define (saturate x k)  (+ x (* (- 1 x) k)))
(define (saturate* x k n) (saturate x (if (zero? n) 0.0 (- 1 (exp (* n (log k)))))))

(define (map-saturate* x k n)
  (if (list? x)
		(map (lambda (y) (saturate* y k n)) x)
		(saturate* x k n)
		))

;; mass_at_age(m, a, M, A, age) = m + (M - m) * (1 - exp( -2 * exp(1) *(age - a)/(A - a)))
;;-->            (+ m (* (- M m) (- 1 (exp (* -2 e (/ (- age a) (- A a)))))))


;; age_at_mass(m, a, M, A, mass) = a + (A-a)/(-2 * exp(1)) * log(1 - (mass - m)/(M - m))
;;--> (+ a (* (/ (- A a) (* -2 (exp 1))) (log (- 1 (/ (- mass m) (- M m))))))

(define (PP ob) ;; Can pp methods as well as procedures
  (if (isa? ob <method>)
		(pp (method-procedure ob))
		(pp ob)))


;; sel is typically an ordering op like <=, value is an accessor for the elements of L
(define (best-N N sel value L)
  (if (not (procedure? value))
		(set! value (lambda (x) (x))))
  
  (let ((split ;; sheep-from-goats
			(lambda (sel value lst)
			  (let loop ((L lst)
							 (s '())
							 (g '()))
				 (if (null? L)
					  (list (reverse s) (reverse g))
					  (let ((q (car L)))
						 (if (sel (value q))
							  (loop (cdr L) (cons q s) g)
							  (loop (cdr L) s (cons q g))))))))
		  )
	 (let pick ((interesting L)
					;(boring '())
					(M (apply max (map value L)))
					(m (apply min (map value L)))
					(set-list '())
					)
		
		(let ((cut@ (/ (+ M m) 2)))
		  (if (<= (length interesting) N)
				interesting
				(let ((sg (split (lambda (x) (sel x cut@)) value interesting)))
				  (pick (car sg)
						  ;;interesting
						  ;;;(cadr sg) 
						  (apply max (map value (car sg)))
						  (apply min (map value (car sg)))
						  (cons interesting set-list))))))))

	 


(define (random-direction #!optional n)
  (if (not n) (set! n 3))
  (let* ((v (map (lambda (x) (+/- (random-real))) (sequence n)))
			(nv (normalise v)))
	 (if (and (not (pair? nv))(nan? nv)) (random-direction n) nv)))


(define ($mass-at-age-function m a M A)
  (lambda (age)
	 (cond
	  ((not (number? age))
		(dnl* "Age is bounded by " a "and" A "and mass is bounded by" m "and" M))
	  ((< age a) (* m 0+1i))
	  ((= age a) m)
	  ((>= age A) (* M 0+1i))
	  (else (+ m (* (- M m) (- 1 (exp (* -2 e (/ (- age a) (- A a))))))))
	  )))

(define ($age-at-mass-function m a M A)
  (lambda (mass)
	 (cond
	  ((not (number? mass))
		(dnl* "Age is bounded by " a "and" A "and mass is bounded by" m "and" M))
	  ((< mass m) (* a 0+1i))
	  ((= mass m) a)
	  ((>= mass M) (* A 0+1i))
	  (else (+ a (* (/ (- A a) (* -2 (exp 1))) (log (- 1 (/ (- mass m) (- M m)))))))
	  )))

(define (mass-at-age-function m a M A)
  (lambda (age)
	 (cond
	  ((not (number? age))
		(dnl* "Age is bounded by " a "and" A "and mass is bounded by" m "and" M))
	  ((<= age a) m)
	  ((>= age A) M)
	  (else (+ m (* (- M m) (- 1 (exp (* -2 e (/ (- age a) (- A a))))))))
	  )))

(define (age-at-mass-function m a M A)
  (lambda (mass)
	 (cond
	  ((not (number? mass))
		(dnl* "Age is bounded by " a "and" A "and mass is bounded by" m "and" M))
	  ((<= mass m) a)
	  ((>= mass M) A)
	  (else (+ a (* (/ (- A a) (* -2 (exp 1))) (log (- 1 (/ (- mass m) (- M m)))))))
	  )))

(define (APPLY f lst)
  (let ((N 8192))
	 (if (< (length lst) N)
		  (apply f lst)
		  (let ((nlst (cons (apply f (list-head lst N)) (list (APPLY f (list-tail lst N))))))
			 (APPLY  f nlst)))))

;; this records all the calls defined with define%, model-method% or model-body%
(define %%%-time-register-%%% '()) 

(define time-field-width 8)
(define ps-default-margin 10) ;; implicitly mm NOTE

(define support-dir "./")

(define (Note flag #!rest args)
  (void))


(define (Alternates? lst p q)
  (if (null? lst)
		#t
		(and (pair? lst) (pair? (cdr lst))
			  (eq? p (car lst)) (eq? q (cadr lst))
			  (alternates? (cddr lst) p q))))

;; This version does not insist that the list be even
(define (alternates? lst p q)
  (if (null? lst)
		#t
		(and (pair? lst) (eq? (car lst)) (alternates? q p (cdr lst)))))

(define (make-list k #!rest v)
  (cond
	((zero? k) '())
	((negative? k) (error "Negative length in make-list" k ))
	(else (cons (car v) (make-list (- k 1) (car v))))))

;; (define (make-circle-perimeter location radius #!optional divisions)
;;   (if (not (number? divisions)) (set! divisions 120))
;;   (let ((tau (* 2 (acos -1))))
;; 	 (translate-pointlist 
;; 	  location 
;; 	  (scale-pointlist radius 
;; 							 (map (lambda (x) 
;; 									  (list (cos (/ (* tau x) divisions)) 
;; 											  (sin (/ (* tau x) divisions))))
;; 									(list-tabulate divisions (lambda (x) x))
;; 									)))))

(define (make-circle-perimeter location radius #!optional divisions)
  (let ((divisions (if (not divisions) 120 divisions))
		  (tau (* 2 (acos -1.0))))
	 (translate-pointlist
	  location
	  (scale-pointlist radius
							 (append
							  '((1 0))
							  (map (lambda (x) (list (cos (* tau (/ x divisions)))
															 (sin (* tau (/ x divisions)))))
									 (sequence divisions))
							  '((1 0)))))))

(define (make-box ll ur)
  (list ll (list (car ll) (cadr ur)) ur (list (cadr ll) (car ur)) ll))

(define (project-from-one-to-another from to point)
  (let ((f->m (get-loci$al->model from ))
		  (m->t (get-model->local to)))
	 (if (or (not (procedure? f->m)) (not (procedure? m->t)))
		  (error "bad projection chain"))
	 (m->t (f->m point))))


(define (refresh-introspection-targets iagent Q)
  (let ((selector (slot-ref iagent 'introspection-selector)))
	 (slot-set! iagent 'introspection-targets (filter selector Q))))
"Note well: model migration may need to prompt a refresh of the introspection agents' lists"


;; The following three are used in loading default values from the parameter files
(define %%default-initialisations%% '())
(define (add-default-initialisation-for key lst)
  (set! %%default-initialisations%% (cons (cons key lst) %%default-initialisations%%)))

(define (initialisation-defaults-for key)
  (let ((l (filter (lambda (x) (eq? key (car x))) %%default-initialisations%%)))
	 (if (null? l)
		  l
		  (apply append (map cdr l)))))


;; This is a list of data added by a (definition-comment ...) clause associated with a
;; (define...)


(define (set-uninitialised-slots obj lst value)
  (for-each
	(lambda (slt)
	  (if (uninitialised? (slot-ref obj slt))
			(begin
			  ;;(dnl*  "Setting"  slt "to" value "for" (cnc obj) (slot-ref obj 'taxon))
			  (slot-set! obj slt value)
			  (if (not (equal? (slot-ref obj slt) value)) (error "Bad initialisation in set-uninitialised-slots" (cnc obj) (slot-ref obj 'taxon) slt value ))
			  )
			))
	lst))

(define (fail-on-uninitialised-slots obj lst)
  (let ((lst (filter (lambda (x) x) (map 
												 (lambda (x)
													(if (uninitialised? (slot-ref obj x))
														 x
														 #f))
												 lst))))
	 (if (pair? lst)
		  (begin
			 (dnl*  "The following slots need to be set for" (cnc obj) (slot-ref obj 'taxon))
			 (pp lst)
			 (abort 'uninitialised-slots lst)))
	 ))



(define definition-comments '()) ;; collects comments from the code


;; ;; basic projection -- scales both axes independently
;; (define (U:mspace->ospace m-domain o-domain)
;;   (let* ((Mo (map - (cadr m-domain) (car m-domain)))
;; 			(Oo (map - (cadr o-domain) (car o-domain)))
;; 			(f (lambda (p)
;; 				  (map exact->inexact
;; 						 (map + (map * (map / (map - p (car m-domain)) Mo)
;; 										 Oo)
;; 								(car o-domain))))))
;; 	 f))

(define (page-domain s)
  (cond
	((and (pair? s) (pair? (car s)) (pair? (cadr s))
			(= (length s) 2) (= (length (car s)) 2) (= (length (cadr s)) 2))
	 s)
	((and (pair? s) (number? (car s)) (number? (cadr s))	(= (length s) 2))
	 (list '(0 0) s))
	(else (error "bad papersize" s))))

;; (define (m->ps xy)
;;   (rescale 2834.64646464646464646 x))

;; (define (ps->m x)
;;   (rescale (/ 1.0 2834.64646464646464646) x))

;; (define (km->ps xy)
;;   (rescale 2834646.46464646464646 x))

;; (define (ps->km x)
;;   (rescale (/ 1.0 2834646.46464646464646) x))


(define *default-projections*
  ;; The projections must take whole vectors since we cannot (properly) map
  ;; geographic ordinates otherwise.  mapf converts an ordinate-wise mapping
  ;; to a vector function.
  (list
	(cons 'I  (lambda (x) x)) ;; The identity mapping
	;;	(cons 'km->ps (mapf km->ps)) ;; defined above
	;;	(cons 'ps->km (mapf ps->km)) ;; defined above
	;;	(cons 'm->ps (mapf m->ps)) ;; defined above
	;;	(cons 'ps->m (mapf ps->m)) ;; defined above
	;;	(cons 'mm->ps (mapf mm->points)) ;; defined in postscript.scm
	;;	(cons 'ps->mm (mapf points->mm)) ;; defined in postscript.scm
	;;	(cons 'm->mm (mapf (lambda (x) (* x 1000))))
	;;	(cons 'mm->m (mapf (lambda (x) (/ x 1000))))
	;;	(cons 'mm->points (mapf mm->points)) ;; defined in postscript.scm
	;;	(cons 'points->mm (mapf points->mm)) ;; defined in postscript.scm
	;;	(cons 'inches->points (mapf inches->points)) ;; defined in postscript.scm
	;;	(cons 'points->inches (mapf points->inches)) ;; defined in postscript.scm
	;;	(cons 'mm->inches (mapf mm->inches)) ;; defined in postscript.scm
	;;	(cons 'inches->mm (mapf inches->mm)) ;; defined in postscript.scm
	))

  ;;; might be useful sometime
  ;;; (let* ((map:ll-ur->m (mapf (lambda (x) (* units-in-m x))))
  ;;; 			(mll (map:ll-ur->m ll))
  ;;; 			(mur (map:ll-ur->m ur))
  ;;; 			)
(define Domain <uninitialised>)
(define Range <uninitialised>)

(define PageSize isoA4) ;; default value

(define default-margins 10) ;; implicitly in mm.

(define ->map-output (lambda (x) x))

(define Valid-location (lambda x #f)) ;; This must be set before the model runs

;; Page sizes should always be specified in mm 
(define (set-model-domain! ll ur #!optional pagesize) ;; 10mm margins all around
  (set! Domain (list ll ur))
  (set! Range (map - ur ll))

  (set! Valid-location (lambda (p) (and (<= (car ll) (car p))
													 (<= (car p) (car ur))
													 (<= (cadr ll) (cadr p))
													 (<= (cadr p) (cadr ur)))))
;  (add-ps-projection	
;	'model->ps
;	'ps->model
;	(list ll ur)
;	(map mm->points (if (not pagesize) isoA4 pagesize))
;	default-margins)
  (load "output-mappings.scm")
  )

(define (add-to-*default-projections* p k)
  (if (not (assoc k *default-projections*))
		(set! *default-projections* (cons  (cons k p) *default-projections*))
		(error "Projection already exists" k)
		)) 

(define (add-ps-projection key invkey model-domain pagesize #!rest margin)
  (set! margin (if (null? margin) 5 (car margin)))
  (let ((ps (map:domain-to-postscript model-domain pagesize margin)))
	 (add-to-*default-projections* ps key)
	 (if invkey (add-to-*default-projections* (ps 'inverse) invkey))
	 ))

;-- makes a linear mapping function, each of the args is a rectangular bounding box (ll ur)
;; This mapping fits the o-domain by contraction: the scales on the axes may differ!
(define (n-linear-map domain codomain)
  ;; (p-d0)*(D1-D0)/(d1-d0) + D0
  (lambda (p)
	 (let ((d0 (car domain))
			 (d1 (cadr domain))
			 (D0 (car codomain))
			 (D1 	(cadr codomain))
			 )	
		(map + 
			  (map * (map - p d0)
					 (map / (map - D1 D0) (map - d1 d0)))
			  D0)))
  )

(define (reciprocal x) (/ 1 x))


(define (domain-transposed D)
  (let ((D (map (lambda (x) (list-head x 2)) D)))
	 (list (list (car (car D)) (car (cadr D)))
			 (list (cadr (car D)) (cadr (cadr D))))))



(define (is-in x #!rest domain)
  (cond
	((not (pair? domain)) #f)
	((pair? (car domain))
	 (and (<= (caar domain) x) (<= x (cadar domain))))
	((and (number? (car domain)) (not (null? (cdr domain))) (number? (cadr domain)))
	 ((and (<= (car domain) x) (<= x (cadr domain)))))
	(else #f)))




"
The linear-map function constructs a linear mapping from a domain to a codomain
"
(define (linear-map domain codomain #!optional inverse)
  ;; This implicitly clips to the shallowest domain
  ;; (p-d0)*(D1-D0)/(d1-d0) + D0

  ;; This is a hack which we include for convenience.  
  (if (number? (car domain))
		(set! domain (list (make-list (length domain) 0) domain)))
  
  (if (number? (car codomain))
		(set! codomain (list (make-list (length domain) 0) codomain)))
  
  (let* ((N (apply min (map length (append domain codomain))))
			(C (lambda (x) (list-head x N)))
			(d0 (C (car domain)))
			(d1 (C (cadr domain)))
			(D0 (C (car codomain)))
			(D1 (C (cadr codomain)))
			(scale (make-list N (apply min (map abs (map / (map - D1 D0) (map - d1 d0))))))
			(invscale (map reciprocal scale))
			(g inverse)
			) 

	 (letrec ((f (lambda (p)
						(if (and #f (list? p))
							 (begin
								(dnl* 'p p '-- 'd0 d0 'D0 D0 (if inverse 'invscale 'scale) (if inverse invscale scale))
								(dnl* "(map - p d0)" (map - p d0))
								(dnl* "(map * (map - p d0) (if inverse invscale scale))" (map * (map - p d0) (if inverse invscale scale)))
								(dnl* "(map + (map * (map - p d0) (if inverse invscale scale))" (map + (map * (map - p d0) (if inverse invscale scale)) D0))))
						(cond
						 ((and (pair? p) (apply andf (map number? p)))
						  (let ((R (map + (map * (map - p d0) (if inverse invscale scale)) D0)))
							 (if (and (is-in (car R) (car (domain-transposed codomain)))
										 (is-in (cadr R) (cadr (domain-transposed codomain))))
								  R
								  (begin
									 ;;(dnl* "Domain failure:" p "->" R "from" domain "to" codomain) ;; sometimes it just tries to dig outside the sandbox
									 R
									 ))))
						 ((eq? p '1/scale) (map reciprocal (if inverse invscale scale)))
						 ((eq? p 'scale) (if inverse invscale scale))
						 ((eq? p 'verbose) (set! verbose #t))
						 ((eq? p 'quiet) (set! verbose #f))
						 ((eq? p 'domain) codomain)
						 ((eq? p 'codomain) domain)
						 ((eq? p 'inverse)
						  (if (not g)
								(set! g (linear-map domain codomain f)))
						  g)
						 ((eq? p 'help) (dnl* "The symbols are '1/scale scale verbose quiet domain docomain inverse help show N d0 d1 D0 D1 and invscale"))
						 ((eq? p 'show)
						  (dnl "projection:")
						  (dnl* " " 'N N)
						  (dnl* " " 'd0 d0)
						  (dnl* " " 'd1 d1)
						  (dnl* " " 'D0 D0)
						  (dnl* " " 'D1 D1)
						  (dnl* " " 'scale (if inverse invscale scale))
						  (dnl* " " 'invscale (if inverse invscale scale))
						  (pp f))
						 (else (error "Bad argument to linear map, try a point, 'inverse 'scale, 'domain or 'codomain" p))))))
		f)))




;-- makes a linear mapping function, each of the args is a rectangular bounding box (ll ur)
;; This mapping fits the o-domain by contraction
(define (linear2d:model-space->output-space domain codomain)
  (let* ((c2 (lambda (x) (list-head x 2)))
			(domain (c2 domain))
			(codomain (c2 codomain)))
	 (linear-map domain codomain)))

;-- makes a *linear mapping function, each of the args is a rectangular bounding box (ll ur)
;; This mapping fits the o-domain by using different scales for the axes
(define (*linear2d:model-space->output-space domain codomain)
  (let* ((c2 (lambda (x) (list-head x 2)))
			(domain (c2 domain))
			(codomain (c2 codomain)))
	 (n-linear-map domain codomain)))

;; (define (map:domain-to-postscript model-domain pagesize #!optional margin #!rest use-*linear-map)
;;   ;; Defaults to a regular scale.
;;   ;; margin must come in as a measure in mm, if margin is a pair, the car is side
;;   ;; margins and the cadr is the top and bottom length

;;   (if (not margin) (set! margin ps-default-margin))

;;   (set! use-*linear-map (if (null? use-*linear-map) #f (car use-*linear-map)))

;;   (set! margin ;; convert margin into points
;; 		  ((mapf mm->points)
;; 				  (list-head (cond
;; 					((number? margin) (list margin margin))
;; 					((and (pair? margin) (< (length margin) 2)) (list (car margin) (car margin)))
;; 					(#t margin)) 2)))

;;   ((if use-*linear-map
;; 		 *linear2d:model-space->output-space
;; 		 linear2d:model-space->output-space)
;; 	(list-head model-domain 2)
;; 	(list margin (map - ((mapf mm->points) (list-head pagesize 2)) margin)) ;; expects page sizes in mm
;; 	))

;; this implicitly makes "1" in the model domain a mm away from "0"
;; as a result of specifying the size of pages in mm
(define (map:domain-to-postscript domain pagesize #!optional margin #!rest use-*linear-map)
  (if (not margin) (set! margin ps-default-margin))
  (if (number? margin) (set! margin (list margin margin)))
  (if (and (pair? margin) (null? (cdr margin))) (set! margin (cons (car margin) margin)))

  (set! margin (map mm->points margin))         ;; set the ordinates to points rather than mm
  (set! pagesize ((mapf mm->points) pagesize))   ;; since postscript works in points by definition
  
  (set! use-*linear-map (if (null? use-*linear-map) #f (car use-*linear-map)))

  (set! margin ;; Do not convert margin into points -- wait till we have done everything in "units"
		  (list-head (cond
						  ((number? margin) (list margin margin))
						  ((and (pair? margin) (< (length margin) 2)) (list (car margin) (car margin)))
						  (else margin)) 2))

  (let ((lm   (if use-*linear-map
						(n-linear-map (map (lambda (x) (list-head x 2)) domain) (list margin (map - (list-head pagesize 2) (map + margin margin))))
						(linear-map (map (lambda (x) (list-head x 2)) domain) (list margin (map - (list-head pagesize 2) (map + margin margin))))
						))
		  )
	 (if #t
		  (begin
			 (dnl "*****************************")
			 (dnl* 'Domain domain '-> (map (lambda (x) (list-head x 2)) domain))
			 (dnl* "Default margins" '-> margin)
			 (dnl* 'Page pagesize '-> (list  margin (map - (list-head pagesize 2) (map + margin margin))))
			 (dnl* "Printing area" '-> (lm (list-head (car Domain) 2)) 'x (lm (list-head (cadr Domain) 2)))
			 (dnl "*****************************"))
		  )
	 lm))








;(define (apply-pointwise-projection 


(define (slow-sort lst)
  (let ((<=? (lambda (x y) (string<=? (object->string x) (object->string y)))))
	 (sort lst <=?)))


(define (count-keyed-members lst) ;; used by monitors
  (let* ((slst (slow-sort lst))
			(keys (uniq (map car slst)))
			(lslst (map (lambda (k) (filter (lambda (c) (eqv? k (car c))) slst)) keys)) ;; 
			)
	 (map length lslst)))

;; subsidiary-agents are agents which may be embedded in a larger dynamic agent. Agents know 
;; what their parent agent is (if they have one) and may indicate to the parent that they should be
;; added to the active list. The parent agent is the one that actually decides if a agent is to move 
;; into the active queue or out of the active queue.  Whe things get moved, "value" from the parent is 
;; moved into the relevant sub-agents.  The set of ecoservices of the parent contains all of the types 
;; represented in its sub-agents.

;(letrec ((m make))
;  (set! make
;		  (lambda (class . initargs)
;			 (let ((instance (apply m (cons class initargs))))
;					 (apply initialise (cons instance initargs))
;				instance))))


;-- Loggers ****

;--- list of tags for introspection and logging

(define adjust-grey #t) ;; used by agents feeding loggers for generating graphical output
(define logger-tags '())
(define (no-default-variables) '()) ;; used to indicate that there are no defaults 


(define (type-of x)
  (cond
	((null? x) 'null)
	((list? x) 'list)
	((class? x) (class-register 'type x))
	((instance? x) (class-register 'type (class-of x)))
	((zero? x) 0)
	((integer? x) 'integer)
	((rational? x) 'rational)
	((and (number? x) (not (complex? x))) 'real)
	((and (number? x) (complex? x)) 'complex)
	((string? x) 'string)
	((character? x) 'character)
	((symbol? x) 'symbol)
	((boolean? x) 'boolean)
	((port? x) 'port)
	((procedure? x) 'procedure)
	((continuation? x) 'continuation)
	((vector? x) 'vector)))


;; Useful for growing things...

(define  (nominal-growth-rate self dt #!optional check)
  (let* ((CK (and check (has-slot? self 'age)
						(has-slot? self 'mass) (has-slot! self 'mass-at-age)))
			(mass-at-age (slot-ref self 'mass-at-age))
			)
	 (if (or CK (not check))
		  (let ((age (slot-ref self 'age))
				  (mass (slot-ref self 'mass)))
			 (/ (- (mass-at-age age) (mass-at-age (+ age dt))) dt))
		  (error "Bad nominal-growth-rate call, missing age, mass or mass-at-age"))))


(define (nominal-growth-rate* self dt)
  (let ((m@a (slot-ref self 'mass-at-age))
		  (age (slot-ref self 'age)))
	 (/ (- (m@a (+ age week)) (m@a age)) week)))




;; This returns a displacement vector, and would typically used in animal movement 
;(-1 + sqrt(1 + 4.0 * dirvy * dirvy * speed * dt)) / (2.0 * dirvy * dirvy);

(define (Xstoch-walk here there dt ndt var spd)
  (let* ((dir (map - there here))
			(delta_r (* (random-real) var pi))
			(N (/ dt ndt))
			;;(rad (var * sqrt(dt/ndt) + (1 - var) * N) * spd * dt);
;			(radius (min (sqrt (apply + (map sqr (map - there here)))) (/ (+ (sqrt (+ 1 (*  var var spd dt))) -1) (* 2 var var))))
			(radius (min (sqrt (apply + (map sqr (map - there here)))) (* (+ (* var  (sqrt N)) (* (- 1 var) N)) dt spd)))
			(rp (simple-nrnd radius (sqrt radius) 0 (* dt spd)))
;			(rp (simple-nrnd radius radius 0 (* dt spd)))
;			(rp (+ radius (simple-nrnd (sqrt radius) radius 0 (* dt spd))))
			(theta (* (- (* 2 (random-real)) 1) (sqr var) pi))
			(displacement (map (lambda (x) (* rp x)) (rotated-vector (normalise dir) theta)))
			)
	 ;;	 (dnl* 'delta_r delta_r 'N N 'radius radius 'rp rp 'theta theta)
	 displacement))


(define (XXstoch-walk here there dt ndt var spd)
  (let* ((dir (normalise (map - there here)))
			(N (/ dt ndt)) ;; ndt is the  notional time between changes in direction
			(radius (* spd (sqrt (/ (* 2 N) 2)) (gamma 3/2)))
			(theta (* (- (* 2 (random-real)) 1) (sqr var) pi))
			(rp (simple-nrnd radius (sqrt radius) 0 (* dt spd)))
			(rw (map (lambda (x) (* (- 1 var) rp x)) (rotated-vector dir theta)))
			(dw (map (lambda (x) (* var rp x)) dir))
			)
	 ;;(dnl* "[][][][]--- " 'here '-> there ":" 'direct dw 'random rw 'theta theta 'rad rp)
	 (map + rw dw)))




(define (stoch-walk here there dt ndt attr sp)	 
  (let* ((v-component (lambda (p v) (map (lambda (x) (* p x)) v)))
			(dir (v-component attr (normalise (map - there here))))
			(theta (* (- (* 2 (random-real)) 1) pi))
			(d (list (cos theta) (sin theta)))
			(D (v-component (- 1.0 attr) (map + dir d)))
			;;(radius (* spd (sqrt (/ (* 2 N) 2)) (gamma 3/2)))
			(radius (min (* sp N) (power (random-real) -1/2)))
			)
	 (map (lambda (x) (* radius x)) (map + D dir))))
		  
(define (remainder* f g)
  (remainder (inexact->exact (round f))
				 (inexact->exact (round g))))

;; treats the year as homogeneous
(define (ok-to-reproduce t age min-age start duration probability)
  (let ((y (remainder t year)))
	 (and (> age min-age)
			(>= y start)
			(<= y (+ start duration))
			(< (random-real) probability))))


;; (define trials (map (lambda (x) (stoch-walk 12 2 0.5 '(1 0) 2)) (seq 2000)))
;; (define trialdat (map vlen trials))
;; (apply min trialdat)
;; (apply max trialdat)
;; (/ (apply + trialdat) (length trialdat))
;; (histograph trialdat "stuff")


;-  The End 


;; Local Variables: 
;; comment-end: "" ;;
;; comment-start: ";; " ;;
;; mode: scheme ;;
;; outline-regexp: ";-+" ;;
;; comment-column: 0 ;;
;; End:


