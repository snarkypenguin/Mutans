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

;-  Code 


(define (APPLY f lst)
  (let ((N 8192))
	 (if (< (length lst) N)
		  (apply f lst)
		  (let ((nlst (cons (apply f (list-head lst N)) (list (APPLY f (list-tail lst N))))))
			 (APPLY  f nlst)))))

;; this records all the calls defined with define%, model-method% or model-body%
(define %%%-time-register-%%% '()) 

(define time-field-width 8)
(define ps-default-margin (* 10 mm))

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
	(#t (cons (car v) (make-list (- k 1) (car v))))))

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
  (let ((f->m (get-local->model from ))
		  (m->t (get-model->local to)))
	 (if (or (not (procedure? f->m)) (not (procedure? m->t)))
		  (error "bad projection chain"))
	 (m->t (f->m point))))

(define (construct-symbol sym #!rest tagels)
  (if (string? sym) (set! sym (string->symbol sym)))
  (set! tagels (map (lambda (x) (if (string? x) (string->symbol x) x)) tagels))
  	
  (string->symbol
	(apply string-append
			 (map object->string
					(cons sym
							(let loop ((l '()) (t tagels))
							(if (null? t)
								 (reverse l)
								 (loop (cons (car t)
												 (cons '- l))
										 (cdr t)))))))))


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
		  (if (uninitialised? (slot-ref obj slt)) (slot-set! obj slt value))
		  (if (not (equal? (slot-ref obj slt) value)) (error "Bad initialisation in set-uninitialised-slots" slt value ))
		  )
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
				(display  "The following slots need to be set!\n")
				(pp lst)
				(abort)))
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
	(#t (error "bad papersize" s))))
	  
(define (m->ps xy)
  (rescale 2834.64646464646464646 x))

(define (ps->m x)
  (rescale (/ 1.0 2834.64646464646464646) x))

(define (km->ps xy)
  (rescale 2834646.46464646464646 x))

(define (ps->km x)
  (rescale (/ 1.0 2834646.46464646464646) x))


(define *default-projections*
  ;; The projections must take whole vectors since we cannot (properly) map
  ;; geographic ordinates otherwise.  mapf converts an ordinate-wise mapping
  ;; to a vector function.
  (list
	(cons 'I  (lambda (x) x)) ;; The identity mapping
	(cons 'km->ps (mapf km->ps)) ;; defined above
	(cons 'ps->km (mapf ps->km)) ;; defined above
	(cons 'm->ps (mapf m->ps)) ;; defined above
	(cons 'ps->m (mapf ps->m)) ;; defined above
	(cons 'mm->ps (mapf mm->points)) ;; defined in postscript.scm
	(cons 'ps->mm (mapf points->mm)) ;; defined in postscript.scm
	(cons 'm->mm (mapf (lambda (x) (* x 1000))))
	(cons 'mm->m (mapf (lambda (x) (/ x 1000))))
	(cons 'mm->points (mapf mm->points)) ;; defined in postscript.scm
	(cons 'points->mm (mapf points->mm)) ;; defined in postscript.scm
	(cons 'inches->points (mapf inches->points)) ;; defined in postscript.scm
	(cons 'points->inches (mapf points->inches)) ;; defined in postscript.scm
	(cons 'mm->inches (mapf mm->inches)) ;; defined in postscript.scm
	(cons 'inches->mm (mapf inches->mm)) ;; defined in postscript.scm
	))

  ;;; might be useful sometime
  ;;; (let* ((map:ll-ur->m (mapf (lambda (x) (* units-in-m x))))
  ;;; 			(mll (map:ll-ur->m ll))
  ;;; 			(mur (map:ll-ur->m ur))
  ;;; 			)
(define Model-Domain '<uninitialised>) 
(define default-margins 10) ;; implicitly in mm.

(define (set-model-domain ll ur #!rest pagesize) ;; 10mm margins all around
  (set! Model-Domain (list ll ur))
  (add-ps-projection
	'model->ps
	'ps->model
	(list ll ur)
	(if (null? pagesize) isoA4 (car pagesize))
	default-margins)
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
;; This mapping fits the o-domain by contraction
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


(define (linear-map domain codomain)
  ;; This implicitly clips to the shallowest domain
    ;; (p-d0)*(D1-D0)/(d1-d0) + D0
  (let* ((N (apply min (map length (append domain codomain))))
			(C (lambda (x) (list-head x N)))
			(d0 (C (car domain)))
			(d1 (C (cadr domain)))
			(D0 (C (car codomain)))
			(D1 (C (cadr codomain)))
			(scale (make-list N (apply min (map abs (map / (map - D1 D0) (map - d1 d0))))))
			(invscale (map reciprocal scale))
			)
	 (letrec ((f	(lambda (p)
						  (cond
							((pair? p) (map + (map * (map - p d0) scale) D0))
							((eq? p 'inverse)
							 (lambda (q)
								(cond
								 ((pair? q)	(map + (map / (map - q D0) scale) d0))
								 ((eq? q 'scale) (map reciprocal scale))
								 ((eq? q 'domain) codomain)
								 ((eq? q 'codomain) domain)
								 ((eq? q 'inverse) f)
								 (#t (error "Bad argument to linear map, try a point, 'inverse 'scale, 'domain or 'codomain" p)))))))))
		f)))







;-- makes a linear mapping function, each of the args is a rectangular bounding box (ll ur)
;; This mapping fits the o-domain by contraction
(define (linear2d:model-space->output-space domain codomain)
  (let* ((c2 (lambda (x) (list-head x 2)))
			(domain (c2 domain))
			(codomain (c2 codomain)))
	 (linear-map domain codomain)))

;-- makes a bilinear mapping function, each of the args is a rectangular bounding box (ll ur)
;; This mapping fits the o-domain by using different scales for the axes
(define (bilinear2d:model-space->output-space domain codomain)
  (let* ((c2 (lambda (x) (list-head x 2)))
			(domain (c2 domain))
			(codomain (c2 codomain)))
	 (n-linear-map domain codomain)))

(define (map:domain-to-postscript model-domain pagesize #!optional margin #!rest use-bilinear-map)
  ;; Defaults to a regular scale.
  ;; margin must come in as a measure in mm, if margin is a pair, the car is side
  ;; margins and the cadr is the top and bottom length

  (if (not margin) (set! margin ps-default-margin))

  (set! use-bilinear-map (if (null? use-bilinear-map) #f (car use-bilinear-map)))

  (set! margin ;; convert margin into points
		  ((mapf mm->points)
				  (list-head (cond
					((number? margin) (list margin margin))
					((and (pair? margin) (< (length margin) 2)) (list (car margin) (car margin)))
					(#t margin)) 2)))
  
  ((if use-bilinear-map
		 bilinear2d:model-space->output-space
		 linear2d:model-space->output-space)
	(list-head model-domain 2)
	(list margin (map - ((mapf mm->points) (list-head pagesize 2)) margin)) ;; expects page sizes in mm
	))

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


;; --- The following is deprecated, mainly because it was a complex solution to a simple problem.

;; (define submodel-register '())

;; (define (register-submodel tag . filelist)
;;   (if (assoc tag submodel-register)
;; 		(set-cdr! (assoc tag submodel-register)
;; 					 (append (assoc tag submodel-register) filelist))
;; 		(set! submodel-register
;; 				(cons (cons tag filelist) submodel-register)))
;;   )

;; (define (load-submodels)
;;   (let ((L load))
;; 	 "The following code takes the list of registered submodels and loads any files they may be 
;; dependent on.  Loggers must be loaded after the other submodels, so we take two passes."

;; 	 (let ((submodel-files
;; 			  (!filter null? (map	cdr (!filter null? (!filter (lambda (x) (member (car x) logger-tags)) submodel-register))))
;; 			  ))

;; 		(if (pair? submodel-files)
;; 			 (begin 
;; 				;;(dnl "Submodels: " submodel-files)
;; 				(for-each (if #t
;; 								  L 
;; 								  (lambda (x)
;; 									 (display "loading submodel: ")
;; 									 (display x)
;; 									 (newline)
;; 									 (L x))
;; 								  )
;; 							 submodel-files))
;; 			 (dnl "No submodel files to be loaded"))


;; ;\;; loggers get inserted at the head of the queue

;; 		(let ((logger-files
;; 				 (!filter
;; 				  null?
;; 				  (map
;; 					cdr
;; 					(!filter
;; 					 null?
;; 					 (filter
;; 					  (lambda (x) (member (car x) logger-tags))
;; 					  submodel-register))))
;; 				 ))

;; 		  (if (pair? logger-files)
;; 				(begin 
;; 				  ;;(dnl "Loggers: " logger-files)
;; 				  (for-each (if #t
;; 									 L 
;; 									 (lambda (x)
;; 										;;(display "loading logger: ")
;; 										;;(display x)
;; 										;;(newline)
;; 										(L x))
;; 									 )
;; 								logger-files))
;; 				(dnl "No logger files to be loaded"))
;; 		  )
;; 		)

;; 	 )
;;   )

;-  The End 


;; Local Variables: 
;; comment-end: "" ;;
;; comment-start: ";; " ;;
;; mode: scheme ;;
;; outline-regexp: ";-+" ;;
;; comment-column: 0 ;;
;; End:


