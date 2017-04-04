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

(define support-dir "./")

(define (Note flag #!rest args)
  (void))


;; This is a list of data added by a (definition-comment ...) clause associated with a
;; (define...)


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

	 

(define (composite-projection submodel1 submodel2)
  (dnl* (cnc submodel1) (cnc submodel2))

  (let ((->ms (if (agent? submodel1)
						(slot-ref submodel1 'inv-local-projection)
						submodel1))
				  
		  (ms-> (if (agent? submodel2)
						(slot-ref submodel2 'local-projection)
						submodel2))
		  )
	 (lambda (x) 
		(ms-> (->ms x)))))


(define %%default-initialisations%% '())
(define (add-default-initialisation-for key lst)
  (set! %%default-initialisations%% (cons (cons key lst) %%default-initialisations%%)))

(define (initialisation-defaults-for key)
  (let ((l (filter (lambda (x) (eq? key (car x))) %%default-initialisations%%)))
	 (if (null? l)
		  l
		  (apply append (map cdr l)))))
		  

;-- makes a linear mapping function, each of the args is a rectangular bounding box (ll ur)
;; This mapping fits the o-domain by contraction
(define (linear2d:model-space->output-space m-domain o-domain)
  (let* ((Mo (map - (cadr m-domain) (car m-domain))) ;; 
			(Oo (map - (cadr o-domain) (car o-domain))) ;; 
			(Ms (list (apply max Mo) (apply max Mo)))
			(Os (list (apply min Oo) (apply min Oo)))
			(f (lambda (p)
				  (map exact->inexact
						 (map + (map * (map / (map - p (car m-domain)) Ms)
										 Os)
								(car o-domain))))))
	 f))

;-- makes a bilinear mapping function, each of the args is a rectangular bounding box (ll ur)
;; This mapping fits the o-domain by using different scales for the axes
(define (bilinear2d:model-space->output-space m-domain o-domain)
  (let* ((Mo (map - (cadr m-domain) (car m-domain))) ;; 
			(Oo (map - (cadr o-domain) (car o-domain))) ;; 
			(Ms (list (apply max Mo) (apply max Mo)))
			(Os (list (apply min Oo) (apply min Oo)))
			(f (lambda (p)
				  (map exact->inexact
						 (map + (map * (map / (map - p (car m-domain)) Mo)
										 Oo)
								(car o-domain))))))
	 f))



(define (map:domain-to-postscript model-domain margin #!rest use-bilinear-map)
  ;; Defaults to a regular scale.
  ;; margin will be in mm, if margin is a pair, the car is side
  ;; margins and the cadr is the top and bottom length
  (set! use-bilinear-map (if (null? use-bilinear-map) #f (car use-bilinear-map)))

  (let ((pagesize  pagesize)
		  (margin (if (pair? margin)
						  (map / margin '(25.4 25.4))
						  (list (/ margin 25.4)(/ margin 25.4)))))
  
	 ((if use-bilinear-map
		  bilinear2d:model-space->output-space
		  linear2d:model-space->output-space)
	  model-domain
	  (list margin (map - pagesize margin))
	  )))


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


