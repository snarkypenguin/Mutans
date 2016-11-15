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


;; This is a list of data added by a (definition-comment ...) clause associated with a
;; (define...)

(define adjust-grey #t)

(define definition-comments '())

(define slot-ref
  (letrec ((osr slot-ref))
	 (if #t
		  osr
		  (lambda (ob slot)
			 (begin
				(display "slot-ref: ")
				(display ob)
				(display " ")
				(display slot)
				(newline))
			 (osr ob slot)))))


(definition-comment 'load-model
  "load the configuration file")



;; subsidiary-agents are agents which may be embedded in a larger dynamic agent. Agents know 
;; what their parent agent is (if they have one) and may indicate to the parent that they should be
;; added to the active list. The parent agent is the one that actually decides if a agent is to move 
;; into the active queue or out of the active queue.  Whe things get moved, "value" from the parent is 
;; moved into the relevant sub-agents.  The set of ecoservices of the parent contains all of the types 
;; represented in its sub-agents.


;; This routine calls all applicable methods appropriate for parent classes, in contrast to the
;; "method"-parent hook that is passed in as the first arg in a method.

(define (call-all-parents methd self #!rest args)
  (let* ((ml (map method-procedure ((compute-methods methd) (cons self args)))))
	 (map (lambda (m)
			  (apply m (cons (lambda x x) (cons self args))))
			ml)))


;; The following routine is very similar to the "meth"-parent
(define (call-first-parent methd self #!rest args)
  (let* ((ml (map method-procedure ((compute-methods methd) (cons self args)))))
	 (map (lambda (m)
			  (apply m (cons (lambda x x) (cons self args))))
			(car ml)))) 


(define (load-model)
  (let ((l load)) ;; gets us past the make-bolus filtering
	 (l "model.config")
	 )
  )
;; These need to preceed framework-classes.

(define (class-slots-of x)
  (cond
	((isa? x <object>) (map (lambda (x) (if (pair? x) (car x) x)) (class-slots (class-of x))))
	((isa? x <generic>) (map (lambda (x) (if (pair? x) (car x) x)) (append (class-slots (class-of x)) (class-slots x))))
	((isa? x <class>) (map (lambda (x) (if (pair? x) (car x) x)) (append (class-slots (class-of x)) (class-slots x))))
	(#t '())
	)
  )  


(define (dumpslots ent)
  (let ((s (class-slots-of ent)))
	 (map (lambda (x)
			  (list x (slot-ref ent x)))
			s)))

(define (class-name-of x)
  (cond
	((isa? x <agent>) 
	 (let ((n (class-register 'name? (class-of x))))
		(and n
			  ;;(string->symbol (string-append "instance-of-"
			  ;;   (symbol->string n)))
			  n
			  )
		))
	((and (primitive-object? x) (assoc x (class-register))) 
	 (let ((n (class-register 'name? x)))
		(and n
			  (string->symbol (string-append "class:" (object->string n)))
			  )))
	(else
	 (let ((p (class-name-of (class-of x))))
		(if p
			 (string->symbol (string-append "instance:" (object->string p)))
			 #f)))))


(define (class-names-of-supers x)
  (map class-name-of (class-cpl (class-of x))))

(define (primitive-object? a)
  (and (%instance? a) #t))

(define (instance? a)
  (and (%instance? a) #t))

(define (agent? a)
  (and (%instance? a) (isa? a <agent>) #t))

(define (has-slot? a k) 
  (member k (class-slots-of a)))

(define (slot-values a)
  (map (lambda (x) (cons x (slot-ref a x))) (class-slots-of a)))


(define (uninitialised? x #!rest y)
  (if (null? y)
		(eqv? x '<uninitialised>)
		(uninitialised? (slot-ref x (car y)))))
		

;; Queries all the registers for the object passed

(define (look-for thing)
  (dnl* "class-register:" (class-register 'rec? thing))
  (dnl* "generic-method-register:" (generic-method-register 'rec? thing))
  (dnl* "method-register:" (method-register 'rec? thing))
  (dnl* "agent-register:" (agent-register 'rec? thing))
  (dnl* "object-register:" (object-register 'rec? thing))
  )

(define (set-state-variables self arguments)
  (let ((snames (class-slots-of  self))
		  (args (if (and (pair? arguments) (pair? (car arguments)) (= 1 (length arguments))) (car arguments) arguments)))
	 (cond
	  ((or (equal? args '(())) (null? args)) #f)
	  ((odd? (length args)) (abort "Bad set of initialising arguments: " args))
	  ((>= (length args) 2)
		(if (member (car args) snames)
			 (slot-set! self (car args) (cadr args))
			 (kdnl* '(init parameters) "Ignoring parameter" (car args) (cadr args) "for" (class-name-of self)))
		)
	  )
	 (if (> (length args) 2) (set-state-variables self (cddr args)))))

(define (make-object class #!rest initargs)
  (let ((instance (allocate-instance class)))
	 ;;(for-each (lambda (x) (slot-set! instance x '<uninitialised>)) (class-slots-of instance))
	 ;;(if (pair? initargs)
	 ;;	  (set-state-variables instance initargs))
	 (initialize instance initargs)
	 (object-register 'add instance class)
	 instance))

(define (make-agent class . initargs)
  (let ((instance (allocate-instance class)))
	 (for-each (lambda (x) (slot-set! instance x '<uninitialised>)) (class-slots-of instance))
	 ;;(if (pair? initargs)
	 ;;	  (set-state-variables instance initargs))
	 (initialize instance initargs)
	 (agent-register 'add instance class)
	 (object-register 'add instance class)
	 instance))




;-- Loggers ****

;--- list of tags for introspection and logging

(define logger-tags '())

(define submodel-register '())
(define (register-submodel tag . filelist)
  (if (assoc tag submodel-register)
		(set-cdr! (assoc tag submodel-register)
					 (append (assoc tag submodel-register) filelist))
		(set! submodel-register
				(cons (cons tag filelist) submodel-register)))
  )


(define (load-submodels)
  (let ((L load))
	 "The following code takes the list of registered submodels and loads any files they may be 
dependent on.  Loggers must be loaded after the other submodels, so we take two passes."

	 (let ((submodel-files
			  (!filter null? (map	cdr (!filter null? (!filter (lambda (x) (member (car x) logger-tags)) submodel-register))))
			  ))

		(if (pair? submodel-files)
			 (begin 
				;;(dnl "Submodels: " submodel-files)
				(for-each (if #t
								  L 
								  (lambda (x)
									 (display "loading submodel: ")
									 (display x)
									 (newline)
									 (L x))
								  )
							 submodel-files))
			 (dnl "No submodel files to be loaded"))


;;; loggers get inserted at the head of the queue

		(let ((logger-files
				 (!filter
				  null?
				  (map
					cdr
					(!filter
					 null?
					 (filter
					  (lambda (x) (member (car x) logger-tags))
					  submodel-register))))
				 ))

		  (if (pair? logger-files)
				(begin 
				  ;;(dnl "Loggers: " logger-files)
				  (for-each (if #t
									 L 
									 (lambda (x)
										;;(display "loading logger: ")
										;;(display x)
										;;(newline)
										(L x))
									 )
								logger-files))
				(dnl "No logger files to be loaded"))
		  )
		)

	 )
  )

;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
