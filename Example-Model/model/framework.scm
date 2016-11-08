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

(define definition-comments '())


(definition-comment 'load-model
  "load the configuration file")

(define (load-model)
  (load "model.config")
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
           (string->symbol (string-append "class:" (symbol->string n)))
           )))
   (else
	 (let ((p (class-name-of (class-of x))))
		(if p
			 (string->symbol (string-append "instance:" (symbol->string p)))
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


;; Registers to associate  classes, methods and objects with their name.


(define overdue-loans '())

(define (abstract-register thingtype thingname . unique-names)
  (letrec ((register '()))
    (lambda args
      (if (null? args)
          (copy-list register)
          (letrec ((cmd (car args))
						 (opts (if (null? (cdr args)) #f (cdr args))))

				(if (and #f opts) (dnl* "TOME:" unique-names cmd opts (assq (car opts) register)))

				(if (and  unique-names (eqv? cmd 'add) opts (assq (car opts) register))
					 (dnl* unique-names "Attempting to re-register a " thingtype "/" thingname ":" args)
					 )
            (cond
				 ((not (symbol? cmd))
				  (abort "+++DIVISION BY DRIED FROG IN THE CARD CATALOG+++" cmd))
				 ((eqv? cmd 'help)
				  (dnl* "'help")
				  (dnl* "passing no arguments or 'get returns a copy of the register")
				  (dnl* "'reg returns the register")
				  (dnl* "'flush or 'clear  sets the register to null")
				  (dnl* "'dump prints the register")
				  (dnl* "'add" thingtype (string-append thingtype "name") "description")
				  (dnl* "'add-unique" thingtype (string-append thingtype "name") "description")
				  (dnl* "'name?" thingtype "- not" thingname)
				  (dnl* "'type?" thingname)
				  (dnl* "'rec/name" thingtype)
				  (dnl* "'rec/type" thingtype)
				  (dnl* "'rec?" thingname thingtype "or the print string")
				  )

				 ((eqv? cmd 'reg) register)
             ((eqv? cmd 'get)
              (copy-list register)
              )

				 ((member cmd '(flush clear))
				  (set! register '()))


             ((eqv? cmd 'dump)
              (for-each pp register)
              )

             ((eqv? cmd 'add-unique)
				  (bind-string-to-closure (car opts))
				  (if (not (assq (car opts) register))
						(set! register ;; save things as lists
								(acons ((car opts) (cdr opts) register))))
				  (car opts)
				  )

             ((eqv? cmd 'add)
				  (bind-string-to-closure (car opts))
              (set! register ;; save things as lists
                    (acons (car opts) (cdr opts) register))
				  (car opts)
              )

             ((and (member cmd '(name? name)) opts)
              (let ((a (assq (car opts) register)))
                (and a (cadr a))))

             ((and (member cmd '(rec? record?)) opts)
              (let ((a (filter (lambda (x) (or (eqv? (car x) (car opts))
															  (string=? (object->string (car x))(object->string (car opts)))
															  (string=? (object->string (cdr x)) (object->string (car opts)))
															  )) register)))
					 (if (null? a) #f a)) )

             ((and (member cmd '(rec/type record-by-type rec-by-type rb-type)) opts)
              (let ((a (assq (car opts) register)))
                a))

             ((and (member cmd '(type? type)) opts)
              (let ((a (filter (lambda (x)
                                 (eqv? (car opts) (cadr x)))
                               register)))
                a
					 ))

             ((and (member cmd '(rec/type record-by-name rec-by-name rb-name)) opts)
              (let ((a (filter (lambda (x)
                                 (eqv? (car opts) (cadr x)))
                               register)))
                (and a (car a))))

             (else
				  (dnl* "Called a " thingtype "/" thingname "register with " cmd )
				  
				  (pp (cdr args))
				  (display "... Didn't really work, was that a real command?\n")
				  (error "\n\n+++BANANA UNDERFLOW ERROR+++\n" args))
				 )
				)
			 )
		)
	 )
  )


(define generic-method-register
  (abstract-register "generic-method" "generic-method-name" #t))

;; classes ought to be unique
(define class-register
  (abstract-register "class" "class-name" #t))

;; We can (must) have many methods of the same name, like "dump"
(define method-register
  (abstract-register "method" "method-name"))


(define object-register
  (abstract-register "object" "object-name"))

(define agent-register
  (abstract-register "agent" "agent-name"))

(define (uninitialised? x)
  (eqv? x '<uninitialised>))

;; Queries all the registers for the object passed

(define (look-for thing)
  (dnl* "class-register:" (class-register 'rec? thing))
  (dnl* "generic-method-register:" (generic-method-register 'rec? thing))
  (dnl* "method-register:" (method-register 'rec? thing))
  (dnl* "agent-register:" (agent-register 'rec? thing))
  (dnl* "object-register:" (object-register 'rec? thing))
)

(define (make-object class . initargs)
  (let ((instance (allocate-instance class)))
	 (for-each (lambda (x) (slot-set! instance x '<uninitialised>)) (class-slots-of instance))
	 (initialize instance initargs)
	 (object-register 'add instance class)
	 instance))

(define (make-agent class . initargs)
  (let ((instance (allocate-instance class)))
	 (for-each (lambda (x) (slot-set! instance x '<uninitialised>)) (class-slots-of instance))
	 (initialize instance initargs)
	 (agent-register 'add instance class)
	 (object-register 'add instance class)
	 instance))

(define (set-state-variables self arguments)
  (let ((snames (class-slots-of  self))
		  (args (if (and (pair? arguments) (pair? (car arguments)) (= 1 (length arguments))) (car arguments) arguments)))
	 (cond
	  ((or (equal? args '(())) (null? args)) #f)
	  ((odd? (length args)) (abort "Bad set of initialising arguments: " args))
	  ((>= (length args) 2)
		(if (member (car args) snames)
			 (slot-set! self (car args) (cadr args))
			 (dnl* "Ignoring parameter" (car args) (cadr args) "for" (class-name-of self)))
		)
	  )
	 (if (> (length args) 2) (set-state-variables self (cddr args)))))


(define (aborts . args) (error (apply string-append args)))



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
  "The following code takes the list of registered submodels and loads any files they may be 
dependent on.  Loggers must be loaded after the other submodels, so we take two passes."

  (let ((submodel-files
			(!filter null? (map	cdr (!filter null? (!filter (lambda (x) (member (car x) logger-tags)) submodel-register))))
			))

	 (if (pair? submodel-files)
		  (begin 
			 ;;(dnl "Submodels: " submodel-files)
			 (for-each (if #t
								load 
								(lambda (x)
								  ;;(display "loading submodel: ")
								  ;;(display x)
								  ;;(newline)
								  (load x))
								)
						  submodel-files))
		  (dnl "No submodel files to be loaded"))
	 )


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
								load 
								(lambda (x)
								  ;;(display "loading logger: ")
								  ;;(display x)
								  ;;(newline)
								  (load x))
								)
						  logger-files))
		  (dnl "No logger files to be loaded"))
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
