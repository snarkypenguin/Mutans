; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	sclos-extn.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.11.26
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/sclos-extn.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2016 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(define-macro (register regname mname . val)
  (let ((txt
			(if (not (null? val))
				 `(,(string->symbol (string-append (symbol->string regname) "-register")) 'add ,mname ',mname ,@val)
				 `(,(string->symbol (string-append (symbol->string regname) "-register")) 'add ,mname ',mname)) )
		  )
	 ;(display "register: ") (pp txt)  ;; Uncomment to print the constructed code during the startup phase 
	 txt))

(define-macro (register-unique regname mname . val)
  (let ((txt
			(if (not (null? val))
				 `(,(string->symbol (string-append (symbol->string regname) "-register")) 'add-unique ,mname ',mname ,@val)
				 `(,(string->symbol (string-append (symbol->string regname) "-register")) 'add-unique ,mname ',mname)) )
		  )    ;(display "register-unique: ") (pp txt) ;; Uncomment to print the constructed code during the startup phase 
	 txt))



(define-macro (define-class #!rest args)
  (let ((cname (car args))
		  (parents (cadr args))
		  (variables (append (caddr args) (list (string->symbol (string-append (symbol->string (car args)) "-initialised")))))
		  )
	 (let ((txt `(define ,cname
						(let ((,cname (make-class ,parents ,variables)))
						  (register-unique class ,cname ,cname)
						  (class-register)
						  ,cname))))
		;;(pp txt)
		txt
		)))

(define-macro (declare-method name descr . otherstuff)
  `(define ,name (register-unique generic-method (make-generic)))
  )

(define-macro (with-exit name . body)
  `(call-with-current-continuation
    (lambda (,name) ,@body)))

(define-macro (state-variables . lst)
  (let ((rslt `'(,@lst)))
	 ;;(pp rslt)
	 rslt))

;; This makes it clear when classes are defining NO state vars

(define-macro (inherits-from . iflst) `(list ,@iflst))

;; Support for the "isa?" calls and for a mapping from the class
;; object to a name

;; Remember, if you try to unquote classtype without splicing, the
;; interpreter tries to evaluate it.


;; cpl... class precedent list
(define-macro (isa? self . classtype)
  (letrec ((orf (lambda x (if (null? x #f) (or (car x) (apply orf (cdr x)))))))
	 (let ((txt `(let ((ancestors (class-cpl (class-of ,self))))
						(if (apply orf
									  (map (lambda (x) (member x ancestors))
											 (list ,@classtype))) 
							 #t #f)
						)
					))
		;;(pp txt) ;; Uncomment to print the constructed code during the startup phase 
		txt)))



(define (bind-string-to-closure cls)
  ;(dnl "quack")
  (let ((f (open-output-file "/tmp/trash")))
	 (write cls f)
	 (close-output-port f))
  ;(dnl "kcauq")
  )
	 
(define (abstract-register thingtype thingname . errata)
  (letrec ((register '()))
	 (lambda args
		(let ((help (lambda x
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
						  )))
		  (if (null? args)
				(copy-list register)
				(letrec ((cmd (car args))
							(opts (if (null? (cdr args)) #f (cdr args))))
				  (cond
					((not (symbol? cmd))
					 (help)
					 (abort "+++DIVISION BY DRIED FROG IN THE CARD CATALOG+++" cmd))
					((eqv? cmd 'help)
					 (help)
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

					((eqv? cmd 'add)
					 ;;				  (display "<<<+")
					 (bind-string-to-closure (car opts))
					 (set! register ;; save things as lists
							 (cons (cons (car opts) (cdr opts)) register))
					 ;;				  (display "+>>>\n")
					 (car opts)
					 )

					((eqv? cmd 'add-unique)
					 ;;				  (display "<<<-")
					 (bind-string-to-closure (car opts))
					 (if (not (assq (car opts) register))
						  (set! register ;; save things as lists
								  (cons (cons (car opts) (cdr opts)) register)))
					 ;;				  (display "->>>\n")
					 (car opts)
					 )

					((and (member cmd '(name? name)) opts)
					 (if (> (length opts) 1)
						  (map (lambda (x) (register cmd x)) opts)
						  (let ((a (assq (car opts) register)))
							 (and a (cadr a)))))

					((and (member cmd '(rec? record?)) opts)
					 (if (> (length opts) 1)
						  (map (lambda (x) (register cmd x)) opts)
						  (let ((a (filter (lambda (x) (or (eqv? (car x) (car opts))
																	  (string=? (object->string (car x))(object->string (car opts)))
																	  (string=? (object->string (cdr x)) (object->string (car opts)))
																	  )) register)))
							 (if (null? a) #f a)) ))

					((and (member cmd '(rec/type record-by-type rec-by-type rb-type)) opts)
					 (if (> (length opts) 1)
						  (map (lambda (x) (register cmd x)) opts)
						  (let ((a (assq (car opts) register)))
							 a)))

					((and (member cmd '(type? type)) opts)
					 (if (> (length opts) 1)
						  (map (lambda (x) (register cmd x)) opts)
						  (let ((a (filter (lambda (x)
													(eqv? (car opts) (cadr x)))
												 register)))
							 a)))

					((and (member cmd '(rec/type record-by-name rec-by-name rb-name)) opts)
					 (if (> (length opts) 1)
						  (map (lambda (x) (register cmd x)) opts)
						  (let ((a (filter (lambda (x)
													(eqv? (car opts) (cadr x)))
												 register)))
							 (and a (car a)))))

					(else
					 (dnl* "Called a " thingtype "/" thingname "register with " cmd )
					 
					 (pp (cdr args))
					 (display "... Didn't really work, was that a real command?\n")
					 (help)
					 (error "\n\n+++BANANA UNDERFLOW ERROR+++\n" args))
					)
				  )
				)
		  )
		)
	 )
  )




;; classes ought to be unique
(define class-register (abstract-register "class" "class-name" #t))

;; We can (must) have many methods of the same name, like "dump"
(define generic-method-register (abstract-register "generic-method" "generic-method-name" #t))
(define method-register (abstract-register "method" "method-name"))
(define object-register (abstract-register "object" "object-name"))
(define agent-register (abstract-register "agent" "agent-name"))

;; we will add a "list" primitive ... different from "pair"
;(define <pair>        (make-primitive-class))
(define <list>         (make-primitive-class)) 
(define <integer>      (make-primitive-class))
(define <rational>     (make-primitive-class))
(define <real>         (make-primitive-class))
(define <complex>      (make-primitive-class))

;(define <null>        (make-primitive-class))
;(define <symbol>      (make-primitive-class))
;(define <boolean>     (make-primitive-class))
;(define <procedure>   (make-primitive-class <procedure-class>))
;(define <number>      (make-primitive-class))
;(define <vector>      (make-primitive-class))
;(define <char>        (make-primitive-class))
;(define <string>      (make-primitive-class))
;(define  <input-port> (make-primitive-class))
;(define <output-port> (make-primitive-class))




;--- objects 

"<primitive-object> is a (the?) basic class for SCLOS -- the name was
changed so we could use <object> as the basic entity in the framework.
An <object> knows very little about the modelling framework, and has no
implicit connections to any of the other model classes; thus, it has
no inherent representation of time or space, nor of inter-entity
communication (without cheating)."

(define-class <object>
  (inherits-from <primitive-object>)
  (state-variables note map-projection)
  ;; 'note is just explanatory data
  ;; 'map-projection exists to assist projecting data in and out of the object's data-space
  )

;--- agents

(define-class <agent>
  (inherits-from <object>)
  (state-variables name type representation agent-state
						 note
						 kernel
						 need-all-parent-bodies
						 subjective-time priority jiggle 
						 dt
						 schedule
						 migration-test timestep-schedule counter
						 map-projection
						 state-flags
						 agent-epsilon
						 agent-schedule
						 dont-log
						 agent-body-ran

						 ;; a list of things the agent "provides"
						 provides 
						 ;; a list of things the agent "requires"
						 requires

						 ;; acting as a kernel for others
						 suspended-at
						 subsidiary-agents active-subsidiary-agents

						 ;;
						 maintenance-list
						 )
	)





;--- (define (class-slots-of x)
(define (class-slots-of x)
  (letrec ((orf (lambda y
					  (cond
						((null? y) #f)
						((car y) #t)
						(#t (apply orf (cdr y))))) ))
	 (cond
	  ((isa? x <primitive-object>) (map (lambda (x) (if (pair? x) (car x) x)) (class-slots (class-of x))))
	  ((isa? x <generic>) (map (lambda (x) (if (pair? x) (car x) x)) (append (class-slots (class-of x)) (class-slots x))))
	  ((isa? x <class>) (map (lambda (x) (if (pair? x) (car x) x)) (append (class-slots (class-of x)) (class-slots x))))
	  (#t '())
	  )
	 ))



(define general-class-of
  (let* ((primitive-class-of class-of)
			(co (lambda (x)
					(cond ;; these are carefully ordered!
					 ((list? x)        <list>)
					 ((integer? x)     <integer>) 
					 ((rational? x)    <rational>)
					 ((real? x)        <real>)
					 ((complex? x)     <complex>)
					 (#t (primitive-class-of x))))))
	 (set! class-of co)))
			



;--- (define (dumpslots ent)
(define (dumpslots ent)
  (let ((s (class-slots-of ent)))
	 (map (lambda (x)
			  (list x (slot-ref ent x)))
			s)))

(define (class-name-of x)
  (class-register 'name? x))

(define class-name class-name-of)


;; ;--- (define (class-name-of x) ;; returns symbols
;; (define (class-name-of x) ;; returns symbols
;;   (cond
;; 	((or (eq? x <class>) (eq? (class-of x) <class>)) '<class>)
;; 	((or (eq? x <top>) (eq? (class-of x) <top>)) '<top>)
;; 	((or (eq? x <generic>) (eq? (class-of x) <generic>)) '<generic>)
;; 	((or (eq? x <class>) (eq? (class-of x) <class>)) '<class>)
;; 	((or (eq? x <procedure-class>) (eq?  (class-of x) <procedure-class>)) '<procedure-class>)
;; 	((or (eq? x <entity-class>) (eq? (class-of x) <entity-class>)) '<entity-class>)
;; 	((or (eq? x <method>) (eq? (class-of x) <method>)) '<method>)
;; 	((or (eq? x <primitive-class>) (eq? (class-of x) <primitive-class>)) '<primitive-class>)
	
;; 	((isa? x <agent>) 
;; 	 (let ((n (class-register 'name? (class-of x))))
;; 		(and n
;; 			  ;;(string->symbol (string-append "instance-of-"
;; 			  ;;   (symbol->string n)))
;; 			  n
;; 			  )
;; 		))
;; 	((and (primitive-object? x) (assoc x (class-register))) 
;; 	 (let ((n (class-register 'name? x)))
;; 		(and n
;; 			  (string->symbol (string-append "class:" (object->string n)))
;; 			  )))
;; 	(else
;; 	 (let ((p (class-name-of (class-of x))))
;; 		(if p
;; 			 (string->symbol (string-append "instance:" (object->string p)))
;; 			 #f)))))

;--- (define (class-names-of-supers x)
(define (class-names-of-supers x)
  (map class-name-of (class-cpl (class-of x))))

;--- (define (primitive-object? a)
(define (primitive-object? a)
  (and (%instance? a) #t))

;--- (define (instance? a)
(define (instance? a)
  (and (%instance? a) #t))

;--- (define (agent? a)
(define (agent? a)
  (and (%instance? a) (isa? a <agent>) #t))

;--- (define (has-slot? a k)
(define (has-slot? a k) 
  (member k (class-slots-of a)))

;--- (define (slot-values a)
(define (slot-values a)
  (map (lambda (x) (cons x (slot-ref a x))) (class-slots-of a)))


;--- (define (uninitialised? x #!rest y)
(define (uninitialised? x #!rest y)
  (if (null? y)
		(eqv? x '<uninitialised>)
		(uninitialised? (slot-ref x (car y)))))
		


;-- Accessors, predicates 
;; These need to preceed framework-classes.
(add-method initialize (make-method (list <object>)
												(lambda (call-next-method object initargs)
												  ;;(dnl* "calling initialize for <object>")
												  
												  (initialise object initargs)
												  object) ))

(define (has-slot self slotname)
  (let ((slts (dumpslots self)))
	 (assq slotname slts)))

(define no-slot-in-object 'no-slot-in-object)

;;; (define slot-ref -;
;;;   (letrec ((slot-ref slot-ref)) -;
;;; 	 (lambda (self slotname) -;
;;; 		(if (has-slot? self slotname) -;
;;; 			 (slot-ref self slotname) -;
;;; 			 no-slot-in-object)))) -;

;;; (define slot-set! -;
;;;   (letrec ((slot-set! slot-set!)) -;
;;; 	 (lambda (self slotname val) -;
;;; 		(if (has-slot? self slotname) -;
;;; 			 (or (slot-set! self slotname val) #t) -;
;;; 			 #f)))) -;


;--- helpers/warts




;--- (define (look-for thing) -- Queries all the registers for the object passed
(define (look-for thing)
  (dnl* "object-register:" (object-register 'rec? thing))
  (dnl* "agent-register:" (agent-register 'rec? thing))
  (dnl* "method-register:" (method-register 'rec? thing))
  (dnl* "generic-method-register:" (generic-method-register 'rec? thing))
  (dnl* "class-register:" (class-register 'rec? thing))
  )

;-- Method dispatch and "multiclass" type arguments.

"
 canonical order of arguments:
 direction selection class-restriction method self #!rest arguments

 direction: class->baseclass | baseclass->class
 selection: get-methods
 class-restriction: either a non-class entity (conventionally *), a class (like <agent>) or a list of classes


 BY CONVENTION we will use the symbol * to indicate that we want all methods... this might cause problems if
 someone declares * to be something other than multiplication

 This routine calls all applicable methods appropriate for parent classes, in contrast to the
 'method'-parent hook that is passed in as the first arg in a method.


 typical invocations might be
   (get-methods '* adjust-status - this-agent current-environment current-prey)
 or
   (get-methods (list <dolphin> <porpoise> <basic-animal>) this-agent current-environment current-prey)

 The second form returns applicable methods common to both the class of this-agent and the <basic-animal> class

"

(define class->baseclass (lambda x x))
(define baseclass->class reverse)

;--- (sortless-unique lst)
(define (sortless-unique lst)  ;; This is so we can ensure that we don't call a method twice.
  (let loop ((r '())
				 (l lst))
	 (if (null? l)
		  (reverse r)
		  (if (memq (car l) r)
				(loop r (cdr l))
				(loop (cons (car l) r) (cdr l))))))

;--- (get-methods class-restriction methd self #!rest args)
(define (get-methods class-restriction methd self #!rest args) ;; if class-restriction is null return all methods, else restrict to list
  (sortless-unique
	(let ((mine ((compute-methods methd) (cons self args))))
	  (cond
		((list? class-restriction)
		 (apply append (map (lambda (x) (apply get-methods (cons x (cons methd (cons self args))))) class-restriction))
		 )
		((primitive-object? class-restriction)
		 (let ((theirs (apply compute-methods (cons methd (cons (allocate-instance class-restriction) args)))))
			(filter (lambda (x) (memq x theirs)) mine)))
		(#t mine)))))

;--- (apply-method methd obj #!rest args) -- Applies methd to obj with appropriate arguments
(define (apply-method methd obj #!rest args)
  ;;((method-procedure (cadr rm)) (lambda x x) Rob)
  (if methd
		(apply (method-procedure methd) (cons (lambda x x) (cons obj args)))))


;--- (define (old-call-all-initialisers self #!rest args) --- this is explicit since it is so common.
(define (old-call-all-initialisers direction self #!rest args)
  (dnl* "old-call-all-initialisers" self args)
  (let* (;(ml (direction ((compute-methods initialise) (cons self args))))
			(ml (apply get-methods (cons direction (cons * (cons initialise (cons self args))))))
			)
	 (map (lambda (m)
			  (apply m (cons (lambda x x) (cons self (list args)))))
			(reverse (map method-procedure ml)) ;; We want to initialise from the most general to the most specific.
			)))

;--- (define (call-all-initialisers self #!rest args) --- this is explicit since it is so common.
(define (call-all-initialisers direction self)
  ;;(dnl* "\nCalling all initialisers .... Calling all initialisers ....")
  (apply call-all-parent-methods (cons direction (cons get-methods (cons * (cons initialise (cons self '()))))))
  )


;--- (define (call-all-parent-methods selector class-restriction methd self #!rest args)
(define (call-all-parent-methods direction selector class-restriction methd self #!rest args)
  ;;(dnl* "DINK" (or (method-register 'rec? methd) (generic-method-register 'rec? methd)))
  (let* ((ml (direction (apply selector (cons class-restriction (cons methd (cons self args))))))
			(result (map (lambda (x)
								(if (or (generic-method-register 'rec? x) (method-register 'rec? x) )
									 (begin 
										;;(dnl  "applying method " (or (generic-method-register 'rec? x) (method-register 'rec? x) ))
										
										(apply apply-method (cons x (cons self args))))))
							 ml)))
	 ;;(dnl* "KNID" (or (method-register 'rec? methd) (generic-method-register 'rec? methd)))
	 result))

;--- (define (call-first-method selector class-restriction methd self #!rest args)
(define (call-first-method direction selector class-restriction methd self #!rest args) ;; The following routine is very similar to the "meth"-parent
  (let* ((ml (direction (apply selector (cons class-restriction (cons methd (cons self args))))))
			)
	 (apply apply-method (cons (car ml) (cons self args)))))


;-- Instantiating and initialising

;--- (define (set-state-variables self arguments)
(define (set-state-variables self arguments)
  (let ((automatically-load-type-file-params #t))
  ;; (if (list? arguments)
  ;; 		(begin
  ;; 		  (pp (evens arguments))
  ;; 		  (pp (odds arguments))
  ;; 		  ))
  (if (and (pair? arguments) (even? (length arguments)))
		(let* ((slotnames (class-slots-of self))
				 (tags (evens arguments))
				 (vals (odds arguments))
				 )
		  (for-each (lambda (x y)
						  (if (memv x slotnames) ;; member using eqv, y'know
								(slot-set! self x y)
								))
						tags vals))))
  )

;--- (define (make-object class #!rest initargs)
(define (make-object class #!rest initargs)
  ;;(dnl "**** entering make-object ****")
  (let ((instance (if #f
							 (allocate-instance class)
							 (apply make (cons class initargs)))))
	 ;;(for-each (lambda (x) (slot-set! instance x '<uninitialised>)) (class-slots-of instance))
	 ;;(if (pair? initargs)
	 ;;	  (set-state-variables instance initargs))

	 (if (pair? initargs) (set-state-variables instance initargs))
;?(call-all-initialisers baseclass->class instance)
	 (object-register 'add instance class)
	 ;;(dnl "**** leaving make-object ****")
	 instance))

;--- (define (make-agent class . initargs)

(define (make-agent class #!rest initargs)
  ;;(dnl "**** entering make-agent ****")
  (let ((instance (apply make (cons class initargs))))
	 (for-each (lambda (x) (slot-set! instance x '<uninitialised>)) (class-slots-of instance))
	 ;;(if (pair? initargs)
	 ;;	  (set-state-variables instance initargs))
	 ;;(call-all-initialisers baseclass->class instance initargs) ;; This ought to be called in the leaf-most class

	 (slot-set! instance 'type (class-name class)) ;;; This may be replaced by the set-state-variables call and the
	                                               ;;; initialise call
	 
	 (if (pair? initargs) (set-state-variables instance initargs))

	 (case 'use-apply
		((no-apply) (initialise instance initargs))
		((use-apply) (apply initialise (cons instance initargs)))
		)

;?(call-all-initialisers baseclass->class instance)
	 (agent-register 'add instance class)
	 (object-register 'add instance class)
	 ;;(dnl "**** leaving make-agent ****")
	 instance))


;; Finally register sclos classes and the basic extensions
(register-unique class <pair>)
(register-unique class <list>)
(register-unique class <null>)
(register-unique class <boolean>)
(register-unique class <integer>)
(register-unique class <rational>)
(register-unique class <real>)
(register-unique class <complex>)
(register-unique class <symbol>)
(register-unique class <procedure>)
(register-unique class <number>)
(register-unique class <vector>)
(register-unique class <char>)
(register-unique class <string>)
(register-unique class <input-port>)
(register-unique class <output-port>)
(register-unique class <class>)
(register-unique class <top>)
(register-unique class <primitive-object>)
(register-unique class <procedure-class>)
(register-unique class <entity-class>)
(register-unique class <generic>)
(register-unique class <method>)




;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
