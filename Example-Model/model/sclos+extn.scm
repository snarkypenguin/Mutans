; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	sclos+extn.scm -- Written by Randall Gray

;-  Variables/constants both public and static

(define <uninitialized> '<uninitialized>) ;; this ought to be an eigensymbol
(define <nameless> '<nameless>) ;; this ought to be an eigensymbol

;-  Code 

(include "framework")
;; sclos.scm is included after the definition of (abstract-register ...) and a few 
;; instances of registers.


;-- Define abstract-register ... routine to create registers
;; Registers to associate  classes, methods and objects with their name.
(define (abstract-register thingtype thingname . unique-names)
  (letrec ((register '())
  			  (bind-string-to-closure (lambda x x))
			  )
	 (lambda args
		(if (null? args)
			 (list-copy register)
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
				  (list-copy register)
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
								(acons (car opts) (cdr opts) register)))
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

;-- define class-register generic-method-register method-register object-register and agent-register

;; classes ought to be unique
(define class-register (abstract-register "class" "class-name" #t))

;; We can (must) have many methods of the same name, like "dump"
(define generic-method-register (abstract-register "generic-method" "generic-method-name" #t))
(define method-register (abstract-register "method" "method-name"))
(define object-register (abstract-register "object" "object-name"))
(define agent-register (abstract-register "agent" "agent-name"))

;; make missing classes

(define <list> (make-class (list <pair>) '()))
(define <integer> (make-class (list <number>)'()))
(define <rational> (make-class (list <number>) '()))
(define <real> (make-class (list <number>) '()))
(define <complex> (make-class (list <number>) '()))

;-- include a bunch of things in the class register

;; Finally register sclos classes and the basic extensions
(register-unique class <pair>)
(register-unique class <list>);
(register-unique class <null>)
(register-unique class <boolean>)
(register-unique class <integer>);
(register-unique class <rational>);
(register-unique class <real>);
(register-unique class <complex>);
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


;-- Begin defining the fundamental classes for entities in the models

;--- objects 

"<primitive-object> is a (the?) basic class for SCLOS -- the name was
changed so we could use <object> as the basic entity in the framework.
An <object> knows very little about the modelling framework, and has no
implicit connections to any of the other model classes; thus, it has
no inherent representation of time or space, nor of inter-entity
communication (without cheating).

(define-class ...)  should *only* be used to define classes derived 
from object.
"

(define-class <object>
  (inherits-from <primitive-object>)
  (state-variables type note map-projection initialized)
  ;; 'note is just explanatory data
  ;; 'map-projection exists to assist projecting data in and out of the object's data-space
  )


;--- agents

(define-class <agent>
  (inherits-from <object>)
  (state-variables taxon
						 name ;; Unique id ideally
						 type ;; categorical classifier, typically the submodel
						 representation ;; in case a class may actually implement more than one repr.
						 agent-state
						 kernel
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



;-- Routines to interrogate or identify entities

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

;--- (define (has-slot? a k)
(define (has-slot? a k) 
  (member k (class-slots-of a)))

(define (class? a)
  (has-slot a 'cpl))

;--- (define (primitive-object? a)
(define (primitive-object? a)
  (and (%instance? a) #t))

;--- (define (instance? a)
(define (instance? a)
  (and (%instance? a) #t))

;--- (define (object? a)
(define (object? a)
  (and (%instance? a) (isa? a <object>) #t))

;--- (define (agent? a)
(define (agent? a)
  (and (%instance? a) (isa? a <agent>) #t))


;--- (define (classes-of-supers x) ;; returns classes
(define (classes-of-supers x) ;; takes objects... or classes
  (if (object? x)
		(class-cpl (class-of x))
		(class-cpl x)) 
  )

;--- (define (class-names-of-supers x) ;; returns strings
(define (class-names-of-supers x) ;; takes objects... or classes
  (map class-name-of (classes-of-supers x) ))

;--- (define (slot-values a)
(define (slot-values a)
  (map (lambda (x) (cons x (slot-ref a x))) (class-slots-of a)))


;--- (define (uninitialized? x #!rest y)
(define (uninitialized? x #!rest y)
  (if (null? y)
		(or (equal? x '<uninitialized>) (eqv? x '<uninitialized>))
		(uninitialized? (slot-ref x (car y)))))

;--- (define (nameless? x #!rest y)
(define (nameless? x #!rest y)
  (if (null? y)
		(or (equal? x '<nameless>) (eqv? x '<nameless>))
		(nameless? (slot-ref x (car y)))))


;-- Accessors, predicates 
;; These need to preceed framework-classes.
(add-method initialize (make-method (list <object>)
												(lambda (call-next-method self #!rest initargs)
												  
												  (if (and (pair? initargs) (null? (cdr initargs)))
														(set! initargs (car initargs))
														)
												  (set-state-variables self initargs)
												  ;(initialise self initargs)
												  ;(apply initialise (cons self initargs))
												  self) ))

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

;-- Method dispatch and "multiclass" type arguments. Particularly pertinent for parent calls

"
 canonical order of arguments:
 direction selection class-restriction method self #!rest arguments

 direction: class->baseclass | baseclass->class
 selection: get-methods
 class-restriction: either a non-class entity (conventionally *), a class (like <agent>) or a list of classes


 BY CONVENTION we will use the symbol '* to indicate that we want all methods... this might cause problems if
 someone declares '* to be something other than multiplication.  Or, we will use a number to indicate the list-head
 we are interested in.  Using #f is equivalent to a list-head of 0, and #t indicates only the first entry.


 This routine calls all applicable methods appropriate for parent classes, in contrast to the
 'method'-parent hook that is passed in as the first arg in a method.


 typical invocations might be
   (get-methods '* adjust-status - this-agent current-environment current-prey)
 or
   (get-methods (list <dolphin> <porpoise> <basic-animal>) adjust-status this-agent current-environment current-prey)

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
  (dnl* "gm: " class-restriction)
  (set! class-restriction (if (procedure? class-restriction) (list class-restriction) class-restriction))
  (sortless-unique
	(let ((mine ((compute-methods methd) (cons self args))))
	  ;;(dnl* "Got mine" mine (if (has-slot? self 'note) (slot-ref self 'note) "''"))
	  "Note, (compute-method class generic-method  arg1 ...) returns a list of 
      candidate methods in the order of most matched to least matched (least 
      generic to most generic)."
	  (cond
		((list? class-restriction)
		 (dnl "get-methods -- " class-restriction)
		 (apply append (map (lambda (x) (apply (compute-methods x) (cons self args))) class-restriction))
		 )
		((primitive-object? class-restriction)
		 (dnl "get-methods -- " class-restriction)
		 (let ((theirs (apply (compute-methods methd) (cons (allocate-instance class-restriction) args))))
			(filter (lambda (x) (memq x theirs)) mine)))
		(#t mine)))))

;--- (apply-method methd obj #!rest args) -- Applies methd to obj with appropriate arguments
(define (apply-method methd obj #!rest args)
  ;;((method-procedure (cadr rm)) (lambda x x) Rob)
  (if methd
		(apply (method-procedure methd) (cons (lambda x x) (cons obj args)))))


;--- (define (old-call-initialisers self #!rest args) --- this is explicit since it is so common.
(define (old-call-initialisers direction self #!rest args)
  (dnl* "old-call-initialisers" self args)
  (let* (;(ml (direction ((compute-methods initialise) (cons self args))))
			(ml (apply get-methods (cons direction (cons '* (cons initialise (cons self args))))))
			)
	 (map (lambda (m)
			  (apply m (cons (lambda x x) (cons self (list args)))))
			(reverse (map method-procedure ml)) ;; We want to initialise from the most general to the most specific.
			)))

;; ;--- (define (call-initialisers self classes #!rest args) --- this is explicit since it is so common.
;; (define (call-initialisers direction self classes)
;;   (let ((n (if (pair? classes) (length classes) +inf.0)))
;; 	 (set! classes (cond
;; 						 ((null? classes) (set! n 0) '())
;; 						 ((or (eq? (car classes) '*) (eq? (car classes) '*)) '*)
;; 						 ((number? classes) (set! n (absolute-value classes)) '*)
;; 						 (#t classes)))
;; 	 (cond
;; 	  ((zero? n) #t)
;; 	  ((or (eq? classes '*) (= n +inf.0)) (apply call-parent-methods (cons direction (cons get-methods (cons '* (cons initialise (cons self '())))))))
;; 	  (#t (apply call-parent-methods (list-head (cons direction (cons get-methods (cons classes (cons initialise (cons self '()))))) n)))
;; 	 )
;;   ;;(dnl* "\nCalling all initialisers .... Calling all initialisers ....")
;;   )
;

;--- (define (call-parent-methods selector classes methd self #!rest args)
(define (call-parent-methods direction selector classes methd self #!rest args)
  ;;(dnl* "cpm: " (or (method-register 'rec? methd) (generic-method-register 'rec? methd)) " " classes)

  (let ((n (if (pair? classes) (length classes) +inf.0)))
	 (set! classes (cond
						 ((null? classes) (set! n 0) '())
						 ((equal? classes 'none) #f)
						 ((equal? classes 'all) classes)
						 ((or (eq? (car classes) '*) (eq? (car classes) '*)) (set! n #f) '*)
						 ((number? classes) (set! n (absolute-value classes)) '*)
						 (#t classes)))
	 (if classes
		  (let* ((ml (direction (apply selector (cons classes (cons methd (cons self args))))))
					;;(brainfart (dnl* "... cpm got " ml))
					(result (map (lambda (x)
										(if (or (generic-method-register 'rec? x) (method-register 'rec? x) )
											 (begin 
												;;(dnl  "applying method " (or (generic-method-register 'rec? x) (method-register 'rec? x) ))
												(apply apply-method (cons x (cons self args))))))
									 (if n (list-head ml n) ml)
									 )))
			 result)
		  #t
		  ) ))

(define (call-parents& methd classes self #!rest args)
  (apply call-parent-methods (append (list base-class->class get-methods classes methd self) args)))

(define (call-initialisers classes self #!rest args)
  (apply call-parent-methods (append (list baseclass->class get-methods classes initialise self) args)))

(define (reverse-call-initialisers classes self #!rest args)
  (apply call-parent-methods (append (list class->baseclass get-methods classes initialise self) args)))



;--- (define (call-first-method selector classes methd self #!rest args) -- the most/least restrictive match
(define (call-first-method direction selector classes methd self #!rest args) ;; The following routine is very similar to the "meth"-parent
  (let* ((ml (direction (apply selector (cons classes (cons methd (cons self args))))))
			)
	 (apply apply-method (cons (car ml) (cons self args)))))


;-- Instantiating and initialising

;--- a few supporting variables.  

;; Uninitialisable classes/instances

(define *uninitializable*
  (list <top> <primitive-object> <class> <pair> <null> <boolean>
		  <symbol> <procedure> <number> <vector> <char> <string> <input-port>
		  <output-port> <procedure-class> <entity-class> <generic> <method>))
										
(define global-parameter-alist '())
(define **entity-index**
  (let* ((i 1)
			(L (lambda x
				  (let ((n i))
					 (set! i (+ i 1))
					 n)))
			)
	 L)) ;;... and no take-backs!

(define (serial-number it)
  (let ((name (cond
					((string? it) it)
					((symbol? it) (symbol->string it))
					((number? it) (number->string it))
					((class? it) (class-name-of it))
				  ((instance? it) (class-name-of (class-of it)))
					(#t (object->string it)))))
	 (if (equal? name (void))
		  (set! name (object->string it)))
	 (string-append name ":" (number->string (**entity-index**)))))



;--- (define (set-state-variables self arguments)
(define (set-state-variables self arguments)
  (if (and (pair? arguments) (even? (length arguments)))
		(let* ((slotnames (class-slots-of self))
				 (tags (evens arguments))
				 (vals (odds arguments))
				 )
		  (for-each (lambda (x y)
						  (if (memv x slotnames) ;; member using eqv, y'know
								(slot-set! self x y)
								))
						tags vals)))
  )

(define (string-tail s n)
  (list->string (reverse (list-head (reverse (string->list s)) n))))

(define (initialize-flag s)
  (if (string? s)
		(if (string=? (string-tail s (string-length "initialized")) "initialized")
			 #f
			 <uninitialized>)
		(initialize-flag (object->string s))))
		

;--- (define (make-object class #!rest initargs)
(define (make-object class #!rest initargs)
  ;;(dnl "**** entering make-object ****")
  (let ((instance (if #f
							 (allocate-instance class)
							 (apply make (cons class initargs)))))
	 (for-each (lambda (x) (slot-set! instance x
												 (initialize-flag x)
												 )) (class-slots-of instance))
	 (slot-set! instance 'type (class-name class)) ;;; This may be replaced by the set-state-variables call and the
	                                               ;;; initialise call
 
	 (if (pair? initargs) (set-state-variables instance initargs))
	 (object-register 'add instance class)

	 instance))


;--- (define (make-agent class . initargs)

(define (make-agent class #!rest initargs)
  (let ((instance (apply make-object (cons class initargs))))
	 (agent-register 'add instance class)
	 (if (or (eqv? (slot-ref instance 'name) <uninitialized>)
				(eqv? (slot-ref instance 'name) <nameless>))
		  (slot-set! instance 'name (serial-number class))) ;; may be overridden/overwritten
	 instance))

;; (define (check-param-sig filename)
;;   (let ((not-tilde (not (char=? #\~ (car (reverse (string->list filename))))))
;; 		  (isparameters (equal? 'Parameters (with-input-from-file filename (lambda () (read))))))
;; 	 (dnl "not-tilde " not-tilde ", Parameters " isparameters)

;;   (and not-tilde isparameters)))

;--- (iflag clss)  constructs the flag indicating a class has had its default initialisation
(define (iflag clss)
  (cond
	((string? clss) (string->symbol (string-append clss "-initialized")))
	((symbol? clss) (iflag (symbol->string clss)))
	((class? clss) (iflag (class-name-of clss)))
	((instance? clss) (iflag (class-of clss)))
	(#t #f)))
  
;--- (apply-initialisation instance key #!rest verbose)  loads parameters from the global-parameter list
"Arguably, this whole initialisation thing might be considered part of the 'model' and sit 
in one of the framework classes.  I have put  it here since object initialisation is really
a pretty fundamental part of an object oriented approach to anything, and the initialisation
of entities within the model isn't really an issue w.r.t. the model at all."

(define (apply-initialisation instance key #!rest verbose)
  (dnl* "***** apply initialisation for " (class-name-of (class-of instance)) key "****")

  (let ((p (assoc key global-parameter-alist))
		  (flag (iflag key))
		  )
	 (if p
		  (begin
			 (for-each
			  (lambda (v) ;; a spec with only one element is implicitly converted to an atom
				 (dnl "--- processing " v " ---")
				 (let ((np (if (and (pair? (cdr v)) (null? (cddr v)))	(cadr v)	(cdr v))))
					(slot-set! instance
								  (car v)
								  (if (and (pair? np) (or (equal? (car np) '@) (eqv? (car np) '@)))
										(let () (dnl "EVAL: " (cdr np)) (apply eval (cdr np)))
										np))))
			  (cdr p))
			 (if (and flag (has-slot? instance flag))
				  (slot-set! instance flag #t))
			 ))
	 )
  )
  

;; Both create and create- make <objects> and things derived from <object>
;; This version does not apply a taxon specific initialisation
(define (create- class #!rest statevars)
  (let* ((instance (allocate-instance  class))
			;(cpl (class-cpl cls))
			(the-classes (classes-of-supers class))
			)
	 (object-register 'add instance class)

	 ;; Set *all* slots to uninitialised
	 (for-each
	  (lambda (x)
		 (slot-set! instance x (initialize-flag x)))
	  (class-slots-of instance))

	 ;; First load the states of the classes from base->most-refined
	 (for-each
	  (lambda (x)
		 (if (not (member x *uninitializable*))
			  (begin
				 (apply-initialisation instance x)
				 (slot-set! instance (iflag x) #t))
			  )
		 )
	  (reverse the-classes)) ;; run from most general to most specific
  instance
  ))



(define (create class taxon #!rest statevars)
  (let* ((instance (apply create- (cons class statevars)))
			)
	 (agent-register 'add instance class)
	 (if (has-slot? instance 'taxon) (slot-set! instance 'taxon taxon))
	 
	 (apply-initialisation instance taxon #t)
	 (slot-set! instance 'initialized #t)

	 (dumpslots instance)
	 
	 (if (or (eqv? (slot-ref instance 'name) <uninitialized>)
				(eqv? (slot-ref instance 'name) <nameless>))
		  (slot-set! instance 'name (serial-number class))) ;; may be overridden/overwritten

	 instance
	 )
  )

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
