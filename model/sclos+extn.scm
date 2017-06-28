;-*- mode: scheme; -*-
;-  Identification and Changes

;--
;	sclos+extn.scm -- Written by Randall Gray 
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

(include "framework")
;; sclos.scm is included after the definition of (abstract-register ...) and a few 
;; instances of registers.

(define eval-marker '$)


(define (all-slotnames obj)
  (map car (class-slots (class-of obj))))

(define (all-slots obj)
  (let ((names (all-slotnames obj)))
	 (map (lambda (x) (list x (slot-ref obj x))) names)))


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

;; This comes here since we might want the registers available for sclos.scm
(include "sclos.scm")

(define-class <list>
  (inherits-from <pair>)
  (no-state-variables)
  )

(define-class <integer>
  (inherits-from <number>)
  (no-state-variables)
  )
(define-class <rational>
  (inherits-from <number>)
  (no-state-variables)
  )
(define-class <real>
  (inherits-from <number>)
  (no-state-variables)
  )
(define-class <complex>
  (inherits-from <number>)
  (no-state-variables)
  )

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


(define not-an-object (list <method> <generic> <entity-class> <procedure-class> <primitive-object> <top> <class>))

;-- Begin defining the fundamental classes for entities in the models

;--- objects 

"<primitive-object> is a (the?) basic class for SCLOS -- the name was
changed so we could use <object> as the basic entity in the framework.
An <object> knows very little about the modelling framework, and has no
implicit connections to any of the other model classes; thus, it has
no inherent representation of time or space, nor of inter-entity
communication (without cheating)."

(define-class <object>
  (inherits-from <primitive-object>)
  (state-variables note)
  ;; 'note is just explanatory data
  )

;--- agents

(define-class <agent>
  (inherits-from <object>) ;; type is used as a categorical value in kernel-calls
  (state-variables name taxon representation agent-state
						 note
						 kernel
						 subjective-time priority jiggle 
						 dt
						 migration-test timestep-schedule counter
						 state-flags
						 agent-epsilon
						 dont-log
						 always-log
						 agent-body-ran
						 no-model-body
						 ;; this needs to be true if the agent does
						 ;; not implement its own model-body

						 ;; a list of things the agent "provides"
						 provides 
						 ;; a list of things the agent "requires"
						 requires

						 ;; acting as a kernel for others
						 suspended-at
						 subsidiary-agents active-subsidiary-agents

						 ;;
						 maintenance-list
						 initialised
						 )
  )


(define (classes-of-supers x)
  (if (class? x)
		(slot-ref x 'direct-supers)
		(slot-ref (class-of x) 'direct-supers)))

;;-- This returns the parents of an instance or class


(define (parent-classes x)
  (let* ((flense (lambda (x) (!filter (lambda (c) (member c not-an-object)) x)))
			(c* (map flense (map class-cpl (classes-of-supers x))))
			)
	 (map cdr
			(sort 
			 (map (lambda (x) (cons (length (parent-classes x)) x))
					(sortless-unique (apply append c*)))
			 (lambda (x y)
				(>= (car x) (car y))))
			)))


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
  (let ((s (class-slots-of ent))
		  )
	 (if (member 'dont-log s)
		  (let* ((dont (slot-ref ent 'dont-log))
					(donot (if (list? dont) dont (list dont))))
			 (map (lambda (x)
					  (list x (slot-ref ent x)))
					(!filter (lambda (y) (member y donot)) s)))
		  (map (lambda (x)
					(list x (slot-ref ent x)))
				 s))))

(define (examine-instance entity)
  (for-each (lambda (x) (dnl (list (car x) (slot-ref entity (car x))))) (class-slots (class-of entity))))

(define (class-name-of q)
  (class-register 'name? q))

(define (class-name-of-instance q)
  (class-register 'name? (class-of q)))


(define class-name class-name-of)

;--- (define (class-names-of-supers x)
(define (class-names-of-supers x)
  (map class-name-of (class-cpl (class-of x))))

;--- (define (primitive-object? a)
(define (primitive-object? a)
  (and (%instance? a) #t))

;--- (define (instance? a)
(define (instance? a)
  (and (%instance? a) #t))


;;; Not really useful....
;; (define (standard-type? x #!rest lst)
;;   (set! lst (if (pair? lst) (car lst) lst))
;;   (let ((gambit-type-predicates
;; 			(list pair? symbol? number? string? vector? port? char? continuation?
;; 					null? readtable? boolean? symbol? box? procedure?
;; 					keyword? uninterned-symbol? uninterned-keyword? thread-group?
;; 					condition-variable? mutex? thread? table? will? random-source?
;; 					#f)
;; 			))

;;   (cond
;; 	((null? lst) (standard-type? x gambit-type-predicates))
;; 	((and (pair? lst) (null? (cdr lst))) ((car lst) x))
;; 	(#t
;; 	 (or ((car lst) x) (standard-type? x (cdr lst))))
;; 	)))

;--- (class? a) ... not perfect, but close enough
(define (class? a)
  (if (member a (list <top> <class> <procedure-class> <entity-class>))
		#t
		(and (equal? (class-slots-of a) '(direct-supers direct-slots cpl slots nfields field-initialisers getters-n-setters))))
  )


;--- (define (object? a)
(define (object? a)
  (and (%instance? a) (isa? a <object>) #t))

;--- (define (agent? a)
(define (agent? a)
  (and (%instance? a) (isa? a <agent>) #t))

;--- (define (has-slot? a k)
(define (has-slot? a k)
  (cond
	((object? a) (member k (class-slots-of a)))
	((class? a) (member k (map car (slot-ref a 'slots))))
	(else #f)
	)
  )

(define (parent-classes? x)
  (map class-name-of (parent-classes x)))

;--- (define (class-names-of-supers x) ;; returns strings
(define (class-names-of-supers x) ;; takes objects... or classes
  (map class-name-of (classes-of-supers x) ))

;--- (define (slot-values a)
(define (slot-values a)
  (map (lambda (x) (cons x (slot-ref a x))) (class-slots-of a)))


;--- (define (uninitialised? x #!rest y)
"There are three things used to indicate an uninitialised value: 
   * the function of arbitrary arguments, uninitialised
   * the symbol '<uninitialised>
   * the object <uninitialised>
"
(define (uninitialised? x #!rest y)
  (if (object? x) (uninitialised? (slot-ref x (car y)))
		(if (null? y)  (or (eq? x uninitialised) (eq? x '<uninitialised>)(eqv? x <uninitialised>))
			 (and (uninitialised? x)
					(apply uninitialised? y)))))

;--- (define (uninitialised? x #!rest y)
(define (uninitialised#? x #!rest y)
  (if (object? x) (uninitialised? (slot-ref x (car y)))
		(if (null? y)  (or (not x) (eq? x '<uninitialised>)(eqv? x <uninitialised>))
			 (and (uninitialised? x)
					(apply uninitialised? y)))))

;--- (define (nameless? x #!rest y)
(define (nameless? x #!rest y)
  (if (null? y)
		(or (eq? x '<nameless>) (eqv? x <nameless>))
		(nameless? (slot-ref x (car y)))))


;-- State variable routines, accessors, predicates

;;--- (defval sym value)
(define (defval sym value #!optional note)
  (if (symbol? sym)
		(list sym value)
		(abort "Bad defval arguments" sym value args)))

(define (2list? l)
  (if (null? l)
		#f
		(and (list? l) (pair? l) (symbol? (car l)) (pair? (cdr l)) (null? (cddr l)))))

(define (list-of-2list? L)
  (if (null? L)
		#f
		(apply andf (map 2list? L))))

;--- (define (set-state-variables self arguments)
"This requires a little explanation: the arguments can be specified as either a flat list of 
tags and values, or as a list of tag-value pairs.  This is because the flat list is easier on
the eyes and the other is easier for the code.  In practice, it is as easy to write code that 
handles mixed lists.  Whether that is a good idea is a matter of practicality versus taste.
"

(define (set-state-variables-2lists self arglist)
  (if (not (null? arglist))
		(let ((argl (car arglist)))
;			 (dnl* "2l !null" (cnc self) (car argl) (has-slot? self (car argl)))
		  (if (has-slot? self (car argl))
				(begin
;					 (dnl* "setting 2l"  (cnc self) argl)
				  (slot-set! self (car argl) (cadr argl))))
		  (cdr arglist))
		arglist))

(define (set-state-variables-flat-list self arglist)
  (if (not (null? arglist))
		(begin
;		  (dnl* "fl !null" (cnc self) (car arglist) (has-slot? self (car arglist)))
		  (if (has-slot? self (car arglist))
				(begin
;				  (dnl* "setting fl" (cnc self) (car arglist) (cadr arglist))
				  (slot-set! self (car arglist) (cadr arglist)))
;				(dnl* "no slot" (car arglist))
				)
;		  (dnl* "continuing after" (car arglist) "and" (cadr arglist) "with" (cddr arglist))
		  (cddr arglist))
		arglist))


(define (set-state-variables self arguments)
  (if (and (= (length arguments) 1)
			  (pair? (car arguments)))
		(set-state-variables self (car arguments))
		(cond
		 ((null? arguments)
		  (void))
		 ((2list? (car arguments))
		  (set-state-variables self (set-state-variables-2lists self arguments)))
		 ((and (symbol? (car arguments)) (pair? arguments) (pair? (cdr arguments)))
		  (set-state-variables self (set-state-variables-flat-list self arguments)))
		 (else (abort "Bad state variable list for" (cnc self) "#" (length arguments) "|" arguments ))))
  )

;; These need to act before framework-classes.
(add-method initialise (make-method (list <object>)
												(lambda (call-next-method self #!rest initargs)
												  
												  (if (and (pair? initargs) (null? (cdr initargs)))
														(set! initargs (car initargs))
														)
												  (set-state-variables self initargs)
;(initialise self initargs)
;(apply initialise (cons self initargs))
												  self) ))

(define (null=#f arg)
  (if (or (not arg) (null? arg))
		#f
		arg))

(define no-slot-in-object 'no-slot-in-object)

;; The *is- and *has- routines return predicate functions
(define (*is-class? targetclass #!rest plural)
  (set! targetclass (cons targetclass plural))
  ;; this odd way of specifying arguments  ensures at last one arg
  (let ((targets (filter class? targetclass)))
	 (lambda (x)
		(null=#f (if (eq? x 'inspect)
						 ;;(dnl* "targetclass =" targetclass ": plural =" plural ": targets =" targets)
						 (list targets)
						 (apply orf (map null=#f (map (lambda (target) (isa? x target)) targets)))))
		)))


"Examples might be
    (*is-class? <fish>)
    (*is-class? <fish> <mollusc> <amphipod> <monkey>)
"

(define (*has-slot? slot)
  (let ((slot (filter symbol? slot)))
	 (lambda (x) 
		(null=#f (if (list? slot)
						 (apply orf (map (lambda (y) (has-slot? x y)) slot))
						 (has-slot? x slot))))))


(define (*has-slot-value? slot v)
  (let ((slot slot)
		  (v v))
	 (cond
	  ((procedure? v) ;; v is a predicate function
		(lambda (x) (null=#f (if (not (has-slot? x slot))
										 #f
										 (v x)))))
	  ((list? v)
		(lambda (x) (null=#f (if (not (has-slot? x slot))
										 #f
										 (eqv? v x)))))
	  
	  (#t 	 (lambda (x) (null=#f (if (not (has-slot? x slot))
												  #f
												  (eq? v x))))))))
"Examples might be 
   (*has-slot-value 'age (lambda (age) (and (<= 7 age) (<= age 20))))
   (*has-slot-value 'reproductive-state '(adolescent adult post-breeding))
   (*has-slot-value 'water-stressed #t)

If the agent does not possess the slot, it cannot have the indicated property
and so it is excluded.
"


(define (*is-taxon? target #!rest s)
  (let* ((targets (filter string? (cons target s)))
			(cmpop string=?))
	 (lambda (x)
		(null=#f (if (null? targets)
						 #f
						 (if (list? targets)
							  (apply orf (map null=#f (map (lambda (y) (cmpop y (taxon x))) (filter string? targets))))
							  (cmpop targets (taxon x))))))))


(define (*is-taxon-ci? targets #!rest s)
  (let* ((targets (filter string? (cons target s)))
			(cmpop string-ci=?))
	 (lambda (x)
		(null=#f (if (null? targets)
						 #f
						 (if (list? targets)
							  (apply orf (map null=#f (map (lambda (y) (cmpop y (taxon x))) (filter string? targets))))
							  (cmpop targets (taxon x))))))))

(define (*is-taxon-wild? targets #!rest s)
  (let* ((targets (filter string? (cons target s)))
			(cmpop wildmatch))
	 (lambda (x)
		(null=#f (if (null? targets)
						 #f
						 (if (list? targets)
							  (apply orf (map null=#f (map (lambda (y) (cmpop y (taxon x))) (filter string? targets))))
							  (cmpop targets (taxon x))))))))


(define (*is-taxon-wild-ci? target #!rest s)
  (let* ((targets (filter string? (cons target s)))
			(cmpop wildmatch-ci))
	 (lambda (x)
		(null=#f (if (null? targets)
						 #f
						 (if (list? targets)
							  (apply orf (map null=#f (map (lambda (y) (cmpop y (taxon x))) (filter string? targets))))
							  (cmpop targets (taxon x))))))))

(define (*provides? target #!rest s)
  (let ((targets (filter symbol? (cons target s))))
	 (lambda (A)
		(null=#f (if (null? targets)
						 #f
						 (apply orf (map null=#f (map (lambda (s) (provides? A s)) targets))))))))


(define (*is-*? target #!rest s)
  (let ((targets (if (and (list? target)(null? s)) target (cons target s))))
	 (C (*is-class? targets))
	 (S (*has-slot? targets))
	 (T (*is-taxon? targets))
	 )
  (lambda (x)
	 (null=#f (or (C x) (S x) (T x)))))

(define (*provides-*? target #!rest s)
  (let ((targets (if (and (list? target)(null? s)) target (cons target s))))
	 (let ((C (apply *is-class? targets))
			 (P (apply *provides? targets))
			 (T (apply *is-taxon? targets))
			 )
		(lambda (x)
		  (null=#f (or (C x) (P x) (T x)))))))

(define (cnc a) (class-name-of (class-of a)))
(define (cncs a) (string->symbol (class-name-of (if (class? a) a (class-of a)))))
;; this is mostly used in the provides/requires code

(define (nm? a) (if (isa? a <agent>) (slot-ref a 'name) a))

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
 class-restriction method self #!rest arguments

 class-restriction: either a non-class entity (conventionally *), 
                    a class (like <agent>) or a list of classes


 BY CONVENTION we will use the symbol '* to indicate that we want all
 methods... (it will recognise the default multiplication operator as
 being equivalent, but this might cause problems if someone declares '*
 to be something other than multiplication -- best to use the symbol).
 Or, we will use a number to indicate the list-head we are interested
 in.  Using #f is equivalent to a list-head of 0, and #t indicates only
 the first entry.


 This routine calls all applicable methods appropriate for parent
 classes, in contrast to the parent-'method' hook that is passed in as
 the first arg in a method.


 typical invocations might be
   (get-methods '* adjust-status - this-agent 
                current-environment current-prey)
 or
   (get-methods (list <dolphin> <basic-animal> <tracker>) 
                adjust-status this-agent 
                current-environment current-prey)
... in the second case the class <dolphin> might be defined as 
(define-class <dolphin> (inherits-from <basic-animal> <sonar> <tracker>) (state-variables....)



  The second form returns applicable methods common to both the class
  of this-agent and its parents (but not *grandparents...).

  "

;--- (sortless-unique lst)
(define (sortless-unique lst)  ;; This is so we can ensure that we don't call a method twice.
  (let loop ((r '())
				 (l lst))
	 (if (null? l)
		  (reverse r)
		  (if (member (car l) r)
				(loop r (cdr l))
				(loop (cons (car l) r) (cdr l))))))

;--- (apply-method methd obj #!rest args) -- Applies methd to obj with appropriate arguments
(define (apply-method methd obj #!rest args)
  ;;((method-procedure (cadr rm)) (lambda x x) Rob)
  (if (and (pair? args) (pair? (car args)) (null? (cdr args))) (set! args (car args)))
  (if methd
		(apply (method-procedure methd) (cons (lambda x x) (cons obj args)))))


;; ;--- (define (old-call-all-initialisers self #!rest args) --- this is explicit since it is so common.
;; (define (old-call-all-initialisers direction self #!rest args)
;;   (dnl* "old-call-all-initialisers" self args)
;;   (let* (;(ml (direction ((compute-methods initialise) (cons self args))))
;;                         (ml (apply get-methods (cons direction (cons * (cons initialise (cons self args))))))
;;                         )
;;          (map (lambda (m)
;;                           (apply m (cons (lambda x x) (cons self (list args)))))
;;                         (reverse (map method-procedure ml)) ;; We want to initialise from the most general to the most specific.
;;                         )))



;--- (get-methods class-restriction methd self #!rest args(define (get-methods class-restriction methd self #!rest args) ;; if class-restriction is null return all methods, else restrict to list
(define (get-methods class-restriction methd self #!rest args) ;; if class-restriction is null return all methods, else restrict to list
  (kdebug 'get-methods class-restriction)
  (if (and (pair? args) (pair? (car args)) (null? (cdr args))) (set! args (car args)))
  (set! class-restriction (if (procedure? class-restriction) (list class-restriction) class-restriction))

  (sortless-unique
	(let ((mine ((compute-methods methd) (cons self args))))
	  (cond
		((null? mine)
		 (dnl* "Missing method?" (method-register 'rec? mine)))
		((list? class-restriction)
		 (apply append (map (lambda (x) (apply get-methods (cons x (cons methd (cons self args))))) class-restriction))
		 )
		((member class-restriction '(* all ()))
		 mine)
		((primitive-object? class-restriction) ;; single class
		 
		 (let ((theirs (apply compute-methods (cons methd (cons (allocate-instance class-restriction) args)))))
			(filter (lambda (x) (member x theirs)) mine)))
		(#t mine)
		))
	)
  )


;--- (define (call-parent-methods classes methd self #!rest args)

(define (call-parent-methods classes methd self #!rest args)
  (if (and (pair? args) (pair? (car args)) (null? (cdr args))) (set! args (car args)))
  (let ((n (if (pair? classes) (length classes) +inf.0)))
	 (set! classes (cond
						 ((null? classes) #f);
						 ((eq? classes 'all) (parent-classes self))
						 ((eq? classes '*) (parent-classes self))
						 ((eq? classes 'none) #f) ;; in case we have a 
						 (#t classes)))
	 (if (pair? classes)
		  (let* ((ml (apply get-methods (cons classes (cons methd (cons self args)))))
					(result
					 (map
					  (lambda (x)
						 (if (or (generic-method-register 'rec? x)
									(method-register 'rec? x)
									)
							  (begin 
								 (kdebug 'model-body "applying method " (or (generic-method-register 'rec? x) (method-register 'rec? x) ))
								 (apply apply-method (cons x (cons self args))))
							  (begin
								 (kdebug 'model-body "Did not apply method, it wasn't in the registers")
								 #f)
							  )
						 )
					  classes)
					 ))
			 (kdebug 'model-body "call-parent-methods returns " result)
			 result)
		  #t
		  ) )
  )

(define (call-parents& classes methd self #!rest args)
  (if (and (pair? args) (pair? (car args)) (null? (cdr args))) (set! args (car args)))
  (let ((pname (lambda (x)
					  (cond
						((class? x) (class-name-of x))
						((instance? x) (class-name-of (class-of x)))
						(#t x)))))
	 (kdebug 'model-body "calling parents" (if (pair? classes) (map pname classes) (pname classes)))
	 (let ((result
			  (call-parent-methods classes methd self args)
			  ))
		(kdebug 'model-method "... yields " result)
		result)
	 ))


;--- (define (call-first-method classes methd self #!rest args) -- the most/least restrictive match
(define (call-first-method classes methd self #!rest args) ;; The following routine is very similar to the parent-"meth"
  (if (and (pair? args) (pair? (car args)) (null? (cdr args))) (set! args (car args)))
  (let* ((ml (apply get-methods(cons classes (cons methd (cons self args)))))
			)
	 (apply apply-method (cons (car ml) (cons self args)))))


;-- Instantiating and initialising

;--- a few supporting variables.  

;; Uninitialisable classes/instances

(define *uninitialisable*
  (list <top> <class> <procedure-class> <entity-class> <generic>
		  <method> <generic> <primitive-object> <class> <pair>
		  <vector> <string> <list> <input-port> <output-port>
		  <null> <boolean> <symbol> <procedure> <number> <char> 
		  ))

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
	 (if (eqv? name (void))
		  (set! name (object->string it)))
	 (string-append name ":" (number->string (**entity-index**)))))



(define (string-tail s n)
  (list->string (reverse (list-head (reverse (string->list s)) n))))

(define (uninitialise-flag s)
  (if (string? s)
		(if (string=? (string-tail s (string-length "initialised")) "initialised")
			 #f
			 <uninitialised>)
		(uninitialise-flag (object->string s))))

;--- (iflag clss)  constructs the flag indicating a class has had its default initialisation
(define (iflag clss)
  (cond
	((string? clss) (string->symbol (string-append clss "-initialised")))
	((symbol? clss) (iflag (symbol->string clss)))
	((instance? clss) (iflag (class-name-of clss)))
	((class? clss) (iflag (class-name-of clss)))
	((instance? clss) (iflag (class-of clss)))
	(#t #f)))

;--- (define (make-object class #!rest initargs)
(define (make-object class #!rest initargs)
  ;;(dnl "**** entering make-object ****")
  (error "You need to use (create- <object-class-of-some-sort>)" class initargs)
  (let ((instance (if #f
							 (allocate-instance class)
							 (apply make (cons class (initargs))))))
	 (for-each (lambda (x)
					 (slot-set! instance x
									(uninitialise-flag x)
									)) (class-slots-of instance))
 	 (object-register 'add instance class)
	 (set-state-variables instance initargs)
	 instance))


;--- (define (make-agent class . initargs)

(define (make-agent class #!rest initargs)
  (error "You need to use (create <agent-class-of-some-sort>)" class initargs)
  (let ((instance (apply make-object (cons class initargs))))
	 (agent-register 'add instance class)
	 (if (or (eqv? (slot-ref instance 'name) <uninitialised>)
				(eqv? (slot-ref instance 'name) <nameless>)) 
		  (slot-set! instance 'name (serial-number class))) ;; may be overridden/overwritten
	 instance))

(define (check-param-sig filename)
  (let* ((not-tilde (not (char=? #\~ (car (reverse (string->list filename))))))
			(key  (with-input-from-file filename (lambda () (read))))
			(isparameters (equal? (quote (quote Parameters)) key)))
	 (and not-tilde isparameters)))

;--- (apply-initialisation instance key)  loads parameters from the global-parameter list
"Arguably, this whole initialisation thing might be considered part of the 'model' and sit 
in one of the framework classes.  I have put  it here since object initialisation is really
a pretty fundamental part of an object oriented approach to anything, and the initialisation
of entities within the model isn't really an issue w.r.t. the model at all."

(define (p-eval k)
  (cond
	((and (pair? k) (or (eqv? (car k) eval-marker) (equal? (car k) eval-marker)))
	 ;;(kdebug 'state-vars-eval "EVAL: " (cdr k))
	 (apply eval (cdr k)))
	((and (pair? k) (null? (cdr k)))
	 (car k))
	(#t k))
  )

(define (void? x) (eqv? x (void)))

(define (apply-initialisation instance key)
  (let ((kdebug (if #t kdebug dnl*))
		  (verbose (if #f dnl* (lambda x (void))))
		  )
	 (verbose
				"***** apply initialisation for "
				(class-name-of (class-of instance)) "with a key"
				(if (class? key) (class-name-of key) key) "*****")

	 (let* ((flag (iflag key))
			  (p (let ((t (assoc key global-parameter-alist)))
					 (cond
					  ((eqv? t (void)) #f)
					  ((and (pair? t) (pair? (cdr t))) (if (void? (cadr t)) #f (cdr t)))
					  ((pair? t) #f)
					  (#t t))))
			  
			  (tlist (list-intersection (map car (class-slots (class-of instance)))
												 (if (or (not p) (eqv? p (void)))
													  '()
													  (map car  p))))
			  (p* (if p
						 (filter (lambda (n) (member (car n) tlist)) p)
						 #f))
			  )
		
		(verbose "Initialising slots:" tlist)
		(verbose "                  :" p*)
		(kdebug 'initialisation "apply-initialisation:")
		(kdebug 'initialisation "key:   " flag)
		(kdebug 'initialisation "p:     " p )
		(kdebug 'initialisation "tlist: " tlist)
		(kdebug 'initialisation "p*:    " p*)

		(if p*
			 (let ((R (map cons (map car p*) (map p-eval (map cadr p*)))))
				(for-each
				 (lambda (kv)
					(verbose (car kv) '<- (cdr kv))
					(kdebug 'initialisation 'kv: kv)
					(slot-set! instance (car kv) (cdr kv))
					(kdebug 'state-vars (class-name-of (class-of instance)) 'slot-set kv)
					)
				 R)))
		) )
  )


;; This returns a list of the form (key ...) where the "value" is
;; often a list containing a single number.
(define (parameter-lookup class taxon key)
  (dnl* 'parameter-lookup (class-name class) taxon key)
  (if (not (and (isa? class <class>)
					 (string? taxon)
					 (symbol? key)))
		(error "Bad arguments to parameter-lookup: they must be a class, a string (taxon) and a symbol (key)" class taxon key)

		(let ((the-chain (append (!filter (lambda (x) (member x *uninitialisable*))
													 ;;(class-cpl class)
													 (parent-classes class) )
										 (list taxon)))
				(returnval #f))
		  (let ((v (filter (lambda (t) t)
								 (map (lambda (l)
										  (let ((K (assoc key l)))
											 (if K (cdr K) #f)))
										(reverse
										 (filter (lambda (t) t)
													(map (lambda (x)
															 (let ((c (assoc x global-parameter-alist)))
																(if c (cdr c) #f)))
														  the-chain))))
								 ))
				  )
			 (if (and v (not (null? v)))
				  (car v)
				  #f)))))



(define (Xparameter-lookup class taxon key)
  (if (not (and (isa? class <class>)
					 (string? taxon)
					 (symbol? key)))
		(error "Bad arguments to parameter-lookup: they must be a class, a string (taxon) and a symbol (key)" class taxon key)
		(let ((the-classes (!filter (lambda (x) (member x *uninitialisable*))
											 ;;(class-cpl class)
											 (parent-classes class)
											 ))
				(returnval #f))
		  (for-each
			(lambda (x)
			  (if (member x *uninitialisable*)
					(let* ((clst (assoc class global-parameter-alist))
							 (v (if clst (assoc key (cdr clst)) (void)))
							 )
					  (if (not (void? v))
							(set! returnval v))))
			  )
			(reverse the-classes))
		  (let* ((tlst (assoc taxon global-parameter-alist))
					(v (if tlst (assoc key (cdr tlst)) (void))))
			 
			 (if (not (void? v))
				  (set! returnval v)
				  ))
		  returnval)))

(define (boolean-parameter-lookup class taxon key)
  (let ((r (parameter-lookup class taxon key)))
	 (if (and r (pair? r) (boolean? (car r)))
		  (car r)
		  #f)))
(define (numeric-parameter-lookup class taxon key)
  (let ((r (parameter-lookup class taxon key)))
	 (if (and r (pair? r) (number? (car r)))
		  (car r)
		  #f)))

(define (string-parameter-lookup class taxon key)
  (let ((r (parameter-lookup class taxon key)))
	 (if (and r (pair? r) (string? (car r)))
		  (car r)
		  #f)))

(define (symbol-parameter-lookup class taxon key)
  (let ((r (parameter-lookup class taxon key)))
	 (if (and r (pair? r) (symbol? (cadr r)))
		  (car r)
		  #f)))

(define (list-parameter-lookup class taxon key)
  (let ((r (parameter-lookup class taxon key)))
	 (if (and r (pair? (cdr r)) (list? (cadr r)))
		  (cadr r)
		  #f)))

(define (procedure-parameter-lookup class taxon key)
  (error "not tested yet")
  (let ((r (parameter-lookup class taxon key)))
	 (if (and r (pair? (cdr r)) (procedure? (cadr r)))
		  (cadr r)
		  #f)))





(define alort #f)
(define clort #f)
;; Both create and create- make <objects> and things derived from <object>
;; This version does not apply a taxon specific initialisation
(define (create- class #!rest statevars)
  (let ((kdebug (if #t kdebug dnl*))
		  )
	 (if (and (= (length statevars) 1) (list? (car statevars)))
		  (set! statevars (car statevars)))

	 (if (eq? class <polygon>) (kdebug '(object-creation initialisation) "Creating object" (class-name-of class) statevars))
	 (let* ((instance (allocate-instance  class))
			  (the-classes (!filter (lambda (x) (member x *uninitialisable*)) (parent-classes class)))
			  )
		(set! alort instance)
		(set! clort the-classes)
		
		(kdebug 'initialisation-C-- (class-name-of (class-of alort)))
		(kdebug 'initialisation-P-- (map class-name-of the-classes))

		(object-register 'add instance class)

		;; Set *all* slots to uninitialised
		(for-each
		 (lambda (x)
			(let ((flag x))
			  (kdebug 'initialisation-U-- 'uninitialise x)
			  (slot-set! instance x (uninitialise-flag flag))))
		 (class-slots-of instance))

		;; First load the states of the classes from base->most-refined
		(for-each
		 (lambda (x)
			(if (not (member x *uninitialisable*))
				 (begin
					(apply-initialisation instance x)                         ;; these come from the parameter files
					(slot-set! instance (iflag (class-name-of x)) #t)
					)
				 )
			)
		 (append (reverse the-classes) (list (class-of instance)))) ;; run from most general to most specific

		(set-state-variables instance statevars) ;; these come from the create call...
		(if (kdebug? 'initialisation) (dumpslots instance))
		instance
		))
  )

(define (create class taxon #!rest statevars)
  (if (and (= (length statevars) 1) (list? (car statevars)))
		(set! statevars (car statevars)))

  (let* ((instance (create- class statevars))
			)
	 (kdebug '(agent-creation initialisation) "Creating agent" (class-name-of class) taxon statevars)
	 (agent-register 'add instance class)
	 (if (has-slot? instance 'taxon) (slot-set! instance 'taxon taxon))
	 
	 (slot-set! instance 'subjective-time 0)
	 (slot-set! instance 'counter 0)

	 ;; subjective time must be set either in the taxon, in the
    ;; statevars, or explicitly after initialisation
	 (apply-initialisation instance taxon)
	 (slot-set! instance 'initialised #t)

	 (set-state-variables instance statevars)

	 (if (or (eqv? (slot-ref instance 'name) <uninitialised>)
				(eqv? (slot-ref instance 'name) <nameless>))
		  (slot-set! instance 'name (serial-number taxon))) ;; may be overridden/overwritten

	 (if (not (number? (slot-ref instance 'jiggle))) (slot-set! instance 'jiggle 0))

	 ;;(initialisation-checks instance) This is done in the prep-agent routine.
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
