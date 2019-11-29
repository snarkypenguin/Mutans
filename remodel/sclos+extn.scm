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
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(include "remodel-framework")
;; sclos.scm is included after the definition of (abstract-register ...) and a few 
;; instances of registers.

(define eval-marker '$)


(define (all-slotnames obj)
  (map car (class-slots (class-of obj))))

(define (all-slots obj)
  (let ((names (all-slotnames obj)))
	 (map (lambda (x) (list x (slot-ref obj x))) names)))


;; make missing classes

;; This comes here since we might want the registers available for sclos.scm
(include "sclos.scm")

(define <list> (make-primitive-class))
(define-class <null> (inherits-from <list>) (no-state-variables))
(define-class <integer> (inherits-from <number>) (no-state-variables))
(define-class <rational> (inherits-from <integer>) (no-state-variables))
(define-class <real> (inherits-from <rational>) (no-state-variables))
(define-class <complex> (inherits-from <real>) (no-state-variables))
(define-class <input-output-port> (inherits-from <input-port> <output-port>) (no-state-variables))
(define-class <point> (inherits-from <list>) (no-state-variables))
(define-class <point-list> (inherits-from <list>) (no-state-variables))


;-- include a bunch of things in the class register

(load "abstract-register.scm")

;; Finally register sclos classes and the basic extensions


(register-unique class <point-list>)
(register-unique class <point>)
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
(register-unique class <input-output-port>)
(register-unique class <class>)
(register-unique class <top>)
(register-unique class <primitive-object>)
(register-unique class <procedure-class>)
(register-unique class <entity-class>)
(register-unique class <generic>)
(register-unique class <method>)

(define not-an-object (list <method> <generic> <entity-class> <procedure-class> <primitive-object> <top> <class>))

;-- Begin defining the fundamental classes for entities in the models

(define <> (make-class '() '())) ;; This acts as a null class -- technically there, but outside all ken


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

(define-class <kernel> (inherits-from <object>) (no-state-variables))
"Ultimately, we ought to flesh this out so that all of the state associated with the kernel
is, indeed, a part of a kernel object.  I think this would be a very good avenue for deeper
exploration."

(define -kernel- (allocate-instance <kernel>))

;;(define <> '<>)


;; We add the data-ref and data-set! methods so that we can mask array references
;; in <proxy>

(declare-method has-data? "wrapper for slot-ref")
(declare-method data-ref "wrapper for slot-ref")
(declare-method data-set! "wrapper for slot-set!")
(declare-method @data-ref "wrap slot-ref and access to 'data/data-name slots with extra dereferencing")
(declare-method @data-set! "wrap slot-ref and access to 'data/data-name slots with extra dereferencing")

(model-method (<object> <symbol>) (data-ref self key)
				  (slot-ref self key))

(model-method (<object> <symbol>) (data-set! self key value)
				  (slot-set! self key value))

;;(define has-data? (make-generic)) ;; this is done by the declare-method at the beginning of the file
(model-method (<object> <symbol>) (has-data? self sym)
				  (or (and (object? a)
							  (has-slot? a sym))))


;; Generally this routine returns #t if it is a slot, otherwise it
;; return a partial list of data-names or #f if no match



;; ;;(define data-ref (make-generic)) ;; this is done by the declare-method at the beginning of the file
;; (add-method data-ref
;; 				(make-method (list <object> <symbol>)
;; 								 (lambda (call-next-method self sym)
;; 									(slot-ref self sym))))


;; ;;(define data-set! (make-generic)) ;; this is done by the declare-method at the beginning of the file
;; (add-method data-set!
;; 				(make-method (list <object> <symbol>)
;; 								 (lambda (call-next-method self sym val)
;; 									(slot-set! self sym val))))






;--- agents

(define-class <agent>
  (inherits-from <object>) ;; type is used as a categorical value in kernel-calls
  (state-variables s/n name taxon representation
						 class-migration-list ;; a list that indicates what the classes this class may be moved to
						 transition-points ;; boundaries for transitions in the form (sym val sym val sym ...)
						 transition-function ;; a function calculated using the transition points
						 must-maintain ;; list of state variables that need to be maintained

						 proxy-class
						 replaced-by ;; There is no "replaces" ... if there were, there would be no way of garbage-collecting things (circular lists...)
						 may-run                                ;; as it is, maintenace closures need to be carefully constructed to make sure that they
						 queue-state agent-state                ;; are free from references -- use of list-copy for things like perimeters is essential.
						 current-class-depth                    ;; agent-state is specifically restricted to values associated with its interaction with 
						 note                                   ;; the runqueue: (dead terminated ready-for-prep active suspended ...)
						 kernel
						 subjective-time priority jiggle 
						 dt
						 migration-test timestep-schedule counter
						 state-flags                            ;; Agents may read and write to other agent's state flags to pass information about 
						 agent-epsilon                          ;; representation changes, or other "ensemble-level" data (such as alarm pheromones).
						 default-font
						 default-size
						 default-colour
						 dont-log
						 always-log
						 agent-body-ran
						 no-model-body
						 ;; this needs to be true if the agent does
						 ;; not implement its own model-body

						 ;; a list of "niches" this fills.

						 niche
						 ;; a list of things the agent "provides"
						 provides 
						 ;; a list of things the agent "requires"
						 requires

						 ;; acting as a kernel for others
						 suspended-at
						 subsidiary-agents active-subsidiary-agents

						 ;;
						 memory ;; used for caching information across invocations
						 maintenance-list 
						 initialised
						 )
  )

(declare-method s/n "return the serial number of an agent")
(model-method <agent> (s/n self) (slot-ref self 's/n))

(declare-method adaptable? "indicate if the agent has an alternative representation")
(model-method (<agent>) (adaptable? self))

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
					 ((point? x)        <point>)
					 ((point-list? x)   <point-list>)
					 ((integer? x)     <integer>) 
					 ((rational? x)    <rational>)
					 ((real? x)        <real>)
					 ((complex? x)     <complex>)
					 ((null? x)			 <null>)
					 ((list? x)	       <list>)
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
  (or (null? a)
		(boolean? a)
		(number? a)
		(pair? a)
		(symbol? a)
		(procedure? a)
		(string? a)
		(vector? a)
		(and (%instance? a) #t)))

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
  (if (member a (list <> <top> <class> <procedure-class> <entity-class>))
		#t
		(equal? (class-slots-of a) (class-slots-of <object>))
			  ))



;--- (define (object? a)
(define (object? a)
  (and (%instance? a) (isa? a <object>) #t))

;--- (define (agent? a)
(define (agent? a)
  (and (%instance? a) (isa? a <agent>) #t))

(define (is-class? o c)
  (isa? o c))

;--- (define (has-slot? a k)
(define (has-slot? a k)
  (if (cond
		 ((object? a) (member k (class-slots-of a)))
		 ((class? a) (member k (map car (slot-ref a 'slots))))
		 (else #f)
		 )
		#t
		#f
  ))

(define (input-output-port? x)
  (and (port? x) (input-port? x)(output-port? x)))

(define (file-handle? x)
  (or (output-port? x)
      (input-port? x)
      (postscript? x)))
  

(define (close-output handle)
  (cond
   ((postscript? handle) (handle 'close))
   ((output-port? handle) (close-output-port handle))
   (else (display "Bad argument to (close-output ...)" handle) (abort))))

(define (parent-classes? x)
  (map class-name-of (parent-classes x)))

;--- (define (class-names-of-supers x) ;; returns strings
(define (class-names-of-supers x) ;; takes objects... or classes
  (map class-name-of (classes-of-supers x) ))

;--- (define (slot-values a)
(define (slot-values a)
  (map (lambda (x) (cons x (slot-ref a x))) (class-slots-of a)))


(define pi (acos -1.))
(define 2pi (* 2.0 pi))
(define pi*2 2pi)
(define tau 2pi)
(define sqrt2pi (sqrt 2pi))

;(define e (exp 1))
(define e*2 (* 2 e))
(define e*-2 (* -2 e))

(define 100pi 314)
(define 10000pi 31416)

(define 1-1/e (- 1 (/ 1 e))) ;; ~ .6321
(define lightspeed 299792458);  (/ m second), by the definition of a metre.


;; Used as the placeholder for uninitialised things in classes
(define uninitialised (lambda args (abort 'uninitialised-function)))
;--- (define (uninitialised? x #!rest y)
"There are three things used to indicate an uninitialised value: 
   * the function of arbitrary arguments, uninitialised
   * the symbol '<uninitialised>
   * the object <uninitialised>
"
(define (uninitialised? x #!optional y)
  (cond
	((not y) (member x (list uninitialised '<uninitialised> <uninitialised>)))
	((and (object? x) (symbol? (car y))) (uninitialised? (slot-ref x (car y))))
	(else #f)))


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

;; These need to act before remodel-classes.
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


"NOTE: 'isa' functions take the agent first, then the class.  'is-' functions go the other way.
The primary reason for this is that the 'is-' functions are geared toward filtering lists of agents
and the isa functions are geared toward seeing if an agent 'isa' class/
"


;; The *is- and *has- routines return predicate functions
(define (*is-class? targetclass #!rest plural)
  (set! targetclass (cons targetclass plural))
  ;; this odd way of specifying arguments  ensures at least one arg
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


(define (*has-data? slot)
  (let ((slot (filter symbol? slot)))
	 (lambda (x) 
		(null=#f (if (list? slot)
						 (apply orf (map (lambda (y) (has-data? x y)) slot))
						 (has-data? x slot))))))


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

There is no has-data-value? analogue---slots refer to single items, where data
(plural!) may have all sorts of exotic dereferencing for any given datum within
the corpus.
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

;; (define (*is-*? target #!rest s)
;;   (let* ((targets (if (and (list? target)(null? s)) target (cons target s)))
;; 			  (C (*is-class? targets))
;; 			  (S (*has-slot? targets))
;; 			  (T (*is-taxon? targets))
;; 			  )
;; 	 (lambda (x)
;; 		(dnl* (C x) (S x) (T x))
;; 		(null=#f (or (C x) (S x) (T x))))))

(define (*provides-*? target #!rest s)
  (let* ((targets (if (and (list? target)(null? s)) target (cons target s)))
			(C (apply *is-class? targets))
			(P (apply *provides? targets))
			(T (apply *is-taxon? targets))
			)
	 (lambda (x)
		(null=#f (or (C x) (P x) (T x))))))

;; (define (*matches? target #!rest s)
;;   (let* ((targets (if (and (list? target)(null? s)) target (cons target s)))
;; 			(C (apply *is-class? targets))
;; 			(T (apply *is-taxon? targets))
;; 			(P (apply *provides? targets))
;; 			(D (apply *has-data? targets))
;; 			(S (apply *has-slot? targets))
;; 			)

;; 		(lambda (x)
;; 		  (let ((rslt 
;; 					(null=#f (or (if (P x) 'provides #f)
;; 									 (if (D x) 'has-data #f)
;; 									 (if (S x) 'has-slot #f)
;; 									 (T x) (C x) 
;; 									 )
;; 								))
;; 				  )
;; 			 rslt
;; 			 ))
;; 		)) 

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
(define (check-registers-for thing)
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


 BY CONVENTION we will use the symbol <> to indicate that we want all
 methods... (it will recognise the default multiplication operator as
 being equivalent, but this might cause problems if someone declares <>
 to be something other than multiplication -- best to use the symbol).
 Or, we will use a number to indicate the list-head we are interested
 in.  Using #f is equivalent to a list-head of 0, and #t indicates only
 the first entry.


 This routine calls all applicable methods appropriate for parent
 classes, in contrast to the parent-'method' hook that is passed in as
 the first arg in a method.


 typical invocations might be
   (get-methods <> adjust-status - this-agent 
                current-environment current-prey)
 or
   (get-methods (list <dolphin> <basic-animal> <tracker>) 
                adjust-status this-agent 
                current-environment current-prey)
... in the second case the class <dolphin> might be defined as 
(define-class <dolphin> (inherits-from <basic-animal> <sonar> <tracker>) (state-variables....)



  The second form returns applicable methods common to both the class
  of this-agent and some of its parents.

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
(define (apply-method2 methd args)
  (if (and methd (pair? args) (list? args) ) ;; it is a valid thing
		;;  In the following the (lambda x x) plays the role of the (absent) parent call
		(let ((allarg (cons (lambda x (void)) args)))
;;		  (dnl* allarg)
		  (apply (method-procedure methd) (cons (lambda x (void)) args)))
		(abort))
  )

;--- (apply-method methd obj #!rest args) -- Applies methd to obj with appropriate arguments
;; Use like (apply-method mthd ob 3 14 15 'rad)
(define (apply-method methd #!rest args)
  (dnl* "APPLY-METHOD:" args)
  (if (null? args) (abort "Missing 'self' argument (apply-method)"))
  (apply-method2 methd args))


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



(define (get-methods% methd #!rest signature) ;; This returns the methods in the "natural" order
  (if (null? signature) (begin (dnl* "Missing 'self' argument (get-method%)") (abort)))
  (if (not (isa? methd <generic>)) (begin (dnl* "The argument in the 'method' position is not a <generic> method.") (abort)))
  (let ((cm (compute-methods methd)))
	 (if (procedure? cm)
		  (let ((ML (cm signature)))
			 (if (> (length ML) 1)
				  (reverse (cdr (sortless-unique (cm signature)))) ;; The cdr prunes the one which is (most likely) calling
				  '())))))

;--- (get-methods class-restriction methd self #!rest signature)
;; with class-restrictions, the order of the methods returned corresponds to the restrictions
;; without restrictions, it corresponds to the "natural" order
;; signature is of the form '(method self arg...)
(define (get-methods$ class-restriction #!rest signature) ;; if class-restriction is null return all methods, else restrict to list
  (if (isa? class-restriction <generic>)
		(apply get-methods%  (cons class-restriction signature))
		(begin
		  (if (< (length signature) 2) (begin (dnl* "The call must at least have a method and a self variable.") (abort)))
		  (if (not (is-class? (car signature) <generic>)) (begin (dnl* "The argument in the 'method' position is not a <generic> method.") (abort)))
		  (if (member class-restriction '(() * all))
				(set! class-restriction '())
				(set! class-restriction (if (procedure? class-restriction) (list class-restriction) class-restriction)) )

		  (sortless-unique
			(if (member class-restriction '(() * all))
				 (apply get-methods% signature)
				 (apply append (map (lambda (x) (apply get-methods% (cons (car signature) (cons (allocate-instance x) (cddr signature))))) class-restriction)))))
		))

;--- (get-methods class-restriction methd self #!rest signature) ;; This always returns methods in the "natural" order
;; signature is of the form '(method self arg...)
(define (get-methods class-restriction #!rest signature) ;; if class-restriction is null return all methods, else restrict to list
  (if (isa? class-restriction <generic>)
		(apply get-methods%  (cons class-restriction signature))
		(begin
		  (if (< (length signature) 2) (begin (dnl* "The call must at least have a method and a self variable") (abort)))
		  (if (not (is-class? (car signature) <generic>)) (begin (dnl* "The argument in the 'method' position is not a <generic> method.") (abort)))

		  (let ((m (apply get-methods% signature)))
			 (if (member class-restriction '(() * all))
				  m
				  (let ((M (apply get-methods$ (cons class-restriction signature))))
					 ;;			 (dnl* M)
					 (filter (lambda (x) (member x M)) m))
				  )
			 ))))


;; (apply-method (car (get-methods% name (car Q))) (car Q))
;;->  (apply-method (car (get-methods% the-method object) object))
;; (map (lambda (x) (apply-method x (car Q))) gg)
;; (map (lambda (x) (apply-method x (car Q) 4)) gg)
;; (map (lambda (x) (apply apply-method (cons x (cons (car Q) (list 4))))) gg)
;;-> (let ((signature (cons agent args)))
;;->   (map (lambda (meth) (apply apply-method (cons meth signature) (get-methods [] signature signature)
;;->   )
;(define (X-call-all-parent-methods methd #!rest signature)
;  (let ((invitations (apply get-methods% (cons methd signature))))
;	 (dnl* 'invitations invitations)
;	 (if (null? invitations)
;		  '()
;		  (let ((rsvp
;					(map (lambda (x) (apply apply-method (cons x signature))) invitations)))
;			 (dnl* rsvp)
;			 rsvp))))

(define invites '())
(define (call-all-parent-methods #!rest signature)
;;                                             signature is of the form '(method self arg...)
  (let ((invitations (apply get-methods% signature)))
	 (set! invites invitations)
	 (if (null? invitations)
		  '()
		  (let ((rsvp
					(map (lambda (x) (apply-method2 x (cdr signature))) invitations))
				  )
			 rsvp))))


(define (call-parent-methods$ class-restriction #!rest signature)
;;                                                           signature is of the form '(method self arg...)
  (let ((invitations (apply get-methods$ (cons class-restriction signature))))
	 (if (null? invitations)
		  '()
		  (let ((rsvp (map
							(lambda (x)
							  (apply-method2 x  (cdr signature)))
							invitations)))
			 rsvp))))


(define (call-parent-methods class-restriction #!rest signature)
  (let* ((invitations (apply get-methods (cons class-restriction signature))))
	 (if (null? invitations)
		  '()
		  (let ((rsvp (map
							(lambda (x)
							  (apply-method2  x (cdr signature)))
							invitations)))
			 rsvp))))

	 


;--- (define (call-first-method classes methd self #!rest signature) -- the most/least restrictive match
(define (call-first-method classes methd #!rest signature) ;; The following routine is very similar to the parent-"meth"
  (if (null? signature) (begin (dnl* "Missing 'self' argument (call-first-method)") (abort)))

  (if (and (pair? signature) (pair? (car signature)) (null? (cdr signature))) (set! signature (car signature)))
  (let* ((ml (apply get-methods(cons classes (cons methd (cons self signature)))))
			)
	 (apply apply-method (cons (car ml) (cons self signature)))))


;-- Instantiating and initialising

;--- a few supporting variables.  

;; Uninitialisable classes/instances

(define *uninitialisable*
  (list <top> <class> <procedure-class> <entity-class> <generic>
		  <method> <generic> <primitive-object> <class> <pair>
		  <vector> <string> <list> <point> <point-list> 
		  <input-output-port> <input-port> <output-port>
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

;; This is actually accessible generally unlike the s/n
;; ... the number bits may not always be equal
(define (serial-number it)
  (let ((name (cond
					((string? it) it)
					((symbol? it) (symbol->string it))
					((number? it) (number->string it))
					((class? it) (class-name-of it))
					((instance? it) (class-name-of (class-of it)))
					(#t (object->string it)))))
	 (cond
	  ((eq? it #!void) (number->string (**entity-index**)))
	  ((eqv? name (void)) (string-append (object->string it) "_" (number->string (**entity-index**))))
	  (else (string-append name "_" (number->string (**entity-index**)))))
	 ))


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
(define (make-object% class #!rest initargs)
  (let ((instance (if #f
							 (allocate-instance class)
							 (apply make (cons class (initargs))))))
	 (for-each (lambda (x)
					 (slot-set! instance x
									(uninitialise-flag x)
									)) (class-slots-of instance))
 	 (if use-agent-register (object-register 'add instance class))
	 (set-state-variables instance initargs)
	 instance))


;--- (define (make-agent class . initargs)

(define (make-agent% class #!rest initargs)
  (let ((instance (apply make-object (cons class initargs))))
	 (if use-agent-register (agent-register 'add instance class))
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
in one of the remodel classes.  I have put  it here since object initialisation is really
a pretty fundamental part of an object oriented approach to anything, and the initialisation
of entities within the model isn't really an issue w.r.t. the model at all."

(define (p-eval k)
  (cond
	((and (pair? k) (or (eqv? (car k) eval-marker) (eq? (car k) eval-marker)))
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
  (if (and (eqv? class <>) (string=? taxon ""))
		(error "You must at least specify a class or a taxon, more usually you want both" class taxon key))

  (if (not (and (class? class)
					 (string? taxon)
					 (symbol? key)))
		(error "Bad arguments to parameter-lookup: they must be a class, a string (taxon) and a symbol (key)" class taxon key)

		(let ((the-chain (append (!filter (lambda (x) (member x *uninitialisable*))
													 (if (eqv? class <>) '() (class-cpl class)	))
										 ;;(if (eqv? class <>) '() (parent-classes class)) )
										 ;;(parent-classes class) 
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


;; (define (Xparameter-lookup class taxon key)
;;   (if (not (and (isa? class <class>)
;; 					 (string? taxon)
;; 					 (symbol? key)))
;; 		(error "Bad arguments to parameter-lookup: they must be a class, a string (taxon) and a symbol (key)" class taxon key)
;; 		(let ((the-classes (!filter (lambda (x) (member x *uninitialisable*))
;; 											 ;;(class-cpl class)
;; 											 (parent-classes class)
;; 											 ))
;; 				(returnval #f))
;; 		  (for-each
;; 			(lambda (x)
;; 			  (if (member x *uninitialisable*)
;; 					(let* ((clst (assoc class global-parameter-alist))
;; 							 (v (if clst (assoc key (cdr clst)) (void)))
;; 							 )
;; 					  (if (not (void? v))
;; 							(set! returnval v))))
;; 			  )
;; 			(reverse the-classes))
;; 		  (let* ((tlst (assoc taxon global-parameter-alist))
;; 					(v (if tlst (assoc key (cdr tlst)) (void))))
			 
;; 			 (if (not (void? v))
;; 				  (set! returnval v)
;; 				  ))
;; 		  returnval)))

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
  (let ((r (parameter-lookup class taxon key)))
	 ;;(dnl* (class-name-of class) taxon key ":" r)
	 (if (and r (pair? r) (procedure? (car r)))
		  (car r)
		  #f)))





(define last-object-created #f)
(define last-object-classes-used #f)


;; Both create and create-object make <objects> and things derived from <object>
;; This version does not apply a taxon specific initialisation
(define (make-object class #!rest statevars)
  (let ((kdebug (if #t kdebug dnl*))
		  )
	 (if (and (= (length statevars) 1) (list? (car statevars)))
		  (set! statevars (car statevars)))

	 (if (eq? class <polygon>) (kdebug '(object-creation initialisation) "Creating object" (class-name-of class) statevars))
	 (let* ((instance (allocate-instance  class))
			  (the-classes (!filter (lambda (x) (member x *uninitialisable*)) (parent-classes class)))
			  )
		(set! last-object-created instance)
		(set! last-object-classes-used the-classes)
		
		(kdebug 'initialisation-C-- (class-name-of (class-of last-object-created)))
		(kdebug 'initialisation-P-- (map class-name-of the-classes))

		(if use-agent-register (object-register 'add instance class))

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

		(set-state-variables instance statevars) ;; these come from the make call...
		(if (kdebug? 'initialisation) (dumpslots instance))
		instance
		))
  )


;; We define things this way so that each agent gets a unique serial number
;; which may be useful for making identifiers unique.
(define make-agent
  (let* ((S/N 0)
			(CREATE
			 (lambda (class #!optional (taxon 'fail) #!rest statevars)
				(if (not (member <agent> (parent-classes class)))
					 (error "Use make-object to create <objects> that are not <agents>" class taxon))
				(if (eq? taxon 'fail)
					 (error "Missing 'taxon' string in attempt to make an agent" class))
				(if (and (= (length statevars) 1) (list? (car statevars)))
					 (set! statevars (car statevars)))
				(if (not (string? taxon))
					 (begin
						(dnl* "The indicated taxon for" class
								"is a symbol.  Taxa should always be specified as strings,")
						(dnl* "and this is treated as a fatal error." class taxon)
						(abort)))

				(let* ((instance (make-object class statevars))
						 )
				  (slot-set! instance 's/n S/N)
				  (set! S/N (+ S/N 1))

				  (kdebug '(agent-creation initialisation) "Creating agent" (class-name-of class) taxon statevars)
				  (if use-agent-register (agent-register 'add instance class))
				  (if (has-slot? instance 'taxon) (slot-set! instance 'taxon taxon))
				  
				  (slot-set! instance 'may-run #t) ;; all agents may run by default, except for <proxy> agents
				  (slot-set! instance 'subjective-time 0)
				  (slot-set! instance 'counter 0)
				  (slot-set! instance 'queue-state 'ready-for-prep)
				  (slot-set! instance 'agent-state 'ready-for-prep)
				  (slot-set! instance 'current-class-depth 0) ;; 0 is the "leafmost" class

				  ;; subjective time must be set either in the taxon, in the
				  ;; statevars, or explicitly after initialisation
				  (apply-initialisation instance taxon)
				  (slot-set! instance 'initialised #t)

				  (set-state-variables instance statevars)

				  (if (or (eqv? (slot-ref instance 'name) <uninitialised>)
							 (eqv? (slot-ref instance 'name) <nameless>))
						(slot-set! instance 'name (serial-number taxon))) ;; may be overridden/overwritten

				  (if (not (number? (slot-ref instance 'jiggle))) (slot-set! instance 'jiggle 0))

				  ;;(initialise instance) This is done in the prep-agent routine.
				  instance
				  )
				))
			)
	 CREATE))

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
