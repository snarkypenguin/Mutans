
;;-- This returns the parents of an instance or class

"
The strategy is to check the car of each list against its presence in
any following list --- if it is found, it isn't an immediate parent and 
can be dropped.
"

(define (parent-classes x)
  (define (c*tst x y)
	 (cond
	  ((null? y) #f)
	  ((member x (car y)) #t)
	  (#t (c*tst x (cdr y)))))

  (let ((c* (reverse (cdr (map class-cpl (classes-of-supers (if (class? x) (allocate-instance x) x))))))
		  )
	 (let loop ((C c*)
					(actual-parents '()))
		;;(if (and (pair? C)(pair? (car C))) (dnl* 'T: (class-name-of (caar C))))
		;;(if (pair? C) (dnl* 'C: (map (lambda (s) (class-name-of (car s))) (cdr C))))
		;;(dnl* 'P: (map class-name-of actual-parents))
		(cond
		 ((null? C) actual-parents)
		 ((and (pair? C) (pair? (car C)) (c*tst (caar C) (cdr C)))
		  (loop (cdr C) actual-parents))
		 (#t (loop (cdr C) (cons (caar C) actual-parents))))))
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
(define (uninitialised? x #!rest y)
  (if (object? x) (uninitialised? (slot-ref x (car y)))
		(if (null? y)  (or (eq? x '<uninitialised>)(eqv? x <uninitialised>))
			 (and (uninitialised? x)
					(apply uninitialised? y)))))

;--- (define (nameless? x #!rest y)
(define (nameless? x #!rest y)
  (if (null? y)
		(or (eq? x '<nameless>) (eqv? x <nameless>))
		(nameless? (slot-ref x (car y)))))


;-- Accessors, predicates
;--- (define (set-state-variables self arguments)
(define (set-state-variables self arguments)
  (if (null? arguments)
		(void)
		(if (not (and (pair? arguments) (even? (length arguments))))
			 (error "Bad state variable list!" (class-name-of (class-of self)) arguments)
			 (cond
			  ((null? arguments) (void))
			  ((<= 2 (length arguments))
				(let ()
				  (if (has-slot? self (car arguments))
						(slot-set! self (car arguments) (cadr arguments))
						(kdnl* 'state-vars-missed (class-name-of (class-of self)) "does not have a slot" (car arguments)))
				  (set-state-variables self (cddr arguments)))
				)
			  (#t (error "The list of initialisers is missing something!" arguments))))))


;; These need to preceed framework-classes.
(add-method initialise (make-method (list <object>)
												(lambda (call-next-method self #!rest initargs)
												  
												  (if (and (pair? initargs) (null? (cdr initargs)))
														(set! initargs (car initargs))
														)
												  (set-state-variables self initargs)
												  ;(initialise self initargs)
												  ;(apply initialise (cons self initargs))
												  self) ))


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
 classes, in contrast to the 'method'-parent hook that is passed in as
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
		  (if (memq (car l) r)
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
  (kdnl* 'get-methods class-restriction)
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
				  (filter (lambda (x) (memq x theirs)) mine)))
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
								 (kdnl* 'model-body "applying method " (or (generic-method-register 'rec? x) (method-register 'rec? x) ))
								 (apply apply-method (cons x (cons self args))))
							  (begin
								 (kdnl* 'model-body "Did not apply method, it wasn't in the registers")
								 #f)
							  )
						 )
					  classes)
					  ))
			 (kdnl* 'model-body "call-parent-methods returns " result)
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
	 (kdnl* 'model-body "calling parents" (if (pair? classes) (map pname classes) (pname classes)))
	 (let ((result
			  (call-parent-methods classes methd self args)
						))
		(kdnl* 'model-method "... yields " result)
		result)
))


;--- (define (call-first-method classes methd self #!rest args) -- the most/least restrictive match
(define (call-first-method classes methd self #!rest args) ;; The following routine is very similar to the "meth"-parent
  (if (and (pair? args) (pair? (car args)) (null? (cdr args))) (set! args (car args)))
  (let* ((ml (apply get-methods(cons classes (cons methd (cons self args)))))
			)
	 (apply apply-method (cons (car ml) (cons self args)))))


;-- Instantiating and initialising

;--- a few supporting variables.  

;; Uninitialisable classes/instances

(define *uninitialisable*
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
	 (if (eqv? name (void))
		  (set! name (object->string it)))
	 (string-append name ":" (number->string (**entity-index**)))))



(define (string-tail s n)
  (list->string (reverse (list-head (reverse (string->list s)) n))))

(define (initialise-flag s)
  (if (string? s)
		(if (string=? (string-tail s (string-length "initialised")) "initialised")
			 #f
			 <uninitialised>)
		(initialise-flag (object->string s))))
		


;--- (define (make-object class #!rest initargs)
(define (make-object class #!rest initargs)
  ;;(dnl "**** entering make-object ****")
  (error "You need to use (create- <object-class-of-some-sort>)" class initargs)
  (let ((instance (if #f
							 (allocate-instance class)
							 (apply make (cons class (initargs))))))
	 (for-each (lambda (x)
					 (slot-set! instance x
									(initialise-flag x)
									)) (class-slots-of instance))
	 (slot-set! instance 'type (class-name class)) ;;; This may be replaced by the set-state-variables call and the
	                                               ;;; initialise call
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

;--- (iflag clss)  constructs the flag indicating a class has had its default initialisation
(define (iflag clss)
  (cond
	((string? clss) (string->symbol (string-append clss "-initialised")))
	((symbol? clss) (iflag (symbol->string clss)))
	((class? clss) (iflag (class-name-of clss)))
	((instance? clss) (iflag (class-of clss)))
	(#t #f)))
  
;--- (apply-initialisation instance key #!rest verbose)  loads parameters from the global-parameter list
"Arguably, this whole initialisation thing might be considered part of the 'model' and sit 
in one of the framework classes.  I have put  it here since object initialisation is really
a pretty fundamental part of an object oriented approach to anything, and the initialisation
of entities within the model isn't really an issue w.r.t. the model at all."

(define (p-eval k)
  (cond
	((and (pair? k) (or (eqv? (car k) eval-marker) (equal? (car k) eval-marker)))
	 (kdnl* 'state-vars-eval "EVAL: " (cdr k))
	 (apply eval (cdr k)))
	((and (pair? k) (null? (cdr k)))
	 (car k))
	(#t k))
  )

(define (apply-initialisation instance key #!rest verbose)
  (kdnl* 'initialisation "***** apply initialisation for " (class-name-of (class-of instance)) "with a key" key "****")

  
  (let* ((flag (iflag key))
			(p (let ((t (assoc key global-parameter-alist)))
				  (if (and t (pair? t)) (cdr t) t)))
				
			(tlist (list-intersection (map car (class-slots (class-of instance)))
											  (if p (map car  p) '())))

			(p* (if p
					  (filter (lambda (n) (member (car n) tlist)) p)
					  #f))
			)

	 (kdnl* 'initialisation 'p: p)
	 (kdnl* 'initialisation 'tlist: tlist)
	 (kdnl* 'initialisation 'p*: p*)



	 (if p*
		  (let ((R (map cons (map car p*) (map p-eval (map cadr p*)))))
			 (for-each
			  (lambda (kv)
				 (kdnl* 'initialisation 'kv: kv)
				 (slot-set! instance (car kv) (cdr kv))
				 (kdnl* 'state-vars (class-name-of (class-of instance)) 'slot-set kv)
				 )
			  R)))
	 ))


(define alort #f)
(define clort #f)
;; Both create and create- make <objects> and things derived from <object>
;; This version does not apply a taxon specific initialisation
(define (create- class #!rest statevars)
  (kdnl* '(object-creation initialisation) "Creating object" (class-name-of class) statevars)
  (let* ((instance (allocate-instance  class))
;(cpl (class-cpl cls))
			(the-classes (class-cpl class))
			)
	 (set! alort instance)
	 (set! clort the-classes)
	 
	 (kdnl* 'initialisation-C-- (class-name-of (class-of alort)))
	 (kdnl* 'initialisation-P-- (map class-name-of the-classes))

	 (object-register 'add instance class)

	 ;; Set *all* slots to uninitialised
	 (for-each
	  (lambda (x)
		 (kdnl* 'initialisation-U-- 'uninitialise x)
		 (slot-set! instance x (initialise-flag x)))
	  (class-slots-of instance))

	 ;; First load the states of the classes from base->most-refined
	 (for-each
	  (lambda (x)
		 (kdnl* 'initialisation-CLASS--  "looking at " (class-name-of x))

		 (let ((can-do (not (member x *uninitialisable*))))
			(kdnl* 'initialisation-CLASS--  (if can-do "" "NOT") "applying initialisation for [" (class-name-of x) "]")
			(if can-do
				 (begin
					(set-state-variables instance (initialisation-defaults-for class)) ;; these are the state set by a (default-initialisation <class>) clause
					(apply-initialisation instance x)                         ;; these come from the parameter files
					(slot-set! instance (iflag x) #t))))
		 )
	  (reverse the-classes)) ;; run from most general to most specific

	 (set-state-variables instance statevars) ;; these come from the create call...
	 (if (kdnl*? 'initialisation) (dumpslots instance))
  instance
  ))



(define (create class taxon #!rest statevars)
  (let* ((instance (apply create- (cons class statevars)))
			)
	 (kdnl* '(agent-creation initialisation) "Creating agent" (class-name-of class) taxon statevars)
	 (agent-register 'add instance class)
	 (if (has-slot? instance 'taxon) (slot-set! instance 'taxon taxon))
	 
	 (slot-set! instance 'subjective-time 0)
	 ;; subjective time must be set either in the taxon, in the
    ;; statevars, or explicitly after initialisation
	 (apply-initialisation instance taxon #t)
	 (slot-set! instance 'initialised #t)

	 (dumpslots instance)
	 
	 (if (or (eqv? (slot-ref instance 'name) <uninitialised>)
				(eqv? (slot-ref instance 'name) <nameless>))
		  (slot-set! instance 'name (serial-number class))) ;; may be overridden/overwritten

	 (set-state-variables instance statevars)
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
