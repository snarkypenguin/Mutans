; -*- mode: scheme; -*-

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

				(if (and  unique-names (eq? cmd 'add) opts (assq (car opts) register))
					 (dnl* unique-names "Attempting to re-register a " thingtype "/" thingname ":" args)
					 )
				(cond
				 ((not (symbol? cmd))
				  (abort "+++DIVISION BY DRIED FROG IN THE CARD CATALOG+++" cmd))
				 ((eq? cmd 'help)
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

				 ((eq? cmd 'reg) register)
				 ((eq? cmd 'get)
				  (list-copy register)
				  )

				 ((member cmd '(flush clear))
				  (set! register '()))


				 ((eq? cmd 'dump)
				  (for-each pp register)
				  )

				 ((eq? cmd 'add-unique)
				  (bind-string-to-closure (car opts))
				  (if (not (assq (car opts) register))
						(set! register ;; save things as lists
								(acons (car opts) (cdr opts) register)))
				  (car opts)
				  )

				 ((eq? cmd 'add)
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



;; classes ought to be unique
(define class-register (abstract-register "class" "class-name" #t))

;; We can (must) have many methods of the same name, like "dump"
(define generic-method-register (abstract-register "generic-method" "generic-method-name" #t))
(define method-register (abstract-register "method" "method-name"))
(define object-register (abstract-register "object" "object-name"))
(define agent-register (abstract-register "agent" "agent-name"))
