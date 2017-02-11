; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	playpen.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.11.05
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/playpen.scm
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

(load "sclos.scm")
(define (dnl* . args)
  (if (not (null? args))
      (let ((h (car args))
            (t (cdr args)))
        (display (car args))
        (for-each (lambda (x) (display " ") (display x)) t)))
  (newline))

  
(define (dnl . args)
  (if (not (null? args))
      (let ((h (car args))
            (t (cdr args)))
        (display (car args))
        (for-each (lambda (x) (display x)) t)))
  (newline))


(define <base1> (make-class (list <object>) (list 'b1a 'b1b)))
(add-method initialise
				(make-method (list <base1>)
								 (lambda (initialise-parent me inits)
									(for-each (lambda (initarg-name slot-name)
													(slot-set! me
																  slot-name
																  (getl inits initarg-name 0)))
												 '(b1a b1b)
												 '(b1a b1b))
									(pp (list 'base1: (slot-ref me 'b1a) (slot-ref me 'b1b)))
									)))

(define <base2> (make-class (list <object>) (list 'b2a 'b2b)))
(add-method initialise
				(make-method (list <base1>)
								 (lambda (initialise-parent me inits)
									(for-each (lambda (initarg-name slot-name)
													(slot-set! me
																  slot-name
																  (getl inits initarg-name 0)))
												 '(b2a b2b)
												 '(b2a b2b))
									(pp (list 'base2: (slot-ref me 'b2a) (slot-ref me 'b2b)))
									)))

(define <testclass> (make-class (list <base1> <base2>) (list 'c1 'c2)))
(add-method initialise
				(make-method (list <testclass>)
								 (lambda (initialise-parent me inits)
									(for-each (lambda (initarg-name slot-name)
													(slot-set! me
																  slot-name
																  (getl inits initarg-name 0)))
												 '(c1 c2)
												 '(c1 c2))
									(pp (list 'testclass: (slot-ref me 'c1) (slot-ref me 'c2)))
									)))
(define <testclassp> (make-class (list <base1> <base2>) (list 'c1 'c2)))
(add-method initialise
				(make-method (list <testclass>)
								 (lambda (initialise-parent me inits)
									(for-each (lambda (initarg-name slot-name)
													(slot-set! me
																  slot-name
																  (getl inits initarg-name 0)))
												 '(c1 c2)
												 '(c1 c2))
									(initialise-parent)
									(pp (list 'testclassp: (slot-ref me 'c1) (slot-ref me 'c2)))
									)))


(define C (make <testclass> 'c1 0 'c2 1 'b2a 3 'b2b 4 'b1a 5 'b1b 6))
(define Cp (make <testclassp> 'c1 0 'c2 1 'b2a 3 'b2b 4 'b1a 5 'b1b 6))



"
  SCLOS doesn't have a mechanism (that I am aware of) for coercing the type of an instance
  to one of its parent classes, so chaining back to any but the last listed 'parent' methods 
  is difficult if two of the parents have methods with the same name.
"







(define-macro (state-variables . lst) `'(,@lst))
;; This makes it clear when classes are defining state vars

(define-macro (no-state-variables) `'())
;; This makes it clear when classes are defining NO state vars

(define-macro (inherits-from . iflst) `(list ,@iflst))

;; Support for the "isa?" calls and for a mapping from the class
;; object to a name

;; Remember, if you try to unquote classtype without splicing, the
;; interpreter tries to evaluate it.

;; cpl... class precedent list
(define-macro (isa? self . classtype)
  `(let ((ancestors (class-cpl (class-of ,self))))
	  (if (apply orf
					 (map (lambda (x) (member x ancestors))
							(list ,@classtype))) 
			#t #f)
	  )
  )
(define (register-class . args)
  (write args)(newline))

(define-macro (define-class cname . args)
  `(begin
	  (define ,cname (make-class ,@args (symbol->string ',cname)))
	  ,cname
	  )
)





;;; (define <position> (make-class (list <object>) (list 'x 'y))) ;;;
;;; (add-method initialise ;;;
;;; 				(make-method (list <position>) ;;;
;;; 								 (lambda (initialise-parent pos inits) ;;;
;;; 									(for-each ;;;
;;; 									 (lambda (initarg-name slot-name) ;;;
;;; 										(dnl* "position has " inits) ;;;
;;; 										(dnl* "position setting" slot-name "to" (getl inits initarg-name 0)) ;;;
;;; 										(slot-set! pos slot-name (getl inits initarg-name 0))) ;;;
;;; 									 '(x y) ;;;
;;; 									 '(x y))))) ;;;

;;; (define <n-position> (make-class (list <position>) (list 'z+ 'n))) ;;;
;;; (add-method initialise ;;;
;;; 				(make-method (list <n-position>) ;;;
;;; 								 (lambda (initialise-parent npos inits) ;;;
;;; 									(initialise-parent) ;;;
;;; 									(dnl* "n-position has " inits) ;;;
;;; 									(slot-set! npos 'z+ 42) ;;;
;;; 									(slot-set! npos 'n 3)))) ;;;
									
									

									 





;-  The End 


;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
