; -*- mode: scheme; -*-
(load "preamble.scm")
(load "sort.scm")
;(load "sclos-exp.scm")
(include "sclos+extn.scm")
(include "remodel")

(define initialise (make-generic))

(define-class <object> (inherits-from <primitive-object>) (state-variables kind intensity))
(add-method initialise (make-method (list <object>)
												(lambda (call-next-method object initargs)
												  (dnl* "calling initialise for <object>")
												  (call-next-method)
												  (initialise object initargs)
												  object) ))
									
(define-class <metal> (inherits-from <primitive-object>) (state-variables material))
(define-class <spoon> (inherits-from <metal> <object>) (state-variables course))
(define-class <fork> (inherits-from <metal> <object>) (state-variables tine-count))
(define-class <knife> (inherits-from <metal> <object>) (state-variables length sharpness))

(define-class <cutlery> (inherits-from <knife> <fork> <spoon>) (state-variables cutler))

;; (add-method initialise
;; 				(make-method (list <object>)
;; 								 (lambda (call-next-method object initargs) (call-next-method) (display " (initialise <object> ")(display initargs)(newline) object)))

;; (add-method initialise
;; 				(make-method (list <metal>)
;; 								 (lambda (call-next-method object initargs)
;; 									(display " [initialise <metal>] ")
;; 									(display initargs)
;; 									(display "\n--> ")
;; 									(call-next-method)
;; 									(newline)
;; 									object)))
;; (add-method initialise
;; 				(make-method (list <fork>)
;; 								 (lambda (call-next-method object initargs)
;; 									(call-next-method)
;; 									(display " [initialise <fork>] ")
;; 									(display initargs)
;; 									(newline)
;; 									object)))

;; (add-method initialise
;; 				(make-method (list <spoon>)
;; 								 (lambda (call-next-method object initargs)
;; 									(call-next-method)
;; 									(display " [initialise <spoon>] ")
;; 									(display initargs)
;; 									(newline)
;; 									object)))

;; (add-method initialise
;; 				(make-method (list <knife>)
;; 								 (lambda (call-next-method object initargs)
;; 									(call-next-method)
;; 									(display " [initialise <knife>] ")
;; 									(display initargs)
;; 									(newline)
;; 									object)))

(add-method initialise
				(make-method (list <object>)
								 (lambda (call-next-method object initargs)
									;(call-next-method)
									(display " {initialise <object>} ")
									(display initargs)
									(newline)
									object)))

(add-method initialise
				(make-method (list <metal>)
								 (lambda (call-next-method object initargs)
									(display " {initialise <metal>} ")
									(display initargs)
									(call-next-method)
									(newline)
									object)))
(add-method initialise
				(make-method (list <knife>)
								 (lambda (call-next-method object initargs)
									(call-next-method)
									(display " {initialise <knife>} ")
									(display initargs)
									(newline)
									object)))

(add-method initialise
				(make-method (list <spoon>)
								 (lambda (call-next-method object initargs)
									(call-next-method)
									(display " {initialise <spoon>} ")
									(display initargs)
									(newline)
									object)))

(add-method initialise
				(make-method (list <fork>)
								 (lambda (call-next-method object initargs)
									(call-next-method)
									(display " {initialise <fork>} ")
									(display initargs)
									(newline)
									object)))
(add-method initialise
				(make-method (list <cutlery>)
								 (lambda (call-next-method object initargs)
									(call-next-method)
									(display " {initialise <cutlery>} ")
									(display initargs)
									(newline)
									object)))

				
(define ob1-knob (make <knife> 'kind 3 'metal 'silver))
(define silverware (make <cutlery> 'kind 3 'metal 'silver))




;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:


