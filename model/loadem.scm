; -*- mode: scheme; -*-

(define (sym->scm x) (let ((fn (string-append (symbol->string x) ".scm")))
							  ;;(display fn)(newline)
							  fn))

;; Must happen before framework  or they have no effect
(load "preamble.scm")
;(load 'print-loading)

(for-each load (map sym->scm '(sort wildmatch utils timer wildmatch tree-ring kdebug model-flags sclos)))
(include "framework") ;; must come before sclos+extn.scm

(for-each load (map sym->scm '(sclos+extn)))


(for-each load (map sym->scm '(units constants maths integrate matrix papersizes postscript)))
;(load "support.o1")

(for-each load (map sym->scm '(basic-population)))
;; postscript should (logically) come after the mathematical files ... it uses matrices and such			 
(for-each load (map sym->scm '(framework-declarations framework framework-classes)))
(for-each load (map sym->scm '(introspection-classes monitor-classes log-classes)))
(for-each load (map sym->scm '(diffeq-classes landscape-classes plant-classes animal-classes)))
(for-each load (map sym->scm '(framework-wrappers declarations framework-methods)))
(for-each load (map sym->scm '(introspection-methods monitor-methods log-methods)))
(for-each load (map sym->scm '(diffeq-methods landscape-methods oak plant-methods animal-methods)))

;; The kernel alway comes last.
(load "kernel.scm")
;(load "kernel.o1")

(load "model-flags.scm") ;; Flags for kdebug/kdebug? messages/code and other global flags
(load "parameters.scm") ;; code to handle the parameter files 

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
