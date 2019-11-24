;-  Identification and Changes

;-- model.scm -- Written by Randall Gray 

;-- code

(include "framework")

(compile-file "support-lib.scm")
(load "support-lib.o1")
(compile-file "sclos.scm")
(load "sclos.o1")
(compile-file "framework-controls.scm")
(load "framework-controls.o1")
(compile-file "declarations.scm")
(load "declarations.o1")
(compile-file "log-declarations.scm")
(load "log-declarations.o1")
(compile-file "framework-classes.scm")
(load "framework-classes.o1")
(compile-file "log-classes.scm")
(load "log-classes.o1")
(compile-file "framework-methods.scm")
(load "framework-methods.o1")
(compile-file "log-methods.scm")
(load "log-methods.o1")
(compile-file "model-classes.scm")
(load "model-classes.o1")
(compile-file "landscape-classes.scm")
(load "landscape-classes.o1")
(compile-file "animal-classes.scm")
(load "animal-classes.o1")
(compile-file "model-methods.scm")
(load "model-methods.o1")
(compile-file "animal-methods.scm")
(load "animal-methods.o1")
(compile-file "landscape-methods.scm")
(load "landscape-methods.o1")
(compile-file "kernel.scm")
(load "kernel.o1")

(load "model-configuration.scm")

(define (model T)
  (doit Q T)
  (shutdown-agents Q))


(display "How long? ")
(model (read))

;; (run-simulation Q 0 20)

;(doit Q 200)
;(shutdown-agents Q)


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
