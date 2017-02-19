;-  Identification and Changes

;-- model.scm -- Written by Randall Gray 

;-- code

(include "framework")

;;; (compile-file "support-lib.scm")
;;; (load "support-lib.o1")
;;; (compile-file "sclos.scm")
;;; (load "sclos.o1")
;;; (compile-file "framework-controls.scm")
;;; (load "framework-controls.o1")
;;; (compile-file "declarations.scm")
;;; (load "declarations.o1")
;;; (compile-file "log-declarations.scm")
;;; (load "log-declarations.o1")
;;; (compile-file "framework-classes.scm")
;;; (load "framework-classes.o1")
;;; (compile-file "log-classes.scm")
;;; (load "log-classes.o1")
;;; (compile-file "framework-methods.scm")
;;; (load "framework-methods.o1")
;;; (compile-file "log-methods.scm")
;;; (load "log-methods.o1")
;;; (compile-file "model-classes.scm")
;;; (load "model-classes.o1")
;;; (compile-file "landscape-classes.scm")
;;; (load "landscape-classes.o1")
;;; (compile-file "animal-classes.scm")
;;; (load "animal-classes.o1")
;;; (compile-file "model-methods.scm")
;;; (load "model-methods.o1")
;;; (compile-file "animal-methods.scm")
;;; (load "animal-methods.o1")
;;; (compile-file "landscape-methods.scm")
;;; (load "landscape-methods.o1")
;;; (compile-file "kernel.scm")
;;; (load "kernel.o1")

;;; (load "model-configuration.scm")

(define loaded #f)

(define load-list-1 '("sclos.o1" "support.o1" "classdecs.o1" "kernel.o1"))
(define load-list-2 '("chassisa.o1" "chassisb.o1" "chassisc.o1" "chassisd.o1"))
(define load-list-3 '("log.o1"))
(define load-list-4 '("landscape.o1"))
(define load-list-5 '("plant.o1" "animal.o1"))

(define (load-em lst)
  (for-each
	(lambda (x)
	  (display "Loading ")
	  (display x)
	  (load x)
	  (newline))
	lst))
 

(define (load-em-all)
  (load-em (append load-list-1 load-list-2 load-list-3 load-list-4 load-list-5))
  (set! loaded #t)
  )


(define (model T)
  (doit Q T)
  (shutdown-agents Q))


(define (go)
  (if (not loaded) (load-em-all))
  (display "Model configuration file? ")
  (let ((file (object->string (read))))
	 (load file)
	 (display "Run the model for how long? ")
	 (model (read)))
)
;(dnl "(run-simulation Q 0 20)")
;(dnl "(doit Q 200)")
;(dnl "(shutdown-agents Q)")

(dnl "Try typing '(go)'")



;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:



