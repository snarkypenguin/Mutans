;-  Identification and Changes

;-- model.scm -- Written by Randall Gray 

;-- code

(define yup-we-are-loaded #f)

;; This calls doit on the list of agents -- Q -- with an indicated interval -- T
;; which then shuts everything down (cleaning up and closing files ... though
;; not in that order)

(define (model T)
  (doit Q T)
  (shutdown-agents Q))

(if (eq? yup-we-are-loaded #f)
	 (load "loadem.scm"))


;; This is a wrapper for 
(define (go)
   (display "Model configuration file? ")
  (let ((file (object->string (read))))
	 (load file)
	 (display "Run the model for how long? ")
	 (model (read)))
)


;(dnl "(run-simulation Q 0 20)")
;(dnl "(doit Q 200)")
;(dnl "(shutdown-agents Q)")

(dnl "Now try typing \"(go)\"")

;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:



