;-  Identification and Changes

;-- model.scm -- Written by Randall Gray 

;-- code

(define yup-we-are-loaded #f)

(define (model T)
  (doit Q T)
  (shutdown-agents Q))

(if (eq? yup-we-are-loaded #f)
	 (load "loadem.scm"))

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



