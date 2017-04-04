;-  Identification and Changes


(define-macro (testdef sym  lst)
  (map (lambda (x) `'(,sym ,@x)) lst))

  



(testdef Wobble (this is it) (no it is not) (Yes it is) No Yes No)

;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
