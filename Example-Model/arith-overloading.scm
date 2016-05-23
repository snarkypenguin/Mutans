


(define (extend-arith-op-to-funcs op) 
  (lambda args
	 (lambda (x)
		(apply op (map (lambda (f) (if (procedure? f) (f x) f)) args)))))


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
