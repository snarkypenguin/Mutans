

(define (conjugate c)
  (+ (real-part c) (* 0+1i (- (imag-part c)))))

(define (arg c)
  (atan (imag-part c) (real-part c)))

(define (complex-log c)
  (+ (real-part (log (sqrt (* c (conjugate c))))) 
	  (* 0+1i (arg c))))

(define (complex-exp c)
  (let ((ev (exp (real-part c)))
		  (sx (sin (imag-part c)))
		  (cx (cos (imag-part c)))
		  )
	 (+ (* ev cx) (* 0+1i ev sx))))


(define (complex-pow b e)
  (complex-exp (* e (complex-log b))))


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
