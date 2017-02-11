; -*- mode: scheme; -*-

(define OD display)
(display "display ") (display display) (newline)
(display "OD      ") (display OD) (newline)

(define display (lambda x (if (> (length x) 1)
										(begin
										  (OD "[" (cadr x))
										  (OD  (car x) (cadr x))
										  (OD "]" (cadr x)))
										(begin
										  (OD "[")
										  (OD  (car x))
										  (OD "]"))
								))
  )

(define WR write)
(define write (lambda x (if (> (length x) 1)
										(begin
										  (OD "[%" (cadr x))
										  (WR  (car x) (cadr x))
										  (OD "%]" (cadr x)))
										(begin
										  (OD "[%")
										  (WR  (car x))
										  (OD "%]"))
								))
  )

(define PRETTY-PRINT pretty-print)
(define pretty-print (lambda x (if (> (length x) 1)
										(begin
										  (OD "[$" (cadr x))
										  (PRETTY-PRINT  (car x) (cadr x))
										  (OD "$]" (cadr x)))
										(begin
										  (OD "[$")
										  (PRETTY-PRINT  (car x))
										  (OD "$]"))
								))
  )

(define PP pp)
(define pp (lambda x
				 (error "Not to be here!")
				 (if (> (length x) 1)
					  (begin
						 (OD "[:" (cadr x))
						 (PP  (car x) (cadr x))
						 (OD ":]" (cadr x)))
					  (begin
						 (OD "[:")
						 (PP  (car x))
						 (OD ":]"))
					  ))
  )

(define NL newline)
(define newline (lambda x (if (> (length x) 0)
										(begin
										  (OD "NL" (cadr x))
										  (NL (cadr x)))
										(begin
										  (OD "NL")
										  (NL))
										)))

(define (output-probe)
  (display "display ") (display display) (newline)
  (display "OD      ") (display OD) (newline)
  (display "WR      ") (display WR) (newline)
  (display "PP      ") (display PP) (newline)
  (display "PP*     ") (display pretty-print) (newline)
  )

(output-probe)

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
