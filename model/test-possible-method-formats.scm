; -*- mode: scheme; -*-


; (default-agent-initialisation <class>)                          ;; 1: args should be null
; (default-agent-initialisation <class> ())                       ;; 2: args should be a pair and (car args) should be null
; (default-agent-initialisation <class> (<parent> ...))           ;; 3: (pair? (car args)),  (caar args) is a symbol (cadr args is a list) (caadr args) is a symbol
; (default-agent-initialisation <class> 'var val ...))            ;; 4: (cdr args) should be a list whose first element is quote
; (default-agent-initialisation <class> () 'var val ...)          ;; 5: (car args) is null cdr should be a list whose first element is quote
; (default-agent-initialisation <class> (<parent> ...) 'var val ...)
                                                                  ;; 6: (car args) is a symbol (cadr args is a list) (caadr args) is a symbol, (cddr args) should be a list whose first element is quote

(define (probe n)
  (display "->")(display n)(newline) #t)

(define-macro (check name #!rest args)
  (display args) (newline)	
  (if (pair? args) (begin (display (car args)) (newline)))
  (if (pair? args) (begin (display (cdr args)) (newline)))
  
  (cond
	((pair? name) (error "Bad name" name))
	((null? args) (display "null args\n")
	 1)
	((and (null? (car args))
			(null? (cdr args)))
	 2)
	((and (pair? (car args))
			(symbol? (caar args)) (not (equal? (caar args) 'quote))
			(null? (cdr args)))
	 3)
	((and (pair? (car args))
			(equal? (caar args) 'quote)
			(not (null? (cdr args)))) 
	 4)
	((and (null? (car args))
			(not (null? (cdr args)))
			(pair? (cadr args)) (equal? (caadr args) 'quote)
			(not (null? (cddr args))))
	 5)
	((and (pair? (car args))
			(probe (caadr args))
			(symbol? (caar args)) (not (equal? (caar args) 'quote))
			(pair? (cdr args))
			(symbol? (caadr args)) (equal? (caadr args) 'quote) (not (null? (cddr args))))
	 6)
	(#t (error "Bad default-agent-initialisation" name args)))
  )



(define (% . args)
  (if (pair? args)
		(begin
		  (display (car args))
		  (for-each (lambda (x) (display " ") (if (list? x) (display x))) (cdr args))
		  ))
  (newline))


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
