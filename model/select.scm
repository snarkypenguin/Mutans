; -*- mode: scheme; -*-

;; These are identical to filter and !filter

(define (select selector lst)
  (if (pair? lst)
		(if (selector (car lst))
			 (cons (car lst) (select selector (cdr lst)))
			 (select selector (cdr lst)))
		lst))

;; (!select selector lst) -- returns a list of those elements which fail the selector
(define (!select selector lst)
  (select (lambda x (not (apply selector x))) lst))

;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
