; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	plotting.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.12.11
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/plotting.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2016 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(define (ixin sym lst)
  (- (length lst) (length (member sym lst))))


(define (symbolist #!rest a)
  (if (pair? a)
		a
		(map string->symbol (map list->string (map list (string->list (if (null? a) char:curves (car a))))))))

(define (getix lst #!rest a)
  (let ((s (apply symbolist a)))
	 (map (lambda (x) (ixin x s)) lst)))

(define (species-subset sys species-syms #!rest a)
  (let ((ixs (apply getix (cons species-syms (symbolist a)))))
  (lambda (x)
	 (dat (sys x))
	 (map (lambda (y) (list-ref dat y)) ixs))))

(define (species-subset% sys ixs)
  (let ((ixs ixs))
	 (lambda (x)
		(let ((dat (sys x)))
		  (map (lambda (y) (list-ref dat y)) ixs)))))


(define (plot-species data syms s f)
  (let* ((c:c char:curves)
			(ixlist (getix syms))
			)
	 (set! char:curves (apply string-append (map symbol->string syms)))
	 (plot (species-subset% data ixlist) s f)
	 (set! char:curves c:c)
  ))
	 
(define (subset-species data glst lst) 
  (let* ((symbols (symbolist glst))
			(m (length symbols))
			(I (list-intersection symbols lst))
			(ix (getix lst))
			)

	 (lambda (i)
		(let ((x (data i)))
		 (map (lambda (y)
				  (list-ref x y))
				ix)
		 ))
	 ))

(define (species-subset-list data lst #!rest a) 
  (let* ((symbols (apply symbolist a))
			(m (length symbols))
			(I (list-intersection symbols lst))
			(ix (getix lst))
			)

	 (map
	  (lambda (x)
		 (map (lambda (y)
				  (list-ref x y))
				ix)
		 )
	  data)))
	 
;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
