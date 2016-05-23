;-  Identification and Changes

;--
;	utils.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2003.06.16
;		Location: Odin.valhalla.asgard:/home/gray/scm/utils.scm
;
;	History:
;

;-  Copyright 


;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;(require 'stdio)
;(require 'pretty-print)

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

;(load "root.scm")

'(display "Loading utils.scm\n")

(define (d* . args)
  (map display args)
 )

(define (dnl . args)
  (map display args)
  (newline))

(define (gcar y)
  (if (pair? y) (car y) y))

(define (gcdr y)
  (if (pair? y) (cdr y) y))


(define (fand a . b) (if (null? b) (not (not a)) (and a (apply fand b))))
(define (for a . b) (if (null? b) (not (not a)) (or a (apply fand b))))

(define (real->integer k)
  (let* ((s (number->string k))
	 (i (string-index s #\.))
	 (ss (if i (substring s 0 i) s))
	 )
    (string->number ss)))
    

(define (o->s obj fprec)
  (if (zero? fprec)
      (object->string obj)
      (let ((s (object->string obj))
	    (prec (abs fprec)))

	(if (string? obj) (set! s obj))

	(if (< (string-length s) prec)
	    (let loop ()
	      (if (< (string-length s) prec)
		  (if (< fprec 0)
		      (set! s (string-append s " "))
		      (set! s (string-append " " s)))
		  )
	      (if (< (string-length s) prec) (loop)) ))

	(if (>  (string-length s) prec)
	    (if (< fprec 0)
		(set! s (substring s 0 prec))
		(set! s (substring s (- (string-length s) prec) (string-length s)))))
	s)))
  

(define (attrib-level level attrib-levels) 
  (let loop ((bl attrib-levels) (ix 0))
    (cond
     ((null? bl) ix)
     ((< level (gcar bl)) ix)
     (else 
      (if (list? bl) 
	  (loop (gcdr bl) (+ ix 1))
	  ix
	  ))
     )
    ))

(define (generate-matrix x-axis y-axis f)
  (let ((m '())
	(row '()))

    (let oloop ((y 0))
      (if (<= y (- (length y-axis) 1))
	  (begin 
	    (set! row '())
	    (let iloop ((x (- (length x-axis) 1)))
	      (if (>= x 0)
		  (begin
		    (set! row (cons (f (list-ref x-axis x) (list-ref y-axis y)) row))
		    (iloop (- x 1))
		    )) )
	    (set! m (cons row m))
	    (oloop (+ y 1)) )))
    m))
      

(define tm-schema '(
   "<tabular|<tformat|<table" ; preamble
   "|<row" ; row-start
   "|<cell|" ; cell-start
   "\"" ; start-quote
   "\\|" ; vinculum
   "\"" ; end-quote
   ">" ; cell-end
   ">" ; row-end
   " ----" ; first-divider
   " "; vinculum-gap
   "----" ; divider
   " " ; second-divider
   ">>>\n")) ;appendix

(define scm-schema '(
   "(tabular (tformat (table " ; preamble
   "(row " ; row-start
   "(cell " ; cell-start
   "\"" ; start-quote
   "|" ; vinculum
   "\"" ; end-quote
   ") " ; cell-end
   ") " ; row-end
   " ----" ; first-divider
   " "; vinculum-gap
   "----" ; divider
   " " ; second-divider
   ")))\n")) ;appendix

(define debug-schema '(
   "pre\n" ; preamble
   "rs: " ; row-start
   " [" ; cell-start
   ">" ; start-quote
   "|" ; vinculum
   "<" ; end-quote
   "]" ; cell-end
   " :re\n" ; row-end
   " ----" ; first-divider
   " "; vinculum-gap
   "----" ; divider
   " " ; second-divider
   "post\n")) ;appendix


(define ascii-schema '(
   "\n" ; preamble
   "" ; row-start
   "" ; cell-start
   "" ; start-quote
   "|" ; vinculum
   "" ; end-quote
   "" ; cell-end
   "\n" ; row-end
   "-----------------" ; first-divider
   " "; vinculum-gap
   "-----------------" ; divider
   "" ; second-divider
   "\n")) ;appendix


;;; (define (print-matrix schema fprec matrix x-axis y-axis . mapping) ***
;;;   (let ((preamble (list-ref schema 0)) ***
;;; 	(row-start (list-ref schema 1)) ***
;;; 	(cell-start (list-ref schema 2)) ***
;;; 	(start-quote (list-ref schema 3)) ***
;;; 	(vinculum (list-ref schema 4)) ***
;;; 	(end-quote (list-ref schema 5)) ***
;;; 	(cell-end (list-ref schema 6)) ***
;;; 	(row-end (list-ref schema 7)) ***
;;; 	(first-divider (list-ref schema 8)) ***
;;; 	(vinculum-gap (list-ref schema 9)) ***
;;; 	(divider  (list-ref schema 10)) ***
;;; 	(second-divider (list-ref schema 11))  ***
;;; 	(appendix (list-ref schema 12)) ) ***
    
;;;     (printf "%s" preamble) ***

;;;     ; Print body of matrix ***
;;;     (let oloop ((i matrix) ***
;;; 		(y (reverse y-axis))) ***
;;;       (if (not (null? i)) ***
;;; 	  (begin ***
;;; 	    (printf "%s" row-start) ***
;;; 	    ; print y-axis labels if requested ***
;;; 	    (if (not (null? y)) ***
;;; 		(printf "%s%s%s%s%s%s"  ***
;;; 			cell-start  ***
;;; 			start-quote (o->s (gcar y) fprec) vinculum end-quote  ***
;;; 			cell-end)) ***
	    
;;; 	    ; print row of matrix ***
;;; 	    (let iloop ((j (gcar i))) ***
;;; 	      (if (not (null? j)) ***
;;; 		  (begin ***
;;; 		    (printf "%s%s%s%s%s"  ***
;;; 			    cell-start  ***
;;; 			    start-quote (o->s (if (null? mapping) (gcar j) ((car mapping) (gcar j))) fprec) end-quote  ***
;;; 			    cell-end) ***
;;; 		    (iloop (gcdr j))))) ***
;;; 	    (printf "%s" row-end) ***
;;; 	    (oloop (gcdr i) (gcdr y)) ***
;;; 	    ))) ***
;;;     ; Print x-axis labels if requested ***
;;;     (if (and (list? x-axis) (not (null? x-axis))) ***
;;; 	(begin ***
;;; 	  ; space for y labels ***
;;; 	  (if (and (list? y-axis) (not (null? y-axis))) ***
;;; 		(printf "%s%s%s%s%s%s"  ***
;;; 			cell-start  ***
;;; 			start-quote (o->s first-divider fprec) vinculum end-quote  ***
;;; 			cell-end)) ***

;;; 	  (let aloop ((j x-axis)) ***
;;; 	    (if (not (null? j)) ***
;;; 		(begin ***
;;; 		  (printf "%s%s%s%s%s"  ***
;;; 			  cell-start  ***
;;; 			  start-quote (o->s divider fprec) end-quote  ***
;;; 			  cell-end) ***
;;; 		  (aloop (gcdr j))))) ***
;;; 	  (printf "%s" row-end) ***

;;; 	  ; space for y labels ***
;;; 	  (if (and (list? y-axis) (not (null? y-axis))) ***
;;; 	      (printf "%s%s%s%s%s%s%s"  ***
;;; 		      row-start  ***
;;; 		      cell-start start-quote (o->s second-divider fprec) vinculum-gap end-quote  ***
;;; 		      cell-end)) ***

;;; 	  (let nloop ((j x-axis)) ***
;;; 	    (if (not (null? j)) ***
;;; 		(begin ***
;;; 		  (printf "%s%s%s%s%s"  ***
;;; 			  cell-start  ***
;;; 			  start-quote (o->s (gcar j)  fprec) end-quote  ***
;;; 			  cell-end) ***
;;; 		  (nloop (gcdr j))))) ***

;;; 	  (printf "%s" row-end) ***
;;; 	  )) ***
;;;     (printf "%s" appendix) ***
;;;     (display "") ***
;;;     ) ***
;;; ) ***

(define (stats-bin)
  (let* ((mode 'accurate) ;; fast or accurate
		  (fmin 1e+99)
		  (fmax -1e+99)
		  (mean 0)
		  (epsilon 0.0)
		  (variance 0.0)
		  (n 0.0)
		  (ssq 0.0)
		  (sum 0.0)
		  (data '())
		  (add-number 
			(lambda (x . i)
			  (if (null? i)
					(set! i 1)
					(set! i (car i)))

			  (set! fmin (min x fmin))
			  (set! fmax (max x fmax))

			  (if (<= i 1)
					(begin
					  (set! n (+ n 1))

					  (set! ssq (+ ssq (* x x)))
					  (set! sum (+ sum x)) 

					  (set! data (cons x data))
					  (set! epsilon (/ (- x mean) n))

					  (case mode
						 ('fast
						  (set! mean (/ sum n))
						  (set! variance (/ (- ssq (* n mean mean)) n) ))
						 (else ;; 'accurate
						  (set! variance (+ (* (/ (- n 1) n) variance) 
												  (* epsilon epsilon (- n 1))))
						  (set! mean (+ epsilon mean)) )) )

					(begin
					  (set! n (+ n i))

					  (set! ssq (+ ssq (* x x i)))
					  (set! sum (+ sum (* x i)))

					  (set! data (append (make-list i x) data))

					  (case mode
						 ('fast
						  (set! epsilon (/ (- x (/ (- sum x) (- n 1)) n)))
						  (set! mean (/ sum n))
						  (set! variance (/ (- ssq (* n  mean mean)) n) ))
						 (else ;; 'accurate
						  (let statloop ((j 0))
							 #f
							 )))
					  (if (< j i)
							(begin
							  (set! epsilon (/ (- x mean) n))
							  (set! variance (+ (* (/ (- n 1) n) variance) 
													  (* epsilon epsilon (- n 1))))
							  (set! mean (+ epsilon mean)) 
							  (statloop (+ j 1)))))) 
			  )
			)
		  )

	 (lambda d
		(cond
		 ((null? d)
		  (set! data (sort! data <))
		  (list 
			(cons 'stat:mode mode)
			(cons 'stat:n n) 
			(cons 'stat:mean mean)
			(if (<= 0 (length data))
				 (cons 'stat:median 0)
				 (cons 'stat:median (list-ref data (/ (length data) 2))))
			(cons 'stat:min fmin)
			(cons 'stat:max fmax)
			(cons 'stat:sum_x sum)
			(cons 'stat:sum_x-min (- sum (* n fmin)))
			(cons 'stat:sum_max-x (- (* n fmax) sum))
			(cons 'stat:sum_xx ssq)
			(cons 'stat:variance variance)
			(cons 'stat:variance-1 
					(if (> n 0) (/ (* variance n) (- n 1)) 0))
			(cons 'stat:stddev (sqrt variance))))

		 ((number? (car d))
		  (add-number (car d)))

		 ((symbol? (car d))
		  (case (car d)
			 ('stat:mode mode)
			 ('stat:n n) 
			 ('stat:mean mean)
			 ('stat:median (if (<= 0 (length data))
									 0
									 (list-ref data (/ (length data) 2))))
			 ('stat:min fmin)
			 ('stat:max fmax)
			 ('stat:sum_x sum)
			 ('stat:sum_x-min (- sum (* n fmin)))
			 ('stat:sum_max-x (- (* n fmax) sum))
			 ('stat:sum_xx ssq)
			 ('stat:variance variance)
			 ('stat:variance-1 (if (> n 0) (/ (* variance n) (- n 1)) 0))
			 ('stat:stddev (sqrt variance))

			 ('fast (set! mode 'fast))
			 ('accurate (set! mode 'accurate))
			 ('fill-to 
			  (if (< (length d) 2)
					'error-in-stat-bin
					(let ((i (- n (cadr d)))
							(v (if (= (length d) 2) 0 (caddr d))))
					  (if (> i 0) 
							(add-number v i)))))
			 ('add
			  (if (< (length d) 2)
					'error-in-stat-bin
					(let ((i (cadr d))
							(v (if (= (length d) 2) 0 (caddr d))))
					  (if (> i 0) 
							(add-number v i)))))
			 (else (list 'unimplemented-stat d)))
		  )
		 (#t #f) 
		 )
		) 
	 )
  )

;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
