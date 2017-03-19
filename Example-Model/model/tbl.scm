; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	tbl.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2017.02.15
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/tbl.scm
;
;	History:
;  Format originated in the NWSJEMS research project cf \cite{Gray2006nws} in thesis.bib

;-  Discussion 

"
   Designed for both input, output and simple processing by other programs (particularly
   unix/linux style pipes).
   Format: 

        TBL
        fieldname1 _ fielname2 _ ...
        type1 _ type2 _ ...
        .
        .
        .

   Fields and data columns are space separated.  There is no mechanism
   for including a space in a character string field.

   Fieldnames are strings of nonspace characters (typically alphanums)

   Types can be
        C -- character (string)
        I -- integer
        T -- time
        F -- float
        H -- histogram (a set of numbers separated by commas)
        ll -- lat/lon location (a set of two or three numbers separated by commas)
        L -- location (a set of ordinates (usually two or three numbers) separated by commas)
"
(load "preamble.scm")

(define tbl-alpha-fields '(C))

(define tbl-numeric-fields '(I F))

(define tbl-composite-fields '(T L H LL)) ;; T H L and ll not yet implemented
  
(define supported-tbl-fields (append tbl-numeric-fields tbl-alpha-fields tbl-composite-fields))

(define (new-tbl filename field-list)
  (let ((fcheck (filter (lambda (x) (member x supported-tbl-fields)) field-list))
		  (N (length field-list))
		  (filename filename)
		  (time->external (lambda x x))
		  (time->internal (lambda x x))
		  (location->external (lambda x x))
		  (location->internal (lambda x x))
		  )
	 (if (or (not (string? filename)) (< (string-length filename) 1))
		  (error "Bad filename" filename)
		  (if (< (length fcheck) (length field-list))
				(error "Unsupported field type in tbl field list" field-list)
				(let* ((data (list (map symbol->string field-list) (list "TBL")))
						 (k 0)
						 (tbl (lambda x
								  (cond
									((null? x) data)
									((eq? (car x) 'internal-time-mapping) ;; 
									 (if (procedure? (cadr x))
											 (set! time->internal (cadr x))
											 (error "arguments to 'internal-time-mapping should be a function that maps to internal time" x)))
									((eq? (car x) 'external-time-mapping) ;; 
									 (if (procedure? (cadr x))
											 (set! time->external (cadr x))
											 (error "arguments to 'external-time-mapping should be a function that maps to external time" x)))
									((eq? (car x) 'external-location-mapping) ;;
									 (if (procedure? (cadr x))
											 (set! location->external (cadr x))
											 (error "arguments to 'external-location-mapping should be a function that maps to external time" x)))
									((eq? (car x) 'internal-location-mapping) ;;
									 (if (procedure? (cadr x))
											 (set! location->internal (cadr x))
											 (error "arguments to 'internal-location-mapping should be a function that maps to internal time" x)))
									((null? (cdr x))
									 (cond
									  ((eq? (car x) 'close)
										(with-output-to-file filename
										  (lambda ()
											 (let ((data (reverse data)))
												(apply display (car data))
												(newline)
												(for-each
												 (lambda (a) (display a)(display " "))
												 (cadr data))
												(newline)
												(for-each
												 (lambda (x)
													(for-each
													 (lambda (y)
														(display y)
														(display " ")
														)
													 x)
													(newline)
													)
												 (cddr data)
												 ))
										  ))
										(set! data #f)
										)
									 ((= N (length (car x)))
									  (set! k (+ k 1))
									  (set! data (cons (car x) data)))
									  (#t (error "Closed file or unsupported command: " x))
									  )
									 )
									 ((and (= (length x) N) data) ;; This is problematic for things one element long  .... fix it
									  ;; When I get around to it we check types here
									  (set! k (+ k 1))
									  (set! data (cons x data))
									  )
									 ((and (= (length (car x)) N) data)
									  ;; When I get around to it we check types here
									  (set! k (+ k 1))
									  (set! data (cons (car x) data))
									  )
									 (#t (error "Unsupported command or closed file: " x))
								  )
								)
						 ))
				  tbl)))))

(define latlon->string object->string)
(define location->string object->string)
(define histogram->string object->string)

(define (TBLent->type t v)
  (case t
	 ((C)
	  (cond
		((string? v) v)
		((number? v) (number->string v))
		((histogram? v) (histogram->string v))
		((latlon? v) (latlon->string v))
		((location? v) (location->string v))
		(else (error "Unrecognised field type" t))
		)
	  )
	 ((I F N)
	  (cond
		((string? v) (string->number v))
		((number? v) v)
		((or (histogram? v)
			  (location? v))
			  (if (and (null? (cdr v)) (number? (car v)))
					(car v)
					(error "bad number" v)))
		(else (error "Unrecognised field type" t))
		))
	 ((L ll H) (error "Field type not implemented yet" t))
	 (else (error "Unrecognised field type" t))))

(define (load-tbl filename)
  (let* ((p (open-input-file filename))
			(data (load-list-from-port p)))
	 (close-port p)
	 data)
  (if (and (string-ci=? (car data) "TBL")
			  (= (length (cadr data))
				  (length (caddr data))))
		(let ((syms (map string->symbol (cadr data)))
				(types (map string->symbol (caddr data))))
		  (cons (map cons syms types)
				  (map (lambda (x)
							(map TBLent->type t x) types x)
						 (cddr data))))))




;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
