;-  Identification and Changes

;--
;	boink.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2013.06.03
;		Location: odin:/home/gray/study/src/boink.scm
;
;	History:
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(load "/etc/script.scm")


(define (definitions filename)
  (let* ((rawdata (with-input-from-file filename
						  (lambda ()
							 (let loop ((file '()))
								(let ((bit (read)))
								  (cond
									((eof-object? bit) (reverse file))
									((not (pair? bit))
									 (loop file))
									(else 
									 (loop (cons bit file))))))
							 ))
						)
		 (defines (map cadr (filter (lambda (x) (eqv? (car x) 'define)) rawdata)))
		 )
	 defines))

(define file-list
  '("support.scm" "utils.scm" "units.scm" "sort.scm"
	 "maths.scm" "utils.scm" "integrate.scm" "maths.scm" 
	 "matrix.scm" "integrate.scm" "postscript.scm" 
	 "basic-population.scm" "sclos.scm" "framework-controls.scm"
	 "declarations.scm" "log-declarations.scm" "framework-classes.scm" 
	 "support.scm" "framework-methods.scm" "log-classes.scm" 
	 "log-methods.scm" "model-classes.scm" "landscape-classes.scm" 
	 "animal-classes.scm" "model-methods.scm" "animal-methods.scm"
	 "landscape-methods.scm" "kernel.scm" "model-configuration.scm" 
	 "model-parameters.scm" "model.scm"))


(define defn-lists (map definitions file-list))

(define symbols (map cons file-list (map (lambda (f) (filter atom? f)) defn-lists)))
(define procedures (map cons file-list (map (lambda (f) (filter procedure? f)) defn-lists)))


													 

;- The End 

;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
