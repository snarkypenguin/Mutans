; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	rebuild-load-sequence.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.10.30
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/rebuild-load-sequence.scm
;

(define file-contents (load-list-from-file "rewrite"))
(define (process-entry fileent)
  (let ((ent (cons (string-append (car fileent) ".scm") (cddr fileent))))
	 (if (member (car ent) (cdr ent))
		  (begin (display "Found a file trying to rewrite itself: ")
					(display (car ent))
					(newline))
		  (with-output-to-file
				(car ent)
				(lambda ()
				  (for-each (lambda (fe)
								  (if (not (string=? fe (car ent)))
										(begin
										  (display "(load \"")
										  (display fe)
										  (display "\")\n")) (cdr ent))
								  ) (cdr ent))
				  ))
		  )))
(define (process-entry-for-source-load ent)
  (if (member (car ent) (cdr ent))
		(begin (display "Found a file trying to rewrite itself: ")
				 (display (car ent))
				 (newline))
		(for-each (lambda (fe)
						(if (not (string=? fe (car ent)))
							 (begin
								(display "(load \"")
								(display fe)
								(display "\")\n")) (cdr ent))
						) (cdr ent))
		))


(for-each process-entry file-contents)



;; (with-output-to-file "model-source"
;;   (display "(include \"framework\")\n")
;;   (for-each
;; 	(lambda (ent)
;; 	  (for-each
;; 		(lambda (x) 
;; 		  (display "(load \"")
;; 		  (display x)
;; 		  (display "\")\n"))
;; 		(cddr ent)))
;; 	file-contents)
;;   )


