(include "framework")
; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;  framework.scm -- Written by Randall Gray 
;  Initial coding: 
;     Date: 2016.07.26
;     Location: zero:/home/randall/Thesis/Example-Model/model/framework.scm
;
;  History:
;
;  This has the "defined" bits, not just the macros.

;-  Code 

(define support-dir "./")


;; This is a list of data added by a (definition-comment ...) clause associated with a
;; (define...)


(define definition-comments '()) ;; collects comments from the code


(define (slow-sort lst)
  (let ((<=? (lambda (x y) (string<=? (object->string x) (object->string y)))))
	 (sort lst <=?)))


(define (count-keyed-members lst) ;; used by monitors
  (let* ((slst (slow-sort lst))
			(keys (uniq (map car slst)))
			(lslst (map (lambda (k) (filter (lambda (c) (equal? k (car c))) slst)) keys))
			)
	 (map length lslst)))
	 
;; subsidiary-agents are agents which may be embedded in a larger dynamic agent. Agents know 
;; what their parent agent is (if they have one) and may indicate to the parent that they should be
;; added to the active list. The parent agent is the one that actually decides if a agent is to move 
;; into the active queue or out of the active queue.  Whe things get moved, "value" from the parent is 
;; moved into the relevant sub-agents.  The set of ecoservices of the parent contains all of the types 
;; represented in its sub-agents.

;(letrec ((m make))
;  (set! make
;		  (lambda (class . initargs)
;			 (let ((instance (apply m (cons class initargs))))
;					 (apply initialise (cons instance initargs))
;				instance))))


;-- Loggers ****

;--- list of tags for introspection and logging

(define adjust-grey #t) ;; used by agents feeding loggers for generating graphical output
(define logger-tags '())
(define (no-default-variables) '()) ;; used to indicate that there are no defaults 





;;; --- The following is deprecated, mainly because it was a complex solution to a simple problem.

;;; (define submodel-register '()) -;

;;; (define (register-submodel tag . filelist) -;
;;;   (if (assoc tag submodel-register) -;
;;; 		(set-cdr! (assoc tag submodel-register) -;
;;; 					 (append (assoc tag submodel-register) filelist)) -;
;;; 		(set! submodel-register -;
;;; 				(cons (cons tag filelist) submodel-register))) -;
;;;   ) -;

;;; (define (load-submodels) -;
;;;   (let ((L load)) -;
;;; 	 "The following code takes the list of registered submodels and loads any files they may be  -;
;;; dependent on.  Loggers must be loaded after the other submodels, so we take two passes." -;

;;; 	 (let ((submodel-files -;
;;; 			  (!filter null? (map	cdr (!filter null? (!filter (lambda (x) (member (car x) logger-tags)) submodel-register)))) -;
;;; 			  )) -;

;;; 		(if (pair? submodel-files) -;
;;; 			 (begin  -;
;;; 				;;(dnl "Submodels: " submodel-files) -;
;;; 				(for-each (if #t -;
;;; 								  L  -;
;;; 								  (lambda (x) -;
;;; 									 (display "loading submodel: ") -;
;;; 									 (display x) -;
;;; 									 (newline) -;
;;; 									 (L x)) -;
;;; 								  ) -;
;;; 							 submodel-files)) -;
;;; 			 (dnl "No submodel files to be loaded")) -;


;;; ;\;; loggers get inserted at the head of the queue -;

;;; 		(let ((logger-files -;
;;; 				 (!filter -;
;;; 				  null? -;
;;; 				  (map -;
;;; 					cdr -;
;;; 					(!filter -;
;;; 					 null? -;
;;; 					 (filter -;
;;; 					  (lambda (x) (member (car x) logger-tags)) -;
;;; 					  submodel-register)))) -;
;;; 				 )) -;

;;; 		  (if (pair? logger-files) -;
;;; 				(begin  -;
;;; 				  ;;(dnl "Loggers: " logger-files) -;
;;; 				  (for-each (if #t -;
;;; 									 L  -;
;;; 									 (lambda (x) -;
;;; 										;;(display "loading logger: ") -;
;;; 										;;(display x) -;
;;; 										;;(newline) -;
;;; 										(L x)) -;
;;; 									 ) -;
;;; 								logger-files)) -;
;;; 				(dnl "No logger files to be loaded")) -;
;;; 		  ) -;
;;; 		) -;

;;; 	 ) -;
;;;   ) -;

;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
