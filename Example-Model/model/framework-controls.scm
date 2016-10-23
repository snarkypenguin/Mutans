;-  Identification and Changes

;--
;	framework-controls.scm -- Written by Randall Gray 

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data


;; This is a list of symbols (or strings, I guess) -- kdnl* calls
;; which have a member of this list, get printed during the run.

(define adjust-grey #t)
(define nested-agents '()) ;; needs this for the moment


(define (symbol<? s t)
  (if (not (and (symbol? s) (symbol? t)))
		(error "Passed a non symbol to symbol<?" s t)
		(string<? (symbol->string s) (symbol->string t))))
	


;--- Flag to make access to the kernel impossible for agents when they aren't running

;; If this is set to true, the kernel becomes inaccessible to the
;; agent when it is not running. Thus, it is unable to access
;; non-local information when answering queries and performing updates

(define blue-meanie #f)


;--- registers which help with introspection and logging

(define logger-tags '())
(define submodel-register '())
(define (register-submodel tag . filelist)
  (if (assoc tag submodel-register)
		(set-cdr! (assoc tag submodel-register)
					 (append (assoc tag submodel-register) filelist))
		(set! submodel-register
				(cons (cons tag filelist) submodel-register)))
  )




;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
