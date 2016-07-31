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
	
;-- Warning messages and kernel reporting

;--- warnings

(define show-warnings #f)


(define (warning . args)
  (if show-warnings
		(begin
		  (display "*** warning: ")
		  (apply dnl* args)
		  )))

;--- kernel message closure and accessors

(define kernel-message? #f)
(define clear-kernel-messages! #f)
(define copy-kernel-messages! #f)
(define set-kernel-messages! #f)
(define add-kernel-message! #f)
(define remove-kernel-message! #f)
(define kdnl* #f)

(let* ((kernel-messages '())
		 (isa? (lambda (x)
					(member x kernel-messages)))
		 (copy (lambda ()
					(copy-list kernel-messages)))
		 (clear (lambda ()
					 (set! kernel-messages '())))
		 (set (lambda (msg)
			    (if (not (list? lst))
					  (error "set-kernel-messages! requires a list of symbols" lst))
				 (set! kernel-messages (copy-list lst))))
		 (add (lambda (msg)
				  (if (not (symbol? msg))
						(error "add-kernel-message! requires a symbol" msg))
				  (set! kernel-messages (uniq (sort (cons msg kernel-messages)) symbol<?))))
		 (remove (lambda ()
					  (if (not (symbol? msg))
							(error "remove-kernel-messages! requires a symbol" msg))
					  (set! kernel-messages (filter (lambda (x) (not (eq? msg x))) kernel-messages))))
		 (kdnl (lambda (msg . args)
					(if (or (member msg kernel-messages) (member '* kernel-messages)
							  (and (list? msg)
									 (not (null? (intersection msg kernel-messages)))))
						 (begin
							(display msg)(display " ==> ")
							(apply dnl* args)))))

		 )
  (set! kernel-message? isa?)
  (set! clear-kernel-messages! clearn)
  (set! copy-kernel-messages! copy)
  (set! set-kernel-messages! set)
  (set! add-kernel-message! add)
  (set! remove-kernel-message! remove)
  (set! kdnl* kdnl)
)


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
