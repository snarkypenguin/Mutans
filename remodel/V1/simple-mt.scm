;-  Identification and Changes

;--
;	simple-mt.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2013.11.28
;		Location: pooh:/local/home/randall/study/src/simple-mt.scm
;
;	History:
;


;!;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

;; This thread terminates 
(define (make-test-thread-1 N #!optional name)
  (let ((body (lambda () 
					 (let loop ((i 0)) 
						(if name (begin (display name) (display ": ")))
						(display i)
						(newline)
						(system "sleep 2") 
						(if (< i N) 
							 (loop (+ 1 i)))
						))
				  ))
  ;;(let ((thr (apply make-thread (cons body (if name (list name) '())))))
  ;;thr)
  (apply make-thread (cons body (if name (list name) '())))
  )
)



;;(define u (make-test-thread 10))
;;(define v (make-test-thread 10 'Tux))

;;(thread-name u)
;;(thread-name v)
;;(eq? (thread-name u) #!void) ;; ==> #t

(define (dnl . args) (if (null? args) (display "") (let () (map display args) (newline))))


(define (make-toy-agent name agent EndAt dt) 
;; for a "real" agent we'd also include the kernel and some mechanism for getting the time/dt
;; for the tick....  Say 

  (let* ((t 0)
			(dt dt)
			(name name)
			(agent agent)
			(endat EndAt)
			(body (lambda ()
						  (dnl "{entering agent with " t " " dt "}")
						  (if name 
								(begin
								  (if (>= t endat)
										(dnl "["name " is terminating]")
										(begin
										  (dnl "["  name " is running: " t "-->" (+ t dt) "]")
										  (system "sleep 2")
										  (dnl "["  name " is now at " (+ t dt) "]")
										  (set! t (+ t dt))
										  ))
								  )
								(dnl "Unnamed agent.")
								)))
			)
	 (let ((thread (apply make-thread (cons body (if name (list name) '())))))
		(thread-specific-set! thread agent)
		thread)
	 ))
	 


;; The "specific" field of the interaction queue is a runqueue

(define (add-to-interaction-queue intqueue qid agent)
  (thread-specific-set! intqueue (q-insert (thread-specific intqueue) agent Qcmp)))







#|
  What I need is a little set of "agents" tbat run at different times, and cluster in a few threads.

  Issues: 

  		overall temporal coherence
		proper sequencing within a thread

		SOMEWAY to handle many to one interactions

|#





				
;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
