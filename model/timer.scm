; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	timer.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2017.05.19
;		Location: zero:/home/randall/Thesis/model/timer.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2017 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

; The timer routines are not particularly dependedn on anything else except for the
;; assq-append routine
  
(define start-timer #f)
(define stop-timer #f)
(define dump-timers #f)
(define timer-record #f)
(define aggregate-time #f)
(define timer-intervals #f)
(define aggregate-time* #f)
(define timer-intervals* #f)
(define purge-timers #f)
(define timer-keys #f)
(define timer-data #f)
(define enable-timers #f)
(define disable-timers #f)

  
(let* ((stopwatches '())
		 (nop (lambda x (void)))
		 (keys '())
		 (stag (lambda (tag s) (string->symbol (string-append (symbol->string tag) "|" s))))
		 (purge (lambda () (set! stopwatches '())))
		 (intervals (lambda (which key)
						  (let* ((startlst (assq (stag key "start") stopwatches))
									(endlst   (assq (stag key "stop") stopwatches))
									(n (min (length startlst) (length endlst)))
									)
							 (if (and (pair? startlst) (pair? endlst))
								  (map (lambda (a z)
											(let ((i (case which
														  ((0 cpu) 0)
														  ((1 system sys) 1)
														  ((2 real clock) 2)
														  (else (error "Bad selector for call to aggregate-time" which)))
														))
											  (- (f64vector-ref z i) (f64vector-ref a i))))
										 (list-head (cdr startlst) n) (list-head (cdr endlst) n))
								  0))))

		 (aggregate	(lambda (which key)
						  (APPLY + (intervals which key))))

		 (record (lambda (key) 
					  (let* ((startlst (assq (stag key "start") stopwatches))
								(endlst   (assq (stag key "stop") stopwatches))
								(start (assq (stag key "start") stopwatches))
								(stop (assq (stag key "stop") stopwatches)))
						 (cond
						  ((and start stop) (list start stop))
						  (start (list start '()))
						  (#t #f)))))
		 (data stopwatches)				 
		 (dump (lambda () (pp stopwatches)))
		 (keylist (lambda () keys))

		 ;; WARNING -- there is nothing to stop multiple starts on a tag,
		 ;; followed by multiple stops, such as might occur in a recursive
		 ;; routine. This may be a useful thing, or not.

		 (start 
		  (lambda (tag)
			 (if (not (member tag (map car stopwatches)))
				  (set! keys (cons tag keys)))
			 (let* ((s (stag tag "start"))
					  (t (assq s stopwatches)))
				(set! stopwatches (assq-append stopwatches s (process-times))))))
		 (stop
		  (lambda (tag)
			 (if (not (member tag (map car stopwatches)))
				  (set! keys (cons tag keys)))
			 (let* ((s (stag tag "stop"))
					  (t (assq s stopwatches)))
			 (set! stopwatches (assq-append stopwatches s (process-times))))))
		 (enable (lambda ()
					  (set! timer-keys keylist)
					  (set! start-timer start)
					  (set! stop-timer stop)
					  (set! dump-timers dump)
					  (set! timer-record record)
					  (set! aggregate-time aggregate)
					  (set! timer-intervals intervals)
					  (set! purge-timers purge)
					))
		 (disable (lambda ()
					  (set! timer-data nop)
					  (set! timer-keys nop)
					  (set! start-timer  nop)
					  (set! stop-timer  nop)
					  (set! dump-timers  nop)
					  (set! timer-record  nop)
					  (set! aggregate-time  nop)
					  (set! timer-intervals  nop)
					  (set! purge-timers  nop)
					))
		 )
  (set! disable-timers disable)
  (set! enable-timers enable)

  ;(enable)
  (disable)
  )



;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
