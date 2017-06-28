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

; The timer routines are not particularly dependent on anything else except for the
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
(define timer-status #f)
(define enable-timers #f)
(define disable-timers #f)

(let* ((stopwatches '()) ;; of the form '((key start|stop timevec_n) ... (key start timevec_0)
		 (nop (lambda x (void)))
		 (uniq (lambda (lst) ;; returns the unique elements w.r.t. eq?: should be sorted
					(cond
					 ((null? lst) '())
					 ((null? (cdr lst)) lst)
					 ((not (equal? (car lst) (cadr lst))) (cons (car lst) (uniq (cdr lst))))
					 (else (uniq (cdr lst))))
					))
		 
		 (purge (lambda () (set! stopwatches '())))
		 (intervals (lambda (which key)
						  (let* ((lst (filter (lambda (x) (eq? key (car x))) stopwatches))
									(startlst (filter (lambda (x) (eq? (cadr x) 'start)) stopwatches))
									(endlst   (filter (lambda (x) (eq? (cadr x) 'stop)) stopwatches))
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

		 (aggregate	(lambda (key #!optional which)
						  (set! which (if which which 0))
						  (APPLY + (intervals which key))))

		 (record (lambda (key) 
					  (let* ((lst (filter (lambda (x) (eq? key (car x))) stopwatches))
								(startlst (filter (lambda (x) (eq? (cadr x) 'start)) stopwatches))
								(endlst   (filter (lambda (x) (eq? (cadr x) 'stop)) stopwatches))
								(n (min (length startlst) (length endlst)))
								)
						 (cond
						  ((and start stop) (list start stop))
						  (start (list start '()))
						  (#t #f)))))
		 
		 (data (reverse stopwatches))
		 (dump (lambda () (pp (reverse stopwatches))))
		 (keylist (lambda () (uniq (sort (map car stopwatches) (lambda (x y) (string<=? (symbol->string x) (symbol->string y)))))) )

		 ;; WARNING -- there is nothing to stop multiple starts on a tag,
		 ;; followed by multiple stops, such as might occur in a recursive
		 ;; routine. This may be a useful thing, or not.

		 (add-entry (lambda (tag entry #!optional comment)
						  (set! stopwatches (cons (list tag entry (process-times) comment) stopwatches))
						  ))
						
		 (start 
		  (lambda (tag #!optional comment) (add-entry tag 'start comment)
					 ))

		 (stop
		  (lambda (tag #!optional comment) (add-entry tag 'stop comment)
					 ))
		 
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
  ;; let* body ---
  (set! disable-timers disable)
  (set! enable-timers enable)
  (set! timer-status (lambda () (not (eq? start-timer nop))))
  )


(enable-timers)
;(disable-timers)


(define (timer-report)
  (let ((timers (timer-keys)))
	 (for-each
	  (lambda (key)
		 (display key)
		 (display ": ")
		 (display (aggregate-time key))
		 (newline))
	  timers)))



;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
