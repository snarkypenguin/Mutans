;-   Identification and Changes

;--

;	kernel.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2008.04.07
;		Location: localhost:/usr/home/gray/Study/playpen/kernel.scm.scm
;
;-  Code 


;;(require 'sort)
;;(require 'pretty-print)
;;(require 'common-list-functions)

;;(load "stats-bins.scm")

;;*** The kernel must be loaded *after* the agents are all created --
;;this is so it has access to the "run" "migrate" ... generic
;;methods.


;;---------------------------------------------------
;; Important routines which I Really Ought to Know ;;
;;---------------------------------------------------


;; (queue t stop runqueue . N)
;; t is model time, stop is when the whole model ought to stop 
;; runqueue is a list of run-records

;; (q-insert Q rec reccmp) inserts the record "rec" into the queue "Q"
;; sorting records with "reccmp"
;;; Call: (set! rq (q-insert rq (make <whatever> ...) Qcmp))

(define lookit-running-names #f) ;; emits the name of the running
											;; agent if true

(load "postscript.scm")
;;; This is loaded here since it may be used to produce all sorts of
;;; snapshot data spanning agents


(define developing #t)
(define current-page #f)
(define current-page-number 0)


(define terminating-condition-test 
			(lambda rq #f))


;---------------------------------------------------
;               Kernel support
;---------------------------------------------------

;; Compares the subjective time of two agents
(define (Qcmp r1 r2)
  (let ((st1 (subjective-time r1))
		  (st2 (subjective-time r2)))
	 (cond
	  ((< st1 st2) #t)
	  ((> st1 st2) #f)
	  (#t (let ((p1 (priority r1))
					(p2 (priority r2)))
			  (cond
				((> p1 p2) #t)
				((< p1 p2) #f)
				(#t (< (jiggle r1) (jiggle r2)))
				))))))

(define (map-q arg q)
  (map (lambda (s) (arg s)) q))


(define (running-queue rq stop)
  (cond
	((null? rq) #f)
	((not (pair? rq)) #f)
	((not (list? rq)) #f)
	((not (agent? (car rq))) #f)
	(#t (let ((f (car rq)))
			(< (subjective-time f) stop)))))

;; returns the "time" of the agent at the head of the queue (unused)
(define (model-time rq stop)
  (if (running-queue rq stop)
		(if (or (null? rq) (not (list? rq)))
			 rq
			 (let ((f (car rq)))
				(subjective-time f))
			 )
		'end-of-run
		)
  )

;; returns an interval (tick-length) based on the current time, the
;; desired tick length, the nominated end of the run and a list of
;; target times
(define (interval t ddt stopat tlist)
  ;; tlist is a sorted queue of times to run
  (if (< (- stopat t) ddt)
		(set! ddt (- stopat t)))

  (cond
	((null? tlist)	
	 ddt)
	((and (list? tlist) 
			(number? (car tlist))
			(eq? (car tlist) t)
			)
	 ddt)
	((and (list? tlist) 
			(number? (car tlist))
			)
	 (- (car tlist) t))
	(else 'bad-time-to-run)))

;; remove stale times in the time-to-run queue
(define (prune-local-run-queue tm ttr)
  (let (
;		  (call-starts (cpu-time))
		  (r '())
		  )
	 (set! r (let loop ((l ttr))
				  (if (or (null? l)
							 (> tm (car l))
							 )
						l
						(loop (cdr l)))))
;	 (set! kernel-time (+ kernel-time (- (cpu-time) call-starts)))
	 r )
  )

;; insert a run record in the queue Q
(define (q-insert Q rec reccmp)
  (let ((j (jiggle rec))
;		  (call-starts (cpu-time))
		  )
	 (set! Q (remove rec Q))

	 (if (and (positive? j) (< j 1.0))
		  (set-jiggle! rec (abs (random-real))))

	 (let* ((f (append Q (list rec)))
			  (sf (sort f reccmp))
			  )
;		(set! kernel-time (+ kernel-time (- (cpu-time) call-starts)))
		sf)
	 )
  )


;---------------------------------------------------
;           Kernel -- the main loop
;---------------------------------------------------

;; this is so we catch runaway growth
(define (test-queue-size rq N)
  (if (and (number? N) (> (length rq) N))
		(begin
		  (dnl "There are " (length rq)
				 " entries in the runqueue when we expect at most " N);
		  (dnl "These entities total "
				 (apply + (map (lambda (x) (members x)) rq)) " members");
		  
		  (map (lambda (me) 
					(dnl " " (representation me) ":" (name me) " (" me ")	@ "
						  (subjective-time me) " with " (members me))) rq)
		  (abort "Failed queue size test")
		  ))
  )


;; This is the main loop which runs agents and reinserts them after
;; they've executed.
;;
;; There is extra support for inter-agent communication and migration.
;;
;;     The queue doesn't (and *shouldn't*) care at all about how much
;;     time the agents used.

;;

(define (queue t stop runqueue . N)
  (set! N (if (null? N) #f (car N)))

  (let loop ((rq runqueue))
	 (cond
	  ((terminating-condition-test rq)
		(list 'terminated rq))
	  ((null? rq)
		'empty-queue)
	  ((and (list? rq) (symbol? (car rq)))
		rq)
	  ((file-exists? "halt")
		(delete-file "halt")
		(shutdown-agents rq)
		rq)
	  ((and (< t stop) (running-queue rq stop))
		(set! t (apply min (map-q subjective-time rq)))
		(test-queue-size rq N)
		(set! rq (run-agent t stop rq))
		(test-queue-size rq N)
			 (loop rq))
	  )
	 )
  )

;; converts the parameter vector "params" to reflect a new representation
(define (convert-params params rep)
  (let* ((newparams params)
			)
	 (list-set! newparams 3 rep)
	 (list-set! newparams 5 (if (eq? rep 'individual) 1 0))
	 newparams
	 ))


(define (distances-to what agentlist loc)
  (map (lambda (agent) 
			(if (and (procedure? agent)
						(eq? (representation agent) what))
				 (distance loc (location agent ))
				 2e308)
			)
		 agentlist)
  )

(define (distances-to-agents agentlist loc)
  (map (lambda (agent) 
			(if (procedure? agent)
				 (distance loc (location agent))
				 1e308)
			)
		 agentlist)
  )

(define (distances-to-populations agentlist loc)
  (distances-to 'population agentlist loc))


;; returns the index of the left-most value
(define (min-index n-list)
  (cond 
	((not (list? n-list)) 'not-a-list)
	((null? n-list) #f)
	((not (apply andf (map number? n-list)))
	 'Non-number-entry)
	(#t
	 (let* ((k (length n-list))
			  (result (let loop ((ix 0)
										(best #f)
										)
							(if (>= ix k)
								 best
								 (let ((n (list-ref n-list ix)))
									
									(cond 
									 ((infinite? n) ;; skip invalid entries
									  (loop (1+ ix) best))
									 ((and (number? n)
											 (or 
											  (not best)
											  (let ((b (list-ref n-list best)))
												 (or (infinite? b)
													  (and (number? best) (<= n b)))))
											 )
									  (loop (1+ ix) ix))
									 (#t (loop (1+ ix) best)))
									))) ))
		(if (or (not result) (infinite? result) (>= result 1e308))
			 #f
			 result)
		))
	)
  )

(define (kernel-call Q client query #!optional args)
  (cond
   ((procedure? query) ;; Do not return the client
	 (filter (lambda (x) (and (query x) (not (eq? x client)))) Q))
	 
   ((symbol? query)
    (cond
     ((eq? query 'time) (model-time Q +inf.0))
     ((eq? query 'agent-count) (length Q))
     ((eq? query 'next-agent) (if (null? Q) Q (car Q)))
	  ((eq? query 'min-time)
		(if (null? Q) 0 (apply min (map subjective-time Q))))
	  ((eq? query 'max-time)
		(if (null? Q) 0 (apply max (map subjective-time Q))))
	  ((eq? query 'mean-time)
		(if (null? Q)
			 0
			 (/ (apply + (map subjective-time Q)) (length Q))))
     (#t (abort 'kernel-call:not-defined)))
	 )
   (#t (abort 'kernel-call:bad-argument))
   )
  )



;; The agent function, "process",  must respond to the following things
;;    (snapshot agent)

;;    (i-am agent)
;;    (is-a agent)
;;    (representation agent)
;;    (name agent)
;;    (subjective-time agent)
;;    (dt agent)
;;    (parameters agent)

;;    (run-at agent t2)
;;    (run agent currenttime stoptime kernel)



;; The return values from agents fall into the following categories:

;; 	a symbol
;; 		is automatically inserted at the head of the queue and
;; 		execution is terminated (for debugging)

;; 	dt 
;; 		normal execution

;; 	(list 'introduce-new-agents dt list-of-new-agents)
;; 	   indicates that the agents in list-of-new-agents should be
;; 	   added to the simulation

;; 	(list 'remove-me dt)
;; 	   indicates that an agent should be removed from the simulation

;; 	(list 'migrate dt list-of-suggestions)
;; 	   the list-of-suggestions is so that an external assessment routine 

;; 	(list 'domain dt message-concering-domain-problem)
;; 		usually something like requests for greater resolution...

;; 	(list 'migrated dt)
;;      indicates that a model has changed its representation for some reason


(define (prep-agents Q start end . args)
  (kdnl* 'prep "Prepping from" start "to" end "    with" Q)
  (for-each
	(lambda (A)
	  (kdnl* 'prep "Prepping in lambda" (name A))
	  (let ((kernel (lambda x (apply kernel-call (append (list rq A) x ))))
				)
		 (kdnl* 'prep "Prepping in apply" (name A))
		 (apply agent-prep (append (list A start end kernel) args))
		 )
	  )
	Q)
  )

(define (shutdown-agents Q . args)
  (for-each
	(lambda (A)
	  (let* ((kernel (lambda x (apply kernel-call (append (list rq process) x))))
				)
		 (apply agent-shutdown (append (list A kernel) args))
		 )
	  )
	Q)
	)


;; Dispatches a call to the agent through the "run" routine. It also
;; handles special requests from the agent like mutation and spawning.
;; subjective time is set in  (run ...)
(define run-agent
  (let ((populist '())) ;; Remember the population list across
								;; invocations ... (equiv to a "static" in C,
								;; folks.)


	 (lambda (t stop rq . N) ;; This is the function "run-agent"
		(set! N (if (null? N) #f (car N)))

		(let* ((rq rq)
				 (process (if (and (not (null? rq)) (list? rq)) (car rq) #f)) 
				 ;; ... either false or the lambda to run
				 (agent-state (slot-ref process 'agent-state))
				 )

		  (or (eq? agent-state 'running)
				(eq? agent-state 'ready-to-run)
				(and (eq? agent-state 'ready-for-prep)
					  (abort (string-append
								 "Attempted to run " 
								 (symbol->string (class-name-of process)) ":"
								 (name process) " before it has been prepped")))
				(abort (string-append
						  "Attempted to run " 
						  (symbol->string (class-name-of process)) ":"(name process)
						  " when it is in the state " (object->string agent-state))))

		  (if process (kdnl* 'running "running" (name process) "at" t))

		  (test-queue-size rq N)
		  ;; remove the agent's run request from the top of the queue
		  (set! rq (remove process rq))
		  (test-queue-size rq N)
		  
		  (kdnl* 'run-agent "In run-agent")

		  (slot-set! process 'agent-body-ran #f) ;; Mark things as not
		  ;; having run through
		  ;; the body list
		  
		  ;; Here result should be a complex return value, not the
		  ;; number of ticks used.
		  (let* ((kernel
					 (lambda x (apply kernel-call (append (list rq process) x ))))
					
					(result (if (symbol? process) 
									'bad-runqueue
									(if (eq? agent-state 'suspended)
										 ;; A suspended agent "consumes" its
										 ;; time without doing anything, except
										 ;; update its subj. time.
										 (let ((dt (interval t
																	(slot-ref process 'dt) stop
																	(slot-ref process 'timestep-schedule))
													  )
												 (st (slot-ref process 'subjective-time)))
											'ok)
										 ;; If the thing queued is actually an
										 ;; agent, run the agent
										 (if (member <agent>
														 (class-cpl (class-of process)))
											  (let ((r (run process t stop kernel)))
												 (if lookit-running-names
													  (dnl (slot-ref process 'name)
															 " " (subjective-time process)
															 " " r))
												 (kdnl* 'run-agent "finished with run"
														  (name process) "@"
														  (subjective-time process) "+" r)
												 r
												 )
											  (begin
												 (dnl "Found a non-agent, dropping it.")
												 'not-an-agent)
											  )
										 )
									))
					)

			 (if (not (slot-ref process 'agent-body-ran))
				  (begin
					 (error
					  (string-append
						"The agent " (class-name-of process) ":" (name process)
						" failed to chain back to the base <agent> "
						"animal-classes.scm.\n"
						"This suggest that things have gone very wrong; "
						"either call (parent-body) or (skip-parent-body).")
					  )
					 'missed-model-body))
				  (let ()
					 (cond
					  ((eq? result 'ok)
						(set! rq (q-insert rq process Qcmp))
						rq)

					  ((number? result) 
						;; The result (in this case) is the amount of time used
						;; measured from the subjective-time of the agent.
						;; q-insert knows how to find out "when" the agent is,
						;; and will re-insert it correctly.  subjective-time is
						;; updated
						(abort "(run ...) returned a number rather than a state")
						(set! rq (q-insert rq process Qcmp)))

					  ((symbol? result) ;;----------------------------------------------
						(let ()
						  (dnl "Got " result)

						  (cond
							((eq? result 'remove)
							 rq)
							(else 
							 (cons result rq)))
						  ))
					  ((eq? result #!void)
						(let ((s
								 (string-append "A " (symbol->string
															 (class-name-of process))
													 " tried to return a void from its "
													 "model-body.  This is an error")))
						  ;;(Abort s)
						  (abort s)
						  ))
					  (#t
						(set! rq (q-insert rq process Qcmp)))
					  ((list? result)
						(case (car result)
						  
						  ;; Remove ===============================================================
						  ('remove
							rq
							) ;; end of the migration clause

						  ;; Migrate to a different model representation ==========================
						  ('migrate
							#f
							) ;; end of the migration clause

						  ;; insert spawned offspring into the
						  ;;system ** not implemented in make-entity
						  ('spawnlist ;;-----------------------------------------------------------
							#f
							)
						  (else 'boink)
						  ) ; case
						) ; cond clause
					  )
					 )
				  (test-queue-size rq N)
				  (kdnl* 'run-agent "Finished with run-agent" (name process)
							"@" (subjective-time process))
				  ;; *********** THIS IS NOT THE ONLY WAY TO DO THIS **************
				  ;; One might need to have a method that will take a kernelcall procedure from
				  ;; another agent if there is out-of-band activity that requires a kernelcall.
				  ;; This is the conservative option.
				  rq)
			 )
		  )
		)
	 )

(define nested-agents '())

;; Q is the preloaded run-queue
;; Start and End are numbers s.t. Start < End
(define (run-simulation Q Start End . close-up-shop) 
  (prep-agents Q Start End)

  (set! Q (queue Start End Q))

  ;; We don't shut down just now, we are still developing
  (if (not developing) (shutdown-agents Q))

  (if (and (not (null? close-up-shop)) (procedure? (car close-up-shop)))
      ((car close-up-shop)))
  )

(define (continue-simulation Q End . close-up-shop) 
  (set! Q (queue (subjective-time (car Q)) End Q))
  (if (and (not (null? close-up-shop)) (procedure? (car close-up-shop)))
      ((car close-up-shop)))
  )



;-  The End 

;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
