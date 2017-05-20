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


;  
;  ***                                  *                      *     
;   *                                   *                      *     
;   *                                   *                      *     
;   *   * * **   * ***    ****   * **  ****    ****   * ***   ****   
;   *   ** *  *  **   *  *    *   *     *          *  **   *   *     
;   *   *  *  *  *    *  *    *   *     *      *****  *    *   *     
;   *   *  *  *  **   *  *    *   *     *     *    *  *    *   *     
;   *   *  *  *  * ***   *    *   *     *  *  *   **  *    *   *  *  
;  ***  *  *  *  *        ****    *      **    *** *  *    *    **   
;                *                                                   
;                *        

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

(include "framework")

(definition-comment 'lookit-running-names
  "This is a flag used when debugging; if true the name of the running agent is"
  "printed when it starts to run.")

(define lookit-running-names #f) ;; emits the name of the running
;; agent if true

(define monitors-monitor-themselves #t) ;; If false, monitors will ignore any other monitor

;(load "postscript.scm")
;;; This is loaded here since it may be used to produce all sorts of
;;; snapshot data spanning agents

;; These variables are used by the postscript.scm code
(define developing #t)
(define current-page #f)
(define current-page-number 0)

(define ACQ '()) ;; queue of things to insert before the next agent runs

(define keep-terminated-agents #t)
(define terminated-agents '())

(define (terminating-condition-test . args)
  #f)

(define valid-agent-states '(ready-for-prep ready-to-run running terminated dead suspended))

(define Mortuary '()) ;; a list of dead agents

;---------------------------------------------------
;               Kernel support
;---------------------------------------------------

;; remove an object from a list
(define (excise obj lst)
  (letrec ((head (list '*head*)))
	 (letrec ((inner-remove
				  (lambda (lst tail)
					 (cond ((null? lst)
							  lst)
							 ((not (pair? lst))
							  #f)
							 ((eqv? obj (car lst)) (inner-remove (cdr lst) tail))
							 (#t
							  (set-cdr! tail (list (car lst)))
							  (inner-remove (cdr lst) (cdr tail)))))))
		(inner-remove lst head))
	 (cdr head)))

(definition-comment 'Qcmp "Compares the subjective time of two agents -- this is a < style comparison")
(define (Qcmp r1 r2) ;; reducing the amount of time this takes will have a performance impact
  ;;(start-timer 'Qcmp)
  ;;(let ((result 
  (let ((st1 (slot-ref r1 'subjective-time))
		  (st2 (slot-ref r2 'subjective-time)))
	 (cond
	  ((> st1 st2) #f)
	  ((<= st1 st2) #t)
	  (#t (let ((p1 (priority r1))
					(p2 (priority r2)))
			  (cond
				((<= p1 p2) #f)
				((> p1 p2) #t)
				(#t (<= (jiggle r1)(jiggle r2)))
				)
			  )))
	 ))
;)
;;(stop-timer 'Qcmp)
	 ;;result)
;  )



(definition-comment 'map-q "applies a query function (like 'subjective-time') to members of a runqueue, q")
(define (map-q arg q)
  (map (lambda (s) (arg s)) q))


(definition-comment 'q-filter
  "uses 'filter' and the passed in predicate to return a subset" "of the list of agents, rq")
(define (q-filter qf-rq predicate)
  (filter (lambda (process) (predicate process)) qf-rq))

(definition-comment 'running-queue
  "This returns a boolean which indicates if 'rq' is a valid runqueue" "which has not finished running")
(define (running-queue rq-rq stop)
  (cond
	((null? rq-rq) #f)
	((not (pair? rq-rq)) #f)
	((not (list? rq-rq)) #f)
	((not (agent? (car rq-rq))) #f)
	(#t (let ((f (car rq-rq)))
			(< (slot-ref f 'subjective-time) stop)))))

(definition-comment 'model-time "returns the current 'now'  of the agent at the head of the queue (unused)")
(define (model-time mt-rq stop)
  (if (running-queue mt-rq stop)
		(if (or (null? mt-rq) (not (list? mt-rq)))
			 mt-rq
			 (let ((f (car mt-rq)))
				(slot-ref f 'subjective-time))
			 )
		'end-of-run
		)
  )

(definition-comment 'interval
  "returns an interval (tick-length) based on the current time, the"
  "desired tick length, the nominated end of the run and a list of"
  "target times")

(define (interval t ddt stopat tlist)
  ;; tlist is a sorted queue of times to run
  (kdebug 'kernel-scaffolding "Called interval, T =" t ", ddt =" ddt ", stops at" stopat ", tlist:" tlist)
  (if (< (- stopat t) ddt)
		(set! ddt (- stopat t)))

  ;; This could (and should) be a lot simpler.

  (cond
	((or (not tlist) (null? tlist)) ;; treat a false tlist as equivalent to '()
	 (kdebug 'kernel-scaffolding "no tlist")
	 ddt)
	((and (list? tlist) 
			(number? (car tlist))
			(= (car tlist) t)
			)
	 (kdebug 'kernel-scaffolding "t == (car tlist)")
	 (if (pair? (cdr tlist)) ;; if there is a ttr in the tlist which is closer than t+ddt...
		  (let ((p (- (cadr tlist) t)))
			 (< p ddt)
			 (kdebug 'kernel-scaffolding "(cadr tlist) < t+ddt")
		  (set! ddt p)))
	 ddt)
	((and (list? tlist) 
			(number? (car tlist))
			)
	 (let ((dt (- (car tlist) t)))
		(if (< dt ddt)
			 dt
			 ddt)))
	(else 'bad-time-to-run)))



(definition-comment 'insert@ "Returns the index AT which to insert")
(define (insert@ lst ob cmp) 
  (cond
	((null? lst) 0)
	((null? (cdr lst)) 1)	 ;; Singleton.
	(#t
	 (let insert-loop  ((m 0) (M (- (length lst) 1)))
		;;(kdebug 'queue-insertion (name ob)"at time" (slot-ref ob 'subjective-time) ": m =" m "M =" M)

		(let* ((m! (list-ref lst m))
				 (M! (list-ref lst M))
				 (H (truncate (/ (+ m M) 2)))
				 (H! (list-ref lst H))
				 (o<m (cmp ob m!))
				 (m<o (cmp m! ob))
				 (o<M (cmp ob M!))
				 (M<o (cmp M! ob))
				 (H<o (cmp H! ob))
				 (o<H (cmp ob H!))
				 )
		  (cond
			((= m M) m) ;; Must be so
			((and (= (+ m 1) M) o<M) M)
			(o<m m) ;; off the edge
			(M<o (+ 1 M)) ;; off the other edge
			((not (or o<m m<o)) m) ;; must be equal
			((not (or o<H H<o)) H) ;; must be equal
			((not (or o<M M<o)) M) ;; must be equal
			(o<H (insert-loop m H))
			(H<o (insert-loop H M))
			(#t (abort))))))
	)
  )

(definition-comment 'q-insert
  "inserts a run record in the right place in the queue Q")
(define kernel-time 0)

;; This returns the queue, and it must be called using the global variable Q like so: (set! Q (q-insert Q ...))
(define (q-insert Q rec reccmp)
  (if (memq (slot-ref rec 'agent-state) '(ready-to-run running suspended))
		(begin
		  (start-timer 'q-insert)
		  (let ((result 
					(cond
					 ((not (isa? rec <agent>)) (error "Passed a non-agent to q-insert" rec))
					 ((eq? (slot-ref rec 'agent-state) 'terminated)
					  (if keep-terminated-agents
							(set! terminated-agents (cons rec terminated-agents)))
					  Q)
					 (#t
					  (let ((j (slot-ref rec 'jiggle))
							  )
						 (if (pair? Q) (set! Q (excise rec Q)))
						 
						 (if (or (eq? j #t) (number? j))
							  (if (or (boolean? j) (and (positive? j) (< j 1.0)))
									(set-jiggle! rec (abs (random-real)))
									)
							  (begin
								 (set-jiggle! rec 0))) ;; Coerce non-numerics to zero
						 
						 (stop-timer 'q-insert)
						 (let ((ix (insert@ Q rec reccmp)))
							(start-timer 'q-insert)
							(if (number? ix)
								 (append (list-head Q ix) (cons rec (list-tail Q ix)))
								 (error "bad return from insert@" ix))))
					  )
					 ))
				  )
			 (stop-timer 'q-insert)
			 result))
		(if (memq (slot-ref rec 'agent-state) '(terminated dead))
			 (begin
				(set! Mortuary (cons rec Mortuary))
				Q)
			 (append Q (list rec))) ;; we stick them at the end of the list to keep them in the system
		))
		


;;; (define (q-mis-insert Q rec reccmp) ***
;;;   (error "This is probably buggered") ***
;;;   (if (eq? (slot-ref rec 'agent-state) 'ready-for-prep) ***
;;; 		(cons rec Q) ***
;;; 		(let ((lQ (length Q))) ***
;;; 		  (if (isa? rec <agent>) ***
;;; 				(let ((j (slot-ref rec 'jiggle)) ***
;;; 						(call-starts (cpu-time)) ***
;;; 						) ***
;;; 				  (if (pair? Q) (set! Q (excise rec Q))) ***
				  
;;; 				  (if (or (eq? j #t) (number? j)) ***
;;; 						(if (or (boolean? j) (and (positive? j) (< j 1.0))) ***
;;; 							 (set-jiggle! rec (abs (random-real))) ***
;;; 							 ) ***
;;; 						(begin ***
;;; 						  (set-jiggle! rec 0))) ;; Coerce non-numerics to zero ***
;;; 				  (if (or (< lQ 2) (sorted? Q reccmp)) ***
;;; 						(let ((ix (insert@ Q rec reccmp))) ***
;;; 						  (set! kernel-time (+ kernel-time (- (cpu-time) call-starts))) ***
;;; 						  (if (> ix lQ) ***
;;; 								(append Q (list rec)) ***
;;; 								(append (list-head Q ix) (list rec) (list-tail Q ix))) ***
;;; 						  ) ***
;;; 						(begin ***
;;; 						  (let* ((f (append Q (list rec))) ***
;;; 									(sf (sort f reccmp)) ***
;;; 									) ***
;;; 							 (set! kernel-time (+ kernel-time (- (cpu-time) call-starts))) ***
;;; 							 sf)) ***
;;; 						) ***
;;; 				  ) ***
;;; 				(begin ***
;;; 				  (dnl* "The item:" rec "was passed to be inserted into the runqueue.  Dropping it.") ***
;;; 				  (kdebug 'error "The item:" rec "was passed to be inserted into the runqueue.  Dropping it.") ***
;;; 				  Q) ***
;;; 				))) ***
;;;   ) ***



;---------------------------------------------------
;           Kernel -- the main loop
;---------------------------------------------------

(definition-comment 'test-queue-size
  "this is so we catch runaway growth")
(define (test-queue-size tqs-rq N)
  (if (and (number? N) (> (length tqs-rq) N))
		(begin
		  (dnl "There are " (length tqs-rq)
				 " entries in the runqueue when we expect at most " N);
		  (dnl "These entities total "
				 (apply + (map (lambda (x) (members x)) tqs-rq)) " members");
		  
		  (map (lambda (me) 
					(dnl " " (representation me) ":" (name me) " (" me ")	@ "
						  (slot-ref me 'subjective-time) " with " (members me))) tqs-rq)
		  (abort "Failed queue size test")
		  ))
  )


(definition-comment 'queue
  "This is the main loop which runs agents and reinserts them after"
  "they've executed. Typically one would begin the simulation by "
  "calling (queue start stop run-queue)."
  ""
  "(queue) dispatches control to an agent by a call to (run-agent ...)"
  ""
  "There is extra support for inter-agent communication and migration."
  "The queue doesn't (and *shouldn't*) care at all about how much"
  "time the agents used.")

(define indicate-progress #f)
;; If set to #f nothing is printed out to indicate progress
;; set to -1 (or any negative number) to make it chatter

;; This typically runs the agent at the head of the queue
(define (queue t stop runqueue #!rest N)
  (if (eq? indicate-progress #t) (set! indicate-progress -1))
  (set! N (if (null? N) #f (car N)))

  (start-timer 'queue)
  (let loop ((q-rq runqueue)
				 )
	 (if (pair? ACQ)
		  (begin
			 (set! q-rq (sort (append q-rq ACQ) Qcmp))
			 (set! ACQ '())
			 ))
	 (if (and indicate-progress (number? indicate-progress) (> t indicate-progress))
		  (begin
			 (set! indicate-progress t)
			 (display "t = ")
			 (display  (subjective-time (car q-rq)))
			 (display " ")
			 (display (string-append (number->string (round (* 100 (/ (subjective-time (car q-rq)) end)))) "%"))
			 (newline)
			 )
		  )
	 
	 (cond
	  ((terminating-condition-test q-rq)
		(list 'terminated q-rq))
	  ((null? q-rq)
		'empty-queue)
	  ((and (list? q-rq) (symbol? (car q-rq)))
		q-rq)
	  ((file-exists? "halt")
		(stop-timer 'queue)
		(delete-file "halt")
		(shutdown-agents q-rq)
		q-rq)
	  ((and (< t stop) (running-queue q-rq stop))
		(set! t (apply min (map-q subjective-time q-rq)))
		;;(test-queuesize q-rq N)
		(if (memq (slot-ref (car q-rq) 'agent-state) '(dead terminated))
			 (begin
				(dnl* "Terminating" (name (car q-rq))  (slot-ref (car q-rq) 'agent-state))
				(set! terminated-agents (cons (car q-rq) terminated-agents))
				(loop (cdr q-rq)))
			 (begin
				(stop-timer 'queue)
				(set! q-rq (run-agent t stop q-rq))
				(start-timer 'queue)
				(test-queue-size q-rq N)
				(loop q-rq)))
		)
	  )
	 )
  (stop-timer 'queue)
  )

(definition-comment 'convert-params "converts the parameter vector 'params' to reflect a new representation")
(define (convert-params params rep)
  (let* ((newparams params)
			)
	 (list-set! newparams 3 rep)
	 (list-set! newparams 5 (if (eqv? rep 'individual) 1 0))
	 newparams
	 ))


(definition-comment 'distances-to "Returns the nominal distance between agents of a given type and a location")
(define (distances-to what agentlist loc)
  (map (lambda (agent) 
			(if (and (procedure? agent)
						(eqv? (representation agent) what))
				 (distance loc (location agent ))
				 2e308)
			)
		 agentlist)
  )


(definition-comment 'distances-to-agents "Returns a list of nominal distance between a location and a members of a set of agents")
(define (distances-to-agents agentlist loc)
  (map (lambda (agent) 
			(if (procedure? agent)
				 (distance loc (location agent))
				 1e308)
			)
		 agentlist)
  )

(definition-comment 'distances-to "Returns the nominal distance between <population>s and a location")
(define (distances-to-populations agentlist loc)
  (distances-to 'population agentlist loc))


(definition-comment 'min-index "returns the index of the left-most (on the number line) value in n-list")
(define (min-index number-list)
  (let ((n (and (list? number-list) (pair? number-list) (apply andf (map number? number-list)) (apply min number-list))))
	 (let loop ((i 0)
					 (nl number-list))
		(cond
		 ((= (car nl) n) i)
		 ((null? nl) (error "bad result"))
		 (#t (loop (+ 1 i) (cdr n)))))))


;; Dunno *what* I was thinking :-)
;;; (define (Min-index n-list)
;;;   (cond 
;;; 	((not (list? n-list)) 'not-a-list)
;;; 	((null? n-list) #f)
;;; 	((not (apply andf (map number? n-list)))
;;; 	 'Non-number-entry)
;;; 	(#t
;;; 	 (let* ((k (length n-list))
;;; 			  (result (let loop ((ix 0)
;;; 										(best #f)
;;; 										)
;;; 							(if (>= ix k)
;;; 								 best
;;; 								 (let ((n (list-ref n-list ix)))
									
;;; 									(cond 
;;; 									 ((infinite? n) ;; skip invalid entries
;;; 									  (loop (1+ ix) best))
;;; 									 ((and (number? n)
;;; 											 (or 
;;; 											  (not best)
;;; 											  (let ((b (list-ref n-list best)))
;;; 												 (or (infinite? b)
;;; 													  (and (number? best) (<= n b)))))
;;; 											 )
;;; 									  (loop (1+ ix) ix))
;;; 									 (#t (loop (1+ ix) best)))
;;; 									))) ))
;;; 		(if (or (not result) (infinite? result) (>= result 1e308))
;;; 			 #f
;;; 			 result)
;;; 		))
;;; 	)
;;;   )



(define (agent-kcall me #!rest args)
  (if (null? args)
		(error "No query made to kernel through" me)
		(apply (slot-ref me 'kernel) args)))



(definition-comment 'kernel-call
  "This is the call into the kernel, agents can query for agent lists, agent count, times....
If the tag is a string it is silently converted to a symbol, *and* the argument is 
to the query is *NOT* unwrapped --- if args end up of the form ((....)), it stays that way!

Agents have a wrapper for this function--- kernel --- which makes calls simpler.  Using this 
wrapper from the repl might look like

> ((slot-ref (car Q) 'kernel) 'check)

though in the agent's code it would look like
        (kernel 'check)
)

since the function saved in the kernel slot captures the agent's closure.  More often it
will be invoked within an agents model-body like
   ...
	(for-each forage-if-coincident (kernel 'providers? 'vegetation))
   ...
or something like that.


NOTE PARTICULARLY: if the flag blue-meanie is set, agents can only access the kernel while 
their model-body is running.  
")

(define (kernel-call Q client query #!rest args)
  (if (null? args) (set! args #f))
  
  (if (string? query)
		(set! query (string->symbol query))
		(if (and (pair? args) (null? (cdr args)) (pair? (car args)) (= 1 (length args)))
			 (set! args (car args))) ;;
		)
  (cond
   ((and (or (eqv? client 'KERNEL) (isa? client <monitor>)) (procedure? query))
	 (if monitors-monitor-themselves
		  (filter query Q) ;; *can* return itself ... monitors can monitor monitors
		  ))
	
	;; The format expected is (kernel /target-agent/ /target-agent-class/  /method/ args....)
	((and args (pair? args) (isa?  query (car args)))
	 (apply (cadr args) (cons query (cddr query))))

   ((symbol? query)
    (case query
		((runqueue)
		 (if (or (eqv? client 'KERNEL) (isa? client <monitor>) (isa? client <introspection>))
			  Q
			  #f))
		((time) (model-time Q +inf.0)) ;; current time in the model (the subjective-time of the head of the queue)
		
		((acquire) ;; expects a list of agents to introduce into an agent's subsidiary list
		 ;; (kernel-call Q caller 'acquire host-agent active? agent-list)
		 (let ((okQ (filter (lambda (x) (isa? x <agent>)) (caddr args))))
			(apply acquire-agents args)
			))

		((check)
		 (list client 'mate))

		((check!)
		 (list client 'mate!))
		
		
		((find-agent)
		 (let ((type (car args))) 
			(filter (lambda (x)
						 (let ((xtype (slot-ref x 'type)))
							(or (eqv? xtype type)
								 (and (string? type)
										(string? xtype)
										(string=? type xtype))
								 (and (procedure? type)
										(type x)) 
								 )))
					  Q)))

		((locate)
		 (let ((location (car args))
				 (selector (if (pair? (cdr args)) (cadr args) #f))
				 (radius (if (pair? (cddr args)) (caddr args) #t)))
			(locate Q selector location radius))

		 )

		((shutdown)
		 (let ((A (if (pair? args) args (list args))))
			(shutdown-agents A) ;; WAS HAVING ISSUES WITH A SINGLE AGENT BEING PASSED
			(for-each (lambda (a) (set! Q (excise a Q))) A)
			))

		((remove) ;; expects a list of agents to be removed from the runqueue
		 (let ((A (if (pair? args) args (list args))))
			(for-each (lambda (a) (set! Q (excise a Q))) A)
			))
		((containing-agents) ;; returns a list of agents which report as containing any of the list of arguments
		 (filter (lambda (x) (i-contain x args)) Q))

		((providers?) ;; returns a list of agents which provide indicated things
		 (cond
		  ((and (pair? args) (pair? (car args)) (null? (cdr args))) (filter (lambda (x) (provides? x (car args))) Q))
		  ((and (pair? args) (pair? (car args)) (not (null? (cdr args))))
			(error "bad options passed in kernel call to providers?" args))
		  (#t (filter (lambda (x) (provides? x args)) Q)))
		 )	

		((resource-value) ;; returns the value of a resource from a nominated agent -- (kernel 'resource-value other 'seeds)
		 (if (apply provides? args)
			  (apply value args)
			  #f))

		((set-resource-value!) ;; sets the value of a resource from a nominated agent
		 (if (apply provides? args)
			  (or (apply set-value! args) #t)
			  #f))

		((add-resource-value!) ;; adds to the value of a resource from a nominated agent
		 (if (apply provides? args)
			  (or (apply add-value! args) #t)
			  #f))

		((agent-count) (length Q))
		((next-agent)
		 (if (or (eqv? client 'KERNEL) (isa? client <monitor>))
			  (if (null? Q) Q (car Q))
			  #f))
		((min-time)
		 (if (null? Q) 0 (apply min (map subjective-time Q))))
		((max-time)
		 (if (null? Q) 0 (apply max (map subjective-time Q))))
		((mean-time)
		 (if (null? Q)
			  0
			  (/ (apply + (map subjective-time Q)) (length Q))))
		(else (error "Unrecognised kernel-call request" query args)))
	 )
   (#t (error 'kernel-call:bad-argument))
   )
  )	



;; The agent function, "process",  must respond to the following things
;;    (snapshot agent)

;;    (i-am agent)
;;    (is-a agent)
;;    (representation agent)
;;    (name agent)
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


(definition-comment 'prep-agents
  "prepares agents in Q for running, particularly at the start of a simulation"
  "Both prep-agents and shutdown-agents make use of 'kernel-call' whose"
  "arguments are the runqueue, the agent making the request, and any"
  "arguments that might be required."
  )	

(define boink 'undone)

(define (prep-activate pa-rq q-entry)
  (kdebug 'prep "Prepping, in lambda")
  (set! boink q-entry)
  (if (isa? q-entry <agent>)
		(let ((kernel (lambda x (apply kernel-call (cons pq-rq (cons q-entry x )))))
				)
		  (kdebug 'prep "Prepping in apply" (name q-entry))
		  (slot-set! q-entry 'agent-state 'ready-to-run)
		  ;;(agent-prep q-entry start end)
		  )
		(and (kdebug 'complaint q-entry "is not an agent: cannot prep") #f)
		)
  )

(define (prep-agents Q start end)
  (kdebug 'prep "Prepping from" start "to" end "    with" Q)
  (set! Q (sort Q Qcmp))
  (kdebug 'prep "sorted queue")
  
  ;;  (dnl* "Prepping from" start "to" end)
;(pp (map (lambda (x) (cons (name x) (slot-ref x 'agent-state))) Q))
  
  (for-each (lambda (x) (prep-activate Q x)) Q)
  )

(definition-comment 'shutdown-agents "Tells each agent in Q to shutdown")
(define (shutdown-agents Q . args)
  (for-each
	(lambda (A)
	  (let* ((kernel (lambda x (apply kernel-call (cons Q (cons A x)))))
				)
		 (apply agent-shutdown (cons A (cons kernel args)))
		 )
	  )
	Q)
  )

(define location-tree  '())
(define split-at 16) ;; bisects cell when there are 16 entities or more
(define split-flexibly #f) ;; Not implemented yet -- will allow each subdivision to be unequal in area
(define use-list-for-locate #t)

(define (locate Q sel location #!rest radius)
  (let ((selector (cond
						 ((not sel) (lambda (x) #t)) ;; return all candidates
						 ((procedure? sel) sel)
						 (#t (error "Bad selector passed to locate" sel)))))
	 (if use-list-for-locate
		  (filter (lambda (x)
						(if (isa? x <thing>)
							 (and (selector x) (or (eq? radius #t) (<= (distance x location) radius)))
							 #f)
						)
					 Q)
		  (letrec ((traverse
						(lambda (node)
						  (cond
							((null? node)
							 '())
							((= (length node) 3) ;; bottom
							 (if (point-in-polygon location (bbox (car node) (cadr node)))
								  node
								  #f))
							((= (length node) 6) ;; bottom
							 (if (not (point-in-polygon location (bbox (car node) (cadr node))))
								  #f
								  (let* ((q (map traverse (cddr node)))
											(qr (filter (lambda (x) x) q)))
									 (if (null? qr)
										  '()
										  (car qr)))))
							(#t (error "bad traversal in environment locate call" node location))))))
			 (traverse location-tree))
		  )
		))


(define (add-thing-to-location self entity location)
  (UNFINISHED-BUSINESS "Need to flesh this out")
  #t
  )

(define (remove-thing-from-location  entity location)
  (UNFINISHED-BUSINESS "Need to flesh this out")
  )
(define (split-location-tree)
  (UNFINISHED-BUSINESS "Need to flesh this out")
  #t)
			


(definition-comment 'run-agent
  "Dispatches a call to the agent through the 'run' routine. It also"
  "handles special requests from the agent like mutation and spawning."
  "subjective time is set in  (run ...)")
(define run-agent
  (let ((populist '())
		  )
	 ;; Remember the population list across invocations ... (equiv to a "static" in C, folks.)

	 (lambda (t stop run-agent-runqueue . N) ;; This is the function "run-agent"
		(set! N (if (null? N) #f (car N)))

		(cond
		 ((member (slot-ref (car run-agent-runqueue) 'agent-state) '(terminated dead))
		  (if keep-terminated-agents
				(set! terminated-agents (cons (car run-agent-runqueue) terminated-agents)))
		  (cdr run-agent-runqueue)
		  )
		 (#t (let* ((local-run-agent-runqueue run-agent-runqueue)
					  (process (if (and (not (null? local-run-agent-runqueue)) (list? local-run-agent-runqueue)) (car local-run-agent-runqueue) #f)) 
					  ;; ... either false or the lambda to run
					  (agent-state (slot-ref process 'agent-state))
					  )
				 (or (eqv? agent-state 'running)
					  (eqv? agent-state 'ready-to-run)
					  (and (eqv? agent-state 'ready-for-prep)
							 (abort (string-append
										"Attempted to run " 
										(cnc process) ":"
										(name process) " before it has been prepped")))
					  (abort (string-append
								 "Attempted to run " 
								 (cnc process) ":"(name process)
								 " when it is in the state " (object->string agent-state))))

				 (if process (kdebug 'running "running" (name process) "at" t))

				 (test-queue-size local-run-agent-runqueue N)
				 ;; remove the agent's run request from the top of the queue
				 (set! local-run-agent-runqueue (excise process local-run-agent-runqueue))
				 (test-queue-size local-run-agent-runqueue N)
				 
				 (kdebug 'run-agent "In run-agent")

				 (slot-set! process 'agent-body-ran #f)
				 ;; Mark things as not having run through the body list
				 
				 ;; Here result should be a complex return value, not the
				 ;; number of ticks used.
				 (let* ((kernel
							(lambda x (apply kernel-call (cons local-run-agent-runqueue (cons process x))))
							)
						  
						  (result (cond
									  ((symbol? process)  'bad-runqueue)
									  ((eqv? agent-state 'suspended)
										;; A suspended agent "consumes" its
										;; time without doing anything, except
										;; update its subj. time.
										(let ((dt (interval t (slot-ref process 'dt) stop (slot-ref process 'timestep-schedule)))
												(st (slot-ref process 'subjective-time)))
										  'ok)
										)
									  ;; If the thing queued is actually an
									  ;; agent, run the agent
									  ((isa? process <agent>) ;; equivalent to (member <agent> (class-cpl (class-of process)))
										(let ((r (run process t stop kernel))) ;; (run ...) is in framework-methods.scm
										  (if (or lookit-running-names (kdebug? 'timing))
												(dnl* (slot-ref process 'name)
														(slot-ref process 'subjective-time)
														r))
										  (kdebug 'run-agent "finished running "
													 (name process) "@"
													 (slot-ref process 'subjective-time) "+" r)
										  r)
										)
									  (#t (dnl "Found a non-agent, dropping it.")
											'not-an-agent)
									  )
									 )
						  )
				 
				 (if (not (slot-ref process 'agent-body-ran))
					  (begin
						 (error
						  (string-append
							"The agent " (cnc process) ":" (name process)
							" failed to chain back to the base <agent> "
							"model-body.\n"
							"This suggest that things have gone very wrong; "
							"check that there is a call like (call-next-parent-body).")
						  )
						 'missed-model-body)
					  (slot-set! process 'runcount (+ (slot-ref process 'runcount) 1))
					  )
				 (let ()
					(cond
					 ((eqv? result 'ok)
					  (set! local-run-agent-runqueue (q-insert local-run-agent-runqueue process Qcmp))
					  local-run-agent-runqueue)

					 ((number? result) 
					  ;; The result (in this case) is the amount of time used
					  ;; measured from the subjective-time of the agent.
					  ;; q-insert knows how to find out "when" the agent is,
					  ;; and will re-insert it correctly.  subjective-time is
					  ;; updated
					  (abort "(run ...) returned a number rather than a state")
					  (set! local-run-agent-runqueue (q-insert local-run-agent-runqueue process Qcmp)))

					 ((symbol? result) ;;----------------------------------------------
					  (let ()
						 (dnl "Got " result)

						 (cond
						  ((eqv? result 'remove)
							local-run-agent-runqueue)
						  (else 
							(cons result local-run-agent-runqueue)))
						 ))
					 ((eqv? result #!void)
					  (let ((s
								(string-append "A " (cnc process)
													" tried to return a void from its "
													"model-body.  This is an error")))
						 ;;(Abort s)
						 (abort s)
						 ))
					 (#t
					  (set! local-run-agent-runqueue (q-insert local-run-agent-runqueue process Qcmp)))
					 ((list? result)
					  (case (car result)
						 
						 ;; Remove ===============================================================
						 ('remove
						  local-run-agent-runqueue
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
				 (test-queue-size local-run-agent-runqueue N)
				 (kdebug 'run-agent "Finished with run-agent" (name process)
							"@" (slot-ref process 'subjective-time))
				 ;; *********** THIS IS NOT THE ONLY WAY TO DO THIS **************
				 ;; One might need to have a method that will take a kernelcall procedure from
				 ;; another agent if there is out-of-band activity that requires a kernelcall.
				 ;; This is the conservative option.
				 local-run-agent-runqueue)
				 )
			  )
		 )
		)
	 )
  )


(definition-comment 'run-simulation
  "Q is the preloaded run-queue"
  "Start and End are numbers s.t. Start < End")
(define (run-simulation Q Start End . close-up-shop) 
  (prep-agents Q Start End)

  (set! Q (queue Start End Q))

  ;; We don't shut down just now, we are still developing
  (if (not developing) (shutdown-agents Q))

  (if (and (not (null? close-up-shop)) (procedure? (car close-up-shop)))
      ((car close-up-shop)))
  )

(definition-comment 'continue-simulation
  "Q is the currently running list of agents (run-queue)"
  "End is a number that indicates the end of everything are numbers s.t. Start < End"
  "close-up-shop is a procedure taking no arguments that may be run at the end.")
(define (continue-simulation Q End . close-up-shop) 
  (set! Q (queue (slot-ref (car Q) 'subjective-time) End Q))
  (if (and (not (null? close-up-shop)) (procedure? (car close-up-shop)))
      ((car close-up-shop)))
  )



;-  The End 

;;; Local Variables: 
;;; mode: scheme
;;; outline-regexp: ";-+" 
;;; comment-column:0
;;; comment-start: ";;; "  
;;; comment-end:"\n" 
;;; End: 
