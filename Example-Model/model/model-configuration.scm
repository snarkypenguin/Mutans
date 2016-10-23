;--
;	model.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2013.02.05
;		Location: odin:/home/gray/study/src/model.scm
;
;-  Code 

;--- logging

;(add-kernel-msg-tag '*)

;(add-kernel-msg-tag 'introspection) ;; all introspection agents
;(add-kernel-msg-tag 'log-*) ;; all log messages
(add-kernel-msg-tag 'running) ;; messages to track running
(add-kernel-msg-tag 'run-agent) ;; messages indicating that "agent" code is running

(display "Execute (kdnl*-info) to get more info\n")

;(dump wally)(newline)
;(aborts "Incomplete initialisation is making things fail when it runs")

;- Define the model domain now


;-- Temporal
(define start 0) ;; day zero
(define end 61) ;; end at some day after the start

(if (< end start) (error "+++Entropy Error+++  The model doesn't work in that direction"))

;--- Set scheduled tick times.
;;Scheduled dump times for the logger: by default the first six days,
;;then every tenth day from the start.  I should make it so that it
;;can be a function rather than a list.
(define N-days 740)
(define schedtimes (unique
						  (sort
							 (cons 0 (seq end))
							<)))
;-- Spatial

(define A4domain (list 210 294 80)) ;; (x y z) corresponds to the size of an A4 page
(define mA4domain (list 178 250 80))

(define domain mA4domain)


;- Global data ---------------------------------------------------------------

(define missing-value 0)  ;; things arrived at by mortality are likely
								  ;; to be inexact
(define Q '())            ;; This is the queue which holds the agents
								  ;; in the simulation

;(define end (* 365.25 32)) ;; simulate 32 years
;(define end (* 365.25 10)) ;; simulate 10 years
;(define end (* 365.25 6))  ;; simulate 6 years
(define end (* 365.25 4))  ;; simulate 4 years
;(define end (* 365.25 2))  ;; simulate 2 years

(define record-ecoservice-history #t) ;; track ecoservice changes in a
												  ;; list

(define (filter-hist-by-patch s d)
  (filter (lambda (x) (string=? (car x) s)) d))


;-- Set kernel flags ---------------------------------------------------------

;; The kernel will emit messages (with kdnl*) which have a label which
;; matches something in the kernel-msg-tags list

;(set! kernel-msg-tags (append '(*) kernel-msg-tags))


;- Indicate any nesting
;; As an example patches may be present either as independent things or 
;; as components within a habitat

;(set! nested-agents '(nested-habitat)) ;; No, each patch does its own thing....



(load-submodels)

;-- Example code to run things....

(define (Doit q) ;; Run till end without pause
  (if #f
		(check-service-data-lists service-name-list
										  service-type-list service-eqn-sym-list))
  (prep-agents q start end)
  (set! q (queue start end q))
  )


(define Dunnit #f)
(define *dunnit* #f)

(define (doit q . n)
  (set! Dunnit (lambda () (shutdown-agents q)))
  (set! n (if (pair? n) (car n) 1))
  (if (not *dunnit*) (begin (prep-agents q start end) (set! *dunnit* 0)))
  (set! q (queue *dunnit* (+ *dunnit* n) q))
  (set! *dunnit* (+ *dunnit* n))
  )



;-- nominate the models to include

(define use-psdumper #f)  ;;; Not currently working
;;; (define psdumper 
;;;   (make <log-map> (list 'name "Map" 
;;; 								'format 'ps
;;; 								'timestep-schedule schedtimes 
;;; 								'filename "map-" 'filetype "0.ps"
;;; 							  )
;;; 		  ))

;; <log-data> is pretty forgiving, but at the expense of verbosity
;; <log-agent-table> insists that only one agent be logged
;; <log-table> insists that all the agents possess all the fields, 

(define logger 
  (make <log-data> (list 'name "Data" 
							  'timestep-schedule schedtimes 
							  'filename "Data"
							  'variables (list 'name 'subjective-time 'value))
;; log-table does not automatically log the name at the front of the line
							  )
		  )


(define habitat
  (make-habitat "Kunlun" 300 (list (list 0 0 300)
											  (append A4domain (list 900)))
					 (lambda (x y)
						(abs (+  (* (- 120 x)
										(- x 60)
										(+ x 10))
									(* (- y 120)
										(- (* y x)
											560)
										(+ y 80))
									)
											  ))
					 (make-grid 3 3 '(0 0) A4domain
									<patch> <polygon> "kunlun"
									'local-landscape %patch-initialiser)))

(define trees '())

(for-each
 (lambda (p)
	(slot-set!
	 p
	 'service-list
	 (list
	  (simple-ecoservice "Trees" 't (+ 60 (+ 1 (* (random-real) 30))) ;; value
								(+ 200 (+ 1 (* (random-real) 60))) ;; Capacity
								1.0 ;; steepness of sigmoid
								(days 7) ;; max dt
								#t       ;; do growth
								'sigmoid p)
	  (simple-ecoservice "Fruit" 'f (+ 200 (+ 1 (* (random-real) 30))) ;; value
								(+ 850 (+ 1 (* (random-real) 20))) ;; Capacity
								1.0 ;; steepness of sigmoid
								(days 7) ;; max dt
								#t       ;; do growth
								'sigmoid p)
	  (simple-ecoservice "Seeds" 's (+ 500 (+ 1 (* (random-real) 30))) ;; value
								(+ 1200 (+ 1 (* (random-real) 20))) ;; Capacity
								1.0 ;; steepness of sigmoid
								(days 7) ;; max dt
								#t       ;; do growth
								'sigmoid p)
	  (for-each
		(lambda (q)
		  (let ((t (make-simple-plant p (+ 10 (* 30 (random-real))))))
			 (set! Q (q-insert Q t Qcmp))
			 (set! trees (cons t trees))))
		(seq 4))
	  )))
 (slot-ref habitat 'patch-list))





;; This will be the runqueue!
(define Q '());

;; Define a nice function to insert an agent into the runqueue
(define (iQ agnt)
  (set! Q (q-insert Q agnt Qcmp)))



(iQ habitat) ;; Add the habitat..................
(for-each iQ (slot-ref habitat  'patch-list)) ;; and its subsidiary agents


;; An introspection-list is a list of agents to be examined by a logging agent (in this case "logger")
;;(set-introspection-list! psdumper (copy-list Q))
(set-introspection-list! logger (copy-list (service-list habitat)))


;; Tell each agent what spatial ordinate system their output should be in (if we don't do this,
;; it defaults to whatever they use internally) 
(for-each (lambda (x) (set-map-projection! x mm->points)) Q) 

(if use-psdumper
	 (set! Q (cons psdumper Q))
	 (set! Q (cons logger Q))
	 )



(definition-comment 'terminating-condition
  "If this condition becomes true, (queue) bails out immediately. Recall that it is"
  "(run-simulation) that forces a 'shutdown on agents."
  "Often this will just be a lambda returning #f")

(define terminating-condition-test
  (let* ((tct terminating-condition-test) ;; chains from other terminating conditions
			(l (lambda (Q)
				  (and (tct Q)
						 #f
				  )
				)))
	 l))
















;;;======================================================================

(dnl "Run with (Doit Q) to run from the start to the end\n")
(dnl "Run with (doit Q n) to run for n days (not necessarily n ticks!)")
(dnl "             so you can run the next step in a similar fashion\n")
(dnl "Close up shop with (shutdown-agents Q) -- this closes files and things.")


;(display "Loaded: ")
;(apply dnl* (map (lambda (x) (slot-ref x 'name)) Q))

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
