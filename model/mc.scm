;--
;	model-configuration.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2013.02.05
;		Location: odin:/home/gray/study/src/model.scm
;
;-  Code 


;(dump wally)(newline)
;(aborts "Incomplete initialisation is making things fail when it runs")

;- Define the model domain now

(load "loadem.scm")

(error "Nothing to see, here.  Move along.  Try the 'model' down the street")
(define start 0) ;; day zero
(define end 61) ;; end at some day after the start

(if (< end start) (error "The model doesn't work in that direction"))

(define A4domain (list 210 294 80)) ;; (x y z) corresponds to the size of an A4 page
(define mA4domain (list 178 250 80))
(define domain mA4domain)


;-- Global data ---------------------------------------------------------------

(define missing-value 0)  ;; things arrived at by mortality are likely
								  ;; to be inexact
(define Q '())            ;; This is the queue which holds the agents
								  ;; in the simulation

;(define end (* 365.25 32)) ;; simulate 32 years
;(define end (* 365.25 10)) ;; simulate 10 years
;(define end (* 365.25 6))  ;; simulate 6 years
(define end (* 365.25 4))  ;; simulate 4 years
;(define end (* 365.25 2))  ;; simulate 2 years

;-- Set kernel flags ---------------------------------------------------------

;; The kernel will emit messages (with kdnl*) which have a label which
;; matches something in the kernel-messages list

;(set! kernel-messages (append '(*) kernel-messages))

;; Indicate which agents are "nested"; as an example patches may be
;; present either as independent things or as components within a
;; habitat

;(set! nested-agents '(nested-habitat)) ;; No, each patch does its own thing....

(add-kernel-msg-tag 'introspection)
(add-kernel-msg-tag 'log-*)

;; options include focus stomach hunger-proximity eating log animal-running

;-- extensions to basic framework (more complex models)  ---------------------

;---- Load habitat support code

;;(load "habitat-support.scm")

;- Load the model properly ---------------------------------------------------

;-- load specific models -----------------------------------------------------

;; (make <landscape> ...)
;; (make <habitat> ...)
;; ...
;(append! Q ...)



"The following code takes the list of registered submodels and loads any files they may be 
dependent on.  Loggers must be loaded after the other submodels, so we take two passes."


;-- Code to run single agents for a little bit of time

(define play-with #f)
(define test-run #f)
(define Q? #f)

(letrec ((T 0)
			(Q '())
			(pw (lambda (A start end)
					(set! T start)
					(set! Q (if (pair? A) A (list A)))
					(prep-agents Q start end)
					))
			(tr (lambda (dt)
					(set! Q (queue T (+ T dt) Q))
					(set! T (+ T dt)))
				 )
			(q? (lambda x
					(if (null? x)
						 (map dump Q)
						 (for-each
						  (lambda (x)
							 (if (and (number? x) (< x (length Q)))
								  (dump (list-ref Q x))))
						  x))))
			)
  (set! play-with pw)
  (set! test-run tr)
  (set! Q? q?))

  
;-- Example code to run things....

(define (Doit q) ;; Run till end without pause.  If you are a penguin, run without paws.
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

(define PB (make-agent <patch> 'name "Bear" 'type 'area 'rep (make-object <polygon> 'locus '(0 0) 'perimeter (make-box '(-201 -201) '(201 201)))))
(slot-set! PB 'service-list '())

(for-each
 (lambda (q)
	(display "tree...")
	(let*  ((loc (list (- (* 400 (random-real)) 200) (- (* 400 (random-real)) 200)))
			(t (make-simple-plant-xy PB loc (+ 10 (* 30 (random-real)))))
			)
	  (dnl "!")
	  (set! Q (q-insert Q t Qcmp))
	  (set! trees (cons t trees))))
 
 (seq 20)
 )






;;; ;; This will be the runqueue!
;;; (define Q '());

;;; (dnl "Loading run queue")
;;; ;; Define a nice function to insert an agent into the runqueue
;;; (define (iQ agnt)
;;;   (set! Q (q-insert Q agnt Qcmp)))



;;; (iQ habitat) ;; Add the habitat..................
;;; (for-each iQ (slot-ref habitat  'patch-list)) ;; and its subsidiary agents


;;; ;; An introspection-list is a list of agents to be examined by a logging agent (in this case "logger")
;;; ;;(set-introspection-list! psdumper (list-copy Q))
;;; (set-introspection-list! logger (list-copy (service-list habitat)))


;;; ;; Tell each agent what spatial ordinate system their output should be in (if we don't do this,
;;; ;; it defaults to whatever they use internally) 

;;; (dnl "The queue has " (length Q) " entries")

;;; (if use-psdumper
;;; 	 (set! Q (cons psdumper Q))
;;; 	 (set! Q (cons logger Q))
;;; 	 )



;;; (definition-comment 'terminating-condition
;;;   "If this condition becomes true, (queue) bails out immediately. Recall that it is"
;;;   "(run-simulation) that forces a 'shutdown on agents."
;;;   "Often this will just be a lambda returning #f")
;;; (define terminating-condition-test
;;;   (let* ((tct terminating-condition-test) ;; chains from other terminating conditions
;;; 			(l (lambda (Q)
;;; 				  (and (tct Q)
;;; 						 (number? (slot-ref wally 'mass))
;;; 						 (number? (slot-ref wilma 'mass)))
;;; 				  )
;;; 				))
;;; 	 l))
















;;;======================================================================

(dnl "Run with (Doit Q) to run from the start to the end\n")
(dnl "Run with (doit Q n) to run for n days (not necessarily n ticks!)")
(dnl "             so you can run the next step in a similar fashion\n")
(dnl "Close up shop with (shutdown-agents Q) -- this closes files and things.")


(display "Loaded: ")
(apply dnl* (map (lambda (x) (slot-ref x 'name)) Q))

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
