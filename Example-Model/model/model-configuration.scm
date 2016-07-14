;-  Identification and Changes

;--
;	model.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2013.02.05
;		Location: odin:/home/gray/study/src/model.scm
;
;-  Code 


;(dump wally)(newline)
;(aborts "Incomplete initialisation is making things fail when it runs")

;- Define the model now

;-- nominate the models to include



;-- Global data -------------------------------------------------------------------------------------------

(define missing-value 0)  ;; things arrived at by mortality are likely to be inexact
(define Q '())            ;; This is the queue which holds the agents in the simulation

(define start 0)          ;; start at day zero

(define end (* 365.25 32)) ;; simulate 32 years
;(define end (* 365.25 10)) ;; simulate 10 years
;(define end (* 365.25 6))  ;; simulate 6 years
(define end (* 365.25 4))  ;; simulate 4 years
;(define end (* 365.25 2))  ;; simulate 2 years

(define record-ecoservice-history #t) ;; track ecoservice changes in a list
(define (filter-hist-by-patch s d) (filter (lambda (x) (string=? (car x) s)) d))


(define A4domain (list 210 294 80))  ;; (x y z) corresponds to the size of an A4 page
(define mA4domain (list 178 250 80)) ;; Model domain (x-size y-size z-size)

(define domain mA4domain)

;--- Set scheduled tick times.
;;Scheduled dump times for the logger:  by default the first six days, then every tenth day from the start.
;; I should make it so that it can be a function rather than a list.
(define schedtimes (append 
						 (cons 0 (seq 6))
						 (map (lambda (x) (* 10.0 (1+ x))) (seq 7400))
  ) ;; first six days, then on every tenth day from the beginning for 74000 days
)

;-- Set kernel flags --------------------------------------------------------------------------------------

;; The kernel will emit messages (with kdnl*) which have a label which matches something in 
;; the kernel-messages list 

;(set! kernel-messages (append '(*) kernel-messages))

;; Indicate which agents are "nested"; as an example patches may be
;; present either as independent things or as components within a
;; habitat

(set! nested-agents '(nested-habitat)) ;; No, each patch does its own thing....

;(set! kernel-messages (append kernel-messages '(*)))
;(set! kernel-messages (append kernel-messages '(running  log-*)))
;(set! kernel-messages (append kernel-messages '(running focus stomach hunger-proximity eating)))
;(set! kernel-messages (append kernel-messages '(running animal-running focus log)))
;(set! kernel-messages (append kernel-messages '(running focus stomach hunger-proximity eating)))
;(set! kernel-messages (append kernel-messages '(animal-running stomach hunger-proximity eating)))


;-- extensions to basic framework (more complex models)  --------------------------------------------------

;---- Load habitat support code

;;(load "habitat-support.scm")

;- Load the model properly --------------------------------------------------------------------------------

;-- load specific models ----------------------------------------------------------------------------------

;; (make <landscape> ...)
;; (make <habitat> ...)
;; ...
;(append! Q ...)


(let ((submodel-files (apply append
									(map
									 (lambda (x) (let ((l (assoc x submodel-register)))
														(if l
															 (cdr l)
															 '())
														))
									 logger-tags)))
		)
  
  (for-each (if #t
					 load 
					 (lambda (x)
						(display "loading submodel: ")
						(display x)
						(newline)
						(load x))
					 )
				submodel-files)
  )


;;; loggers get inserted at the head of the queue

(let ((logger-files (apply append
									(map
									 (lambda (x) (let ((l (assoc x submodel-register)))
														(cdr l)
														'())
														)
									 logger-tags))))
  (if (not (apply andf (map file-exists? logger-files)))
		(begin
		  (dnl "The files " (!filter file-exists? logger-files) " do not exist.")
		  (abort "missing files")))

  
  (for-each 
	(if #f
		 load
		 (lambda (x)
			(display "loading logger: ")
			(display x)
			(newline)
			(load x)))
		 logger-files)
  )


;-- Example code to run things....

(dnl "Run with (Doit Q) to run from the start to the end\n")
(dnl "Run with (doit Q n) to run for n days (not necessarily n ticks!)\nso you can run the next step in a similar fashion\n")
(dnl "Close up shop with (shutdown-agents Q) -- this closes files and things.")

(define (Doit q) ;; Run till end without paus
  ;;(check-service-data-lists service-name-list service-type-list service-eqn-sym-list)
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





;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
