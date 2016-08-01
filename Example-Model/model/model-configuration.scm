
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

;- Define the model domain now


(define start 0) ;; day zero
(define end 741) ;; end at some day after the start

(if (< end start) (error "The model doesn't work in that direction"))

;;(define log-schedtimes 
(define log-schedtimes (append 
						  (cons 0 (seq 6))
						 (map (lambda (x) (* 10.0 (1+ x))) (seq (/ end 10))))
  ) ;; first six days, then on every tenth day from the beginning 


(define A4domain (list 210 294 80)) ;; (x y z) corresponds to the size of an A4 page
(define mA4domain (list 178 250 80))

(define domain mA4domain)

(set! nested-agents '(nested-habitat)) ;; No, each patch does its own thing....


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

(define record-ecoservice-history #t) ;; track ecoservice changes in a
												  ;; list

(define (filter-hist-by-patch s d)
  (filter (lambda (x) (string=? (car x) s)) d))


(define A4domain (list 210 294 80))  ;; (x y z) corresponds to the
												 ;; size of an A4 page
(define mA4domain (list 178 250 80)) ;; Model domain (x-size y-size
												 ;; z-size)

(define domain mA4domain)

;--- Set scheduled tick times.
;;Scheduled dump times for the logger: by default the first six days,
;;then every tenth day from the start.  I should make it so that it
;;can be a function rather than a list.
(define schedtimes (append 
						 (cons 0 (seq 6))
						 (map (lambda (x) (* 10.0 (1+ x))) (seq 7400))
  ) ;; first six days, then on every tenth day from the beginning for
	 ;; 74000 days
)

;-- Set kernel flags ---------------------------------------------------------

;; The kernel will emit messages (with kdnl*) which have a label which
;; matches something in the kernel-messages list

;(set! kernel-messages (append '(*) kernel-messages))

;; Indicate which agents are "nested"; as an example patches may be
;; present either as independent things or as components within a
;; habitat

(set! nested-agents '(nested-habitat)) ;; No, each patch does its own thing....

(add-kernel-message! 'introspection)
(add-kernel-message! 'log-*)

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

(dnl "Registered submodels: " submodel-register)

(let ((submodel-files
		 (!filter
		  null?
		  (map
			cdr
			(!filter
			 null?
			 (!filter
			  (lambda (x) (member (car x) logger-tags))
			  submodel-register))))
		 ))

  (if (pair? submodel-files)
		(begin 
		  (dnl "Submodels: " submodel-files)
		  (for-each (if #t
							 load 
							 (lambda (x)
								(display "loading submodel: ")
								(display x)
								(newline)
								(load x))
							 )
						submodel-files))
		(dnl "No submodel files to be loaded"))
  )


;;; loggers get inserted at the head of the queue

(let ((logger-files
		 (!filter
		  null?
		  (map
			cdr
			(!filter
			 null?
			 (filter
			  (lambda (x) (member (car x) logger-tags))
			  submodel-register))))
		 ))

  (if (pair? logger-files)
		(begin 
		  (dnl "Loggers: " logger-files)
		  (for-each (if #t
							 load 
							 (lambda (x)
								(display "loading logger: ")
								(display x)
								(newline)
								(load x))
							 )
						logger-files))
		(dnl "No logger files to be loaded"))
  )


;-- Example code to run things....

(define (Doit q) ;; Run till end without paus
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



;;; Not currently working
;(define psdumper 
;  (make <log-map> (list 'name "Map" 
;								'format 'ps
;								'timestep-schedule schedtimes 
;								'filename "map-" 'filetype "0.ps"
;							  )
;		  ))

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
					 (make-grid 3 3 '(0 0) '(210 294)
									<patch> <polygon> "kunlun"
									'environs %patch-initialiser)))

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
	  )))
 (slot-ref habitat 'patch-list))

(define Q '());

(define (iQ agnt)
  (set! Q (q-insert Q agnt Qcmp)))



(iQ habitat)
(for-each iQ (slot-ref habitat  'patch-list))

;;(set-introspection-list! psdumper (copy-list Q))

(set-introspection-list! logger (copy-list (service-list habitat)))


(for-each (lambda (x) (set-map-projection! x mm->points)) Q)

;;(if use-psdumper
;;	 (set! Q (cons psdumper Q))
	 (set! Q (cons logger Q))
;;	 )

(define terminating-condition-test
  (let* ((tct terminating-condition-test)
			(l (lambda (Q)
				  (and (tct Q)
						 (number? (slot-ref wally 'mass))
						 (number? (slot-ref wilma 'mass)))
				  )
				))
	 l))
















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
