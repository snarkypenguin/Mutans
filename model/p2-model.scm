; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	p2-model.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.07.25
;		Location: zero:/home/randall/Thesis/Example-Model/model/p2-model.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2016 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(define start 0)
(define end 741)


;;(define log-schedtimes 
(define log-schedtimes (append 
						  (cons 0 (seq 6))
						 (map (lambda (x) (* 10.0 (1+ x))) (seq (/ end 10))))
  ) ;; first six days, then on every tenth day from the beginning 


(define A4domain (list 210 294 80)) ;; (x y z) corresponds to the size of an A4 page
(define mA4domain (list 178 250 80))

(define domain mA4domain)

(set! nested-agents '(nested-habitat)) ;; No, each patch does its own thing....




;;; Not currently working
(define psdumper 
  (make <log-map> (list 'name "Map" 
								'format 'ps
								'timestep-schedule schedtimes 
								'filename "map-" 'filetype "0.ps"
							  )
		  ))

;; <log-data> is pretty forgiving, but at the expense of verbosity
;; <log-agent-table> insists that only one agent be logged
;; <log-table> insists that all the agents possess all the fields, 

(define logger 
  (make <log-data> (list 'name "Data" 
							  'timestep-schedule schedtimes 
							  'filename "Data"
							  'variables (list 'name 'subjective-time 'value)) ;; log-table does not automatically log the name at the front of the line
							  )
		  )






(define _terrain_ (plane 0 0.11 0.11))

(define habitat (make-habitat> "Kunlun" (list (list 0 0 300) (append A4domain (list 900)))
										 (lambda (x y) (abs (+  (* (- 120 x) (- x 60) (+ x 10))
																 (* (- y 120) (- (* y x) 560) (+ y 80))
																 )
													))
										 
							 'minv (0 0)
							 'maxv (cadr A4domain)
							 _terrain_ 'default-value 100
							 'patch-list (make-grid 3 3 '(0 0) '(210 294) <patch> <polygon> %patch-initialiser)))

(for-each (lambda (p)
				(set-services! p (list
										(simple-ecoservice "Trees" 't (+ 60 (+ 1 (* (random-real) 30))) ;; Current value trees
																	  (+ 200 (+ 1 (* (random-real) 60))) ;; Capacity
																	  1.0 ;; steepness of sigmoid
																	  (days 7) ;; max dt
																	  #t       ;; do growth
																	  'sigmoid p)
											 (simple-ecoservice "Fruit" 'f (+ 200 (+ 1 (* (random-real) 30))) ;; Current value fruit
																	  (+ 850 (+ 1 (* (random-real) 20))) ;; Capacity
																	  1.0 ;; steepness of sigmoid
																	  (days 7) ;; max dt
																	  #t       ;; do growth
																	  'sigmoid p)
											 (simple-ecoservice "Seeds" 's (+ 500 (+ 1 (* (random-real) 30))) ;; Current value seeds
																	  (+ 1200 (+ 1 (* (random-real) 20))) ;; Capacity
																	  1.0 ;; steepness of sigmoid
																	  (days 7) ;; max dt
																	  #t       ;; do growth
																	  'sigmoid p)
											 ))
				(slot-ref habitat 'patchlist)))

(define Q '());

;; This is a convenience macro -- mainly for use during model initialisation
;; This *only* works for a run-queue called Q
(define-macro (iQ agnt)
  `(set! Q (q-insert Q ,agnt Qcmp)))




(iQ habitat)
(for-each iQ (slot-ref habitat  'patchlist))

(set-introspection-list! psdumper (list-copy Q))
(set-introspection-list! logger (list-copy (service-list habitat)))


(for-each (lambda (x) (set-local-projection! x mm->points)) Q)

(if use-psdumper
	 (set! Q (cons psdumper Q))
	 (set! Q (cons logger Q))
	 )

(define terminating-condition-test
  (let* ((tct terminating-condition-test)
			(l (lambda (Q)
				  (and (tct Q) (number? (slot-ref wally 'mass)) (number? (slot-ref wilma 'mass)))
				  )
				))
	 l))
	 










;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
