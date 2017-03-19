; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	introspection-methods.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.12.29
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/introspection-methods.scm
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
(include "framework")

;; (agent-initialisation-method
;;  <introspection> (args) (no-default-variables)
;;  (set-state-variables
;;   self (list 'type 'introspection
;; 				 'priority introspection-priority
;; 				 'jiggle 0 'introspection-list '() 
;; 				 'timestep-epsilon 1e-6 'file #f
;; 				 'dont-log '(ready-for-prep
;; 								 ;; agent things
;; 								 agent-body-ran agent-schedule
;; 								 agent-epsilon map-projection counter 
;; 								 migration-test state-flags
;; 								 dont-log timestep-schedule kernel
								 
;; 								 ;; log agent things
;; 								 introspection-list introspection-schedule
;; 								 timestep-epsilon 

;; 								 dims ;; thing things

;; 								 ;; environment things
;; 								 default-value minv maxv 

;; 								 ;; ecoservice things
;; 								 plateau-interval growth-rate 

;; 								 ;; landscape things
;; 								 service-list service-update-map
;; 								 update-equations terrain-function
;; 								 dump-times scale 
;; 								 log-services-from-patch
;; 								 log-patches-from-habitat

;; 								 ;; animal things
;; 								 domain-attraction food-attraction 
;; 								 near-food-attraction searchspeed
;; 								 wanderspeed foragespeed	
;; 								 movementspeed foodlist homelist
;; 								 breedlist habitat
;; 								 )
;; 				 'variables-may-be-set #t
;; 				 ))
;;  (initialise-parent) ;; call "parents" last to make the
;;  ;; initialisation xxxxxxxxxxxxxxxxxxxblist work
;;  (set-state-variables self args)
;;  )

(model-method (<introspection> <number> <number>) (agent-prep self start end)
				  (agent-prep-parent self start end) ;; parents should prep first
				  )

(model-method <introspection> (agent-shutdown self #!rest args)
					 (agent-shutdown-parent)
					 )

(model-body <introspection>
				(kdnl* '(introspection-trace)
						 "[" (my 'name) ":" (class-name-of self) "]"
						 "Introspection: model-body")

				(let ((sched (my 'timestep-schedule))
						)

				  (set! dt (if (and (pair? sched) (< (car sched) (+ t dt)))
									(- (car sched) t)
									dt))

				  (kdnl* '(introspection-trace)
							"      list:     " (my 'introspection-list))
				  (kdnl* '(introspection-trace)
							"      schedule: "
							(list-head (my 'timestep-schedule) 3)
							(if (> (length (my 'timestep-schedule)) 3)
								 '... ""))
				  
				  (set-my! 'variables-may-be-set #f)

				  ;;(skip-parent-body)
				  (parent-body)
				  ;;(max dt (* 2.0 dt))
				  dt
				  ))

(define (exclude-voids lst)
  (filter (lambda (x) (not (equal? #!void x))) lst))

(model-method (<introspection> <agent>) (insert-agent! self target)
				  (set-my! 'introspection-list (exclude-voids  (cons target
																 (my 'introspection-list)))))

(model-method (<introspection> <agent>) (append-agent! self target)
				  (if (not (equal? target #!void))
						(set-my! 'introspection-list (exclude-voids (append (my 'introspection-list)
																		 (list target))))))

(model-method <introspection> (introspection-list self)
				  (my 'introspection-list))
(model-method <introspection> (introspection-times self)
				  (my 'timestep-schedule))

(model-method (<introspection> <list>) (set-introspection-list! self lst)
				  (set-my! 'introspection-list (exclude-voids lst)))
(model-method (<introspection> <list>) (set-introspection-times! self lst)
				  (set-my! 'timestep-schedule (exclude-voids lst)))

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
