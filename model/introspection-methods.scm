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

"
    Copyright 2017 Randall Gray

    This file is part of Remodel.

    Remodel is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Remodel is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Remodel.  If not, see <http://www.gnu.org/licenses/>.
"
;
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
;; 				 'jiggle 0 'introspection-targets '() 
;; 				 'timestep-epsilon 1e-6 'file #f
;; 				 'dont-log '(ready-for-prep
;; 								 ;; agent things
;; 								 agent-body-ran 
;; 								 agent-epsilon counter 
;; 								 migration-test state-flags
;; 								 dont-log timestep-schedule kernel
								 
;; 								 ;; log agent things
;; 								 introspection-targets
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
;; 								 near-food-attraction search-speed
;; 								 wander-speed forage-speed	
;; 								 movement-speed foodlist homelist
;; 								 breedlist habitat
;; 								 )
;; 				 'variables-may-be-set #t
;; 				 ))
;;  (parent-initialise) ;; call "parents" last to make the
;;  ;; initialisation xxxxxxxxxxxxxxxxxxxblist work
;;  (set-state-variables self args)
;;  )

(model-method (<introspection> <number> <number>) (agent-prep self start end)
				  (parent-agent-prep self start end) ;; parents should prep first
				  #t
				  )

(model-method <introspection> (agent-shutdown self #!rest args)
					 (parent-agent-shutdown)
					 )

(model-method (<introspection>) (my-list self)
				  (let ((mit (my 'introspection-targets))
						  (Q (agent-kcall self 'runqueue))) ;; This is how an agent would usually call the kernel 

					 (sortless-unique
					  (letrec ((loop (lambda (mitr mlist)
											 (cond
											  ((null? mitr) mlist)
											  ((isa? (car mitr) <agent>) (loop (cdr mitr) (cons (car mitr) mlist)))
											  ((class? (car mitr)) (loop (cdr mitr) (append (filter (*is-class? (car mitr)) Q) mlist)))
											  ((string? (car mitr)) (loop (cdr mitr) (append (filter (*is-taxon? (car mitr)) Q) mlist)))
											  ((symbol? (car mitr)) (loop (cdr mitr) (append (filter (*has-slot? (car mitr)) Q) mlist)))
											  ((procedure? (car mitr)) (loop (cdr mitr) (append (filter procedure Q) mlist)))
											  (#t (error "args to my-list must be agents, strings, symbols classes or procedures" (car mlist)))))
										  ))
						 (loop mit '())))
					 )
				  )


(model-body <introspection>
				(kdebug '(introspection-trace)
						 "[" (my 'name) ":" (cnc self) " at " (subjective-time self) "]"
						 "Introspection: model-body")


				(let ((sched (my 'timestep-schedule))
						)

				  (set! dt (if (and (pair? sched) (< (car sched) (+ t dt)))
									(- (car sched) t)
									dt))

				  (kdebug '(introspection-trace)
							"      list:     " (my 'introspection-targets))
				  (kdebug '(introspection-trace)
							"      schedule: "
							(list-head (my 'timestep-schedule) 3)
							(if (> (length (my 'timestep-schedule)) 3)
								 '... ""))
				  
				  (set-my! 'variables-may-be-set #f)

				  ;;(max dt (* 2.0 dt))
				  dt
				  ))

(define (exclude-voids lst)
  (filter (lambda (x) (not (equal? #!void x))) lst))

(model-method (<introspection> <agent>) (insert-agent! self target)
				  (set-my! 'introspection-targets (exclude-voids  (cons target
																 (my 'introspection-targets)))))

(model-method (<introspection> <agent>) (append-agent! self target)
				  (if (not (equal? target #!void))
						(set-my! 'introspection-targets (exclude-voids (append (my 'introspection-targets)
																		 (list target))))))

(model-method <introspection> (introspection-targets self)
				  (my 'introspection-targets))
(model-method <introspection> (introspection-times self)
				  (my 'timestep-schedule))

(model-method (<introspection> <list>) (set-introspection-targets! self lst)
				  (set-my! 'introspection-targets (exclude-voids lst)))
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
