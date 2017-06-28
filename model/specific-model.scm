; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	specific-model.scm -- Written by Randall Gray 

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 
(include "framework") ;; This provides a number of useful bits

;- =================  Define the model domain now =======================
;========================================================================

"The model domain must be defined before the model itself is loaded."

;-- First,  space


;(define Model-Domain '((-500 -500 -50) (500 500 50)))
(define Model-Domain '((0 0 -50) (1000 1500 50)))
;; takes the ll, ur and any z component
(set-model-domain! (car Model-Domain) (cadr Model-Domain) isoA4)


"Note that the parameter files are all loaded at this point, references to
mapping functions that are generated by the set-model-domain will not be 
accessible (and will cause a load failure) unless delayed evaluation is used
and the state variable is assigned a value returned by an (eval ..), like so:

	????
"

;;(set-model-domain '(-5040 -5040 -100) '(5040 5040 3000) isoA4)
                  ;; ll ur optional-pagesize
;(set-model-domain '(-5040 -5040 -100) '(5040 5040 3000) isoA4r)
;; rotated


; (map (lambda (x) ((mapf (slot-ref pslog 'model->local)) x)) (map perimeter patchlist))




;-- generate output mappings (must happen after the model domain is set
;;  using set-model-domain

;-- ...and time

(define start 0) ;; day zero
;(define end (* 12 days)) ;; of christmas y'know
;(define end (* 29 days)) 
(define end (* 51 days)) ;; 
;(define end (* 61 days)) ;; 
;(define end (* 3 years)) ;; 
;(define end (* 480 days)) ;; 
;(define end (* 72 years)) ;;

(if (< end start)
	 (error "The model doesn't work in that direction")
	 (dnl* "***** Running from" start "to" end))

(disable-timers)


;--  the cast....
(dnl "The stage") ;-------------------------------------------------------------------

(define N 1) ;; Number cells along the "east-west" axis
(define M 1) ;; Number cells along the "north-south" axis

;; calculate area of patch, divide by number of clusters,
;; calculate radius of a circle with the same area and use that as the basis
;; for the clustering radius

(define chains-per-patch 4)
(define clusters-per-chain 4) ;; 
(define cluster-radius (* 50 m))
(define trees-per-cluster 3)
(define seeding-radius (* cluster-radius 2.5)) ;; 


;; ;; This returns a list of locations and masses
;; (define (tree-cluster-mass-and-location cellcentre pos meanage N)
;;   (let* ((n (length cellcentre))
;; 			(P (make-list n (if (number? pos) (make-pprocess pos) pos))))
;; 	 (random-cluster cellcentre prng (make-pprocess (* 1/4 meanage)) N)))

;; (random-cluster '(0 0 0) (list (make-pprocess 20) (make-pprocess 20) (make-pprocess 12)) 40)

(dnl "The set") ;-------------------------------------------------------------------

;--- First the cells, implemented as patches with water, fruit and seeds variables

(define seed-germination-rate  0.001) ;; THIS REALLY OUGHT TO BE IN A PARAMETER FILE!
(define fruit-germination-rate 0.0001)
(define patchlist 
  (let* ((grid (make-grid <patch> "ch3cell" "gridcell" 'area
						  N M ;; array 
						  Domain ;; geographic ll and ur
						  (lambda (x y) ;; vertical displacement function
							 (let ((x (/ x 10))
									 (y (/ y 10)))
								(* 10 (sqrt (+ (* x x) (* y y))))))
						  'dt (* 1 days)
						  'caretaker (lambda (self t dt)
											;; The number of seeds is attenuated by the ecoservice;
											;; here, we potentially germinate some....
											(let* ((seeds (value self 'seeds))
													 (fruit (value self 'fruit))
													 (fg (* fruit fruit-germination-rate))
													 (sgerminated (inexact->exact (truncate (* seed-germination-rate seeds))))
													 (fgerminated (inexact->exact (truncate fg)))
													 )
											  (if (> (+ sgerminated fgerminated) 0)
													(begin
													  (dnl* "**** total number:" (+ seeds (* fruit 0+1i)))
													  (set-value! self 'seeds (- seeds sgerminated))
													  (set-value! self 'fruit (- fruit fgerminated))
													  (dnl* "**** total number:" (+ sgerminated (* fgerminated 0+1i)))
													  (let* ((indices (seq (+ fgerminated sgerminated)))
																(result 
																 (map
																  (lambda (i)
																	 (create <example-plant> "B.exemplarii"
																				'location (random-point-in-box (perimeter self))
																				'domain self 'habitat self
																				'age 0)
																	 )
																  indices)))
														 (list 'introduce result)))
													'())
													)
						  )
					)
			))
	 grid))

;; We insert the patches into the queue so that their subjective time is incremented.
;; Bog standard patches don't *do* anything, so it is really just cosmetic ... if they
;; were dynamic-patches, it would be quite a different matter!

(for-each ;; Insert each patch into the queue
 (lambda (p)
	(set! Q (q-insert Q p Qcmp)))
 patchlist)

(dnl "Arden") ;-------------------------------------------------------------------
(define tree-taxon "B.exemplarii")

(define trees '())
"Clusters of trees are seeded randomly and without regard to the footprint of the patches.
when each tree is created, we check which patch it is in, and assign it appropriately.

Initially a cluster location within the domain is selected in a uniformly random way, the 
next cluster location is termined by a pprnd jump from the centroid of the first, and so
on for a chain with a lenght equal to the number of cells we are using.



"

(define  (make-daisy-chain domain scale n lst )
  (cond
	(#f (list (list-head (apply random-point-in-box domain) 2)))
	((<= n 0) lst)
	((null? lst)
	 (let ((p (list-head (apply random-point-in-box domain) 2)))
		(make-daisy-chain (list-head domain 2) scale (- n 1) (list p))
		))
	(else
	 (let loop ((p (map + (car lst) (map (lambda (x) (+/- (pprnd scale))) (make-list (length (car lst)) 0)) )))
		(if (Valid-location p)
			 (make-daisy-chain domain scale (- n 1) (cons p lst))
			 (loop (map + (car lst) (map (lambda (x) (+/- (pprnd scale))) (make-list (length (car lst)) 0)) ))
			 )
		))
	))


(define initial-loci
  (let* ((nC (apply + (map (lambda (x) (inexact->exact (round (nrnd clusters-per-chain)))) (make-list (* N M chains-per-patch) 0))))
			(nT (apply + (map (lambda (x) (inexact->exact (round (nrnd trees-per-cluster)))) (make-list nC 0))))
			(chains
			 (map (lambda (x) (make-daisy-chain Domain cluster-radius clusters-per-chain '()))
					(make-list (* N M chains-per-patch) '())))
			)
	 (apply append chains)))



(define trees
  (map (lambda (x) ;; x marks the spot
			(let ((t (create <example-plant> "B.exemplarii" 'location x 'domain #f 'habitat #f))) ;; mass is set using the mass-at-age function in the agent-prep stage
			  (set! Q (q-insert Q t Qcmp))
			  (for-each
				(lambda (p)
				  (if (contains? p x) 
						(begin
						  (if (not (slot-ref t 'domain)) (slot-set! t 'domain p))
						  (if (not (slot-ref t 'habitat)) (slot-set! t 'habitat p))
						  ))
				  )
				patchlist
				)
			  ))
		 initial-loci))

;; (make-copse (lambda (t)
;; 					 (set! Q (q-insert Q t Qcmp))
;; 					 (set! trees (cons t trees))
;; 					 (display "^"))
;; 				  <example-plant> "B.exemplarii"
;; 				  (nrnd trees-per-cluster 1.0 1 (* 2 trees-per-cluster))
;; 				  p
;; 				  (lambda () (pprnd (* 0.1 B.ex-longevity) B.ex-longevity)) ;; age
;; 				  C
;; 				  (* (radius p) 1/3))
;; )



(dnl " ... and their fruit")

(define ecological-services '())

(for-each
 (lambda (c)
	(let ((fruitdecay (numeric-parameter-lookup <example-plant> tree-taxon 'fruit-decay));; This is how to get a parameter from a taxon specific file 
			(seeddeath (numeric-parameter-lookup <example-plant> tree-taxon 'seed-decay))) ;; You can use numeric-... string-... symbol-... and list-parameter-lookup
	  ;; taxon  name           variable value cap r maxdt growing? growthmodel . P) 
	  (let ((fruit (simple-ecoservice "fruit" (* 2 days) "B. ex. fruit" 'fruit 0 +inf.0 0 (* 6 hour) #t (lambda (t dt val) (max 0 (* val (if fruitdecay (exp (* fruitdecay dt)) 1)))) c))
			  (seeds (simple-ecoservice "seeds" (* 4 days) "B. ex. seeds" 'seeds 0 +inf.0 0 (* 6 hour) #t (lambda (t dt val) (max 0 (* val (if seeddeath (truncate (exp (* seeddeath dt))) 1))))  c))
			  )
		 (slot-set! fruit 'history #t)
		 (set-value! fruit (pprnd 25))
		 (slot-set! seeds 'history #t)
		 (set-value! seeds (pprnd 4000))

		 (set! ecological-services (cons fruit ecological-services))
		 (set! ecological-services (cons seeds ecological-services))
		 ;;		 (set! ecological-services (cons jcarn ecological-services))
		 
		 (set! Q (q-insert Q fruit Qcmp))
		 (set! Q (q-insert Q seeds Qcmp))
		 ;;		 (set! Q (q-insert Q jcarn Qcmp))
		 
		 ;;		 (slot-set! c 'service-list (list fruit seeds jcarn)))))
		 (slot-set! c 'service-list (list fruit seeds)))))
 patchlist)

(newline)

(dnl "The players")

;; Herbivores and carnivores --------------------------------------------

;; ----- herbivores -----
;; Adult herbivores eat trees, juveniles only eat fruit.  We start with
;; 1/4 as many juveniles as adults.

(dnl "NOT DONE YET (specific-model.scm)")

(define HJ '())
(define HA '())
(define CA '())

(for-each
 (lambda (p)
	(if #t
		 (for-each
		  (lambda (i)
			 (let ((j (create <jherb> "juvenile-herbivore" 'name (serial-number "herbivore") 'sex (if (odd? (random-integer 5)) 'male 'female)))
					 )
				(slot-set! j 'habitat p)
				(slot-set! j 'domain p)
				(slot-set! j 'location (map + (location p) (list (- (pprnd 200) (pprnd 200)) (- (pprnd 100) (pprnd 100)))))
				(set! Q (q-insert Q j Qcmp))
				(set! HJ (cons j HJ))
				))
		  (seq 16)))

	(if #t
		 (for-each
		  (lambda (i)
			 (let ((a (create <aherb> "adult-herbivore" 'name (serial-number "herbivore") 'sex (if (odd? (random-integer 5)) 'male 'female)))
					 )
				(slot-set! a 'habitat p)
				(slot-set! a 'domain p)
				(slot-set! a 'location (map + (location p) (list (- (pprnd 200) (pprnd 200)) (- (pprnd 200) (pprnd 200)))))
				(set! Q (q-insert Q a Qcmp))
				(set! HA (cons a HA))
				))
		  (seq 4))
		 )

	(if #f
		 (for-each
		  (lambda (i)
			 (let ((c (create <acarn> "carnivore" 'name (serial-number "carnivore") 'sex (if (odd? (random-integer 3)) 'male 'female)))
					 )
				(slot-set! c 'habitat p)
				(slot-set! c 'domain p)
				(slot-set! c 'location (map + (location p) (list (- (pprnd 200) (pprnd 200)) (- (pprnd 300) (pprnd 300)))))
				(set! Q (q-insert Q c Qcmp))
				(set! CA (cons c CA))
				))
		  (seq  2))
		 )
	)
 patchlist)

;; ----- carnivores -----
;; Adults eat juvenile herbivores, juveniles are assumed to eat something
;; that is abundant and not modelled.


;; And the audience -----------------------------------------------------

;(define schedtimes (append 
;						 (cons 0 (seq 6))
;						 (map (lambda (x) (* 10.0 (1+ x))) (seq 360)))
;  ) ;; first six days, then on every tenth day from the beginning for 370 days


;; If the patches aren't in the Q, the loggers cannot find them
;;(for-each (lambda (p) (set! Q (q-insert Q p Qcmp))) patchlist)



(define tdlog
  (if #t
		(let ((tdlog (create <log-data> "tree-logger" 'filename "tree-data"
									;;'file (current-output-port)
									;; Recall: internally time---end for example---is in seconds.
									;;			 'timestep-schedule (map (lambda (x) (* x day)) (seq (+ 1 (/ end week))))
									'dt (* 10 days)
									'variables (list 'subjective-time 'name 'age 'mass 'leaf-area 'forage-damage)
									'introspection-targets (list <plant>))))
						 ;; there will be more trees, so we want a mechanism that adapts
		  (set! Q (q-insert Q tdlog Qcmp)) ;; these will go in earlier than the others 
		  )
		(void)))

(define sdlog
  (if #t
		(let ((sdlog (create <log-data> "service-logger" 'filename "service-data"
								  ;;'file (current-output-port)
								  ;; Recall: internally time---end for example---is in seconds.
								  ;;		 'timestep-schedule (map (lambda (x) (* x day)) (seq (+ 1 (/ end day)))) 
								  'dt (* 5 days)
								  'variables (list 'name 'subjective-time 'value)
								  'introspection-targets ecological-services)))
		  ;; We specify these by list since the number of services stays the same
		  (set! Q (q-insert Q sdlog Qcmp)) ;; these will go in earlier than the others 
		  )
		(void)))

(define pslog
  (if #t
      ;;                                       model  page  margin                           
		(let ((mapping (map:domain-to-postscript Domain isoA4 10))) ;; NOTE page dimensions are *always* specified in mm.
		  (let ((pslog
					(create <log-map> "map-maker" 'filename "domain-map"
							  ;; Recall: internally time---end for example---is in seconds.
							  ;;			 'timestep-schedule (map (lambda (x) (* x 4 weeks)) (seq (+ 1 (/ end (* 4 weeks)))))
							  'model->local mapping
							  'local->model (mapping 'inverse)
							  'dt (* 5 days)
							  'introspection-targets (append (list <patch> <example-plant> <aherb> <jherb>) patchlist)
							  )))
			 (set! Q (q-insert Q pslog Qcmp)) ;; these will go in earlier than the others 
			 ))
		(void))          ;; *is-class? creates a predicate function, there are also
                      ;; *is-taxon? *has-slot?
  )


;========================================================================

;-- Example code to run things....



(define (ps-check filename)
  (let ((ps (make-ps filename '(Helvetica))))
	 (ps 'moveto 300 300)
	 (ps 'display (string-append "File: " filename))
	 (map (lambda (x) (dnl* (name x)) (ps-dump x ps)) (filter (*has-slot? 'location) Q))
	 (close-output-port ps)))




	 
;-  The End


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
