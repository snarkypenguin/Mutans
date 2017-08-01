
(include "framework")
;- Identification and Changes

"The basic classes from which the others are ultimately derived are
kept in 'sclos+extn.scm' since they are supposed to be /fundamental/."


;- Load initial libraries 

;-- Define/allocate new classes

;--- substrate


;;; ;--- agent based classes

;;; (define-class <agent>
;;;   (inherits-from <object>)
;;;   (state-variables name taxon representation agent-state
;;; 						 note
;;; 						 kernel
;;; 						 subjective-time priority jiggle 
;;; 						 dt
;;; 						 migration-test timestep-schedule counter
;;; 						 projection-assoc-list local-projection inv-local-projection
;;; 						 state-flags
;;; 						 agent-epsilon
;;; 						 agent-schedule
;;; 						 dont-log
;;; 						 agent-body-ran

;;; 						 ;; acting as a kernel for others
;;; 						 suspended-at
;;; 						 subsidiary-agents active-subsidiary-agents

;;; 						 ;;
;;; 						 maintenance-list
;;; 						 )
;;; 	)


;; subsidiary-agents are run within the time-step and auspices of
;; another (presumably related) agent. Agents may know what their
;; parent agent is (if they have one) and may indicate to the parent
;; that they should be added to the active list. The parent agent is
;; the one that actually decides if a agent is to move into the active
;; queue or out of the active queue.  Whe things get moved, "value"
;; from the parent is moved into the relevant sub-agents.  The set of
;; ecoservices of the parent contains all of the types represented in
;; its sub-agents.
;;
;; priority is an integer, the higher the integer the greater the
;; priority.  The default value is zero jiggle is a real number in (0,
;; 1).  Setting jiggle to values outside that domain suppresses the
;; the randomisation of the jiggle.  If an agent has a timestep-schedule,
;; then it WILL be used in determining the next dt.

;; name is a string
;; representation is a symbol
;; subjective-time, and dt are numbers representing a time and an interval,
;; body is a function (lambda (self t df . args) ...)
;; migration-test is a function (lambda (self t df . args) ...)
;; timestep-schedule is a list of times at which the agent needs to run
;;    (a list of monotonically increasing numbers)
;; kernel is a function that can be used to interact with
;;    the kernel of the simulation


;;;(load "monitor-classes.scm")
;;;(load "log-classes.scm") ;; These are used to generate output.

(define-class <projection>
  (inherits-from <object>)
  (state-variables projection-assoc-list
						 local->model model->local
						 default-font default-size
						 default-color
						 map-color map-contrast-color
						 ))
"The model->local is a projection used to map model coordinates
into agent's *internal* coordinates.  There must also be a
corresponding local->model to map the other
direction"
"

(define-class <file>
  (inherits-from <projection>) ;; there must be support for an mapping into output space
  (state-variables file filename))

(define-class <output>
  (inherits-from <file>)
  (no-state-variables)
  )

(define-class <output*>
  (inherits-from <output>)
  (state-variables basename filetype filename-timescale)
  )

(define-class <input>
  (inherits-from <file>) ;; there must be support for an input mapping into modelspace
  (no-state-variables)
)

(define-class <txt-output>  ;; one big file
  (inherits-from <output>)
  (no-state-variables)
  )
					  
(define-class <txt-output*> ;; small files
  (inherits-from <output*>)
  (state-variables))
					  
(define-class <tbl-output>
  (inherits-from <output>)
  (state-variables fields types))

(define-class <tbl-output*>
  (inherits-from <output*>)
  (state-variables fields types))

(define-class <ps-output>
  (inherits-from <output>))
					  
(define-class <ps-output*>
  (inherits-from <output*>))
"

;;; (define-class <mem-agent>
;;;   (inherits-from <agent>)
;;;   (state-variables memory)

(define-class <tracked-agent>
  (inherits-from <agent> <projection>)
  (state-variables track
						 location direction
						 speed max-speed
						 tracked-paths 
						 track-datum ;; a value to be emitted with the track or #f
						 ps-rad ;; a radius to use in plotting a tracked object's location
						 default-font default-size
						 glyph scale/slot plot-magnification))
;; "track" will either be a list like (... (t_k x_k y_k) ...) or false
;; "tracked-paths" is a list of non-false traces or false This is the
;; basic class to derive things that have a memory of their past.
;; glyph is either a point list which notionally faces "east" a number
;; which indicates the number of facets in a regular polyhedron, or a
;; procedure which returns a point-list.  It is the responsibility of
;; the agent to update the plot magnification appropriately.

(define-class <thing>
  (inherits-from <tracked-agent> <projection>)
  (state-variables mass dim))

(define-class <living-thing>
  (inherits-from <thing>)
  (state-variables
	ndt ;; nominal timestep for movement/decision making.  Used in the wander-around method
	dead-color
	age
	age-at-instantiation ;; the age at which something is "born"

	longevity ;; used for calculating the mass-at-age curve -- equivalent to the max-age of the species
	max-age	;; This is either a precalculated hard-kill (corresponds, to genetic influence?) or a non-number
	probability-of-mortality ;;; a number [typically compared against a (random-real) call] or a
	                         ;;; procedure that takes the age of the entity and returns a number
	decay-rate ;; when it's no longer living
	mass-at-age ;; not optional    but the growth will, so an entity that misses out on a bursty period
					;; will always be smaller than others in its cohort
	habitat  ;; an landscape agent that encompasses a number of potential domains
	         ;;    or a list that does the same thing ... not currently used
	domain   ;; an environment of some sort (they have to live somewhere!)

	environmental-threats ;; list of environmental conditions or entities that may be threatening.
	;; In this instance, we might have bounding regions which indicate prevailing conditions (fire, flood...)
	;; and may change fairly rapidly.  In the presence of environmental threats, the animals probably need
	;; a very short timestep.
	))

(define-class <environment>
  (inherits-from <agent> <projection>)
  (state-variables default-value minv maxv inner-radius outer-radius rep)
  ;; minv and maxv form a bounding volume in however many dimensions
  ;; rep is usually something like a polygon, a DEM or something like that 
  )

(define-class <blackboard>
  (inherits-from <agent>) ;; 
  (state-variables label message-list))
  

(define-class <model-maintenance>
  (inherits-from <object>) ;; We have <projection> since I'd really rather not have two distinct maintenance classes
  (state-variables maintenance-list I-need))
;; I-need is a list of fields which comprise the env-vector, and must be filled in by the
;;   agent which is maintaining the reduced model.
;; Agents with a model-maintenance class must supply state-variables and a method to update them
;; the form for the function is (update-state self t dt)
;; 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:




