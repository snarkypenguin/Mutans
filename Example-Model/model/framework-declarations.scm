;-  Identification and Changes

;--
;	framework-declarations.scm -- Written by Randall Gray 


;--- basic wrangling -- ALL classes
(declare-method
 initialise ;; *** Note spelling ***
 "handles the initialisation of state variables") 
(declare-method
 dump
 "dumps a 'readable' view of the state of an object")

(declare-method
 agent-prep
 "runs any 'prep' code, like opening files")
(declare-method
 agent-shutdown
 "runs any 'shutdown' code, like closing files")

(declare-method
 set-state-flag!
 "set a flag in the ad hoc state-variable list")
(declare-method
 add-state-flag
 "add a flag to the ad hoc state-variable list")
(declare-method
 state-flag
 "return the value of the flag in the ad hoc state-variable list")

(declare-method
 insert-agent!
 "add an agent in the introspection list")
(declare-method
 append-agent!
 "add an agent in the introspection list")

;--- Misc.

(declare-method total-value "description needed")

;--- state variables
(declare-method
 name
 "A unique identifier for the agent")
(declare-method
 set-name!
 "Set the unique name")
(declare-method
 type
 "the categorical type of agent (analogous to species or genus)")
(declare-method
 set-type!
 "set/change the type of an agent")
(declare-method
 representation
 "individual, aggregate, analytic, circle, polygon...")
(declare-method
 set-representation!
 "record the agent's representation")
(declare-method
 comment
 "reference information for agent")
(declare-method
 set-comment!
 "reference information for agent")
(declare-method
 subjective-time
 "the agent's current time")
(declare-method
 set-subjective-time!
 "adjust the agents view of 'now'")
(declare-method
 priority
 "run priority")
(declare-method
 set-priority!
 "change the run priority")
(declare-method
 jiggle
 "the current random adjustment when being inserted in the runqueue")
(declare-method
 set-jiggle!
 "reset the jiggle")
(declare-method
 dt
 "the amount of time the agent is running for or will run")
(declare-method
 set-dt!
 "change the dt")
(declare-method
 migration-test
 (string-append "a function which indicates if an entity"
					 " should change representation: "
					 "(f self t ldt return)"))
(declare-method
 set-migration-test!
 "set/change the migration test function")
(declare-method
 timestep-schedule
 "a list of scheduled timesteps")
(declare-method
 set-timestep-schedule!
 "change the timestep list")
(declare-method
 kernel
 "function which handles kernel queries")
(declare-method
 set-kernel!
 "change the kernel query function")

;--- supporting the modelling framework
(declare-method
 maintain-state
 "constructs a closure which can maintain and report the state, 
  and provide a list of things needed from other representations")

(declare-method
 snapshot
 "used by logging mechanism")
(declare-method
 i-am
 "returns the representation of the entity")
(declare-method
 is-a
 "representation predicate")
(declare-method
 parameter-names
 "names of all parameters (slots) of the agent's class") 
(declare-method
 parameters
 "values of the parameters (slots) of the agent;s class")
(declare-method
 set-parameters!
 (string-append "set parameters using a list of the form "
					 "'(pval...) where vals are in the right order"))
(declare-method
 query
 "Send a query to the kernel")
(declare-method
 run-at
 "schedule a tick at a particular time")
(declare-method
 run
 "run an agent")
(declare-method
 run-model-body
 "run an agent's model-body")
(declare-method
 run-nested-model-body
 "run a nested model body")
(declare-method
 run-agents
 "This routine runs a list of agents using the passed 'run' procedure")
(declare-method
 run-nested-agents
 "This routine hands a list of nested agents to run-agents")


;;(declare-method model "description needed")
;;(declare-method set-model! "description needed")

(declare-method
 extra-variable
 "refer used in the logging to get non-slot data out of a class")
(declare-method
 extra-variable-list
 "refer used in the logging to get non-slot data out of a class")

(declare-method migrate "description needed")

;--- <thing>
(declare-method
 mass
 "Return the mass of an entity")
(declare-method
 set-mass!
 "Set the mass of an entity")
(declare-method
 dim
 "Return the number of dimension an entity works in")
(declare-method
 set-dim!
 "Set the dimensionality of an entity -- (x y z) would be 3'")
(declare-method
 location
 "Return the location of an entity")
(declare-method
 set-location!
 "Set the location of an entity")
(declare-method
 direction
 "Return the direction of an entity")
(declare-method
 set-direction!
 "Set the direction of an entity")
(declare-method
 speed
 "Return the speed of an entity")
(declare-method
 set-speed!
 "Set the speed of an entity")

(declare-method
 track-locus!
 "Insert the time and location in the track list")
(declare-method
 track
 "return the entity's track")
(declare-method
 set-track!
 "set the track to the supplied 'track' list")
(declare-method
 new-track!
 "indicates that a new segment of tracking is to start")
(declare-method
 tracks
 "returns a list of track segments (or false)")

;--- <environment> generics
(declare-method
 contains?
 "predicate which indicates containment")
(declare-method
 value
 "typically the value of an ecoservice or patch")
(declare-method
 set-value!
 "set a value -- in the case of the environment, its default value")
(declare-method
 min-bound
 "list of minima in the dimensions")
(declare-method
 max-bound
 "list of maxima for the dimensions")


(load "log-declarations.scm")

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
