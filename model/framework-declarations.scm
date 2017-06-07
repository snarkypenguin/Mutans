(include "framework")
;-  Identification and Changes


;***** When you add things to this file (or any other *-declarations.scm file)
;***** you *must* run "make declarations.scm" or a more comprehensive make.
;*****
;***** This is because the model includes "declarations.scm" rather than the many
;***** other *-declaractions.scm files -- "declarations.scm" is filtered to exclude
;***** duplicate declarations, and this stops methods simply disappearing.



;--
;	framework-declarations.scm -- Written by Randall Gray 

; These need to be loaded before the framework methods are loaded


;--- basic wrangling -- ALL classes

;;; (declare-method
;;;  dump%
;;;  "dumps a 'readable' view of the state of an object")

(define agent-prep (make-generic))

(register-unique class <class>)
(register-unique class <top>)
(register-unique class <generic>)
(register-unique class <class>)
(register-unique class <procedure-class>)
(register-unique class <entity-class>)
(register-unique class <method>)
(register-unique class <primitive-class>)
(register-unique generic-method allocate-instance)
(register-unique generic-method compute-getter-and-setter)
(register-unique generic-method compute-cpl)
(register-unique generic-method compute-slots)
(register-unique generic-method compute-methods)
(register-unique generic-method compute-method-more-specific?)
(register-unique generic-method compute-apply-methods)
(register-unique generic-method initialise) ;; we *don't* want to overwrite this!

(generic-method-register 'add agent-prep 'agent-prep)
(declare-method adjust-state "adjust the state of an agent: usually used when changing representations")
(declare-method action "trigger a representation change, or whatever")
(declare-method acquire-agents "insert an agent or a list of agents into a subsidiary-agent-list")
(declare-method activate "make a subsidiary agent active")
(declare-method add-state-flag "add a flag to the ad hoc state-variable list")
(declare-method add-thing "add a thing to the location tracking system")
(declare-method agent-shutdown "runs any 'shutdown' code, like closing files")
(declare-method append-agent! "add an agent in the introspection list")
(declare-method apply-parameters "load a new parameterisation and type from a file in ./params") ;; Hardcoded at the moment
(declare-method change-type "change the type (and possibly state-variables) of an agent")
(declare-method change-taxon "change the taxon, probably trigger a resetting of state-variables")
(declare-method close-p/n "close a logging port")
(declare-method comment "reference information for agent")
(declare-method contains? "predicate which indicates containment")
(declare-method data-emit-page "specific for data output")
(declare-method data-log-data "specific for data output")
(declare-method data-log-track-segment "description needed")
(declare-method deactivate "make a subsidiary agent inactive")
(declare-method dim "Return the number of dimension an entity works in")
(declare-method direction "Return the direction of an entity")
(declare-method dt "the amount of time the agent is running for or will run")
(declare-method dump% "Internal (fixed arg) version for dumping an agent")
(declare-method emit-page "set the list of agents to be examined")
(declare-method extend-variables! "extend the list of variables")
(declare-method extra-variable "refer used in the logging to get non-slot data out of a class")
(declare-method extra-variable-list "refer used in the logging to get non-slot data out of a class")
(declare-method i-am "returns the representation of the entity")
(declare-method insert-agent! "add an agent in the introspection list")
(declare-method insert-schedule-time! "insert a time into a schedule")
(declare-method introspection-targets "return the introspection list")
(declare-method introspection-times "return the introspection list")
(declare-method is-a "representation predicate")
(declare-method jiggle "the current random adjustment when being inserted in the runqueue")
(declare-method kernel "function which handles kernel queries")
(declare-method kernel-check "check the ability of the agent to call the kernel")
(declare-method kquery "Send a query to the kernel")
(declare-method locate "locate something or a list of something (usually other agents)")
(declare-method location "Return the location of an entity")
(declare-method log-data "err, ...log data to an open output") 
(declare-method log-this-agent "log an agent's data")
(declare-method maintain-state "constructs a closure which can maintain and report the state, and provide a list of things needed from other representations")
(declare-method map-emit-page "specific for postscript output")
(declare-method map-log-track-segment "description needed")
(declare-method projection-assoc-list "returns the list of available projections")
(declare-method set-projection-assoc-list! "sets the list of available projections")
(declare-method projection "returns a projection for a given symbol")
(declare-method project-datum "projects a datum using an indicated symbol")
(declare-method mass "Return the mass of an entity")
(declare-method max-bound "list of maxima for the dimensions")
(declare-method migrate "migrate to a new representation")
(declare-method migration-test "used by an entity to test if it should change representation")
(declare-method min-bound "list of minima in the dimensions")
(declare-method name "A unique identifier for the agent")
(declare-method new-track! "indicates that a new segment of tracking is to start")
(declare-method open-p/n "open a port for logging")
(declare-method page-epilogue "make things tidy")
(declare-method page-preamble "open files & such")
(declare-method parameter-names "names of all parameters (slots) of the agent's class") 
(declare-method parameters "values of the parameters (slots) of the agent;s class")
(declare-method pass-preparation "Does any processing a <monitor> needs to do before the runqueue is checked")
(declare-method pass-resolution "Implements any actions a <monitor> may need")
(declare-method priority "run priority")
(declare-method process-agent "called by <monitor>model-body, this processes them")
(declare-method project-datum "map a point to a new data space")
(declare-method projection-assoc-list "get a mapping function using a key")
(declare-method provides "return the list of things the agent provides")
(declare-method provides? "pass a list of requirements in and return the list of those that this agent provides")
(declare-method provides! "add to the list of services")
(declare-method query "query other agents")
(declare-method remove-thing "remove a thing from the location tracking system")
(declare-method representation "individual, aggregate, analytic, circle, polygon...")
(declare-method representation-assessment "method to return an assessment")
(declare-method requires "return the list of things the agent requires")
(declare-method requires? "pass a list of things that may be provided and return the list of those that this agent requires")
(declare-method requires! "add to the list of requirements")
(declare-method resolve-agent "part of the monitor set, resolves the outstanding issues with each agent (if any)")
(declare-method run "run an agent")
(declare-method run-agents "This routine runs a list of agents using the passed 'run' procedure")
(declare-method run-at "schedule a tick at a particular time")
(declare-method run-model-body "run an agent's model-body")
(declare-method run-subsidiary-agents "run agents in the active-subsidiary-agent list")
(declare-method schedule-epsilon "return a schedule's epsilon")
(declare-method schedule-times "return a schedule")
(declare-method set-comment! "reference information for agent")
(declare-method set-dim! "Set the dimensionality of an entity -- (x y z) would be 3'")
(declare-method set-direction! "Set the direction of an entity")
(declare-method set-dt! "change the dt")
(declare-method set-introspection-targets! "set the list of agents to be examined")
(declare-method set-introspection-times! "set the list of agents to be examined")
(declare-method set-jiggle! "reset the jiggle")
(declare-method set-kernel! "change the kernel query function")
(declare-method set-location! "Set the location of an entity")
(declare-method set-projection-assoc-list! "description needed")
(declare-method set-mass! "Set the mass of an entity")
(declare-method set-migration-test! "set/change the migration test function")
(declare-method set-name! "Set the unique name")
(declare-method set-parameters! "set parameters using a list")
(declare-method set-priority! "change the run priority")
(declare-method set-representation! "record the agent's representation")
(declare-method set-schedule-epsilon! "set the epsilon for the schedule")
(declare-method set-schedule-times! "set the times the agents is scheduled to do something")
(declare-method set-speed! "Set the speed of an entity")
(declare-method set-state-flag! "set a flag in the ad hoc state-variable list")
(declare-method set-subjective-time! "adjust the agents view of 'now'")
(declare-method set-timestep-schedule! "change the timestep list")
(declare-method set-track! "set the track to the supplied 'track' list")
(declare-method set-type! "set/change the type of an agent")
(declare-method set-value! "set a value -- in the case of the environment, its default value")
(declare-method set-variables! "set the list of variables")
(declare-method snapshot "used by logging mechanism")
(declare-method speed "Return the speed of an entity")
(declare-method state-flag "return the value of the flag in the ad hoc state-variable list")
(declare-method subjective-time "the agent's current time")
(declare-method transfer-agent "transfers an agent to the kernel's runqueue")
(declare-method timestep-schedule "a list of scheduled timesteps")
(declare-method total-value "description needed")
(declare-method track "return the entity's track")
(declare-method track-location! "Insert the time and location in the track list")
(declare-method tracks "returns a list of track segments (or false)")
(declare-method type "the categorical type of agent (analogous to species or genus)")
(declare-method value "typically the value of an ecoservice or patch")
;;(declare-method model "description needed")
;;(declare-method set-model! "description needed")

;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
