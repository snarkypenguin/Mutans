;-  Identification and Changes

;--
;	framework-declarations.scm -- Written by Randall Gray 


;--- basic wrangling -- ALL classes
(declare-method initialise "handles the initialisation of state variables") ;; Note *proper* spelling
(declare-method dump "dumps a 'readable' view of the state of an object")

(declare-method agent-prep "runs any 'prep' code, like opening files")
(declare-method agent-shutdown "runs any 'shutdown' code, like closing files")

(declare-method set-state-flag! "set a flag in the ad hoc state-variable list")
(declare-method add-state-flag "add a flag to the ad hoc state-variable list")
(declare-method state-flag "return the value of the flag in the ad hoc state-variable list")

(declare-method insert-agent! "add an agent in the introspection list")
(declare-method append-agent! "add an agent in the introspection list")

;--- Misc.

(declare-method total-value "description needed")

;--- state variables
(declare-method name "description needed")
(declare-method set-name! "description needed")
(declare-method type "description needed")
(declare-method set-type! "description needed")
(declare-method representation "description needed")
(declare-method set-representation! "description needed")
(declare-method subjective-time "description needed")
(declare-method set-subjective-time! "description needed")
(declare-method priority "description needed")
(declare-method set-priority! "description needed")
(declare-method jiggle "description needed")
(declare-method set-jiggle! "description needed")
(declare-method dt "description needed")
(declare-method set-dt! "description needed")
(declare-method migration-test "description needed")
(declare-method set-migration-test! "description needed")
(declare-method timestep-schedule "description needed")
(declare-method set-timestep-schedule! "description needed")
(declare-method kernel "description needed")
(declare-method set-kernel! "description needed")

;--- supporting the modelling framework
(declare-method snapshot "description needed")
(declare-method i-am "description needed")
(declare-method is-a "description needed")
(declare-method parameter-names "description needed") 
(declare-method parameters "description needed")
(declare-method set-parameters! "description needed")
(declare-method query "description needed")
(declare-method run-at "description needed")
(declare-method run "description needed")
(declare-method run-model-body "description needed")
(declare-method run-nested-model-body "description needed")
(declare-method run-agents "This routine runs a list of agents using the passed 'run' procedure")
(declare-method run-nested-agents "This routine hands a list of nested agents to run-agents")


;;(declare-method model "description needed")
;;(declare-method set-model! "description needed")

(declare-method extra-variable "refer used in the logging to get non-slot data out of a class")
(declare-method extra-variable-list "refer used in the logging to get non-slot data out of a class")

(declare-method migrate "description needed")

;--- <thing>
(declare-method mass "Return the mass of an entity")
(declare-method set-mass! "Set the mass of an entity")
(declare-method dim "Return the number of dimension an entity works in")
(declare-method set-dim! "Set the dimensionality of an entity -- (x y z) would be 3'")
(declare-method location "Return the location of an entity")
(declare-method set-location! "Set the location of an entity")
(declare-method direction "Return the direction of an entity")
(declare-method set-direction! "Set the direction of an entity")
(declare-method speed "Return the speed of an entity")
(declare-method set-speed! "Set the speed of an entity")

(declare-method track-locus! "Add the indicated time and location to entity's current location to the track list")
(declare-method track "return the entity's track")
(declare-method set-track! "set the track to the supplied 'track' list")
(declare-method new-track! "indicates that a new segment of tracking is to start")
(declare-method tracks "returns a list of track segments (or false)")


;--- <environment> generics
(declare-method contains? "predicate which indicates containment")
(declare-method value "typically the value of an ecoservice or patch")
(declare-method set-value! "set a value -- in the case of the environment, its default value")
(declare-method min-bound "list of minima in the dimensions")
(declare-method max-bound "list of maxima for the dimensions")


(load "log-declarations.scm")

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
