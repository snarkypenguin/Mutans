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

(load "declarations.scm")

;;; (declare-method accessor "returns an an accessor -- can be a function or a method") -;
;;; (declare-method acquire-agents "insert an agent or a list of agents into a subsidiary-agent-list") -;
;;; (declare-method action "trigger a representation change, or whatever") -;
;;; (declare-method activate "make a subsidiary agent active") -;
;;; (declare-method add! "add!") -;
;;; (declare-method add-data-record "add a record to the data in an <array> agent") -;
;;; (declare-method add-fruit "adjusts the number of fruit in a cell") -;
;;; (declare-method add-patch! "add a patch to a habitat") -;
;;; (declare-method add-patches! "add a list of patches to a habitat") -;
;;; (declare-method add-projection! "add a projection function") -;
;;; (declare-method add-service! "add-service!") -;
;;; (declare-method add-state-flag "add a flag to the ad hoc state-variable list") -;
;;; (declare-method add-thing "add a thing to the location tracking system") -;
;;; (declare-method adjust-state "adjust the state of an agent: usually used when changing representations") -;
;;; (declare-method age-at-mass "return the age of a <living-thing> given its mass (mainly used for initialisation)") -;
;;; (declare-method agent-shutdown "runs any 'shutdown' code, like closing files") -;
;;; (declare-method agent-state "return the agent-state") -;
;;; (declare-method age "return the age of the animal") -;
;;; (declare-method aggregate-value "aggregate-value of a services within a nominated circle") -;
;;; (declare-method alter "request a change in another agent of the form (alter victim self 'sym ....)") -;
;;; (declare-method append-agent! "add an agent in the introspection list") -;
;;; (declare-method apply-parameters "load a new parameterisation and type from a file in ./params") ;; Hardcoded at the moment -;
;;; (declare-method area "area of an object") -;
;;; (declare-method capacity "returns the carrying capacity of an ecoservice") -;
;;; (declare-method centre "returns the centre/centroid of an object") -;
;;; (declare-method change-taxon "change the taxon, probably trigger a resetting of state-variables") -;
;;; (declare-method change-territory "move to another region (change cells)") -;
;;; (declare-method change-type "change the type (and possibly state-variables) of an agent") -;
;;; (declare-method close-p/n "close a logging port") -;
;;; (declare-method comment "reference information for agent") -;
;;; (declare-method composite-prj_src->dst "return a projection from one agent's space to another's") -;
;;; (declare-method contains? "predicate which indicates containment") -;
;;; (declare-method crowded? "determine if the region is too crowded for the animal") -;
;;; (declare-method data-emit-page "specific for data output") -;
;;; (declare-method data-index "map  names to column numbers in an <array> agent") -;
;;; (declare-method data-length "number of records in an <array> agent") -;
;;; (declare-method data-log-data "specific for data output") -;
;;; (declare-method data-log-track-segment "description needed") -;
;;; (declare-method data-names "return the names of columns in an <array> agent") -;
;;; (declare-method deactivate "make a subsidiary agent inactive") -;
;;; (declare-method dead? "Am I?") -;
;;; (declare-method define-population-dynamics! "Sets things up for population dynamics using definitions, alternative to set-population-dynamics!") -;
;;; (declare-method define-system-dynamics! "specifies a system of differential equations") -;
;;; (declare-method die "routine corresponding to the death of an animal (calls shutdown)") -;
;;; (declare-method dimensions "returns the number of dimensions") -;
;;; (declare-method dim "Return the number of dimension an entity works in") -;
;;; (declare-method direction "Return the direction of an entity") -;
;;; (declare-method disable-all-service-growth! "disables the service level growth model for the dynamic-patch") -;
;;; (declare-method disable-growth! "disables the growth model for the ecoservice/patch/etc") -;
;;; (declare-method disable-service-growth! "disables the service level growth model for for a service in the dynamic-patch") -;
;;; (declare-method distance-to-boundary "returns the distance to a boundary") -;
;;; (declare-method distance-to-centre "distance-to-centre") -;
;;; (declare-method distance-to-interior "distance-to-interior") -;
;;; (declare-method do-fruiting "fruit and add them to the environment") -;
;;; (declare-method dt "the amount of time the agent is running for or will run") -;
;;; (declare-method dump% "Internal (fixed arg) version for dumping an agent") -;
;;; (declare-method eat "add food to the stomach, process food from the stomach, grow, return the amount eaten") -;
;;; (declare-method eat "Eat food ") -;
;;; (declare-method emit-and-record-if-absent "registers an agent and its subjective time with a logger") -;
;;; (declare-method emit-page "set the list of agents to be examined") -;
;;; (declare-method enable-all-service-growth! "enables the service level growth model for the dynamic-patch") -;
;;; (declare-method enable-growth! "disables the growth model for the ecoservice/patch/etc") -;
;;; (declare-method enable-service-growth! "enables the service level growth model for a service in the dynamic-patch") -;
;;; (declare-method extend-variables! "extend the list of variables") -;
;;; (declare-method extra-variable-list "refer used in the logging to get non-slot data out of a class") -;
;;; (declare-method extra-variable "refer used in the logging to get non-slot data out of a class") -;
;;; (declare-method forage "move about looking for a region with food") -;
;;; (declare-method fruit-count "returns the number of fruit produced by an agent") -;
;;; (declare-method get-local->model "returns the projection an agent uses to map local coords into global coords") -;
;;; (declare-method get-model->local "returns the projection a agent uses to map global coords into local coords") -;
;;; (declare-method get-projection "returns a projection from an agent's list of projections") -;
;;; (declare-method growth "does the business of growing") -;
;;; (declare-method growth-model "returns the growth model for the ecoservice/patch/...") -;
;;; (declare-method growth-rate "returns the growth rate for an animal (<jherb>, etc), rate dependent on water stress and current mass") -;
;;; (declare-method has-data? "tests to see if an <array> agent has a data field or any agent has 'data' or a slot ") -;
;;; (declare-method hunt "hunt an animal, typically <jherb>, etc)") -;
;;; (declare-method i-am "returns the representation of the entity") -;
;;; (declare-method initialise-instance "coerces uninitialised state variables to reasonable values") -;
;;; (declare-method insert-agent! "add an agent in the introspection list") -;
;;; (declare-method insert-schedule-time! "insert a time into a schedule") -;
;;; (declare-method install-boundary "set/reset the boundary for a patch") -;
;;; (declare-method introspection-targets "return the introspection list") -;
;;; (declare-method introspection-times "return the introspection list") -;
;;; (declare-method is-a "representation predicate") -;
;;; (declare-method jiggle "the current random adjustment when being inserted in the runqueue") -;
;;; (declare-method kernel-check "check the ability of the agent to call the kernel") -;
;;; (declare-method kernel "function which handles kernel queries") -;
;;; (declare-method kquery "Send a query to the kernel") -;
;;; (declare-method leaf-area "leaf-area returns the leaf area less any damage")		 -;
;;; (declare-method leaf-mass "leaf-mass returns the mass of the leaves on the plant")		 -;
;;; (declare-method local-environment-check "Check the state of the local environment for things like water availability, densities of other players, fire, flood....") -;
;;; (declare-method local->model "return the function for projecting a submodels ords into modelspace") -;
;;; (declare-method locate "locate something or a list of something (usually other agents)") -;
;;; (declare-method location2 "return an agent's planar location from an agent") -;
;;; (declare-method location3 "return an agents location which includes altitude") -;
;;; (declare-method location "return a location from an agent") -;
;;; (declare-method location* "return the entire location vector from an agent") -;
;;; (declare-method log-data "err, ...log data to an open output")  -;
;;; (declare-method log-map-polygon "routine to plot a polygon for things like trees....") -;
;;; (declare-method log-this-agent "log an agent's data") -;
;;; (declare-method look-for<= "looks for the presence of specific categories of entities (mates, predators...)") -;
;;; (declare-method look-for>= "looks for the presence of specific categories of entities (mates, predators...)") -;
;;; (declare-method look-for "looks for the presence of specific categories of entities (mates, predators...)") -;
;;; (declare-method look-for* "looks for the presence of specific categories of entities (mates, predators...) ignoring temporal consistency") -;
;;; (declare-method lose-foliage "remove foliage from a plant") -;
;;; (declare-method maintain-state "constructs a closure which can maintain and report the state, and provide a list of things needed from other representations") -;
;;; (declare-method manage-offspring-check "routine for ranking the management of offspring") -;
;;; (declare-method map-emit-page "specific for postscript output") -;
;;; (declare-method map-log-track-segment "description needed") -;
;;; (declare-method mass-at-age "mass-at-age") -;
;;; (declare-method mass "Return the mass of an entity") -;
;;; (declare-method max-bound "list of maxima for the dimensions") -;
;;; (declare-method maxima "returns the maxima for the ordinates") -;
;;; (declare-method maximum-speed "returns the maximum speed for a thing based on its state") -;
;;; (declare-method mean-value "mean value of a resource or service") -;
;;; (declare-method migrate "migrate to a new representation") -;
;;; (declare-method migration-test "used by an entity to test if it should change representation") -;
;;; (declare-method min-bound "list of minima in the dimensions") -;
;;; (declare-method minima "returns the minima for the ordinates") -;
;;; (declare-method modal-dt "returns a dt based on the state of the agent") -;
;;; (declare-method model->local "return the function for projecting model ords into a submodel's space") -;
;;; (declare-method my-list "returns an introspection agents introspection list or the function which selects them from the runqueue") -;
;;; (declare-method my-rep "returns the underlying agent which defines the domain") -;
;;; (declare-method name "A unique identifier for the agent") -;
;;; (declare-method new-data-name "add an additional name to the names of columns in an <array> agent") -;
;;; (declare-method new-data-names "add additional names to the names of columns in an <array> agent") -;
;;; (declare-method new-track! "indicates that a new segment of tracking is to start") -;
;;; (declare-method open-output-handle "sets up the output file for a logger") -;
;;; (declare-method open-p/n "open a port for logging") -;
;;; (declare-method ordinates "returns the indices for a given location") -;
;;; (declare-method origin "returns the origin") -;
;;; (declare-method page-epilogue "make things tidy") -;
;;; (declare-method page-preamble "open files & such") -;
;;; (declare-method parameter-names "names of all parameters (slots) of the agent's class")  -;
;;; (declare-method parameters "values of the parameters (slots) of the agent class") -;
;;; (declare-method pass-preparation "Does any processing a <monitor> needs to do before the runqueue is checked") -;
;;; (declare-method pass-resolution "Implements any actions a <monitor> may need") -;
;;; (declare-method patch-at "returns a list of any patches containing a supplied location") -;
;;; (declare-method patch-list% "return a list of all patches or only the ones with particular services") -;
;;; (declare-method perimeter "return the perimeter of an item (if it has one)") -;
;;; (declare-method plot-glyph "plot an agent in a <log-map> file") -;
;;; (declare-method plot-plant "puts a glyph in a map") -;
;;; (declare-method populate-cells "sets up the cell agents") -;
;;; (declare-method pp-a "'pretty-print' an agent") -;
;;; (declare-method prey-present "returns a list of the animal's prey in a suitable locality") -;
;;; (declare-method priority "run priority") -;
;;; (declare-method pristine-leaf-mass "mass without forage damage") -;
;;; (declare-method process-agent "called by <monitor>model-body, this processes them") -;
;;; (declare-method project-datum "map a point to a new data space") -;
;;; (declare-method projection-assoc-list "get a mapping function using a key") -;
;;; (declare-method projection "returns a projection for a given symbol") -;
;;; (declare-method provides! "add to the list of services") -;
;;; (declare-method provides? "pass a list of requirements in and return the list of those that this agent provides") -;
;;; (declare-method provides "return the list of things the agent provides") -;
;;; (declare-method ps-dump "Dumps some agent to an open ps file") -;
;;; (declare-method query "query other agents") -;
;;; (declare-method queue-state "like agent-state, but says what the agent is doing queue-wise") -;
;;; (declare-method radius "inner radius (inscribed circle) or other radius") -;
;;; (declare-method Radius "outer radius (containing circle)") -;
;;; (declare-method random-point "returns a random point within the object") -;
;;; (declare-method remove-data-record "remove a record to the data in an <array> agent") -;
;;; (declare-method remove-patch! "remove a patch from a habitat") -;
;;; (declare-method remove-projection! "remove a projection function") -;
;;; (declare-method remove-service! "remove-service!") -;
;;; (declare-method remove-thing "remove a thing from the location tracking system") -;
;;; (declare-method representation-assessment "method to return an assessment") -;
;;; (declare-method representation "individual, aggregate, analytic, circle, polygon...") -;
;;; (declare-method rep "returns the representation object") -;
;;; (declare-method reproduce "create offspring") -;
;;; (declare-method request-accessor "get a hunk of code that gives access to internal state") -;
;;; (declare-method requires! "add to the list of requirements") -;
;;; (declare-method requires? "pass a list of things that may be provided and return the list of those that this agent requires") -;
;;; (declare-method requires "return the list of things the agent requires") -;
;;; (declare-method resolve-agent "part of the monitor set, resolves the outstanding issues with each agent (if any)") -;
;;; (declare-method rest-recover-check "determine how imperative rest/recovery is") -;
;;; (declare-method run-agents "This routine runs a list of agents using the passed 'run' procedure") -;
;;; (declare-method run-at "schedule a tick at a particular time") -;
;;; (declare-method run-model-body "run the body of a submodel") -;
;;; (declare-method run "run an agent") -;
;;; (declare-method run-subsidiary-agents "run agents in the active-subsidiary-agent list") -;
;;; (declare-method rvalue "returns the r value (sharpness or exponent) of an ecoservice's growth model") -;
;;; (declare-method scale "returns the scale vector or the scale associated with a nominated ordinate") -;
;;; (declare-method scale! "scale!") -;
;;; (declare-method schedule-epsilon "return a schedule's epsilon") -;
;;; (declare-method schedule-times "return a schedule") -;
;;; (declare-method service-list-index "returns the index of a species in the species list") -;
;;; (declare-method service-list% "service-list%") ;; returns value -;
;;; (declare-method service-matrix-index "returns the index of a species in the species matrix") -;
;;; (declare-method service? "service?") -;
;;; (declare-method service "service") ;; returns value -;
;;; (declare-method service-sites "returns the list of patches with particular services") -;
;;; (declare-method services% "services") ;; returns value -;
;;; (declare-method service-values "values of species/services (aggregates things with the same id)") ;; returns value -;
;;; (declare-method set-agent-state! "set the agent-state") -;
;;; (declare-method set-age! "set the age of the animal") -;
;;; (declare-method set-centre! "returns the centre/centroid of an object") -;
;;; (declare-method set-comment! "reference information for agent") -;
;;; (declare-method set-default-font "sets the font for production quality output when the file is opened") -;
;;; (declare-method set-dim! "Set the dimensionality of an entity -- (x y z) would be 3'") -;
;;; (declare-method set-direction! "Set the direction of an entity") -;
;;; (declare-method set-dt! "change the dt") -;
;;; (declare-method set-introspection-targets! "set the list of agents to be examined") -;
;;; (declare-method set-introspection-times! "set the list of agents to be examined") -;
;;; (declare-method set-jiggle! "reset the jiggle") -;
;;; (declare-method set-kernel! "change the kernel query function") -;
;;; (declare-method set-local->model! "Sets the default projection for mapping into model space from a submodel") -;
;;; (declare-method set-location! "Set the location of an entity") -;
;;; (declare-method set-mass! "Set the mass of an entity") -;
;;; (declare-method set-migration-test! "set/change the migration test function") -;
;;; (declare-method set-model->local! "Sets the default projection for mapping into a submodel's space") -;
;;; (declare-method set-name! "Set the unique name") -;
;;; (declare-method set-parameters! "set parameters using a list") -;
;;; (declare-method set-population-dynamics! "Sets things up for population dynamics using differential equations and rk4*") -;
;;; (declare-method set-priority! "change the run priority") -;
;;; (declare-method set-projection-assoc-list! "sets the list of available projections") -;
;;; (declare-method set-queue-state! "like set-agent-state, but says what the agent is doing queue-wise") -;
;;; (declare-method set-radius! "sets the radius for circles") -;
;;; (declare-method set-representation! "record the agent's representation") -;
;;; (declare-method set-schedule-epsilon! "set the epsilon for the schedule") -;
;;; (declare-method set-schedule-times! "set the times the agents is scheduled to do something") -;
;;; (declare-method set-services! "set-services!") ;; sets value -;
;;; (declare-method set-sex! "set the sex of the animal") -;
;;; (declare-method set-speed! "Set the speed of an entity") -;
;;; (declare-method set-state-flag! "set a flag in the ad hoc state-variable list") -;
;;; (declare-method set-subjective-time! "adjust the agents view of 'now'") -;
;;; (declare-method set-system-dynamics! "(re)set the system of equations in a diffeq-system") -;
;;; (declare-method set-taxon! "set the taxon of the agent") -;
;;; (declare-method set-timestep-schedule! "change the timestep list") -;
;;; (declare-method set-track! "set the track to the supplied 'track' list") -;
;;; (declare-method set-type! "set/change the type of an agent") -;
;;; (declare-method set-value! "set a value -- in the case of the environment, its default value") -;
;;; (declare-method set-variables! "set the list of variables") -;
;;; (declare-method sex "return the sex of the animal") -;
;;; (declare-method snapshot "used by logging mechanism") -;
;;; (declare-method spatial-scale "an indication of some 'natural scale' of the habitat based on the dist between patches") -;
;;; (declare-method specific-services% "names of species (or services)")  -;
;;; (declare-method speed "Return the speed of an entity") -;
;;; (declare-method state-flag "return the value of the flag in the ad hoc state-variable list") -;
;;; (declare-method stats-callback "call to collect statistics") -;
;;; (declare-method subjective-time "the agent's current time") -;
;;; (declare-method symbol "returns the symbol associated with the service") -;
;;; (declare-method taxon "returns an agent's taxon") -;
;;; (declare-method terminated? "is the agent terminated") -;
;;; (declare-method terminate "mark an agent as terminated") -;
;;; (declare-method timestep-schedule "a list of scheduled timesteps") -;
;;; (declare-method total-capacity "returns the total capacity of all the niches") -;
;;; (declare-method total-value "description needed") -;
;;; (declare-method track-location! "Insert the time and location in the track list") -;
;;; (declare-method track "return the entity's current track") -;
;;; (declare-method tracks "returns a list of track segments (or false)") -;
;;; (declare-method transfer-agent "transfers an agent to the kernel's runqueue") -;
;;; (declare-method type "the categorical type of agent (analogous to species or genus)") -;
;;; (declare-method value "typically the value of an ecoservice or patch") -;
;;; (declare-method wander-around "Wander around with a bias toward a nomintated point") -;




;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
