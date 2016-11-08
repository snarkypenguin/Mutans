(include "framework")
;--
;	landscape-declarations.scm -- Written by Randall Gray 

;--- <ecoservice> generics (also <patch>)

(declare-method add! "add!")
(declare-method scale! "scale!")
(declare-method enable-growth! "disables the growth model for the ecoservice")
(declare-method disable-growth! "disables the growth model for the ecoservice")
(declare-method growth-model "returns the growth model for the ecoservice")
(declare-method rvalue "returns the r value (sharpness or exponent) of an ecoservice's growth model")

;--- <boundary>, <circle> and <polygon> generics
(declare-method contains? "indicates whether a location is within a boundary object")
(declare-method my-rep "returns the underlying agent which defines the domain")
(declare-method distance-to-boundary "returns the distance to a boundary")
(declare-method rep "returns the representation object")
(declare-method centre "returns the centre/centroid of an object")
(declare-method set-centre! "returns the centre/centroid of an object")
(declare-method random-point "returns a random point within the object")

;--- <patch> generics 

(declare-method install-boundary "set/reset the boundary for a patch")
(declare-method service? "service?")
(declare-method add-service "add-service")
(declare-method remove-service "remove-service")
(declare-method service "service") ;; returns value
(declare-method service-list% "service-list%") ;; returns value
;;(declare-method service-list "service-list") ;; returns value
(declare-method services% "services") ;; returns value
(declare-method specific-services% "names of species (or services)") 
(declare-method service-values "values of species/services (aggregates things with the same id)") ;; returns value
(declare-method set-services! "set-services!") ;; sets value
(declare-method distance-to-centre "distance-to-centre")
(declare-method distance-to-interior "distance-to-interior")
(declare-method radius "radius")
(declare-method set-radius! "sets the radius")
(declare-method capacity "returns the carrying capacity of an ecoservice")
(declare-method total-capacity "returns the total capacity of all the niches")

;--- <dynamic-patch> generics 

(declare-method service-list-index "returns the index of a species in the species list")
(declare-method service-matrix-index "returns the index of a species in the species matrix")
(declare-method set-population-dynamics! "Sets things up for population dynamics using differential equations and rk4*")
(declare-method define-population-dynamics!"Sets things up for population dynamics using definitions, alternative to set-population-dynamics!")
(declare-method growth-model "returns the growth model for the patch (ties all the represented populations together)")
(declare-method enable-growth! "enables the system level growth model for the dynamic-patch")
(declare-method disable-growth! "disables the system level growth model for the dynamic-patch")
(declare-method enable-service-growth! "enables the service level growth model for a service in the dynamic-patch")
(declare-method disable-service-growth! "disables the service level growth model for for a service in the dynamic-patch")
(declare-method enable-all-service-growth! "enables the service level growth model for the dynamic-patch")
(declare-method disable-all-service-growth! "disables the service level growth model for the dynamic-patch")




;--- <landscape> generics

;--- <environment> generics
;(declare-method contains? "contains?")
;(declare-method value "value")
;(declare-method set-value! "set-value!")
;(declare-method min-bound "min-bound")
;(declare-method max-bound "max-bound")
(declare-method mean-value "mean value of a resource or service")

;--- <habitat> (and possibly <gridenv>) generics

(declare-method service-sites "returns the list of patches with particular services")
(declare-method add-patch "add a patch to a habitat")
(declare-method remove-patch "remove a patch from a habitat")
(declare-method patch-list% "return a list of all patches or only the ones with particular services")
(declare-method aggregate-value "aggregate-value of a services within a nominated circle")
(declare-method spatial-scale "an indication of some 'natural scale' of the habitat based on the dist between patches")

;(declare-method contains? "contains?")  -- defined for environment
;(declare-method value "value")      -- defined for environment
;(declare-method set-value! "set-value!") -- defined for environment
;(declare-method min-bound "min-bound")  -- defined for environment
;(declare-method max-bound "max-bound")  -- defined for environment
;(declare-method services "services") ;; returns patches


;; where there are queries for things like value

(declare-method populate-cells "sets up the cell agents")
(declare-method dimensions "returns the number of dimensions")
(declare-method origin "returns the origin")
(declare-method scale "returns the scale vector or the scale associated with a nominated ordinate")
(declare-method ordinates "returns the indices for a given location")
(declare-method patch-at "returns a list of any patches containing a supplied location")


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
