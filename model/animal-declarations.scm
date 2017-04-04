(include "framework")
;-  Identification and Changes

;***** When you add things to this file (or any other *-declarations.scm file)
;***** you *must* run "make declarations.scm" or a more comprehensive make.
;*****
;***** This is because the model includes "declarations.scm" rather than the many
;***** other *-declaractions.scm files -- "declarations.scm" is filtered to exclude
;***** duplicate declarations, and this stops methods simply disappearing.

;--
;	animal-declarations.scm -- Written by Randall Gray 

;- <metabolism> generics
(declare-method eat "add food to the stomach, process food from the stomach, grow, return the amount eaten")


;--- <animal> generics
(declare-method age "return the age of the animal")
(declare-method set-age! "set the age of the animal")
(declare-method sex "return the sex of the animal")
(declare-method set-sex! "set the sex of the animal")
(declare-method wander-around "Wander around with a bias toward a nomintated point")

(declare-method eat "Eat food ")
(declare-method die "routine corresponding to the death of an animal (calls shutdown)")
(declare-method prey-present "returns a list of the animal's prey in a suitable locality")
(declare-method growth "does the business of growing")
(declare-method forage "move about looking for a region with food")
(declare-method crowded? "determine if the region is too crowded for the animal")
(declare-method change-territory "move to another region (change cells)")
(declare-method reproduce "create offspring")


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
