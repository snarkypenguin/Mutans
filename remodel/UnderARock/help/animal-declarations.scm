;-  Identification and Changes
;--
;	animal-declarations.scm -- Written by Randall Gray 

;- <metabolism> generics
(declare-method eat "add food to the stomach, process food from the
stomach, grow, return the amount eaten")


;--- <animal> generics
(declare-method age "return the age of the animal")
(declare-method set-age! "set the age of the animal")
(declare-method sex "return the sex of the animal")
(declare-method set-sex! "set the sex of the animal")
(declare-method wander-around
					 "Wander around with a bias toward a nomintated point")



;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
