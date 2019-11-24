;-  Identification and Changes

;--
;	model-declarations.scm -- Written by Randall Gray 

;-  Discussion 
#|
This file ought to have "includes" for each logical group of classes.
The generics are all defined before we start adding classes to the system
because it Just Seems to Work Better.
|#


(include "landscape-declarations.scm")
(include "animal-declarations.scm")
;;(include "boat-declarations.scm")
;;(include "benthic-declarations.scm")
;;(include "fish-declarations.scm")
;;(include "fish-mgmt-declarations.scm")
;; ...



;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
