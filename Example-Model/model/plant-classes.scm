(include "framework")
; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	plant-classes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.02
;		Location: zero:/home/randall/Thesis/Example-Model/model/plant-classes.scm
;
;	History:
;

;-  Code 

(define <simple-plant>
  (make-class (inherits-from <model-maintenance> <thing>)
				  (state-variables
					max-age ;; dies beyond this
					max-mass ;; unlikely to reach this
					age ;; age of plant in [0,4)
					lai ;; leaf area index in (0,10)
					water-stress  ;; [0,1]
					water-use ;; amount of water required for a square metre
					water-stress-effect ;; True if we factor water stress
					;; into fruiting
					reproduction-mass ;; how big it must be...
					reproduction-period ;; how long between reproductions
					reproduction-offset ;; 
					reproduction-mechanism ;; <fruit> or (list <agent> method val)
					fruiting-rate      ;; relative to mass, influenced by
											 ;; level of water stress
					seeds-per-fruit
					habitat            ;; #f or a landscape thing
					;; <thing> mass is our mass
					)))
(register-class <simple-plant>)




;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
