;-  Identification and Changes

;--
;	seabed-parameters.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2014.10.12
;		Location: pooh:/local/home/randall/study/src/seabed-parameters.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2014 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(load 'load-list?)
(define gsr-species-individuals ;; used in the rk4* equations
  (let ((grscale 0.0)
		  (mrscale 0.0)
		  )
	;; mort is the natural mortality, growth is the non-sigmoidal growth rate (exponential)
	;; Basic species data (name, type growth rate, mortality rate, age at "max mass"
	(list (list "seagrass" 'grass 'G (* grscale 0.04) (* mrscale 0.0005) (years 0.25))
			(list "sponge" 'sponge 's (* grscale 0.008) (* mrscale 0.0005) (years 0.25))
			(list "reef" 'reef 'r (* grscale 0.0023) (* mrscale 0.0005) (years 50.))
			)
		)
	)


(define gsr-predation-matrix ;; competition matrix, really ... the mortality/growth associated with an interaction
  (make-matrix 
	;; (g s r)
	'((0      0.0004 0.0002) ;; g
	  (0.0003 0      0.0005) ;; s 
	  (0.0005 0.0005 0)      ;; r
	 )))


(define gsr-efficiency-matrix ;; says it all really ... efficiency convert from the victim mass to victor mass
  (make-matrix 
	;; (g s r)
	'((0 0.3 0.3) ;; g
	  (0.2 0 0.2) ;; s 
	  (0.2 0.2 0) ;; r
	 )))



(define gsr-population
  (make-population-structure
	
	;; Defines the basic predatory interaction rates between species
	gsr-predation-matrix

	;; Defines the basic predatory interaction rates between species
	gsr-efficiency-matrix

	;; Basic species data (name, type growth rate, mortality rate, age at "max mass"
	gsr-species-individuals

	;; Patch template for population level data.  "r" is the "sharpness" parameter for sigmoidal growth
	(list
	 (list 0.5  ;prob  name   class   MaxCap   CurVal dt r dTmax
			 (list 0.99 "sponge" 'sponge 41000 4100 3 1 (years 20))
			 (list 0.99 "seagrass" 'grass 41000 4100   3  1 (years 5))
			 (list 0.99 "reef" 'reef 41000 6100 300 1.0001 (years 275))
			 )
	 (list 0.5
			 (list 0.99 "sponge" 'sponge 42000 6200 3 1 (years 20))
			 (list 0.99 "seagrass" 'grass 42000 4200   3  1 (years 5))
			 (list 0.99 "reef" 'reef 42000 4200 300 1.0001 (years 275))
			 )
	 (list 0.5
			 (list 0.99 "sponge" 'sponge 43000 4300 3 1 (years 20))
			 (list 0.99 "seagrass" 'grass 43000 6300   3  1 (years 5))
			 (list 0.99 "reef" 'reef 43000 4300 300 1.0001 (years 275))
			 )
	 (list 0.5
			 (list 0.99 "sponge" 'sponge 43000 4300 3 1 (years 20))
			 (list 0.99 "seagrass" 'grass 43000 4300   3  1 (years 5))
			 (list 0.99 "reef" 'reef 43000 4300 3000 1.0001 (years 275))
			 )
	 ))
  )


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:






