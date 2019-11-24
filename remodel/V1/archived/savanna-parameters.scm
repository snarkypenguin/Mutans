;-  Identification and Changes

;--
;	savanna-parameters.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2014.10.12
;		Location: pooh:/local/home/randall/study/src/savanna-parameters.scm
;
;	History:

;-  Discussion 

;; The plants are toxic when young, so the herbivores only eat mature plants.

;; The carnivores can only catch herbivores which are 1/10 their own size
;;     the carnivores also use older plants for breeding (which kills the plant)


;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(load 'load-list?)

(define (basic-plant t species cap gr as-predator-ratio predator-efficiency mort as-prey-ratio populations)
  (let ((grow1 (+ 1.0 (* gr (sin (* t  piyr)))))
  		  (grow2 (apply + (map * as-predator-ratio predator-efficiency populations)))
  		  (mort1 mort)
  		  (mort2 (apply + (map * as-prey-ratio populations))))
  	 (dnl "Populations: " populations)
  	 (dnl "plant (" species "): grow1 " grow1 ", grow2 " grow2 ", mort1 " mort1 ", mort2 " mort2)
  	 (dnl " = "(growth-terms-- (+ 1.0 (* gr (sin (* t  piyr))))  (apply + (map * as-predator-ratio predator-efficiency populations))))
  	 (dnl "   + " (mortality-terms-- (max 0 (* mort (- cap biomass))) (apply + (map * as-prey-ratio populations))))
  	 )
  (growth-terms-- (+ 1.0 (* gr (sin (* t  piyr))))  (apply + (map * as-predator-ratio predator-efficiency populations))
						(mortality-terms-- (max 0 (* mort (- cap biomass))) (apply + (map * as-prey-ratio populations))))
  )

(define (basic-animal t species cap gr as-predator-ratio predator-efficiency mort as-prey-ratio populations)
  (let ((grow1 (+ 0.95 (* gr (sin (* t piyr)))))
		  (grow2 (apply + (map * as-predator-ratio predator-efficiency populations)))
		  (mort1 mort)
		  (mort2 (apply + (map * as-prey-ratio populations))))
	 (dnl "Populations: " populations)
	 (dnl "animal (" species "): grow1 " grow1 ", grow2 " grow2 ", mort1 " mort1 ", mort2 " mort2)
	 (dnl " = " (growth-terms-- (+ 0.95 (* gr (sin (* t  piyr))))  (apply + (map * as-predator-ratio predator-efficiency populations))))
	 (dnl "   +  " (mortality-terms-- (max 0 (* mort (- cap biomass))) (apply + (map * as-prey-ratio populations))))
	 )
  (growth-terms-- (+ 0.95 (* gr (sin (* t  piyr)))) (apply + (map * as-predator-ratio predator-efficiency populations))
							(mortality-terms-- (max 0 (* mort (- cap biomass))) (apply + (map * as-prey-ratio populations))))
  )

(define grass-dt 4)
(define cow-dt 8)
(define leopard-dt 16)
(define piyr (/ (* 2.0 (acos -1.0)) 365.25))


(define savanna-species-individuals ;; used in the rk4* equations
  (let ((grscale 1.0) ;; growth scale
		  (mrscale 1.0) ;; mortality scale
		  )
	;; mort is the natural mortality, growth is the non-sigmoidal growth rate (exponential)
	;; Basic species data (name, type, growth rate, mortality rate, age at "max mass"
	 
	 (list (list "vegetation" 'plant 'g
					 (* grscale 1.0)
					 (* mrscale 0.3)
					 (years 3.0) basic-plant)
			 (list "herbivore" 'herbivore 'h
					 (* grscale 0.66)
					 (* mrscale 0.01)
					 (years 8.0)
					 basic-animal)
			 (list "carnivore" 'carnivore 'c
					 (* grscale 0.0)
					 (* mrscale 0.6)
					 (years 10.0)
					 basic-animal)
			)
		)
	)


(define savanna-predation-matrix 
  ;; competition matrix, really ... the mortality(c)/predation(r) associated with an organism 
  ;; a strict grass-sheep-wolf relationship would be along the subdiagonal
  (make-matrix 
	;; (g h c)
	'((0   0   0)      ;; g
	  (0.3 0   0)      ;; h 
	  (0   0.06 0)      ;; c
	 )))


(define savanna-efficiency-matrix 
  ;; says it all really ... efficiency convert from the victim mass to victor mass
  ;; a strict grass-sheep-wolf relationship would be along the subdiagonal
  (make-matrix 
	;; (g h c)
	'((0.0 0.0 0.0) ;; g
	  (0.6 0.0 0.0) ;; h
	  (0.0 0.4 0.0) ;; c
	 )))

;;; cf landscape-methods.scm:81... We need to be able to do this....******
;(define savanna-growth-functions (list (lambda (t . populations)


(define savanna-population
  (make-population-structure
	
	;; Defines the basic predatory interaction rates between species
	savanna-predation-matrix

	;; Defines the basic predatory interaction rates between species
	savanna-efficiency-matrix

	;; Basic species data (name, type growth rate, mortality rate, age at "max mass"
	savanna-species-individuals

	;; Patch template for population level data.  "r" is the "sharpness" parameter for sigmoidal growth
	(list
	 (list 0.5  ;prob  name         class      MaxCap  CurVal   dt            r    dTmax
			 (list 0.99  "vegetation" 'grass      31000   20000   grass-dt   1       16)
			 (list 0.99  "herbivore"  'herbivore  4100      300   cow-dt     1       20)
			 (list 0.99  "carnivore"  'carnivore  310        15   leopard-dt 1.0001  12) 
			 )
	 (list 0.5
			 (list 0.99  "vegetation" 'grass      32000   18000   grass-dt   1       16)
			 (list 0.99  "herbivore"  'herbivore  4200      500   cow-dt     1       20)
			 (list 0.99  "carnivore"  'carnivore  320        20   leopard-dt 1.0001  12) 
			 )
	 (list 0.5
			 (list 0.99  "vegetation" 'grass      33000   22000   grass-dt   1       16)
			 (list 0.99  "herbivore"  'herbivore  4300      800   cow-dt     1       20)
			 (list 0.99  "carnivore"  'carnivore  330       100   leopard-dt 1.0001  12) 
			 )
	 (list 0.5
			 (list 0.99  "vegetation" 'grass      33000   24300   grass-dt   1       16)
			 (list 0.99  "herbivore"  'herbivore  4300      400   cow-dt     1       20)
			 (list 0.99  "carnivore"  'carnivore  330        10   leopard-dt 1.0001  12) 
			 )
	;;; (list
	;;;  (list 0.5  ;prob  name         class      MaxCap  CurVal   dt         r    dTmax 
	;;; 		 (list 0.99  "vegetation" 'grass      41000   20000    grass-dt   1       16)
	;;; 		 (list 0.99  "herbivore"  'herbivore  4100      400    cow-dt     1       20)
	;;; 		 (list 0.99  "carnivore"  'carnivore  310         4    leopard-dt 1.0001  12) 
	;;; 		 )
	;;;  (list 0.5
	;;; 		 (list 0.99  "vegetation" 'grass      42000   18000    grass-dt   1       16)
	;;; 		 (list 0.99  "herbivore"  'herbivore  4200      400    cow-dt     1       20)
	;;; 		 (list 0.99  "carnivore"  'carnivore  320        12    leopard-dt 1.0001  12) 
	;;; 		 )
	;;;  (list 0.5
	;;; 		 (list 0.99  "vegetation" 'grass      43000   22000    grass-dt   1       16)
	;;; 		 (list 0.99  "herbivore"  'herbivore  4300      600    cow-dt     1       20)
	;;; 		 (list 0.99  "carnivore"  'carnivore  330         8    leopard-dt 1.0001  12) 
	;;; 		 )
	;;;  (list 0.5
	;;; 		 (list 0.99  "vegetation" 'grass      43000   24300    grass-dt   1       16)
	;;; 		 (list 0.99  "herbivore"  'herbivore  4300      300    cow-dt     1       20)
	;;; 		 (list 0.99  "carnivore"  'carnivore  330        20    leopard-dt 1.0001  12) 
	;;; 		 )
	 )
	)
  )


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
;--    Static data

;--    Public data 

;-  Code 

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
