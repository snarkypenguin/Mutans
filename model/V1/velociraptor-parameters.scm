;-  Identification and Changes

;--
;	velociraptor-parameters.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2014.10.12
;		Location: pooh:/local/home/randall/study/src/velociraptor-parameters.scm
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
(define (velociraptor-brain me age t dt condn stconts gutsz)
;  (dnl "focussing with " t "@" dt " c:" condn " s:" stconts" g:" gutsz)
  (cond
	((or (< stconts (* 0.5 gutsz)) (< condn 0.3)) 'hungry)
	(#f 'flee)
	(#f 'breed)
	(#t 'wander)
	(else #f)))

(define wallymass 160.0)
(define wilmamass 180.0)
;;(set! wallymass 98.0)
;;(set! wilmamass 108.0)

(define (condition m) (* 0.2 m))


(define V-species-individuals
  (let ((grscale 1.0)
		  (mrscale 1.0)
		  )
	;; Basic species data (name, type growth rate, mortality rate, age at "max mass"
	(list (list "grass" 'grass 'G (* grscale 0.008) (* mrscale 0.0) (years 0.25))
			(list "hay" 'grass 'h (* grscale 0.008) (* mrscale 0.0) (years 0.25))
			(list "myrtle" 'tree 'm (* grscale 0.0023) (* mrscale 0.0) (years 50.))
			(list "eucalypt" 'tree 'e (* grscale 0.0019) (* mrscale 0.0) (years 120))
			(list "wattle" 'tree 'w (* grscale 0.0029) (* mrscale 0.0) (years 10))
			(list "rabbit" 'rabbit 'r (* grscale 0.0008) (* mrscale 0.1) (years 0.3))
			(list "sheep" 'caprid 's (* grscale 0.00075) (* mrscale 0.1) (years 4))
			(list "goat" 'caprid 'g (* grscale 0.00075) (* mrscale 0.1) (years 4))
			(list "beef-cattle" 'bovine 'b (* grscale 0.0007) (* mrscale 0.1) (years 5))
			(list "dairy-cattle" 'bovine 'd (* grscale 0.0007) (* mrscale 0.1) (years 4))
			(list "bos-indicus" 'bovine 'B (* grscale 0.0007) (* mrscale 0.1) (years 6))
			(list "pig" 'pig 'p (* grscale 0.07) (* mrscale 0.003) (years 12))
			(list "velociraptor" 'predator 'V (* grscale 0.0006) (* mrscale 0.3) (years 14))
			)
		)
	)

(define V-predation-matrix 
  (if #t
		;; Defines the basic predatory interaction rates between species
		(make-matrix
		 ;;                       PREY
		 ;; Predation by the organism
		 ;;(G     h    m     e    w     r   s  g  b    d   B   p   V)    ;;   PREDATORS
		 '((0     0    0     0    0     0   0  0  0    0   0   0   0)    ;; G    P  o  O 
			(0     0    0     0    0     0   0  0  0    0   0   0   0)    ;; h    r  n  r
			(0     0    0     0    0     0   0  0  0    0   0   0   0)    ;; m    e     a
			(0     0    0     0    0     0   0  0  0    0   0   0   0)    ;; e    d  t  n
			(0     0    0     0    0     0   0  0  0    0   0   0   0)    ;; w    a  h  i
			(.5   .5   .01   .01   .01   0   0  0  0    0   0   0   0)    ;; r    t  e  s
			(.5  .5  .1   .01   .01   0   0  0  0    0   0   0   0)       ;; s    i     m
			(.21 .22 .02   .02   .02   0   0  0  0    0   0   0   0)      ;; g    o      
			(.18 .14 .03   .03   .03   0   0  0  0    0   0   0   0)      ;; b    i
			(.23 .25 .02   .02   .02   0   0  0  0    0   0   0   0)      ;; d    n
			(.18 .14 .03   .03   .03   0   0  0  0    0   0   0   0)      ;; B    
			(.25 .27 .007  .007 .007 .01   .3 .2 .13  .02 .02 .05 .05)    ;; p          
			(0    0    0     0    .15  .3    .3 .3 .3   .3  .3  .3  .05)  ;; V    
			)
		 )
		(make-matrix 
		 (copy-list (make-list 13 (make-list 13 0)))
		 )
		)
  )

(define V-efficiency-matrix
  (make-matrix 
	(copy-list (make-list 13 (make-list 13 0.3)))))


(define (pasture-or-hay p) (if (< (random-real) p) "pasture" "hay-crop"))

(define V-rich-population
  (make-population-structure 

	;; Defines the basic predatory interaction rates between species
	 V-predation-matrix

	;; Defines the basic predatory interaction rates between species
	 V-efficiency-matrix

	;; Basic species data (name, type growth rate, mortality rate, age at "max mass"
	V-species-individuals

	;; Patch template for population level data.  "r" is the "sharpness" parameter for sigmoidal growth
	(list 
	 (list 0.2   ;prob  name   class   MaxCap   CurVal dt r dTmax
			 (list 0.025 "myrtle" 'tree 300100 30100 3 (years 180))
			 (list 0.8 "wattle" 'tree 200100 20100 3 1 (years 30))
			 (list 0.9 (pasture-or-hay 0.3) 'grass 1400100 140100 3 1 (years 0.3))
			 (list 0.3 "rabbit" 'rabbit 2010 210 3 1 (years 0.8))
			 (list 0.5 "sheep" 'caprid 10100 1100 3 1 (years 13))
			 (list 0.5 "goat" 'caprid 6010 610 3 1 (years 14))
			 )
	 (list 0.2
			 (list 0.8 "wattle" 'tree 300200 30100 3 1 (years 160))
			 (list 0.9 (pasture-or-hay 0.7) 'grass 1400200 140200 3 1 (years 0.28))
			 (list 0.45 "rabbit" 'rabbit 2200 220 3 1 (years 0.78))
			 (list 0.5 "sheep" 'caprid 10200 1200 3 1 (years 12))
			 (list 0.5 "goat" 'caprid 6020 620 3 1 (years 13))
			 )
	 (list 0.2
			 (list 0.3 "wattle" 'tree 200300 20300 3 1 (years 170))
			 (list 0.5 "gum" 'tree 200300 20300 3 1 (years 280))
			 (list 0.9 (pasture-or-hay 0.5) 'grass 1400300 140300 3 1 (years 0.3))
			 (list 0.3 "rabbit" 'rabbit 2030 230 3 1 (years 0.8))
			 (list 0.85 "dairy-cattle" 'bovine 2030 230 3 1 (years 4))
			 )
	 (list 0.2
			 (list 0.8 "wattle" 'tree 200400 20400 3 1 (years 0.160))
			 (list 0.9 (pasture-or-hay 0.9) 'grass 1400400 140400 3 1 (years 0.8))
			 (list 0.1 "rabbit" 'rabbit 2040 240 3 1 (years 0.3))
			 (list 0.825 "beef-cattle" 'bovine 3040 340 3 1 (years 5))
			 (list 0.5 "Bos-indicus" 'bovine 2040 240 3 1 (years 6))
			 )
	 (list 0.2
			 (list 0.8 "wattle" 'tree 200500 20500 3 1 (years 150))
			 (list 0.9 (pasture-or-hay 0.9) 'grass 1400500 140500 3 1 (years 0.4))
			 (list 0.7 "rabbit" 'rabbit 2050 250 3 1 (years 0.28))
			 (list 0.8 "Bos-indicus" 'bovine 2050 250 3 1 (years 6))
			 )
	 (list 0.2
			 (list 0.9 (pasture-or-hay 0.05) 'grass 20006000 2006000 3 1 (years 0.4))
			 (list 0.8 "rabbit" 'rabbit 6060 660 3 1 (years 0.3))
			 (list 0.8 "goat" 'caprid 166 16 3 1 (years 12))
			 )
	 (list 0.2
			 (list 0.025 "myrtle" 'tree 30007000 3007000 3 1 (years 180))
			 (list 0.125 "gum" 'tree 8070 870 3 1 (years 220))
			 (list 0.8 "wattle" 'tree 20007000 2007000 3 1 (years 80))
			 (list 0.9 (pasture-or-hay 0.95) 'grass 140700 14700 3 1 (years 0.3))
			 (list 0.9 "rabbit" 'rabbit 2070 270 3 1 (years 0.3))
			 (list 0.5 "pig" 'pig 1070 170  3 1 (years 20))
			 )
	 (list 0.2
			 (list 0.025 "myrtle" 'tree 30008000 3008000 3 1 (years 150))
			 (list 0.125 "gum" 'tree 8080 880 3 1 (years 200))
			 (list 0.8 "wattle" 'tree 20008000 2008000 3 1 (years 70))
			 (list 0.8 "pig" 'pig 1080 180 3 1 (years 15))
			 )
	 ))
  )


(define V-sparse-population
  (make-population-structure
	
	;; Defines the basic predatory interaction rates between species
	V-predation-matrix

	;; Defines the basic predatory interaction rates between species
	 V-efficiency-matrix

	;; Basic species data (name, type growth rate, mortality rate, age at "max mass"
	V-species-individuals

	;; Patch template for population level data
	(list
	 (list 0.5  ;prob  name   class   MaxCap   CurVal dt r dTmax
			 (list 0.025 "myrtle" 'tree 30000 3000 3 1 (years 180))
			 (list 0.8 "wattle" 'tree 200000 20000 3 1 (years 50))
			 (list 0.9 (pasture-or-hay 0.5) 'grass 1400000 140000 3 1 (years 0.3))
			 (list 0.3 "rabbit" 'rabbit 2000 200 3 1 (years 0.8))
			 (list 0.5 "sheep" 'caprid 10000 1000 3 1 (years 12))
			 )
	 (list 0.5
			 (list 0.8 "wattle" 'tree 310000 31000 3 1 (years 75))
			 (list 0.9 (pasture-or-hay 0.5) 'grass 1410000 141000 3 1 (years 0.4))
			 (list 0.45 "rabbit" 'rabbit 6100 610 3 1 (years 0.85))
			 (list 0.5 "goat" 'caprid 6100 610 3 1 (years 14))
			 )
	 (list 0.5
			 (list 0.3 "wattle" 'tree 220000 22000 3 1 (years 40))
			 (list 0.5 "gum" 'tree 220000 22000 3 1 (years 220))
			 (list 0.9 (pasture-or-hay 0.5) 'grass 1420000 142000 3 1 (years 0.3))
			 (list 0.1 "rabbit" 'rabbit 2200 220 3 1 (years 0.8))
			 (list 0.25 "dairy-cattle" 'bovine 23000 2300 3 1 (years 6))
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
