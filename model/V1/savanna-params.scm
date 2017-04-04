;-  Identification and Changes

;--
;	savanna-params.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2014.11.06
;		Location: pooh:/local/home/randall/study/src/savanna-params.scm
;
;	History:
;

(load "population-support")

(define grass-dt 4)
(define zebra-dt 8)
(define leopard-dt 16) 
(define piyr (/ (* 2.0 (acos -1.0)) 365.25))


;; This  is a mess. We cannot instantiate either the dP function or the differential-model till we know the cap.
;; Worse, we'll need a new closure for each agent, lest the 'multiply associated with things like capacities mess up the
;; captured variables  in the closure returned in the species parameters block.  I think the word for it all is "Bugger"


;; The that code that creates patches will typically take one or more of these and introduce additional parameters which
;; are added to a copy of the a-lists (below) and then passed to the instantiation code.

;; 


(define savanna-species-parameters
  (let ((grscale 1.0) ;; growth scale
		  )
	 (define-species-parameters-list
	  ;;	             "name"       'class     'symbol   specific-parameter
		(define-species "vegetation" 'plant     'p        (define-parameters
																			 ;; ecoservice level
																			 (var 'growth-func Logistic)
																			 (var 'growth 1.001)
																			 (var 'amplitude 30)
																			 (var 'offset 0)
																			 (var 'K/ha 3000)
																			 ;; interactions
																			 
																			 ))
	  		  ;; growth rate of 30DMkg/(ha*day) ... 
			  ;;Source: Information was originally compiled for MLA’s More Beef from Pastures manual and the PROGRAZE® Manuals, developed cooperatively with State Agriculture/Primary Industries Departments in NSW, Victoria, Tasmania, South Australia and Western Australia.
			  ;;New South Wales – pasture growth rate patterns Source: NSW PROGRAZE® Notes, Appendix 4
			  ;; predation-dynamics function

		(define-species "zebra"      'herbivore 'h        (define-parameters
																			 ;; ecoservice level
																			 (var 'growth 1.0025)
																			 (var 'K/ha 1600)
																			 ;; interactions
																			 ))

		(define-species "leopard"    'carnivore 'c        (define-parameters
																			 ;; ecoservice level
																			 (var 'growth 1.002)
																			 (var 'K/ha 300)
																			 ;; interactions
																			 ))
	  )
	  ))
	  ;; Standard parameters will be the symbols growth_p, growth_i, K/ha, amplitude, offset, fecundity, broodsize, 
	  ;; age_breed, age_max, M_o, M_max
	  ;; ... and more as we think of them.  Clearly not all are appropriate for any given model or organism.





(define savanna-predation-matrix 
  ;; This implies that the there is a take-from-one/give-to-the-other relationship
  ;; a strict grass-sheep-wolf relationship would be along the subdiagonal
  (make-matrix 
	;; (p h c)
	'((0   0   0)      ;; p
	  (0.3 0   0)      ;; h 
	  (0   0.06 0)     ;; c
	  )))

(define savanna-efficiency-matrix 
  ;; says it all really ... efficiency convert from the victim mass to victor mass
  ;; a strict grass-sheep-wolf relationship would be along the subdiagonal
  (make-matrix 
	;; (p h c)
	'((0.0 0.0 0.0) ;; p
	  (0.6 0.0 0.0) ;; h
	  (0.0 0.4 0.0) ;; c
	  )))

(define savanna-interaction-efficiency-matrix 
  ;; doesn't imply conflict -- presence of particular plants may benefit a bird w/o a reciprocal relationship
  ;; a strict grass-sheep-wolf relationship would be along the subdiagonal
  (make-matrix 
	;; (p h c)
	'((0 0 0)      ;; p
	  (0 0 0)      ;; h 
	  (0 0 0)     ;; c
	  )))


;; Patch/landscape specific stuff
(define savanna-patch-template
  ;; Patch template for population level data.  "r" is the "sharpness" parameter for sigmoidal growth
  (list
	(list 0.5  ;prob  name/class      MaxCap  CurVal   dt             dTmax
			(list 0.99  "vegetation"    301000   20000   grass-dt         16)
			(list 0.99  'herbivore        4100     300   zebra-dt         20)
			(list 0.99  'carnivore         310      15   leopard-dt       12) 
			)
	(list 0.5
			(list 0.99  'plant          302000   18000   grass-dt         16)
			(list 0.99  'herbivore        4200     500   zebra-dt         20)
			(list 0.99  'carnivore         320      20   leopard-dt       12) 
			)
	(list 0.5
			(list 0.99  "vegetation"    303000   22000   grass-dt         16)
			(list 0.99  "herbivore"       4300     800   zebra-dt         20)
			(list 0.99  "carnivore"        330     100   leopard-dt       12) 
			)
	(list 0.5
			(list 0.99  "vegetation"    303000   24300   grass-dt         16)
			(list 0.99  "herbivore"       4500     400   zebra-dt         20)
			(list 0.99  "carnivore"        330      10   leopard-dt       12) 
			)
	)

  )







;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
