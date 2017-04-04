; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	exmod.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.11.18
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/exmod.scm
;
;	History:
;


;-- Assessment loop

(define (monitor-pass:alg1 current-niche-configuration-list #!rest args)
;; This deviates from Alg. 1 in that it doesn't actually *select* configurations 
  (for-each
	(lambda (n)
	  (for-each
		(lambda (s) ;; submodels in niche
		  (for-each
			(lambda (a) ;; agent in submodel
			  (let ((as (generate-agent-state a s n))
					  (ss (generate-submodel-state s n)))
				 (update-notes a s n as ss)
				 ))
			(agents-in s)
			))
		(submodels-in n)
		))
	(generate-niche-state)
	)

  (run-and-update-niche-assessment)
  (flag-whole-of-model-issues)
  (for-each
	(lambda (current-niche-config)
	  (if (and (is-untenable current-niche-config)
				  (not (necessary-config  current-niche-config)))
			(deprecate current-niche-config)))
	current-niche-configuration-list)

  (sort (current-niche-configuration-list) compare-niches)) 
	  
			
	  
;-- Fruit and seeds Niches

(define (fruit-production t domain)    ;; domain is an agent (possibly just a "number" agent)
  (query 'fruit-production domain t))  ;; t is time

(define (fruit-eaten domain)
  (query 'fruit-eaten domain))

(define (fruit-spoilage t domain)
  (query 'fruit-spoilage domain t))



(define (dN_F t domain) (- (fruit-production t domain) (fruit-spoilage t domain) (fruit-eaten domain)))
;; This is the instantaneous change in the quantity of fruit
;; domain corresponds to the whole domain, a cell or (possibly) some other subset of fruit

(define (dN_S t domain)
  (- (* (Seeds domain) (fruit-eaten domain) (* (- 1 (/ (plant-biomass t) K_p)) seed-germination-rate))))
;; This is the instantaneous change in the number of seeds
;; domain corresponds to the whole domain, a cell or (possibly) some other subset of fruit

(define 






;-  The End 


;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
