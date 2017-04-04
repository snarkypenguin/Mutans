(load "basic-population.scm")

(define default-schema '(
								 (0.025 "myrtle" tree 30000) 
								 (0.125 "gum" tree 80000)
								 (0.8 "wattle" tree 20000) 
								 (0.9 "pasture" grass 14000) 
								 (0.3 "hay-crop" grass 20000) 
								 (0.4 "rabbit" rabbit 2000) 
								 (0.5 "sheep" caprid 10000) 
								 (0.05 "goat" caprid 6000) 
								 (0.25 "dairy-cattle" bovine 23750)
								 (0.25 "beef-cattle" bovine 34500)
								 (0.05 "Bos-indicus" bovine 25500)
								 (0.5 "pig" pig 1500)
))


(define (make-patch schema)
  )


convert the ecoservice to something simpler,
make a means of defining d/dt *other* than using the "standard" way








;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
