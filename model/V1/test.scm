;-  Identification and Changes

;--
;	model.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2013.02.05
;		Location: odin:/home/gray/study/src/model.scm
;
;-  Code 


;(dump wally)(newline)
;(aborts "Incomplete initialisation is making things fail when it runs")


(load-model)

(define start 0)
;(define end 500)
;(define end 6.1)
(define end 741)

;;(define dumptimes 
(define schedtimes (append 
						 (cons 0 (seq 6))
						 (map (lambda (x) (* 10.0 (1+ x))) (seq 36)))
  ) ;; first six days, then on every tenth day from the beginning for 370 days



(define wally (make <thing> (list 'name "wally" 'type 'testy 'representation 'golem 'dt 0.5
			  'mass 0.0
			  'location '(100 200 40) 'direction '(1 0 0) 'dim 3)))

(define dumper (make <introspection> (list 'introspection-list (list wally) 'name "dumper" 'type 'tester 'timestep-schedule (seq 20))))

(run-simulation (list dumper wally) 0 20)





;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
