; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	monitor-classes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.08.01
;		Location: zero:/home/randall/Thesis/Example-Model/model/monitor-classes.scm
;
;	History:
;
;-  Code 
(include "framework")

(define-class <monitor>
	(inherits-from <introspection>)
	(state-variables
	 KnownGood KnownBad
	 )
	)

"<monitor> agents play a special role, keeping an eye on subsets of
the population of agents.  Monitors can effect changes in the
composition of both the population and the constituents which comprise
an agent. In many ways, monitors are similar to introspection agents."

(define-class <simple-monitor>
  (inherits-from <monitor>)
  (state-variables target-class trigger-selector)
  ;; the monitor only selects things that match (isa? x target-class)
  ;; trigger-selector is passed "self" ... does whatever it does, and either returns #f to indicate
  ;;     no action to be taken, #t to indicate that change is in the offing, or a class to migrate to
  )

(define-class <ensemble-trigger>
  (inherits-from <monitor>)
  (state-variables target-class-list trigger-selector)
  ;; the monitor only selects things that match (isa? x target-class) where target-class is in 
  ;;    target-class-list
  ;; trigger-selector is passed "self" ... does whatever it does, and either returns #f to indicate
  ;;    no action to be taken, #t to indicate that change is in the offing, or a class to migrate to
  )


;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
