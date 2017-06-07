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

;; Monitors are derived from introspection since they really need to
;; be able to assess things in a temporally consistent way. The
;; timesteps used for agent-monitors would typically be shorter than
;; the timesteps for niche-level or domain-level monitors

(define-class <monitor>
	(inherits-from <introspection>)

	(state-variables selector ;; selects agents (used mostly in derived classes)
						  configuration-candidates ;; 
						  domain*-options ;; set of sets of trees associated with domains
						  niche*-options  ;; set of sets of trees associated with niches
						  assessment-function ;; compares set of states
						  history  ;; records transitions
						  target-list ;; target agents 
						  specific-agents ;; agents which are always included
						  KnownGood KnownBad

						  ;; the monitor only selects things in the 
						  ;; specific-agents list or those for which
						  ;;    (selector self) 
						  ;; returns #f to indicate no action to be taken,
						  ;; #t to indicate that change is in the offing,
						  ;; a class to migrate to, or some symbolic value
						  ;; "history" is a state variable which 
						  ;; records the historical state of the monitor.
	 )
	)

"<monitor> agents play a special role, keeping an eye on subsets of
the population of agents.  Monitors may effect changes in the
composition of both the population and the constituents which comprise
an agent. In many ways, monitors are similar to introspection agents."






(define-class <domain-monitor>
  (inherits-from <monitor>)
  (state-variables domain-list domain-selector)
  ;; the domain list is the list of agents which comprise the notional domain
  ;; the 
  )

;; A domain monitor may have a number of niche level monitors which
;; are associated with it; they would typically be included in its
;; domain-list


(define-class <niche-monitor>
  (inherits-from <monitor>)
  (state-variables representation-alternatives
						 domain ;; contains candidate agents
						 niche-selector ;; selects the agents within the niche
						 )
  )


(define-class <agent-monitor>
  (inherits-from <monitor>)
  (state-variables zones indicator-function)
  ;; zones is a list of lists which are either of the form (variable-symbol flag-symbol min max) or
  ;; of the form (flag-symbol indicator-function) where the function is passed "self"
  ;; if the indicated variable has a value in a given interval, or the function returns #t,
  ;; the corresponding flag is included in a component list of a returned list.
)




;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
