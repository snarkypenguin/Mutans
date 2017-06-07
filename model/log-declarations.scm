(include "framework")
;-  Identification and Changes

;***** When you add things to this file (or any other *-declarations.scm file)
;***** you *must* run "make declarations.scm" or a more comprehensive make.
;*****
;***** This is because the model includes "declarations.scm" rather than the many
;***** other *-declaractions.scm files -- "declarations.scm" is filtered to exclude
;***** duplicate declarations, and this stops methods simply disappearing.



;(declare-method introspection-targets "return the introspection list")
;(declare-method set-introspection-targets! "set the list of agents to be examined")
;(declare-method introspection-times "return the introspection list")
;(declare-method set-introspection-times! "set the list of agents to be examined")
;(declare-method page-preamble "open files & such")
;(declare-method log-this-agent "log an agent's data")
;(declare-method page-epilogue "make things tidy")
;(declare-method set-variables! "set the list of variables")
;(declare-method extend-variables! "extend the list of variables")


;(declare-method schedule-times "return a schedule")
;(declare-method schedule-epsilon "return a schedule's epsilon")
;(declare-method set-schedule-times! "set the times the agents is scheduled to do something")
;(declare-method set-schedule-epsilon! "set the epsilon for the schedule")
;(declare-method insert-schedule-time! "insert a time into a schedule")

;(declare-method log-data% "err, ...log data to an open output") 
;(declare-method emit-page "set the list of agents to be examined")
;(declare-method open-p/n "open a port for logging")
;(declare-method close-p/n "close a logging port")

;(declare-method map-log-track-segment "description needed")
;(declare-method map-emit-page "specific for postscript output")

;(declare-method data-log-track-segment "description needed")
;(declare-method data-log-data "specific for data output")
;(declare-method data-emit-page "specific for data output")

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
