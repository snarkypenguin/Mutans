;-  Identification and Changes

#|

The way this works is like so: 

	An agent is created which has a list of entities which are polled
when it runs (it calls "log-data") to cause the entities to generate
appropriate output which is sent to a port.  Before it processes the
list of entities, it calls "prepare-to-dump", and after it has
finished it calls "finished-dumping" -- these two routines handle
pagination page wrapping.

|#








(define <introspection> (make-class (list <agent>)
									  '(file filename filetype format variables variables-may-be-set 
												missing-val show-field-name preamble-state
												introspection-list 
												;;introspection-schedule 
												timestep-epsilon)))
;; file is the output handle
;; if filename is not a string, things go to stdout
;; if append-time is not false it must either be true or the size of the number (digit count)
;; if filetype is defined it must be a string and it will be appended to the filename 
;; the introspection list is the list of agent to be looked at
;; the introspection schedule is the set of times to run at (this is really probably better done with timestep-schedule)

(register-class <introspection>)

(define <logfile> (make-class (list <introspection>)
									  '()))
;; file is the output handle
;; if filename is not a string, things go to stdout
;; if append-time is not false it must either be true or the size of the number (digit count)
;; if filetype is defined it must be a string and it will be appended to the filename 
(register-class <logfile>)

(define <snapshot> (make-class (list <introspection>)
									  '(lastfile currentfile)))
;; file is the output handle
;; if filename is not a string, things go to stdout
;; if append-time is not false it must either be true or the size of the number (digit count)
;; if filetype is defined it must be a string and it will be appended to the filename 
(register-class <snapshot>)


;; This is the basis for all the logging things like the log-map agent and the log-data


(define <log-map> (make-class (list <snapshot>)
										'(ps png)))
;; ps and png indicate whether the files should be left on disk at the end of the step

(register-class <log-map>)

(define <log-data> (make-class (list <logfile>)
									  '())) 
;; projections is an association list of slot-names and the projection functions to apply before output
;; Data is generated using 

(register-class <log-data>)

(define <log-agent-table> (make-class (list <log-data>)
									  '(target-agent))) 
;; projections is an association list of slot-names and the projection functions to apply before output
;; Data is generated using 

(register-class <log-agent-table>)

(define <log-table> (make-class (list <log-data>)
									  '())) 
;; projections is an association list of slot-names and the projection functions to apply before output
;; Data is generated using 

(register-class <log-table>)

(define <log-agent-table*> (make-class (list <log-agent-table>)
									  '())) 
;; <log-agent-table*> hands off most of the output generation to the class entity being output
;; projections is an association list of slot-names and the projection functions to apply before output
;; Data is generated using 

(register-class <log-agent-table*>)

(define <log-table*> (make-class (list <log-table>)
									  '())) 
;; <log-table*> hands off most of the output generation to the class entity being output
;; projections is an association list of slot-names and the projection functions to apply before output
;; Data is generated using 

(register-class <log-table*>)


;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
