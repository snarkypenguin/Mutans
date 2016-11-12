(include "framework")
;-  Identification and Changes

;;; #|

;;; The way this works is like so: 

;;; 	An agent is created which has a list of entities which are
;;; 	polled when it runs (it calls "log-data") to cause the entities
;;; 	to generate appropriate output which is sent to a port.  Before
;;; 	it processes the list of entities, it calls "prepare-to-dump",
;;; 	and after it has finished it calls "finished-dumping" -- these
;;; 	two routines handle pagination page wrapping.

;;; |#

(define logger-tags '())


(define <introspection>
  (make-class (inherits-from <agent>)
				  (state-variables file filename filetype format variables
										 variables-may-be-set missing-val show-field-name
										 preamble-state introspection-list
										 timestep-epsilon)))
;;- file is the output handle
;;  if filename is not a string, things go to stdout
;-  if append-time is not false it must either be true or the size of
;;- the number (digit count)
;;- if filetype is defined it must be a string and it will be appended
;;  to the filename
;;- the introspection list is the list of agent to be looked at
;;- the timestep schedule is the set of times to run at

(register-unique class <introspection>)

(define <logfile> (make-class (inherits-from <introspection>)
										(no-state-variables)
										))
;;- file is the output handle
;;- if filename is not a string, things go to stdout
;;- if append-time is not false it must either be true or the size of
;;  the number (digit count)
;;- if filetype is defined it must be a string and it will be appended
;;  to the filename

(register-unique class <logfile>)

(define <snapshot> (make-class (inherits-from <introspection>)
									  (state-variables lastfile currentfile)))
;;- file is the output handle
;;- if filename is not a string, things go to stdout
;;- if append-time is not false it must either be true or the size of
;;  the number (digit count)
;;- if filetype is defined it must be a string and it will be appended
;;  to the filename

(register-unique class <snapshot>)


;; This is the basis for all the logging things like the log-map agent
;; and the log-data


(define <log-map> (make-class (inherits-from <snapshot>)
										(state-variables ps png)))
;; ps and png indicate whether the files should be left on disk at the
;; end of the step

(register-unique class <log-map>)

(define <log-data> (make-class (inherits-from <logfile>)
										 (no-state-variables)
										 ))

;;- projections is an association list of slot-names and the projection
;;  functions to apply before output
;;- Data is generated using

(register-unique class <log-data>)

(define <log-agent-table> (make-class (inherits-from <log-data>)
												  (state-variables target-agent))) 

;;- projections is an association list of slot-names and the projection
;;  functions to apply before output
;;- Data is generated using

(register-unique class <log-agent-table>)

(define <log-table> (make-class (inherits-from <log-data>)
										  (no-state-variables))) 
;;- projections is an association list of slot-names and the
;;  projection functions to apply before output
;;- Data is generated using 

(register-unique class <log-table>)

(define <log-agent-table*> (make-class (inherits-from <log-agent-table>)
													(no-state-variables))) 
;;- <log-agent-table*> hands off most of the output generation to the
;;  class entity being output
;;- projections is an association list of slot-names and the
;;  projection functions to apply before output
;;- Data is generated using 

(register-unique class <log-agent-table*>)

(define <log-table*> (make-class (inherits-from <log-table>)
											(no-state-variables))) 

;;- <log-table*> hands off most of the output generation to the class
;;  entity being output
;;- projections is an association list of slot-names and the
;;  projection functions to apply before output
;;- Data is generated using 

(register-unique class <log-table*>)


;-  The End 


;- Local Variables:
;- mode: scheme
;- outline-regexp: ";-+"
;- comment-column:0
;- comment-start: ";;;- "
;- comment-end:"" 
;- End:
