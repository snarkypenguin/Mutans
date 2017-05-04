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

(define-class <log-introspection> (inherits-from <introspection>  <projection>)
				  (state-variables file filename filetype filename-timescale format variables
										 variables-may-be-set missing-val show-field-name
										 preamble-state introspection-targets
										 report-time-table
										 ))
;;- file is the output handle
;;  if filename is not a string, things go to stdout
;-  if append-time is not false it must either be true or the size of
;;- the number (digit count)
;;- if filetype is defined it must be a string and it will be appended
;;  to the filename
;;- the introspection list is the list of agent to be looked at
;;- the timestep schedule is the set of times to run at


(define-class <logfile> (inherits-from <log-introspection>)
  (state-variables)
  
  )
;;- file is the output handle
;;- if filename is not a string, things go to stdout
;;- if append-time is not false it must either be true or the size of
;;  the number (digit count)
;;- if filetype is defined it must be a string and it will be appended
;;  to the filename


(define-class <snapshot> (inherits-from <log-introspection>)
									  (state-variables lastfile currentfile))
;;- file is the output handle
;;- if filename is not a string, things go to stdout
;;- if append-time is not false it must either be true or the size of
;;  the number (digit count)
;;- if filetype is defined it must be a string and it will be appended
;;  to the filename


;; This is the basis for all the logging things like the log-map agent
;; and the log-data


(define-class <log-map> (inherits-from <snapshot>)
										(state-variables))


(define-class <log-data> (inherits-from <logfile>)
										 (state-variables)
										 )

;; Ultimately, there will be a <log-movie> which inherits from <log-data> 
;; but generates images (frames) like <log-map> for a movie-loop either 
;; in something like a gif or mpeg format.

;;- projections is an association list of slot-names and the projection
;;  functions to apply before output
;;- Data is generated using
;;

;;(define-class <log-table> (inherits-from <log-data>)
;;										  (state-variables))
;;- projections is an association list of slot-names and the
;;  projection functions to apply before output


;;(define-class <log-agent-table> (inherits-from <log-data>)
;;												  (state-variables target-agent))

;;- projections is an association list of slot-names and the projection
;;  functions to apply before output


;;(define-class <log-agent-table*> (inherits-from <log-agent-table>)
;;													(state-variables))

;;- <log-agent-table*> hands off most of the output generation to the
;;  class entity being output
;;- projections is an association list of slot-names and the
;;  projection functions to apply before output


;;(define-class <log-table*> (inherits-from <log-table>)
;;											(state-variables))

;;- <log-table*> hands off most of the output generation to the class
;;  entity being output
;;- projections is an association list of slot-names and the
;;  projection functions to apply before output

;-  The End 


;- Local Variables:
;- mode: scheme
;- outline-regexp: ";-+"
;- comment-column:0
;- comment-start: ";;;- "
;- comment-end:"" 
;- End:
