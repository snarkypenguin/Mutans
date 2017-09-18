(include "framework")
;-  Identification and Changes

"
    Copyright 2017 Randall Gray

    This file is part of Remodel.

    Remodel is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Remodel is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Remodel.  If not, see <http://www.gnu.org/licenses/>.
"
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

(define-class <log> (inherits-from <introspection>  <projection>)
  (state-variables file format variables
						 filename
						 variables-may-be-set missing-val show-field-name
						 preamble-state ;;introspection-targets
						 report-time-table ;output-projection
						 ;; These last two are here because it is
						 ;; entirely likely that there will be
						 ;; tex, latex and eps-text formats for production
						 ;; quality output
						 ))
;;- file is the output handle
;;  if filename is not a string, things go to stdout
;-  if append-time is not false it must either be true or the size of
;;- the number (digit count)
;;- if filetype is defined it must be a string and it will be appended
;;  to the filename
;;- the introspection list is the list of agent to be looked at
;;- the timestep schedule is the set of times to run at

(define-class <logfile> (inherits-from <log>)
  (state-variables
	pagecount
	preamble-state ;;introspection-targets
	default-file-arguments
	include-key
	separate-pages ;; -- #t indicates that you want
   ;; each page in a separate file
   ;; These last two are here because it is
   ;; entirely likely that there will be
   ;; tex, latex and eps-text formats for production
   ;; quality output
   )   
  
  )
;;- file is the output handle
;;- if filename is not a string, things go to stdout
;;- if append-time is not false it must either be true or the size of
;;  the number (digit count)
;;- if filetype is defined it must be a string and it will be appended
;;  to the filename


;(define-class <snapshot> (inherits-from <log>)
;									  (state-variables pagecount separate-pages))
;;- file is the output handle
;;- if filename is not a string, things go to stdout
;;- if append-time is not false it must either be true or the size of
;;  the number (digit count)
;;- if filetype is defined it must be a string and it will be appended
;;  to the filename


;; This is the basis for all the logging things like the log-map agent
;; and the log-data


(define-class <log-map> (inherits-from <logfile>)
  (state-variables)
  )

(define-class <log-data> (inherits-from <logfile>)
  (state-variables)
  )


(define-class <log-statistics> (inherits-from <log-data>)
  (state-variables
	stats ;; an instance of a stats-filter
	selector ;; function to select which agents to poll
	methodlist ;; methods to call to gather statistics, where the
				  ;; methods are of the form (get-stats target #!optional
	           ;; lst-of-data)
	))
  

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
