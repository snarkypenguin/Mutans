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



(warning "I really need to separate the logging from the file handling.  
bAt the moment we have the <logfile> branch and the <log-data> branch; this
needs to change: the creation of a logging agent needs to have a 
file-handling class (logfile, snapshot, or output?) --- everything is 
routed through these, and page-preamble and emit-page (called for each timestep)
do the appropriate thing.")

(define (stopit #!rest args) (void))

(define DONT-FILTER-TARGET-VARIABLES #t)


"The following list determines the association of a format with the sort of file-handle 
to use.  "

(define logfile-associations
  (list (cons 'stdout (current-output-port))
		  (cons 'stderr (current-error-port))
		  (cons 'ps make-postscript)
		  (cons 'text make-text)
		  (cons 'data make-text)
		  (cons 'stats make-text)))


"In the logging code, a 'page' is notionally a pass through the target
agent list.  The page-preamble may open a file and write some
preliminary stuff, or it may just assume that the file is open already
and do nothing at all.  Similarly the page-epilogue does things like
close pages and emit 'showpage' for postscript stuff.
"

(define map:linewidth 0.2) ;; useful width as a default
(define map:lightwidth 0.1) ;; useful light weight width for extra info


(define introspection-priority 10000)

;;Examples might be
;;    (*is-taxon? "Thunnus*" 'wild)
;;    (*is-taxon? "Red VW-microbus" 'ci)
;;	   (*is-taxon? (list "grocer" "haberdasher" "butcher" "wheelwright"))


(model-method <log-introspection> (initialise-instance self)
				  (if (uninitialised? (my 'report-time-table))
						(begin
						  ;(warning-log (dnl* "A" (cnc self) " had trouble setting up its report-time-table."))
						  (slot-set! self 'report-time-table (make-table))))
				  )				  

(model-body <log-introspection>
				(let (
						(file (my 'file))
						)
				  ;;(dnl* (cnc self) "model-body")
					(kdebug '(log-* introspection-trace)
							  "[" (my 'name) ":" (cnc self) "@" (subjective-time self) "]"
							  "Log-introspection: model-body")

					(let ((sched (my 'timestep-schedule))
							)
					  
					  (if (kdebug? 'introspection-trace)
							(pp (dumpslots self)))
					  
					  (set! dt (if (and (pair? sched) (< (car sched) (+ t dt)))
										(- (car sched) t)
										dt))

					  (kdebug '(log-* introspection-trace)
								 "      list:     " (map name (my-list self)))
					  (kdebug '(log-* introspection-trace)
								 "      schedule: "
								 (list-head (my 'timestep-schedule) 3)
								 (if (> (length (my 'timestep-schedule)) 3)
									  '... ""))

					  (set-my! 'variables-may-be-set #f)
					  (emit-page self)

					  ;;(skip-parent-body)
					  (call-all-parents) ;; parent body sets the time step used
					  )
					))



(model-method <log-introspection> (open-output-handle self)
				  ;(+kdebug!)
  (let ((filename (slot-ref self 'filename))
		  (file (slot-ref self 'file))
		  (format (slot-ref self 'format))
		  )
	 ;;(dnl* "###############  MAYBE OPENING " filename " ###############")
	 ;;(dnl* "###############  maybe OPENING" filename "for" (cnc self) (name self) "###############")
	 (if (or (not (member format (map car logfile-associations)))  (not (string? filename)) (zero? (string-length filename))) (abort))
  
	 (if (not file)
		  (let ((fh-opener (assoc format logfile-associations)))
			 ;;(dnl* "###############  DEFINITELY OPENING" filename "for" (cnc self) (name self) "############### [" file "]")
			 (cond
			  ((not fh-opener)
				(dnl* "Bad format specified for" (name self))
				(abort))
			  ((port? (cdr fh-opener))
				(set! file (cdr fh-opener))
				(set-my! 'file file)
				)
			  (else
				(set! file ((cdr fh-opener) filename (slot-ref self  'default-file-arguments)))
				(set-my! 'file file)
				)
			  )))
			 
			 file))

(model-method (<log-introspection> <number> <number>) (agent-prep self start end)
				  (parent-agent-prep) ;; parents should prep first
				  (open-output-handle self)
				  #t
				  )


(model-method <log-introspection> (agent-shutdown self #!rest args)
	      (let ((file (my 'file)))
		(if (my 'file)
		    (cond
		     ((and (output-port? (my 'file))
			   (not (memq (my 'file)
				      (list (current-output-port)
					    (current-error-port)))))
		      (close-output-port file))
		     ((file-handle? (my 'file))
		      ((my 'file) 'close))
		     (else #f)))
		
		(set-my! 'file #f)
		(parent-agent-shutdown)
		))

(model-method (<log-introspection> <list>) (set-variables! self lst)
				  ;(+kdebug!)
				  (if (and (my 'variables-may-be-set) (list? lst))
						(set-my! 'variables lst)
						(abort "cannot extend variables after it starts running")
						))

(model-method (<log-introspection> <list>) (extend-variables! self lst)
				  ;(+kdebug!)
				  (if (and (my 'variables-may-be-set) (list? lst))
						(set-my! 'variables (unique* (append (my 'variables) lst)))
						(abort "cannot extend variables after it starts running")
						))

(model-method (<log-introspection> <agent> <number>) (emit-and-record-if-absent self agent t)
				  ;(+kdebug!)
				  (let* ((tbl (my 'report-time-table))
							(rec (table-ref tbl agent #f))
							)
					 ;(dnl* (name self) "looking at" (name agent) "rec:" rec "tbl:" tbl )
					 (if (kdebug? 'logger-redundancy-check)
						  (begin
							 (dnl "LOGGER TIME CHECK: record for " (name agent) ":" (slot-ref agent 'subjective-time) " returns " rec)
							 ;;(dnl* "   logger is currently examining the time" rec "<" t)
							 )
						  )
					 (if (not (is-class? self <log-map>)) ;; log-map agents always emit and never record
						  (if (or (not rec) (and (number? t) (< rec t)))
								(begin
								  (table-set! tbl agent t)
								  #t)
								#f)
						  #t))) 
								

(model-method (<log-introspection>) (emit-page self)
				  (kdebug '(chaintrack) "(model-method (<log-introspection>) (emit-page self)")
				  ;(+kdebug!)
				  (if (not (or (my 'file) (file-handle? (my 'file)) (output-port? (my 'file))))
						(open-output-handle self))

				  (let ((format (my 'format)))
					 (kdebug '(log-* introspection-trace)
								"[" (my 'name) ":" (cnc self) "]"
								"Introspection: emit-page")
					 (kdebug '(log-*) (my-list self))
					 (kdebug '(chaintrack) "about to call page-preamble")
					 (page-preamble self self format)
					 (kdebug '(chaintrack) "back from page-preamble")
					 
					 (let ((proc (lambda (ila)
										(kdebug '(log-* introspection-trace) " ==> processing "
												  (cnc ila) " "  (procedure? ila) format (my 'variables))

										(kdebug '(chaintrack) "about to call log-data" (my-list self))
										(log-data ila self format (my 'variables))
										(kdebug '(chaintrack) "back from log-data")
										(kdebug '(log-* introspection-trace) " <== PROCESSED "
												  (cnc ila) " "  (procedure? ila))
										#f
										)))
						(for-each proc (my-list self))
						)
					 (kdebug '(chaintrack) "Ummm, about to run epilogue")
					 (page-epilogue self self format)
					 (kdebug '(chaintrack) "Back from page-epilogue, leaving emit page")
					 ))

;---- logfile methods -- <logfile>s open a single file and use that till they finish running

(model-method <logfile> (agent-prep self start end)
				  (parent-agent-prep) ;; parents should prep first
				  #t
)


(model-method <logfile> (page-preamble self logger format)
				  ;(+kdebug!)
				  (kdebug '(page-boundary) "page-preamble " (cnc self) (name self) " " (name logger) " " format)
				  (kdebug '(introspection logfile) "[" (my 'name) ":")

				  (let ((filename (my 'filename))
						  (file (my 'file))
						  )
					 
					 (if (not file)
						  (set! file (open-output-handle self)))
					 

					 (kdebug 'logfile-issues "In: logfile preamble filename " filename "and file" file)
					 )
				  )


(model-method <logfile> (page-epilogue self logger format)
				  (kdebug '(page-boundary) "page-epilogue " (name self) " " (name logger) " " format " # " (my 'pagecount))
				  (kdebug 'logfile-issues "In: logfile epilogue filename " (my 'filename) "and file" (my 'file))
				  (kdebug '(introspection logfile) "[" (my 'name) ":"
							 (cnc self) "]" "has finished a dump")
				  (kdebug 'logfile-issues "Out: logfile epilogue filename " (my 'filename) "and file" (my 'file))
				  #!void)



(define (colour-mapping C)
  (cond
	((number? C)
	 C
	 )
	((or (boolean? C) (symbol? C))
	 (case C
		((#t) 0.0)
		((#f) 1.0)
		((red) '(1.0 0.0 0.0))
		((darkred) '(0.5 0.0 0.0))
		((lightred) '(1.0 0.5 0.5))
		((green) '(0.0 1.0 0.0))
		((darkgreen) '(0.0 0.5 0.0))
		((lightgreen) '(0.5 1.0 0.5))
		((blue) '(0.0 0.0 1.0))
		((darkblue) '(0.0 0.0 0.5))
		((lightblue) '(0.5 0.5 1.0))
		((grey gray) '(0.5 0.5 0.5))
		((lightgrey lightgray) '(0.82 0.82 0.82))
		((midgrey midgray) '(0.75 0.75 0.75))
		((darkgrey darkgray) '(0.3 0.3 0.3))
		((black) '(0.0 0.0 0.0))
		((white) '(1.0 1.0 1.0))
		(else
		 (cond
		  ((and (number? C) (inexact? C) (<= 0.0 C) (<= C 0.1))
			(make-list 3 C))

		  ((and (number? C) (integer? C) (<= 0 C) (<= C 255))
			(map (lambda (v) (/ v 255.0)) (make-list 3 C)))

		  ((and (list? C) (= (length C) 3) (apply andf map (lambda (v) (and (number? v) (<= 0 v) (<= v 1.0))) C))
			C)

		  ((and (list? C) (= (length C) 3) (apply andf map (lambda (v) (and (integer? v) (<= 0 v) (<= v 255))) C))
			(map (lambda (v) (/ v 255.0)) C))
		  (else (error "Bad colour" C))))))
	(else (error "Bad colour" C))
	))


(model-method <log-map> (agent-prep self start end)
				  (parent-agent-prep) ;; parents should prep first
				  #t
				  
)


(model-method (<log-map> <list> <symbol>) (map-polygon self model-space-poly format #!optional colour)
				  (let* ((perimeter (map (lambda (x) (model->local self x)) model-space-poly))
							(col (if (symbol? colour) (colour-mapping colour) (my-map-color self)))
							(fh (my 'file)))
					 (case format
						('ps
						 (fh 'comment (string-append "map-polygon " (object->string perimeter)))
						 (plot-polygon fh map:linewidth col perimeter))
						(else
						 (error "Only postscript is supported at the moment" (bummer))))))



;(use-parent-body <log-map>)

(model-method (<log-map> <log-introspection> <symbol>) (page-preamble self logger format)
				  ;(+kdebug!)
				  (parent-page-preamble)
				  ;; This *must* replace it's parent from <log-data> since
				  ;; it doesn't work with a traditional port
				  (kdebug '(page-boundary) "page-preamble " (cnc self) (name self) " " (name logger) " " format  " # " (my 'pagecount))
				  (kdebug '(log-* log-map) (name self) "[" (my 'name) ":"
							 (cnc self) "]" "in page-preamble")
				  ((my 'file) 'start-page)
				  )

(model-method (<log-map> <log-introspection> <symbol>) (page-epilogue self logger format)
				  ;; This *must* replace it's parent from <log-data> since
				  ;; it doesn't work with a traditional port
				  ;;(dnl* "IN LOG-MAP EPILOGUE")
				  (parent-page-epilogue)
				  (if (my 'separate-pages)
						(begin
						  (kdebug '(page-boundary) "page-epilogue " (name self) " " (name logger) " " format " # " (my 'pagecount))
						  (kdebug '(log-* log-map) (name self) "[" (my 'name) ":"
									 (cnc self) "]" "has page-epilogue")

						  (let ((file (my 'file)))
							 (file 'end-page)
							 (file 'showpage)
						  )
						  ))
				  )


;; This logs to an open file
(model-method (<log-map> <log-introspection> <symbol>) (log-data self logger format targets)
				  (let ((kdebug (if #t kdebug dnl*))
						  )
					 (kdebug '(chaintrack) "(model-method (<log-map> <log-introspection> <symbol>) (log-data self logger format targets)")
					 
					 (kdebug 'log-horrible-screaming 'log-map (cnc self) (cnc logger) (cnc format) (cnc targets))
					 (lambda (target)	
						(kdebug '(log-* log-map) (name self) "[" (my 'name)
								  ":" (cnc self) "]" "in log-data"
								  (cnc target) (slot-ref target 'name))

						(let* ((name (slot-ref target 'name))
								 (p (slot-ref self 'local-projection))
								 ;; to spit out a ps file we need to project the 
								 ;; modelspace data into the PS domain
								 (ps (slot-ref self 'file))
								 )
						  (ps 'Comment "logging data for " name "****************")
						  (ps 'moveto (list (p '(20 20))))
						  (ps 'push-color (my-map-color self))
						  (ps 'Helvetica 14)
						  (ps 'show (string-append (slot-ref self 'name)))
						  (ps 'pop-color)
						  (ps 'comment "finished logging data for " name)
						  )))
				  )


;---- log-data methods (inherits from <logfile>)

;(use-parent-body <log-data>)

(model-method (<log-data> <number> <number>) (agent-prep self start end)
				  ;(+kdebug!)
				  ;; This opens the output file on initialisation.
				  (parent-agent-prep) ;; parents should prep first
				  (kdebug '(log-* log-data) (name self) "[" (my 'name) ":"
							 (cnc self) "]" "in agent-prep")
				  
				  (if (null? (my 'variables))
						(let ((vars (reverse
										 (unique*
										  (reverse
											(append
											 '(name subjective-time)
											 (apply append
													  (map extra-variable-list
															 (my-list self)))))))))
						  (slot-set! self 'variables vars)))
				  #t
				  )


(model-method <log-data> (agent-shutdown self #!rest args)
				  ;(+kdebug!)
				  (kdebug '(log-* log-data) (name self) "[" (my 'name) ":"
							(cnc self) "]" "in agent-shutdown")
				  (if (and (my 'file) (file-handle? (my 'file))
							  (not (memq (my 'file)
											 (list (current-output-port)
													 (current-error-port)))))
						(let ((file (my 'file)))
						  (cond
							((output-port? file) (close-output-port file))
							((file-handle? file) (file 'close)))
						  (set-my! 'file #f) ;; leave it the way it should be left
						  ))
				  (set-my! 'file #f)
				  (parent-agent-shutdown) ;; Parents should shutdown last
				  )

(model-method (<log-data> <log-introspection> <symbol>) (page-preamble self logger format)
				  ;(+kdebug!)
				  (kdebug 'log-issues "In: log-data preamble, filename: " (my 'filename) "and file" (my 'file))

				  (parent-page-preamble) ;; opens the file

				  (kdebug 'log-issues "In: log-data, after logfile preamble filename " (my 'filename) "and file" (my 'file))

				  (kdebug 'log-init "Logfile is" (my 'filename)
							"(file-handle?" (my 'file) ") =" (file-handle? (my 'file)))

				  (if (not (file-handle? (my 'file)))
						(abort "Serious problems getting an output port for "
								 (my 'name)))

				  (let ((il (my-list self))
						  (file (my 'file))
						  (show-field-name (my 'show-field-name))
						  (missing-val (my 'missing-val))
						  )
					 (case format
						((ps)
						 #f)
						(else
						 (if (not (member 'header (my 'preamble-state)))
							  (begin
								 (if (and (pair? il)
											 (null? (cdr il))) ;; agent name
																	 ;; comes first
													             ;; since it is
																	 ;; easy to prune
																	 ;; the first line
									  (begin
										 (file 'display (string-append "# " (name (car il))))
										 (file 'newline)))
							  
								 (let ((header 
										  (if missing-val
												(my 'variables)
												(let loop ((all-vars '())
															  (entities il))
												  (if (null? entities)
														(intersection
														 (uniq
														  (map
															string->symbol
															(sort (map symbol->string
																		  all-vars)
																	string<?)))
														 (my 'variables))
														(loop
														 (append
														  (map car
																 (class-slots-of (car entities)))
														  (extra-variable-list (car entities))
														  all-vars) (cdr entities))))
												)
										  ))
									(file 'display "# ")
									(for-each
									 (lambda (x) (file 'display " ") (file 'display x))
									 header)
									(file 'newline))
								 (set-my! 'preamble-state
											 (cons 'header (my 'preamble-state)))
								 )
							  )
						 )
						)
					 )
				  
				  (kdebug 'logfile-issues "Out: log-data, after logfile preamble filename " (my 'filename) "and file" (my 'file))
				  
				  )
						
(model-method (<log-data> <log-introspection> <symbol> <list>) (log-data self logger format target-variables)
				  ;(+kdebug!)
				  (let ((kdebug (if #t kdebug dnl*))
						  )
					 (kdebug '(chaintrack) "(model-method (<log-data> <log-introspection> <symbol> <list>) (log-data self logger format target-variables)")
					 (kdebug '(chaintrack) "in <log-data> (log-data" (name self) (name logger) format target-variables)
									 
					 ;; (error "(-: Oops, really ought to never get here. :-)")
					 (kdebug '(log-* log-data) (name self) "[" (my 'name) ":"
								(cnc self) "]" "in log-data")
					 (let ((file (my 'file))
							 (show-field-name (my 'show-field-name))
							 (subjects (my-list self))
							 (target-variables (my 'variables))
							 (missing-val (my 'missing-val))
							 )
						(for-each (lambda (subject) 
										(file 'display "**")
										(for-each ;; field in the variable list
										 (lambda (field)
											(if show-field-name
												 (begin
													(file 'display " ")
													(file 'display field)))

											(cond
											 ((member field
														 (map car
																(class-slots-of subject)))
											  (kdebug '(log-* log-data logging-debug)
														 "     Dumping " field "="
														 (if (has-slot? self t)
															  (slot-ref self t)
															  "missing!"))
											  
											  (file 'display " ")
											  (file 'display (slot-ref subject field))
											  )
											 ((member field (extra-variable-list subject))
											  (file 'display " ")
											  (file 'display (extra-variable subject field))
											  )
											 (missing-val
											  (file 'display " ")
											  (file 'display missing-val)))
											)
										 (if DONT-FILTER-TARGET-VARIABLES
											  target-variables
											  (filter (not-member (my 'dont-log)) target-variables)))
										(file 'newline)
										)
									 subjects)
						)
					 )
				  )

(model-method (<log-data> <log-introspection> <symbol>) (page-epilogue self logger format)
				  ;(+kdebug!)
				  (kdebug '(log-* log-data) (name self) "[" (my 'name) ":"
							 (cnc self) "]" "in page-epilogue")
				  (let ((ml (my-list self)))
					 (if (and (pair? ml)
								 (pair? (cdr ml)))
						  (or #t ((my 'file) 'newline)))
					 ;; We don't want a blank line between each record!
					 ;; -- change #t to #f to get lines between "pages"
					 )
				  )






(model-method (<log-statistics> <agent>)(stats-callback self #!optional data-list)
				  #t)



;-  The End 


;; Local Variables:
;; mode: scheme
;; outline-regexp: ";-+"
;; comment-column:0
;; comment-start: ";; "
;; comment-end:"" 
;; End:
