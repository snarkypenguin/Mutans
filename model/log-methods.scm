(include "framework")
;-  Identification and Changes

(warning "I really need to separate the logging from the file handling.  
At the moment we have the <logfile> branch and the <snapshot> branch; this
needs to change: the creation of a logging agent needs to have a 
file-handling class (logfile, snapshot, or output?) --- everything is 
routed through these, and page-preamble and emit-page (called for each timestep)
do the appropriate thing.")



(define (stopit #!rest args) (void))

(define DONT-FILTER-TARGET-VARIABLES #t)


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


(define (introspection-filename filename filetype filename-timescale #!optional time)
  ;; filename will be the first part of the  filename
  ;; the type will correspond to the last component, such as "ps", "data", or "tbl"
  ;; the filename-timescale will be a number of seconds which  the time variable will be divided by, such as 'days'
  (let ((field-width 12)) ;; this may need to be increased 
	 (if (string? filename)
		  (if time
				(string-append filename "-" (pno (inexact->exact (/ time filename-timescale)) 12) "." filetype) 
				(string-append filename "." filetype))
		  #f)))


;; (model-method <log-introspection> (set-default-font self file)
;;   (if (member (my 'format) '(ps postscript postscript-text))
;; 		(begin
;; 		  (file 'set-font (my 'default-font) (my 'default-size))
;; 		  (file 'set-gray ps-black))))

;; Logger agents (things that inherit from introspection, really) have
;; a high priority; as a consequence they get sorted to the front of a
;; timestep
;; (agent-initialisation-method <log-introspection> (args)
;;  (no-default-variables)
;;  (set-state-variables
;;   self (list 'type 'logger
;; 				 'priority introspection-priority ;; also set in <introspection>
;; 				 'jiggle 0 'introspection-targets '()  ;; also set in <introspection>
;; 				 'timestep-epsilon 1e-6 'file #f ;; also set in <introspection>
;; 				 'filename #f 'filetype #f
;; 				 'format 'text 'missing-val "NoData"
;; 				 'show-field-name #f 'preamble-state '()
;; 				 'dont-log '(ready-for-prep
;; 								 ;; agent things
;; 								 agent-body-ran 
;; 								 agent-epsilon local-projection inv-local-projection counter 
;; 								 migration-test state-flags
;; 								 dont-log timestep-schedule kernel
								 
;; 								 ;; log agent things
;; 								 introspection-targets
;; 								 timestep-epsilon 

;; 								 dims ;; thing things

;; 								 ;; environment things
;; 								 default-value minv maxv 

;; 								 ;; ecoservice things
;; 								 plateau-interval growth-rate 

;; 								 ;; landscape things
;; 								 service-list service-update-map
;; 								 update-equations terrain-function
;; 								 dump-times scale 
;; 								 log-services-from-patch
;; 								 log-patches-from-habitat

;; 								 ;; animal things
;; 								 domain-attraction food-attraction 
;; 								 near-food-attraction search-speed
;; 								 wander-speed forage-speed	
;; 								 movement-speed foodlist homelist
;; 								 breedlist habitat

;; 				 'variables-may-be-set #t
;; 				 ))
;;  (parent-initialise) ;; call "parents" last to make the
;;  ;; initialisation list work
;;  (set-state-variables self args)
;;  )

(model-method <log-introspection> (initialisation-checks self)
				(if (uninitialised? (my 'filename-timescale))
					 (set-my! 'filename-timescale 6))

				(if (uninitialised? (my 'report-time-table))
					 (begin
						;;(warning-log (dnl* "A" (cnc (class-of self)) " had trouble setting up its report-time-table."))
						(slot-set! self 'report-time-table (make-table))))
)				  

(model-body% <log-introspection>
   (let ((kdebug (if #f kdebug dnl*)))
	  (call-parents)
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
		 (call-parents) ;; parent body sets the time step used
		 dt
		 )
	  ))


(model-method (<log-introspection>) (my-list self)
				  (let ((mit (my 'introspection-targets))
						  (Q (agent-kcall self 'runqueue))) ;; This is how an agent would usually call the kernel 

					 (sortless-unique
					  (letrec ((loop (lambda (mitr mlist)
											 (cond
											  ((null? mitr) mlist)
											  ((isa? (car mitr) <agent>) (loop (cdr mitr) (cons (car mitr) mlist)))
											  ((class? (car mitr)) (loop (cdr mitr) (append (filter (*is-class? (car mitr)) Q) mlist)))
											  ((string? (car mitr)) (loop (cdr mitr) (append (filter (*is-taxon? (car mitr)) Q) mlist)))
											  ((symbol? (car mitr)) (loop (cdr mitr) (append (filter (*has-slot? (car mitr)) Q) mlist)))
											  ((procedure? (car mitr)) (loop (cdr mitr) (append (filter procedure Q) mlist)))
											  (#t (error "args to my-list must be agents, strings, symbols classes or procedures" (car mlist)))))
										  ))
						 (loop mit '())))
					 ))

(model-method (<log-introspection> <number> <number>) (agent-prep self start end)
				  (parent-agent-prep self start end) ;; parents should prep first
				  )

(model-method <log-introspection> (agent-shutdown self #!rest args)
				  (let ((file (my 'file)))
					 (if (and (my 'file)
								 (output-port? (my 'file))
								 (not (memq (my 'file)
												(list (current-output-port)
														(current-error-port)))))
						  (close-output-port file))
					 (set-my! 'file #f)
					 (parent-agent-shutdown)
					 ))

(model-method (<log-introspection> <list>) (set-variables! self lst)
				  (if (and (my 'variables-may-be-set) (list? lst))
						(set-my! 'variables lst)
						(abort "cannot extend variables after it starts running")
						))

(model-method (<log-introspection> <list>) (extend-variables! self lst)
				  (if (and (my 'variables-may-be-set) (list? lst))
						(set-my! 'variables (unique* (append (my 'variables) lst)))
						(abort "cannot extend variables after it starts running")
						))

(model-method (<log-introspection> <agent> <number>) (emit-and-record-if-absent self agent t)
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


					 (if (or (not rec) (and (number? t) (< rec t)))
						  	(begin
							  (table-set! tbl agent t)
							  ;(dnl* "      returning #t")
							  #t)
							(begin
							  ;(dnl* "      returning #f")
							  #f))))
								
(model-method (<log-introspection>) (emit-page self)
				  (kdebug '(log-* introspection-trace)
							"[" (my 'name) ":" (cnc self) "]"
							"Introspection: emit-page")
				  (kdebug '(log-*) (my-list self))				  
				  (let ((format (my 'format)))
;					 (dnl* "ERR: in model-method:<log-introspection> (emit-page self)" (cnc self))
;					 (dnl* "     format" format "... heading into preamble")
					 (page-preamble self self format) ;; for snapshots,
																 ;; this will be
																 ;; "opening", for
																 ;; logfiles, it will
																 ;; only open the
																 ;; first time

;					 (dnl* "ERR: about to dispatch to targets: " (map cnc (my-list self)))
					 (let ((proc (lambda (ila)
										(kdebug '(log-* introspection-trace) " ==> processing "
												 (cnc ila) " "  (procedure? ila))
										(log-data ila self format (my 'variables))
										(kdebug '(log-* introspection-trace) " <== PROCESSED "
												 (cnc ila) " "  (procedure? ila))
										#f
										)))
						(for-each proc (my-list self))
						)
;					 (dnl* "ERR: about to run epilogue")
					 (page-epilogue self self (slot-ref self 'format))
;					 (dnl* "ERR: leaving emit page")
					 
					 )
				  )


;---- snapshot methods -- <snapshot>s open a new file each time they run model-body

;; (agent-initialisation-method <snapshot> (args) (no-default-variables)
;; 				  (parent-initialise) ;; call "parents" last to make the
;; 											 ;; initialisation list work
;; 				  (set-state-variables self (list 'type snapshot 'lastfile #f
;; 												 'currentfile #f))
;; 				  (set-state-variables self args)
;; 				  )

(model-method <snapshot> (page-preamble self logger format)
				  (kdebug '(introspection snapshot)"[" (my 'name) ":"
							(cnc self) "]" "<snapshot> is preparing to dump")
				  (let ((filename (my 'filename))
						  (filetype (my 'filetype))
						  (file (my 'file))
						  (t (my 'subjective-time))
						  )

					 (cond ;; Check for error conditions
					  ((not (or (and (not filename) (not (string? filename))) (string? filename)))
						(error (string-append (my 'name)" has a filename which "
													 "is neither false, nor a string.")))

					  ((not (or (and (not filetype) (not (string? filename))) (string? filetype)))
						(error (string-append (my 'name) " has a filetype which "
													 "is neither false, nor a string.")))

					  ((not (number? t))
						(error (string-append (my 'name) " has a subjective time "
													 "which is not a number.")))
					  )

					 (kdebug '(introspection logfile) "[" (my 'name) ":"
							  (cnc self) "]" "is opening a log file" "(" filename ")")

					 ;; Open a new file
					 (cond
					  ((not file)
						(let ((fn (introspection-filename (my 'filename)
																	 (my 'filetype)
 																	 (my 'filename-timescale) t))) ;; t is time
						  (kdebug '(introspection snapshot) "[" (my 'name) ":"
									(cnc self) "]" "opening" fn)
						  (set-my! 'lastfile (my 'currentfile))
						  (set-my! 'currentfile fn)
						  (if (or (not (string? fn) (not fn) (zero? (string-length fn))))
								(set! file (current-output-port))
								(set! file (open-output-file fn)))
;;						  (set-default-font self file)
						  ))
					  ((memq file (list (current-output-port) (current-error-port)))
						;; do nothing really
						(kdebug '(introspection snapshot) "[" (my 'name) ":"
								 (cnc self) "]"
								 "is writing to stdout or stderr")
						#!void
						)
					  (else 
						(kdebug '(introspection  snapshot) "[" (my 'name) ":"
								 (cnc self) "]" " "
								 "has hit page-preamble with a file that is still open."
								 "\nThis is an error.\nClosing the file ("
								 (my 'lastfile) ") and continuing.")
						(close-output-port file)
						(set-my! 'file #f)
						(let ((fn (introspection-filename (my 'filename)
																	 (my 'filetype) t)))
						  (set-my! 'lastfile (my 'currentfile))
						  (set-my! 'currentfile fn)
						  (if (or (not fn) (and (string? fn) (zero? (string-length fn))))
								(set! file (current-output-port))
								(set! file (open-output-file fn)))
;;						  (set-default-font self file)
						  )
						)
					  )
					 
					 (set-my! 'file file)
;;					 (set-default-font self file)

					 (kdebug '(introspection logfile) "[" (my 'name) ":"
							  (cnc self) "]" "opened" file)
					 )
				  )

(model-method <snapshot> (page-epilogue self logger format)
				  (let ((file (my 'file)))
					 (if (and file (not (memq file (list (current-output-port)
																	 (current-error-port)))))
						  (begin
							 (kdebug '(introspection snapshot) "[" (my 'name) ":"
									  (cnc self) "]"
									  "is closing the output port")
							 (close-output-port file)
							 (set-my! 'file #f)))))


;(use-parent-body <snapshot>)

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
		 
;; This does not care if it is a <log-map> or a <log-map*>
(model-method (<log-map> <list> <symbol>) (log-map-polygon self model-space-poly format #!optional colour)
				  (let* ((perimeter (map (lambda (x) (model->local self x)) model-space-poly))
							(col (colour-mapping colour))
							(fh (my 'file)))
					 (case format
						('ps
						 (fh 'comment (string-append "log-map-polygon " (object->string perimeter)))
						 (plot-polygon fh map:linewidth col perimeter))
						(else
						 (error "Only postscript is supported at the moment" (bummer))))))




;(use-parent-body <log-map>)

(model-method (<log-map> <log-introspection> <symbol>) (page-preamble self logger format)
				  ;; This *must* replace it's parent from <snapshot> since
				  ;; it doesn't work with a traditional port
				  (kdebug '(log-* log-map) (name self) "[" (my 'name) ":"
							(cnc self) "]" "in page-preamble")
				  (let ((filename (my 'filename))
						  (filetype (my 'filetype))
						  (file (my 'file))
						  (format (my 'format))
						  (t (my 'subjective-time))
						  )

					 (cond ;; Check for error conditions
					  ((not (or (not filename) (string? filename)))
						(error (string-append (my 'name) " has a filename which is "
													 "neither false, nor a string.")))

					  ((not (or (not filetype) (string? filetype)))
						(error (string-append (my 'name) " has a filetype which is "
													 "neither false, nor a string.")))

					  ((not (number? t))
						(error (string-append (my 'name) " has a subjective time "
													 "which is not a number.")))
					  ;; The following are specific to the "map" classes
					  ((eqv? format 'png)
						(error "<log-map> only supports postscript at the moment"))

					  ((eqv? format 'svg)
						(error "<log-map> only supports postscript at the moment"))

					  ((eqv? format 'gif)
						(error "<log-movie> isn't implemented yet"))

					  ((eqv? format 'mpg)
						(error "<log-movie> isn't implemented yet"))

					  ((not (member format '(ps))) ;; svg png
						(error "Currently <log-map> only supports postscript." format))
					  )

					 ;; Open a new file
					 (cond
					  ((memq file (list (current-output-port) (current-error-port)))
						;; do nothing really
						(kdebug '(introspection log-map) "[" (my 'name) ":"
								 (cnc self) "]" "has nothing to do")
						#!void
						)
					  ((not (output-port? file))
						(kdebug '(introspection log-map) "[" (my 'name) ":"
								 (cnc self) "]" "<log-map>" "is preparing to dump")
						
						(let ((fn (if (number? (my 'filename-timescale))
										  (introspection-filename (my 'filename) (my 'filetype) (my 'filename-timescale) t)
										  (introspection-filename (my 'filename) (my 'filetype) t))))
						  (set-my! 'lastfile (my 'currentfile))
						  (set-my! 'currentfile fn)
						  (if (not fn)
								(void)
								(if (and (string? fn) (zero? (string-length fn)))
									 (abort "Oh. Bother.")
									 (set! file (make-ps fn '(Helvetica)))
										))
						  )
						(kdebug '(introspection log-map) "[" (my 'name) ":"
								 (cnc self) "]" "returning from preamble")
						)
					  (else 
						(kdebug '(introspection log-map) "[" (my 'name) ":"
								 (cnc self) "]"
								 " Good, we've hit page-preamble with a file "
								 "that is still open.\nClosing the file (" 
								 (my 'lastfile) ") and opening a new one.")
						(if (output-port? file)
							 (close-output-port file))
						(set-my! 'file #f)
						(let ((fn (introspection-filename (my 'filename)
																	 (my 'filetype) t)))
						  (set-my! 'lastfile (my 'currentfile))
						  (set-my! 'currentfile fn)
						  (if (or (not fn) (string? fn) (zero? (string-length fn)))
								(abort "Oh. Bother.")
								(set! file (make-ps fn '(Helvetica))))
						  )
						)
					  )


					 (set-my! 'file file)))

(model-method (<log-map> <log-introspection> <symbol>) (page-epilogue self logger format)
				  ;; This *must* replace it's parent from <snapshot> since
				  ;; it doesn't work with a traditional port
				  (kdebug '(log-* log-map) (name self) "[" (my 'name) ":"
							(cnc self) "]" "has page-epilogue")
				  (let ((file (my 'file))
						  (name (my 'currentfile)))
					 (if file
						  (begin
							 (file 'close)
							 (set-my! 'file #f)))
					 )
				  )


;; This logs to an open file
(model-method (<log-map> <log-introspection> <symbol>) (log-data self logger format targets)
				  (let ((kdebug (if #f kdebug dnl*))
						  )
					 
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
						  (ps 'setgray 0.0)
						  (ps 'Helvetica 14)
						  (ps 'show (string-append (slot-ref self 'name)))
						  (ps 'comment "finished logging data for " name)
						  )))
				  )
;---- logfile methods -- <logfile>s open a single file and use that till they finish running

(model-method <logfile> (page-preamble self logger format)
				  (kdebug '(introspection logfile) "[" (my 'name) ":"
							(cnc self) "]" "<logfile> is preparing to dump, file is currently" (my 'filename) (my 'file))

				  (let ((filename (my 'filename))
						  (file (my 'file))
						  )
					 
					 (kdebug 'logfile-issues "In: logfile preamble filename " filename "and file" file)

					 (if (or (uninitialised? filename) (not (or (not filename) (string? filename))))
						  (error (string-append (my 'name) " has a filename which is "
														"neither false, nor a string.")))
					 ;; Open a new file
					 (if (or (uninitialised? file) (not file))
						  (begin
							 (kdebug '(introspection logfile) "[" (my 'name) ":"
									  (cnc self) "]" "is opening a log file" "(" filename ")")
							 (if (kdebug? '(introspection logfile))
								  (if (or (not filename) (not (string? filename)) (zero? (string-length filename)))
										(dnl* "opening current output port" (not filename) (string? filename) (zero? (string-length filename)))
										(dnl* "opening " filename))
								  )
								  
							 (if (or (not filename) (not (string? filename)) (zero? (string-length filename)))
								  (set! file (current-output-port))
								  (set! file (open-output-file filename))
								  )
;;							 (set-default-font self file)
							 (kdebug '(introspection logfile) "[" (my 'name) ":"
									  (cnc self) "]" "opened" file)
							 )
						  )
					 (kdebug 'logfile-issues "Mid: logfile preamble filename " (my 'filename) "and file" (my 'file)"/"file)
					 (set-my!'file file)
					 (kdebug 'logfile-issues "Out: logfile preamble filename " (my 'filename) "and file" (my 'file)"/"file)
					 )
				  )

(model-method <logfile> (page-epilogue self logger format)
				  (kdebug 'logfile-issues "In: logfile epilogue filename " (my 'filename) "and file" (my 'file))
				  (kdebug '(introspection logfile) "[" (my 'name) ":"
							(cnc self) "]" "has finished a dump")
				  (kdebug 'logfile-issues "Out: logfile epilogue filename " (my 'filename) "and file" (my 'file))
				  #!void)



;---- log-data methods (inherits from <logfile>)

;(use-parent-body <log-data>)

(model-method (<log-data> <number> <number>) (agent-prep self start end)
				  ;; This opens the output file on initialisation.
				  (parent-agent-prep self start end) ;; parents should prep first
				  (kdebug '(log-* log-data) (name self) "[" (my 'name) ":"
							(cnc self) "]" "in agent-prep")
				  
				  (let ((filename (my 'filename))
						  (filetype (my 'filetype)))
					 (if (string? (my 'filename))
						  (begin
							 (kdebug '(log-* log-data) (name self) "[" (my 'name)
									  ":" (cnc self) "]" "opening "
									  (introspection-filename filename
																	  (if filetype filetype "")))
							 (set-my! 'file
										 (open-output-file
										  (introspection-filename filename
																		  (if filetype
																				filetype
																				""))))
							 (current-output-port))
						  (begin
							 (kdebug '(log-* log-data) (name self) "[" (my 'name) ":"
									  (cnc self) "]"
									  "using stdout as the output file " )
							 (set-my! 'file (current-output-port))
							 )
						  )
;;					 (set-default-font self (my 'file))
					 )
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
				  )


(model-method <log-data> (agent-shutdown self #!rest args)
				  (kdebug '(log-* log-data) (name self) "[" (my 'name) ":"
							(cnc self) "]" "in agent-shutdown")
				  (if (and (my 'file) (output-port? (my 'file))
							  (not (memq (my 'file)
											 (list (current-output-port)
													 (current-error-port)))))
						(begin
						  (close-output-port (my 'file))
						  (set-my! 'file #f) ;; leave it the way it should be left
						  ))
				  (parent-agent-shutdown) ;; Parents should shutdown last
				  )

(model-method (<log-data> <log-introspection> <symbol>) (page-preamble self logger format)
				  (kdebug 'log-issues "In: log-data preamble filename " (my 'filename) "and file" (my 'file))

				  (parent-page-preamble) ;; opens the file

				  (kdebug 'log-issues "In: log-data, after logfile preamble filename " (my 'filename) "and file" (my 'file))

				  (kdebug 'log-init "Logfile is" (my 'filename)
							"(output-port?" (my 'file) ") =" (output-port? (my 'file)))
				  (if (not (output-port? (my 'file)))
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
										 (display (string-append "# " (name (car il))) file)
										 (newline file)))
							  
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
									(display "# " file)
									(for-each
									 (lambda (x) (display " " file) (display x file))
									 header)
									(newline file))
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
				  (let ((kdebug (if #f kdebug dnl*))
						  )
									 
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
										(display "**" file)
										(for-each ;; field in the variable list
										 (lambda (field)
											(if show-field-name
												 (begin
													(display " " file)
													(display field file)))

											(cond
											 ((member field
														 (map car
																(class-slots-of subject)))
											  (kdebug '(log-* log-data logging-debug)
														 "     Dumping " field "="
														 (if (has-slot? self t)
															  (slot-ref self t)
															  "missing!"))
											  
											  (display " " file)
											  (display (slot-ref subject field) file)
											  )
											 ((member field (extra-variable-list subject))
											  (display " " file)
											  (display (extra-variable subject field) file)
											  )
											 (missing-val
											  (display " " file)
											  (display missing-val file)))
											)
										 (if DONT-FILTER-TARGET-VARIABLES
											  target-variables
											  (filter (not-member (my 'dont-log)) target-variables)))
										(newline file)
										)
									 subjects)
						)
					 )
				  )

(model-method (<log-data> <log-introspection> <symbol>) (page-epilogue self logger format)
				  (kdebug '(log-* log-data) (name self) "[" (my 'name) ":"
							(cnc self) "]" "in page-epilogue")
				  (let ((ml (my-list self)))
					 (if (and (pair? ml)
								 (pair? (cdr ml)))
						  (or #t (newline (my 'file))))
					 ;; We don't want a blank line between each record!
					 ;; -- change #t to #f to get lines between "pages"
					 )
				  )






(model-method (<log-statistics> <agent>)(stats-callback agnt #!optional data-list)
				  #t)



;-  The End 


;; Local Variables:
;; mode: scheme
;; outline-regexp: ";-+"
;; comment-column:0
;; comment-start: ";; "
;; comment-end:"" 
;; End:
