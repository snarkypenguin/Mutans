; -*- mode: scheme; -*-
;-  Identification and Changes

;	load-parameters.scm -- Written by Randall Gray 

;- Discussion parameter loading should occur as the first step in
; loading a model configuration -- that is *after* the kernel and the
; rest of the modelling framework has been loaded, but before agents
; and objects are created.
;
; ***   Currently this happens at the end of "loadem.scm"   ***
;

; This loads the parameterisation into the top level environment.
; By doing things this way, we can set up default parameters for
; component classes and have refinements of models override the
; basic parameterisation, and finally allow the "taxon" specific
; parameterisations to override all other parameterisations.
;
; Initialisation might be slower, with a fully populated directory,
; but it makes the process of gradually refining and deepening the
; model much easier.

;- Base units are the SI units.  For the moment we only have mass,
; time, length and volume. We define any extensions to the set of units
; available to the parameters here too.  Most of the units are
; declared in "units.scm" along with a few convenient conversion
; functions.


;- Load the files -- expressions will be evaluated in the process
;;  Note that the indentation is a rough attempt to group things
;;  into their chain of inheritance, but don't rely on this.

;;(define parameter-dir "params")



;; (dnl "Loading explicit parameter files"
;;  (load (string-append parameter-dir "/<object>"))
;;  (load (string-append parameter-dir "/<polygon>"))
;;  (load (string-append parameter-dir "/<circle>"))
;;  (load (string-append parameter-dir "/<agent>"))
;;   (load (string-append parameter-dir "/<model-maintenance>"))
;;   (load (string-append parameter-dir "/<introspection>"))
;;     (load (string-append parameter-dir "/<monitor>"))
;;       (load (string-append parameter-dir "/<niche-monitor>"))
;;       (load (string-append parameter-dir "/<agent-monitor>"))
;;       (load (string-append parameter-dir "/<domain-monitor>"))
;;     (load (string-append parameter-dir "/<log-introspection>"))
;;       (load (string-append parameter-dir "/<log-data>"))
;;       (load (string-append parameter-dir "/<snapshot>"))
;;       (load (string-append parameter-dir "/<logfile>"))
;;       (load (string-append parameter-dir "/<log-map>"))

;;   (load (string-append parameter-dir "/<blackboard>"))

;;   (load (string-append parameter-dir "/<population-system>"))
;;   (load (string-append parameter-dir "/<diffeq-system>"))

;;   (load (string-append parameter-dir "/<thing>"))
;;     (load (string-append parameter-dir "/<tracked-agent>"))

;;     (load (string-append parameter-dir "/<animal>"))
;;       (load (string-append parameter-dir "/<simple-animal>"))
;;       (load (string-append parameter-dir "/<example-animal>"))
;;       (load (string-append parameter-dir "/<simple-metabolism>"))
;;       (load (string-append parameter-dir "/<metabolism>"))

;;    (load (string-append parameter-dir "/<plant>"))
;;    (load (string-append parameter-dir "/<example-plant>"))

;;   (load (string-append parameter-dir "/<environment>"))
;;     (load (string-append parameter-dir "/<ecoservice>"))
;;     (load (string-append parameter-dir "/<patch>"))
;;     (load (string-append parameter-dir "/<dynamic-patch>"))
;;     (load (string-append parameter-dir "/<habitat>"))
;;     (load (string-append parameter-dir "/<habitat*>"))
;;     (load (string-append parameter-dir "/<landscape>"))

(let ((pd (open-directory parameter-dir)))
  (kdnl* 'parameter-load "Loading parameters from" parameter-dir)
	(if (port? pd)
		 (for-each
		  (lambda (fn)
			 (let ((fn (string-append parameter-dir "/" fn)))
				(if (check-param-sig fn)
					 (begin
						(dnl* 'loading fn)
						(load fn))
					 (dnl* fn "is not a parameter file:" fn)
					 )))
		  (read-all pd))
		 (error  "Cannot open parameter directory!" parameter-dir)
		 ))
 


;--    Public data 

;-  Code 

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
