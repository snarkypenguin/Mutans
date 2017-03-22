; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	load-parameters.scm -- Written by Randall Gray 

;-  Discussion 
; This loads the parameterisation into the top level environment.
; By doing things this way, we can set up default parameters for
; component classes and have refinements of models override the
; basic parameterisation, and finally allow the "taxon" specific
; parameterisations to override all other parameterisations.
;
; Initialisation might be slower, with a fully populated directory,
; but it makes the process of gradually refining and deepening the
; model much easier.





;-- Load the files -- expressions will be evaluated in the process
;;  Note that the indentation is a rough attempt to group things
;;  into their chain of inheritance, but don't rely on this.

(load "params/<object>")
(load "params/<polygon>")
(load "params/<circle>")
(load "params/<agent>")
  (load "params/<model-maintenance>")
  (load "params/<introspection>")
    (load "params/<monitor>")
      (load "params/<niche-monitor>")
      (load "params/<agent-monitor>")
      (load "params/<domain-monitor>")
    (load "params/<log-introspection>")
      (load "params/<log-data>")
      (load "params/<snapshot>")
      (load "params/<logfile>")
      (load "params/<log-map>")

  (load "params/<blackboard>")

  (load "params/<population-system>")
  (load "params/<diffeq-system>")

  (load "params/<thing>")
    (load "params/<tracked-agent>")

    (load "params/<animal>")
      (load "params/<simple-animal>")
      (load "params/<example-animal>")
      (load "params/<simple-metabolism>")
      (load "params/<metabolism>")

   (load "params/<simple-plant>")
   (load "params/<example-plant>")

  (load "params/<environment>")
    (load "params/<ecoservice>")
    (load "params/<patch>")
    (load "params/<dynamic-patch>")
    (load "params/<habitat>")
    (load "params/<habitat*>")
    (load "params/<landscape>")



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
