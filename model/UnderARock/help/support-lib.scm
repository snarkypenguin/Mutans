; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	support-lib.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.07.13
;		Location: zero:/home/randall/Thesis/Example-Model/model/support-lib.scm
;

;-  Discussion 

;; Pulls in support files

(if #t
	 (and 
	  (load (string-append support-dir "utils.scm"))
	  (load (string-append support-dir "units.scm"))
	  (load (string-append support-dir "sort.scm"))
	  (load (string-append support-dir "maths.scm"))
	  (load (string-append support-dir "integrate.scm"))
	  (load (string-append support-dir "matrix.scm"))
	  (load (string-append support-dir "postscript.scm"))
	  (load (string-append support-dir "basic-population.scm")))
	 (load (string-append support-dir "support-lib.o1")))
