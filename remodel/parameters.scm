; -*- mode: scheme; -*-
;-  Identification and Changes

;	parameters.scm -- Written by Randall Gray 

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

;- Discussion

; parameter loading should occur as the first step in loading a model
; configuration -- that is *after* the kernel and the rest of the
; modelling framework has been loaded, but before agents and objects
; are created.

; ***   Currently this happens at the end of "loadem.scm"   ***


; This loads the parameterisation into the top level environment.
; By doing things this way, we can set up default parameters for
; component classes and have refinements of models override the
; basic parameterisation, and finally allow the "taxon" specific
; parameterisations to override all other parameterisations.

;- Base units are the SI units.  For the moment we only have mass,
; time, length and volume. We define any extensions to the set of units
; available to the parameters here too.  Most of the units are
; declared in "units.scm" along with a few convenient conversion
; functions.


;- Load the files -- expressions will be evaluated in the process
; Note: The order they are loaded in really does not matter.

(define (load-parameter-set parameter-dir)
  (let ((pd (open-directory parameter-dir)))
	 (kdebug 'parameter-load "Loading parameters from" parameter-dir)
	 (if (port? pd)
		  (for-each
			(lambda (fn)
			  (let ((fn (string-append parameter-dir "/" fn)))
				 (if (check-param-sig fn)
					  (begin
						 (kdebug 'loading "Loading " fn)
						 (load fn))
					  (dnl* fn "is not a parameter file:" fn)
					  )))
			(read-all pd))
		  (error  "Cannot open parameter directory!" parameter-dir)
		  ))
  )


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
