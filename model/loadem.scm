; -*- mode: scheme; -*-

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


(define (sym->scm x) (let ((fn (string-append (symbol->string x) ".scm")))
							  ;;(display fn)(newline)
							  fn))


(define slib-gambit.init "/usr/share/slib/gambit.init")
(define SLIB #f)

;; Must happen before framework  or they have no effect
(load "preamble.scm")
;(load 'print-loading)

;; "model-flags.scm" contains flags which have effect when the model
;; is running either interpretively or as compiled code.  In
;; particular, the flag "production-run" will cause the model-method
;; and model-body macros to include argument checking code in some
;; calls like (set-my ...) and (my ...).

;; framework-flags.scm contains flags which only have effect during the
;; initial parsing of the code.


(for-each load (map sym->scm '(sort wildmatch utils timer tree-ring kdebug model-flags sclos registers)))
(include "framework") ;; must come before sclos+extn.scm

(for-each load (map sym->scm '(sclos+extn)))

(for-each load (map sym->scm '(units constants maths integrate matrix papersizes postscript glyphs)))

;(load "support.o1")

(for-each load (map sym->scm '(basic-population)))
;; postscript should (logically) come after the mathematical files ... it uses matrices and such			 
(for-each load (map sym->scm '(framework-declarations framework framework-classes)))
(for-each load (map sym->scm '(introspection-classes monitor-classes log-classes)))
(for-each load (map sym->scm '(landscape-classes plant-classes animal-classes)))
(for-each load (map sym->scm '(framework-wrappers framework-methods)))
;; We want to load a single declarations file....(for-each load (map sym->scm '(framework-wrappers declarations framework-methods)))
(for-each load (map sym->scm '(introspection-methods monitor-methods log-methods)))
(for-each load (map sym->scm '(landscape-methods oak plant-methods animal-methods)))

;; The kernel alway comes last.
(load "kernel.scm")
;(load "kernel.o1")

(load "model-flags.scm") ;; Flags for kdebug/kdebug? messages/code and other global flags
(load "parameters.scm") ;; code to handle the parameter files 


;; This comes last ... things go awry otherwise?
(if (file-exists? slib-gambit.init)
	 (begin
		(load slib-gambit.init)
		(require 'printf) ;; for controlling output precision
		(require 'charplot) ;; for rough, ascii plotting within the interpreter.
		(set! SLIB #t)
		))
			 

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
