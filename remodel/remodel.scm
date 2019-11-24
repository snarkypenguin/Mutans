; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;  remodel.scm -- Written by Randall Gray 
;  Initial coding: 
;     Date: 2016.07.26
;     Location: remodel.scm
;
;  History:
;
;  This has the "defined" bits, not just the macros.
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

;-  Code 

(define (sym->scm x) (let ((fn (string-append (symbol->string x) ".scm")))
							  ;;(display fn)(newline)
							  fn))

(define slib-gambit.init "/usr/share/slib/gambit.init")
(define SLIB #f)

(include "remodel-framework")  ; Probably not necessary here, but here as an example

;;; This is chock full of macros.  Macros need to be *included* in basically every file
;;; Gambit's define-macro only creates syntax rewriters that apply to the module (file)
;;; *currently* being processed.  At least that is my understanding.

(define all-support-files-in-load-order
  (append
	;; basic libraries
	(map sym->scm '(sort wildmatch utils timer tree-ring kdebug model-flags sclos abstract-register))
	;; This loads the extensions to the class system that provide less arcane access
	(map sym->scm '(sclos+extn))

	;; load the basic utilities postscript should (logically) come after
	;; the mathematical files ... it uses matrices and such.  
	;; Since the above are bodies of code used in other projects as well,
	;; we also load the Remodel specific file below which defines things
	;; that are project specific.
	(map sym->scm '(units constants maths integrate matrix papersizes postscript glyphs)) ;;(load "support.o1")
	))

(define all-classes-in-load-order
  (append
	(map sym->scm '(basic-population)) ;; Assumes logistic growth, predation, mortality.  Good for grass ;-)
	(map sym->scm '(remodel-declarations remodel remodel-classes)) ;; this provides very simple 'behaviours' and interactions
	(map sym->scm '(introspection-classes monitor-classes log-classes)) ;; classes which probe the state of other agents
	(map sym->scm '(landscape-classes plant-classes animal-classes)) ;; main players
	(map sym->scm '(remodel-wrappers remodel-methods))  ;; provides methods for things common to all participants 
	
	;; We want to load a single declarations file....(for-each load (map sym->scm '(remodel-wrappers declarations remodel-methods)))

	(map sym->scm '(introspection-methods monitor-methods log-methods))
	;; the introspection methods have special abilities to probe other
	;; agents at particular times

	(map sym->scm '(landscape-methods oak plant-methods animal-methods))
	;; These implement the flora and fauna

	(map sym->scm '(assessment-methods))
	;; Assessement agents look at how things are configured with respect to the state
	;; of the pertinent agents and the system as a whole, they may be able to trigger changes of representation.

	))

(map load all-support-files-in-load-order)
(map load all-classes-in-load-order)

;; The kernel always comes after the agent classes, precedence breaks
;; if you try and load it too early.

(load "kernel.scm") ;(load "kernel.o1")
" The kernel manages the multiplexing of the active agents in the
system. This is accomplished using a priority queue.  The kernel code
keeps track of 'live' agents, agents which have been 'terminated' and
agents which are 'dead' but not terminated.  The distinction between
'dead' and 'terminated' is that 'dead' things might still play a role
in the simulation as habitat or food.  'terminated' agents are removed
at the earliest opportunity.

The kernel provides communication channels between agents -- most
inter-agent communication happens (or may happen) through a call to
the kernel.  It also is responsible for introducing new agents --
usually created by other agents by reproduction -- and the mechanisms
which support migration between representations.
"

(load "model-flags.scm") ;; Flags for kdebug/kdebug? messages/code and other global flags

(load "parameters.scm") ;; code to handle the parameter files 

;; This comes last ... things go awry otherwise?  We use slib for generating output and data plotting
(if (file-exists? slib-gambit.init)
	 (begin
		(load slib-gambit.init)
		(require 'printf) ;; for controlling output precision
		(require 'charplot) ;; for rough, ascii plotting within the interpreter.
		(set! SLIB #t)
		))
			 

;-  The End 


;; Local Variables: 
;; comment-end: "" ;;
;; comment-start: ";; " ;;
;; mode: scheme ;;
;; outline-regexp: ";-+" ;;
;; comment-column: 0 ;;
;; End:


