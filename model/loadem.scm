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

"
 

This is the starting point for Remodel: it loads all the components
required to run a simulation.

The preamble is loaded first. In the first small section, it provides
some of the basic things like smart-loading for files (which could
probably be improved on), it implements the warning log, protects the
global call to 'getenv', provides a function for constructing symbols
with 'tags' which are used in the kernel debugging message
support (search for 'kdebug').

After that, a smart-loader is provided to reduce multiple inclusions
of files when a model is loaded.  This serves two purposes: to reduce
the total time loading, and to keep the model's symbol-tables from
having multiple instances of functions or classes.
"

(define (sym->scm x) (let ((fn (string-append (symbol->string x) ".scm")))
							  ;;(display fn)(newline)
							  fn))

(define slib-gambit.init "/usr/share/slib/gambit.init")
(define SLIB #f)

;; the preamble has to come first so that the registers that keep track
;; of classes and methods (to prevent shadowing) are present from the
;; first instanciation.

(load "preamble.scm")
;(load 'print-loading)

;; "model-flags.scm" contains flags which have effect when the model
;; is running either interpretively or as compiled code.  In
;; particular, the flag "production-run" will cause the model-method
;; and model-body macros to include argument checking code in some
;; calls like (set-my ...) and (my ...).

;; remodel-flags.scm contains flags which only have effect during the
;; initial parsing of the code.


;; Haven't implemented a* to Remodel yet ... not really sure if it is all that useful in this context anyway.
;;(for-each load (map sym->scm '(sort wildmatch utils a*list timer tree-ring kdebug model-flags sclos registers)))

(for-each load (map sym->scm '(sort wildmatch utils timer tree-ring kdebug model-flags sclos abstract-register)))

;; Sort provides routines for sorting lists
;; wildmatch provides predicates for matching wildcard strings
;; utils is full of basic workhorse functions, mostly data manipulation and mathematical functions
;; timer provides mechanisms to estimate the amount of time spent in parts of the model
;; tree-ring provides the mathematical structure for the assessments
;; kdebug and  model-flags  are discussed above
;; sclos is a small scheme version of CLOS which overlays the class structure and resolves appropriate methods in calls.
;; abstract-registers keep track of the entities (classes, methods, instanciations...) so we don't clobber things, and have a way
;;    of interrogating them at arbitrary moments. There are several registers that are particularly important
;;        class-register -- keeps track of the classes we've created
;;        generic-method-register -- keeps track of the generic methods, so we can avoid accidentally clobbering them
;;        method-register -- much like the generic-method-register, with these two we can specify specific methods when there is ambiguity
;;        object-register -- keeps track of objects (things that cannot "run")
;;        agent-register -- keeps track of the active agents (submodels) in the system


"The remodel file defines a number of macros that help keep the model on its tracks.  There are a number of macros
which are there to help maintain robustness in the system [such as (assert test ...message...)], but there are also other
that are useful for finding bottlenecks [such as (timing-block tag . body-to-be-timed), for example], and all of the 'syntax' 
for defining agents, objects, update-closures, new registers, classes, methods, model-methods, and model-bodies
"
(include "remodel") ;; must be *included* before sclos+extn.scm and
							 ;; all files that make use of sclos: it defines
							 ;; macros that everything else needs


(for-each load (map sym->scm '(sclos+extn))) ;; This loads the
;; extensions to the class system that provide less arcane access


(for-each load (map sym->scm '(units constants maths integrate matrix papersizes postscript glyphs)))
;; load the basic utilities postscript should (logically) come after
;; the mathematical files ... it uses matrices and such

;(load "support.o1")

(for-each load (map sym->scm '(basic-population))) ;; Assumes logistic growth, predation, mortality.  Good for grass ;-)
(for-each load (map sym->scm '(remodel-declarations remodel remodel-classes))) ;; this provides very simple 'behaviours' and interactions
(for-each load (map sym->scm '(introspection-classes monitor-classes log-classes))) ;; classes which probe the state of other agents
(for-each load (map sym->scm '(landscape-classes plant-classes animal-classes))) ;; main players
(for-each load (map sym->scm '(remodel-wrappers remodel-methods)))  ;; provides methods for things common to all participants 
;; We want to load a single declarations file....(for-each load (map sym->scm '(remodel-wrappers declarations remodel-methods)))

(for-each load (map sym->scm '(introspection-methods monitor-methods log-methods)))
;; the introspection methods have special abilities to probe other
;; agents at particular times

(for-each load (map sym->scm '(landscape-methods oak plant-methods animal-methods)))
;; These implement the flora and fauna

(for-each load (map sym->scm '(assessment-methods)))
;; Assessement agents look at how things are configured with respect to the state
;; of the pertinent agents and the system as a whole, they may be able to trigger changes of representation.

;; The kernel always comes after the agent classes, precedence breaks
;; if you try and load it too early.

(load "kernel.scm")
;(load "kernel.o1")
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


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
