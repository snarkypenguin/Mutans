; -*- mode: scheme; -*-
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

;--
;	diffeq-classes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.12.07
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/diffeq-classes.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2016 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 
(include "remodel-framework")

(define-class <diffeq-system>
  (inherits-from <agent>)
  (state-variables
	variable-names        ;; list of strings associated with each value
	variable-values       ;; list of strings associated with each value
	variable-symbols      ;; unique symbols for each of the values
	d/dt-list             ;; differential equations which describe the dynamics
	subdivisions          ;; Number of intervals a given dt is subdivided into
	variable-values       ;; the list of symbol+values returned from get-externals
	                      ;; or from P
	get-externals         ;; a list of symbol+functions which return the current 
	                      ;; state of the external variables
	external-update       ;; an a-list of symbol+functions which update the external
                         ;; variables			
	domains               ;; List of predicates that define the domains of each of the
	                      ;; variables (returns #f if the value is outside the domain)

   ;;; The order of all the preceeding lists must be consistent. There is no way for
	;;; the code to ensure that this is so.  Be warned.

	;; An alternate representation for input
	variable-definitions  ;; list of the form ((nameA dA/dt) ...)  or (nameZ agent accessor)

	too-small             ;; time steps smaller than this cause an abort
	epsilon               ;; for little things
	do-processing         ;; if #f, no processing is done at all

	P                     ;; This is generated using rk4* and d/dt-list
	;; The functions can return either the value passed in, #f or a "clipped value"
	;; a boolean false indicates a catastrophic failure, a clipped value keeps things
	;; going (restricted to the domain) otherwise it just acts like the (lambda (x) x)
	))





;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
