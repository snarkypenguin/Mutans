; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	introspection-classes.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.12.29
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/introspection-classes.scm
;
;	History:
;

;-  Copyright 

;
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



;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 
(include "remodel")

(define-class <introspection> (inherits-from <agent>)
  (state-variables introspection-targets ;; agents if the selector is false, otherwise classes, strings or lists
						 timestep-epsilon
						 ))

;; introspection-targets is either a list of agents or a selector function which is
;; used with filter and the runqueue.

;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
