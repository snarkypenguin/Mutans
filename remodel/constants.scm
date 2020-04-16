; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	constants.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.11.10
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/constants.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2016 Randall Gray

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


;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

;--- Remember the primitives

(define default-implementation-arithmetic-operator 
  (let ((originals (list (cons '+ +) (cons '- -) (cons '* *) (cons '/ /)))
		  )
	 (lambda (sym)
		(let ((r (assoc sym originals)))
		  (if r (cdr r) #f)))))


;; For constants that do not fit in the maths.scm or units.scm files.
;; Note: uninitialised and <uninitialised> are defined in preamble.scm.

;;Format            Total bits 	Significand bits   Exponent bits  Smallest number    Largest number
;;Single precision          32 	23 + 1 sign             8 	        ~ 1.2*10^-38       ~ 3.4*10^38
;;Double precision          64 	52 + 1 sign            11          ~ 2.2*10^-308 	   ~ 1.8*10^308

(define min32bitdouble 1.2e-38)
(define max32bitdouble 3.4e38)
(define max64bitdouble 1.8e8)
(define min64bitdouble 2.2e-308)



;-  The End 


;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
