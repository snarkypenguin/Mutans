; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	remodel-wrappers.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.10.29
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/remodel-wrappers.scm
;
;;; This code provides a few convenience functions

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


(include "remodel")

;;; (define (set-state-variables self args) ;;;
;;;   ;; args should be null or a list of the form ('tag value ...), ;;;
;;;   ;; slotlist is a list of valid slotnames ;;;
;;;   (if (and (pair? args) (pair? (car args)) (= (length args) 1)) ;;;
;;; 		(set! args (car args))) ;;;
;;;   (let ((slots  (map car (class-slots-of self)))) ;;;
;;; 	 (for-each  ;;;
;;; 	  (lambda (slotname argval) ;;;
;;; 		 (if (member slotname slots) ;;;
;;; 			  (slot-set! self slotname argval) ;;;
;;; 			  (begin ;;;
;;; 				 (display ;;;
;;; 				  (string-append ;;;
;;; 					"Use of undeclared class variable: " ;;;
;;; 					(if (string? slotname) ;;;
;;; 						 slotname ;;;
;;; 						 (object->string slotname)) ;;;
;;; 					" in " (symbol->string (class-name-of self)) "\n")) ;;;
;;; 				 (error "+++Redo from Start+++" '--Hex:TP!=NTP)) ;;;
;;; 			  ) ;;;
;;; 		 ) ;;;
;;; 	  (evens args) (odds args))) ;;;
;;;   ) ;;;



;; This is the default logging device (procedure or method) in the absence of more specific code.


;;; (define (log-data self logger format  targets . file) ;;;
;;;   (if (null? file) ;;;
;;; 		(log-data% self logger format  targets #f) ;;;
;;; 		(log-data% self logger format  targets (car file)))) ;;;
		
(define (dump self . args)
  (if (null? args)
		(dump% self 0)
		(dump% self (car args))))

(define (patch-list self . args)
  (if (null? args)
		(patch-list% self '())
		(patch-list% self (car args))))
  
(define (services self . args)
  (if (null? args)
		(services% self '())
		(services% self (car args))))
  
(define (specific-services self . args)
  (if (null? args)
		(specific-services% self '())
		(specific-services% self (car args))))
  

(define (service-list self . args)
  (if (null? args)
		(service-list% self '())
		(service-list% self (car args))))
  


;-  The End 


;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
