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


(define model-timing-data '())

(define (timing-report)
  (for-each
	(lambda (x)
	  (dnl* (let* ((s (object->string (car x))) (S (string-append (make-string (max 0 (- 40 (string-length s))) #\space) s))) S)
;			  "| cpu:"  (cadr x) ": sys" (caddr x) ": real-time" (cadddr x)
			  "| cpu:"  (cadr x) ": real-time" (cadddr x)
			  ))
	model-timing-data))

(define (aggregate-model-timing-data)
  (dnl* 'Oink)
  (for-each
	(lambda (x)
	  (let ((dat (apply add (cdr x))))
		 (set-cdr! x dat) ;; dat is a list of lists of numbers, we want the colum sums
		 ))
	model-timing-data))

