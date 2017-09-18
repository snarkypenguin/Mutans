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

(define (timing-report #!optional flag)
  (let ((mtd (case flag
				  ((#f all cpu) (sort model-timing-data (lambda (x y) (<= (cadr x) (cadr y)))))
				  ((sys) (sort model-timing-data (lambda (x y) (<= (cadr x) (caddr y)))))
				  ((real) (sort model-timing-data (lambda (x y) (<= (cadr x) (cadddr y)))))
			  )



  (for-each
	(lambda (x)
	  (apply dnl* (let* ((s (object->string (car x))) (S (string-append (make-string (max 0 (- 40 (string-length s))) #\space) s))) S)
				(case flag
				  ((#f)  (list "| cpu:"  (cadr x) ": real-time" (cadddr x)))
				  ((all) (list "| cpu:"  (cadr x) ": sys" (caddr x) ": real-time" (cadddr x)))
				  ((cpu) (list "| cpu:"  (cadr x)))
				  ((sys) (list "| sys" (caddr x)))
				  ((real) (list "| real-time" (cadddr x)))
			  )))
	model-timing-data))

(define (aggregate-model-timing-data)
  (for-each
	(lambda (x)
	  (let ((dat (apply add (cdr x))))
		 (set-cdr! x dat) ;; dat is a list of lists of numbers, we want the colum sums
		 ))
	model-timing-data))

(define (elapsed-times tag)
  (let ((v (assq tag model-timing-data)))
	 (if v (list-tail  1) v)))

(define (elapsed-cpu-time tag)
  (let ((v elapsed-times tag))
	 (if v (car v) v)))

(define (elapsed-sys-time tag)
  (let ((v elapsed-times tag))
	 (if v (cadr v) v)))

(define (elapsed-real-time tag)
  (let ((v elapsed-times tag))
	 (if v (caddr v) v)))


