;#| -*-Scheme-*-
;
;$Id: copyright.scm,v 1.4 2005/12/13 06:41:00 cph Exp $
;
;Copyright 2005 Massachusetts Institute of Technology
;
;This file is part of MIT/GNU Scheme.
;
;MIT/GNU Scheme is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 2 of the License, or (at
;your option) any later version.
;
;MIT/GNU Scheme is distributed in the hope that it will be useful, but
;WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with MIT/GNU Scheme; if not, write to the Free Software
;Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
;USA.
;
;|#



;;; UNIFORM-RANDOM produces an inexact number x,    0 <= x < 1

;#|
;(define uniform-random
;  (let* ((random-max (expt 2 23))
;	 (frandom-max (exact->inexact random-max)))
;    (lambda ()
;      (/ (random random-max)
;	 frandom-max))))
;|#

(define uniform-random random-real)

(define (nonzero-uniform-random)
  (let ((x (uniform-random)))
    (if (= x 0.)
	(nonzero-uniform-random)
	x)))

;;; Given uniform random numbers, we can produce pairs of
;;; gaussian-distributed numbers, with zero mean and unit
;;; standard deviation, by the following trick:

(define (gaussian-random-pair)
  ;; continue = (lambda (y1 y2) ...)
  (let ((x1 (uniform-random))
		  (x2 (uniform-random)))
    (let ((r (sqrt (* -2.0 (log x1)))))
      (cons (* r (cos (* 2pi x2))) (* r (sin (* 2pi x2)))))))

(define (gaussian-random)
  (car (gaussian-random-pair)))
