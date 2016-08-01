; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	point-in-poly.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.07.19
;		Location: zero:/home/randall/Thesis/Example-Model/model/point-in-poly.scm
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


(define (point-in-polygon point polygon)
  (if (not (equal? (car polygon) (car (reverse polygon))))
		(set! polygon (append polygon (list (car polygon)))))
  (display "p: ") (display point)(newline)
  (display "P: ") (display polygon)(newline)

  (let loop ((ply polygon)
				 (in-ply #f))
	 (cond
	  ((null? ply) in-ply)
	  ((ray-intersects-segment point polygon)
		(loop (cdr ply) (not ply)))
	  (#t (loop (cdr ply) ply))
	  )))
 
(define (ray-intersects-segment point segment . epsilon )
  (set! epsilon (if (null? epsilon) 0.001 (car epsilon)))
  (let ((px (car point))
		  (py (cadr point))
		  (ax (car (car segment)))
		  (ay (cadr (car segment)))
		  (bx (car (cadr segment)))
		  (by (cadr (cadr segment))))

	 (if (< ay by) (let ((z ay))
						  (set! ay by)
						  (set! by z)
						  (set! z ax)
						  (set! ax bx)
						  (set! bx z)))

      (if (or (= py ay) (= py by))
        (set! py (+ py epsilon)))

      (cond
       ;; point is above, below, or to the right of the rectangle
       ;; determined by segment; ray does not intesect the segment.
       ((or (> px (max ax bx)) (> py (max ay by)) (< py (min ay by)))
        #f)
       ;; point is to left of the rectangle; ray intersects segment
       ((< px (min ax bx))
        #t)
       ;; point is within the rectangle...
       (#t (let ((m-red (if (= ax bx)
									 #f
									 (/ (- by ay) (- bx ax))))
					  (m-blue (if (= px ax)
									  #f
									  (/ (- py ay) (- px ax)))))
            (cond
             ((null? m-blue) t)
             ((null? m-red) nil)
             (#t (>= m-blue m-red))))))))
;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" -;
;;; comment-start: ";;; " -;
;;; mode: scheme -;
;;; outline-regexp: ";-+" -;
;;; comment-column: 0 -;
;;; End:
