; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	glyphs.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2017.07.29
;		Location: zero:/home/randall/Thesis/model/glyphs.scm

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
;	History:
;



"NOTE: Compound glyphs are merely a list of simple glyphs, but the
procedures which manipulate them treat the component parts rather
differently.  In a compound glyph, the head of the list is the anchor
for the rest of the components when we shift it to the origin.  Once
things are at the origin, everything else plays nicely."

(define (simple-glyph? G)
  (and (pair? G) (list? G)
		 (apply andf (map (lambda (x) (and (eq? (length x) 2) (number? (car x)) (number? (cadr x)))) G))))

(define (compound-glyph? G)
  (and (pair? G) (list? G)
		 (apply andf (map simple-glyph? G))))

(define (glyph? G) (or (simple-glyph? G) (compound-glyph? G)))


  
(define (rescale-simple-glyph scale glyph)
  (let ((rsp (lambda (x) (* scale x))))
         (map list (map rsp (map car glyph))  (map rsp (map cadr glyph)))))


(define (translate-simple-glyph offset glyph)
  (map (lambda (x) (map + offset x)) glyph))

(define (shift-simple-glyph-to-origin glyph)
  (let* ((N (length glyph))
			(Cx (/ (apply + (map car glyph)) N))
			(Cy (/ (apply + (map cadr glyph)) N))
			)
	 (translate-simple-glyph (map - (list Cx Cy)) glyph)))


(define (rescale-simple-glyph-to-unit glyph)
	 (let* ((mx (apply min (map car glyph)))
			 (Mx (apply max (map car glyph)))
			 (my (apply min (map cadr glyph)))
			 (My (apply max (map cadr glyph)))
			 (K (apply max (map abs (list mx Mx my My)))))
		(rescale-simple-glyph (/ 1.0 K) glyph)))


;; This rotation is implicitly around the origin of the glyph.
(define (rotate-simple-glyph theta glyph)
  (let ((R (2d-rotation-matrix theta)))
	 (map (lambda (p)
							  (let ((v (@ * R (make-matrix (map list p)))))
								 ;; (pp (v))
								 (map exact->inexact (apply append  (v)))))
							glyph)
							))

(define (translate-glyph offset glyph)
  (if (simple-glyph? glyph)
		(translate-simple-glyph offset glyph)
		(map (lambda (g) (translate-simple-glyph noffset g)) glyph)))

(define (shift-glyph-to-origin glyph)
  (if (and (glyph? glyph) (simple-glyph? glyph))
		(shift-simple-glyph-to-origin glyph)
		(let* ((N (length (car glyph)))
				 (Cx (/ (apply + (map caar glyph)) N))
				 (Cy (/ (apply + (map cadar glyph)) N))
				 )
		  (map
			(lambda (element)
			  (translate-simple-glyph (map - (list Cx Cy)) element))
			glyph))))

(define (rescale-glyph-to-unit glyph)
  (if (and (glyph? glyph) (simple-glyph? glyph))
		(rescale-simple-glyph-to-unit glyph)
		(let* ((mx (apply min (map caar glyph)))
				(Mx (apply max (map caar glyph)))
				(my (apply min (map cadar glyph)))
				(My (apply max (map cadar glyph)))
				(K	(apply max (map abs (list mx Mx my My)))))
		  (map (lambda (element)
					(rescale-simple-glyph (/ 1.0 K) element))
				 glyph))))

(define (rotate-glyph theta glyph)
  (if (and (glyph? glyph) (simple-glyph? glyph))
		(rotate-simple-glyph theta glyph)
		(map (lambda (x) (rotate-simple-glyph theta x)) glyph)))


(define (rescale-glyph scale glyph)
  (if (and (glyph? glyph) (simple-glyph? glyph))
		(rescale-simple-glyph scale glyph)
		(map (lambda (x) (rescale-simple-glyph scale x)) glyph)))


(define (normalise-glyph glyph)
  (rescale-glyph-to-unit (shift-glyph-to-origin glyph)))


;;-------------- Glyphs go below this line -------------------------------

(define herbi-glyph-North
  '((957 4371) (1150 4371) (1257 4317) (1377 4171) (1390 3697)
	 (1437 3597) (1470 3371) (1430 3144) (1370 2824) (1377 2697)
	 (1430 2531) (1430 2491) (1263 2197) (1143 2004) (1103 1917)
	 (1097 1857) (1097 1811) (1117 1791) (1203 1731) (1203 1691)
	 (1123 1691) (1057 1704) (970 1631) (890 1457) (777 1477)
	 (690 1497) (690 1571) (677 1704) (657 1764) (617 1797)
	 (570 1804) (497 1757) (490 1711) (497 1791) (557 1877)
	 (643 1911) (730 1984) (730 2037) (683 2157) (677 2264)
	 (663 2531) (670 2664) (737 2784) (750 2937) (717 3117)
	 (650 3311) (623 3511) (650 3651) (737 3744) (650 3977)
	 (670 4131) (770 4317) (850 4371) (1150 4371)))


(define herbi-glyph
  (normalise-glyph (rotate-glyph (- (/ pi 2)) herbi-glyph-North)))


(define carn-glyph-element-S0
  '(
	 (3600 6435) (3735 6435) (3915 6390) (4050 6210) (4230 6120) (4455 5985)
	 (4545 5850) (4590 5670) (4500 5535) (4500 5400) (4500 5265) (4500 5175)
	 (4410 5130) (4320 5130) (4185 5220) (4320 5130) (4185 4995) (4005 4950)
	 (3870 4950) (3735 4905) (3690 4905) (3690 4950) (3690 5085) (3690 5265)
	 (3690 5130) (3645 4950) (3600 4905) (3510 4860) (3465 4860) (3420 4950)
	 (3420 5040) (3330 5130) (3240 5310) (3240 5445) (3195 5625) (3240 5760)
	 (3330 5895) (3420 5985) (3420 6120) (3465 6210) (3510 6255) (3510 6345)
	 (3600 6435) (3645 6570) (3690 6615) (3780 6795) (3870 6930) (3870 7065)
	 (3915 7155) (3915 7245) (3915 7290) (3915 7560) (3915 7650) (3870 7740)
	 (3825 7785) (3825 7830) (3825 7875) (3870 8010) (3825 8145) (3960 8325)
	 (4140 8235) (4275 8100) (4320 7875) (4365 7830) (4365 7650) (4455 7515)
	 (4500 7380) (4590 7200) (4635 6885) (4635 6750) (4635 6615) (4635 6390)
	 (4635 6300) (4635 6210) (4635 6165) (4635 6120) (4680 5985) (4770 5805)
	 (4815 5760) (4860 5670) (4905 5580) (4905 5535) (4950 5445) (4995 5400)
	 (4995 5355) (4995 5310) (5040 5220) (5040 5175) (5085 5175) (5130 5040)
	 (5175 4815) (5220 4725) (5220 4635) (5220 4545) (5175 4500) (5175 4410)
	 (5130 4275) (5085 4095) (5040 4005) (4995 3960) (4950 3915) (4905 3825)
	 (4905 3780) (4905 3690) (4905 3600) (5040 3330) (5040 3285) (5085 3195)
	 (5085 3150) (5085 3015) (5130 2880) (5175 2655) (5175 2610) (5175 2520)
	 (5175 2160) (5175 2070) (5175 1890) (5175 1800) (5175 1710) (5130 1395)
	 (5130 1215) (5130 900) (5130 720) (5040 585) (5040 405) (4995 315)
	 (4950 270) (4905 270) (4860 315) (4815 360) (4815 405) (4815 495)
	 (4770 675) (4770 720) (4770 765) (4770 900) (4770 945) (4770 1035)
	 (4770 1125) (4770 1170) (4770 1260) (4725 1440) (4725 1620) (4725 1800)
	 (4725 1800) (4725 1845) (4725 1890) (4725 2025) (4725 2115) (4725 2160)
	 (4725 2250) (4725 2295) (4725 2385) (4725 2430) (4680 2520) (4635 2565)
	 (4590 2655) (4545 2745) (4500 2880) (4455 2970) (4410 3060) (4410 3150)
	 (4365 3195) (4365 3240) (4365 3285) (4365 3330) (4365 3375) (4185 3420)
	 (4095 3420) (4050 3420) (4005 3465) (3915 3465) (3825 3510) (3735 3600)
	 (3690 3645) (3645 3690) (3555 3780) (3555 3870) (3465 4005) (3420 4095)
	 (3420 4140) (3375 4230) (3330 4320) (3285 4410) (3105 4635) (3105 4680)
	 (3015 4815) (2925 4995) (2880 5085) (2835 5175) (2835 5265) (2835 5310)
	 (2790 5355) (2790 5400) (2880 5400) (2925 5400) (2970 5445) (3015 5445)
	 (3105 5310) (3150 5175) (3150 5130) (3240 4995) (3330 4905) (3375 4860)
	 (3375 4770) (3420 4680) (3375 4770) (3375 4860) (3330 4905) (3195 5085)
	 (3150 5175) (3150 5265) (3105 5355) (3015 5445) (2970 5535) (2970 5580)
	 (2970 5625) (2925 5670) (2880 5715) (2835 5850) (2790 5940) (2790 5985)
	 (2745 6075) (2745 6120) (2700 6210) (2700 6255) (2700 6345) (2655 6390)
	 (2655 6390) (2655 6390) (2655 6390) (2610 6480) (2475 6660) (2430 6840)
	 (2430 6930) (2565 6975) (2835 6840) (2880 6840) (3060 6705) (3105 6660)
	 (3375 6480) (3465 6390) (3510 6390) (3600 6435))
  )

(define carn-glyph-element-S1
  '(
	 (3555 6120) (3567 6158) (3621 6199) (3665 6237) (3668 6276) (3668 6294)
	 (3606 6294) (3698 6311) (3677 6288) (3683 6252) (3730 6237) (3825 6152)
	 (3825 6146))
  )

(define carn-glyph-element-S2
  '(
	 (3627 5764) (3783 5595) (3920 5853) (3982 5853) (4053 5844) (4121 5785)
	 (4135 5746) (4094 5723) (4032 5714) (3958 5729) (3925 5820))
  )

(define carn-glyph-element-S3
  '(
	 (3597 5782) (3624 5696) (3600 5628) (3556 5590) (3508 5590) (3482 5610)
	 (3488 5655) (3526 5723) (3594 5782))
  )

(define carn-glyph-South
  (list carn-glyph-element-S0 carn-glyph-element-S1
		  carn-glyph-element-S2 carn-glyph-element-S3))


(define carn-glyph (normalise-glyph (rotate-glyph (/ pi 2) carn-glyph-South)))

;-  (The End) 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
