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

;	papersizes.scm


;; ISO paper sizes ... all sizes are in mm, modelspace should be projected into mm
(define iso4A0	'(1682  2378))
(define iso2A0	'(1189  1682))
(define isoA0	'(841 1189))
(define isoB0	'(1000 1414))
(define isoC0	'(917 1297))
(define isoA1	'(594 841))
(define isoB1	'(707 1000))
(define isoC1	'(648 917))
(define isoA2	'(420 594))
(define isoB2	'(500 707))
(define isoC2	'(458 648))
(define isoA3	'(297 420))
(define isoB3	'(353 500))
(define isoC3	'(324 458))
(define isoA4	'(210 297))
(define isoB4	'(250 353))
(define isoC4	'(229 324))
(define isoA5	'(148 210))
(define isoB5	'(176 250))
(define isoC5	'(162 229))
(define isoA6	'(105 148))
(define isoB6	'(125 176))
(define isoC6	'(114 162))
(define isoA7	'(74 105))
(define isoB7	'(88 125))
(define isoC7	'(81 114))
(define isoA8	'(52 74))
(define isoB8	'(62 88))
(define isoC8	'(57 81))
(define isoA9	'(37 52))
(define isoB9	'(44 62))
(define isoC9	'(40 57))
(define isoA10	'(26 37))
(define isoB10	'(31 44))
(define isoC10	'(28 40))
(define iso4A0r	'(2378 1682))
(define iso2A0r	'(1682 1189))
(define isoA0r	'(1189 841))
(define isoB0r	'(1414 1000))
(define isoC0r	'(1297 917))
(define isoA1r	'(841 594))
(define isoB1r	'(1000 707))
(define isoC1r	'(917 648))
(define isoA2r	'(594 420))
(define isoB2r	'(707 500))
(define isoC2r	'(648 458))
(define isoA3r	'(420 297))
(define isoB3r	'(500 353))
(define isoC3r	'(458 324))
(define isoA4r	'(297 210))
(define isoB4r	'(353 250))
(define isoC4r	'(324 229))
(define isoA5r	'(210 148))
(define isoB5r	'(250 176))
(define isoC5r	'(229 162))
(define isoA6r	'(148 105))
(define isoB6r	'(176 125))
(define isoC6r	'(162 114))
(define isoA7r	'(105 74))
(define isoB7r	'(125 88))
(define isoC7r	'(114 81))
(define isoA8r	'(74 52))
(define isoB8r	'(88 62))
(define isoC8r	'(81 57))
(define isoA9r	'(52 37))
(define isoB9r	'(62 44))
(define isoC9r	'(57 40))
(define isoA10r	'(37 26))
(define isoB10r	'(44 31))
(define isoC10r	'(40 28))

(define USletter '(216 279))
(define USlegal '(216 356))
(define UStabloid '(279 432))
(define USledger  '(432 279 ))

(define ArchA '(229 305))
(define Arch1 '(229 305))
(define ArchB '(305 457))
(define Arch2 '(305 457))
(define ArchC '(457 610))
(define Arch3 '(457 610))
(define ArchD '(610 914))
(define Arch4 '(610 914))
(define ArchE1 '(762 1067))
(define Arch5 '(762 1067))
(define ArchE '(914 1219))
(define Arch6 '(914 1219))
	 
  

;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
