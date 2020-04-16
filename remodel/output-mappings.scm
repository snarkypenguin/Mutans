; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	output-mappings.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2017.05.26
;		Location: zero:/home/randall/Thesis/model/output-mappings.scm
;
;	History:
;

"This requires the variables defined in the file 'papersizes.scm' to
be accessible.  The values in the variables are expected to be in
millimetres. The variable 'Domain', a list containing the lower left
and upper right ordinates of the simulation, must also be defined."

;; Mappings for ISO paper sizes
(define ->ps-iso4A0 (map:domain-to-postscript Domain '(1682  2378) 10))
(define ->ps-iso2A0 (map:domain-to-postscript Domain '(1189  1682) 10))
(define ->ps-isoA0 (map:domain-to-postscript Domain '(841 1189) 10))
(define ->ps-isoB0 (map:domain-to-postscript Domain '(1000 1414) 10))
(define ->ps-isoC0 (map:domain-to-postscript Domain '(917 1297) 10))
(define ->ps-isoA1 (map:domain-to-postscript Domain '(594 841) 10))
(define ->ps-isoB1 (map:domain-to-postscript Domain '(707 1000) 10))
(define ->ps-isoC1 (map:domain-to-postscript Domain '(648 917) 10))
(define ->ps-isoA2 (map:domain-to-postscript Domain '(420 594) 10))
(define ->ps-isoB2 (map:domain-to-postscript Domain '(500 707) 10))
(define ->ps-isoC2 (map:domain-to-postscript Domain '(458 648) 10))
(define ->ps-isoA3 (map:domain-to-postscript Domain '(297 420) 10))
(define ->ps-isoB3 (map:domain-to-postscript Domain '(353 500) 10))
(define ->ps-isoC3 (map:domain-to-postscript Domain '(324 458) 10))
(define ->ps-isoA4 (map:domain-to-postscript Domain '(210 297) 10))
(define ->ps-isoB4 (map:domain-to-postscript Domain '(250 353) 10))
(define ->ps-isoC4 (map:domain-to-postscript Domain '(229 324) 10))
(define ->ps-isoA5 (map:domain-to-postscript Domain '(148 210) 10))
(define ->ps-isoB5 (map:domain-to-postscript Domain '(176 250) 10))
(define ->ps-isoC5 (map:domain-to-postscript Domain '(162 229) 10))
(define ->ps-isoA6 (map:domain-to-postscript Domain '(105 148) 10))
(define ->ps-isoB6 (map:domain-to-postscript Domain '(125 176) 10))
(define ->ps-isoC6 (map:domain-to-postscript Domain '(114 162) 10))
(define ->ps-isoA7 (map:domain-to-postscript Domain '(74 105) 10))
(define ->ps-isoB7 (map:domain-to-postscript Domain '(88 125) 10))
(define ->ps-isoC7 (map:domain-to-postscript Domain '(81 114) 10))
(define ->ps-isoA8 (map:domain-to-postscript Domain '(52 74) 10))
(define ->ps-isoB8 (map:domain-to-postscript Domain '(62 88) 10))
(define ->ps-isoC8 (map:domain-to-postscript Domain '(57 81) 10))
(define ->ps-isoA9 (map:domain-to-postscript Domain '(37 52) 10))
(define ->ps-isoB9 (map:domain-to-postscript Domain '(44 62) 10))
(define ->ps-isoC9 (map:domain-to-postscript Domain '(40 57) 10))
(define ->ps-isoA10 (map:domain-to-postscript Domain '(26 37) 10))
(define ->ps-isoB10 (map:domain-to-postscript Domain '(31 44) 10))
(define ->ps-isoC10 (map:domain-to-postscript Domain '(28 40) 10))
(define ->ps-iso4A0r (map:domain-to-postscript Domain '(2378 1682) 10))
(define ->ps-iso2A0r (map:domain-to-postscript Domain '(1682 1189) 10))
(define ->ps-isoA0r (map:domain-to-postscript Domain '(1189 841) 10))
(define ->ps-isoB0r (map:domain-to-postscript Domain '(1414 1000) 10))
(define ->ps-isoC0r (map:domain-to-postscript Domain '(1297 917) 10))
(define ->ps-isoA1r (map:domain-to-postscript Domain '(841 594) 10))
(define ->ps-isoB1r (map:domain-to-postscript Domain '(1000 707) 10))
(define ->ps-isoC1r (map:domain-to-postscript Domain '(917 648) 10))
(define ->ps-isoA2r (map:domain-to-postscript Domain '(594 420) 10))
(define ->ps-isoB2r (map:domain-to-postscript Domain '(707 500) 10))
(define ->ps-isoC2r (map:domain-to-postscript Domain '(648 458) 10))
(define ->ps-isoA3r (map:domain-to-postscript Domain '(420 297) 10))
(define ->ps-isoB3r (map:domain-to-postscript Domain '(500 353) 10))
(define ->ps-isoC3r (map:domain-to-postscript Domain '(458 324) 10))
(define ->ps-isoA4r (map:domain-to-postscript Domain '(297 210) 10))
(define ->ps-isoB4r (map:domain-to-postscript Domain '(353 250) 10))
(define ->ps-isoC4r (map:domain-to-postscript Domain '(324 229) 10))
(define ->ps-isoA5r (map:domain-to-postscript Domain '(210 148) 10))
(define ->ps-isoB5r (map:domain-to-postscript Domain '(250 176) 10))
(define ->ps-isoC5r (map:domain-to-postscript Domain '(229 162) 10))
(define ->ps-isoA6r (map:domain-to-postscript Domain '(148 105) 10))
(define ->ps-isoB6r (map:domain-to-postscript Domain '(176 125) 10))
(define ->ps-isoC6r (map:domain-to-postscript Domain '(162 114) 10))
(define ->ps-isoA7r (map:domain-to-postscript Domain '(105 74) 10))
(define ->ps-isoB7r (map:domain-to-postscript Domain '(125 88) 10))
(define ->ps-isoC7r (map:domain-to-postscript Domain '(114 81) 10))
(define ->ps-isoA8r (map:domain-to-postscript Domain '(74 52) 10))
(define ->ps-isoB8r (map:domain-to-postscript Domain '(88 62) 10))
(define ->ps-isoC8r (map:domain-to-postscript Domain '(81 57) 10))
(define ->ps-isoA9r (map:domain-to-postscript Domain '(52 37) 10))
(define ->ps-isoB9r (map:domain-to-postscript Domain '(62 44) 10))
(define ->ps-isoC9r (map:domain-to-postscript Domain '(57 40) 10))
(define ->ps-isoA10r (map:domain-to-postscript Domain '(37 26) 10))
(define ->ps-isoB10r (map:domain-to-postscript Domain '(44 31) 10))
(define ->ps-isoC10r (map:domain-to-postscript Domain '(40 28) 10))

(define ->ps-USletter (map:domain-to-postscript Domain '(216 279) 10))
(define ->ps-USlegal (map:domain-to-postscript Domain '(216 356) 10))
(define ->ps-UStabloid (map:domain-to-postscript Domain '(279 432) 10))
(define ->ps-USledger (map:domain-to-postscript Domain '(432 279 ) 10))

(define ->ps-ArchA (map:domain-to-postscript Domain '(229 305) 10))
(define ->ps-Arch1 (map:domain-to-postscript Domain '(229 305) 10))
(define ->ps-ArchB (map:domain-to-postscript Domain '(305 457) 10))
(define ->ps-Arch2 (map:domain-to-postscript Domain '(305 457) 10))
(define ->ps-ArchC (map:domain-to-postscript Domain '(457 610) 10))
(define ->ps-Arch3 (map:domain-to-postscript Domain '(457 610) 10))
(define ->ps-ArchD (map:domain-to-postscript Domain '(610 914) 10))
(define ->ps-Arch4 (map:domain-to-postscript Domain '(610 914) 10))
(define ->ps-ArchE1 (map:domain-to-postscript Domain '(762 1067) 10))
(define ->ps-Arch5 (map:domain-to-postscript Domain '(762 1067) 10))
(define ->ps-ArchE (map:domain-to-postscript Domain '(914 1219) 10))
(define ->ps-Arch6 (map:domain-to-postscript Domain '(914 1219) 10))
	 
  

;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:



;-  The End 


;;; Local Variables: 
;;; comment-end: ""
;;; comment-start: "; "
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column: 0
;;; End:
