; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	animal.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.07.13
;		Location: zero:/home/randall/Thesis/Example-Model/model/animal.scm
;
;	History:

;-  Discussion 

;;; This pulls in the files for the animal class

(register-submodel 'simple-animal)
(register-submodel 'animal)

(load "animal-classes.scm")
(load "animal-declarations.scm")
(load "animal-methods.scm")

;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" -;
;;; comment-start: ";;; " -;
;;; mode: scheme -;
;;; outline-regexp: ";-+" -;
;;; comment-column: 0 -;
;;; End:
