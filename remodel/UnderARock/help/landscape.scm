; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	landscape.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.07.13
;		Location: zero:/home/randall/Thesis/Example-Model/model/landscape.scm
;
;-  Discussion 

;;  This pulls in the code for the landscape classes (which incorporates populations)

(register-submodel 'ecoservice)
(register-submodel 'patch)
(register-submodel 'dynamic-patch)
(register-submodel 'landscape)
(register-submodel 'habitat)

(load "landscape-classes.scm")
(load "landscape-declarations.scm")
(load "landscape-methods.scm")

;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
