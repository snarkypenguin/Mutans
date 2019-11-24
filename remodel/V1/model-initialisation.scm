;-  Identification and Changes

;--
;	model-initialisation.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2014.10.12
;		Location: pooh:/local/home/randall/study/src/model-initialisation.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2014 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 



(if (member 'seabed model-list) (load "seabed-init.scm"))
;(if (member 'dinos model-list) (load "velociraptor-init.scm"))
(if (member 'savanna model-list) (load "savanna-init.scm"))



;; Now edit the following file if necessary.....
(load "pre-run-setup.scm")



;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
