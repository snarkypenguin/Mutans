; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	log.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.07.13
;		Location: zero:/home/randall/Thesis/Example-Model/model/log.scm
;
;	History:

;-  Discussion 

;;; This pulls in the files for the log class

(load "log-classes.scm")
(load "log-declarations.scm")
(load "log-methods.scm")

(set! logger-tags '(logfile snapshot log-map log-data log-table log-agent-table log-table* log-agent-table*))
(for-each register-submodel logger-tags)
;;; (register-submodel 'logfile) -;
;;; (register-submodel 'snapshot) -;
;;; (register-submodel 'log-map) -;
;;; (register-submodel 'log-data) -;
;;; (register-submodel 'log-table) -;
;;; (register-submodel 'log-agent-table*) -;
;;; (register-submodel 'log-table*) -;
;;; (register-submodel 'log-agent-table) -;



;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" -;
;;; comment-start: ";;; " -;
;;; mode: scheme -;
;;; outline-regexp: ";-+" -;
;;; comment-column: 0 -;
;;; End:
