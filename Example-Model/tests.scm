; -*- mode: scheme; -*-
(load "examples.scm")


(test-properties T1 t1 (tree* -1 t1))
(test-properties T1 t1 t2)


(pp (tree+ t1 (tree* -1 t1)))

(dnl (test-distribution T1 t1 (tree* -1 t1))))


;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
