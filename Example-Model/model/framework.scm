; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;  framework.scm -- Written by Randall Gray 
;  Initial coding: 
;     Date: 2016.07.26
;     Location: zero:/home/randall/Thesis/Example-Model/model/framework.scm
;
;  History:
;
;  This has the "defined" bits, not just the macros.

;-  Code 


(if #t
    ;; If you are *not* running with the code

           ;;; ;;; (define load-level #f)
           ;;; ;;; (let* ((bl load)
           ;;; ;;;       (ll 0)
           ;;; ;;;       (nl (lambda x
           ;;; ;;;             (set! ll (+ ll 1))
           ;;; ;;;             (apply bl x)
           ;;; ;;;             (set! ll (- ll 1))))
           ;;; ;;;       (ll? (lambda () ll)))
           ;;; ;;;   (set! load nl)
           ;;; ;;;   (set! load-level ll?))
           
    ;; this block should not be run; it only exists to remind me
    ;; to (include ...) the framework file rather than (load ...) it.

    (if (not (zero? (load-level)))
        (error (string-append "You should be including the framework file,"
                              " rather than loading it.\n")))
)

;; These are not necessarily controls, but they are used in a similar
;; fashion and need to preceed framework-classes.

(define (class-name-of x)
  (cond
   ((isa? x <agent>) 
    (let ((n (class-register 'name? (class-of x))))
      (and n
           ;;(string->symbol (string-append "instance-of-"
           ;;   (symbol->string n)))
           n
          )
      ))
   ((and (sclos-object? x) (assoc x (class-register))) 
    (let ((n (class-register 'name? x)))
      (and n
           (string->symbol (string-append "class:" (symbol->string n)))
           )))
   (else #f)))
   

(define class-register 
  (let ((register '())
        )
    (lambda args
      (if (null? args)
          register
          (let ((cmd (car args))
                (opts (if (null? (cdr args)) #f (cdr args))))
            (cond
             ((and (eq? cmd 'dump))
              (for-each dnl register)
              )

             ((and (eq? cmd 'add)
                   opts
                   (assq (car opts) register))
              (error "Attempting to re-register a class!" opts args)
              )

             ((and (eq? cmd 'add) opts (not (assq (car opts) register)))
              (set! register ;; save things as lists
                    (acons (car opts) (cdr opts) register)) 
              )

             ((and (eq? cmd 'name?) opts)
              (let ((a (assq (car opts) register)))
                (and a (cadr a))))

             ((and (eq? cmd 'rec-by-class) opts)
              (let ((a (assq (car opts) register)))
                a))

             ((and (eq? cmd 'class?) opts)
              (let ((a (filter (lambda (x)
                                 (eq? (car opts) (cadr x)))
                               register)))
                (and a (car a) (caar a))))

             ((and (eq? cmd 'rec-by-name) opts)
              (let ((a (filter (lambda (x)
                                 (eq? (car opts) (cadr x)))
                               register)))
                (and a (car a))))

             (else (error "failed call to class-register"
                          args))))))))
                  

(define generic-register 
  (let ((register '())
        )
    (lambda args
      (if (null? args)
          register
          (let ((cmd (car args))
                (opts (if (null? (cdr args)) #f (cdr args))))
            (cond
             ((and (eq? cmd 'dump))
              (for-each dnl register)
              )

             ((and (eq? cmd 'add)
                   opts
                   (not (assq (car opts) register)))
              (set! register ;; save things as lists
                    (acons (car opts) (cdr opts) register)) 
              )

             ((and (eq? cmd 'name?) opts)
              (let ((a (assq (car opts) register)))
                (and a (cadr a))))

             ((and (eq? cmd 'rec-by-generic) opts)
              (let ((a (assq (car opts) register)))
                a))

             ((and (eq? cmd 'generic?) opts)
              (let ((a (filter (lambda (x)
                                 (eq? (car opts) (cadr x)))
                               register)))
                (and a (car a) (caar a))))

             ((and (eq? cmd 'rec-by-name) opts)
              (let ((a (filter (lambda (x)
                                 (eq? (car opts) (cadr x)))
                               register)))
                (and a (car a))))

             (else (error "failed call to generic-register"
                          args))))))))
                  

(define (sclos-object? a)
  (and (%instance? a) #t))

(define (agent? a)
  (and (%instance? a) (isa? a <agent>) #t))

(define (has-slot? a k) 
  (member k (map car (class-slots (class-of a)))))

(define (dnl* . args)
  (if (not (null? args))
      (let ((h (car args))
            (t (cdr args)))
        (display (car args))
        (for-each (lambda (x) (display " ") (display x)) t)))
  (newline))

  
(define (dnl . args)
  (if (not (null? args))
      (let ((h (car args))
            (t (cdr args)))
        (display (car args))
        (for-each (lambda (x) (display x)) t)))
  (newline))
  

(define files-included-immediately
  '("framework-controls.scm"     
    "framework-classes.scm"      
    "framework-declarations.scm" 
    "framework-methods.scm"))

(define framework-load
  (let* ((oload load)
         (always-print-loading #f)
         (always-print-loaded #f)
         (always-load #f)
         (warn-loaded #f)
         (loaded '())
         (loading '())
         (prload (lambda (x y)
                   (display (string-append "\n[" x " "))
                   (display y)(display "]\n")))
         )
    (lambda (fn . args)

      (if (not (null? fn))
          (if (member fn loaded)
              (if warn-loaded
                  (dnl "[" fn " is already loaded"))
              (cond
               ((member fn (list "loaded?"))
                (for-each dnl (reverse loaded))
                )
               ((and (string? fn) (not (null? args)))
                (dnl "forced loading of " fn)
                (oload fn) ;; forces a load, doesn't record it
                )
               ((eq? fn 'warn-loaded)
                (set! warn-loaded #t))

               ((eq? fn '!warn-loaded)
                (set! warn-loaded #f))

               ((eq? fn 'always-load)
                (set! always-load #t))

               ((eq? fn '!always-load)
                (set! always-load #f))

               ((eq? fn '!print-loaded)
                (set! always-print-loaded #f))

               ((eq? fn '!print-loading!)
                (set! always-print-loading #f))

               ((eq? fn 'print-loaded)
                (set! always-print-loaded #t))

               ((eq? fn 'print-loading!)
                (set! always-print-loading #t))

               ((member fn '(loaded loaded-list loaded? loaded-list?))
                (prload "loaded" loaded))

               ((member fn '(loading loading-list
                                     loading? loading-list? load-list?))
                (prload "loading" loading))

               (#t
                (set! loading (cons fn loading))
                (if always-print-loading (prload "loading" loading))
                (if (or always-load (not (member fn loaded)))
                    (begin
                      (oload fn)
                      (set! loading (cdr loading))
                      (set! loaded (cons fn loaded))
                      (if always-print-loaded (prload "loaded" loaded))))
                ))
              )
          )))
  )

(define load framework-load)

(define (aborts . args) (error (apply string-append args)))

(define (load-model . extra-files)
  (if (and (pair? extra-files) (pair? (car extra-files)))
      (set! extra-files (car extra-files)))
  
  (if (file-exists? "support-lib.o1")
		(load "support-lib.o1")
		(load "support-lib.scm"))
  
  (for-each load files-included-immediately)

  ;; submodel code -- all the classes get loaded here
  (load "submodels.scm")

  ;; The kernel -- last because it *feels* right.
  (load "kernel.scm")
  (load "model-configuration.scm")
  (if (pair? extra-files)
      (for-each load extra-files))
  )



(display "Now run: (load-model) or (load-model filename ...)\n")

;-  The End 


;;; Local Variables: 
;;; comment-end: "-;" -;
;;; comment-start: ";;; " -;
;;; mode: scheme -;
;;; outline-regexp: ";-+" -;
;;; comment-column: 0 -;
;;; End:
