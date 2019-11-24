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


(define support-dir "./")


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

               ((eq? fn 'flush)
					 (display "Flushed load list\n   ")
					 (display loaded)(newline)
                (set! loaded '())
					 )

               ((eq? fn 'warn-loaded)
					 (display "Warning when files are already loaded\n")
                (set! warn-loaded #t))

               ((eq? fn '!warn-loaded)
					 (display "Not warning when files are already loaded\n")
                (set! warn-loaded #f))

               ((eq? fn 'always-load)
					 (display "Always loading.\n")
                (set! always-load #t))

               ((eq? fn '!always-load)
					 (display "Only loading new files.\n")
                (set! always-load #f))

               ((eq? fn 'print-loaded)
					 (display "Printing loaded files at each load.\n")
                (set! always-print-loaded #t))

               ((eq? fn '!print-loaded)
					 (display "Not printing loaded files at each load.\n")
                (set! always-print-loaded #f))

               ((eq? fn 'print-loading!)
					 (display "Printing currently loading files at each load.\n")
                (set! always-print-loading #t))

               ((eq? fn '!print-loading!)
					 (display "Not printing currently loading files at each load.\n")
                (set! always-print-loading #f))

               ((member fn '(loaded? loaded loaded-list loaded-list?))
                (prload "loaded" loaded))

               ((member fn '(loading? loading loading-list
                                     loading-list? load-list?))
                (prload "loading" loading))

					((eq? fn 'help)
					 (display
					  (string-append
						"This load routine suppresses loading a file more than once "
						                "by default.\n"
						"The load procedure responds to a number of symbols:\n\n"
						"help           this help message\n"
						"loaded?        prints the list of loaded files\n"
						"loading?       prints the list of files that are currently "
                                  "loading\n"
						"flush          flushes the list of loaded files\n"
						"warn-loaded    sets the flag that makes it warn about "
						                "attempts to\n"
						"               load a file more that once\n"
						"!warn-loaded   turns off the warning\n"
						"always-load    (load ...) will alway load the files "
						                "indicated\n"
						"!alway-load    (load ...) only loads files which have not been "
                                  "loaded\n"
						"print-loaded   the list of loaded files is printed at each call "
                                  "to (load ...)\n"
						"!print-loaded  turns off the previous flag\n"
						"print-loading  the list of loading files is printed at each call "
                                  "to (load ...)\n"
						"!print-loading turns off the previous flag\n"
						"\n")))
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

;; This is a list of data added by a (definition-comment ...) clause associated with a
;; (define...)
(define definition-comments '())


(definition-comment 'load-model
  "This first loads the basic files which build the modelling framework"
  "and the supporting function libraries.  It then loads the files which"
  "are specific to a particular model---submodels.scm, kernel.scm and"
  "mode-configuration.scm.  'submodels.scm' defines the object classes"
  "which are to be used, 'kernel.scm' contains the code which manages the"
  "execution loop, and 'model-configuration.scm' actually instantiates"
  "the entities in the model, configures them, and calls the kernel to"
  "begin the run.")

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
;;; comment-end: "-;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
