; -*- mode: scheme; -*-
;-  Identification and Changes

;--
;	remodel-load.scm -- Written by Randall Gray 
;	Initial coding: 
;		Date: 2016.11.08
;		Location: zero.grayrabble.org:/home/randall/Thesis/Example-Model/model/remodel-load.scm
;
;	History:
;

;-  Copyright 

;
;   (C) 2016 Randall Gray
;   All rights reserved
;

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(define --load-- load) ;; just so we remember
(define smart-load
  (let* ((oload load)
         (always-print-loading #f)
         (always-print-loaded #f)
         (always-load #f)
         (warn-loaded #t)
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
                  (dnl "[" fn " is already loaded]"))
              (cond
               ((member fn (list "loaded?"))
                (for-each dnl (reverse loaded))
                )
               ;; ((and (string? fn) (not (null? args)))
               ;;  (dnl "forced loading of " fn)
               ;;  (oload fn) ;; forces a load, doesn't record it
               ;;  )
					((and (member fn '(force force-load)) (not (null? args)))
					 (for-each oload args))

               ((eqv? fn 'revert)
					 (display "Reverted to original load\n   ")
					 (set! load oload)
					 )

               ((eqv? fn 'flush)
					 (display "Flushed load list\n   ")
					 (display loaded)(newline)
                (set! loaded '())
					 )

               ((eqv? fn 'warn-loaded)
					 (display "Warning when files are already loaded\n")
                (set! warn-loaded #t))

               ((eqv? fn '!warn-loaded)
					 (display "Not warning when files are already loaded\n")
                (set! warn-loaded #f))

               ((eqv? fn 'always-load)
					 (display "Always loading.\n")
                (set! always-load #t))

               ((eqv? fn '!always-load)
					 (display "Only loading new files.\n")
                (set! always-load #f))

               ((eqv? fn 'print-loaded)
					 (display "Printing loaded files at each load.\n")
                (set! always-print-loaded #t))

               ((eqv? fn '!print-loaded)
					 (display "Not printing loaded files at each load.\n")
                (set! always-print-loaded #f))

               ((eqv? fn 'print-loading!)
					 (display "Printing currently loading files at each load.\n")
                (set! always-print-loading #t))

               ((eqv? fn '!print-loading!)
					 (display "Not printing currently loading files at each load.\n")
                (set! always-print-loading #f))

               ((member fn '(loaded? loaded loaded-list loaded-list?))
                (prload "loaded" loaded))

               ((member fn '(loading? loading loading-list
                                     loading-list? load-list?))
                (prload "loading" loading))

					((eqv? fn 'help)
					 (display
					  (string-append
						"This load routine suppresses loading a file more than once "
						                "by default.\n"
						"The load procedure responds to a number of symbols:\n\n"
						"help           this help message\n"
						"revert!        reverts back to original load function\n"
						"loaded?        prints the list of loaded files\n"
						"current?       prints the file that is currently loading\n"
						"loading?       prints the list of files that are currently "
                                  "loading\n"
						"flush!         flushes the list of loaded files\n"
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


(define load smart-load)


;-  The End 


;;; Local Variables: 
;;; comment-end: " ;;;" ;;;
;;; comment-start: ";;; " ;;;
;;; mode: scheme ;;;
;;; outline-regexp: ";-+" ;;;
;;; comment-column: 0 ;;;
;;; End:
