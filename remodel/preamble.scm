; -*- mode: scheme; -*-

;- NOTES

"This file defines a number of basic and essential variables (though
a lot of them ought to be constants).  

Some basic functions are defined to do things like print things,
variables are defined to cache system supplied functions which are overridden.

It also defines a 'smart-load' which prevents accidentally loading a file more than once. 
By default, files are loaded if they have never been loaded, or if their modification time
is more recent than the file modification time of the file on the disk.
"

;; (define-syntax define*
;;   (lambda (X)
;; 	 (syntax-case X (!rest !optional)
;; 		((define* var) (syntax (define var (void))))
;; 		((define* var val) (syntax (define var val)))
;; 		((define* (proc) body ...) (syntax (define (proc) body ...)))
;; 		((define* (proc !rest arg) body ...) (syntax (define proc (lambda arg body ...))))
;; 		((define* (proc !optional arg) body ...)
;; 		 (syntax
;; 		  (define proc (lambda arg (let ((arg (if (null? arg) #f (car arg))))
;; 											  body ...))))
;; 		  )
;; 		 )
;; 		((define* (proc params ... !rest arg) body ...) (syntax (define proc (lambda arg body ...))))
;; 		((define* (proc !optional arg !rest arg*) body ...)
;; 		 (syntax
;; 		  (define proc (lambda arg (lambda arg (let ((arg (if (null? arg) #f (car arg)))

;; 																	)
;; 															  body ...))))
;; 		  )
;; 		 )

;; 	)))

;- Include files: remodel-framework

(include "remodel-framework")

;- Record warnings in a file called "warning.log" 

;-- variables which represent uninitialised objects or classes ... NEVER ASSIGN TO THEM!

;; These represent "uninitialised" things

(define uninitialised (lambda args (abort 'uninitialised-function)))
(define <uninitialised> '<uninitialised> )

(define <nameless> '<nameless> )

(define warn <uninitialised>)

(define (warning-log #!rest args)
  (if (eqv? warn <uninitialised>)
		(set! warn (open-output-file "warning.log"))
		(display "Unable to open warning log\n"))
  (if (not (output-port? warn))
		(set! warn (current-output-port)))
  
  (display args warn)
  (newline warn))


;- Variables

;-- Scheme interpreter identification, etc
(define *scheme-version* "gambit")
(define *current-interpreter* (string->symbol *scheme-version*))

(setenv "SCHEME_RC_LOADED" *scheme-version*)
;(setenv "SCHEME_LIBRARY_PATH" "/usr/share/slib/")

;-- boolean equivalents, true and false
(define true #t)
(define false #f)

;-- command line
(define argv command-line)

;-- loading flag
(define currently-loading #f)

;-- stdin stdout and stderr, not really used much
;; for C junkies :->
(define stdin 'use:current-input-port)
(define stdout 'use:current-output-port)
(define stderr 'use:current-error-port)

;-- Various registers 

(define overdue-loans '())
(define definition-comments '()) ;; collects comments from the code
(define %%%-time-register-%%% '()) ;; this records all the calls defined with define%, model-method% or model-body%


;-- Cache builtin functions

;--- display and newline
(define original-display display)
(define fake-display (lambda x (void)))

(define original-newline newline)
(define fake-newline (lambda x (void)))

;--- *primitive-load* is used by smart-load (below)
(define *primitive-load* load)


;(define original-ednl ednl)
;(define fake-ednl (lambda x (void)))

;(set! display fake-display)
;(set! newline fake-newline)
;(set! ednl fake-ednl)

;-- SLIB -- currently assumed to be loaded before you load Remodel, loading printf and charplot

"Here be dragons, if you want to be able to run without SLIB, go with
my blessing, but see the comments at the beginning of the file
'If-there-are-errors' first.  While SLIB is not strictly necessary,
I've found it worth having loaded.   Remember that there is no warranty
to void, so do whatever you like :-)

I'd rather work on Remodel than muck about trying to get slib to load
correctly for every scheme program I use it in, so if you fix this to
gracefully continue with slib loaded during the interpreter's startup,
*and* with it loading later you'll achieve minor fame and my gratitude.
"

;; SLIB needs to be loaded before this point.
(require 'printf)
(require 'charplot)



;-- Scheme interpreter identification and information

;;?(if (not (getenv "SCHEME_LIBRARY_PATH")) (setenv "SCHEME_LIBRARY_PATH" "/usr/share/slib/"))
;;?(setenv "SCHEME_LIBRARY_PATH" "/usr/share/slib/")

(if (not (getenv "GAMBIT_IMPLEMENTATION_PATH")) (setenv "GAMBIT_IMPLEMENTATION_PATH" "/usr/share/gambit/"))
;;?(if (not (getenv "SLIB_IMPLEMENTATION_PATH")) (setenv "SLIB_IMPLEMENTATION_PATH" "/usr/share/slib/"))


;- smart-load -- suppresses redundant loads in the development cycle

" Smart load prevent us from accidentally loading a file twice.  This could pose problems if the
  file *needs* to be loaded more than once, but this can be dealt with by a forced load.


  These provide a shotgun equality test which works with the classes and instances.
  My goal is to make difficult bugs in subsequent work less likely.


  smart-load recognises when a file has been loaded and avoids loading it twice.
"

;; Like list-set! but for a-lists
;; This is a mutator -- the list needs to exist first for it to work, though.

(define (sl-assoc key alist)
  (if alist
		(let ((r (assoc key alist)))
		  (if (pair? r) (cdr r) r))
		alist))
		  

(define sl-environment
  (let* ((data
			 `(
				(oload . ,load)
				(verbose . #f)
				(always-print-loading . #f)
				(always-print-loaded . #f)
				(always-load . #f)
				(warn-loaded . #t)
				(loaded . ,(list))
				(loading . ,(list))
				(EQ? . ,eqv?)
				(MEMB . ,memq)
				(ASSQ . ,sl-assoc)
				)))
	 (let ((assoc-set! (lambda (alist key val)
								(let loop ((l alist))
								  (if (null? l) 
										(append alist (cons (cons key val) '()))
										(if (equal? (caar l) key)
											 (set-cdr! (car l) val)
											 (loop (cdr l))))))))
		(lambda args
		  (dnl* "sl environment called with" args)
		  (cond
			((null? args)
			 (list-copy data)) ;; you cannot modify the list except through explicit calls

			((and (member (car args) (map car data)) (null? (cdr args)))
			 (cdr (assoc (car args) data)))

			((and (member (car args) (map car data)) (pair? (cdr args)))
			 (assoc-set! data (car args) (cadr args)))
			((eqv? (car args) 'flush)
			 (set! data '()))
			((eqv? (car args) 'set!) ;; Used like (sl-environment 'SET-ALIST!
			 (set! data (cdr args)))       ;;             '((verbose . #f) ...)
			(else (failure "bad call to sl-environment"))
			)
		  ))))


(define smart-load-expression
  '(lambda (fn . args)
	  (let* ((slenv sl-environment)
				(oload (slenv 'oload))
				(verbose (slenv 'verbose))
				(always-print-loading (slenv 'always-print-loading))
				(always-print-loaded (slenv 'always-print-loaded))
				(always-load (slenv 'always-load))
				(warn-loaded (slenv 'warn-loaded))
				(loaded (slenv 'loaded))
				(loading (slenv 'loading))
				(EQ? (slenv 'EQ?))
				(MEMB (slenv 'MEMB))
				(prload (lambda (x y)
							 (display (string-append "\n[" x " "))
							 (display y)(display "]\n")))
				)
		 (dnl* "Calling smart-load with" fn "and" args)
		 (cond
		  ((eq? fn 'reload!)
			(set! smart-load (eval smart-load-expression))
			)
		  ;; We are trying to load file
		  ((and (string? fn) (file-exists? fn))
			(let* ((fmodt (time->seconds (file-info-last-modification-time (file-info fn))))
					 (lfmodt (let ((lfmodt?  (sl-assoc fn loaded)))
								  (if (and lfmodt? (not (null? lfmodt))) (cdr lfmodt?) 0)))
					 )
			  (dnl "File:" fn "last saved at" fmodt
			  (if (and (sl-assoc fn loaded) (<= fmodt (cdr lfmodt))) ;; if the file is already in the system....
					(begin
					  (if warn-loaded
							(dnl "[" fn " is already loaded]"))
					  
					  (cond
						;; ((and (string? fn) (not (null? args)))
						;;  (dnl "forced loading of " fn)
						;;  (oload fn) ;; forces a load, doesn't record it
						;;  )
						
						;; Load the file irrespective of whether it has been loaded before
						((and (MEMB fn '(force force-load)) (not (null? args)))
						 (for-each
						  (lambda (f)
							 (oload f)
							 (if verbose
								  (begin (display f) (newline))))
						  args)
						 )
						
						((string? fn)
						 (set! loading (cons (cons fn fmodt) loading))
						 (if always-print-loading (prload "loading" loading))
						 (if (or always-load (not (sl-assoc fn loaded)) (< lfmodt fmodt))
							  (begin
								 (oload fn)
								 (if verbose
									  (begin (display fn) (newline)))
								 
								 (set! loading (cdr loading))
								 (set! loaded (cons (cons fn loaded) loaded))
								 (if always-print-loaded (prload "loaded" loaded))))
						 )
						(#t (error "bad argument to (load)" fn))
						)
					  )
					)
			  )
			))
		  ;; The next two substantially change the behaviour ------
		  ((EQ? fn 'revert!)
			(display "smart-load: Reverted to original load\n")
			(set! load oload))

		  ((or (EQ? fn 'flush!)(EQ? fn 'flush))
			(display "smart-load: Flushed load list\n   ")
			(display loaded)(newline)
			(set! loaded '())
			)

		  ;; the next three return state information ---------------
		  ((EQ? fn 'loaded-list)
			(map car (reverse loaded)))

		  ((EQ? fn 'loading-list)
			loading)

		  ((MEMB fn '(now-loading now-loading? current-file current-file?))
			;;(prload "loaded" loaded)
			(if (pair? loading) (car loading) #f)
			)


		  ((MEMB fn '(prloaded? loaded loaded-list loaded-list?))
			(prload "loaded" loaded))

		  ((MEMB fn '(prloading? loaded loaded-list loaded-list?))
			(prload "loading" loading)
			)

		  ;; Set/reset flags --------------------------------------
		  ((EQ? fn 'warn-loaded)
			(display "smart-load: Warning when files are already loaded\n")
			(set! warn-loaded #t))

		  ((EQ? fn '!warn-loaded)
			(display "smart-load: Not warning when files are already loaded\n")
			(set! warn-loaded #f))

		  ;;--

		  ((EQ? fn 'always-load)
			(display "smart-load: Always loading.\n")
			(set! always-load #t))

		  ((EQ? fn '!always-load)
			(display "smart-load: Only loading new files.\n")
			(set! always-load #f))

		  ;;--

		  ((EQ? fn 'print-loaded)
			(display "smart-load: Printing loaded files at each load.\n")
			(set! always-print-loaded #t))

		  ((EQ? fn '!print-loaded)
			(display "smart-load: Not printing loaded files a each load.\n")
			(set! always-print-loaded #f))

		  ;;--

		  ((EQ? fn 'print-loading)
			(display "smart-load: Printing currently loading files at each load.\n")
			(set! always-print-loading #t))

		  ((EQ? fn 'assume-loaded!)
			(let ((alf* (cdr args)))
			  (for-each
				(lambda (fn)
				  (let ((fmodt (if (and (string? fn) (file-exists? fn))
										 (time->seconds (file-info-last-modification-time (file-info fn)))
										 #f)))
					 (if fmodt (begin
									 ;; (oload fn) ;; if this were uncommented, it would force a load
									 (set! loading (cons (cons fn fmodt) loading)))))
				  )
				alf*)
			  ))
		  
		  ((EQ? fn '!print-loading)
			(display "smart-load: Not printing currently loading files at each load.\n")
			(set! always-print-loading #f))
		  ;;--

		  ((EQ? fn 'verbose)
			(display "smart-load: Printing the filename after loading it.\n")
			(set! verbose #t))

		  ((EQ? fn '!verbose)
			(display "smart-load: Don't print filename after loading it.\n")
			(set! verbose #f))

		  ((EQ? fn 'reload-smart-load!)
			(display "smart-load: Don't print filename after loading it.\n")
			(set! verbose #f))


		  ;; Print help -----------------------------------------------
		  ((EQ? fn 'help)
			(display
			 (string-append
			  "This load routine suppresses loading a file more than once "
			  "by default.\n"
			  "The load procedure responds to a number of symbols:\n\n"
			  "help           this help message\n"
			  "verbose        always print the filename after it has loaded\n"
			  "verbose!       do not print the filename after it has loaded\n"
			  "revert!        revert to the original load procedure\n"
			  "flush!         flushes the list of loaded files\n"
			  "assume-loaded! takes a filename which is assumed to have been loaded\n"
			  "list-loading   returns the list of active files\n"
			  "now-loading?   returns the file currently being loaded\n"
			  "loading\n"
			  "list-loaded    returns the list of files already loaded\n"
			  "loading?       prints the list of files that are currently "
			  "loading\n"
			  "loaded?        prints the list of loaded files\n"
			  "warn-loaded    sets the flag that makes it warn about "
			  "attempts to\n"
			  "               load a file more that once\n"
			  "!warn-loaded   turns off the warning\n"
			  "always-load    (load...) will alway load the files "
			  "indicated\n"
			  "!alway-load    (load...) only loads files which have not been "
			  "loaded\n"
			  "print-loading  the list of loaded files is printed at each call "
			  "to (load...)\n"
			  "!print-loaded  turns off the previous flag\n"
			  "print-loading  the list of loading files is printed at each call "
			  "to (load...)\n"
			  "!print-loading turns off the previous flag\n"
			  "reload-smart-load!    to update the smart-load function when bug-fixing\n"
			  "\n")))
		  )

		 ;; Set new values in the environment 
		 (slenv 'oload oload)
		 (slenv 'verbose verbose)
		 (slenv 'always-print-loading always-print-loading)
		 (slenv 'always-print-loaded  always-print-loaded )
		 (slenv 'always-load always-load)
		 (slenv 'warn-loaded warn-loaded)
		 (slenv 'loaded loaded)
		 (slenv 'loading loading)
		 (slenv 'EQ? EQ?)
		 (slenv 'MEMB MEMB)
		 ))
  ) ;; end of the definition of the expression for smart-load


(define smart-load
  (let ()
	 (eval smart-load-expression)
	 ))

;(define load smart-load) ;; supplant the standard load routine

;(load 'assume-loaded! "preamble.scm")


;; Makes gambit work with slib

;(include "/usr/lib/slib/gambit.init")

;--    Included files

;(require 'rev2-procedures)
;(require 'rev3-procedures)
;(require 'pretty-print)
;(require 'common-list-functions)

;(require 'repl)
;(require 'macro)
;(require 'macros-that-work)
;(repl:top-level macro:eval)

;(require 'stdio)
;(require 'sort)
;(require 'strcase)
;(require 'string-search)
;(require 'str)
;(require 'with-file)
;(require 'line-i/o)
;(require 'random)

;; Utility functions -- some may be overwritten by functions in utils.scm

;; to get shell environment variables
(define getenv (let ((ge getenv))
					  (lambda (var . rest)
						 (ge var))))

;; Construct a symbol out of a number of components
(define (construct-symbol sym #!rest tagels)
  (if (string? sym) (set! sym (string->symbol sym)))
  (set! tagels (map (lambda (x) (if (string? x) (string->symbol x) x)) tagels))
  
  (string->symbol
	(apply string-append
			 (map object->string
					(cons sym
							(let loop ((l '()) (t tagels))
							  (if (null? t)
									(reverse l)
									(loop (cons (car t)
													(cons '- l))
											(cdr t)))))))))


(define (maybe-expand-path path . dir)
  (if (string-index path #\~)
		(path-expand path)
		path))


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

(define ednl dnl)
(define ednl* dnl*)



;; This allows me to put in comments which I can "articulate" if needed.
(define (Comment . args) #!void)

;; set the value associated with key in a-list
;; (assoc-set! list key value)

(define (list-copy l) ;; Remember, list-copy is the canonical list duplication routine
  (if (not (pair? l))
		l
		(cons (list-copy (car l)) (list-copy (cdr l)))))


;---- (filter selector lst) -- returns a list of those elements which pass the selector

(define (filter selector lst)
  (if (pair? lst)
		(if (selector (car lst))
			 (cons (car lst) (filter selector (cdr lst)))
			 (filter selector (cdr lst)))
		lst))

;---- (!filter selector lst) -- returns a list of those elements which fail the selector

(define (!filter selector lst) 
  (filter (lambda x (not (apply selector x))) lst))

;; This is a list of data added by a (definition-comment ...) clause associated with a
;; (define...)
(define definition-comments '())

(define (definition-comment tag . comments)
  (set! definition-comments (append definition-comments (list (cons tag comments)))))

(define (describe item)
  (let* ((r (assq item definition-comments))
			(s (assq (cond
						 ((string? item) (string->symbol item))
						 ((symbol? item) (symbol->string item))
						 (#t '()))
						definition-comments)))
	 (cond
	  ((and r s)
		(display item)(display ":\n")
		(pp r)
		(pp s)
		(newline))
	  (r (display item)(display ":\n")
		  (pp r)
		  (newline))
	  (s (display item)(display ":\n")
		  (pp s)
		  (newline))
	  (#t (display "The indicated object, ")
			(write item)
			(display "does not have a comment recorded by (definition-comment ...)\n")))
	 ))

(define (symbol<? s t)
  (if (not (and (symbol? s) (symbol? t)))
		(error "Passed a non symbol to symbol<?" s t)
		(string<? (symbol->string s) (symbol->string t))))

(define (call-with-input-port p thunk)
  (let ((pp (current-input-port)))
	 (current-input-port p)
	 (thunk)
	 (current-input-port pp)))

(define (call-with-output-port p thunk)
  (let ((pp (current-output-port)))
	 (current-output-port p)
	 (thunk)
	 (current-output-port pp)))

(define (call-with-error-port p thunk)
  (let ((pp (current-error-port)))
	 (current-error-port p)
	 (thunk)
	 (current-error-port pp)))


;; ************* Examples using the next few functions follow after their definition. *************

;; The following functions expect the command to be an array of strings.
;; They work like any other port, really.

(define current-input-port
  (let ((cp current-input-port))
	 (lambda x
		(cond
		 ((> (length x) 1) 'too-many-arguments)
		 ((null? x) (cp))
		 (#t (set-current-input-port (car x)))))))

(define current-output-port
  (let ((cp current-output-port))
	 (lambda x
		(cond
		 ((> (length x) 1) 'too-many-arguments)
		 ((null? x) (cp))
		 (#t (set-current-output-port (car x)))))))


;#; ;; This is slower the subsequent version never leaves the interpreter's code
;#; (define (load-flat-list-from-file filename)
;#;   (reverse
;#;    (with-input-from-file filename
;#;      (lambda () 
;#;        (let loop ((new-entry (read)) (data '()))
;#; 	 (if (not (eof-object? new-entry))
;#; 	     (begin
;#; 	       (loop (read) (cons new-entry data)))
;#; 	     data))))))


(define (capture-string-from thunk)
  (let ((sp (open-output-string)))
	 (call-with-output-port sp thunk)
	 (let ((result (get-output-string sp)))
		(close-port sp) ;; **** chibi needs a close-port
		result)))

(define (load-lines-from-port f)
  (let loop ((line (read-line f))
				 (file '())
				 )
	 (if (eof-object? line)
		  file
		  (loop (read-line f) (append file (list line)))
		  )))


(define (capture-lines-from thunk)
  (let ((sp (open-output-string)))
	 (call-with-output-port sp thunk)
	 (let ((result (load-lines-from-port sp)))
		(close-port sp) ;; **** chibi needs a close-port
		result)))

(define (load-list-from-port f)
  (let loop ((line (read-line f))
				 (port '())
				 )
	 (if (not (eof-object? line))
		  (loop (read-line f) (append port (list (collapsing-strtok line " \t"))))
		  port)))


(define (capture-list-from thunk)
  (let ((sp (open-output-string)))
	 (call-with-output-port sp thunk)
	 (let ((result (load-list-from-port sp)))
		(close-port sp) ;; **** chibi needs a close-port
		result)))

(define (load-flat-list-from-port f)
  (let loop ((line (read-line f))
				 (port '())
				 )
	 (if (not (eof-object? line))
		  (loop (read-line f) (append port (collapsing-strtok line " \t")))
		  port)))

(define (capture-flat-list-from thunk)
  (let ((sp (open-output-string)))
	 (call-with-output-port sp thunk)
	 (let ((result (load-flat-list-from-port sp)))
		(close-port sp) ;; **** chibi needs a close-port
		result)))

; Hard to map to gambit
(define (load-list-from-pipe cmd-array . extras)
  (let ((cmd-array (if (string? cmd-array) (collapsing-strtok cmd-array " \t") cmd-array)))
	 (let* ((f (if (null? extras)
						(begin
						  (open-input-pipe cmd-array))
						(begin
						  (apply (lambda (x) (open-input-pipe cmd-array x) extras)))))
			  
			  (v (load-list-from-port f)))
		(close-port f)
		v)
	 ))

;; (define size+filename-list (load-list-from-pipe (list "ls" "-1s")))



;;(define (load-flat-list-from-pipe command . args)
;;  (let* ((f (apply open-input-pipe (cons command args)))
;;        (v (load-flat-list-from-port f)))
;;    (close-port f)
;;    v
;;    )
;;  )

(define (load-flat-list-from-pipe cmd-array . args)
  (let ((cmd-array (if (string? cmd-array) (collapsing-strtok cmd-array " \t") cmd-array)))
	 (let* ((f (apply (lambda () (open-input-pipe cmd-array)) args))
			  (v (load-flat-list-from-port f)))
		(close-port f)
		v
		)
	 ))

;;(define (load-list-from-command cmdline)
;;  (apply load-list-from-pipe (split-commandline cmdline)))

;;(define (load-flat-list-from-command cmdline)
;;  (apply load-flat-list-from-pipe (split-commandline cmdline)))


(define (load-list-from-file filename)
  (let ((f (open-input-file (maybe-expand-path filename))))
	 (let loop ((line (read-line f))
					(file '())
					)
		(if (not (eof-object? line))
			 (loop (read-line f) (append file (list line)))
			 file)))
  )

;; (define (load-list-from-file filename)
;;   (let* ((f (open-input-file (maybe-expand-path filename)))
;; 			(v (load-list-from-port f)))
;; 	 (close-port f)
;; 	 v))





(define (load-flat-list-from-file filename)
  (let ((f (open-input-file (maybe-expand-path filename))))
	 (let loop ((line (read-line f))
					(file '())
					)
		(if (not (eof-object? line))
			 (loop (read-line f) (append file (collapsing-strtok line " \t")))
			 file))))


(define (load-lines-from-file filename)
  (let ((f (open-input-file (maybe-expand-path filename))))
	 (let loop ((line (read-line f))
					(file '())
					)
		(if (eof-object? line)
			 file
			 (loop (read-line f) (append file (list line)))
			 ))))

(define (load-lists-from-file filename separator)
  (map (lambda (x) (strtok x separator)) (load-lines-from-file filename)))


;  (define (load-list-from-file filename)
;    (with-input-from-pipe (string-append "echo '('; cat " filename "; echo ')'")
;				  (lambda () 
;				    (read))))

;(define (read-array-from-file fname . bounds)
;  (let ((f (open-file (maybe-expand-path fname) "r"))
;		  (a (apply make-uniform-array (append (list 0.0) bounds)))
;		  )
;	 (serial-array-map! a (lambda (x) (read f)) a)
;	 (close-port f)
;	 a))
;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
