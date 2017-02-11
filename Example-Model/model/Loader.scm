(define *scheme-version* "gambit")
(define *current-interpreter* (string->symbol *scheme-version*))

(define load-level #f)
(define currently-loading #f)
(define true #t)
(define false #f)

;; These may need changing from time to time!

(if (not (getenv "SCHEME_LIBRARY_PATH")) (setenv "SCHEME_LIBRARY_PATH" "/usr/share/slib/"))
(setenv "SCHEME_LIBRARY_PATH" "/usr/share/slib/")

(if (not (getenv "GAMBIT_IMPLEMENTATION_PATH")) (setenv "GAMBIT_IMPLEMENTATION_PATH" "/usr/share/gambit/"))
(if (not (getenv "SLIB_IMPLEMENTATION_PATH")) (setenv "SLIB_IMPLEMENTATION_PATH" "/usr/share/slib/"))

(define *primitive-load* load)


;; Makes gambit work with slib
(load "/usr/lib/slib/gambit.init"))

;--    Included files

(require 'rev2-procedures)
;(require 'rev3-procedures)
(require 'pretty-print)
(require 'common-list-functions)

;(require 'repl)
;(require 'macro)
;(require 'macros-that-work)
;(repl:top-level macro:eval)

(require 'stdio)
(require 'sort)
;(require 'strcase)
(require 'string-search)
;(require 'str)
;(require 'with-file)
;(require 'line-i/o)
;(require 'random)


(setenv "SCHEME_RC_LOADED" *scheme-version*)
(setenv "SCHEME_LIBRARY_PATH" "/usr/share/slib/")

 ;; for C junkies :->
 (define stdin 'use:current-input-port)
 (define stdout 'use:current-output-port)
 (define stderr 'use:current-error-port)


 (define (maybe-expand-path path . dir)
   (if (string-index path #\~)
       (path-expand path)
       path))


 ;; set the value associated with key in a-list
 ;; (assoc-set! list key value)


