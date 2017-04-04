;-  Identification and Changes

;--
;	postscript.scm -- Written by Randall Gray 

; deprecated, see ~/scm/postscript.scm

;-  Discussion 

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

;;(load "utils.scm")
;;(load "constants.scm")

(define (make-it-a-string s) 
  (or (and (string? s) s) (and (char? s) (make-string 1 s)) (object->string s)))

(define (ps-andf . args)
  (if (null? args)
		#t
		(and (car args) (apply ps-andf (cdr args)))))

(define (gmap l x)
  (if (pair? x)
		(map l  x)
		(l x)))

(define (rescale s x)
  (if (pair? s)
		(gmap (lambda (t) (map * s t)) x)
		(gmap (lambda (t) (* s t)) x))  )

(define (inches->points x)
  (rescale 72.0 x))

(define (points->inches x)
  (rescale (/ 1.0 72.0) x))


(define (points->mm x)
  (rescale (/ 1.0 2.83464646464646464646) x))

(define (mm->points x)
  (rescale 2.83464646464646464646 x))

(define (mm->inches x)
  (rescale (/ 1.0 24.5) x))

(define (inches->mm x)
  (rescale 24.5 x))

(define pagesize '(595 841)) ;; in 1/72 inches...

(define (scaled-by-x x pagesize)
  (list x (* x (/ (cadr pagesize) (car pagesize)))))

(define (scaled-by-y y pagesize)
  (list y (* y (/ (car pagesize) (cadr pagesize)))))

;(define target-size (inches->points (scaled-by-x 6.8 pagesize)))

(define (make-list-matrix . rows)
  (if (apply = (map length rows))
		(map list-copy rows)
		#f))

(define (ps-list-ref*? lst ix)
  (cond
	((and (number? ix) (<= 0 ix) (< ix (length lst)))
	 #t)
	((and (pair? lst) (number? (car ix)) (<= 0 (car ix)) (< (car ix) (length lst)))
	 (ps-list-ref*? (list-ref lst (car ix)) (cdr ix)))
	((and (null? lst) (not (null? ix))) #f)
	((and (null? ix) (not (null? lst))) #t)
	(#t #t)))

;; (list-ref* '((a b c d) (e f g h i j) (k l m)) '(1 0)) => e
;;; (define (list-ref* lst ix)
;;;   (cond
;;; 	((number? ix)
;;; 	 (list-ref lst ix))
;;; 	((= (length ix) 1)
;;; 	 (list-ref lst (car ix)))
;;; 	(else
;;; 	 ;;		(list-ref* (map (lambda (x) (list-ref x (car ix))) lst) (cdr ix))
;;; 	 (list-ref (map (lambda (x) (list-ref* x (cdr ix))) lst) (car ix))
;;; 	 )))

(define (ps-list-ref* lst ix)
  (cond
	((and (list? ix) (= 1 (length ix))) (list-ref lst (car ix)))
	((and (number? ix) (<= 0 ix) (< ix (length lst)))
	 (list-ref lst ix))
	((and (pair? lst) (number? (car ix)) (<= 0 (car ix)) (< (car ix) (length lst)))
	 (ps-list-ref* (list-ref lst (car ix)) (cdr ix)))
	((and (null? lst) (not (null? ix))) (error "Index list ran off the end of the list" ix))
	((and (null? ix) (not (null? lst))) lst)
	(#t (error "Should never get here, bad arguments" lst ix))))


;; (define p '((a b c d) (e f g h i j) (k l m)))
;; (list-set* p '(1 0) '(tst))
;; p  => ((a b c d) ((tst) f g h i j) (k l m))
(define (ps-list-set* lst ix vv)
  (cond
	((number? ix)
	 (list-set! lst ix vv))
	((= (length ix) 1)
	 (dnl "unit list")
	 (list-set! lst (car ix) vv))
	(else
	 (let ((tv (ps-list-ref* lst ix)))
		(if (atom? tv)
			 ;; indices fully resolve an element
			 (let* ((short-ix (reverse (cdr (reverse ix))))
					  (tv (ps-list-ref* lst short-ix)))
				(list-set! tv (car (reverse ix)) vv))
			 (if (= (length tv) (length vv))
				  ;; it's ok, do it
				  (ps-list-set* (map (lambda (x) (ps-list-ref* x (car ix)) lst) (cdr ix)) vv)
				  (abort "The value list does not have the indicated number of elements"))))
	 )))

(define (ps-make-list* . dims)
  (let ((defval 0))
	 (if (and (pair? dims) (pair? (car dims))) 
		  (begin
			 (if (pair? (cdr dims))
				  (set! defval (cadr dims)))

			  (set! dims (car dims))))

		  (if (null? (cdr dims))
				(make-list (car dims) defval)
				(map (lambda (x) (ps-make-list* (cdr dims) defval)) (make-list (car dims) )))))

(define (ps-simple-list? l)	
  (apply ps-andf (map atom? l)))


(define (ps-list-matrix? m)
  (and (pair? m) 
		 (apply ps-andf (map ps-simple-list? m)) 
		 (apply = (map length m))))

(define (transpose-list-matrix A)
  (if (ps-list-matrix? A) 
		(let* ((dima (list (length A) (length (car A))))
				 (B (ps-make-list* (list (cadr dima) (car dima)) 0)))
		  (for-each
			(lambda (i) 
			  (for-each
				(lambda (j)
				  (ps-list-set* B (list j i) (ps-list-ref* A (list i j))))
				(seq (cadr dima))))
			(seq (car dima)))
		  B)
		#f))

(define (*-matrix a b)
  (cond
	((and (number? a) (number? b)) (* a b))
	((and (number? b) (ps-list-matrix? a)) (map (lambda (x) (map (lambda (y) (* b y)) x)) a))
	((and (number? a) (ps-list-matrix? b)) (map (lambda (x) (map (lambda (y) (* a y)) x)) b))
	(else 
	 (let* ((dima (list (length a) (length (car a))))
			  (dimb (list (length b) (length (car b))))
			  )
		(if (not (= (cadr dima) (car dimb)))
			 (abort "incompatible matrices")
			 (map (lambda (x) (map (lambda (y) (apply + (map * x y))) (transpose-list-matrix b))) a))))))



;;; ;; from http://www.larcenists.org/Twobit/benchmarksAbout.html -- http://www.larcenists.org/Twobit/src/pnpoly.scm
;;; ;; With changes 
;;; (define (pt-in-poly2 xp yp x y) 
;;;   (let loop ((c #f) 
;;; 				 (i (- (length xp) 1)) 
;;; 				 (j 0))
;;;     (if (< i 0)
;;;       c
;;;       (if (or (and (or (> (list-ref yp i) y)
;;;                        (>= y (list-ref yp j)))
;;;                    (or (> (list-ref yp j) y)
;;;                        (>= y (list-ref yp i))))
;;;               (>= x
;;;                        (+ (list-ref xp i)
;;;                                (/ (*
;;;                                         (- (list-ref xp j)
;;;                                                 (list-ref xp i))
;;;                                         (- y (list-ref yp i)))
;;;                                        (- (list-ref yp j)
;;;                                                (list-ref yp i))))))
;;;         (loop c (- i 1) i)
;;;         (loop (not c) (- i 1) i)))))


;;; (define (point-in-polygon p poly)
;;;   (if (or? (null? p) (null? poly) (< (length poly) 3)) 
;;; 		#f
		
;;; 		(let ((ptail (car (reverse poly))))
;;; 		  (if (not (equal? (car poly) ptail))
;;; 				(set! poly (append poly (list (car poly)))))))

;;;   (pt-in-poly2 (map car poly) (map cadr poly) (car p) (cadr p)))




(define (rotate-point-list theta pointlist)
  (map (lambda (x) (let ((m (make-list-matrix `(,(cos theta) ,(- (sin theta))) `(,(sin theta) ,(cos theta)))))
							;;???(*-matrix-vector m x)
							(*-matrix m x)
							))
		 pointlist))

(define (adjust operator deviant pointlist)
  (if (and (pair? pointlist) (pair? (car pointlist)))
		(if (pair? deviant)
			 (map (lambda (pt) (map (lambda (s o) (operator s o )) deviant pt)) pointlist)
			 (map (lambda (pt) 
					  (map (lambda (o) 
								(operator deviant o)) pt)) pointlist)
			 )
		(if (pair? deviant)
			 (map operator deviant pointlist)
			 (map (lambda (o) (operator deviant o)) pointlist))))


(define (scale-pointlist k pointlist)
  (adjust * k pointlist))

(define (translate-pointlist offset pointlist)
  (adjust + offset pointlist))

(define (translate-pointlist* n offset lstlst) ;; This should be generalised....
  (if (zero? n)
		(adjust + offset lstlst)
		(map (lambda (lst) (translate-pointlist* (- n 1) offset lst)) lstlst)))


(define (fold-A-series-paper aN)
  (list (/ (cadr aN) 2.0) (car aN)))


(define a4 (list (mm->points 210) (mm->points 297)))
(define a5 (fold-A-series-paper a4))
(define a6 (fold-A-series-paper a5))

;; == 595.276 841.89 points
;; == 8.26772 x 11.6929 inches

(define sl '())
(define isl '())

(define (list-tabulate len proc)
  (do ((i (- len 1) (- i 1))
       (ans '() (cons (proc i) ans)))
      ((< i 0) ans)))

(define (make-circle location radius-pts divisions)
  (let ((tau (* (acos -1))))
	 (translate-pointlist 
	  location 
	  (scale-pointlist radius-pts 
							 (map (lambda (x) 
									  (list (cos (/ (* tau x) divisions)) 
											  (sin (/ (* tau x) divisions))))
									(list-tabulate divisions (lambda (x) x))
									)))))

(define (deep-string->number lst) 
  (cond
	((null? lst) lst)
	((and (not (string? lst)) (atom? lst)) lst)
	((and (string? lst) (string->number lst)) (string->number lst))
	((pair? lst) (map deep-string->number lst))
	(#t lst)))

(define (deep-string->symbol lst) 
  (cond
	((null? lst) lst)
	((and (not (string? lst)) (atom? lst)) lst)
	((and (atom? lst) (string->number lst)) lst)
	((and (string? lst) (string->symbol lst)) (string->symbol lst))
	((pair? lst) (map deep-string->symbol lst))
	(#t lst)))

(define (load-data fname)
  (let ((data (deep-string->number (load-list-from-file fname))))
	 (display (string-append "Loaded " fname "\n"))
	 data))


(define (adjusted-plot-polygon ps width greyvalue open-path project-point-fn point-list)
  (let ((plot (if project-point-fn (map project-point-fn point-list) point-list)))
;(pp plot)
	 (plot-polygon ps width greyvalue plot open-path)))

(define (adjusted-plot-filled-polygon ps width bordervalue interiorvalue project-point-fn point-list)
  (let ((plot (if project-point-fn (map project-point-fn point-list) point-list)))
;(pp plot)
	 (plot-filled-polygon ps width bordervalue interiorvalue plot)))

;; These are support routines that get called both here and from functions in other files.....

(define ps-circle
  (lambda (ps rad x width whiteness #!optional filled)
	 ;; (adjusted-plot-polygon pshandle width whiteness openpath? projectionfn pointlist) ;; whiteness is in [0,1]
	 (if filled
		  (adjusted-plot-filled-polygon ps width whiteness #f #f 0.1 (make-circle x rad 120))
		  (adjusted-plot-polygon ps width whiteness #f #f (make-circle x rad 120))		)

	 )
  )

;Old code may need
;     (define projection linear-projection)
;
(define (linear-projection loc range co-range)
  (lambda (x) (translate-pointlist loc (scale-pointlist (/ co-range range) x))))


(define (set-font ps . args)
  (case (length args)
	 ((1) (if (eq? (car args) 'reset)
				 (times-roman 12)
				 (error "argument to set-font should either be 'reset or a font and size" args)))
	 ((2)  (apply ps (cons  'font  args)))
	 (else (error "Too many arguments to set-font" args))))

(define (set-color ps . args)
  (case (length args)
	 ((1 3) (if (eq? (car args) 'reset)
					(ps 'set-grey 0.0)
					(if (= 1 (length args))
						 (ps  'setgrey (car args))
						 (apply ps (cons 'setrgb args)))))

	 (else (error "argument to set-color should either be 'reset or a single number or a triplet" args))))
(define set-colour set-color)

(define (set-linewidth ps . args)
  (case (length args)
	 ((1) (if (eq? (car args) 'reset)
				 (times-roman 12)
				 (ps 'linewidth 0.2)))
	 ((2)  (apply ps (cons  'font  args))
	  				 (apply ps (cons 'linewidth args)))
	 (else (error "Too many arguments to set-linewidth" args))))


(define (make-ps port/filename fontlist)
  (let ((file (cond
					((output-port? port/filename) port/filename)
					(else (open-output-file port/filename))))
		  (fonts fontlist)
		  (pagescale '(1.0 1.0))
		  (pageoffset '(0 0))
		  (pagecount 0)
		  )

    (define (ps-display thing)
      (display thing file)
      (display "\n" file))

    (define (ps-1-arg command arg)
      (display arg file)
      (display " " file)
      (display command file)
      (display "\n" file)
      )
    
    (define (ps-2-arg cmd x y)
      (display x file)
      (display " " file)
      (display y file)
      (display " " file)
      (display cmd file)
      (display "\n" file)
      )

    (define (ps-3-arg cmd x y z)
      (display x file)
      (display " " file)
      (display y file)
      (display " " file)
      (display z file)
      (display " " file)
      (display cmd file)
      (display "\n" file)
      )

    (define (ps-4-arg cmd a1 a2 a3 a4 a5)
      (display a1 file)
      (display " " file)
      (display a2 file)
      (display " " file)
      (display a3 file)
      (display " " file)
      (display a4 file)
      (display " " file)
      (display cmd file)
      (display "\n" file))

    (define (ps-5-arg cmd a1 a2 a3 a4 a5)
      (display a1 file)
      (display " " file)
      (display a2 file)
      (display " " file)
      (display a3 file)
      (display " " file)
      (display a4 file)
      (display " " file)
      (display a5 file)
      (display " " file)
      (display cmd file)
      (display "\n" file))

    (define (ps-pair-or-list cmd pointlist)
      (cond 
       ((null? pointlist) #f)
       ((and (list? pointlist) (list? (car pointlist)))
		  (let loop ((p pointlist))
			 (if (not (null? p))
				  (begin
					 (ps-pair-or-list cmd (car p))
					 (loop (cdr p))))))
       ((list? pointlist)
		  (display (car pointlist) file)
		  (display " " file)
		  (display (cadr pointlist) file)
		  (display " " file)
		  (display cmd file)
		  (display "\n" file))
       (#t #f)))
	 

    (define (font nfont size)
      (if (not (null? fonts))
			 (begin
				(display "/" file)
				(display nfont file)
				(display " findfont\n" file)
				(display size file)
				(display " scalefont setfont\n" file))
			 #f))

    (define (times-roman size)
      (font "Times-Roman" size))

    (define (times-italic size)
      (font "Times-Italic" size))
    
    (define (times-bold size)
      (font "Times-Bold" size))
    
    (define (helvetica size)
      (font "Helvetica" size))
    
    (define (helvetica-italic size)
      (font "Helvetica-Italic" size))
    
    (define (helvetica-bold size)
      (font "Helvetica-Bold" size))
    
    (define (emit-header)
      (ps-display "%!PS-Adobe-1.0")

      (display "%%DocumentFonts: " file)
      (if (list? fonts)
			 (map (lambda (x) 
					  (display x file) 
					  (display " " file)) fonts)
			 (display fonts file))
      (display "\n" file)
      
      (ps-display "%%Pages: (atend)\n")
      (ps-display "%%EndProlog\n")
      )
    
    (define (define-unitnames)
      (ps-display "/inch {72 mul} def")
      (ps-display "/mm {2.8346456693 mul} def\n")
      )


    (define (showpage)
      (set! pagecount (+ 1  pagecount))
      (ps-display "showpage"))

    (define (select-page vert horiz) ; in units of one page length or width
      (let ((v (* (/ 297 25.4) 72))
				(h (* (/ 210 25.4) 72))
				)
		  (set! pageoffset (list (* horiz h) (* vert v)))
		  ))
	 

    (define (start-page label number)
      (display "%%Page: " file)
      (display label file)
      (display " " file)
      (display number file)
      (display "\n" file)
      (apply translate pageoffset)
      (apply scale pagescale)
      )
    
    (define (end-page)
      #t
;      (apply translate pageoffset)
;      (apply scale pagescale)
      )

    (define (trailer)
      (ps-display "%%Trailer"))
    
    (define (gsave)
      (ps-display "gsave"))

    (define (grestore)
      (ps-display "grestore"))
    
    (define (lineto x)
      (ps-pair-or-list "lineto" x))
    
    (define (rlineto x)
      (ps-pair-or-list "rlineto" x))
    
    (define (moveto x)
      (ps-pair-or-list "moveto" x))
    
    (define (rmoveto x)
      (ps-pair-or-list "rmoveto" x))
    
    (define (closepath)
      (ps-display "closepath"))

    (define (newpath)
      (ps-display "newpath"))

    (define (exch)
      (ps-display "exch"))

	 (define (currentpoint)
		(ps-display "currentpoint"))

	 (define (stringwidth s)
		(show s)
		(ps-display string-width))

	 (define (stringwidth* lst)
		(show (string-append (map object->string lst)))
		(ps-display string-width))

	 (define (lineskip #!optional specific)
		(if (not specific) 
			 (set! specific "OgHqQ")
			 (cond
			  ((string? specific) #t)
			  ((list? specific) (set! specific (string-append map object->string specific)))
			  (else (set! specific (object->string specific)))))

		(gsave)
		(show-charpath 'true specific)
		(ps-display " exch pop exch sub exch pop %%%% This should be the line height\n")
		(grestore)
		)

	 (define (lineskip* lst)
		(show (string-append (map object->string lst)))
		(lineskip))

    (define (setlinewidth weight)
      (ps-1-arg "setlinewidth" weight))
    
    (define (setgray weight)
      (ps-1-arg "setgray" weight))
    
    (define (setrgb r g b)
      (ps-3-arg "setrgbcolor" r g b))

    (define (sethsv h s v)
      (ps-3-arg "sethsvcolor" h s v))
	     
    (define (stroke)
      (ps-display "stroke"))
    
    (define (fill)
      (ps-display "fill"))

    (define (rotate angle)
      (ps-1-arg "rotate" angle))

    (define (scale x y)
      (ps-2-arg "scale" x y))

    (define (translate x y)
      (ps-2-arg "translate" x y))

    (define (arc cx cy rad startangle endangle)
      (ps-5-arg "arc" cx cy rad startangle endangle))

    (define (arcn cx cy rad startangle endangle)
      (ps-5-arg "arcn" cx cy rad startangle endangle))

    (define (map-character c)
      (if (not (char? c))
			 c
			 (cond 
			  ;;((eq? c #\ht) "\\t")
			  ((eq? c #\tab) "\\t")
			  ((eq? c #\newline) "\\n")
			  ((eq? c #\return) "\\t")
			  ((eq? c #\)) "\\)")
			  ((eq? c #\() "\\(")
			  (#t c))))

    (define (show-map string)
      (map map-character (string->list string)))
	 
    (define (show tlist)
      (if (null? fonts)
			 #f
			 (begin
				(cond 
				 ((string? tlist)
				  (display (string-append "(" tlist ") show\n") file))
				 ((number? tlist)
				  (display (string-append "(" (number->string tlist) ") show\n") file))
				 ((list? tlist) (map show tlist)))
				))
      )

	 (define (show! tlist) ;; keeps the current pointer in the place it starts (at the beginning of the string)
		(ps-display " currentpoint")
		(show tlist)
		(ps-display "moveto"))


    (define (show-charpath mode tlist)
      (if (null? fonts)
			 #f
			 (begin
				(cond 
				 ((string? tlist)
				  (display (string-append "(" tlist ")" (if mode " true " " false ") "charpath pathbbox\n") file))
				 ((number? tlist)
				  (display (string-append "(" (number->string tlist) ")" (if mode " true " " false ") "charpath pathbbox\n") file))
				 ((list? tlist) (map show tlist)))
				))
      )

	 (define (make-place name)
		(if (symbol? name) (set! name (symbol->string)))
		(ps-display (string-append "currentpoint\n/place-" name "-y exch def\n/place-" name "-x exch def"))
		)

	 (define (set-place name)
		(if (symbol? name) (set! name (symbol->string)))
		(ps-display "currentpoint")
		(ps-display (string-append "/place-" name "-y exch store\n/place-" name "-x exch store"))
		)

	 (define (place name)
		(if (symbol? name) (set! name (symbol->string)))
		(ps-display (string-append "/place-" name "-x load\n/place-" name "-y load")))

	 (define (column name)
		(if (symbol? name) (set! name (symbol->string)))
		(ps-display (string-append "/place-" name " load")))

	 (define (row name)
		(if (symbol? name) (set! name (symbol->string)))
		(ps-display (string-append "/place-" name " load")))

	 (define (linefeed n)
		(if (string? n) (set! n (string->number n)))
		(ps-display " 0 ")
		(ps-display " -1.25 ")
		(ps-display n)
		(lineskip)
		(ps-display " mul mul rmoveto"))

    (define (show-centered tlist)
		;;% string x y
		;;/center {moveto dup stringwidth pop -2 div 0 rmoveto show} def

      (gsave)
      (newpath)
      (moveto '(0 0))
      (show-charpath 'true tlist)
      (grestore)
      (ps-display "/scury exch def /scurx exch def /sclly exch def /scllx exch def\n")
      (ps-display "scllx scurx sub 2 div 0 rmoveto\n")
      (show tlist)
      )

	 (define (show-centered! tlist) ;; keeps the current pointer in the place it "starts" (at the centre)
      (gsave)
      (newpath)
      (moveto '(0 0))
      (show-charpath 'true tlist)
      (grestore)
      (ps-display "/scury exch def /scurx exch def /sclly exch def /scllx exch def\n")
      (ps-display "scllx scurx sub 2 div 0 rmoveto\n")
      (show! tlist)
		)

    (define (show-right tlist)
      (gsave)
      (newpath)
      (moveto '(0 0))
      (show-charpath 'true tlist)
      (grestore)
      (ps-display "/scury exch def /scurx exch def /sclly exch def /scllx exch def\n")
      (ps-display "scllx scurx sub 0 rmoveto\n")
      (show tlist)
      )

    (define (show-right! tlist) ;; Keeps the pointer at the beginning of the string, but 
      (gsave)                   ;; writes right to left (keeping the characters in the normal
      (newpath)                 ;; order)
      (moveto '(0 0))
      (show-charpath 'true tlist)
      (grestore)
      (ps-display "/scury exch def /scurx exch def /sclly exch def /scllx exch def\n")
      (ps-display "scllx scurx sub 0 rmoveto\n")
      (show! tlist)
      )

	 (define (show-table tlist)
		(for-each 
		 (lambda (line)
			(currentpoint)
			(show line)
			(ps-display " moveto")
			(linefeed 1)
			)
		 tlist))
	 

    (emit-header)

	 (letrec ((postscript-handle 
				  (lambda x
					 (if (null? x)
						  #f
						  (let ((cmd (car x))
								  (args (cdr x)))
							 (cond
							  ((eq? cmd 'file) file)
							  ((eq? cmd 'close) 
								(trailer)
								(display "%%Pages: " file)
								(display pagecount file)
								(display "\n" file)
								(close-output-port file))

							  ((eq? cmd 'postscript) (apply ps-display args))
							  ((eq? cmd 'comment) 
								(apply ps-display (append (list "\n%%\n%% ") (map make-it-a-string args) (list "\n%%\n")))
								)

							  ((eq? cmd 'set-font) (apply font args))
							  ((eq? cmd 'font) (apply font args))
							  ((eq? cmd 'Times-Roman) (apply font "Times-Roman" args))
							  ((eq? cmd 'Times-Italic) (apply font "Times-Italic" args))
							  ((eq? cmd 'Times-Bold) (apply font "Times-Bold" args))
							  ((eq? cmd 'Helvetica) (apply font "Helvetica" args))
							  ((eq? cmd 'Helvetica-Italic) (apply font "Helvetica-Italic" args))
							  ((eq? cmd 'Helvetica-Bold) (apply font "Helvetica-Bold" args))
							  
							  ((eq? cmd 'show) (apply show args))
							  ((eq? cmd 'show!) (apply show! args))
							  ((eq? cmd 'show-charpath) (apply show-charpath args))
							  ((eq? cmd 'show-centered) (apply show-centered args))
							  ((eq? cmd 'show-right) (apply show-right args))
							  ((eq? cmd 'show-centered!) (apply show-centered! args))
							  ((eq? cmd 'show-right!) (apply show-right! args))

							  ((eq? cmd 'show-table) (apply show-table args))

							  ((eq? cmd 'start-page) (apply start-page args))
							  ((eq? cmd 'end-page) (apply end-page args))

							  ((eq? cmd 'gsave) (gsave))
							  ((eq? cmd 'grestore) (grestore))
							  ((eq? cmd 'showpage) (showpage))

							  ((eq? cmd 'moveto) (moveto args))
							  ((eq? cmd 'rmoveto) (rmoveto args))
							  ((eq? cmd 'lineto) (lineto args))
							  ((eq? cmd 'rlineto) (rlineto args))
							  ((eq? cmd 'closepath) (closepath))
							  ((eq? cmd 'newpath) (newpath))
							  ((eq? cmd 'exch) (exch))

							  ((eq? cmd 'lineweight) (apply setlinewidth args))
							  ((eq? cmd 'setgrey) (apply setgray args))
							  ((eq? cmd 'setrgbcolor) (apply setrgb args))
							  ((eq? cmd 'setrgbcolour) (apply setrgb args))
							  ((eq? cmd 'sethsvcolor) (apply sethsv args))
							  ((eq? cmd 'sethsvcolour) (apply sethsv args))
							  ((eq? cmd 'setlinewidth) (apply setlinewidth args))
							  ((eq? cmd 'setgray) (apply setgray args))
							  ((eq? cmd 'stroke) (stroke))
							  ((eq? cmd 'fill) (fill))

							  ((eq? cmd 'rotate) (apply rotate args))
							  ((eq? cmd 'translate) (apply translate args))
							  ((eq? cmd 'scale) (apply scale args))

							  ((eq? cmd 'arc) (apply arc args))
							  ((eq? cmd 'arcn) (apply arcn args))
							  ((eq? cmd 'pages) pagecount)
							  ((eq? cmd 'define-units) (define-unitnames))

							  ((eq? cmd 'make-place) (apply make-place args))
							  ((eq? cmd 'set-place) (apply set-place args))
							  ((eq? cmd 'place) (apply place args))
							  ((eq? cmd 'column) (apply column args))
							  ((eq? cmd 'row) (apply row args))
							  ((eq? cmd 'linefeed) (apply linefeed args))

							  (#t (map display cmd " is not recognised\n")))
							 ))
					 )))
		postscript-handle
		)
    ))


;; For example
(define (graph-paper ps gridsize)
  (let* ((g (* 72 (/ gridsize 25.4)))
			(W (* (/ 210 25.4) 72))
			(H (* (/ 297 25.4) 72))
			(w W)
			(h H)
			)

    (set! w (* g (- (round (/ W g)) 4)))
    (set! h (* g (- (round (/ H g)) 8)))

    (ps 'setgray 0.5)
    (ps 'setlinewidth 0.072)

    (ps 'translate (/ (- W w) 2.0) (/ (- H h) 2.0))

    (let first ((i 0))
      (if (<= (* i g) w)
			 (begin
				(ps 'moveto (* i g) 0)
				(ps 'rlineto 0 h)
				(first (+ 1  i))
				))
      )
    (let second ((i 0))
      (if (<= (* i g) h)
			 (begin
				(ps 'moveto 0 (* i g))
				(ps 'rlineto w 0)
				(second (+ 1  i))
				))
		
      )
    (ps 'stroke)
    )
  ps
  )



(define (make-graph-paper filename gridsize)
  (let* ((ps (make-ps filename '())))
    (graph-paper ps gridsize)
    (ps 'showpage)
    (ps 'close)
	 ))

(define (plot-polygon ps border weight vlist . open-polygon)
  (ps 'newpath)
  (ps 'moveto (car vlist))
  (let loop ((v (cdr vlist)))
    (if (null? v)
		  (if (or (null? open-polygon) (not (car open-polygon))) (ps 'closepath))
		  (if (and (list? v) 
					  (list? (car v)) 
					  (not (null? (car v))) 
					  (apply ps-andf (map number? (car v))))
				(begin
				  (ps 'lineto (caar v) (cadar v))
				  (loop (cdr v))))))

  (ps 'setgray weight)  
  (ps 'setlinewidth border)
  (ps 'stroke)
  )

(define (plot-filled-polygon ps border bfill pfill vlist)
  (ps 'newpath)
  (ps 'moveto (car vlist))
  (let loop ((v (cdr vlist)))
    (if (null? v)
		  (begin
			 (ps 'closepath)
			 (ps 'gsave))
		  (if (and (list? v) 
					  (list? (car v)) 
					  (not (null? (car v))) 
					  (apply ps-andf (map number? (car v))))
				(	begin
				  (ps 'lineto (caar v) (cadar v))
				  (loop (cdr v))))))
  (ps 'setgray pfill)
  (ps 'fill)
  (ps 'grestore)
  (ps 'setgray bfill)
  (ps 'setlinewidth border)
  (ps 'stroke)
  )

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
