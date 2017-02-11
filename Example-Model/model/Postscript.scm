;-  Identification and Changes

;--
;	postscript.scm -- Written by Randall Gray 

;;; CANONICAL VERSION

;-  Discussion 

;;; "gray" is scaled from 0 (black) to 1 (white)

;-  Configuration stuff 

;-  Included files 

;-  Variables/constants both public and static

;--    Static data

;--    Public data 

;-  Code 

(load "lists.scm")

(define 100pi 314)
(define 10000pi 31416)
(define pi (* 4.0 (atan 1.0)))
 
(define rgb-i 0.33)
(define rgb-half 0.5)
(define rgb-I 0.66)
(define rgb-full 1.0)

(define ps-black 0.0)
(define ps-dark-grey 0.33)
(define ps-grey 0.5)
(define ps-light-grey 0.66)
(define ps-white 1.0)

(define ps-red '(1.0 0.0 0.0))
(define ps-green '(0.0 1.0 0.0))
(define ps-blue '(0.0 0.0 1.0))
(define ps-yellow '(1.0 1.0 0.0))
(define ps-cyan '(0.0 1.0 1.0))
(define ps-magenta '(1.0 0.0 1.0))

(define ps-pale-red '(1.0 0.66 0.66))
(define ps-pale-green '(0.66 1.0 0.66))
(define ps-pale-blue '(0.66 0.66 1.0))
(define ps-pale-yellow '(1.0 1.0 0.66))
(define ps-pale-cyan '(0.66 1.0 1.0))
(define ps-pale-magenta '(1.0 0.66 1.0))

(define ps-mid-red (list 0.66 0.0 0.0))
(define ps-mid-green (list 0.0 0.66 0.0))
(define ps-mid-blue (list 0.0 0.0 0.66))
(define ps-mid-magenta (list 0.66 0.0 0.66))
(define ps-mid-yellow (list 0.66 0.66 0.0))
(define ps-mid-cyan (list 0.0 0.66 0.66))

(define ps-dark-red (list 0.33 0.0 0.0))
(define ps-dark-green (list 0.0 0.33 0.0))
(define ps-dark-blue (list 0.0 0.0 0.33))
(define ps-dark-magenta (list 0.33 0.0 0.33))
(define ps-dark-yellow (list 0.33 0.33 0.0))
(define ps-dark-cyan (list 0.0 0.33 0.33))

(define ps-orange (list 1.0 0.5 0.0))
(define ps-rose (list 1.0 0.0 0.5))
(define ps-springgreen (list 0.5 1.0 0.0))
(define ps-purple (list 0.5 0.0 1.0 ))
(define ps-limegreen (list 0.0 1.0 0.5))
(define ps-seablue (list 0.0 0.5 1.0 ))

(define ps-dark-orange (list 0.66 0.33 0.0))
(define ps-dark-rose (list 0.66 0.0 0.33))
(define ps-dark-springgreen (list 0.33 0.66 0.0))
(define ps-dark-purple (list 0.33 0.0 0.66 ))
(define ps-dark-limegreen (list 0.0 0.66 0.33))
(define ps-dark-seablue (list 0.0 0.33 0.66 ))

(define ps-Rgb (list 0.33 0.33 0.33))
(define ps-rGb (list 0.33 0.33  0.33))
(define ps-rgB (list 0.33 0.33 0.33))




;--- (define (set-colour col) ...)

(define (normalise-colour f)
  (cond
	((number? f) f)
	((and (list? f) (= (length f) 3)) f)
	((and (pair? f) (list? (car f)) (colour? (car f))) (car f))
	((and (pair? f) (list? (cadr f)) (colour? (cadr f))) (cadr f))
	(#t '(0 0 0))))

(define (colour? f)
  (or (number? f) (and (list? f) (apply andf (map number? f))) (member (length f) '(1 3))))


(define set-linewidth
  (letrec((stack '())
			 (ps #f)
			 (twink #f)
			 (sf (lambda args
						(if (null? args)
							 (let ((tos (if (pair? stack) (car stack) 2)))
									 (ps 'setlinewidth tos))
							 (cond
							  ((and (pair? args) (eq? (car args) 'dump))
								(dnl "Postscript file is " ps)
								(dnl "stack:")(pp stack))
							  
							  ((and (pair? args) (eq? (car args) 'reset))
								(set! stack '())
								(set! ps #f))
							  
							  ((and (pair? args) (member (car args) '(size? width? size width)))
								(if (and ps (pair? stack))
									 (car stack)
									 #f))
							  
							  ((and (eq? (length args) 1) (procedure? (car args)) ps) ;; already initialised, replace it
								(let ((o (if (pair? stack) (car stack) #f)))
								  (set! ps (car args))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(ps 'setlinewidth (car stack)))
								  ))

							  ((and (eq? (length args) 1) (procedure? (car args)) (not ps)) ;; not initialised
								(let ((o (if (pair? stack) (car stack) #f)))
								  (set! ps (car args))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(ps 'setlinewidth  (car stack)))
								  ))

							  
							  ((and (eq? (length args) 2) (procedure? (car args)) ps  (number? (cadr args))) ;; already initialised, replace ps
								(let ((o (if (pair? stack) (car stack) #f)))
								  (set! ps (car args))
								  (set! stack (cons (cadr args) stack))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(ps 'setlinewidth  (car stack)))
								  ))
							  
							  ((and (eq? (length args) 2) (procedure? (car args)) (not ps)  (number? (cadr args)))
								(let ((o (if (pair? stack) (car stack) #f)))
								  (set! ps (car args))
								  (set! stack (cons (cadr args) stack))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(ps 'setlinewidth  (car stack)))
								  ))
							  
							  ((and (eq? (length args) 1) (number? (car args)))
								(let ((o (if (pair? stack) (car stack) #f)))
								  (set! stack (cons (car args) stack))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(ps 'setlinewidth  (car stack)))
								  ))


							  ((eq? 'push (car args))
								 (let ((o (if (pair? stack) (car stack) #f)))
									(if (and (= (length args) 2) (number? (cadr args))) (set! stack (cons (cadr args) stack)))
									
									(if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										 (ps 'setlinewidth  (car stack))))
								)

							  ((eq? 'pop (car args))
								(let ((o (if (pair? stack) (car stack) #f)))
								  (if (and (pair? stack)) (not (null? (cdr stack)))
										(set! stack (cdr stack)))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(ps 'setlinewidth  (car stack))))
								)
							  (#t (set-colour--unrecognised-command))
							  )
							 )))
				)
	 sf))


(define set-colour
  (let* ((set-colr (lambda (ps col)
							(set! col (normalise-colour col))
							(cond
							 ((not ps) (fatal-error-ps-not-defined))
							 ((and (list? col) (= (length col) 3) (apply fand (map number? col)))
							  (if (not (= (list-depth col) 1))
									#f
									(ps 'setrgbcolor col)))
							 ((number? col)
							  (ps 'setgray col))
							 ((eq? col #t)
							  (ps 'setgray 0.0))
							 (#t 	(dnl "Colour, " col ", is not a triple or a number")
									(fatal-error)))))
			)
	 (letrec((stack '())
				(ps #f)
				(twink #f)
				(sf (lambda args
						(if (null? args)
							 (let ((tos (car stack)))
								(if (list? tos)
									 (ps 'setrgbcolor tos)
									 (ps 'setgray tos)))
							 (cond
							  ((and (pair? args) (eq? (car args) 'dump))
								(dnl "Postscript file is " ps)
								(dnl "stack:")(pp stack))
							  
							  ((and (pair? args) (eq? (car args) 'reset))
								(set! stack '())
								(set! ps #f))
							  
							  ((and (pair? args) (member (car args) '(color? colour? colour color)))
								(if (and ps (pair? stack))
									 (car stack)
									 #f))
							  
							  ((and (eq? (length args) 1) (procedure? (car args)) ps) ;; already initialised, replace it
								(let ((o (if (pair? stack) (car stack) #f)))
								  (set! ps (car args))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(set-colr ps (car stack)))
								  ))

							  ((and (eq? (length args) 1) (procedure? (car args)) (not ps)) ;; not initialised
								(let ((o (if (pair? stack) (car stack) #f)))
								  (set! ps (car args))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(set-colr ps (car stack)))
								  ))

							  
							  ((and (eq? (length args) 2) (procedure? (car args)) ps (or (list? (cadr args)) (number? (cadr args)))) ;; already initialised, replace ps
								(let ((o (if (pair? stack) (car stack) #f)))
								  (set! ps (car args))
								  (set! stack (cons (cadr args) stack))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(set-colr ps (car stack)))
								  ))
							  
							  ((and (eq? (length args) 2) (procedure? (car args)) (not ps) (or (list? (cadr args)) (number? (cadr args))))
								(let ((o (if (pair? stack) (car stack) #f)))
								  (set! ps (car args))
								  (set! stack (cons (cadr args) stack))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(set-colr ps (car stack)))
								  ))
							  
							  ((or (and (eq? (length args) 1) (number? (car args)))
									 (and (list? (car args)) (= 3 (length (car args))) (apply andf (map number? (car args))) ))
								(let ((o (if (pair? stack) (car stack) #f)))
								  (set! stack (cons (car args) stack))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(set-colr ps (car stack)))
								  ))


								((and (= (length args) 3) (apply andf (map number? args)) )
								 (let ((o (if (pair? stack) (car stack) #f)))
									(if (pair? stack)
										 (set! stack (cons args stack))
										 (set! stack (cons args '())))
									(if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										 (set-colr ps (car stack)))))


							  ((eq? 'push (car args))
								 (let ((o (if (pair? stack) (car stack) #f)))
									(cond
									 ((= (length args) 4) (set! stack (cons (cdr args) stack)))
									 ((and (= (length args) 2) (number? (cadr args))) (set! stack (cons (cadr args) stack)))
									 ((and (= (length args) 2) (list? (cadr args))) (set! stack (cons (cadr args) stack)))
									 )

									(if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										 (set-colr ps (car stack))))
								)

							  ((eq? 'pop (car args))
								(let ((o (if (pair? stack) (car stack) #f)))
								  (if (and (pair? stack)) (not (null? (cdr stack)))
										(set! stack (cdr stack)))
								  (if (or twink (and ps (pair? stack) o (not (eq? o (car  stack)))))
										(set-colr ps (car stack))))
								)
							  (#t (set-colour--unrecognised-command))
							  )
							 )))
				)
		sf)))



(define set-font
  (let* ((stack '())
			(ps #f)
			(sf (lambda  args
					(if (null? args)
						 (ps 'set-font (caar stack) (cadar stack))
						 (cond
						  ((and (pair? args) (eq? (car args) 'dump))
							(dnl "Postscript file is " ps)
							(dnl "Font stack:")(pp stack))
						  ((and (pair? args) (eq? (car args) 'reset))
							(set! stack '())
							(set! ps #f))

						  ((and (pair? args) (member (car args) '(font? font)))
							(if (and ps (pair? stack))
								 (caar stack)
								 #f))

						  ((and (pair? args) (member (car args) '(size? size)))
							(if (and ps (pair? stack))
								 (cadar stack)
								 #f))

						  ((and (eq? (length args) 1) (procedure? (car args)) ps) ;; already initialised, replace it
							(set! ps (car args))
							(if stack (ps 'font (caar stack) (cadar stack)))
							)

						  ((and (eq? (length args) 1) (procedure? (car args)) (not ps)) ;; not yet initialised
							(set! ps (car args))
							(if stack (ps 'font (caar stack) (cadar stack)))
							)

						  ((and (null? stack) (eq? (length args) 2) (symbol? (car args)) (number? (cadr args)))
							(set! stack (list args))
							(if (and ps (pair? stack))(ps 'font (caar stack) (cadar stack)))
							)

						  ((and ps (null? stack) (eq? (length args) 3) (symbol? (cadr args)) (number? (caddr args)) (procedure? (car args)))
							(set! ps (car args))
							(set! stack (list (cdr args)))
							(ps 'font (cadr args) (caddr args))
							)

						  ((and (not ps) (null? stack) (eq? (length args) 3) (symbol? (cadr args)) (number? (caddr args)) (procedure? (car args)))
							(set! ps (car args))
							(set! stack (list (cdr args)))
							(ps 'font (cadr args) (caddr args))
							)

						  ;; do another setfont....
						  ((and ps stack (null? args))
							(ps 'font (caar stack) (cadar stack)))


						  ((and ps stack (null? args))
							(ps 'font (caar stack) (cadar stack)))
						  ((and ps (eq? (car args) 'push) (eq? (length args) 3) (symbol? (cadr args)) (number? (caddr args)))
							(set! stack (cons (cdr args) stack))
							(if (and ps (pair? stack)) (ps 'font (caar stack) (cadar stack))))
						  ((and ps (eq? (car args) 'push) (eq? (length args) 2) (symbol? (cadr args)) )
							(set! stack (cons (list  (cadr args) (cadar stack)) stack))
							(ps 'font (caar stack) (cadar stack)))
						  ((and ps (eq? (car args) 'push) (eq? (length args) 2) (number? (cadr args)) )
							(set! stack (cons (list (caar stack) (cadr args)) stack))
							(ps 'font (caar stack) (cadar stack)))

						  ((and ps (eq? (car args) 'pop) (eq? (length args) 1))
							(if (and (pair? stack) (> (length stack) 1))
								 (begin
									(set! stack (cdr stack))
									(ps 'font (car (car stack)) (cadr (car stack))))
								 (ps 'font (caar stack) (cadar stack))))


						  ;; This inserts at the bottom of the stack font
						  ((and ps stack (eq? (length args) 3) (symbol? (car args)) (eq? (car args) 'newbase) (symbol? (cadr args)) (number? (caddr args)))
							(set! stack (append (list-tail stack (- (length stack) 1)) (list (cdr args))))
							(ps 'font (cadr args) (caddr args))
							)

						  ;; set the top of the stack
						  ((and ps (eq? (length args) 1) (symbol? (car args)))
							(set! stack (cons (list (car args) (cadar stack)) (cdr stack)))
							(ps 'font (caar stack) (cadar stack))
							)

						  ;; set the top of the stack
						  ((and ps (eq? (length args) 1) (number? (car args)))
							(set! stack (cons (list (caar stack) (car args)) (cdr stack)))
							(ps 'font (car args) (cadr args))
							)

						  ;; set the top of the stack
						  ((and ps (eq? (length args) 2) (symbol? (car args)) (number? (cadr args)))
							(set! stack (cons args (cdr stack)))
							(ps 'font (car args) (cadr args))
							)

						  (#t (set-font--unrecognised-command ))
						  )))
				 ))
	 sf))







(define (make-it-a-string s) 
  (or (and (string? s) s) (and (char? s) (make-string 1 s)) (object->string s)))

(define (andf . args)
  (if (null? args)
		#t
		(and (car args) (apply andf (cdr args)))))

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

(define mm->pt mm->points)
(define pt->mm points->mm)

(define (mm->inches x)
  (rescale (/ 1.0 24.5) x))

(define (inches->mm x)
  (rescale 24.5 x))

(define a4 (list (mm->points 210) (mm->points 297)))
;; == 595.276 841.89 points
;; == 8.26772 x 11.6929 inches

(define pagesize '(595 841)) ;; in 1/72 inches...

(define (scaled-by-x x pagesize)
  (list x (* x (/ (cadr pagesize) (car pagesize)))))

(define (scaled-by-y y pagesize)
  (list x (* y (/ (car pagesize) (cadr pagesize)))))

;(define target-size (inches->points (scaled-by-x 6.8 pagesize)))

(define (make-list-matrix . rows)
  (if (apply = (map length rows))
		(map copy-list rows)
		#f))


(define (list-matrix? m)
  (and (pair? m) 
		 (apply andf (map simple-list? m)) 
		 (apply = (map length m))))

(define (transpose-list-matrix A)
  (if (list-matrix? A) 
		(let* ((dima (list (length A) (length (car A))))
				 (B (make-list* (list (cadr dima) (car dima)) 0)))
		  (for-each
			(lambda (i) 
			  (for-each
				(lambda (j)
				  (list-set* B (list j i) (list-ref* A (list i j))))
				(seq (cadr dima))))
			(seq (car dima)))
		  B)
		#f))

(define (*-matrix a b)
  (cond
	((and (number? a) (number? b)) (* a b))
	((and (number? b) (list-matrix? a)) (map (lambda (x) (map (lambda (y) (* b y)) x)) a))
	((and (number? a) (list-matrix? b)) (map (lambda (x) (map (lambda (y) (* a y)) x)) b))
	(else 
	 (let* ((dima (list (length a) (length (car a))))
			  (dimb (list (length b) (length (car b))))
			  )
		(if (not (= (cadr dima) (car dimb)))
			 (abort "incompatible matrices")
			 (map (lambda (x) (map (lambda (y) (apply + (map * x y))) (transpose-list-matrix b))) a))))))


(define (rotate-point-list theta pointlist)
  (map (lambda (x) (let ((m (make-list-matrix `(,(cos theta) ,(- (sin theta))) `(,(sin theta) ,(cos theta)))))
							(*-matrix-vector m x)))
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
			 (map operator deviant pointlist) ;; Original version
			 (map (lambda (o) (if (string? o) o (operator deviant o))) pointlist))))


(define (scale-pointlist k pointlist)
  (adjust * k pointlist))

(define (translate-pointlist offset pointlist)
  (adjust + offset pointlist))

(define (scale-pointlist* n k pointlist)
  (if (zero? n)
		(adjust * k pointlst)
		(map (lambda (lst) (scale-pointlist* (- n 1) k lst)) pointlst)))


(define (translate-pointlist* n offset lstlst) ;; This should be generalised....
  (if (zero? n)
		(adjust + offset lstlst)
		(map (lambda (lst) (translate-pointlist* (- n 1) offset lst)) lstlst)))

(define pi (* 4.0 (atan 1.0)))
(define 100pi 314)
(define 10000pi 31416)
(define tau (* 2.0 pi))

(define (fold-A-series-paper aN)
  (list (/ (cadr aN) 2.0) (car aN)))


(define a4 (list (mm->points 210) (mm->points 297)))
(define a5 (fold-A-series-paper a4))
(define a6 (fold-A-series-paper a5))

;; == 595.276 841.89 points
;; == 8.26772 x 11.6929 inches

(define sl '())
(define isl '())

(define comments-requested #f)


(define (list-tabulate len proc)
  (do ((i (- len 1) (- i 1))
       (ans '() (cons (proc i) ans)))
      ((< i 0) ans)))

(define (make-circle location radius-pts divisions)
  (translate-pointlist 
	location 
	(scale-pointlist radius-pts 
						  (map (lambda (x) 
									(list (cos (/ (* tau x) divisions)) 
											(sin (/ (* tau x) divisions))))
								 (list-tabulate divisions (lambda (x) x))
								 ))))

;--- centred on (0,0)
(define (make-box dX  dY . open-path) ;; anticlockwise polygon
  (let* ((dX/2 (/ dX 2.0)) (dY/2 (/ dY 2.0))
			(l (list (list dX/2 dY/2)
						(list (- dX/2) dY/2)
						(list (- dX/2) (- dY/2))
						(list dX/2 (- dY/2))))
			)
	 (if (or (null? open-path) (not (car open-path)))
		  (append l (list (list dX/2 dY/2)))
		  l
		  )))

;; box centre translated to p and is anticlockwise
(define (make-box-centre-at p dX  dY . open-path)
  (translate-pointlist  p (apply make-box (append  (list dX dY) open-path)))
  )


;; box translated so  that its bottom left corner is at p (dX/2 dY/2) and is anticlockwise
(define (make-box-bl-at p dX  dY . open-path)
;; (let ((box (if (or (null? open-path) (not (car open-path))) (make-box dX dY) (make-box dX dY (car open-path)))))
  (translate-pointlist (map + p (map (lambda (x) (/ x 2.0)) (list dX dY)))
							  (apply make-box (append (list dX dY) open-path) ))	)

;; box translated so that its top right corner is at p (dX/2 dY/2) and is anticlockwise
(define (make-box-tr-at p dX  dY . open-path)
  (translate-pointlist (map + p (map (lambda (x) (/ x -2.0)) (list dX dY)))
							  (apply make-box (append (list dX dY) open-path) ))	)

;; box translated so that its bottom right corner is at p (dX/2 dY/2) and is anticlockwise
(define (make-box-br-at p dX  dY . open-path)
  (translate-pointlist (map + p (map (lambda (x) (/ x 2.0)) (list (- dX) dY)))
							  (apply make-box (append (list dX dY) open-path))))

;; box translated so that its bottom right corner is at p (dX/2 dY/2) and is anticlockwise
(define (make-box-tl-at p dX  dY . open-path)
  (translate-pointlist (map + p (map (lambda (x) (/ x 2.0)) (list dX (- dY))))
							  (apply make-box (append (list dX dY) open-path))))


(define (make-circle location radius-pts divisions)
  (translate-pointlist 
	location 
	(scale-pointlist radius-pts 
						  (map (lambda (x) 
									(list (cos (/ (* tau x) divisions)) 
											(sin (/ (* tau x) divisions))))
								 (list-tabulate divisions (lambda (x) x))
								 ))))

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

(define (adjusted-plot-filled-polygon ps width border-g/rgb int-g/rgb project-point-fn point-list)
  (let ((plot (if project-point-fn (map project-point-fn point-list) point-list)))
;(pp plot)
	 (plot-filled-polygon ps width border-g/rgb int-g/rgb plot)))

;; These are support routines that get called both here and from functions in other files.....

(define ps-circle
  (lambda (ps rad x width g/rgb #!optional filled)
	 ;; (adjusted-plot-polygon pshandle width g/rgb open-path? projectionfn pointlist) ;; g/rgb is in [0,1]
	 (if filled
		  (adjusted-plot-filled-polygon ps width g/rgb #f #f 0.1 (make-circle x rad 120))
		  (adjusted-plot-polygon ps width g/rgb #f #f (make-circle x rad 120))		)

	 )
  )

(define (rplot-ll-box ps width dx dy border fill)
  (let ((B (map (lambda (p) (list (* dx (car p)) (* dy (cadr p))))
					  '((0 0) (1 0) (1 1) (0 1) (0 0)))))
	 (if fill
		  (rplot-filled-polygon ps width border fill B)
		  (rplot-polygon ps width border B))))

(define (rplot-lr-box ps width dx dy border fill)
  (let ((B (map (lambda (p) (list (* dx (car p)) (* dy (cadr p))))
					  '((0 0) (-1 0) (-1 1) (0 1) (0 0)))))
	 (if fill
		  (rplot-filled-polygon ps width border fill B)
		  (rplot-polygon ps width border B))))
	 

(define (projection loc range co-range)
  (lambda (x) (translate-pointlist loc (scale-pointlist (/ co-range range) x))))

(define (make-ps-string .  args)
  (let ((filename 'none)
		  (fontlist 'none)
		  )
	 (for-each (lambda (x)
					 (cond	
					  ((and (or (null? x) (eq? x #t)) (eq? fontlist 'none))
						(set! fontlist
								'(Times-Roman Times-Italic Times-Bold Times-Bold Times-BoldItalic
												  Helvetica Helvetica-Oblique Helvetica-Bold Helvetica-BoldOblique
												  Courier Courier-Oblique Courier-Bold Courier-BoldOblique
												  Symbol)))
					  ((and (list? x) (eq? fontlist #f) (set! fontlist '())))
					  
					  ((and (string? x) (eq? filename 'none)) (set! filename x))
					  ))
				  args)

	 (let ((psp (open-output-string)))
		(make-ps psp fontlist))))




(define (make-ps port/filename . args)
  (let ((filename 'none)
		  (fontlist 'none)
		  )
	 (for-each (lambda (x)
					 (cond	
					  ((and (or (null? x) (eq? x #t)) (eq? fontlist 'none))
						(set! fontlist
								'(Times-Roman Times-Italic Times-Bold Times-Bold Times-BoldItalic
												  Helvetica Helvetica-Oblique Helvetica-Bold Helvetica-BoldOblique
												  Courier Courier-Oblique Courier-Bold Courier-BoldOblique
												  Symbol)))
					  ((and (list? x) (eq? fontlist #f) (set! fontlist '())))
					  
					  ((and (string? x) (eq? filename 'none)) (set! filename x))
					  ))
				  args)


  (let ((file (cond
					((output-port? port/filename) port/filename)
					(else (open-output-file port/filename))))
		  (fonts fontlist)
		  (comments-requested #f)
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

    (define (ps-3-arg cmd r g b)
      (display r file)
      (display " " file)
      (display g file)
      (display " " file)
      (display b file)
      (display " " file)
      (display cmd file)
      (display "\n" file)
      )

    (define (ps-4-arg cmd r g b t)
      (display r file)
      (display " " file)
      (display g file)
      (display " " file)
      (display b file)
      (display " " file)
      (display t file)
      (display " " file)
      (display cmd file)
      (display "\n" file)
      )

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
      (set! pagecount (+ pagecount 1))
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

    (define (dup)
      (ps-display "dup"))

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
    
    (define (setrgbcolor . weight) ;; weight can either be of the form '(r g b) or '((r g b))
		(if (pair? (car weight))
			 (set! weight (car weight)))
		(ps-3-arg "setrgbcolor" (list-ref weight 0) (list-ref weight 1) (list-ref weight 2)))
    
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
      (ps-5-arg "arc" cx cy rad startange endangle))

    (define (arcn cx cy rad startangle endangle)
      (ps-5-arg "arcn" cx cy rad startange endangle))

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

	 (define (charpath tf s)
		(ps-display (string-append "(" s ")" " " (if tf "true " "false ") "charpath")))
    


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

	 (define (show! tlist) ;; keeps the current
		pointer in the place it starts (at the beginning of the string)
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
		(if (symbol? name) (set! name (symbol->string name)))
		(ps-display (string-append "currentpoint\n/place-" name "-y exch def\n/place-" name "-x exch def"))
		)

	 (define (set-place name)
		(if (symbol? name) (set! name (symbol->string name)))
		(ps-display "currentpoint")
		(ps-display (string-append "/place-" name "-y exch store\n/place-" name "-x exch store"))
		)

	 (define (place name)
		(if (symbol? name) (set! name (symbol->string name)))
		(ps-display (string-append "/place-" name "-x load\n/place-" name "-y load")))

	 (define (column name)
		(if (symbol? name) (set! name (symbol->string name)))
		(ps-display (string-append "/place-" name " load")))

	 (define (row name)
		(if (symbol? name) (set! name (symbol->string name)))
		(ps-display (string-append "/place-" name " load")))

	 (define (linefeed n)
		(if (string? n) (set! n (string->number n)))
		(ps-display " 0 ")
		(ps-display " -1.25 ")
		(ps-display n)
		(lineskip)
		(ps-display " mul mul rmoveto"))

    (define (show-left tlist)
		;;% string x y
		;;/center {moveto dup stringwidth pop -2 div 0 rmoveto show} def

      (gsave)
      (newpath)
      (moveto '(0 0))
      (show-charpath 'true tlist)
      (grestore)
      (ps-display "/scury exch def /scurx exch def /sclly exch def /scllx exch def\n")
      (ps-display "scllx 0 rmoveto\n")
      (show tlist)
      )

    (define (show-left! tlist) ;; keeps the current pointer in the place it "starts" (at the centre)
		;;% string x y
		;;/center {moveto dup stringwidth pop -2 div 0 rmoveto show} def

      (gsave)
      (newpath)
      (moveto '(0 0))
      (show-charpath 'true tlist)
      (grestore)
      (ps-display "/scury exch def /scurx exch def /sclly exch def /scllx exch def\n")
      (ps-display "scllx 0 rmoveto\n")
      (show! tlist)
      )

    (define (show-center tlist)
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

	 (define (show-center! tlist) ;; keeps the current pointer in the place it "starts" (at the centre)
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

							 (if comments-requested
								 (apply ps-display (list (string-append  "\n%%\n%% " (with-output-to-string '() (lambda ()	(dnl cmd args))) "\n%%\n"))))
									 
							 (cond
							  ((eq? cmd 'comments-on) (set! comments-requested #t))
							  ((eq? cmd 'comments-off) (set! comments-requested #f))
							  ((eq? cmd 'file) file)
							  ((eq? cmd 'close) 
								(trailer)
								(display "%%Pages: " file)
								(display pagecount file)
								(display "\n" file)
								(close-output-port file))

							  ((eq? cmd 'postscript) (apply ps-display args))
							  ((eq? cmd 'comment) 
								(ps-display (string-append "\n%%\n%% " (apply string-append (map make-it-a-string args)) "\n%%\n"))
								)

							  ((eq? cmd 'font) (apply font args))
							  ((eq? cmd 'Times-Roman) (apply font "Times-Roman" args))
							  ((eq? cmd 'Times-Italic) (apply font "Times-Italic" args))
							  ((eq? cmd 'Times-Bold) (apply font "Times-Bold" args))
							  ((eq? cmd 'Helvetica) (apply font "Helvetica" args))
							  ((eq? cmd 'Helvetica-Italic) (apply font "Helvetica-Italic" args))
							  ((eq? cmd 'Helvetica-Bold) (apply font "Helvetica-Bold" args))

							  ((eq? cmd 'charpath) (apply charpath args))
							  
							  ((eq? cmd 'currentpoint) (currentpoint))
							  ((eq? cmd 'show) (apply show args))
							  ((eq? cmd 'show!) (apply show! args))
							  ((eq? cmd 'show-charpath) (apply show-charpath args))
							  ((eq? cmd 'show-center) (apply show-center args))
							  ((eq? cmd 'show-right) (apply show-right args))
							  ((eq? cmd 'show-left) (apply show-left args))
							  ((eq? cmd 'show-center!) (apply show-center! args))
							  ((eq? cmd 'show-right!) (apply show-right! args))
							  ((eq? cmd 'show-left!) (apply show-left! args))

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
							  ((eq? cmd 'dup) (dup))

							  ((eq? cmd 'lineweight) (apply lineweight args))
							  ((eq? cmd 'grey) (apply grey args))
							  ((eq? cmd 'setlinewidth) (apply setlinewidth args))
							  ((eq? cmd 'setgray) (apply setgray args))
							  ((eq? cmd 'setrgbcolor) (apply setrgbcolor args))
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

							  (#t (display (string-append (object->string cmd) " is not recognised\n")))
							  
							 ))
					 ))) )
		postscript-handle
		)
    ))
  )



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
				(first (+ i 1))
				))
      )
    (let second ((i 0))
      (if (<= (* i g) h)
			 (begin
				(ps 'moveto 0 (* i g))
				(ps 'rlineto w 0)
				(second (+ i 1))
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

(define (plot-polygon ps width rgb vlist . open-path)
  (ps 'newpath)
  (ps 'moveto (car vlist))
  (let loop ((v (cdr vlist)))
    (if (null? v)
		  (if (or (null? open-path) (not (car open-path))) (ps 'closepath))
		  (if (and (list? v) 
					  (list? (car v)) 
					  (not (null? (car v))) 
					  (apply andf (map number? (car v))))
				(begin
						(ps 'lineto (caar v) (cadar v))
						(loop (cdr v))))))

  
  (ps (if (number? rgb) 'setgray 'setrgbcolor) rgb)
		
  (ps 'setlinewidth width)
  (ps 'stroke)
  )

(define (plot-filled-polygon ps width bfill pfill vlist)
;;  (plot-polygon ps width bfill vlist)

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
					  (apply andf (map number? (car v))))
				(begin
				  (ps 'lineto (caar v) (cadar v))
				  (loop (cdr v))))))
    (ps (if (number? pfill) 'setgray 'setrgbcolor) pfill)
  (ps 'fill)

  (ps (if (number? bfill) 'setgray 'setrgbcolor) bfill)
  (ps 'stroke)

  (ps 'grestore)
;;  (if (number? bfill)
;;		(ps 'setgray bfill)
;;		(ps 'setrgbcolor bfill))
;;  (ps 'setlinewidth width)
  ;;  (ps 'stroke)
 )


(define (rplot-polygon ps width rgb vlist . open-path)
  (ps 'newpath)
  (ps 'rmoveto (car vlist))
  (let loop ((v (cdr vlist)))
    (if (null? v)
		  (if (or (null? open-path) (not (car open-path))) (ps 'closepath))
		  (if (and (list? v) 
					  (list? (car v)) 
					  (not (null? (car v))) 
					  (apply andf (map number? (car v))))
				(begin
						(ps 'rlineto (caar v) (cadar v))
						(loop (cdr v))))))

  
  (ps (if (number? rgb) 'setgray 'setrgbcolor) rgb)
		
  (ps 'setlinewidth width)
  (ps 'stroke)
  )

(define (rplot-filled-polygon ps width bfill pfill vlist)
;;  (plot-polygon ps width bfill vlist)

  (ps 'newpath)
  (ps 'rmoveto (car vlist))
  (let loop ((v (cdr vlist)))
    (if (null? v)
		  (begin
			 (ps 'closepath)
			 )
		  (if (and (list? v) 
					  (list? (car v)) 
					  (not (null? (car v))) 
					  (apply andf (map number? (car v))))
				(begin
				  (ps 'rlineto (caar v) (cadar v))
				  (loop (cdr v))))))

  
  (ps (if (number? pfill) 'setgray 'setrgbcolor) pfill)
  (ps 'fill)

  (ps (if (number? bfill) 'setgray 'setrgbcolor) bfill)
  (ps 'stroke)

;;  (if (number? bfill)
;;		(ps 'setgray bfill)
;;		(ps 'setrgbcolor bfill))
;;  (ps 'setlinewidth width)
  ;;  (ps 'stroke)
 )

;-  The End 


;;; Local Variables:
;;; mode: scheme
;;; outline-regexp: ";-+"
;;; comment-column:0
;;; comment-start: ";;; "
;;; comment-end:"" 
;;; End:
