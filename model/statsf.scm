" This code in this file was written by Randall Gray in 2004-2005
   to provide summary statistics for the CSIRO project described in

    Ecosystem model specification within an agent based framework
    by R Gray and EA Fulton and LR Little and R Scott (2006)

    CSIRO, Hobart, Tasmania
    ISBN 1 921061 80 4 (pbk)
    ISBN 1 921061 82 0 (pdf)

  This subset of code makes use of components which were written by
  other researchers, notably,
"

(load "wt/wt.scm")
" wt.scm, which provides code to support weight balanced trees was
  written by Stephen Adams. The copyright, conditions of use code and
  documentation are all contained in the file ./wt/wt.scm.

  and  "

(load "irregex/irregex.scm")
" The irregular expression parsing library, Irregex.scm by Alex Shinn.
  The copyright, conditions of use, code and documentation may be found
  in the directory ./irregex/.
"

;; The statsf program takes data in in the (intensely parochial) TBL format 
;; namely
"
TBL
type1 _ type2 _ ...
datum ...
datum ...
.
.
.
"

;; Fields and data columns are space separated.  There is no mechanism
;; for including a space in a character string field.

;; Fieldnames are strings of nonspace characters (typically alphanums)

;; Types can be
;; C -- character (string)
;; I -- integer
;; F -- float
;; N -- number (most generic)
;; T -- time
;; H -- histogram (a set of numbers separated by commas) ** NOT supported
;;      in this program **





(define (twist args)
  (let loop ((a args)
				 )
    (if (or (null? a) (null? (car a)))
		  '()
		  (cons (map car a) (loop (map cdr a))))))

(define (map* op . l)
  (map (lambda (x) 
			(apply op x)) (twist l))
  )

; (apply map + l1*) takes '((1 2 3) (4 5 6)) -> '(5 7 9)
;
; I need something that takes 
; (fop -  '((9 8 7) (10 11 12)) '((1 2 3) (4 3 2))) --> ((8 6 4) (6 8 10 ))


;======================================================================;
;==                  Basic variable declarations                     ==;
;======================================================================;


(define sN 1) ;; this is the amount subtracted from N in the denominator used in calculating covariance

(define multistats '("covariance" "cov" "correlation" "cor"))

(define use-hash #f) ;; if #f it uses a wt-tree, else a hash table

(define hash-tsize 0)

(define infilename #f)
(define inport #f)
(define colon-ok #f)
(define outfilename #f)
(define outport #f)

(define time-rounding #f)
(define time-ix #f)
(define epoch #f)
(define unix-epoch 0)
(define bd-epoch 0)

(define tbl-output #t)

;  This is either taken from the command line or from the tbl file
(define tbl-column-name '()) 

;  This is either taken from the command line or from the tbl file
(define tbl-column-type '())

;  This is either taken from the command line or from the tbl file
(define input-column-name '()) 
(define key-column-name '()) 

(define covariant-column-name '()) 

;  This is taken from the command line
(define output-column-name '())
(define output-synonym '())
(define output-column-type '())

(define programname '())

(define lno 0)

(define line #f)
(define read-table-file '())

;; tbl-map is an ordered alist of all the columns and column numbers from the tbl file
(define tbl-map '())

;; key-map is an ordered alist of columns and column numbers from the tbl file
;; which form the key used to aggregate data
(define key-map '())

;; the 
(define input-map '())
(define covariant-map '())

(define StatWT '())
(define StatHash '())

(define PopStats '())

(define last-tag #f)
(define last-entry #f)
(define last-stats #f)


;======================================================================;
;==                  Basic function declarations                     ==;
;======================================================================;


(define (reset-stat-filter args)
  (set! use-hash #f) ;; if #f it uses a wt-tree, else a hash table

  (set! hash-tsize 0)

  (set! infilename #f)
  (set! inport #f)
  (set! colon-ok #f)
  (set! outfilename #f)
  (set! outport #f)

  (set! time-rounding #f)
  (set! time-ix #f)
  (set! epoch #f)
  (set! unix-epoch 0)
  (set! bd-epoch #f)

;  This is either taken from the command line or from the tbl file
  (set! input-column-name '()) 
  (set! key-column-name '()) 

  (set! covariant-column-name '()) 

;  This is taken from the command line
  (set! output-column-name '())
  (set! output-synonym '())
  (set! output-column-type '())

  (set! tbl-output #t)

;  This is either taken from the command line or from the tbl file
  (set! tbl-column-name '()) 

;  This is either taken from the command line or from the tbl file
  (set! tbl-column-type '())

  (set! programname (car args))

  (set! lno 0)

  (set! line #f)
  (set! read-table-file '())

  (set! tbl-map '())

  (set! input-map '())
  (set! covariant-map '())

  (set! key-map '())

  (set! StatWT '())
  (set! StatHash '())

  (set! PopStats '())

  (set! last-tag #f)
  (set! last-entry #f)
  (set! last-stats #f)
  )


(define (dnl . args) (map (lambda (x) (display x) (display " ")) args) (newline))
(define (ddnl tag . args)
  (if debugging
      (begin
		  (display "DEBUG ")(display tag)(display ": ")
		  (map (lambda (x) (display x) (display " ")) args) (newline))
		))

(define (pdnl tag . args)
  (if debugging
      (begin
		  (map (lambda (x) 
					(display "DEBUG ")
					(display tag)
					(display ": ")
					(pp x)
					) 
				 args) 
		  (newline)
		  )
      )
  )

(define (pp . l)
  (for-each pretty-print l))

(define (string-match expr str)
  (irregexp-match (irregexp expr) str))


(define (usage) 
  (display "usage: ")
  (display programname)
  (display " -modtime 86400 -epoch YYYY/MM/DD,HH:MM:SS (for example)\n")
  (display " -hash nrecs [-tbl] -i infile [name/num1 name/num2...] -o outfile n/n1 outn1 n/n2 outn2 ...\n")
  (display "for example:\n  ./StatFilter -hash 300000 \n    -i TissueLoad.tbl time taxon toxin\n    -o stat.out time time taxon taxon toxin toxin stat:mean:level pop_mean \n        stat:min pop_min stat:max pop_max stat:stddev pop_stddev\n")
  (display "The fields specified in the '-i filename ....' bit form the key\n")
  (display 
   "
Statfilter is used to generate aggregate statistics on columns in a
TBL file.  It understands times, dates and epochs as used in NWSui and
NWS-InVitro.  

The complete specification of the commandline is given
below:

   StatFilter [-wttree | -hash hashsize] [-no-tbl] [-sN dofn]
	      [-modtime nsecs] [-epoch YYYY/MM/DD,hh:mm:ss]
	      -i infile keyfield [keyfield ...]
	      -o outfile outfield fieldname [outfield fieldname ...]

or 
   StatFilter -h | -help | --help



Options
   
   -wttree

      Forces StatFilter to use the default tree representation for the
      aggregating structure even if a hash table size is specified.

   -hash hashsize

      This determines the size of the hash table which is keyed by the
      key fields and used to hold the statistics.  The hash size ought
      to be very much larger than the number of unique keys or it will
      run very, very slowly at best.

   -no-tbl

      Indicates that the TBL header should not be included in the
      output file.  This is not recommended.

   -sN dofn

      This number gets subtracted from the number of data points in
      the denominator when calculating covariance and correlation.

   -modtime nsecs

      When times (dates) are used as keys, the number of unique keys
      is mind boggling.  This option enables us to aggregate these
      keys by rounding to the nearest 'nsecs' from the start of the
      epoch. 

   -epoch YYYY/MM/DD,hh:mm:ss

      Sets the start of the epoch to the indicated date.  This is
      particularly useful when using '-modtime'.  Date fields are
      suppressed in the output file if a modtime is specified without
      having an epoch specified.  Usually one would set the epoch to
      the same as the epoch used for a model run.  

   -i|-in infile keyfield [keyfield ...]

      The infile specified here should be in TBL format.  The
      keyfields are column names as indicated in the input TBL file.
      These fields are used to form the aggregating key for the rest
      of the data in the record.  You must specify at least one
      keyfield.

   -colon-ok

      Indicates that colons are permitted in output filenames.

   -o|-out outfile outfield fieldname [outfield fieldname ...]

      This indicates what is to be written as output.  The filename
      <outfile> must not contain colons as a precaution against forget-
      ting to specify an output file.  Use '-colon-ok' if you MUST
      specify files with colons in their names.

      The specified output file (outfile) is written in TBL format 
      unless you specify '-no-tbl' with all of the columns (fieldnames)
      specified.  The keyfields are NOT written by default, and if you
      want them included in the output file they must be specified as
      well.  The contents of the columns are specified by the
      'outfield' parameter, which should either be the name of a
      column in the input TBL file, or a specification describing an
      aggregate statistic.  The form of this specification is 

         stat:whatstat:infield

      where infield is the name of one of the columns of the input TBL
      file.  Here 'stat:' is literal text and 'whatstat' is a
      specification drawn from the table below.

      Aggregate specifications for the indicated infield grouped by
      the key:

         n		number of samples
         min		minimum value
         mean		arithmetic mean
	 max		maximum value
	 sum		sum of the data
	 sum_x		sum of the data
	 sum_x-min	sum of (x-min)
	 sum_max-x	sum of (max-x)
	 sum_xx		sum of x squared
	 variance	variance in the data
	 variance-1	sample variance
	 stddev		standard deviation

	 median		the median value

	 median-0	the median value excluding values of zero from
			the sample

         median=m,M	reports the median where infield values less
			than or equal to m and greater than or equal
			to M  are excluded.  Make sure m and M are
			valid numbers.

         centile=c	reports the c centile (c is between 0 and 1)
		        Make sure c is a valid number.

         centile-0=c	reports the c centile (c is between 0 and 1)
		        Make sure c is a valid number.

         centile=c,m,M  reports the c centile (c is between 0 and 1)
                        where infield values less than or equal to m 
		        and greater than or equal to M are excluded.  
		        Make sure c, m and M are valid numbers.

         cov:x*y        calculates the covariance of columns x and y
                        using 
                              Sum[(x-xmean) * (y-ymean)]/(N -sN)

         cor:x*y        calculates the correlation of columns x and y
                        using 
                               Sum[(x-xmean) * (y-ymean)]
                             ------------------------------
                             (N -sN) * stddev(x) * stddev(y)

         covariance:x*y is a synonym for cov:x*y, and similarly for 
         correlations.


Examples:

   Treat each as being a single line.

	StatFilter -modtime 2419200 -epoch 1998/01/01,00:00:00 
		-hash 2000000 -i default/2/Deaths.tbl time date taxon reason 
                -o AIDeaths time time date date taxon taxon reason reason 
                stat:mean:mortality mmean stat:mean:members members 
                stat:mean:conc conc stat:mean:dt dt stat:min:mortality mmin 
                stat:median:mortality mmed stat:max:mortality mmax 
                stat:stddev:mortality mstddev

	StatFilter -i TissueLoad.tbl toxin name -o prawnload.tbl 
                toxin toxin name site stat:min:level min 
                stat:mean:level mean stat:max:level max 
	        stat:median:level median stat:median-0:level median0 
		stat:stddev:level stddev stat:n:level n


        StatFilter -tbl -modtime 2419200 -epoch 2000/01/01
	       -in logger_0000013_environment.tbl taxon time
	       -out test.tbl 
	       taxon taxon
	       stat:min:samplevalue min 
	       stat:mean:samplevalue mean
	       stat:max:samplevalue max
	       stat:centile=0.1:samplevalue c10
               stat:median:samplevalue median
	       stat:median-0:samplevalue median0
	       stat:centile=0.9:samplevalue c90
	       stat:stddev:samplevalue stddev
	       stat:n:samplevalue n
	       time time

       StatFilter -i tst.tbl tag -o otst.tbl tag tag stat:covariance:x*y cov_xy 
               stat:mean:x mx x x

       StatFilter -warning -modtime 2419200 
               -i Green/A/agg_TissueSource.tbl time date taxon toxin 
               -o Green/A/AggTissSource.tbl time time date date outfall outfall 
               taxon taxon toxin toxin plumeix plumeix stat:mean:srcconc srcconc 
               stat:mean:conc conc stat:mean:dt dt stat:mean:level smean 
               stat:min:level smin stat:median:level smedian stat:max:level smax 
               stat:stddev:level sstddev

       StatFilter -warning -i test.tbl abscissa value ordinate 
               -o testout.tbl abscissa x ordinate y value value 
               stat:min:value smin stat:median:value smed stat:max:value smax 
               stat:stddev:value dev
")
  )  



(define (real->integer k)
  (let* ((s (number->string k))
			(i (string-index s #\.))
			(ss (substring s 0 i))
			)
    (string->number ss)))


;======================================================================;
;==                         Initialisation                           ==;
;======================================================================;


(define (strspn str set)
  (let loop ((s str))
    (if (zero? (string-length s))
        (string-length str)
        (if (let inner-loop ((chset set))
              (if (zero? (string-length chset))
                  #f
                  (if (equal? (string-ref s 0)
										(string-ref chset 0))
                      #t
                      (inner-loop (substring chset 1 (string-length chset))))))
            (loop (substring s 1 (string-length s)))
            (- (string-length str) (string-length s))))))


(define (strcspn str set)
  (let loop ((s str))
    (if (zero? (string-length s))
        (string-length str)
        (if (let inner-loop ((chset set))
              (if (zero? (string-length chset))
                  #t
                  (if (equal? (string-ref s 0)
										(string-ref chset 0))
                      #f
                      (inner-loop (substring chset 1 (string-length chset))))))
            (loop (substring s 1 (string-length s)))
            (- (string-length str) (string-length s))))))


(define (strtok str sep)
  (if (string? str)
      (let loop ((results '())
                 (sstr str))
        (if (zero? (string-length sstr))
            results
            (if (zero? (strspn sstr sep))
                (loop (append results (list (substring sstr 0 (strcspn sstr sep)
																		 )))
                      (substring sstr (strcspn sstr sep) (string-length sstr)))
                (loop results
                      (substring sstr (strspn sstr sep) (string-length sstr)))))
		  ))
  )

(define (list-except exclude lst)
  (if (not (list? lst))
      lst
      (reverse 
       (let loop ((l lst) (r '()))
			(cond
			 ((null? l) r)
			 ((exclude (car l))
			  (loop (cdr l) r))
			 (else
			  (loop (cdr l) (cons (car l) r))))))))

(define (gsubstring s minx manx)
  (let ((ls (string-length s)))
    (substring s (min ls minx) (min ls manx))))

(define (dstring->number s) 
  (if (number? s) s 
      (if (string-match "[0-9][0-9.]*e-[3-9][0-9][0-9]" s)
			 0
			 (string->number s))))


;; Now we need to read figure out what the input columns are, 
;; where the input columns are going to be and what the output
;; columns are

; Remove empty strings from a list of strings
(define (filter-empty-strings s)
  (if (null? s) 
      '()
      (if (equal? "" (car s))
			 (filter-empty-strings (cdr s))
			 (cons (car s) (filter-empty-strings (cdr s)))
			 )))




; (define inport (open-input-file inputfile))
; (filter-empty-strings (string-split (read-line inport) #\space))

;======================================================================;
;==                    Define stats collecting stuff                 ==;
;======================================================================;


(define (stats-bin k)
;  (dnl "Defining stats bin with a key of " k)
  (let ((mode 'accurate) ;; fast or accurate
		  (fmin 1e+99)
		  (fmax -1e+99)
		  (mean 0)
		  (epsilon 0.0)
		  (variance 0.0)
		  (n 0.0)
		  (ssq 0.0)
		  (sum 0.0)
		  (data '())
		  (sorted #f)
		  (last-val #f)
		  (key k)
		  )

    (define (add-number x . i)
      (set! sorted #f)
      (if (null? i)
			 (set! i 1)
			 (set! i (car i)))

      (set! last-val x)
      (set! fmin (min x fmin))
      (set! fmax (max x fmax))

      (if (<= i 1)
			 (begin
				(set! n (1+ n))

				(set! ssq (+ ssq (* x x)))
				(set! sum (+ sum x)) 

				(set! data (cons x data))
				(set! epsilon (/ (- x mean) n))

				(cond
				 ((equal? mode 'fast)
				  (set! mean (/ sum n))
				  (set! variance (/ (- ssq (* n mean mean)) n) ))
				 (#t ;; 'accurate
				  (set! variance (+ (* (/ (- n 1) n) variance) 
										  (* epsilon epsilon (- n 1))))
				  (set! mean (+ epsilon mean)) )) )

			 (begin
				(set! n (+ n i))

				(set! ssq (+ ssq (* x x i)))
				(set! sum (+ sum (* x i)))

				(set! data (append (make-list i x) data))

				(cond
				 ((equal? mode 'fast)
				  (set! epsilon (/ (- x (/ (- sum x) (1- n)) n)))
				  (set! mean (/ sum n))
				  (set! variance (/ (- ssq (* n  mean mean)) n) ))
				 (#t
				  (let statloop ((j 0))
					 (if (< j i)
						  (begin
							 (set! epsilon (/ (- x mean) n))
							 (set! variance (+ (* (/ (- n 1) n) variance) 
													 p		 (* epsilon epsilon (- n 1))))
							 (set! mean (+ epsilon mean)) 
							 (statloop (1+ j)))))) ))) )

    (define (centile c)
      (centile-mM c #f #f))

    (define (centile-0 c)
      (centile-mM c 0 #f))

    (define (centile-mM centage m M)
      (if (> (abs centage) 1.0)
			 (set! centage (/ centage 100.0)))

      (if (zero? n)
			 0
			 (let ((d (sort (list-except 
								  (lambda (x) (if (number? M) (>= x M) M))
								  (list-except 
									(lambda (x) (if (number? m) (<= x m) m)) data)
								  )
								 <))
					 )
				(if (pair? d)
					 (list-ref d (real->integer (* (length d) centage)))
					 0)
				)))

    (define (median-mM m M)
      (centile-mM 0.5 m M))

    (define (median-0)
      (median-mM 0 #f))

    (define (median)
      (median-mM #f #f))


    (lambda d
      (let* ((card (if (symbol? (car d)) (symbol->string (car d)) (car d)))
				 (Card (if (string? card) (string->symbol card) (car d)))
				 )
		  (cond
			((or (null? d) (equal? (car d) 'list))
			 (set! data (sort data <))
			 (set! sorted #t)
			 (list 
			  (cons 'stat:data data)
			  (cons 'stat:mode mode)
			  (cons 'stat:n n) 
			  (cons 'stat:mean mean)
			  (if (<= n 0)
					(cons 'stat:median 0)
					(cons 'stat:median (median)))
			  (if (<= n 0)
					(cons 'stat:median-0 0)
					(cons 'stat:median-0 (median-0)))
			  (cons 'stat:min fmin)
			  (cons 'stat:max fmax)
			  (cons 'stat:sum sum)
			  (cons 'stat:sum_x sum)
			  (cons 'stat:sum_x-min (- sum (* n fmin)))
			  (cons 'stat:sum_max-x (- (* n fmax) sum))
			  (cons 'stat:sum_xx ssq)
			  (cons 'stat:variance variance)
			  (cons 'stat:variance-1 
					  (if (> n 0) (/ (* variance n) (1- n)) 0))
			  (cons 'stat:stddev (sqrt variance))))

			((number? (car d))
			 (add-number (car d)))

			((and (or (symbol? (car d)) (string? (car d))) 
					(or 
					 (string-match 
					  "(centile-0|centile|median)=.*" 
					  (if (symbol? (car d)) (symbol->string (car d)) (car d)))
					 (string=? "median" (if (symbol? (car d)) (symbol->string (car d)) (car d)))
					 (string-match "median-.*" (if (symbol? (car d)) (symbol->string (car d)) (car d))))
					)

			 (let* ((m #f)
					  (M #f)
					  (c 0.5)
					  (params (string-split card #\=))
					  (key (car params))
					  (args (if (> (length params) 1) (string-split (cadr params) #\,) '()))
					  )
				(if (or (string=? key "median") (string=? key "median="))
					 (begin
						(set! c 0.5)
						(if (>= (length args) 1) 
							 (set! m (car args)))
						(if (>= (length args) 2) 
							 (set! M (cadr args)))
						)
					 (begin ; centile=
						(set! c (car args))
						(if (eq? (length args) 2) 
							 (set! m (cadr args)))
						(if (eq? (length args) 3)
							 (begin
								(set! m (cadr args))
								(set! M (caddr args))))
						)
					 )
				
				(centile-mM (if c (dstring->number c) c) (if m (dstring->number m) m) (if M (dstring->number M) M))
				) )
			((or (symbol? (car d)) (string? (car d)))
			 (case Card
				('clear
				 (set! fmin 1e+99)
				 (set! fmax -1e+99)
				 (set! mean 0)
				 (set! epsilon 0.0)
				 (set! variance 0.0)
				 (set! n 0.0)
				 (set! ssq 0.0)
				 (set! sum 0.0)
				 (set! data '())
				 (set! sorted #f)
				 )
				
				('key key)
				('prod 'Ouch)
				('last last-val)
				('data data)
				('mode mode)
				('n n)
				('mean mean)
				('median (median))
				('median-0 (median-0))
				('median-mM (apply median-mM (cdr d)))
				('centile (apply centile (cdr d)))
				('centile-0 (apply centile-0 (cdr d)))
				('centile-mM (apply centile-mM (cdr d)))
				('min fmin)
				('max fmax)
				('sum sum)
				('sum_x sum)
				('sum_x-min (- sum (* n fmin)))
				('sum_max-x (- (* n fmax) sum))
				('sum_xx ssq)
				('variance variance)
				('variance-1 (if (> n 0) (/ (* variance n) (1- n)) 0))
				('stddev (sqrt variance))
				
				('fast (set! mode 'fast))
				('accurate (set! mode 'accurate))
				('fill-to 
				 (if (< (length d) 2)
					  'error-in-stat-bin
					  (let ((i (- n (cadr d)))
							  (v (if (= (length d) 2) 0 (caddr d))))
						 (if (> i 0) 
							  (add-number v i)))))
				('add
				 (if (< (length d) 2)
					  'error-in-stat-bin
					  (let ((i (cadr d))
							  (v (if (= (length d) 2) 0 (caddr d))))
						 (if (> i 0) 
							  (add-number v i)))))
				(else (list 'unimplemented-stat d)))
			 )
			(#t #f) ) 

		  ))))

(define (mapop op)
  (lambda (d m) (map* - d m) )
  )


;  (1- (length (let ((m (member sle (reverse (map car lst))) ) )
;		(if m m '()))))

;(define D '())
;(define M '())


(define (stats-bins k uni multi)
  (let ((bins '())
		  (covariants (if (and (list? multi) (not (null? multi))) multi #f))
		  )
    (let loop ((b (reverse (map car uni))))
      (if (not (null? b))
			 (begin
				(set! bins (acons (car b) (stats-bin k) bins))
				(loop (cdr b))))
      )

    (if multi
		  (let cov-init-loop ((m multi))
			 (if (not (null? m))
				  (cov-init-loop (cdr m))))
		  )
	 

    (lambda d
      (cond
       ((null? d)
		  (cons 'stats-bins
				  (let dump-stat ((b bins))
					 (if (not (null? b))
						  (cons (car b) (dump-stat (cdr b)))
						  b)
					 )
				  )
		  )
       ((equal? (car d) 'debug)
		  bins)
       ((number? (car d))
		  (dnl "Needs more than a number boikie!  Gimme a list corresponding to the bins I know about")
		  )

;  Need to be able to update the covariance stuff in this clause too
       ((list? (car d))
		  (set! d (car d))
;  Update data
		  (let load-stat ((b bins) (dd d))
			 (if (and (not (null? dd)) (not (null? b)))
				  (begin 
					 ((cdar b) (car dd)) ;; update individual bins
					 (load-stat (cdr b) (cdr dd)) )
				  #f))
		  )
       ((or (string? (car d)) (symbol? (car d)))
; else decode it and pass it on to univariate stat-filter
		  (let ((card (if (symbol? (car d)) (symbol->string (car d)) (car d))))
			 (if (member card multistats)
				  (let ((mstat (assoc (cadr d) covariants))
						  )
					 (if (not mstat)
						  #f
						  (let* ((keys (string-split (cadr d) #\*))
;			   (ords (fold (lambda (bin) (member (car bin) keys)) bins))
									(ords (map (lambda (k) (assoc k bins)) keys))
									(data (map (lambda (x) ((cdr x) 'data)) ords))
									(smeans (map (lambda (x) ((cdr x) 'mean)) ords))
									(means (map (lambda (x) (make-list (length (car data)) x)) smeans))
									(diffs (map (lambda (x y) (map - x y)) data means))
									(N (length (car diffs)))
									(cov (/ (apply + (apply map * diffs)) (- N sN)))
;			   (cov (/ (apply + (apply map * diffs)) N))

; (f-mean(f))(g-mean(g))'/N-1
									)
							 (cond
							  ((member card '("cov" "covariance"))
								cov)
							  ((member card '("cor" "correlation"))
;			   (/ (/ (* cov (- N sN)) N) (apply * (map (lambda (x) ((cdr x) 'variance)) ords)))
								(/  cov (apply * (map (lambda (x) (sqrt (abs ((cdr x) 'variance-1)))) ords)))
								)
							  (#t (dnl card " is not a recognised bivariate statistic.  \nI only know about " multistats)
									(exit 1)))

							 )
						  )
					 )
				  (let ()
					 (if (null? (cdr d))
						  (map (lambda (x) ((cdr x) (car d))) bins)
						  (if (equal? (length d) 2)
								(let ((b (assoc (cadr d) bins)))
								  (if b 
										((cdr b) (car d))
										(let ()
										  (dnl (cadr d) "is not an input column!")
										  (exit 1)
										  #f)))
								(map (lambda (x) 
										 (let ((b (assoc x bins)))
											(if b 
												 ((cdr b) (car d)) 
												 (let ()
													(dnl (cadr d) "is not an input column!")
													(exit 1)
													#f))))
									  (cdr d))
								)
						  )
					 )
				  )
			 )
		  )
		 (#t #f) )
      )
    )
  )

;======================================================================;
;==                      List handling routines                      ==;
;======================================================================;


(define (flense-alist alist)
  (cond
   ((null? alist) '())
   ((and (pair? alist) (pair? (car alist))) (cons (car alist) (flense-alist (cdr alist))))
   ((pair? alist) (flense-alist (cdr alist)))
   (#t '())))

; (list-ordinate '(a b c d)) ==> (0 1 2 3)
(define (list-ordinate x)
  (let list-ord-loop ((y x)(i 0))
    (if (null? y)
		  '()
		  (cons i (list-ord-loop (cdr y) (+ 1 i)))
		  )))

; (map cdr (map (lambda (x) (assoc x (map cons a-list (list-ordinate a-list)))) keys)) ;returns the ordinates of b d and f

(define (list-refs l keys)
  (if (null? l)
      #f
      (let list-ref-loop ((rsvp '())
								  (k keys))
		  
		  (if (not (null? k))
				(list-ref-loop (append rsvp (list (list-ref l (car k)))) (cdr k))
				rsvp)) ))


(define (flatten-list l)
  (if (and (not (null? l)) (list? l) (list? (car l)))
      (letrec ((al (car l))
					(dl (cdr l)))
		  (if (not (list? al)) (set! al (list al)))
		  (if (not (list? dl)) (set! dl (list dl)))
		  (append (flatten-list al) (flatten-list dl)))
      l))


(define (basic-atom? a)
  (or (number? a) (symbol? a) (char? a) (null? a) (string? a)))


(define (atom<? a b)
  (cond 
   ((and (number? a) (number? b)) (< a b))
   ((and (string? a) (string? b)) (string<? a b))
   ((and (symbol? a) (symbol? b)) (string<? (symbol->string a) (symbol->string b)))
   ((and (char? a) (char? b)) (char<? a b))
   ((and (null? a) (null? b)) #f)
   ((and (null? a) (not (null? b))) #t)
   ((and (not (null? a)) (null? b)) #f)
   (#t 'bad-atom-comparison)
   ))


(define (list-less? a b)
  (cond
   ((and (basic-atom? a) (basic-atom? b)) 
    (atom<? a b))
   ((not (and (list? a) (list? b))) 
    (if (list? b) #t #f))
   ((null? a) #t)
   ((null? b) #f)
   (#t
    (if (and (basic-atom? (car a)) (basic-atom? (car b)))
		  (if (atom<? (car a) (car b))
				#t
				(if (not (atom<? (car b) (car a)))
					 (list-less? (cdr a) (cdr b))
					 #f))
		  (if (list-less? (car a) (car b))
				#t
				(if (not (list-less? (car b) (car a)))
					 (list-less? (cdr a) (cdr b))
					 #f))
		  ))
   ))

(define (popstats-less? a b)
  (list-less? (car a) (car b)))

(define wt-stat-type (make-wt-tree-type list-less?))

(define (convert-wt-tree wt)
  (wt-tree/fold (lambda (key datum list)
						(cons (cons key datum) list))
					 '()
					 wt))

(define (convert-hash-table hashtab)
  (let hte-loop ((hte '())
					  (htix 0))
    (if (< htix hash-tsize)
		  (hte-loop (append (array-ref hashtab htix) hte) (+ htix 1))
		  hte)))



;======================================================================;
;==              Map strings to numbers when necessary               ==;
;======================================================================;



(define (fmodulo a b)
  (let ((fa (inexact->exact  (floor a)))
		  (fb (inexact->exact (floor b))))
    (if (not (equal? fb 0))
		  (modulo  fa fb)
		  0)))

(define (type-map names types line)
  (if (or (null? names) (null? types) (null? line) (not line) (not types) (not names) )
      '()
      (let ((tax (car types)))
		  (if (and (>= (string-length (car line)) 5) 
					  (string=? (gsubstring (car line) 
													(- (string-length (car line)) 5) 
													(string-length (car line))) "e-309"))
				(set-car! line "9e-308"))

		  (cond
			((string=? tax "T") 
			 (let ((a 0) (b 0))


				(let* ((cl (car line))
						 (SL (strtok (if (string? cl) cl "0") "+-"))
						 (s (length SL))
						 )
				  (if (> s 0) (set! a (string->number (car SL))))
				  (if (> s 1) (set! b (string->number (cadr SL))))

				  (if s 
						(append (list (+ a b)) (type-map (cdr names) (cdr types) (cdr line)))
						(append (list (dstring->number (car line))) (type-map (cdr names) (cdr types) (cdr line)))))))
			((or
			  ;; (string=? tax "N") -- not currently supported
			  (string=? tax "F")
			  (string=? tax "I")
			  )
			  (append (list (dstring->number (car line))) (type-map (cdr names) (cdr types) (cdr line))))
			((string=? tax "C") (append (list (car line)) (type-map (cdr names) (cdr types) (cdr line))))
			(#t (append (list (car line)) (type-map (cdr names) (cdr types)  (cdr line))) )
			))))



(define (Lno tag lno)
  (gc)
;  (display "\r ")
;  (display tag)
;  (display " Line ")
;  (display lno)
  )

(define (statspec s)
  (if (not (string-match "stat:[^:][^:]*:.*" s))
      #f
      (let ((l (cdr (string-split s #\:))))
		  (cons (string->symbol (car l)) (cdr l))
		  )))

;;;(define (time-to-date tag)
;;;  (let ((dstr #f))
;;;    (let ((ip (open-input-pipe 
;;;	       (string-append 
;;;		"time-to-date " 
;;;		epoch " " 
;;;		(number->string 
;;;		 (let ((c (map car key-map)))
;;;		   (list-ref tag (- (length c) 
;;;				    (length (member "time" c))))))))))
;;;      (set! dstr (read-line ip 'trim))
;;;      (close-port ip))
;;;    dstr))

(define (time-to-date tag)
  (let* ((c (map car key-map))
			(t (list-ref tag (- (length c) 
									  (length (member "time" c)))))
			(v (+ t unix-epoch))
			)
	 (strftime "%Y/%m/%d,%T" (localtime v))))

(define (sheltered-read-line inport)
  (let ((l (read-line inport))) (if (eof-object? l) "" l)))

(define (data-ref WT HASH ll)
  (if (and use-hash HASH)
      (hash-ref HASH ll)
      (wt-tree/lookup WT ll #f)))

(define (data-set! WT HASH ll vv)
  (if (and use-hash HASH)
      (hash-set! HASH ll vv)
      (let ((dr (data-ref WT HASH ll)))
;	(dnl "data-set!: before " ll ", " dr)
;	(if dr 
;	    (pp (list 'KEY: (dr 'key)))
;	    (pp 'NO-KEY))
		  (wt-tree/delete! WT ll)
		  (wt-tree/add! WT ll vv)
;	(dnl"data-set!: after " ll ", " (data-ref WT HASH ll))

;	(set! WT (wt-tree/add WT ll vv))
		  vv))
  )

;; PROCESSING STUFF --------------------------------------------------

(define (make-input-map tbl-column-name input-column-name)
  (let ((input-map '()))
    (set! input-map 
			 (map (lambda (x)
					  (letrec ((zed 
									(assoc x 
											 (map cons 
													(if (pair? tbl-column-name) tbl-column-name (list tbl-column-name))
													(if (pair? tbl-column-name) (list-ordinate tbl-column-name) (list 0)))
											 )
									))
						 zed)
					  )

					input-column-name
					)
			 )
    (set! input-map (flense-alist input-map))
    input-map))


(define (make-output-type-list output-colum-name input-map tbl-column-type)
  (let ((output-column-type '()))
    (let output-type-loop ((ocn output-column-name))
      (if (not (null? ocn))
			 (begin
				(set! output-column-type (append output-column-type 
															(list (if (assoc (car ocn) input-map) 
																		 (list-ref tbl-column-type (cdr (assoc (car ocn) input-map)))
																		 "F"))) )
				(output-type-loop (cdr ocn)) ) ))
    output-column-type)
  )

;; PROCESSING ASSOCIATED WITH INPUT TABLE FILE --------------------------------


(define (process-cmd-line args)
  (let do-next-arg ((arglist args) 
						  (collecting-infields #f)
						  (collecting-outfields #f)
						  )
    (cond 
     ((null? arglist)
      '())
     
     ((equal? (car arglist) "-warning")
      (dnl "******************************************")
      (dnl "*****    HARDCODED ARGUMENT LIST     *****")
      (dnl "******************************************")
      
      (do-next-arg (cdr arglist) #f #f)
      )
     
     ((equal? (car arglist) "-wttree")
      (set! hash-tsize -1)
      (set! use-hash #f)
      (do-next-arg (cddr arglist) #f #f)
      )
     
     ((equal? (car arglist) "-hash")
      (if (>= hash-tsize 0)
			 (begin
				(set! hash-tsize (dstring->number (cadr arglist)))
				(set! use-hash #t)))
      (do-next-arg (cddr arglist) #f #f)

      )
     
     ((equal? (car arglist) "-no-tbl")
      (set! tbl-output #f)
      (do-next-arg (cdr arglist) #f #f)
      )
     
     ((equal? (car arglist) "-tbl")
      (set! tbl-output #t)
      (do-next-arg (cdr arglist) #f #f)
      )
     
     ((equal? (car arglist) "-modtime")
      (set! time-rounding (string->number (cadr arglist)))
      (do-next-arg (cddr arglist) #f #f)
      )
     
     ((equal? (car arglist) "-epoch")
      (set! epoch (cadr arglist))
      (let ((v #f)
				(sl (string-length epoch))
				)
		  (set! v (strptime "%Y" epoch))
		  (if (< (cdr v) sl) (set! v (strptime "%Y/%m" epoch)))
		  (if (< (cdr v) sl) (set! v (strptime "%Y/%m/%d" epoch)))
		  (if (< (cdr v) sl) (set! v (strptime "%Y/%m/%d,%T" epoch)))
		  (if (< (cdr v) sl) 
				(begin
				  (dnl "The form of an epoch needs to be YYYY[/M[/D[,HH:MM:SS]]]")
				  (exit 1)))

		  (set! bd-epoch (car v))
		  )
      (set! unix-epoch (car (mktime bd-epoch)))

      (do-next-arg (cddr arglist) #f #f)
      )
     
     ((member (car arglist) '("-i" "-in"))
      (set! infilename (cadr arglist))
      (do-next-arg (cddr arglist) #t #f)
      )
     
     ((member (car arglist) '("-colon-ok"))
      (set! colon-ok #t))

     ((member (car arglist) '("-o" "-out"))
      (set! outfilename (cadr arglist))
      (if (and (not colon-ok) (> (length (string-split outfilename #\:)) 1))
			 (begin
				(dnl "It looks like the specified output filename '" outfilename "' is really part of")
				(dnl "a column spec. If you really want colons in the filename use '-colon-ok'")
				(exit 1))
			 (do-next-arg (cddr arglist) #f #t))
      )
     
     (collecting-infields
      (set! key-column-name (append key-column-name (list (car arglist))))
      (set! input-column-name (append input-column-name (list (car arglist))))
      (do-next-arg (cdr arglist) collecting-infields collecting-outfields)
      )
     
     (collecting-outfields ; need to parse the "x*y" bits of multivariate stats
      (let ((discarded #f))
		  (cond
			((member (car arglist) key-column-name)
			 (set! output-column-name (append output-column-name (list (car arglist))))
			 )
			((and time-rounding epoch (equal? (car arglist) "date"))
			 (set! output-column-name (append output-column-name (list (car arglist)))))

			((and (not (member (car arglist) input-column-name))
					(not (string-ci=? (gsubstring (car arglist) 0 5) "stat:")))

;	  (pp (list (car arglist) time-rounding epoch (equal? (car arglist) "date")))
			 (if (not (equal? (car arglist) "date"))
				  (let ()
					 (display "This is deprecated!  You are trying to include a non-aggregated value [")
					 (display (car arglist)) (display "] in the output file\n")))
			 (set! output-column-name (append output-column-name (list (car arglist))))
			 )

; Include the key in the file
			((string-ci=? (car arglist) "key")
			 (set! output-column-name (append output-column-name key-column-name))
			 (set! input-column-name (append input-column-name (list (car arglist)))) ; output columns 
			 )
			
			((string-ci=? (gsubstring (car arglist) 0 5) "stat:")
			 (set! output-column-name (append output-column-name (list (car arglist))))

			 (let ((fields (cdr (string-split (car arglist) #\:))))
				(if (member (car fields) multistats)
					 (let* ((MFIELDS (cdr fields))
							  (entry (cons (car MFIELDS)  (list (car fields) ))) ;; this is the "product", the stat in question, and the stats-bin
							  )                                                     ;;   the variables follow the stats-bin

						(let floop ((f MFIELDS))
						  (if (and (not (null? f)) (not (member (car f) input-column-name)))
								(let vloop ((vars (string-split (car f) #\*)))
								  (if (not (null? vars))
										(let ()
										  (set-cdr! entry (append (cdr entry) (list (car vars))))
										  (if (not (member (car vars) input-column-name))
												(set! input-column-name (append input-column-name (list (car vars)))))
										  (vloop (cdr vars))
										  )
										)
								  )
								)
						  (if (not (null? f)) (floop (cdr f))))
						(set! covariant-map (reverse (cons entry (reverse covariant-map))))
						)
					 (let ()
						(if (not (member (cadr fields) input-column-name))
							 (set! input-column-name (append input-column-name (list (cadr fields)))))
						)
					 )
				)
			 )
			(#t (dnl "Discarding " (car arglist) " since it is not an aggregate value")
				 (set! discarded #t)
				 )
			)
		  
		  (if (not discarded)
				(set! output-synonym (append output-synonym (list (cons (car arglist) (cadr arglist)))))
				)
		  (do-next-arg (cddr arglist) collecting-infields collecting-outfields)
		  )
      )

     ((or (equal? (car arglist) "-help") (equal? (car arglist) "--help") (equal? (car arglist) "-h"))
      (usage)
      (exit 0)
      )
     
     (else 
      (if (> (length arglist) 0)
			 (dnl "Bad option (" (car arglist) ")\n")
			 (display "Incomplete specification\n"))
      (usage) 
      (exit 1))
     )
    )

  ;; Sanity checks for some values
  (if (and use-hash (equal? hash-tsize 0))
      (set! hash-tsize 300017))
  )


(define (process-tbl-header inport)
  (set! line (filter-empty-strings (string-split (sheltered-read-line inport) #\space)))
  
  (if (equal? (car line) "TBL")
      (begin
		  (set! read-table-file #t)
;  Handle the initialisation of the TBL stuff
		  (set! tbl-column-name (filter-empty-strings (string-split (sheltered-read-line inport) #\space)))
		  (set! tbl-column-type (filter-empty-strings (string-split (sheltered-read-line inport) #\space)))

		  (set! line (filter-empty-strings (string-split (sheltered-read-line inport) #\space)))
		  )
      (begin
		  (set! read-table-file #f)
;  Set up the TBL stuff the *other* way -- based on the cmd line
		  (let ((ord (list-ordinate line)))
			 (let tbl-list-loop ((icn input-column-name)
										(orden ord))
				(if (not (null? orden))
					 (if (not (null? icn))
						  (begin
							 (set! tbl-column-name (append tbl-column-name (list (car icn))))
							 (set! tbl-column-type (append tbl-column-name (list "F")))
							 (tbl-list-loop (cdr icn) (cdr orden)))
						  (begin
							 (set! tbl-column-name (append tbl-column-name (list (car orden))))
							 (set! tbl-column-type (append tbl-column-name (list "F")))
							 (tbl-list-loop '() (cdr orden))))
					 )))
		  )
      )

  (set! tbl-map (map cons tbl-column-name (list-ordinate tbl-column-name)))
  )


;; Insert covariant 

; FILE PROCESSING ----------------------------------------------

(define (load-input-data inport)
  (let file-reading-loop ((input-line (type-map tbl-map tbl-column-type line))
								  (lno 1))
    (if (equal? (modulo lno 2000) 0)
		  (Lno "Input" lno))

    (if (and input-line (not (null? input-line)))
		  (let ((input-key (list-refs input-line (map cdr key-map)))
				  )

			 (if time-ix 
				  (begin
					 (list-set! 
					  input-key 
					  time-ix 
					  (- (inexact->exact (floor (list-ref input-key time-ix))) 
						  (fmodulo (list-ref input-key time-ix) time-rounding))))
				  )
;	  (dnl "input-key: " input-key)

			 (let ((stats (if input-key (data-ref StatWT StatHash input-key) #f)))
;	    (if stats
;		(dnl "stats: " (stats 'key))
;		(dnl 'NO-STATS))
				
				(if (and input-key (not (null? input-key)))
					 (begin
						(if (eqv? stats #f)
							 (set! stats (data-set! StatWT StatHash input-key (stats-bins input-key (list-copy input-map) (list-copy covariant-map))))
							 )
						
						(if (not stats) (exit 1))

						(stats (list-refs input-line (map cdr input-map)))
						)) )
			 ) )
    (set! line (read-line inport))
    (if (not (eof-object? line))
		  (file-reading-loop (type-map tbl-map tbl-column-type (filter-empty-strings (string-split line #\space))) (+ 1 lno))
		  (dnl "Final Input Line " lno " " inport) )
    )
  )

(define (dump-tbl-header outport)
  (if tbl-output
      (begin
		  (display "TBL\n" outport)
		  (let tbl-header-names-loop ((nlist output-column-name))
			 (if (not (null? nlist))
				  (begin
					 (if (and time-rounding (not epoch) (string-ci=? (car nlist) "date"))
						  (tbl-header-names-loop (cdr nlist))
						  (begin
							 (if (assoc (car nlist) output-synonym)
								  (display (cdr (assoc (car nlist) output-synonym)) outport)
								  (display (car nlist) outport))
							 (display " " outport)
							 (tbl-header-names-loop (cdr nlist)) )) )))

		  (newline outport) 
		  
		  (let tbl-header-types-loop ((nlist output-column-name)
												(tlist output-column-type))
			 (if (and (not (null? nlist)) (not (null? tlist)))
				  (begin
					 (cond
					  ((and time-rounding (not epoch) (string-ci=? (car nlist) "date"))
						(tbl-header-types-loop (cdr nlist) (cdr tlist)) 
						)
					  ((assoc (car nlist) input-map)
						(display (list-ref tbl-column-type (cdr (assoc (car nlist) input-map))) outport)
						(display " " outport)
						(tbl-header-types-loop (cdr nlist) (cdr tlist)) )		  
					  ((string-ci=? (gsubstring (car nlist) 0 5) "stat:")
						(display "F" outport)
						(display " " outport)
						(tbl-header-types-loop (cdr nlist) tlist) )		  
					  (else 
						(display "C" outport)
						(display " " outport)
						(tbl-header-types-loop (cdr nlist) (cdr tlist)) )
					  ))))
		  (newline outport) 
		  ))
  )

(define (process-output outport)
  (let output-loop ((remainder PopStats))
    (if (equal? (modulo lno 2000) 1)
		  (Lno "Output" lno))

    (set! lno (+ 1 lno))
    (if (pair? remainder)
		  (begin
			 (let* ((entry (car remainder))
					  (tag (car entry))
					  (stats (cdr entry)))

				(set! last-tag tag)
				(set! last-entry entry)
				(set! last-stats stats)

				(let field-loop ((ofield output-column-name))
				  (if (pair? ofield)
						(let ((sspec (statspec (car ofield))))
						  (cond
							((and time-rounding (string-ci=? (car ofield) "date"))
							 (if epoch 
								  (begin
									 (display (time-to-date tag) outport)
									 (display " " outport)))
							 )
							;; It is a member of the key
							((assoc (car ofield) key-map) 
							 (let ((c (map car key-map)))
								(display (list-ref tag (- (length c) (length (member (car ofield) c)))) outport))
							 (display " " outport)
							 )
							;; It is the whole key
							((string-ci=? (car ofield) "key") 
							 (let rest-of-key ((t tag))
								(if (null? t)
									 (display " " outport)
									 (begin
										(display (car t) outport)
										(if (>= (length t) 2) (display "-" outport))
										(rest-of-key (cdr t)))))
							 )
							(sspec
							 (let ((dv (stats (car sspec) (cadr sspec))))
								(if dv 
									 (display dv outport)
									 (display 'nan outport))
								(display " " outport)
								)
							 )
							(#t
							 (dnl (car ofield) "is not an aggregate statistic or part of the key.")
							 (exit 1)
							 ) 
							)
						  (field-loop (cdr ofield))
						  )
						)
				  )
				(newline outport))
			 (if (and (cdr remainder) (not (null? (cdr remainder))))
				  (output-loop (cdr remainder)))
			 )
		  )
    )

  (dnl "Final Output Line " lno) 
  )

(define (flense-maps)
  (set! input-map (flense-alist input-map))
  (set! tbl-map (flense-alist tbl-map))
  (set! key-map (flense-alist key-map))
  )

(define (dump-maps)
;  (pdnl "tbl-column-name" tbl-column-name)
;  (pdnl "tbl-map" tbl-map)
;  (pdnl "key-column-name" key-column-name)
;  (pdnl "key-map" key-map)
;  (pdnl "input-column-name" input-column-name)
;  (pdnl "input-map" input-map)
;  (pdnl "output-column-name" output-column-name)
;  (pdnl "output-synonym" output-synonym)
;  (pdnl "output-column-type" output-column-type)
  #t
  )

;;;;;;;;;;;;;; MAIN PROGRAM -------------------------------------------




(define (rsf)

  ;; Open I/O files
  (if (or (string=? infilename "-") (not infilename))
      (set! inport (current-input-port))
      (set! inport (open-input-file infilename)))
  
  (set! outport (open-output-file  outfilename))
  
  ;;

  (process-tbl-header inport)


;;; convert input map to an a-list of name.column pairs

  (set! input-map (make-input-map tbl-column-name input-column-name))
  (set! output-column-type (make-output-type-list output-column-name input-map tbl-column-type))

  ;; generate key-map
  (set! key-map 
		  (let loop ((km '())
						 (iom input-map))
			 (cond
			  ((null? iom) km)

			  ((and time-rounding (equal? (caar iom) "date")) ;; don't put date in if we are rounding time
				(loop km (cdr iom)))
			  ((member (caar iom) key-column-name) 
				(loop (append km (list (car iom))) (cdr iom)))
			  (else (loop km (cdr iom)))
			  )))

  ;; set up tbl data

  (flense-maps)

  (if (not read-table-file) (set! line (type-map tbl-map tbl-column-type line)))

  (dump-maps)

  ;; Now all the mapping data is set up, do the biz with the stats things .. urk.


;======================================================================;
;==               Read the tbl file and build the stats              ==;
;======================================================================;

  ;; Process input file......
  ;; the columns nominated in the input section correspond to the "identifier" against which the data value is aggregated

  (if use-hash
      (set! StatHash (make-hash-table hash-tsize))
      (set! StatWT (make-wt-tree wt-stat-type))
      )

  (if (and time-rounding (member "time" (map car key-map)))
      (set! time-ix (- (length (map car key-map)) 
							  (length (member "time" (map car key-map)))))
      (set! time-rounding #f))

  (if (and time-rounding time-ix)
      (dnl "Blocking at" time-rounding "intervals" (if epoch (string-append "from " epoch) "omitting date fields")))

  (load-input-data inport)
  (close-port inport)

; s and t are pairs -- the car of the pair is a list (the key to be sorted)
; 
  ;; PopStats is now correct but unsorted.  Write it out (unsorted) to a file in the correct
  ;; format, construct a command line to sort it, sort it, and rewrite it

  (if use-hash
      (set! PopStats (sort (flatten-list (convert-hash-table StatHash)) popstats-less?))
      (set! PopStats (sort (flatten-list (convert-wt-tree StatWT)) popstats-less?))
      )

; Output a tbl header if nedessary
  (dump-tbl-header outport)

  (process-output outport)
  (close-port outport)

;(newline)
  )

(define (run-stat-filter args)
  (reset-stat-filter args)
  (process-cmd-line args)
  (if (null? programname)
      (usage)
      (rsf)
      )
  )



;-  The End 


;;; Local Variables: ***
;;; mode: scheme ***
;;; outline-regexp: ";-+" ***
;;; comment-column:0 ***
;;; comment-start: ";;; "  ***
;;; comment-end:"***" ***
;;; End: ***
