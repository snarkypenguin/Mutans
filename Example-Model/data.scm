;; Uses list-stuff.scm

 ;;
 ;; return a list of maps of lists
 ;;

;(define (list-map function . lists)
;  (map function lists))

 (define (pair-list? l)
   (cond
    ((null? l) #t)
    ((multi-list? l) #f)
    ((double-list? l) #f)
    (#t (and (pair? (car l)) (pair-list? (cdr l))))))

 (define (double-list? l)
   (cond
    ((null? l) #t)
    (#t (and (list? (car l)) (equal? (length (car l)) 2) (double-list? (cdr l))))))

 (define (multi-list? l)
   (if (null? l)
       #t
       (and (list? (car l)) (>= (length (car l)) 2) (double-list? (cdr l)))))


 (define (data<? m n)
   (cond
    ((symbol? m) (string<? (symbol->string m) (symbol->string n)))
    ((string? m) (string<? m n))
    ((char? m) (char<? m n))
    (#t (< m n))))

 (define (basic-atom? a)
   (or (number? a) (symbol? a) (char? a) (null? a) (string? a)))


 (define (atom<? a b)
   (cond 
    ((and (number? a) (number? b)) (< a b))
    ((and (string? a) (string? b)) (string<? a b))
    ((and (symbol? a) (symbol? b)) (string<? (symbol->string a) (symbol->string b)))
    ((and (char? a) (char? b)) (char<? a b))
    ((and (null? a) (not (null? b))) #t)
    ((and (not (null? a)) (null? b)) #f)
    (#t #t)
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

 (define (list-head<? m n)
   (list-less? (car a) (car b)))


 ;;
 ;; The following routines perform the indicated transformations respectively
 ;;
 ;; ((a b) (c d) (a f) ... ) ->  ((a . b) (c . d) (a . f))
 ;; ((a . b) (c . d) (a . f)) -> ((a b) (c d) (a f) ... )
 ;; ((a b) (c d) (a f) ... ) ->  ((a b f ...) (c d ...))
 ;; ((a b f ...) (c d ...)) -> ((a b) (c d) (a f) ... )
 ;; ((a . b) (c . d) (a . f)) -> ((b . a) (d . c) (f . a))
 ;;

 (define (double-list->pair-list l)
   (if (null? l) 
       '()
       (cons (cons (caar l) (cadar l)) (double-list->pair-list (cdr l)))))

 (define (pair-list->double-list l)
   (if (null? l) 
       '()
       (cons (list (caar l) (cdar l)) (pair-list->double-list (cdr l)))))

 (define (double-list->multi-list l)
   (define (only key l) 
     (cond
      ((null? l) '())
      ((equal? (caar l) key)
       (cons (car l) (only key (cdr l))))
      (#t (only key (cdr l)))))

;  (map car l) is a list of all heads
   (map 
    (lambda (x)
      ((lambda (lh)
         (append (list (caar lh)) (apply append (map cdr lh)))) (only x l)))
    (uniq (sort (map car l) data<?)) ) )

 (define (multi-list->double-list l)
   (if (null? l) '()
       (if (null? (cdar l)) (multi-list->double-list (cdr l))
           (cons (cons (caar l) (cons (cadar l) '()))
                 (multi-list->double-list (cons (cons (caar l) (cddar l)) (cdr l)))))))



 (define (reverse-pairs l)
   (if (null? l) '()
       (cons (cons (cdar l) (caar l)) (reverse-pairs (cdr l)))))


 (define (indices n)
   (if (<= n 0) 
       '()
       (append (indices (- n 1)) (list (- n 1)))))

 ;;
 ;; deletes columns of data
 ;;

 (define (delete-columns data deletion-list)
   (map (lambda (elements)
          (members elements (set-difference (indices (length elements)) deletion-list)))
        data))

 ;;
 ;; appends a column of data 
 ;;

 (define (append-column data column)
   (map append 
        (if (and (pair? data) (pair? (car data)))
            data
            (map list data))
        (if (and (pair? column) (pair? (car column)))
            column
            (map list column))))


 ;;
 ;; perform a relational type join where key? and out? are lists of 
 ;; list-ref indices except that a negative index means "and all the rest from here"
 ;;


 (define (join list1 list2 key1 key2 out1 out2)
   (define (subjoin2 l1 l2)
     (if (null? l2)
         '()
         (if (not (equal? (members l1 key1) (members (car l2) key2)))
             (subjoin2 l1 (cdr l2))
             (cons (append (members l1 out1) (members (car l2) out2)) 
                   (subjoin2 l1 (cdr l2))))))

   (define (subjoin1 l1 l2)
     (if (null? l1)
         '()
         (letrec ((bit (subjoin2 (car l1) l2)))
           (if (null? bit)
               (subjoin1 (cdr l1) l2)
               (cons (car bit) (subjoin1 (cdr l1) l2))))))

   (subjoin1 list1 list2))


 ;;
 ;; add-ordinate  adds the ordinates to a list (makes an a-list)
 ;; 
 ;;

 (define (add-ordinates value-list first . args) ; (start . (end step)) or (t-list) or (a-list)
   (cond
    ((and (list? first) (pair? (car first)))
     (let list-loop ((values value-list)(t-list first))
       (if (or (null? t-list) (null? values))
           '()
           (cons (cons (caar t-list) (car values)) (list-loop (cdr values) (cdr t-list))))))
    ((list? first)
     (let list-loop ((values value-list) (t-list first))
       (if (or (null? t-list) (null? values))
           '()
           (cons (cons (car t-list) (car values)) (list-loop (cdr values) (cdr t-list))))))
    (#t
     (let ((end first) (step 1))
       (if (> (length args) 0)
           (set! end (list-ref args 0)))
       (if (> (length args) 1)
           (set! step (list-ref args 1)))

       (let iterate-loop ((values value-list) (start first))
         (if (or (> start end) (null? values))
             '()
             (cons (cons start (car values)) (iterate-loop (cdr values) (+ start step)))))))))


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


 ;;  
 ;; make unix command equivalent
 ;;

;*;	  (define (unix-command command)
;*;	    (lambda args
;*;	      (let ((p (open-input-pipe 
;*;			(string-append command
;*;				       (apply string-append 
;*;					      (map (lambda (x)
;*;						     (string-append " " x))
;*;						   args))))))
;*;		(let loop ((i (read-line p)) (l '()))
;*;		  (if (eof-object? i)
;*;		      (let ()
;*;			(close-port p)
;*;			l)
;*;		      (loop (read-line p) (append l (list i))))))))
;*;
;*;	  (define (string-unix-command command)
;*;	    (lambda args
;*;	      (let ((p (open-input-pipe 
;*;			(string-append command
;*;				       (apply string-append 
;*;					      (map (lambda (x)
;*;						     (string-append " " x))
;*;						   args))))))
;*;		(let loop ((i (read-line p)) (l ""))
;*;		  (if (eof-object? i)
;*;		      (let ()
;*;			(close-port p)
;*;			l)
;*;		      (loop (read-line p) (string-append l i)))))))
;*;
;*;
;*;	  (define hostname (string-unix-command "uname -n"))
;*;	  (define cwd (string-unix-command "pwd"))
;*;	  (define pwd cwd)
;*;	  (define mkdir (unix-command "mkdir -p"))
;*;	  (define uname (unix-command "uname"))
;*;
;*;	  ;;
;*;	  ;; return a list of filenames
;*;	  ;;
;*;	  (define directory (unix-command (string-append "ls -A ")))
;*;
;*;	  ;;
;*;	  ;; Pipe commands
;*;	  ;;
;*;
;*;	  (define close-input-pipe close-port)
;*;	  (define close-output-pipe close-port)
;*;
;*;	  (define (call-with-input-pipe str proc)
;*;	    (let* ((pipe (open-input-pipe str))
;*;		   (ans (proc pipe)))
;*;	      (close-input-port pipe)
;*;	      ans))
;*;
;*;	  (define (call-with-output-pipe str proc)
;*;	    (let* ((pipe (open-output-pipe str))
;*;		   (ans (proc pipe)))
;*;	      (close-output-port pipe)
;*;	      ans))
;*;
;*;	  (define (with-input-from-pipe pipe thunk)
;*;	    (let* ((nport (open-input-pipe pipe))
;*;		   (Ans (with-input-from-port nport thunk)))
;*;	      (close-port nport)
;*;	      ans))
;*;
;*;	  (define (with-output-to-pipe pipe thunk)
;*;	    (let* ((nport (open-output-pipe pipe))
;*;		   (ans (with-output-to-port nport thunk)))
;*;	      (close-port nport)
;*;	      ans))
;*;
;*;	  (define (with-error-to-pipe pipe thunk)
;*;	    (let* ((nport (open-output-pipe pipe))
;*;		   (ans (with-error-to-port nport thunk)))
;*;	      (close-port nport)
;*;	      ans))
;*;
;*;	  ;;
;*;	  ;; example
;*;	  ;;          (define data (load-list-from-pipe "rusers ocean"))
;*;	  ;;          (map (lambda (x) (list-ref x 4)) data) 
;*;	  ;;
;*;	  ;; returns the 4th in each element
;*;	  ;;
;*;
;*;
;*;	  (define (load-list-from-pipe filename)
;*;	    (with-input-from-pipe (string-append "echo '(';" filename "; echo ')'")
;*;				  (lambda () 
;*;				    (read))))
;*;
;*;	  ;;
;*;	  ;; example
;*;	  ;;          (define data (load-list-from-file "/home/gray/model/output/ate.ascii"))
;*;	  ;;          (map (lambda (x) (list-ref x 4)) data) 
;*;	  ;;
;*;	  ;; returns the 4th in each element
;*;	  ;;
;*;
;*;




