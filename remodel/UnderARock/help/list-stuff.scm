;;(include "/etc/scheme.d/implementation-macros.scm")

(define (dnl . args) (if (null? args) (display "") (let () (map display args) (newline))))
(define (DNL . args) (if debugging (apply dnl args)))

(define list-ref
  (letrec ((%list-ref list-ref))
    (lambda (l i)
      (if (or (< i 0) (>= i (length l)))
          (abort 'list-ref-index-out-of-bounds)
          (%list-ref l i)))))

(define (list-set! l i v)
   (if (zero? i)
       (set-car! l v)
       (list-set! (cdr l) (- i 1) v)))



 (define list-ref
   (letrec ((%list-ref list-ref)
            )
     (lambda (l i)
       (cond
        ((integer? i) (%list-ref l i))
        ((list? i) (let loop ((rtn '()) (ii i))
                     (if (null? ii)
                         (if (null? rtn) #f (reverse rtn))
                         (loop (cons (%list-ref l (car ii)) rtn) (cdr ii)))))
        ))))

 (define list-set!
   (letrec ((%list-set! list-set!)
            )
     (lambda (l i v)
       (if (list? i)
           (for-each (lambda (x y) (%list-set! l x y)) i v)
           (%list-set! l i v)))))


;;;; There is no correspoinding "list-set!" for this ... Thinking about it is way too hard :->
;;(define list-ref
;;  (letrec ((%list-ref list-ref))
;;    (letrec ((lr (lambda x
;;                   (let ((n (length x)))
;;                     (cond
;;                      ((< n 2) (abort 'list-ref:need-more-args))
;;                      ((= n 2) (apply %list-ref x))
;;                      (#t
;;                       (map (lambda (y) (apply lr (cons y (cddr x))))
;;                            (apply lr (list (car x) (cadr x)))
;;                           ))
;;                      ) )) ))
;; lr)))


 (define list-set-car! list-set!)

 (define (list-set-cdr! l i v)
   (if (zero? i)
       (set-cdr! l v)
       (list-set-cdr! (cdr l) (- i 1) v)))
 ;; set the value associated with key in a-list
 ;; (assoc-set! list key value)

  ;;
  ;; return the first k elements of a list (analogous to list-tail)
  ;;
  (define (list-head the-list k)
    (if (and (> k 0) (not (null? the-list)))
        (cons (car the-list) (list-head (cdr the-list) (- k 1)))
        '()))

 (define (assoc-set! alist key val)
   (let loop ((l alist))
     (if (null? l) 
         (append alist (cons (cons key val) '()))
         (if (equal? (caar l) key)
             (set-cdr! (car l) val)
             (loop (cdr l))))))


 (define (assoc-append alist key value)
   (if (or (null? alist) (not (assoc key alist)))
       (acons key (list value) alist)
       (map (lambda (x)
              (if (and (pair? x) (equal? (car x) key)) 
                  (cons (car x) (append (cdr x) (list value)))
                  x))
            alist
            ) 
       ))

 (define (assoc-delete alist key)
   (reverse (let loop ((a alist)(r '()))
              (if (null? a)
                  r
                  (if (and (pair? a) (pair? (car a)) (equal? (caar a) key))
                      (loop (cdr a) r)
                      (loop (cdr a) (cons (car a) r)))))))


 (define (assoc-only alist select )
   (reverse 
    (let loop ((a alist) (r '()))
      (cond
       ((or (not a) (null? a))
        r)
       ((not (pair? a))
        r)
       ((and (pair? a) (pair? (car a)))
        (if (select (car a))
            (loop (cdr a) (cons (car a) r))
            (loop (cdr a) r)))
       (else 
        (loop (cdr a) r)))
      )
    )
   )

 ;;
 ;; returns a list of members in a list where keylist is a list of 
 ;; list-ref indices except that a negative index means "and all the rest from here"
 ;;

 (define (members element keylist)
   (map (lambda (x) 
          (if (< x 0)
              (list-tail element (- 0 x))
              (list-ref element x)))
        keylist))

 (define (not-members element keylist)
   (set-difference element 
                   (map (lambda (x) 
                          (if (< x 0)
                              (list-tail element (- 0 x))
                              (list-ref element x)))
                        keylist)))


 ;; removes completely null strands

 (define (denull l)
   (cond
    ((null? l) '())
    ((not (pair? l)) l)
    (else
     (let ((a (denull (car l)))
           (d (denull (cdr l))))
       (cond
        ((and (null? a) (null? d)) '())
        ((null? a) d)
        (else (cons a d)))))))

 (define (denull-and-flatten l)
   (cond
    ((null? l) '())
    ((not (pair? l)) l)
    (else
     (let ((a (denull (car l)))
           (d (denull (cdr l))))
       (cond
        ((and (null? a) (null? d)) '())
        ((null? a) d)
        ((null? d) a)
        (else (cons a d)))))))



 (define (level the-list n )
   (denull 
    (let loop ((tl the-list) (d 0))
      (if (not (pair? tl))
          (if (eq? d n)
              (list tl)
              '())
          (append (loop (car tl) (+ d 1)) (loop (cdr tl) d))))))


;; guarded car

 (define (gcar x)
   (if (pair? x) (car x) #f))

 ;; guarded cdr

 (define (gcdr x)
   (if (pair? x) (cdr x) #f))

 ;; guarded caar

 (define (gcaar x)
   (if (and (pair? x) (pair? (car x))) (caar x) #f))

 ;; guarded cadr

 (define (gcadr x)
   (if (and (pair? x) (pair? (cdr x))) (cadr x) #f))

 ;; guarded cdar

 (define (gcdar x)
   (if (and (pair? x) (pair? (car x))) (cdar x) #f))

 ;; guarded cddr

 (define (gcddr x)
   (if (and (pair? x) (pair? (cdr x))) (cddr x) #f))

 ;; guarded list-head

 (define (glist-head the-list k)
   (if (and (> k 0) (not (null? the-list)))
       (cons (car the-list) (glist-head (cdr the-list) (- k 1)))
       '()))

 ;; guarded list-tail

 (define (glist-tail the-list k)
   (if (and (> k 0) (not (null? the-list)))
       (glist-tail (cdr the-list) (- k 1))
       the-list))

 ;; guarded last

 (define (glast the-list k)
   (glist-tail the-list (- (length the-list) k)))


 ;; (depth the-list) returns the maximum depth of the list

 (define (depth l)
   (let loop ((tl l) (d 0))
     (if (not (pair? tl))
         d
         (max (loop (car tl) (+ d 1)) (loop (cdr tl) d)))))


