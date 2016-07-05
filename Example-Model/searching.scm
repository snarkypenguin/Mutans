(define (dnl . args) (if (null? args) (display "") (let () (map display args) (newline))))
(define (DNL . args) (if debugging (apply dnl args)))


 ;;
 ;; Searching
 ;;


 ;; Depth first search
 (define (dfs node target nodemap carnode cdrnode)
   (if (null? node)
       #f
       (if (equal? (nodemap node) target)
           node
           (if (pair? node)
               (let ((l (dfs (carnode node) target nodemap carnode cdrnode)))
                 (if l
                     l
                     (dfs (cdrnode node) target nodemap carnode cdrnode)))
               #f))))


 (define (dfs-path node target nodemap carnode cdrnode)
   (if (or (null? node) (not node))
       #f
       (if (equal? (nodemap node) target)
           (list node)
           (if (not (pair? node))
               #f
               (letrec ((l (dfs-path (carnode node) target nodemap carnode cdrnode)))
                 (if l
                     (if (pair? l) 
                         (cons (car l) (cons 'car (cdr l)))
                         (cons l 'car))

                     (letrec ((r (dfs-path (cdrnode node) target nodemap carnode cdrnode)))
                       (if r
                           (if (pair? r)
                               (cons (car r) (cons 'cdr (cdr r)))
                               (cons r 'cdr))
                           #f))))
               ))))


 ;; breadth first search
 (define (bfs-list key list-of-lists)
   (let bsof ((lol (list-copy list-of-lists)))
     (if (or (not (pair? lol)) (null? lol))
         #f
         (if (equal? key (car lol))
             (car lol)
             (let ()
               (if (and (list? (car lol)) (list? lol))
                   (bsof (append (cdr lol) (car lol))) ; strip off a level of nesting
                   (bsof (cdr lol)) 
                   ) 
               )
             )
         )
     )
   )


 (define (bfs key list-of-lists unwrapper)
   (let ((uw #f))
     
     (if (not (procedure? unwrapper))
         (set! uw (lambda (x) x))
         (set! uw (lambda (x) (unwrapper x)))
         )
     
     (if (null? list-of-lists)
         #f
         (let bsof ((lol (list-copy list-of-lists)))
           (if (or (not (pair? lol)) (null? lol))
               #f
               (if (pair? (car lol)) 
                   (if (and (pair? lol) (equal? key (uw (car lol))))
                       (car lol)
                       (let ()
                         (if (and (list? (car lol)) (list? lol))
                             (bsof (append (cdr lol) (car lol))) ; strip off a level of nesting
                             (bsof (cdr lol)) 
                             ) 
                         )
                       )
                   (bsof (cdr lol)))
               )
           )
         )
     )
   )




 (define (bfs-old node target nodemap carnode cdrnode)
   (if (or (not node) (null? node))
       #f
       (or 
        (let loop ((n node))
          (if (or (not n) (null? n)) 
              #f
              (if (equal? target (nodemap n))
                  n
                  (loop (cdrnode n)))))
        (bfs (apply append (map (lambda (x) (if (pair? x) x '())) node))
             target nodemap carnode cdrnode))))

 (define (bfs-path node target nodemap carnode cdrnode)
   (if #f #t))

