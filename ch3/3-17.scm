(define (orig-count-pairs x)
    (cond
        ((not (pair? x)) 0)
        (else (+ (orig-count-pairs (car x))
                 (orig-count-pairs (cdr x))
                 1))))

(define (count-pairs x)
    (define ptable (make-hash-table 'eq?))
    (define (cp obj)
            (if (not (hash-table-get ptable obj #f))
                (if (pair? obj)
                    (begin
                        (hash-table-put! ptable obj #t)
                        (+ 1 (cp (car obj)) (cp (cdr obj)))
                     )
                     0)
                0))
        (cp x))

;;tests
;gosh> (define x (cons 'c '()))
;x
;gosh> x
;(c)
;gosh> (define y (cons x x))
;y
;gosh> y
;(#0=(c) . #0#)
;gosh> (define z (cons y y))
;z
;gosh>
;z
;(#0=(#1=(c) . #1#) . #0#)
;gosh> (orig-count-pairs z)
;7
;gosh> (orig-count-pairs y)
;3
;gosh> (define w 'a y)
;*** ERROR: Compile Error: syntax-error: (define w 'a y)
;"(stdin)":46:(define w 'a y)
;
;Stack Trace:
;_______________________________________
;gosh> (define w (cons 'a y))
;w
;gosh> (orig-count-pairs w)
;4
;gosh> (orig-count-pairs z)
;7
;gosh> (count-pairs w)
;3
;gosh> (count-pairs z)
;3
;gosh>
;
