(define (loop? lst)
    (define ptable (make-hash-table 'eq?))
    (define (loop_in? x)
        (cond
            ((pair? x)
                (if (hash-table-get ptable x #f)
                    #t
                    (begin
                        (hash-table-put! ptable x #t)
                        (loop_in? (cdr x)))))
            (else #f)))
    (loop_in? lst))

(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x)

(define z (make-cycle (list 'a 'b 'c)))

;; tests
;gosh> (load "./3-18")
;#t
;gosh> (loop? z)
;#t
;gosh> z
;#0=(a b c . #0#)
;gosh> (loop? (list 1 2 3))
;#f
;gosh> (define x (list 'c))
;x
;gosh> x
;(c)
;gosh> (define y (cons x x))
;y
;gosh> (loop? y)
;#f
;
