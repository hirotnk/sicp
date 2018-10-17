(define (loop? lst)
    (define (loop_in? x y)
        (cond
            ((or (not (pair? x)) (not (pair? y))) #f)
            ((eq? x y) #t)
            (else
                (loop_in? (cdr x) (cddr y)))))
    (loop_in? lst (cdr lst)))

;; tests
;gosh> (define x (list 1 2 3 4 5))
;x
;gosh> (cddddr x)
;(5)
;gosh> (set-cdr! (cddddr x) x)
;#<undef>
;gosh> (loop? x)
;#t
;gosh> (define y (list 1 2 3 4 5))
;y
;gosh> (cddr y)
;(3 4 5)
;gosh> (set-cdr! (cddddr y) (cddr y))
;#<undef>
;gosh> y
;(1 2 . #0=(3 4 5 . #0#))
;gosh> (cddr y)
;#0=(3 4 5 . #0#)
;gosh> (loop? y)
;#t
