(define (same-parity y . l)
    (cons y
    (if (even? y) (filter even? l)
        (filter odd? l))))

