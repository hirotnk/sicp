(define (memq item x)
    (cond
        ((null? x) #f)
        ((eq? item (car x)) x)
        (else
            (memq item (cdr x)))))

(define (equal? a b)
    (cond
        ((eq? a b) #t)
        ((and (pair? a) (pair? b))
            (and
                (equal? (car a) (car b))
                (equal? (cdr a) (cdr b))))
        (else #f)))

