(define (last-pair a)
    (if (null? (cdr a)) a
        (last-pair (cdr a))))

(define (my-reverse a)
    (if (null? a) a
        (append (my-reverse (cdr a)) (list (car a)))))




