(define (square-list items)
    (if (null-list? items)
        '()
        (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-map items)
    (map (lambda (x) (* x x)) items))

(define (square-list-iter items)
    (define (square-list-iter-in items acc)
        (if (null? items)  acc
            (square-list-iter-in (cdr items) (append acc (list (* (car items) (car items)))))))
    (square-list-iter-in items '()))

