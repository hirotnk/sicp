(define (append x y)
    (if (eq? x '())
        y
        (cons (car x) (append (cdr x) y))))

(define (append! x y)
    (set-cdr! (last-pair x) y)
    x)

(define (last-pair x)
    (if (null? (cdr x)) x (last-pair (cdr x))))

