(define (prime a)
    (define (iter x acc)
        (cond
            ((> x a) acc)
            ((prime? x) (iter (+ 2 x) (cons x acc)))
            (else (iter (+ 2 x) acc))))
    (iter 3 (list 2)))

(define (prime? a)
    (define (iter x)
        (cond
            ((> (* 2 x) a) #t)
            ((eq? (mod a x) 0) #f)
            (else (iter (+ x 2)))))
    (cond
        ((eq? (mod a 2) 0) #f)
        (else (iter 3))))

(define (prime-sum a b)
    (define (iter x acc)
        (cond
            ((> x b) acc)
            ((prime? x) (iter (+ x 1) (+ x acc)))
            (else (iter (+ x 1) acc))))
    (iter a 0))

(define (prime-sum2 a b)
    (fold + 0 (filter prime? (enumerate-interval a b))))

(define (enumerate-interval a b)
    (if (> a b) '() (cons a (enumerate-interval (+ a 1) b))))

