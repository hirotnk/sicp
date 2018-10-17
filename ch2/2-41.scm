(load "./nested-map")

(define (tri-sum n s)
    (flatmap
        (lambda(i)
            (flatmap
                (lambda(j)
                    (fold-left
                        (lambda(l k) (if (eq? s (+ i j k)) (cons (list i j k) l) l))
                        '()
                        (enumerate-interval 1 (- j 1))))
                 (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))

