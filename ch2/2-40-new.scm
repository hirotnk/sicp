(include "./nestedloop")
; 1 <= j < i <= n
(define (unique-pairs n)
    (flatmap
        (lambda(i)
            (map
                (lambda(j) (list i j))
                (enumerate 1 (- i 1))))
        (enumerate 1 n)))

(define (prime-sum-pairs n)
    (filter 
        (lambda(pair)
          (if
            (prime? (+ (car pair) (cadr pair))) #t #f))
        (unique-pairs n)))


