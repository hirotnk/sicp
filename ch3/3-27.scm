;;This is a snippet to check how it works.
;;It's not entirely clear to me how it works though, especially when memoize
;;is called recursibely it's making a table. Isn't it inefficient?

(load "./3-3-3-1")

(define (memoize f)
    (let ((table (make-table)))
        (lambda (x)
            (let ((previously-computed-result
                    (lookup x table)))
                (or previously-computed-result
                    (let ((result (f x)))
                            (insert! x result table)
                            result))))))
(define mfib
    (memoize
        (lambda (n)
            (cond
                ((= n 0) 0)
                ((= n 1) 1)
                (else (+ (mfib (- n 1))
                         (mfib (- n 2))))))))
(define (fib n)
    (cond
        ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

