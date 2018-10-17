(define (horner-eval x coeff-seq)
    (accumulate (lambda (this-coeff higher-terms)
                    (+ (* x higher-terms) this-coeff))
                    0
                    coeff-seq))

(define (accumulate f init l)
    (if (null? l) init
        (f (car l) (accumulate f init (cdr l)))))

;1,6,40,32=79

