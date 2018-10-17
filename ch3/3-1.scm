(define (make-accumulator value)
    (lambda (addend)
        (set! value (+ value addend))))

