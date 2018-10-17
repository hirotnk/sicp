(define (make-monitored f)
    (let
        ([cnt 0])
        (lambda (arg)
            (if (eq? arg 'how-many-calls?) cnt
                (begin 
                    (set! cnt (+ cnt 1))
                    (f arg))))))

