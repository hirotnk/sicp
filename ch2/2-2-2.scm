(define (count-leave t)
    (cond
        ((equal? '() t) 0)
        ((not (pair? t)) 1)
        (else
            (+ (count-leave (car t)) (count-leave (cdr t))))))

