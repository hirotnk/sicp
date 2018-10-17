(define (printq q)
    (cond
        ((empty-queue? q) (print "EMPTY"))
        (else (print (front-ptr q)))))


