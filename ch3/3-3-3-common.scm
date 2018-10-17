(define (make-table)
    (list '*table*))

(define (assoc key records)
    (cond
        ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
