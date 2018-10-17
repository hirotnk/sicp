(define compnum-tbl (make-hash-table 'equal?))

(define (get op type)
    (hash-table-get compnum-tbl (cons op type)))

(define (put op type item)
    (hash-table-put! compnum-tbl (cons op type) item))


