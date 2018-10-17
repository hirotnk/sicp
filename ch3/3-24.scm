(define (assoc pred key records)
    (cond
        ((null? records) #f)
        ((pred key (caar records)) (car records))
        (else (assoc pred key (cdr records)))))

(define (make-table cmp)
    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable
                   (assoc cmp key-1 (cdr local-table))))
                 (if subtable
                    (let ((record
                            (assoc cmp key-2 (cdr subtable))))
                         (if record (cdr record) #f))
                    #f)))
            (define (insert! key-1 key-2 value)
                (let ((subtable
                        (assoc cmp key-1 (cdr local-table))))
                     (if subtable
                        (let ((record
                               (assoc cmp key-2 (cdr subtable))))
                             (if record
                                 (set-cdr! record value)
                                 (set-cdr! subtable
                                            (cons (cons key-2 value)
                                                  (cdr subtable)))))
                        (set-cdr! local-table
                                    (cons (list key-1 (cons key-2 value))
                                          (cdr local-table)))))
                'ok)
            (define (dispatch m)
                (cond ((eq? m 'lookup-proc) lookup)
                      ((eq? m 'insert-proc!) insert!)
                      (else (error "unknown operation: TABLE" m))))
            dispatch))

(define (same-key? lhs rhs)
    (cond
        ((and (number? rhs) (number? lhs) (= lhs rhs))
        (else (eq? lhs rhs))))
(define operation-table (make-table same-key?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


