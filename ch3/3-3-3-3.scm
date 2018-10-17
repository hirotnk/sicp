;; Creating local tables
(load "./3-3-3-common")
(define (make-table)
    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable
                   (assoc key-1 (cdr local-table))))
                 (if subtable
                    (let ((record
                            (assoc key-2 (cdr subtable))))
                         (if record (cdr record) #f))
                    #f)))
            (define (insert! key-1 key-2 value)
                (let ((subtable
                        (assoc key-1 (cdr local-table))))
                     (if subtable
                        (let ((record
                               (assoc key-2 (cdr subtable))))
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

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


;; test
;gosh> (load "./3-3-3-3")
;#t
;gosh> (define t1 operation-table)
;t1
;gosh> (get 'letter 'a)
;#f
;gosh> (put 'letter 'a 97)
;ok
;gosh> (get 'letter 'a)
;97
;gosh>

