;; Representing table (key/value)
(load "./3-3-3-common")

(define (lookup key table)
    (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            #f)))
(define (insert! key value table)
    (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table
                        (cons (cons key value)
                              (cdr table)))))
        'ok)
;; tests
;gosh> (load "./3-3-3")
;#t
;gosh> (define t1 make-table)
;t1
;gosh> (lookup 'k1 1 t1)
;*** ERROR: wrong number of arguments for #<closure lookup> (required 2, got 3)
;Stack Trace:
;_______________________________________
;gosh> make-table
;#<closure make-table>
;gosh> (make-table)
;(*table*)
;gosh> (define t1 (make-table))
;t1
;gosh> (lookup 'k1 t1)
;#f
;gosh> (insert! 'k1 1 t1)
;ok
;gosh> (lookup 'k1 t1)
;1
;gosh> (insert! 'k2 2 t1)
;ok
;gosh> (lookup 'k1 t1)
;1
;gosh> (lookup 'k2 t1)
;2

