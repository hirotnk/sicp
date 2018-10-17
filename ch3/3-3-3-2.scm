;; Representing table (two dimentional table)
(load "./3-3-3-common")

(define (lookup key-1 key-2 table)
    (let ((subtable
            (assoc key-1 (cdr table))))
         (if subtable
            (let ((record
                    (assoc key-2 (cdr subtable))))
               (if record
                    (cdr record)
                    #f))
            #f)))
(define (insert! key-1 key-2 value table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! subtable
                              (cons (cons key-2 value)
                                    (cdr subtable)))))
            (set-cdr! table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr table)))))
    'ok)

;; tests
;gosh> (load "./3-3-3-2")
;#t
;gosh> (define t1 (make-table))
;t1
;gosh> (insert! 'letter 'a 97 t1)
;ok
;gosh> (lookup 'letter 'a t1)
;97
;gosh> (insert! 'math '+ 42 t1)
;ok
;gosh> (lookup 'math '+ t1)
;42
;gosh>
;
