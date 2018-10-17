(define (fold-right op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (fold-right op initial (cdr sequence)))))

(define (rreverse sequence)
    (fold-right (lambda (x rest)
                    (display x) (display rest) (newline)
                    (append rest (list x))) '() sequence))

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest) result
            (iter (op result (car rest)) (cdr rest))))
    (iter initial sequence))

(define (lreverse sequence)
    (fold-left (lambda (acc y)
                    (display acc) (display y) (newline)
                    (cons y acc)) '() sequence))
