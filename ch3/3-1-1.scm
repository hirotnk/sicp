(define balance 100)
(define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
                balance)
        "Insufficient balance"))

(define (make-withdraw balance)
    (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                    balance)
        "Insufficient balance")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

