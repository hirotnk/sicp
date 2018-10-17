;; ex 3.3 and 3.4
(define (make-account passwd balance)
    (define is_fraud_counter 0)
    (define (withdraw amount)
        (cond
            ((> amount balance)
                "Insufficient balance")
            (else
                (begin (set! balance (- balance amount))
                        balance))))
    (define (deposit amount)
        (begin
        (set! balance (+ balance amount))
        balance))
    (define call-the-cops
        (lambda (amount) ; ignore amount
            "Calling 911.."))

    ;; dispatch takes 2 parameters, and
    ;; returns a function that takes 1 parameter
    (define (dispatch p m)
        (cond
            ((not (eq? p passwd))
                (begin
                 (set! is_fraud_counter (+ is_fraud_counter 1))
                 (if (> is_fraud_counter 7) call-the-cops
                     (lambda (m) "Wrong pass"))))
            ((eq? m 'withdraw) 
                (begin
                    (set! is_fraud_counter 0)
                    withdraw))
            ((eq? m 'deposit)
                (begin
                    (set! is_fraud_counter 0)
                    deposit))
            (else
                (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch)

;; tests
;gosh> (load "./3-3")
;#t
;gosh> (define acc (make-account 'mypass 100))
;acc
;gosh> ((acc 'mypass 'withdraw) 100)
;0
;gosh> ((acc 'mypass1 'withdraw) 100)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 100)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 100)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 100)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 100)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 100)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 100)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 100)
;"Calling 911.."
;gosh> ((acc 'mypass 'withdraw) 100)
;"Insufficient balance"
;gosh> ((acc 'mypass 'deposit) 100)
;100
;gosh> ((acc 'mypass 'withdraw) 10)
;90
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass 'withdraw) 10)
;80
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass 'withdraw) 10)
;70
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Wrong pass"
;gosh> ((acc 'mypass1 'withdraw) 10)
;"Calling 911.."
;gosh>

