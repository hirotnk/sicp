(define (deriv exp var)
    (cond
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
            (make-sum
                (make-product (multiplier exp)
                                (deriv (multiplicand exp) var))
                (make-product (multiplicand exp)
                                (deriv (multiplier exp) var))))
        ((exponentiation? exp)
            (make-product
                (make-product (exponentiation exp)
                    (make-exponentiation
                        (base exp) (make-sum (exponentiation exp) -1)))
                (deriv (base exp) var)))
        (else
            (error "unknown expression :" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
    (cond
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
            (+ a1 a2))
        ;(else (list '+ a1 a2))))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
    (cond
        ((=number? m1 0) 0)
        ((=number? m2 0) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
            (* m1 m2))
        ;(else (list '* m1 m2))))
        (else (list m1 '* m2))))

(define (make-exponentiation b e)
    (cond
        ((and (number? e) (= e 0)) 1)
        ((and (number? e) (= e 1)) b)
        ((and (number? b) (= b 1)) 1)
        ((and (number? b) (number? e)) (expt b e))
        (else
            ;(list '** b e))))
            (list b '** e))))

(define (=number? exp num) (and (number? exp) (= exp num)))


;(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

;(define (addend s) (cadr s))
(define (addend s) (car s))
(define (augend s)
    (if (null? (cddr s)) 0
        ;(cons '+ (cddr s))))
        (caddr s)))

;(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

;(define (multiplier p) (cadr p))
(define (multiplier p) (car p))
(define (multiplicand p)
    (if (null? (cddr p)) 1
        ;(cons '* (cddr p))))
        (caddr p)))

(define (exponentiation? e)
    ;(and (pair? e) (eq? (car e) '**)))
    (and (pair? e) (eq? (cadr e) '**)))
;(define (base e) (cadr e))
(define (base e) (car e))
;(define (exponentiation e) (caddr e))
(define (exponentiation e) (caddr e))


