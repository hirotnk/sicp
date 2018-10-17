(load "./hash-tbl")
(load "./compnum-common")

(define (deriv exp var)
    (cond
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else
            ;; pay attention to the arguments to actual functions
            ;; operators like '+ or '* are not passed in this case
            ;; this is different from original procedures in 2.3.2
            ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; auxiliary functions
(define (variable? x) (symbol? x))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (install-pair-deriv)
    ;; internal procedures
    ;; selectors
    (define (deriv-sum exp var)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var)))

    (define (deriv-product exp var)
        (make-sum
            (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
            (make-product (multiplicand exp)
                            (deriv (multiplier exp) var))))

    (define (addend s) (car s))
    (define (augend s)
        (cond
             ((null? (cdr s)) 0)
             ((= (length (cdr s)) 1) (cadr s))
             (else
                (cons '+ (cdr s)))))

    (define (multiplier p) (car p))
    (define (multiplicand p)
        (cond
             ((null? (cdr p)) 0)
             ((= (length (cdr p)) 1) (cadr p))
             (else
                (cons '* (cdr p)))))

    ;; constructors
    (define (make-sum a1 a2)
        (cond
            ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2))
                (+ a1 a2))
            (else (list '+ a1 a2))))

    (define (make-product m1 m2)
        (cond
            ((=number? m1 0) 0)
            ((=number? m2 0) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2))
                (* m1 m2))
            (else (list '* m1 m2))))

    ;; interface to the rest of the system
    (put 'deriv '+ (lambda (a b) (deriv-sum a b)))
    (put 'deriv '* (lambda (a b) (deriv-product a b)))
    'done)


;; tests:
;gosh> (load "./2-73")
;#t
;gosh> (install-pair-deriv)
;done
;gosh> (deriv '(* x y) 'x)
;y
;gosh> (deriv '(* x y y) 'x)
;(* y y)
;gosh> (deriv '(* x x y) 'x)
;(+ (* x y) (* x y))
;gosh> (deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* x y) (* (+ x 3) y))
;gosh> (deriv '(+ x y x) 'x)
;2
;gosh> (deriv '(+ x y z) 'x)
;1
;gosh> (deriv '(+ x y z) 'y)
;1
;gosh> (deriv '(+ x y z) 'z)
;1
;gosh> (deriv '(+ x y z) 'w)
;0
;gosh>
;
