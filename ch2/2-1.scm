(define (numer x) (car x))

(define (denom x) (cdr x))

(define (make-rat-norm n d)
    (let ((g (gcd n d)))
        (make-rat-wrap (/ n g) (/ d g))))

(define (make-rat n d)
    (cond     ((and (< n 0) (< d 0))
                (make-rat (- n) (- d)))
            ((< n 0)
                (let ((rat (make-rat-norm (- n) d)))
                    (cons (- (numer rat)) (denom rat))))
            ((< d 0)
                (let ((rat (make-rat-norm n (- d))))
                    (cons (- (numer rat)) (denom rat))))
            (else
                (make-rat-norm n d))))

(define (add-rat x y)
    (make-rat
        (+ (* (numer x) (denom y))
           (* (numer y) (denom x)))
        (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat
        (- (* (numer x) (denom y))
           (* (numer y) (denom x)))
        (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat
        (* (numer x) (numer y))
        (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat
        (* (numer x) (denom y))
        (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom x))
       (* (numer y) (denom y))))

(define (gcd x y)
    (if (> y x) (gcd y x)
        (if (= y 0) x
            (gcd y (modulo x y)))))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x))
    (newline))

