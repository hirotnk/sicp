;; Rectangtangular representation
;; selectors
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
    (sqrt (+
            (square (real-part z))
            (square (imag-part z)))))

(define (angle z)
    (atan (imag-part z) (real-part z)))

;; constructors
(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
    (cons (* r (cons a)) (* r (sin a))))

;; Polar representation
;; selectors
(define (real-part z)
    (* (magnitude z) (cos (angle z))))

(define (imag-part z)
    (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))
(define (angle z) (cdr z))

;; constructors
(define (make-from-real-imag x y)
    (cons
        (sqrt (+ (square x) (square y)))
        (atan y x)))


(define (attach-tag type-tag contents)
    (cons type-tag contents))

(define (type-tag datum)
    (if
        (pair? datum)
        (car datum)
        (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
    (if
        (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
    (eq? (type-tag z) 'rectangular))
(define (polar? z)
    (eq? (type-tag z) 'polar))


(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangualr z)
    (sqrt (+
            (square (real-part-rectangular z))
            (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
    (atan
        (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-rectangular x y)
    (attach-tag 'rectangular (cons x y)))
(define (make-from-real-imag-rectangular r a)
    (attach-tag 'rectangular
                (cons (* r (cos a)) (* r (sin a)))))


(define (real-part z)
    (cond ((rectangular? z)
            (real-part-rectangular (contents z)))
          ((polar? z)
            (real-part-polar (contents z)))
          (else (error "Unknown type: REAL-PART" z))))
(define (imag-part z)
    (cond ((rectangular? z)
            (imag-part-rectangular (contents z)))
          ((polar? z)
            (imag-part-polar (contents z)))
          (else (error "Unknown type: IMAG-PART" z))))
(define (angle z)
    (cond ((rectangular? z)
            (angle-rectangular (contents z)))
          ((polar? z)
            (angle-polar (contents z)))
          (else (error "Unknown type: ANGLE" z))))

