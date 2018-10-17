;1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (* result (term a)))))
  (iter a 1))


(define (factorial a)
  (define (inc n) (+ n 1))
  (define (identiry n) n)
  (define (factorial-in a b)
    (product identity b inc a))
  (factorial-in a 1))


(define (qpi max)
  (define (inc2 a) (+ a 2))
  (define (create-rational a)
    (* (/ (- a 1) a) (/ (+ a 1) a)))
  (product  create-rational 3 inc2 (- max 1)))

