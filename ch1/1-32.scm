(define (filtered-accumulate filter combiner null-value term a next b)
  (if (filter a)
    (if (> a b) null-value
      (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b)))
    (filtered-accumulate filter combiner null-value term (next a) next b)))

(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (accumulate-iter-in combiner term a next b acc) 
    (if (> a b) acc
      (accumulate-iter-in combiner term (next a) next b (combiner (term a) acc))))
  (accumulate-iter-in combiner term a next b null-value))

(define (identity x) x)
(define (inc x) (+ x 1))

