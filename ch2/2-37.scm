(load "./2-36")

(define (dot-product v w)
    (accumulate-n + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
    (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (x) (matrix-*-vector cols x)) m)))


