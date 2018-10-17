(define (accumulate f init l)
    (if (null? l) init
        (f (car l) (accumulate f init (cdr l)))))

(define (mymap p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
