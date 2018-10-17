(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(define (f-iter n)
  (f-iter-in 2 1 0 2 n))
(define (f-iter-in a b c k n)
  (cond ((< n 3) n)
        ((= k n) a)
        (else (f-iter-in (+ a (* 2 b) (* 3 c)) a b (+ k 1) n))))


(f 1)
(f 2)
(f 3)
(f 4)
(f 5)

(f-iter 1)
(f-iter 2)
(f-iter 3)
(f-iter 4)
(f-iter 5)
