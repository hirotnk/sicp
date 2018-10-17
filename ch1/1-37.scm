;gosh> (load "./1-37")
;#t
;gosh> (cont-frac-ite (lambda (i) 1.0) (lambda (i) 1.0) 11)
;0.6180555555555556
;gosh> (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
;0.6180555555555556
;gosh>

(define (cont-frac n d k)
  (define (cont-frac-in n d a b)
    (let ((next-n (n a))
          (next-d (d a)))
          (if (= a b) (/ next-n next-d)
              (/ next-n (+ next-d (cont-frac-in n d (+ a 1) b))))))
  (cont-frac-in n d 1 k))

(define (cont-frac-ite n d k)
  (define (ite n d b result)
      (if (= b 1) (/ (n 1) (+ (d 1) result))
          (ite n d (- b 1) (/ (n b) (+ (d b) result)))))
  (ite n d k 0.0))
    

