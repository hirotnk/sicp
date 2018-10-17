;gosh> (load "./1-38")
;#t
;gosh> (cont-frac (lambda(i) 1.0) seq 14)
;0.7182818284454013
;gosh> (cont-frac (lambda(i) 1.0) seq 13)
;0.7182818287356957
;gosh> (cont-frac (lambda(i) 1.0) seq 12)
;0.7182818229439497
;gosh> (cont-frac (lambda(i) 1.0) seq 10)
;0.7182817182817183
;gosh> (cont-frac (lambda(i) 1.0) seq 9)
;0.7182835820895522


(define (seq i)
    (if (= (modulo (+ i 1) 3) 0)
            (* (/ (+ i 1) 3) 2)
        1.0))

(define (cont-frac n d k)
  (define (cont-frac-in n d a)
    (if (= a k) (/ (n a) (d a))
        (/(n a) (+ (d a) (cont-frac-in n d (+ a 1))))))
  (cont-frac-in n d 1))

(define (cont-frac-ite n d k)
  (define (ite n d b result)
      (if (= b 1) (/ (n 1) (+ (d 1) result))
          (ite n d (- b 1) (/ (n b) (+ (d b) result)))))
  (ite n d k 0.0))
    

