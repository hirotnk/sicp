(load "./nestedloop")

(define (tri-sum n s)
  (filter
    (lambda(triple)
      (if
        (= s (+ (car triple) (cadr triple) (caddr triple))) #t #f))
    (tri-pair n)))

(define (tri-pair n)
  (flatmap
    (lambda(i)
      (flatmap
        (lambda(j)
          (map
            (lambda(k) (list i j k))
            (enumerate 1 (- j 1))))
        (enumerate 1 (- i 1))))
    (enumerate 1 n)))

      
