(define (count-leaves tree n)
  (cond
    ((null? tree) n)
    ((not (pair? tree)) (+ n 1))
    (else
      (count-leaves
          (car tree)
          (count-leaves (cdr tree) n)))))

(define (test-count)
  (count-leaves
    '((1 2) ((3 4) 5 6)) 0))



(define (append x y)
  (cond
    ((null? x) y
    (else
      (cons
        (car x)
        (append
          (cdr x) y))))))

          
