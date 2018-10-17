(include "./2-36")

(define (enumerate start end)
  (if
    (> start end) '()
    (cons start (enumerate (+ start 1) end))))

(define (pair-gen n)
  (accumulate append '()
    (map
      (lambda(i)
        (map
          (lambda(j) (list j i))
          (enumerate 1 (- i 1))))
      (enumerate 1 n))))

(define (flatmap proc seq)
  (accumulate append '()
    (map proc seq)))

(define (prime? a)
    (define (iter x)
        (cond
            ((> (* 2 x) a) #t)
            ((eq? (mod a x) 0) #f)
            (else (iter (+ x 2)))))
    (cond
        ((eq? (mod a 2) 0) #f)
        (else (iter 3))))

(define (prime-sum-pairs n)
  (accumulate
    (lambda(pair rest)
      (if
        (prime? (+ (car pair) (cadr pair))) (cons pair rest)
        rest))
    '()
    (flatmap
      (lambda(i)
        (map
          (lambda(j) (list j i))
          (enumerate 1 (- i 1))))
      (enumerate 1 n))))


