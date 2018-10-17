(load "./fold")
(load "./prime")

(define (permutations s)
    (if (null? s) (list '())
        (flatmap (lambda (x)
                    (map (lambda (p) (cons x p))
                        (permutations (remove x s))))
                 s)))

(define (flatmap proc seq)
    (fold-right append '() (map proc seq)))

(define (remove x s)
    (if (= x (car s)) (cdr s)
        (cons (car s) (remove x (cdr s)))))

(define (prime-sum-pairs n)
    (map make-pair-sum (filter prime-sum? (mytest n))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (mytest n)
        (fold-right append
                    '()
                    (map (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                         (enumerate-interval 1 n))))

(define (enumerate-interval start end)
    (if (> start end) '()
        (cons start (enumerate-interval (+ start 1) end))))

(define (enumerate-interval2 start end)
    (define (iter acc i)
        (if (= i start) (cons i acc)
            (iter (cons i acc) (- i 1))))
    (iter '() end))

                    
