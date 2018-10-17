(define (power-set s)
    (if (null? s) (list '())
        (let ((subsets (power-set (cdr s))))
                (newline)
                (display subsets)
                (newline)
                (append (map (lambda(x)
                                (cond ((eq? x '()) (list (car s)))
                                      ((pair? x) (cons (car s) x))
                                      ((else (cons (car s) (list x))))))
                            subsets) subsets))))

;gosh> (let ((a (power-set (list 1 2 3)))) (newline) (display a) (newline) #t)
;
;(())
;
;((3) ())
;
;((2 3) (2) (3) ())
;
;((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ())
;#t
;gosh>
;
