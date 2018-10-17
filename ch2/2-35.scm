(define (accumulate f init l)
    (if (null? l) init
        (f (car l) (accumulate f init (cdr l)))))

(define (fringe l)
    (cond     ((null? l) l)
            ((not (pair? l)) (list l))
            (else (append (fringe (car l)) (fringe (cdr l))))))

(define (count-leaves t)
    ;in case of t is '(), map returns '(), and accumulate returns 0
    (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

(define (count-leaves2 t)
    (length (fringe t)))
