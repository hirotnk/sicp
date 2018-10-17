(define (fringe l)
    (cond
        ((null? l) l)
        ((not (pair? l)) (cons l '()))
        (else
            (append (fringe (car l)) (fringe (cdr l))))))

;;It looks to me that part of this exercise is to recognize the effect of cons.
;;At first I tried to use cons instead of 'append', but since I expect the return
;;value of 'fringe' is always a list, I realized I can not use cons.

;gosh> (cons 1 2)
;(1 . 2)
;gosh> (cons 1 '())
;(1)
;gosh> (cons (list 1) '())
;((1))
;gosh> (cons 1 (list 2))
;(1 2)
;gosh> (cons (list 1) (list 2))
;((1) 2)

