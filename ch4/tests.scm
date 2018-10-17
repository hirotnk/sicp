(define (perm l)
  (if
    (null? l) '(())
    (let
      ((subset (perm (cdr l))))
      (append subset (map (lambda (a) (cons (car l) a)) subset)))))


