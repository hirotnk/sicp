(define (deep-reverse l acc)
    (cond
      ((null? l) acc)
      ((pair? (car l))
        (deep-reverse (cdr l) (cons (deep-reverse (car l) '()) acc)))
      (else (deep-reverse (cdr l) (cons (car l) acc)))))

