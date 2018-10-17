(define (subsets s)
    (if (null? s) (list '())
        (let
            ((rest (subsets (cdr s))))
            (append rest (map (lambda(e) (cons (car s) e)) rest)))))

;; The pivot of this code is the base case that returns '(()), not '(). After
;; realizing it, the structure is simple, you need current set, and you need to
;; add a new element to all elements in a current set. Since you need to keep
;; current set too, 'append would do.
;gosh> (print (subsets (list 3)))
;(() (3 ()))
;#<undef>
;gosh> (print (subsets (list 2 3)))
;(() (3 ()) (2 ()) (2 (3 ())))
;#<undef>
;gosh> (print (subsets (list 1 2 3)))
;(() (3 ()) (2 ()) (2 (3 ())) (1 ()) (1 (3 ())) (1 (2 ())) (1 (2 (3 ()))))
;#<undef>
;gosh> (print (subsets '()))
;(())
;#<undef>

