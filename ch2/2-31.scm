(define (scale-tree tree factor)
    (cond     ((null? tree) tree)
            ((not (pair? tree)) (* tree factor))
            (else (cons (scale-tree (car tree) factor)
                          (scale-tree (cdr tree) factor)))))

(define (tree-map f tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree) (tree-map f sub-tree)
                (f sub-tree)))
        tree))


