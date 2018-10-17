(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

; ex 2.65
(define (union-set set1 set2)
    (let
        ((set-l1 (tree->list-1 set1))
         (set-l2 (tree->list-1 set2)))
        (let ((union-list (union-set-list set-l1 set-l2)))
                (list->tree union-list))))
(define (intersection-set set1 set2)
    (let
        ((set-l1 (tree->list-1 set1))
         (set-l2 (tree->list-1 set2)))
        (let ((intersection-list (intersection-set-list set-l1 set-l2)))
                (list->tree intersection-list))))

; ex 2.65
;gosh> (union-set (list->tree (list 1 2 3)) (list->tree  (list 4 5 6)))
;(3 (1 () (2 () ())) (5 (4 () ()) (6 () ())))
;gosh> (union-set (list->tree (list 1 2 3)) (list->tree  (list 4 5 6 8 9 10)))
;(5 (2 (1 () ()) (3 () (4 () ()))) (8 (6 () ()) (9 () (10 () ()))))
;gosh> (load "./2-63")
;#t
;gosh> (intersection-set (list->tree (list 1 2 3)) (list->tree  (list 4 5 6 8 9 10)))
;()
;gosh> (intersection-set (list->tree (list 1 2 3)) (list->tree  (list 1 4 5 6 8 9 10)))
;(1 () ())
;gosh> (intersection-set (list->tree (list 1 2 3 8 10)) (list->tree  (list 1 4 5 6 8 9 10)))
;(8 (1 () ()) (10 () ()))
;gosh>


(define (union-set-list set1 set2)
    (cond
        ((null? set1) set2)
        ((null? set2) set1)
        ((let ((x1 (car set1)) (x2 (car set2)))
            (cond
                ((= x1 x2) (cons x1 (union-set-list (cdr set1) (cdr set2))))
                ((< x1 x2) (cons x1 (union-set-list (cdr set1) set2)))
                ((< x2 x1) (cons x2 (union-set-list set1 (cdr set2)))))))))

(define (intersection-set-list set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond
                ((= x1 x2) (cons x1 (intersection-set-list (cdr set1) (cdr set2))))
                ((< x1 x2) (intersection-set-list (cdr set1) set2))
                ((< x2 x1) (intersection-set-list set1 (cdr set2)))))))

(define (element-of-set? x set)
    (cond
        ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
    (cond
        ((null? set) (make-tree x '() '()))
        ((< x (entry set))
            (make-tree (entry set)
                       (adjoin-set x (left-branch set))
                       (right-branch set)))
        ((> x (entry set))
            (make-tree (entry set)
                       (left-branch set)
                       (adjoin-set x (right-branch set))))
        ((= x (entry set)) set)))

(define (tree->list-1 tree)
    (if (null? tree) '()
        (append
            (tree->list-1 (left-branch tree))
            (cons (entry tree)
            (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree) result-list
            (copy-to-list
                (left-branch tree)
                (cons
                    (entry tree)
                    (copy-to-list
                        (right-branch tree)
                        result-list)))))
    (copy-to-list tree '()))


(define (list->tree elements)
    (car (partial-tree elements (length elements))))

(define (partial-tree elements n)
    (if (= n 0) (cons '() elements)
        (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elements left-size)))
                (let ((left-tree (car left-result))
                      (non-left-elts (cdr left-result))
                      (right-size (- n (+ left-size 1))))
                  (let ((this-entry (car non-left-elts))
                        (right-result (partial-tree (cdr non-left-elts) right-size)))
                     (let ((right-tree (car right-result))
                           (remaining-elts (cdr right-result)))
                         (cons (make-tree this-entry left-tree right-tree)
                               remaining-elts))))))))

;;
;;gosh> (quotient 4 1)
;;4
;;gosh> (load "./2-63")
;;#t
;;gosh> (list->tree (list 1 2 3 4 5 6 7))
;;(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ())))
;;gosh> (trace partial-tree)
;;#<closure (debug:trace-procedure debug:trace-procedure)>
;;gosh> (list->tree (list 1 2 3 4 5 6 7))
;;CALL partial-tree (1 2 3 4 5 6 7) 7
;;  CALL partial-tree (1 2 3 4 5 6 7) 3
;;    CALL partial-tree (1 2 3 4 5 6 7) 1
;;      CALL partial-tree (1 2 3 4 5 6 7) 0
;;      RETN partial-tree (() 1 2 3 4 5 6 7)
;;      CALL partial-tree (2 3 4 5 6 7) 0
;;      RETN partial-tree (() 2 3 4 5 6 7)
;;    RETN partial-tree ((...) 2 3 4 5 6 7)
;;    CALL partial-tree (3 4 5 6 7) 1
;;      CALL partial-tree (3 4 5 6 7) 0
;;      RETN partial-tree (() 3 4 5 6 7)
;;      CALL partial-tree (4 5 6 7) 0
;;      RETN partial-tree (() 4 5 6 7)
;;    RETN partial-tree ((3 ...) 4 5 6 7)
;;  RETN partial-tree ((2 ...) 4 5 6 7)
;;  CALL partial-tree (5 6 7) 3
;;    CALL partial-tree (5 6 7) 1
;;      CALL partial-tree (5 6 7) 0
;;      RETN partial-tree (() 5 6 7)
;;      CALL partial-tree (6 7) 0
;;      RETN partial-tree (() 6 7)
;;    RETN partial-tree ((5 () ()) 6 7)
;;    CALL partial-tree (7) 1
;;      CALL partial-tree (7) 0
;;      RETN partial-tree (() 7)
;;      CALL partial-tree () 0
;;      RETN partial-tree (())
;;    RETN partial-tree ((7 () ()))
;;  RETN partial-tree ((6 (5 () ()) (7 () ())))
;;RETN partial-tree ((4 (2 (1 ...) (3 () ...)) (6 (5 ...) (7 () ()))))
;;(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ())))
;;gosh>
;;(list->tree (list 1 2 3))
;;CALL partial-tree (1 2 3) 3
;;  CALL partial-tree (1 2 3) 1
;;    CALL partial-tree (1 2 3) 0
;;    RETN partial-tree (() 1 2 3)
;;    CALL partial-tree (2 3) 0
;;    RETN partial-tree (() 2 3)
;;  RETN partial-tree ((1 () ()) 2 3)
;;  CALL partial-tree (3) 1
;;    CALL partial-tree (3) 0
;;    RETN partial-tree (() 3)
;;    CALL partial-tree () 0
;;    RETN partial-tree (())
;;  RETN partial-tree ((3 () ()))
;;RETN partial-tree ((2 (1 () ()) (3 () ())))
;;(2 (1 () ()) (3 () ()))
;;gosh> (list->tree (list 1 2))
;;CALL partial-tree (1 2) 2
;;  CALL partial-tree (1 2) 0
;;  RETN partial-tree (() 1 2)
;;  CALL partial-tree (2) 1
;;    CALL partial-tree (2) 0
;;    RETN partial-tree (() 2)
;;    CALL partial-tree () 0
;;    RETN partial-tree (())
;;  RETN partial-tree ((2 () ()))
;;RETN partial-tree ((1 () (2 () ())))
;;(1 () (2 () ()))
;;gosh>
;;
