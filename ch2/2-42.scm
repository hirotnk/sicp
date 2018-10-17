(load "./nestedloop")
;; -- first forget everything and write your own n-queen
;; At first, I tried to generate all possible combinations, which includes wrong solutions.
;; Then I added filter. This approach seems simpler for me.

;; my first solution
(define (nqueen boardsize)
    (define (nqueen-in k)
        (if (= k 0) '(())
            (flatmap
                (lambda (queens-sofar)
                    (fold-right
                        (lambda (row acc)
                            (if (safe? queens-sofar row k)
                                 (cons (append queens-sofar (list (list row k))) acc) acc))
                        '()
                        (enumerate 1 boardsize)))
                (nqueen-in (- k 1)))))
    (nqueen-in boardsize))

(define (safe? queens-sofar row col)
    (let ((qpos (list row col)))
        (if (eq? queens-sofar '()) #t
            (if
                (and (safe-holizontal? qpos (car queens-sofar))
                     (safe-diagonal? qpos (car queens-sofar)))
                        (safe? (cdr queens-sofar) (car qpos) (cadr qpos))
                        #f))))

(define (safe-holizontal? p q)
    (if (= (row? p) (row? q)) #f #t))

(define (safe-diagonal? p q)
    (if (= (abs (- (row? p) (row? q)))
           (abs (- (col? p) (col? q)))) #f #t))

(define (row? pos)
    (car pos))

(define (col? pos)
    (cadr pos))


;; ex 2-42
(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0) empty-board
            (filter
                (lambda (positions) (safe2? k positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (map (lambda (new-row)
                                (adjoin-position
                                 new-row k rest-of-queens))
                             (enumerate 1 board-size)))
                    (queen-cols (- k 1))))))
    (queen-cols board-size))

(define empty-board '(()))

(define (adjoin-position row col rest-of-queens)
    (append rest-of-queens (list (list row col))))

(define (safe2? k positions)
    (let ((pos (nth k positions))
          (queens-sofar (remove-nth k positions)))
    (safe? queens-sofar (car pos) (cadr pos))))

(define (nth k l)
    (define (nth-in cur l)
        (if (= k cur) (car l)
            (nth-in (+ cur 1) (cdr l))))
    (nth-in 1 l))

(define (remove-nth x s)
    (define (remove-in cur s)
        (if (eq? s '()) '()
            (if (= cur x) (remove-in (+ cur 1) (cdr s))
                    (cons (car s) (remove-in (+ cur 1) (cdr s))))))
    (remove-in 1 s))

