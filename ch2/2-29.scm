(define (make-mobile left right)
    (list left right))
(define (make-branch length structure)
    (list length structure))
(define (left-branch mobile)
    (car mobile))
(define (right-branch mobile)
    (cadr mobile))
(define (branch-length branch)
    (car branch))
(define (branch-structure branch)
    (cadr branch))
(define (is-branch? structure)
    (if (number? (car structure)) #t #f))

(define (total-weight st)
    (cond
        ((is-branch? st)
            (if
                (number? (branch-structure st))
                    (branch-structure st)
                    (total-weight (branch-structure st))))
        (else
            (+
                (total-weight (left-branch st))
                (total-weight (right-branch st))))))

(define (torque branch)
   (*
       (branch-length branch)
       (total-weight branch)))

(define (is-balanced-branch? branch)
    (cond
        ((number? (branch-structure branch)) #t)
        (else
            (is-balanced? (branch-structure branch)))))
    
(define (is-balanced? mobile)
    (and
        (=
            (torque (left-branch mobile))
            (torque (right-branch mobile)))
        (is-balanced-branch? (left-branch mobile))
        (is-balanced-branch? (right-branch mobile))))

;gosh> (include "./2-29")
;is-balanced?
;gosh>  (define m4 (make-mobile (make-branch 2 6) (make-branch 4 3)))
;m4
;gosh> (define m3 (make-mobile (make-branch 1 m4) (make-branch 1 9)))
;m3
;gosh> (define m2 (make-mobile (make-branch 1 27) (make-branch 1 27)))
;m2
;gosh> (define m1 (make-mobile (make-branch 3 m3) (make-branch 2 m2)))
;m1
;gosh> (is-balanced? m1)
;#f
;gosh> (define m2 (make-mobile (make-branch 1 9) (make-branch 1 9)))
;m2
;gosh> (define m1 (make-mobile (make-branch 3 m3) (make-branch 2 m2)))
;m1
;gosh> (is-balanced? m1)
;#f
;gosh> (define m1 (make-mobile (make-branch 3 m3) (make-branch 3 m2)))
;m1
;gosh> (is-balanced? m1)
;#t
;
;; when we need to change the underlining data structures, we only need to
;; change the constructors and selectors

