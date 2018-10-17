(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (filpped-pairs painter)
    (let ((painter2 (beside painter (flip-vert painter))))
        (below painter2 painter2)))

(define wave5 (flipped-pairs wave))

(define (right-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
            (beside painter (below smaller smaller)))))

(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((up-half (up-split painter (- n 1))))
            (below painter (beside up-half up-half)))))


;; my implementation
(define (corner-split painter n)
    (if (= n 0)
        painter
        (beside
            (below (beside (up-split (- n 1)) (up-split (- n 1)))  painter)
            (below (corder-split painter (- n 1)) (below (right-split (- n 1)) (right-split (- n 1)))))))

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split (- n 1)))
              (right (right-split (- n 1))))
              (let ((top-left (beside up up))
                    (bottom-right (below right right))
                    (corner (corner-split painter (- n 1))))
                    (beside (below painter top-left)
                            (below bottom-right corner))))))

;; my implementation
(define (square-limit painter n)
    (if (= n 0)
        painter
        (let ((quarter (corner-split (- n 1))))
            (beside (below (flip-holiz (flip-vert quarter)) (flip-holiz quarter))
                    (below (flip-vert quarter) quarter)))))

(define (square-limit painter n)
    (if (= n 0)
        painter
        (let ((quarter (corner-split (- n 1))))
            (let ((half (beside (flip-holiz quarter) quarter)))
                (below (flip-vert harf) half)))))

