(load "./3-1-2")
(use math.mt-random)

; gauche specific
(define mt (make <mersenne-twister> :seed (sys-time)))

(define (estimate-integral pred upper_x lower_x upper_y lower_y trials)
    (*
        (/
            (monte-carlo
                trials
                (lambda ()
                    (pred
                        (random-in-range lower_x upper_x)
                        (random-in-range lower_y upper_y)))
                        )
            trials) (area upper_x lower_x upper_y lower_y)))

(define (area ux lx uy ly)
    (* (abs (- ux lx)) (abs (- uy ly))))

;; I did not find a better way, but there should be an appropriate function
;; in gauche library
(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (* (/ (mt-random-integer mt (* 1000 range)) 1000.0)))))

(define (pred-example x y)
    (<=
        (+ (square (- x 5))
           (square (- y 7)))
        (square 3)))

(define (pred-unit x y)
    (<=
        (+ (square x)
           (square y))
        1))

(define (square x) (* x x))

;; test
;;
;;gosh> (+ (estimate-integral pred-unit 1 -1 1 -1 1000) 0.0)
;;0.003212
;;gosh> (+ (estimate-integral pred-unit 1 -1 1 -1 5000) 0.0)
;;6.2576e-4
;;gosh> (+ (estimate-integral pred-unit 1 -1 1 -1 8000) 0.0)
;;3.955625e-4
;;gosh> (+ (estimate-integral pred-unit 1 -1 1 -1 9000) 0.0)
;;3.5101234567901237e-4
;;gosh> (+ (estimate-integral pred-unit 1 -1 1 -1 10000) 0.0)
;;3.1508e-4
;;gosh> (+ (estimate-integral pred-unit 1 -1 1 -1 100000) 0.0)
;;3.1348e-5
;;gosh> (+ (estimate-integral pred-unit 1 -1 1 -1 1000000) 0.0)
;;3.141004e-6
;;gosh>
;;
