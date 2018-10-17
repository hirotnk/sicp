;to start gauche repl: rlwrap gosh
;to load source      :gosh> (load "./1-11")
;the result:
;  gosh> (sum-integers 1 10)
;  55
;  gosh> (sum-cubes 1 10)
;  3025
;  gosh> (integral cube 0 1 0.01)
;  0.24998750000000042
;  gosh> (integral cube 0 1 0.001)
;  0.24999987500000073
;  gosh> (integral cube 0 1 0.0001)
;  0.24999999874993337
;  gosh> (integral cube 0 1 0.000001)
;  0.2500000000014414
;  gosh>


;1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


(define (cube a)
  (* a a a))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

;(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

(define (pi-sum a b) ;; ~ PI/8
  (sum pi-term a pi-next b))

(define (pi-term x)
  (/ 1.0 (* x (+ x 2))))
(define (pi-next x)
  (+ x 4))
  
(* 8 (pi-sum 1 9000)) ;; ~ PI

(define (integral f a b dx)
  (define (add-dx x)
  (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)
  
