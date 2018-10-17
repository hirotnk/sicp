(include "3-5-1")
(include "3-5-2")

;; This one converges faster
;(define (random-update x) (sys-random))

;http://dl.acm.org/citation.cfm?doid=63039.63042
(define (random-update x)
  (modulo
    (* x 16807)
    2147483647))

(define random-numbers
  (cons-stream
    (random-update 17)
    (stream-map (lambda(x) (random-update x)) random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f (stream-cdr (stream-cdr s)))))
(define cesaro-stream
  (map-successive-pairs
    (lambda (r1 r2) (= (gcd r1 r2) 1))
    random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if
    (stream-car experiment-stream)
      (next (+ 1 passed) failed)
      (next passed (+ 1 failed))))
(define pi
  (stream-map
    (lambda(p) (sqrt (/ 6 p)))
    (monte-carlo cesaro-stream 0 0 )))

; Execution
;gosh> (include "./3-5-5")
;pi
;gosh> (stream-ref pi 100)
;3.1780497164141406
;gosh> (stream-ref pi 1000)
;3.190558104305445
;gosh> (stream-ref pi 3000)
;3.147108264588764
;gosh>

