(define (estimate-pi trials)
    (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
    (= (gcd (sys-random) (sys-random)) 1))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond
            ((= trials-remaining 0)
                (/ trials-passed trials))
            ((experiment)
                (iter (- trials-remaining 1)
                      (+ trials-passed 1)))
            (else
                (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))


;
; My first attempt to simulate π
;
;(define (estimate-pi trials)
;    (/ 1.0 (sqrt (/ (/ (monte-carlo trials) (+ 0.0 trials)) 6.0))))
;
;
;(define (monte-carlo trials)
;    (cond
;        ((= 0 trials) 0)
;        ((if
;            (= 1 (gcd (sys-random) (sys-random))) (+ 1 (monte-carlo (- trials 1)))
;            (monte-carlo (- trials 1))))
;        (else (error "Unknown case" trials))))
;; test
;;
;;gosh> (load "./3-1-2")
;;#t
;;gosh> (estimate-pi 10000)
;;3.130609959636102
;;gosh> (estimate-pi 100000)
;;3.142257178644145
;;gosh> (estimate-pi 1000000)
;;3.1417143650889523
;;gosh>
