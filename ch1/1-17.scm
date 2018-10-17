;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 1-17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define (fast-mult a b)
  (cond ((or (= a 0) (= b 0)) 0)
        ((= a 1) b)
        ((< a 0) (* -1 (fast-mult (* a -1) b)))
        ((< a b)
          (if (even? a) (fast-mult (halve a) (double b))
              (+ b (fast-mult (- a 1) b))))
        (else (fast-mult b a))))


(define (halve a)
  (/ a 2))
(define (double a)
  (* a 2))

(fast-mult 0 3)
(fast-mult 1 3)
(fast-mult 2 3)
(fast-mult 2 8)
(fast-mult 8 2)
(fast-mult 8 0)
(fast-mult 8 -1)
(fast-mult 3 6)

