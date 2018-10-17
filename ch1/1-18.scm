;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 1-18) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define (mult a b)
    (mult-iter-in 0 a b))

(define (halve a)
  (/ a 2))
(define (double a)
  (* a 2))
(define (mult-iter-in result a b)
  (cond ((or (= a 0) (= b 0)) 0)
        ((= b 1) (+ a result))
        ((even? b) (mult-iter-in result (double a) (halve b)))
        (else (mult-iter-in (+ result a) a (- b 1)))))




(mult 0 3)
(mult 1 3)
(mult 2 3)
(mult 3 3)

(mult 3 0)
(mult 3 1)
(mult 3 2)
(mult 3 3)

(mult 8 9)
(mult 9 8)
(mult 0 0)
