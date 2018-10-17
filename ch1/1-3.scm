;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 1-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define (square_largers x y z)
  (cond
    ((= x (max x y)) (+ (* x x) (* (max y z) (max y z))))
    (else (square_largers z x y))))
  

(square_largers 2 3 4)
(square_largers 3 4 2)
(square_largers 4 2 3)
