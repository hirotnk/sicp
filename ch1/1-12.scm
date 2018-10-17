;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 1-12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define (pascaltri col row)
  (cond ((= col row) 1)
        ((= col 0) 1)
        (else (+ (pascaltri (- col 1) (- row 1)) (pascaltri col (- row 1))))))

(pascaltri 0 0)
(pascaltri 3 3)
(pascaltri 1 2)
(pascaltri 2 2)
(pascaltri 2 3)
(pascaltri 2 4)
