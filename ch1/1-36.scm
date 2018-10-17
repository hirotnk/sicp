;gosh> (fixed-point (lambda (x) (/ (log 1000) (log x)))  10.0)
;
;2.9999999999999996
;6.2877098228681545
;3.7570797902002955
;5.218748919675316
;4.1807977460633134
;4.828902657081293
;4.386936895811029
;4.671722808746095
;4.481109436117821
;4.605567315585735
;4.522955348093164
;4.577201597629606
;4.541325786357399
;4.564940905198754
;4.549347961475409
;4.5596228442307565
;4.552843114094703
;4.55731263660315
;4.554364381825887
;4.556308401465587
;4.555026226620339
;4.55587174038325
;4.555314115211184
;4.555681847896976
;4.555439330395129
;4.555599264136406
;4.555493789937456
;4.555563347820309
;4.555517475527901
;4.555547727376273
;4.555527776815261
;4.555540933824255
;4.555532257016376
;result:4.555532257016376
;gosh> (fixed-point (lambda (x) (average x (+ 1 (/ 1 x))))  1.0)
;
;1.5
;1.5833333333333333
;1.6074561403508771
;1.6147785476652068
;1.61702925556443
;1.617723628348796
;1.6179380934832117
;1.6180043565683029
;1.6180248320058461
;1.6180311591702674
;result:1.6180311591702674
;gosh>

(define tolerance 0.00001)
(define (identity x) (newline) (display "result:") x)
(define (average x y) (/ (+ x y) 2.0))
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess))) ; bind 'next' to the result of '(f guess)'
      (newline)
      (display next)
      (if (close-enough? guess next)
           (identity next)
          (try next))))
  (try first-guess))


