(include "./3-5-1")

(define (square x) (* x x))
(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
    (stream-filter (lambda (x) (not (divisible? x 7))) integers))

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

;`cons-stream` returns a stream. Notice the second part of the pair is delayed
;object. So this won't continue computation once it determined the contents of
;the delayed promise.
(define (sieve stream)
    (cons-stream
        (stream-car stream)
        (sieve
            ;`stream-filter` takes a predicate and once it finds an element that
            ;satisfies it, it stops computation and returns a stream.
            ;Notice this stream retains the specific lambda.
            ;For example, if the lambda is (lambda(x) (not (divisible? x 2))), 
            ;the returned stream represents the stream that's not divisible by 2.
            (stream-filter
                (lambda (x)
                    (display stream)
                    (display "is ")(display x)(display " divisible by ")(display (stream-car stream))(display " ?")(newline)
                    (not (divisible? x (stream-car stream))))
                (stream-cdr stream)))))
;(define primes (sieve (integers-starting-from 2)))

;Execution of primes:
;gosh> (define primes (sieve (integers-starting-from 2)))
;primes
;gosh> (stream-ref primes 5)
;(2 . #<closure (memo-proc memo-proc)>)is 3 divisible by 2 ?
;(2 . #<closure (memo-proc memo-proc)>)is 4 divisible by 2 ?
;(2 . #<closure (memo-proc memo-proc)>)is 5 divisible by 2 ?
;(3 . #<closure (memo-proc memo-proc)>)is 5 divisible by 3 ?
;(2 . #<closure (memo-proc memo-proc)>)is 6 divisible by 2 ?
;(2 . #<closure (memo-proc memo-proc)>)is 7 divisible by 2 ?
;(3 . #<closure (memo-proc memo-proc)>)is 7 divisible by 3 ?
;(5 . #<closure (memo-proc memo-proc)>)is 7 divisible by 5 ?
;(2 . #<closure (memo-proc memo-proc)>)is 8 divisible by 2 ?
;(2 . #<closure (memo-proc memo-proc)>)is 9 divisible by 2 ?
;(3 . #<closure (memo-proc memo-proc)>)is 9 divisible by 3 ?
;(2 . #<closure (memo-proc memo-proc)>)is 10 divisible by 2 ?
;(2 . #<closure (memo-proc memo-proc)>)is 11 divisible by 2 ?
;(3 . #<closure (memo-proc memo-proc)>)is 11 divisible by 3 ?
;(5 . #<closure (memo-proc memo-proc)>)is 11 divisible by 5 ?
;(7 . #<closure (memo-proc memo-proc)>)is 11 divisible by 7 ?
;(2 . #<closure (memo-proc memo-proc)>)is 12 divisible by 2 ?
;(2 . #<closure (memo-proc memo-proc)>)is 13 divisible by 2 ?
;(3 . #<closure (memo-proc memo-proc)>)is 13 divisible by 3 ?
;(5 . #<closure (memo-proc memo-proc)>)is 13 divisible by 5 ?
;(7 . #<closure (memo-proc memo-proc)>)is 13 divisible by 7 ?
;(11 . #<closure (memo-proc memo-proc)>)is 13 divisible by 11 ?
;13
;gosh>


(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fib (cons-stream 0 (cons-stream 1 (add-streams fib (stream-cdr fib)))))

(define (scale-stream s factor)
  (stream-map (lambda(x) (* factor x)) s))

; This `double` function is confusing. My current understanding is:
; 1 is fixed(already given)
; scale-stream takes car from doulbe, which generates 1 * 2 = 2
; then using cons-stream, it returns stream with car 2
; scale-stream takes car from doulbe, which generates 2 * 2 = 4
; then using cons-stream, it returns stream with car 4
; scale-stream takes car from doulbe, which generates 4 * 2 = 8
; then using cons-stream, it returns stream with car 8
; and so on ...
(define double (cons-stream 1 (scale-stream double 2)))

(define primes (cons-stream 2 (stream-filter prime? (integers-starting-from 2))))
(define (prime? n)
    (define (iter ps)
        (cond
            ((> (square (stream-car ps)) n) #t)
            ((divisible? n (stream-car ps)) #f)
            (else (iter (stream-cdr ps)))))
    (iter primes))

; Ex 3.54
(define (mult-streams s1 s2) (stream-map * s1 s2))
(define factorials
    (cons-stream 1 (mult-streams factorials (integers-starting-from 1))))

; Ex 3.55
(define (partial-sums s)
    (cons-stream
        (stream-car s)
        (add-streams (stream-cdr s) (partial-sums s))))

; Ex 3.56
;;(define S (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 3)) (scale-stream S 5))))
(define (scale-stream s n)
            (stream-map (lambda(x) (* n x)) s))

; Ex 3.58
(define (expand num den radix)
    (cons-stream
        (quotient (* num radix) den)
        (expand (mod (* num radix) den) den radix)))
;; Execution
;;(for-each (lambda(x) (display (stream-ref (expand 1 7 10) x))) (iota 20 1 1))
;;42857142857142857142#<undef>
;;gosh> (for-each (lambda(x) (display (stream-ref (expand 3 8 10) x))) (iota 20 1 1))
;;75000000000000000000#<undef>
;;gosh>

; Ex 3.59a
(define (integrate-series s1)
    (mult-streams s1 (stream-map (lambda(x) (/ 1 x)) integers)))

; Execution
;;gosh> (stream-disp (integrate-series ones) 10)
;;1
;;1/2
;;1/3
;;1/4
;;1/5
;;1/6
;;1/7
;;1/8
;;1/9
;;1/10
;;done
;;gosh> (define tens (cons-stream 10 tens))
;;tens
;;gosh> (stream-disp (integrate-series tens) 10)
;;10
;;5
;;10/3
;;5/2
;;2
;;5/3
;;10/7
;;5/4
;;10/9
;;1
;;done


; Ex 3.59b
; 
(define exp-series
    (cons-stream 1 (integrate-series exp-series)))
; From :http://en.wikipedia.org/wiki/E_(mathematical_constant)
; Execution
;;gosh> (stream-disp exp-series 10)
;;1
;;1
;;1/2
;;1/6
;;1/24
;;1/120
;;1/720
;;1/5040
;;1/40320
;;1/362880
;;done
;;
;;gosh> (define exp 2.71828182845904523536028747135266249775724709369995)
;;exp
;;gosh> (- exp (stream-fold exp-series 10 0 0))
;;3.0288585284310443e-7
;;gosh> (- exp (stream-fold exp-series 20 0 0))
;;0.0
;;gosh> (- exp (stream-fold exp-series 5 0 0))
;;0.00994849512571161
;;gosh> (- exp (stream-fold exp-series 10 0 0))
;;3.0288585284310443e-7
;;gosh> (- exp (stream-fold exp-series 15 0 0))
;;8.15347789284715e-13
;;gosh> (- exp (stream-fold exp-series 20 0 0))
;;0.0
;;gosh>

; From: http://en.wikipedia.org/wiki/List_of_integrals_of_trigonometric_functions#Integrands_involving_only_sine
(define cosine-series (cons-stream 1 (integrate-series (stream-map (lambda(x) (* -1 x)) sine-series))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

;Ex 3.60
(define (mult-series s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (add-streams
      (scale-stream (stream-cdr s2) (stream-car s1) )
      (mult-series (stream-cdr s1) s2))))
;; Execution
;gosh> (stream-disp (add-streams (mult-series sine-series sine-series) (mult-series cosine-series cosine-series)) 10)
;1
;0
;0
;0
;0
;0
;0
;0
;0
;0
;done

;Ex 3.61
(define (invert-unit-series s)
  (cons-stream 1
    (scale-stream
      (mult-series
        (invert-unit-series s) (stream-cdr s)) -1)))

;; Execution
;gosh> (stream-disp (mult-series cosine-series (invert-unit-series cosine-series)) 10)
;1
;0
;0
;0
;0
;0
;0
;0
;0
;0
;done


;Ex 3.62
(define (div-series s1 s2)
  (mult-series s1 (invert-unit-series s2)))

(define tangent-series (div-series sine-series cosine-series))
; Execution
;(stream-disp (cons-stream 1 (mult-series tangent-series tangent-series)) 20)
;1
;0
;0
;1
;0
;2/3
;0
;17/45
;0
;62/315
;0
;1382/14175
;0
;21844/467775
;0
;929569/42567525
;0
;6404582/638512875
;0
;443861162/97692469875
;done
;gosh> (stream-disp (invert-unit-series (mult-series cosine-series cosine-series)) 20)
;1
;0
;1
;0
;2/3
;0
;17/45
;0
;62/315
;0
;1382/14175
;0
;21844/467775
;0
;929569/42567525
;0
;6404582/638512875
;0
;443861162/97692469875
;0
;done
;
