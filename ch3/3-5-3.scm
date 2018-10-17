(include "3-5-1")
(include "3-5-2")

(define (average a b) (/ (+ a b) 2))
(define (sqrt-improve guess x)
    (average guess (/ x guess)))

(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0
                        (stream-map
                            (lambda(guess)
                                (sqrt-improve guess x))
                            guesses)))
    guesses)


(define (pi-summands n)
    (cons-stream (/ 1.0 n)
        (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
    (scale-stream (partial-sums (pi-summands 1)) 4))

;Ex 3.63
(define (sqrt-stream-rec x)
    (cons-stream 1.0 (stream-map
                        (lambda (guess)
                            (sqrt-improve guess x))
                     (sqrt-stream-rec x))))
; The difference of two versions is 'sqrt-stream-rec' is recursively calling
;itself to get the next element of stream. This can be observed by taking the
;trace of both versions.
;The two versions would still differ in efficiency without memo-proc for the function call overhead, but not much difference as before.

;gosh> (use slib)
;#<undef>
;gosh> (require 'trace)
;#t
;gosh> (trace sqrt-stream)
;#<closure (debug:trace-procedure debug:trace-procedure)>
;gosh> (trace sqrt-stream-rec)
;#<closure (debug:trace-procedure debug:trace-procedure)>
;gosh> (trace sqrt-improve)
;#<closure (debug:trace-procedure debug:trace-procedure)>
;gosh> (stream-ref (sqrt-stream 2) 10)
;CALL sqrt-stream 2
;RETN sqrt-stream (1.0 . #[proc])
;CALL sqrt-improve 1.0 2
;RETN sqrt-improve 1.5
;CALL sqrt-improve 1.5 2
;RETN sqrt-improve 1.4166666666666665
;CALL sqrt-improve 1.4166666666666665 2
;RETN sqrt-improve 1.4142156862745097
;CALL sqrt-improve 1.4142156862745097 2
;RETN sqrt-improve 1.4142135623746899
;CALL sqrt-improve 1.4142135623746899 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;1.414213562373095
;gosh> (stream-ref (sqrt-stream-rec 2) 10)
;CALL sqrt-stream-rec 2
;RETN sqrt-stream-rec (1.0 . #[proc])
;CALL sqrt-stream-rec 2
;RETN sqrt-stream-rec (1.0 . #[proc])
;CALL sqrt-improve 1.0 2
;RETN sqrt-improve 1.5
;CALL sqrt-stream-rec 2
;RETN sqrt-stream-rec (1.0 . #[proc])
;CALL sqrt-improve 1.0 2
;RETN sqrt-improve 1.5
;CALL sqrt-improve 1.5 2
;RETN sqrt-improve 1.4166666666666665
;CALL sqrt-stream-rec 2
;RETN sqrt-stream-rec (1.0 . #[proc])
;CALL sqrt-improve 1.0 2
;RETN sqrt-improve 1.5
;CALL sqrt-improve 1.5 2
;RETN sqrt-improve 1.4166666666666665
;CALL sqrt-improve 1.4166666666666665 2
;RETN sqrt-improve 1.4142156862745097
;CALL sqrt-stream-rec 2
;RETN sqrt-stream-rec (1.0 . #[proc])
;CALL sqrt-improve 1.0 2
;RETN sqrt-improve 1.5
;CALL sqrt-improve 1.5 2
;RETN sqrt-improve 1.4166666666666665
;CALL sqrt-improve 1.4166666666666665 2
;RETN sqrt-improve 1.4142156862745097
;CALL sqrt-improve 1.4142156862745097 2
;RETN sqrt-improve 1.4142135623746899
;CALL sqrt-stream-rec 2
;RETN sqrt-stream-rec (1.0 . #[proc])
;CALL sqrt-improve 1.0 2
;RETN sqrt-improve 1.5
;CALL sqrt-improve 1.5 2
;RETN sqrt-improve 1.4166666666666665
;CALL sqrt-improve 1.4166666666666665 2
;RETN sqrt-improve 1.4142156862745097
;CALL sqrt-improve 1.4142156862745097 2
;RETN sqrt-improve 1.4142135623746899
;CALL sqrt-improve 1.4142135623746899 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-stream-rec 2
;RETN sqrt-stream-rec (1.0 . #[proc])
;CALL sqrt-improve 1.0 2
;RETN sqrt-improve 1.5
;CALL sqrt-improve 1.5 2
;RETN sqrt-improve 1.4166666666666665
;CALL sqrt-improve 1.4166666666666665 2
;RETN sqrt-improve 1.4142156862745097
;CALL sqrt-improve 1.4142156862745097 2
;RETN sqrt-improve 1.4142135623746899
;CALL sqrt-improve 1.4142135623746899 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-stream-rec 2
;RETN sqrt-stream-rec (1.0 . #[proc])
;CALL sqrt-improve 1.0 2
;RETN sqrt-improve 1.5
;CALL sqrt-improve 1.5 2
;RETN sqrt-improve 1.4166666666666665
;CALL sqrt-improve 1.4166666666666665 2
;RETN sqrt-improve 1.4142156862745097
;CALL sqrt-improve 1.4142156862745097 2
;RETN sqrt-improve 1.4142135623746899
;CALL sqrt-improve 1.4142135623746899 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-stream-rec 2
;RETN sqrt-stream-rec (1.0 . #[proc])
;CALL sqrt-improve 1.0 2
;RETN sqrt-improve 1.5
;CALL sqrt-improve 1.5 2
;RETN sqrt-improve 1.4166666666666665
;CALL sqrt-improve 1.4166666666666665 2
;RETN sqrt-improve 1.4142156862745097
;CALL sqrt-improve 1.4142156862745097 2
;RETN sqrt-improve 1.4142135623746899
;CALL sqrt-improve 1.4142135623746899 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-stream-rec 2
;RETN sqrt-stream-rec (1.0 . #[proc])
;CALL sqrt-improve 1.0 2
;RETN sqrt-improve 1.5
;CALL sqrt-improve 1.5 2
;RETN sqrt-improve 1.4166666666666665
;CALL sqrt-improve 1.4166666666666665 2
;RETN sqrt-improve 1.4142156862745097
;CALL sqrt-improve 1.4142156862745097 2
;RETN sqrt-improve 1.4142135623746899
;CALL sqrt-improve 1.4142135623746899 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-stream-rec 2
;RETN sqrt-stream-rec (1.0 . #[proc])
;CALL sqrt-improve 1.0 2
;RETN sqrt-improve 1.5
;CALL sqrt-improve 1.5 2
;RETN sqrt-improve 1.4166666666666665
;CALL sqrt-improve 1.4166666666666665 2
;RETN sqrt-improve 1.4142156862745097
;CALL sqrt-improve 1.4142156862745097 2
;RETN sqrt-improve 1.4142135623746899
;CALL sqrt-improve 1.4142135623746899 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;CALL sqrt-improve 1.414213562373095 2
;RETN sqrt-improve 1.414213562373095
;1.414213562373095
;gosh>
;

; Ex 3.64
(define (sqrt x tolerance)
    (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s t)
    (let*
        (   (h1 (stream-car s))
            (h2 (stream-car (stream-cdr s)))
            (diff (abs (- h1 h2))))
        (if
            (< diff t)
                h2
                (stream-limit (stream-cdr s) t))))

; Execution
;gosh> (include "./3-5-3")
;stream-limit
;gosh> (sqrt 2 0.1)
;1.4166666666666665
;gosh> (sqrt 2 0.01)
;1.4142156862745097
;gosh> (sqrt 2 0.001)
;1.4142135623746899
;gosh>
        
; Ex 3.65

(define (ln-summands n)
    (cons-stream (/ 1.0 n)
        (stream-map - (ln-summands (+ n 1)))))
(define ln-stream
    (partial-sums (ln-summands 1)))

(define (euler-transform s)
    (let ((s0 (stream-ref s 0))
          (s1 (stream-ref s 1))
          (s2 (stream-ref s 2)))
         (cons-stream (- s2 (/ (* (- s2 s1) (- s2 s1))
                               (+ s0 (* -2 s1) s2)))
                      (euler-transform (stream-cdr s)))))

; Execution
;gosh>
;(stream-disp ln-stream 10)
;1.0
;0.5
;0.8333333333333333
;0.5833333333333333
;0.7833333333333332
;0.6166666666666666
;0.7595238095238095
;0.6345238095238095
;0.7456349206349207
;0.6456349206349207
;done
;gosh> (stream-disp (euler-transform ln-stream) 10)
;0.7
;0.6904761904761905
;0.6944444444444444
;0.6924242424242424
;0.6935897435897436
;0.6928571428571428
;0.6933473389355742
;0.6930033416875522
;0.6932539682539683
;0.6930657506744464
;done
;gosh>


;Ex 3.66
;
;      1  2  3  4  5   6   7   8   9  10  11  12
;-+-----------------------------------------------
;  1|  1  2  4  6  8  10  12  14  16  18  20  22 ..
;  2|     3  5  9 13 17  21  25  29  33 ..
;  3|        7 11 19 27 35 ..
;  4|          15 23 39 ..
;  5|             31 ..
;  .
;  .
;100|
;
;If we define this as F(row,col), such that row <= col. then
;F(row,col) == 2^row - 1 when row == col
;F(row,col) == F(row,1) + 2^(row - 1) + 2^row * (col - row - 1), in other cases
;                ^          ^              ^
;              first      second value   diff * (sum of steps to the last one)
;
;Therefore,
;F(1,100)   == (2^1 - 1) + 2^(1 - 1) + 2^1 * (100 - 1 - 1)
;           == 1         + 1         + 2*98
;           == 198
;
;F(99,100)  == (2^99 - 1) + 2^(99 - 1) + 2^99 * (100 - 99 - 1)
;           == 2^98 + 2^99 - 1
;
;F(100,100) == 2^100 - 1, since row == col
;

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave-stream
      (stream-map (lambda(k) (list (stream-car s) k)) (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave-stream s1 s2)
  (if
    (eq? s1 stream-null?) s2
    (cons-stream (stream-car s1) (interleave-stream s2 (stream-cdr s1)))))

;; Execution
;gosh> (stream-disp (pairs integers integers) 30)
;(1 1)
;(1 2)
;(2 2)
;(1 3)
;(2 3)
;(1 4)
;(3 3)
;(1 5)
;(2 4)
;(1 6)
;(3 4)
;(1 7)
;(2 5)
;(1 8)
;(4 4)
;(1 9)
;(2 6)
;(1 10)
;(3 5)
;(1 11)
;(2 7)
;(1 12)
;(4 5)
;(1 13)
;(2 8)
;(1 14)
;(3 6)
;(1 15)
;(2 9)
;(1 16)
;done


;Ex 3.67
(define (allpairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave-stream-set
      (list
        (stream-map (lambda(k) (list (stream-car s) k)) (stream-cdr t))
        (stream-map (lambda(j) (list j (stream-car t))) (stream-cdr s))
        (allpairs (stream-cdr s) (stream-cdr t))))))


(define (interleave-stream-set los)
  (cond
    ((eq? '() los) the-empty-stream)
    ((eq? (car los) stream-null?) (interleave-stream-set (cdr los)))
    (else
      (cons-stream
        (stream-car (car los))
        (interleave-stream-set (append (cdr los) (list (stream-cdr (car los)))))))))

;; Execution
;gosh> (stream-disp (allpairs integers integers) 100)
;(1 1)
;(1 2)
;(2 1)
;(2 2)
;(1 3)
;(3 1)
;(2 3)
;(1 4)
;(4 1)
;(3 2)
;(1 5)
;(5 1)
;(3 3)
;(1 6)
;(6 1)
;(2 4)
;(1 7)
;(7 1)
;(4 2)
;(1 8)
;(8 1)
;(3 4)
;(1 9)
;(9 1)
;(2 5)
;(1 10)
;(10 1)
;(5 2)
;(1 11)
;(11 1)
;(4 3)
;(1 12)
;(12 1)
;(2 6)
;(1 13)
;(13 1)
;(6 2)
;(1 14)
;(14 1)
;(4 4)
;(1 15)
;(15 1)
;(2 7)
;(1 16)
;(16 1)
;(7 2)
;(1 17)
;(17 1)
;(3 5)
;(1 18)
;(18 1)
;(2 8)
;(1 19)
;(19 1)
;(8 2)
;(1 20)
;(20 1)
;(5 3)
;(1 21)
;(21 1)
;(2 9)
;(1 22)
;(22 1)
;(9 2)
;(1 23)
;(23 1)
;(4 5)
;(1 24)
;(24 1)
;(2 10)
;(1 25)
;(25 1)
;(10 2)
;(1 26)
;(26 1)
;(3 6)
;(1 27)
;(27 1)
;(2 11)
;(1 28)
;(28 1)
;(11 2)
;(1 29)
;(29 1)
;(6 3)
;(1 30)
;(30 1)
;(2 12)
;(1 31)
;(31 1)
;(12 2)
;(1 32)
;(32 1)
;(5 4)
;(1 33)
;(33 1)
;(2 13)
;(1 34)
;(34 1)
;(13 2)
;

;; Ex 3.68
; No, infinite loop
(define (pairs-f s t)
  (interleave-stream
    (stream-map (lambda(x) (list (stream-car s) x)) t)
    (pairs-f (stream-cdr s) (stream-cdr t))))


;; Ex 3.69
(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave-stream
      (stream-map
        (lambda(pair)
          (cons (stream-car s) pair))
        (pairs (stream-cdr t) (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

;; Execution
;gosh> (stream-disp (triples integers integers integers) 100)
;(1 1 1)
;(1 2 2)
;(2 2 2)
;(1 2 3)
;(2 3 3)
;(1 3 3)
;(3 3 3)
;(1 2 4)
;(2 3 4)
;(1 3 4)
;(3 4 4)
;(1 2 5)
;(2 4 4)
;(1 4 4)
;(4 4 4)
;(1 2 6)
;(2 3 5)
;(1 3 5)
;(3 4 5)
;(1 2 7)
;(2 4 5)
;(1 4 5)
;(4 5 5)
;(1 2 8)
;(2 3 6)
;(1 3 6)
;(3 5 5)
;(1 2 9)
;(2 5 5)
;(1 5 5)
;(5 5 5)
;(1 2 10)
;(2 3 7)
;(1 3 7)
;(3 4 6)
;(1 2 11)
;(2 4 6)
;(1 4 6)
;(4 5 6)
;(1 2 12)
;(2 3 8)
;(1 3 8)
;(3 5 6)
;(1 2 13)
;(2 5 6)
;(1 5 6)
;(5 6 6)
;(1 2 14)
;(2 3 9)
;(1 3 9)
;(3 4 7)
;(1 2 15)
;(2 4 7)
;(1 4 7)
;(4 6 6)
;(1 2 16)
;(2 3 10)
;(1 3 10)
;(3 6 6)
;(1 2 17)
;(2 6 6)
;(1 6 6)
;(6 6 6)
;(1 2 18)
;(2 3 11)
;(1 3 11)
;(3 4 8)
;(1 2 19)
;(2 4 8)
;(1 4 8)
;(4 5 7)
;(1 2 20)
;(2 3 12)
;(1 3 12)
;(3 5 7)
;(1 2 21)
;(2 5 7)
;(1 5 7)
;(5 6 7)
;(1 2 22)
;(2 3 13)
;(1 3 13)
;(3 4 9)
;(1 2 23)
;(2 4 9)
;(1 4 9)
;(4 6 7)
;(1 2 24)
;(2 3 14)
;(1 3 14)
;(3 6 7)
;(1 2 25)
;(2 6 7)
;(1 6 7)
;(6 7 7)
;(1 2 26)
;(2 3 15)
;(1 3 15)
;(3 4 10)
;(1 2 27)
;done
;

(define (pythagorean? l)
  (if (= (+ (square (car l)) (square (cadr l))) (square (caddr l))) #t #f))

(define (square x) (* x x))

(define pytha-stream
  (stream-filter pythagorean? (triples integers integers integers)))

;; Execution: too slow...
;gosh> (stream-disp pytha-stream 10)
;(3 4 5)
;(6 8 10)
;(5 12 13)
;(9 12 15)
;(8 15 17)
;(12 16 20)
;Received too many signals before processing them.  Exitting for the emergency...

; Ex.3.70
(define (merge-weighted s1 s2 weight)
  (cond
    ((eq? s1 stream-null?) s2)
    ((eq? s2 stream-null?) s1)
    (else
      (let
        ((s1car (stream-car s1))
         (s2car (stream-car s2)))
        (cond
          ((< (weight s1car s2car) 0)
            (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
          (else
            (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t wproc)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda(k) (list (stream-car s) k)) (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) wproc) wproc)))

;a)
(define (weight-pair p1 p2)
   (let
    ((p11 (car p1))
     (p12 (cadr p1))
     (p21 (car p2))
     (p22 (cadr p2)))
    (cond
      ((> (+ p11 p12) (+ p21 p22)) 1)
      ((< (+ p11 p12) (+ p21 p22)) -1)
      (else 0))))
;; Execution
;gosh> (stream-disp (weighted-pairs integers integers weight-pair) 100)
;(1 1)
;(1 2)
;(2 2)
;(1 3)
;(2 3)
;(1 4)
;(3 3)
;(2 4)
;(1 5)
;(3 4)
;(2 5)
;(1 6)
;(4 4)
;(3 5)
;(2 6)
;(1 7)
;(4 5)
;(3 6)
;(2 7)
;(1 8)
;(5 5)
;(4 6)
;(3 7)
;(2 8)
;(1 9)
;(5 6)
;(4 7)
;(3 8)
;(2 9)
;(1 10)
;(6 6)
;(5 7)
;(4 8)
;(3 9)
;(2 10)
;(1 11)
;(6 7)
;(5 8)
;(4 9)
;(3 10)
;(2 11)
;(1 12)
;(7 7)
;(6 8)
;(5 9)
;(4 10)
;(3 11)
;(2 12)
;(1 13)
;(7 8)
;(6 9)
;(5 10)
;(4 11)
;(3 12)
;(2 13)
;(1 14)
;(8 8)
;(7 9)
;(6 10)
;(5 11)
;(4 12)
;(3 13)
;(2 14)
;(1 15)
;(8 9)
;(7 10)
;(6 11)
;(5 12)
;(4 13)
;(3 14)
;(2 15)
;(1 16)
;(9 9)
;(8 10)
;(7 11)
;(6 12)
;(5 13)
;(4 14)
;(3 15)
;(2 16)
;(1 17)
;(9 10)
;(8 11)
;(7 12)
;(6 13)
;(5 14)
;(4 15)
;(3 16)
;(2 17)
;(1 18)
;(10 10)
;(9 11)
;(8 12)
;(7 13)
;(6 14)
;(5 15)
;(4 16)
;(3 17)
;(2 18)
;(1 19)
;done


;b)
(define (weight-pair2 p1 p2)
   (let
    ((i1 (car p1))
     (j1 (cadr p1))
     (i2 (car p2))
     (j2 (cadr p2)))
    (cond
      ((> (wvalue i1 j1) (wvalue i2 j2)) 1)
      ((< (wvalue i1 j1) (wvalue i2 j2)) -1)
      (else 0))))

(define (wvalue i j)
  (+ (* 2 i) (* 3 j) (* 5 i j)))

(define non235
  (stream-filter
    (lambda(i)
      (and
        (not (= (modulo i 2) 0))
        (not (= (modulo i 3) 0))
        (not (= (modulo i 5) 0))))
    integers))

;; Execution
;gosh> (stream-disp (weighted-pairs non235 non235 weight-pair2) 100)
;(1 1)
;(1 7)
;(1 11)
;(1 13)
;(1 17)
;(1 19)
;(1 23)
;(1 29)
;(1 31)
;(7 7)
;(1 37)
;(1 41)
;(1 43)
;(1 47)
;(1 49)
;(1 53)
;(7 11)
;(1 59)
;(1 61)
;(7 13)
;(1 67)
;(1 71)
;(1 73)
;(1 77)
;(1 79)
;(11 11)
;(7 17)
;(1 83)
;(1 89)
;(1 91)
;(7 19)
;(11 13)
;(1 97)
;(1 101)
;(1 103)
;(1 107)
;(1 109)
;(7 23)
;(1 113)
;(13 13)
;(1 119)
;(1 121)
;(11 17)
;(1 127)
;(1 131)
;(1 133)
;(1 137)
;(1 139)
;(7 29)
;(11 19)
;(1 143)
;(13 17)
;(7 31)
;(1 149)
;(1 151)
;(1 157)
;(1 161)
;(1 163)
;(13 19)
;(1 167)
;(1 169)
;(11 23)
;(1 173)
;(7 37)
;(1 179)
;(1 181)
;(1 187)
;(17 17)
;(1 191)
;(1 193)
;(7 41)
;(1 197)
;(13 23)
;(1 199)
;(1 203)
;(7 43)
;(1 209)
;(1 211)
;(11 29)
;(17 19)
;(1 217)
;(1 221)
;(1 223)
;(7 47)
;(1 227)
;(11 31)
;(1 229)
;(1 233)
;(7 49)
;(19 19)
;(1 239)
;(1 241)
;(1 247)
;(13 29)
;(1 251)
;(1 253)
;(7 53)
;(17 23)
;(1 257)
;(1 259)

; Ex 3.71
(define (weight-ramanu p1 p2)
   (let
    ((i1 (car p1))
     (j1 (cadr p1))
     (i2 (car p2))
     (j2 (cadr p2)))
    (cond
      ((> (cube i1 j1) (cube i2 j2)) 1)
      ((< (cube i1 j1) (cube i2 j2)) -1)
      (else 0))))

(define (cube i j)
  (+ (* i i i) (* j j j)))

(define (cubepair p)
  (cube (car p) (cadr p)))

(define cubesum
  (weighted-pairs integers integers weight-ramanu))

(define (ramanujan-num s1)
  (if
    (= (cubepair (stream-car s1)) (cubepair (stream-car (stream-cdr s1))))
      (cons-stream
        (stream-car s1)
          (cons-stream
            (stream-car (stream-cdr s1)) (ramanujan-num (stream-cdr (stream-cdr s1)))))
      (ramanujan-num (stream-cdr s1))))

;; Execution
;gosh> (stream-disp (stream-map (lambda(p) (list p (cubepair p))) (ramanujan-num cubesum)) 12)
;((9 10) 1729)
;((1 12) 1729)
;((9 15) 4104)
;((2 16) 4104)
;((18 20) 13832)
;((2 24) 13832)
;((19 24) 20683)
;((10 27) 20683)
;((18 30) 32832)
;((4 32) 32832)
;((15 33) 39312)
;((2 34) 39312)
;done

; Ex 3.72

(define (weight-func p1 p2)
   (let
    ((i1 (car p1))
     (j1 (cadr p1))
     (i2 (car p2))
     (j2 (cadr p2)))
    (cond
      ((> (sqr i1 j1) (sqr i2 j2)) 1)
      ((< (sqr i1 j1) (sqr i2 j2)) -1)
      (else 0))))

(define (sqr i j)
  (+ (* i i) (* j j)))

(define (sqrpair p)
  (sqr (car p) (cadr p)))

(define squaresum
  (weighted-pairs integers integers weight-func))

(define (square-sum s1)
  (if
    (=
      (sqrpair (stream-car s1))
      (sqrpair (stream-car (stream-cdr s1)))
      (sqrpair (stream-car (stream-cdr (stream-cdr s1))))
      
      )
      (cons-stream
        (stream-car s1)
        (cons-stream
          (stream-car (stream-cdr s1))
          (cons-stream
            (stream-car (stream-cdr (stream-cdr s1)))
            (square-sum (stream-cdr (stream-cdr s1))))))
      (square-sum (stream-cdr s1))))

;; Execution
;gosh> (stream-disp (stream-map (lambda(p) (list p (sqrpair p))) (square-sum squaresum)) 100)
;((10 15) 325)
;((6 17) 325)
;((1 18) 325)
;((13 16) 425)
;((8 19) 425)
;((5 20) 425)
;((17 19) 650)
;((11 23) 650)
;((5 25) 650)
;((14 23) 725)
;((10 25) 725)
;((7 26) 725)
;((19 22) 845)
;((13 26) 845)
;((2 29) 845)
;((15 25) 850)
;((11 27) 850)
;((3 29) 850)
;((21 22) 925)
;((14 27) 925)
;((5 30) 925)
;((20 25) 1025)
;((8 31) 1025)
;((1 32) 1025)
;((23 24) 1105)
;((12 31) 1105)
;((9 32) 1105)
;((25 25) 1250)
;((17 31) 1250)
;((5 35) 1250)
;((20 30) 1300)
;((12 34) 1300)
;((2 36) 1300)
;((22 29) 1325)
;((13 34) 1325)
;((10 35) 1325)
;((22 31) 1445)
;((17 34) 1445)
;((1 38) 1445)
;((19 33) 1450)
;((15 35) 1450)
;((9 37) 1450)
;((25 30) 1525)
;((9 38) 1525)
;((2 39) 1525)
;((28 29) 1625)
;((20 35) 1625)
;((16 37) 1625)
;((27 31) 1690)
;((13 39) 1690)
;((3 41) 1690)
;((26 32) 1700)
;((16 38) 1700)
;((10 40) 1700)
;((23 36) 1825)
;((15 40) 1825)
;((12 41) 1825)
;((25 35) 1850)
;((13 41) 1850)
;((1 43) 1850)
;((27 34) 1885)
;((21 38) 1885)
;((11 42) 1885)
;((31 33) 2050)
;((23 39) 2050)
;((5 45) 2050)
;((30 35) 2125)
;((19 42) 2125)
;((10 45) 2125)
;((29 37) 2210)
;((23 41) 2210)
;((19 43) 2210)
;((25 40) 2225)
;((17 44) 2225)
;((4 47) 2225)
;((31 38) 2405)
;((17 46) 2405)
;((14 47) 2405)
;((24 43) 2425)
;((20 45) 2425)
;((11 48) 2425)
;((28 41) 2465)
;((23 44) 2465)
;((16 47) 2465)
;((34 37) 2525)
;((26 43) 2525)
;((5 50) 2525)
;((34 38) 2600)
;((22 46) 2600)
;((10 50) 2600)
;((25 45) 2650)
;((21 47) 2650)
;((7 51) 2650)
;((36 37) 2665)
;((27 44) 2665)
;((19 48) 2665)
;((31 42) 2725)
;((18 49) 2725)
;((15 50) 2725)
;((35 40) 2825)
;done
;gosh>
;
