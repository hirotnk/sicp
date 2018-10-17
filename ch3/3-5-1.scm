(define (memo-proc proc)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

(define-macro (delay exp) `(lambda () ,exp))

(define-macro (delay exp) `(memo-proc (lambda () ,exp)))

(define (force exp) (exp))

(define the-empty-stream '())

(define stream-null? null?)

(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-disp s k)
  (if (<= k 0 ) 'done
      (begin
      (display (stream-car s))
      (newline)
      (stream-disp (stream-cdr s) (- k 1))
      'done)))

(define (stream-fold s k init acc)
  (if (<= k 0 ) (exact->inexact acc)
      (stream-fold (stream-cdr s) (- k 1) init (+ (stream-car s) acc))))


(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (show-stream s n)
  (if (= n 0)
      (newline)
      (begin
        (display " ")
        (display (stream-car s))
        (show-stream (stream-cdr s) (- n 1)))))


;Ex 3.50
(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
         (apply proc (map stream-car argstreams))
         (apply stream-map
                (cons proc (map stream-cdr argstreams))))))
;Ex 3.51
(define (show x)
    (display-line x) x)

;; Execution
;$ [/Users/yoshihiro.tanaka/gith/giggle/sicp/ch3%] rlwrap gosh
;gosh> (load "./3-5-1")
;#t
;gosh> (define x (stream-map show (stream-enumerate-interval 0 10)))
;
;0x
;gosh> (stream-ref x 5)
;
;1
;2
;3
;4
;55
;gosh> (stream-ref x 7)
;
;6
;77
;gosh>

;Ex 3.52
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
    (stream-map accum (stream-enumerate-interval 1 20)))
;1 2 3 4 5..
;1 3 6 10 15 21 28 ..
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

;$ [/Users/yoshihiro.tanaka/gith/giggle/sicp/ch3%] rlwrap gosh
;gosh> (load "./3-5-1")
;#t
;gosh> (stream-ref y 7)
;136
;gosh> (display-stream z)
;
;10
;15
;45
;55
;105
;120
;190
;210done
;gosh> (display-stream y)
;
;6
;10
;28
;36
;66
;78
;120
;136
;190
;210done
;gosh> (display-stream seq)
;
;1
;3
;6
;10
;15
;21
;28
;36
;45
;55
;66
;78
;91
;105
;120
;136
;153
;171
;190
;210done
;gosh>

