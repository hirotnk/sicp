;The problem of interleavings:
;We have two processes, one with three ordered events (a,b,c), and the other
;with three ordered events (x,y,z). How many possible orders are there ?
;This problem can be thought as picking n slots out of n + m slots.
;In this case there are six slots, and we only need to pick three slots out of
;six. Order does not matter because those combinations can always be ordered.
;So the number of possible orders are: C(n+m,n)

(define (make-serializer)
    (let ((mutex (make-mutex)))
        (lambda (p)
            (define (serialized-p . args)
                (mutex 'acquire)
                (let ((val (apply p args)))
                    (mutex 'release)
                    val))
            serialized-p)))

(define (make-mutex)
    (let ((cell (list #f)))
        (define (the-mutex m)
            (cond ((eq? m 'acquire)
                   (if (test-and-set! cell)
                       (the-mutex 'acquire)))
                  ((eq? m 'release) (clear! cell))))
        the-mutex))
(define (clear! cell) (set-car! cell #f))

(define (test-and-set! cell)
    (if (car cell) #t (begin (set-car! cell #t) #f)))

;Ex 3.47a
(define (make-semaphore n)
    (let ((mutex (make-mutex)) (counter n))
        (define (the-semaphore m)
            (cond
                ((eq? m 'acquire)
                    (mutex 'acquire)
                    (if (>= counter n) (the-semaphore m) (set! n (+ n 1)))
                    (mutex 'release))
                ((eq? m 'release)
                    (mutex 'acquire)
                    (set! n (- n 1))
                    (mutex 'release))))
        the-semaphore))
;Ex 3.47b
(define (make-semaphore n)
    (let ((counter n) (cell (list #f)))
        (define (the-semaphore m)
            (cond
                ((eq? m 'acquire)
                    (if (or (> counter n) (test-and-set! cell))
                        (the-semaphore m)
                        (begin
                            (set! n (+ n 1))
                            (clear! cell))))
                ((eq? m 'release)
                    (if (test-and-set! cell)
                        (the-semaphore m)
                        (begin
                            (set! n (- n 1))
                            (clear! cell))))))
        the-semaphore))


;Ex 3.48
; If processes access resources in same order, at the point a first successful
;process gets the first resource, all other processes can not get in. Hence it
;will avoid dead lock.

;Ex 3.49
; When the order of access to resources is un-deterministic, the above strategy
;is not applicable

