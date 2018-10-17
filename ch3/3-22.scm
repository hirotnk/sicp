(define (make-queue)
    (let ((front-ptr '())
          (rear-ptr '())
          (queue (cons '() '())))

        (define (set-front-ptr! item)
            (set! front-ptr item)
            (set-car! queue item))
        (define (set-rear-ptr! item)
            (set! rear-ptr item)
            (set-cdr! queue item))

        (define (empty-queue?)
            (null? (car queue)))
        (define (make-queue) (cons '() '()))
        (define (front-queue)
            (if (empty-queue?)
                (error "FRONT called with an empty queue" queue)
                (car front-ptr)))
        (define (insert-queue! item)
            (let ((new-pair (cons item '())))
                (cond
                    ((empty-queue?)
                        (set-front-ptr! new-pair)
                        (set-rear-ptr! new-pair)
                        queue)
                    (else
                        (set-cdr! rear-ptr new-pair)
                        (set-rear-ptr! new-pair)
                        queue))))
        (define (delete-queue!)
            (cond
                ((empty-queue?)
                    (error "DELETE! called with an empty queue" queue))
                (else
                    (set-front-ptr! (cdr front-ptr))
                    queue)))

         (define (dispatch m)
            (cond
                ((eq? 'front-queue (car m)) (front-queue))
                ((eq? 'insert-queue! (car m)) (insert-queue! (cdr m)))
                ((eq? 'delete-queue! (car m)) (delete-queue!))
                ((eq? 'empty-queue? (car m)) (empty-queue?))
                (else
                    (error "Unknown message passed:" m))))
         dispatch))

(define (frontq qobj)
    (qobj (cons 'front-queue qobj)))
(define (insq qobj val)
    (qobj (cons 'insert-queue! val)))
(define (delq qobj)
    (qobj (cons 'delete-queue! qobj)))
(define (empq? qobj)
    (qobj (cons 'empty-queue? qobj)))


;; tests
;gosh> (load "./3-22")
;#t
;gosh> (define q2 (make-queue))
;q2
;gosh> (empq? q2)
;#t
;gosh> (insq q2 'a)
;(#0=(a) . #0#)
;gosh> (insq q2 'b)
;((a . #0=(b)) . #0#)
;gosh> (insq q2 'c)
;((a b . #0=(c)) . #0#)
;gosh> (empq? q2)
;#f
;gosh> (delq q2)
;((b . #0=(c)) . #0#)
;gosh> (delq q2)
;(#0=(c) . #0#)
;gosh> (frontq q2)
;c
;gosh> (delq q2)
;(() c)
;gosh> (frontq q2)
;*** ERROR: FRONT called with an empty queue (() c)
;Stack Trace:
;_______________________________________
;gosh> (empq? q2)
;#t


