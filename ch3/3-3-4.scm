(load "./3-3-2")

(define (make-wire)
    (let ((signal-value 0) (action-procedures '()))
        (define (set-my-signal! new-value)
            (if (not (= signal-value new-value))
                (begin (set! signal-value new-value)
                       (call-each action-procedures))
                 'done))
        ;Note this is called by logic-gates, and 'after-delay' is
        ;executed here after it's added to 'action-procesures'.
        ;When 'after-delay' is executed, an 'action' is added to
        ;'the-agenda', which is the global variable.
        (define (accept-action-procedure! proc)
            (set! action-procedures (cons proc action-procedures))
            (proc))

        (define (dispatch m)
            (cond ((eq? m 'get-signal) signal-value)
                  ((eq? m 'set-signal!) set-my-signal!)
                  ((eq? m 'add-action!) accept-action-procedure!)
                  (else (error "Unknown operation:WIRE" m))))
        dispatch))


(define (call-each procedures)
    (if (null? procedures)
        'done
        (begin
            ((car procedures))
            (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
    ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
    ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
    (add-to-agenda! (+ delay (current-time the-agenda))
                             action
                             the-agenda))

(define (propagate)
    (if (empty-agenda? the-agenda)
        'done
        (let ((first-item (first-agenda-item the-agenda)))
            (first-item)
            (remove-first-agenda-item! the-agenda)
            (propagate))))

(define (probe name wire)
    (add-action! wire
                 (lambda ()
                    (newline)
                    (display name) (display " ")
                    (display (current-time the-agenda))
                    (display " New-value = ")
                    (display (get-signal wire))
                    (newline))))

;; Builing block of agenda
;Time-segment is a pair :(time, actions_queue)
(define (make-time-segment time queue)
    (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
    (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
    (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
    (null? (segments agenda)))

;Agenda is a list:
;(time, time-segment1, time-segment2,...)
(define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
        (or (null? segments)
            (< time (segment-time (car segments)))))
    (define (make-new-time-segment time action)
        (let ((q (make-queue)))
            (insert-queue! q action)
            (make-time-segment time q)))
    (define (add-to-segments! segments)
        (if (= (segment-time (car segments)) time)
            (insert-queue! (segment-queue (car segments))
                            action)
            (let ((rest (cdr segments)))
                (if (belongs-before? rest)
                    (set-cdr! segments
                              (cons (make-new-time-segment time action)
                                    (cdr segments)))
                    (add-to-segments! rest)))))
    (let ((segments (segments agenda)))
        (if (belongs-before? segments)
            (set-segments!
                agenda
                (cons (make-new-time-segment time action) segments))
            (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
        (delete-queue! q)
        (if (empty-queue? q)
            (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
        (error "Agenda is empty: FIRST-AGENDA-ITEM")
        (let ((first-seg (first-segment agenda)))
            (set-current-time! agenda
                                (segment-time first-seg))
            (front-queue (segment-queue first-seg)))))



;; Global variables
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
        'ok))

(define (full-adder a b c-in sum c-out)
    (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)
        'ok))

(define (inverter input output)
    (define (invert-input)
        (let ((new-value (logical-not (get-signal input))))
            (after-delay inverter-delay
                         (lambda () (set-signal! output new-value)))))
    ; invert-input will be executed on the input wire
    (add-action! input invert-input) 'ok)

(define (logical-not s)
    (cond
        ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
    (if (and (= s1 1) (= s2 1)) 1 0))

(define (logical-or s1 s2)
    (if (and (= s1 0) (= s2 0)) 0 1))

(define (and-gate a1 a2 output)
    (define (and-action-procedure)
        (let ((new-value
                (logical-and (get-signal a1) (get-signal a2))))
           (after-delay
            and-gate-delay
            (lambda () (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok)

; Ex 3.28
(define (or-gate a1 a2 output)
    (define (or-action-procedure)
        (let ((new-value
                (logical-or (get-signal a1) (get-signal a2))))
            (after-delay
             or-gate-delay
             (lambda () (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok)

;; Ex 3.29
;;(define (or-gate a1 a2 output)
;;    (let ((c (make-wire)) (d (make-wire)) (e (make-wire)))
;;        (inverter a1 c)
;;        (inverter a2 d)
;;        (and-gate c d e)
;;        (inverter e output)
;;        'ok))
;; Delay time is one and-gate delay and two inverter delays.


; Ex 3.30
(define (ripple-carry-adder n1 n2 s c)
    (let ((c-in (make-wire)))
        (if (eq? (cdr n1) '())
            (set-signal! c-in 0)
            (ripple-carry-adder (cdr n1) (cdr n2) (cdr s) c-in))
        (full-adder (car n1) (car n2) c-in (car s) c)))


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))


;gosh> (load "./3-3-4")
;#t
;gosh> (probe 'sum sum)
;
;sum 0 New-value = 0#<undef>
;gosh> (probe 'carry carry)
;
;carry 0 New-value = 0#<undef>
;gosh> (half-adder input-1 input-2 sum carry)
;ok
;gosh> (set-signal! input-1 1)
;done
;gosh> (propagate)
;
;sum 8 New-value = 1done
;gosh> (set-signal! input-2 1)
;done
;gosh> (propagate)
;
;carry 11 New-value = 1
;sum 16 New-value = 0done
;

; Ex 3.31
;In logic-gates(or/and/invert), 'after-delay' is being called and 'after-delay'
;adds an action (lambda function in this case) to global variable 'the-agenda'.
;If 'proc' is not called, actions are not added to 'the-agenda' and simulation
;would not work.

; Ex 3.32
;A 'new-value' in logic-gate is determined when a signal is changed. Therefore
;when inputs to and-gate:(a=0,b=1) change to (a=1,b=0), the value of 'a' changes
;to 1, and then 'new-value' would be 1. Then the value of 'b' changes to 0, and
;'new-value' would be 0. For both cases, lambda functions are created and
;registered to 'the-agenda'. So the last lambda function(with 0 as the result)
;needs to be executed last, hence queue is needed.


