(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant)
    ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
    ((connector 'forget) retractor))
(define (connect connector new-constraint)
    ((connector 'connect) new-constraint))

(define (make-connector)
    (let ((value #f) (informant #f) (constraints '()))
        (define (set-my-value newval setter)
            (cond
                ((not (has-value? me))
                    (set! value newval)
                    (set! informant setter)
                    ;Operate on each constrain after the change of connector
                    ;value. Inside a constrain, it also sets value to
                    ;connectors and the changes propagate.
                    (for-each-except setter inform-about-value constraints))
                ((not (= value newval))
                    (error "Contradiction" (list value newval)))
                (else 'ignored)))
        (define (forget-my-value retractor)
            (if (eq? retractor informant)
                (begin
                    (set! informant #f)
                    (for-each-except retractor inform-about-no-value constraints))
                'ignored))
        (define (connect new-constraint)
            (if
                ;Check if 'new-constraint' exists in 'constraints' list
                ;with 'eq?'
                (not (memq new-constraint constraints))
                    (set! constraints (cons new-constraint constraints)))
            (if
                (has-value? me)
                    (inform-about-value new-constraint))
            'done)
        (define (me request)
            (cond
                ((eq? request 'has-value?) (if informant #t #f))

                ;Returns value
                ((eq? request 'value) value)

                ;Returns set-my-value definition
                ((eq? request 'set-value!) set-my-value)

                ;Returns forget-my-value definition
                ((eq? request 'forget) forget-my-value)

                ;Returns connect definition
                ((eq? request 'connect) connect)

                (else (error "Unknown operation: CONNECTOR" request))))
        me))

(define (for-each-except exception procedure list)
    (define (loop items)
        (cond
            ((null? items) 'done)
            ((eq? (car items) exception) (loop (cdr items)))
            (else
                (procedure (car items))
                (loop (cdr items)))))
    (loop list))

;;Interface to constraints
(define (inform-about-value constraint)
    (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
    (constraint 'I-lost-my-value))

(define (adder a1 a2 sum)
    (define (process-new-value)
        (cond
            ((and (has-value? a1) (has-value? a2))
                  (set-value! sum (+ (get-value a1) (get-value a2)) me))
            ((and (has-value? a1) (has-value? sum))
                  (set-value! a2 (- (get-value sum) (get-value a1)) me))
            ((and (has-value? a2) (has-value? sum))
                  (set-value! a1 (- (get-value sum) (get-value a2)) me))))
    (define (process-forget-value)
        (forget-value! sum me)
        (forget-value! a1 me)
        (forget-value! a2 me)
        (process-new-value))
    (define (me request)
        (cond
            ((eq? request 'I-have-a-value) (process-new-value))
            ((eq? request 'I-lost-my-value) (process-forget-value))
            (else (error "Unknown request: ADDER" request))))
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me)

(define (multiplier m1 m2 product)
    (define (process-new-value)
        (cond
            ((or (and (has-value? m1) (= (get-value m1) 0))
                 (and (has-value? m2) (= (get-value m2) 0)))
             (set-value! product 0 me))
            ((and (has-value? m1) (has-value? m2))
             (set-value! product (* (get-value m1) (get-value m2)) me))
            ((and (has-value? m1) (has-value? product))
             (set-value! m2 (/ (get-value product) (get-value m1)) me))
            ((and (has-value? m2) (has-value? product))
             (set-value! m1 (/ (get-value product) (get-value m2)) me))))
    (define (process-forget-value)
        (forget-value! product me)
        (forget-value! m1 me)
        (forget-value! m2 me)
        (process-new-value))
    (define (me request)
        (cond
            ((eq? request 'I-have-a-value) (process-new-value))
            ((eq? request 'I-lost-my-value) (process-forget-value))
            (else (error "Unknown request: MULTIPLIER" request))))
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me)

(define (constant value connector)
    (define (me request)
        (error "Unknown request: CONSTANT" request))
    (connect connector me)
    (set-value! connector value me)
    me)

(define (probe name connector)
    (define (print-probe value)
        (newline) (display "Probe: ") (display name) (display " = ") (display value))
    (define (process-new-value)
        (print-probe (get-value connector)))
    (define (process-forget-value) (print-probe "?"))
    (define (me request)
        (cond
            ((eq? request 'I-have-a-value) (process-new-value))
            ((eq? request 'I-lost-my-value) (process-forget-value))
            (else (error "Unknown request: PROBE" request))))
    (connect connector me)
    me)

(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
    (let ((u (make-connector))
          (v (make-connector))
          (w (make-connector))
          (x (make-connector))
          (y (make-connector)))
        (multiplier c w u)
        (multiplier v x u)
        (adder v y f)
        (constant 9 w)
        (constant 5 x)
        (constant 32 y)
        'ok))

(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)


;;Execution
;
;gosh> (load "./3-3-5")
;#t
;gosh> (set-value! C 25 'user)
;
;Probe: Celsius temp = 25
;Probe: Fahrenheit temp = 77done
;gosh> (set-value! F 212 'user)
;*** ERROR: Contradiction (77 212)
;Stack Trace:
;_______________________________________
;gosh> (forget-value! C 'user)
;
;Probe: Celsius temp = ?
;Probe: Fahrenheit temp = ?done
;gosh> (set-value! F 212 'user)
;
;Probe: Fahrenheit temp = 212
;Probe: Celsius temp = 100done
;gosh>
;

;Ex 3.33
;; It looks there are several ways to implement this ?
(define (averager a b c)
    (let (
            (x (make-connector))
            (y (make-connector)))
        (adder a b x)
        (constant 0.5 y)
        (multiplier x y c)
        'ok))

(define A (make-connector))
(define B (make-connector))
(define D (make-connector))

(averager A B D)
(probe "Averager A" A)
(probe "Averager B" B)
(probe "Averager D" D)

;;Ex 3.33 Execution
;$ [/Users/yoshihiro.tanaka/gith/giggle/ch3%] rlwrap gosh
;gosh> (load "./3-3-5")
;#t
;gosh> (set-value! A 20 'user)
;
;Probe: Averager A = 20done
;gosh> (set-value! B 30 'user)
;
;Probe: Averager B = 30
;Probe: Averager D = 25.0done
;gosh> (forget-value! B 'user)
;
;Probe: Averager B = ?
;Probe: Averager D = ?done
;gosh> (forget-value! A 'user)
;
;Probe: Averager A = ?done
;gosh> (set-value! D 30 'user)
;
;Probe: Averager D = 30done
;gosh> (set-value! A 10 'user)
;
;Probe: Averager A = 10
;Probe: Averager B = 50.0done
;gosh>

;;Ex 3.34
;'multiplier' needs at least 2 connector values in order to
;'process-new-value'. The change on 'product' does not propagate to 'm1' and
;'m2'. For example:
;((and (has-value? m1) (has-value? product))
;(set-value! m2 (/ (get-value product) (get-value m1)) me))
;
;Also the system does not expect a same connector be used in constraints.

;;Ex 3.35
(define (squarer  a b)
    (define (process-new-value)
        (if (has-value? b)
            (if (< (get-value b) 0)
                    (error "square less than 0: SQUARER" (get-value b))
                    (set-value! a (sqrt (get-value b)) me))
            (if (has-value? a)
                (if (< (get-value a) 0)
                        (error "square less than 0: SQUARER" (get-value a))
                        (set-value! b (* (get-value a) (get-value a)) me)))))
    (define (process-forget-value)
        (forget-value! a me)
        (forget-value! b me)
        (process-new-value))
    (define (me request)
        (cond
            ((eq? request 'I-have-a-value) (process-new-value))
            ((eq? request 'I-lost-my-value) (process-forget-value))
            (else (error "Unknown request: SQUARER" request))))
    (connect a me)
    (connect b me)
    me)

(define G (make-connector))
(define H (make-connector))

(squarer G H)
(probe "Squarer G" G)
(probe "Squarer H" H)

;; Ex 3.35 Execution
;$ [/Users/yoshihiro.tanaka/gith/giggle/ch3%] rlwrap gosh
;gosh> (load "./3-3-5")
;#t
;gosh> (set-value! G 2 'user)
;
;Probe: Squarer G = 2
;Probe: Squarer H = 4done
;gosh> (forget-value! G 'user)
;
;Probe: Squarer G = ?
;Probe: Squarer H = ?done
;gosh> (set-value! H 2 'user)
;
;Probe: Squarer H = 2
;Probe: Squarer G = 1.4142135623730951done
;gosh>


;;Ex 3.37
(define (c+ x y)
    (let ((z (make-connector)))
        (adder x y z)
        z))

(define (c* x y)
    (let ((z (make-connector)))
        (multiplier x y z)
        z))

; x / y = z <=> x = y * z
(define (c/ x y)
    (let ((z (make-connector)))
       (multiplier z y x)
       z))

(define (cv x)
    (let ((y (make-connector)))
        (constant x y)
        y))

(define (celsius-fahrenheit-converter x)
    (c+ (c* (c/ (cv 9) (cv 5)) x) (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

;;Ex 3.36 Execution
;gosh> (load "./3-3-5")
;#t
;gosh> (set-value! C 20 'user)
;done
;gosh> (get-value C)
;20
;gosh> (get-value F)
;68
;gosh> (forget-value! C 'user)
;done
;gosh> (set-value! C 0 'user)
;done
;gosh> (get-value F)
;32
;gosh> (forget-value! C 'user)
;done
;gosh> (set-value! C 100 'user)
;done
;gosh> (get-value F)
;212
;gosh>

