(use srfi-1)
(use slib)
(require 'trace)
(debug-print-width 10000)
(load "./repl-common")

(define apply-in-underlying-scheme apply)

;(define (cons x y) (lambda (m) (m x y)))
;(define (car z) (z (lambda (p q) p)))
;(define (cdr z) (z (lambda (p q) q)))
;(define (list-ref items n)
;  (if (= n 0)
;      (car items)
;      (list-ref (cdr items) (- n 1))))
;
;(define (map proc items)
;  (if (null? items) '()
;      (cons (proc (car items)) (map proc (cdr items)))))
;
;(define (scale-list items factor)
;  (map (lambda (x) (* x factor)) items))
;
;(define (add-lists list1 list2)
;  (cond
;    ((null? list1) list2)
;    ((null? list2) list1)
;    (else (cons (+ (car list1) (car list2))
;    (add-lists (cdr list1) (cdr list2))))))
;
;;(define ones (cons 1 ones))
;
;;(define integers (cons 1 (add-lists ones integers)))


(define (apply procedure arguments env)
  (cond
    ((primitive-procedure? procedure)
     (apply-primitive-procedure
      procedure
      (list-of-arg-values arguments env)))
    ((compound-procedure? procedure)
      (eval-sequence
        (procedure-body procedure)
        (extend-environment
          (procedure-parameters procedure)
          (list-of-delayed-args arguments env)
          (procedure-environment procedure))))
    (else
      (error "Unknown procedure type: APPLY" procedure))))

(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp env))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((unbound? exp) (eval-unbound exp env))
    ((if? exp) (eval-if exp env))
    ((and? exp) (eval-and (and-body exp) env))
    ((or? exp) (eval-or (or-body exp) env))
    ((lambda? exp) (make-procedure
                      (lambda-parameters exp)
                      (lambda-body exp)
                      env))
    ((begin? exp) (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((unless? exp) (eval (unless->if exp) env))
    ((let? exp) (eval (let->combination exp) env))
    ((let? exp) (eval (let->combination exp) env))
    ((let*? exp) (eval (let*->nested-lets exp) env))
    ((letrec? exp) (eval (letrec->combination exp) env))
    ((for? exp) (eval (for->combination exp) env))
    ((while? exp) (eval (while->combination exp) env))
    ((print-env? exp) (print the-global-environment))
    ((application? exp)
      (apply
        (actual-value (operator exp) env)
        (operands exp)
        env))
        ; Non-Lazy
        ;(eval (operator exp) env)
        ;(list-of-values (operands exp) env)))
    (else
      (error "Unknown expression type:EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-arg-values exps env)
  (if
    (no-operands? exps) '()
    (cons
      (actual-value (first-operand exps) env)
      (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if
    (no-operands? exps) '()
    (cons
      (delay-it (first-operand exps) env)
      (list-of-delayed-args
        (rest-operands exps) env))))

(define (print-env? exp)
  (tagged-list? exp 'printenv))

(define (eval-if exp env)
  (if
    (not (false? (actual-value (if-predicate exp) env)))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

;; and
(define (and-body exp) (cdr exp))
(define (eval-and exp env)
  (define (eval-and-seq exp0 carry env)
    (if
      (null? exp0) carry
      (let
        ((result (eval (car exp0) env)))
        (if
          (false? result) 'false
            (eval-and-seq (cdr exp0) result env)))))
  (eval-and-seq exp 'true env))

;; or
(define (or-body exp) (cdr exp))
(define (eval-or exp env)
  (define (eval-or-seq exp0 carry env)
    (if
      (null? exp0) carry
      (let
        ((result (eval (car exp0) env)))
        (if
          (not (false? result)) result
          (eval-or-seq (cdr exp0) result env)))))
  (eval-or-seq exp 'false env))


(define (eval-sequence exps env)
  (cond
    ((last-exp? exps)
     (eval (first-exp exps) env))
    (else
      ;(eval (first-exp exps) env)
      (actual-value (first-exp exps) env)
      (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value!
    (assignment-variable exp)
    (eval (assignment-value exp) env)
    env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (eval-unbound exp env)
  (unbound-variable! (unbound-variable exp) env)
  'ok)

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let
    ((input (read)))
      (let ((output (actual-value input the-global-environment)))
           (announce-output output-prompt)
           (user-print output)))
  (driver-loop))

;; XXX: This version of `force-it' is really slow.
;(define (force-it obj)
;  (if
;    (thunk? obj)
;    ; this is an another example of mutual call
;    (actual-value (thunk-exp obj) (thunk-env obj))
;    obj))
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))
(define (force-it obj)
  (cond
    ((thunk? obj)
      (let
        ((result (actual-value (thunk-exp obj) (thunk-env obj))))
        (set-car! obj 'evaluated-thunk)
        (set-car! (cdr obj) result) ;replace exp with its value
        (set-cdr! (cdr obj) '())    ;forget unneeded env
        result))
    ((evaluated-thunk? obj) (thunk-value obj))
    (else obj)))


(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

