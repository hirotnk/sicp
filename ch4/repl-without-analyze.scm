(use srfi-1)
(debug-print-width 10000)
(load "./repl-common")


(define (apply procedure arguments)
  (cond
    ;; use 'apply' of the underlying scheme
    ((primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure)
      (eval-sequence
        (procedure-body procedure)
        (extend-environment
          (procedure-parameters procedure)
          arguments
          (procedure-environment procedure))))
    (else
      (error "Unknown procedure type: APPLY" procedure))))

;;; eval takes:
;;; 1.expression
;;; 2.environment
;;;
;;; eval returns:
;;; 1.self-evaluating value
;;; 2.procedure(lambda) and arguments
(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)

    ;; this returns #t for: `(symbol? (car '(+ 1 2)))'
    ;; meaning that primitive operators are retrieved
    ;; here
    ((variable? exp) (lookup-variable-value exp env))

    ((quoted? exp) (text-of-quotation exp))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((unbound? exp) (eval-unbound exp env))
    ((if? exp) (eval-if exp env))
    ;;((and? exp) (eval-and (and-body exp) env))
    ((and? exp) (eval (and->if (cdr exp)) env))
    ;;((or? exp) (eval-or (or-body exp) env))
    ((or? exp) (eval (or->if (cdr exp)) env))
    ((lambda? exp) (make-procedure
                      (lambda-parameters exp)
                      (lambda-body exp)
                      env))
    ((begin? exp) (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((unless? exp) (eval (unless->if exp) env))
    ((let? exp) (eval (let->combination exp) env))
    ;;((let? exp) (eval (let->combination exp) env))
    ((let*? exp) (eval (let*->nested-lets exp) env))
    ((letrec? exp) (eval (letrec->combination exp) env))
    ((for? exp) (eval (for->combination exp) env))
    ((while? exp) (eval (while->combination exp) env))
    ((print-env? exp) (print the-global-environment))
    ((application? exp)
      (apply
        (eval (operator exp) env)
        (list-of-values (operands exp) env)))
    (else
      (error "Unknown expression type:EVAL" exp))))

(define (print-env? exp)
  (tagged-list? exp 'printenv))

;; left -> right
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let
      ((h (eval (first-operand exps) env)))
      (cons h (list-of-values (rest-operands exps) env)))))

(define (eval-if exp env)
  (if
    (not (false? (eval (if-predicate exp) env)))
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
      (eval (first-exp exps) env)
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

