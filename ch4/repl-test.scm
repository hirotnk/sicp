;;; Unit tests for repl in SICP chapter 4

;;; Execution:
;;; [/Users/yoshi/gith/giggle/sicp/ch4%] gosh repl-test.scm
;;; Testing repl.scm ...
;;; test def append, expects ok ==> ok
;;; test append, expects (a b c d e f) ==> ok
;;;

(use gauche.test)
(test-start "repl.scm")
;(load "./repl-without-analyze")
(load "./repl")

;; Self-evaluating values
(test-section "Self-evaluating values")
(test* "true" 'true (eval 'true the-global-environment))
(test* "false" 'false (eval 'false the-global-environment))
(test* "number" 233 (eval 233 the-global-environment))
(test* "string" "testing" (eval "testing" the-global-environment))

;; variable definition and assignment
(test-section "Defintion & assignment")
(test* "define x" 'ok (eval '(define x 1) the-global-environment))
(test* "check variable x" 1 (eval 'x the-global-environment))
(test* "assignment x" 'ok (eval '(set! x 3) the-global-environment))
(test* "check variable x after assignment" 3 (eval 'x the-global-environment))

;; If
(test-section "if")
(test* "if 1" 100 (eval '(if (= 2 (/ 4 2)) 100 200)  the-global-environment))
(test* "if 2" 200 (eval '(if (= 3 (/ 4 2)) 100 200)  the-global-environment))

;; cond
(test-section "cond")
(test* "cond def" 'ok
  (eval
    '(define (cond-test x) (cond ((= x 1) (* x 2)) ((= x 2) (* x 3)) (else 100)))
    the-global-environment))
(test* "cond test1" 2 (eval '(cond-test 1) the-global-environment))
(test* "cond test2" 6 (eval '(cond-test 2) the-global-environment))
(test* "cond test3" 100 (eval '(cond-test 3) the-global-environment))
;; Ex 4.5
(test* "cond test4" 2
  (eval (quote (cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false)))
  the-global-environment))

;; Ex 4.4
(test-section "Ex 4.4")
(test* "and: true case1" 'true (eval '(and (= 2 (+ 1 1)) (= 3 (/ 9 3))) the-global-environment))
(test* "and: false case1" 'false (eval '(and (= 4 (+ 1 1)) (= 3 (/ 9 3))) the-global-environment))
(test* "or: true case1" 'true (eval '(or (= 2 (+ 1 1)) (= 4 (/ 9 3))) the-global-environment))
(test* "or: false case1" 'false (eval '(or (= 5 (+ 1 1)) (= 4 (/ 9 3))) the-global-environment))

;; lambda
(test-section "lambda")
(test* "lambda (plain definition)" 
  'ok (eval '(define triple (lambda (x) (* x x x))) the-global-environment))
(test* "lambda (plain exec)" 8 (eval '(triple 2) the-global-environment))

;; let
(test-section "let")
(test* "let def"
  'ok (eval '(define (let-test x y) (let ((y (* x 2)) (z (* y 2))) (* y z)))
    the-global-environment))
(test* "let-exec 1" 24 (eval '(let-test 2 3) the-global-environment))
;; Ex 4.7
(test* "let*"
       39
       (eval '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
       the-global-environment))
(test* "named-let def"
       'ok
       (eval '(define (fib n) (let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
       the-global-environment))
(test* "named-let exec"
       8
       (eval '(fib 6) the-global-environment))
(test* "another let test (def)"
       'ok
       (eval '(define foo (lambda (x) (let ((a 1) (b 2)) (set! a 2) (set! b 3) (* (+ a x) b))))
       the-global-environment))
(test* "another let test (exec)"
       12
       (eval '(foo 2)
       the-global-environment))

;; Ex 4.9
(test* "var def" 'ok (eval '(define y 1) the-global-environment))
(test* "for def"
  'ok (eval '(define (for-test mylist) (for x in mylist (set! y (+ x y))))
  the-global-environment))
(test* "for exec" 'ok (eval (quote (for-test '(1 2 3))) the-global-environment))
(test* "for result check" 7 (eval 'y the-global-environment))

;; Ex 4.16
(test* "ex 4.16-1" 'ok (eval '(define foo (lambda (x) (define a 1) (define b 2) (* (+ a x) b))) the-global-environment))
(test* "ex 4.16-2" 8 (eval '(foo 3) the-global-environment))
(test* "ex 4.16-3" 'ok (eval '(define (f x) (define (even? n) (if (= n 0) true (odd? (- n 1)))) (define (odd? n) (if (= n 0) false (even? (- n 1)))) (cond ((even? x) true) ((odd? x) false))) the-global-environment))
(test* "ex 4.16-4" 'true (eval '(f 10) the-global-environment))
(test* "ex 4.16-5" 'false (eval '(f 101) the-global-environment))

;; Ex 4.20
(test* "ex 4.20-1" 3628800 (eval '(letrec ((fact (lambda(n) (if (= n 1) 1 (* n (fact (- n 1))))))) (fact 10)) the-global-environment))


;; Other tests
(test-section "Others")
(test* "def append" 'ok (eval '(define (append l1 l2)
                                  (if 
                                    (null? l1) l2 
                                    (cons (car l1) (append (cdr l1) l2))))
                                the-global-environment))
(test* "append" '(a b c d e f) (eval (quote (append '(a b c) '(d e f))) the-global-environment))

(test-end)
