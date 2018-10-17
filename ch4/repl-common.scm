(define apply-in-underlying-scheme apply)

(define (print-env? exp)
  (tagged-list? exp 'printenv))

;; left -> right
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let
      ((h (eval (first-operand exps) env)))
      (cons h (list-of-values (rest-operands exps) env)))))
;; left -> right (alternative ?)
;(define (list-of-values exps env)
;  (map
;    (lambda(h) (eval h env))
;    exps))


(define (boolean? exp)
  (if (or (eq? exp 'true) (eq? exp 'false)) #t #f))

(define (self-evaluating? exp)
  (cond
    ((number? exp) #t)
    ((string? exp) #t)
    ((boolean? exp) #t)
    ((eq? exp 'false) #t)
    ((eq? exp 'true) #t)
    (else #f)))

(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
;;4.33 Not perfect since it does not work for "(cdr '(1 2 3))"
;(define (text-of-quotation exp env)
;  (let ((text (cadr exp)))
;      (if
;        (pair? text) (eval (make-list text env) env)
;        text)))
;
;(define (make-list text env)
;  (if (null? text) '()
;      (list 'cons (list 'quote (car text))
;      (eval (cdr text) env))))




(define (tagged-list? exp tag)
  (if
    (pair? exp)
      (eq? (car exp) tag)
      #f))

;; assignment
;; (set! <var> <value>)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; definition
;; (define <var> <value>)
;; or
;; (define (<var> <parameter1> .. <parameterN>)
;;         <body>)
;; =>
;; (define <var>
;;   (lambda (<parameter1> .. <parameterN>)
;;           <body>))
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if
    (symbol? (cadr exp)) (cadr exp) ; variable name
    (caadr exp)))                   ; function name
(define (definition-value exp)
  (if
    (symbol? (cadr exp)) (caddr exp)
    (make-lambda
      (cdadr exp)   ;formal parameters
      (cddr exp)))) ;body

;; XXX This scan-out does not work now.
(define (scan-out-defines body)
  (define (get-body def) (list (last def)))
  (let
    ;; Here, it can be definition of variable, or function
    ;; Use definition-variable/definition-value
    ((def-names (get-defcontents body definition-variable))
     (def-bodies (get-defcontents body definition-value)))
    (if
      (> (length def-names) 0)
      (list
        (append
          (append
            (list 'let
              (map
                (lambda (defname) (list defname (quote '*unassigned*)))
                def-names))
            (map
              (lambda (pair) (list 'set! (car pair) (cadr pair)))
              (zip def-names def-bodies))) ; Pair of (defname, defvalue)
        (get-body body)))
      body)))

(define (get-defcontents body fn)
  (fold
    (lambda (elem acc) (if (definition? elem) (cons (fn elem) acc) acc))
    '() body))

;; make-unbound!
;; (make-unbound! <var>)
(define (unbound? exp) (tagged-list? exp 'make-unbound!))
(define (unbound-variable exp) (cadr exp))

;; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; conditionals
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if
    (not (null? (cdddr exp))) (cadddr exp)
;    #f))
    'false)) ;; this version is required to run amb version of repl.

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; and/or
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

;; and->if (derived expression)
(define (and->if exp)
  (define (expand-and-clause exp carry)
    (if
      (null? exp) carry
      (make-if
        (car exp)
        (expand-and-clause (cdr exp) (car exp))
        'false)))
  (expand-and-clause exp 'true))

;; or->if (derived expression)
(define (or->if exp)
  (define (expand-or-clause exp carry)
    (if
      (null? exp) carry
      (make-if
        (car exp)
        (car exp)
        (expand-or-clause (cdr exp) (car exp)))))
  (expand-or-clause exp 'false))


;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond
    ((null? seq) seq)
    ((last-exp? seq) (first-exp seq))
    (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; procedure application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;; need to connect the value of meta-circular representation of truth
;; with underlying scheme here
(define (true? exp) (not (false? exp)))
(define (false? exp) (or (eq? exp #f) (eq? exp 'false)))


;; derived expressions
; let*->nested-lets
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  (make-nested-lets (cadr exp) (caddr exp)))

(define (make-nested-lets varlist body)
  (if
    (null? varlist) body
    (list 'let (list (car varlist))
          (make-nested-lets (cdr varlist) body))))

; let->combination
(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp)
  (and (let? exp) (symbol? (cadr exp))))
(define (named-let-name exp)
  (cadr exp))

(define (let->combination exp)
  (if
    (named-let? exp)
    (let
      ((varlist (map car (caddr exp)))
       (arglist (map cadr (caddr exp)))
       (letbody (cadddr exp))
       (let-name (named-let-name exp)))
       (sequence->exp
         (cons
           (list 'define (cons let-name varlist) letbody)
           (list (cons let-name arglist)))))
    (let
      ((varlist (map car (cadr exp)))
       (arglist (map cadr (cadr exp)))
       (letbody (cddr exp)))
      (cons (make-lambda varlist letbody) arglist))))

; letrec->combination
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec->combination exp)
  (let
    ((varlist (map car (cadr exp)))
     (body (caddr exp)))
    (append
      (list
        'let
        (map (lambda (varname) (list varname (quote '*unassigned*))) varlist))
      (append
        (map
          (lambda (var_arg_pair) (list 'set! (car var_arg_pair) (cadr var_arg_pair)))
          (cadr exp))
        (list body)))))

; unless->if
;(unless predicate exceptional-value usual-value)
(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-predicate exp) (cadr exp))
(define (unless-exceptional-value exp) (caddr exp))
(define (unless-usual-value exp)
  (if
    (null? (cdddr exp))
    'false
    (cadddr exp)))

(define (unless->if exp)
  (make-if
    (unless-predicate exp)
    (unless-usual-value exp)
    (unless-exceptional-value exp)))

; cond->if
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if
    (null? clauses) #f   ;; no else clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if
        (cond-else-clause? first)
          (if
            (null? rest) (sequence->exp (cond-actions first))
            (error "ELSE clause isn't last: COND_IF" clauses))
          (make-if
            (cond-predicate first)
            (let
              ((first-cond-predicate (cond-predicate first))
               (first-cond-actions (cond-actions first)))
              (if
                (eq? (car first-cond-actions) '=>)
                ;; this 'list' is the crucial point
                ;(list (cadr first-cond-actions) first-cond-predicate)
                (cons (cadr first-cond-actions) (list first-cond-predicate))
                (sequence->exp first-cond-actions)))
            (expand-clauses rest))))))

;; for->combination
;; Form: (for <x> in <param list> <for-body>)
;;  <x>: an element in <param list>
;;  <param list>: a list
;;  <for-body>: a sequence of expressions. <x> can be used in <for-body>
;;
;; e.g.
;; (for->combination '(for x in param (print (+ x 1 2))))
;; =>
;; (if
;;   (null? param) 'ok
;;   ((lambda (params)
;;     (define (for-loop x params)
;;       (if
;;         (null? params)
;;         (print (+ x 1 2))
;;         (begin
;;           (print (+ x 1 2))
;;           (for-loop (car params) (cdr params)))))
;;     (for-loop (car params) (cdr params))) param))

(define (for? exp)
  (and
    (tagged-list? exp 'for)
    (eq? (caddr exp) 'in)))
(define (for-variable exp) (cadr exp))
(define (for-params exp) (cadddr exp))
(define (for-body exp) (last exp))
(define (make-loop param1 param2) (list 'for-loop param1 param2))
(define car-params (list 'car 'params))
(define cdr-params (list 'cdr 'params))
(define (for->combination exp)
  (make-if
    ;;Handle empty list case
    (list 'null? (for-params exp))
    (quote 'ok)
    (list
      (make-lambda (list 'params)
        (list
          (list 'define (make-loop (for-variable exp) 'params)
            (make-if
              (list 'null? 'params)
              (for-body exp)
              (cons 'begin
                (list
                  (for-body exp)
                  (make-loop car-params cdr-params)))))
          (make-loop car-params cdr-params))) (for-params exp))))

;; while loop
(define (while? exp) (tagged-list? exp 'while))
(define (while-condition exp) (cadr exp))
(define (while-body exp) (caddr exp))
(define (while->combination exp)
  (list
    (make-lambda '()
      (list (list 'define (list 'while-ite)
        (make-if
          (while-condition exp)
          (cons 'begin
            (list
              (while-body exp)
              (list 'while-ite))) (quote 'ok))) (list 'while-ite)))))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
  ;(list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;(define (make-frame variables values)
;  (if
;    (null? variables) '()
;    (cons
;      (cons (car variables) (car values))
;      (make-frame (cdr variables) (cdr values)))))
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
;(define (add-binding-to-frame! var val frame)
;  (if
;    (null? (cdr frame)
;      (set-cdr! frame (list (cons var val))))
;      (add-binding-to-frame! var val (cdr frame))))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if
    (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if
        (< (length vars) (length vals))
        (error "Too many arguments supplied" vars vals)
        (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))
;(define (lookup-variable-value var env)
;  (if
;    (eq? env the-empty-environment) (error "Unbound variable" var)
;    (env-loop var env (lambda (x) (cdr x)) 'lookup)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

;(define (set-variable-value! var val env)
;  (if
;    (eq? env the-empty-environment) (error "Unbound variable: SET!" var)
;    (env-loop var env (lambda (x) (set-cdr! x val)) 'set)))

(define (env-loop var in-env func op)
  (if
    (eq? in-env the-empty-environment)
    (error "Env-loop: Unbound variable?:" var)
    (let
      ; Retrieve the pair that matches 'var'
      ((retval (assoc var (first-frame in-env))))
      (cond
        ; Not found. Check enclosing environment
        ((eq? retval #f) (env-loop var (enclosing-environment in-env) func op))
        ; Raise error if *unassigned* when looked up
        ((and (pair? retval) (not (eq? op 'set)) (eq? (get-val retval) '*unassigned*))
         (error "env-loop:Unassigned:" retval))
        ; Apply operation to the pair
        ((pair? retval) (func retval))))))

(define (get-var pair) (car pair))
(define (get-val pair) (cdr pair))

;(define (define-variable! var val env)
;  (let
;    ((frame (first-frame env))
;     (retval (assoc var (first-frame env))))
;     (cond
;        ((eq? retval #f) (add-binding-to-frame! var val frame))
;        ((pair? retval) (set-cdr! retval val))
;        (else (error "Scan-frame:" retval)))))
;
;;(define (define-variable! var val env)
;;  (let ((frame (first-frame env)))
;;    (define (scan vars vals)
;;      (cond ((null? vars)
;;             (add-binding-to-frame! var val frame))
;;            (else (scan (cdr vars) (cdr vals)))))
;;    (scan (frame-variables frame) (frame-values frame))))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (unbound-variable! var env)
  (if
    (eq? env the-empty-environment)
    (error "Unbound-variable! - UNBOUND")
    (let
      ((new-frame
        (remove
          (lambda (pair) (eq? var (get-var pair)))
          (first-frame env))))
      (set-car! env new-frame))))

(define my-and (lambda exp 
  (if (null? exp) #t
    (every (lambda (e) (eq? e 'true)) exp))))
      
(define my-or (lambda exp
  (if (null? exp) #f
    (any (lambda (e) (eq? e 'true)) exp))))

(define (my-not exp)
  (cond
    ((eq? exp 'true) #f)
    ((eq? exp 'false) #t)
    (else
      (not exp))))

(define primitive-procedures
  (list
    (list 'car car)
    (list 'cadr cadr)
    (list 'cdr cdr)
    (list 'cddr cddr)
    (list 'cons cons)
    (list 'cadr cadr)
    (list 'assoc assoc)
    (list 'list list)
    (list 'any any)
    (list 'every every)
    (list 'eq? eq?)
    (list 'not my-not) ;; workaround
    (list 'and my-and) ;; workaround
    (list 'or my-or)   ;; workaround
    (list 'memq memq)
    (list '< <)
    (list '> >)
    (list '<= <=)
    (list '>= >=)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    (list '= =)
    (list 'abs abs)
    (list 'length length)
    (list 'member member)
    (list 'print print)
    (list 'newline newline)
    (list 'set! set!)
    (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map
    (lambda (proc) (list 'primitive (cadr proc)))
    primitive-procedures))

(define (setup-environment)
  (let
    ((initial-env
      (extend-environment
        (primitive-procedure-names)
        (primitive-procedure-objects)
        the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))


(define (apply-primitive-procedure proc args)
  (let
    ((retval (apply-in-underlying-scheme (primitive-implementation proc) args)))
    (cond
      ((eq? retval #t) 'true)
      ((eq? retval #f) 'false)
      (else retval))))

(define input-prompt  "::: M-Eval input:")
(define output-prompt "::: M-Eval value:")
;(define (driver-loop)
;  (prompt-for-input input-prompt)
;  (let
;    ((input (read)))
;      (let ((output (eval input the-global-environment)))
;           (announce-output output-prompt)
;           (user-print output)))
;  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if
    (compound-procedure? object)
    (display (list
                'compount-procedure
                (procedure-parameters object)
                (procedure-body object)
                '<procedure-env>))
    (display object)))

(define the-global-environment (setup-environment))
;(define run driver-loop)


;; Excecution

;; [/Users/yoshi/gith/giggle/sicp/ch4%] rlwrap gosh
;; gosh> (load "./repl")
;;
;;
;; ::: M-Eval input:
;; (define (append x y) (if (null? x) y (cons (car x) (append (cdr x) y))))
;;
;; ;;; M-Eval value:
;; ok
;;
;; ::: M-Eval input:
;; (append '(a b c) '(d e f))
;;
;; ;;; M-Eval value:
;; (a b c d e f)
;;
;; ::: M-Eval input:
;; (cons '1 '2)
;;
;; ;;; M-Eval value:
;; (1 . 2)
;;
;; ::: M-Eval input:

