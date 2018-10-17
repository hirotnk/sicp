(load "./machine2.scm")
(define exp-machine
  (make-machine
   (list (list '= =)
         (list '- -)
         (list '* *)
         (list 'read read)
         (list 'print print))
   '(controller
     exp-start
       (perform (op initialize-stack))
       (assign continue (label exp-done))
       (save continue)
       (assign n (op read))
       (assign b (op read))
     exp-loop
       (test (op =) (reg n) (const 0))
       (branch (label base-exp))
       (assign continue (label after-exp))
       (save continue)
       (assign n (op -) (reg n) (const 1))
       (goto (label exp-loop))
     after-exp
       (restore continue)
       (assign val (op *) (reg b) (reg val))
       (goto (reg continue))
     base-exp
       (assign val (const 1))
       (restore continue)
       (goto (reg continue))
     exp-done
       (perform (op print-stack-statistics))
       (perform (op print) (reg val))
       (goto (label exp-start)))))
(start exp-machine)

;; gosh> (load "./Ex5-04.scm")
;; 3
;; 2
;; (total-pushes = 4 current-depth = 0 maximum-depth = 4)
;; 8
;; 8
;; 2
;; (total-pushes = 9 current-depth = 0 maximum-depth = 9)
;; 256
;; 
