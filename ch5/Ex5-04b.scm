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
       (assign n (op read))
       (assign b (op read))
       (assign val (const 1))
     exp-loop
       (test (op =) (reg n) (const 0))
       (branch (label exp-done))
       (assign n (op -) (reg n) (const 1))
       (assign val (op *) (reg b) (reg val))
       (goto (label exp-loop))
     exp-done
       (perform (op print-stack-statistics))
       (perform (op print) (reg val))
       (goto (label exp-start)))))
(start exp-machine)

;; gosh> (load "./Ex5-04b.scm")
;; 3
;; 2
;; (total-pushes = 0 current-depth = 0 maximum-depth = 0)
;; 8
;; 8
;; 2
;; (total-pushes = 0 current-depth = 0 maximum-depth = 0)
;; 256
;; 10
;; 2
;; (total-pushes = 0 current-depth = 0 maximum-depth = 0)
;; 1024
;;
