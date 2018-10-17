(load "./machine2")
(define fact-machine
  (make-machine
   (list (list '= =)
         (list '- -)
         (list '* *)
         (list 'read read)
         (list 'print print))
   '(controller
     fact-start
       (perform (op initialize-stack))
       (assign continue (label fact-done))
       (assign n (op read))
     fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
     after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))
       (goto (reg continue))
     base-case
       (assign val (const 1))
       (goto (reg continue))
     fact-done
       (perform (op print-stack-statistics))
       (perform (op print) (reg val)))))
fact-machine
(print "To start: (start fact-machine)")


