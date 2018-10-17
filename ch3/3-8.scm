(define f
    (let
        ((state 1))
        (lambda (val)
            (set! state (* state val)) state)))

;;gosh> (load "./3-8")
;;#t
;;gosh> (define a f)
;;a
;;gosh> (define b f)
;;b
;;gosh> (+ (a 1) (a 0))
;;1
;;gosh> (+ (b 0) (b 1))
;;0
;;gosh> (+ (a 0) (a 1))
;;0
;;gosh> (+ (a 1) (a 0))
;;0
;;gosh>
;;
