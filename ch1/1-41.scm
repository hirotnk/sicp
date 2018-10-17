(define (double f)
    (lambda (x) (f (f x))))

(define (inc x)
    (+ x 1))

;this one is hard to understand:
;at least 'double' function is called 6 times:
;gosh> (trace double)
;#<closure (debug:trace-procedure debug:trace-procedure)>
;gosh> (((double (double double)) inc) 5)
;CALL double #[proc]
;RETN double #[proc]
;CALL double #[proc]
;RETN double #[proc]
;CALL double #[proc]
;RETN double #[proc]
;CALL double #[proc]
;RETN double #[proc]
;CALL double #[proc]
;RETN double #[proc]
;CALL double #[proc]
;RETN double #[proc]
;21
;gosh>
;
;I think it's about (double f) is different from (double double).
;(double f) => (lambda (x) (f(f(x)))) but
;(double double) => (lambda (x) (double(double(x)))), then
;=> ((lambda (x) (double(double(x)))) inc) => (double((lambda (x) inc(inc(x))))) =>
;(lambda(x) (inc(inc(inc(inc(x)))))), this is kind of inc4.
;now call (double double) 'dd':
;(double dd) is (lambda (x) (dd(dd(x)))), so:
;=> (dd (inc(inc(inc(inc(x)))))), call (inc(inc(inc(inc(x))))) inc4
;=> (dd inc4)
;=> (inc4(inc4(inc4(inc4(x)))))
;=> x + 16
;
;However, still not sure :/
;
