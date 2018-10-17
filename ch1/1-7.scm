;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 1-7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require racket/trace)
(define (sqrt2 guess x)
  (if (and (> (/ guess (improve guess x)) 0.999) (< (/ guess (improve guess x)) 1.001) ) 
      guess
      (sqrt2 (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; At first I reviewed the format of IEEE754. then saw the distribution of floating point
;; numbers becomes sparse as the number grows. This means you can not express precise
;; value of your guess for a very large number, and hence can not compare them as:
;; (< (abs (- (* guess guess) val)) 0.001)
;; 
;; for 'precision': http://en.wikipedia.org/wiki/Accuracy_and_precision
;; For floating point representation: Computer Organization and Design: the Hardware/Software Interface by John L. Hennessy and David Patterson
;;
;; also found an interesting paper:
;;
;; Florian Loitsch. 2010. Printing floating-point numbers quickly and accurately with integers.
;; SIGPLAN Not. 45, 6 (June 2010), 233-243. DOI=10.1145/1809028.1806623 http://doi.acm.org/10.1145/1809028.1806623
;;
;; See how sparse for large numbers of floating point in Erlang:
;; 11> 100002088845279766.0.
;; 100002088845279760.0
;; 12> 100002088845279767.0.
;; 100002088845279760.0
;; 13> 100002088845279768.0.
;; 100002088845279780.0
;;
;; This is the example in Ruby:
;; irb(main):003:0> n0 = 100002088845279768.0
;; => 1.0000208884527978e+17
;; irb(main):004:0> n1 = 100002088845279768.0 - 1.0
;; => 1.0000208884527978e+17
;; irb(main):005:0> p n0 == n1
;; true
;; => true
;; irb(main):006:0> p 100002088845279768.0 + 1.0
;; 1.0000208884527978e+17
;; => 1.0000208884527978e+17
;; irb(main):007:0> p 100002088845279768.0 + 2.0
;; 1.0000208884527978e+17
;; => 1.0000208884527978e+17
;; irb(main):008:0> p 100002088845279768.0 + 3.0
;; 1.0000208884527978e+17
;; => 1.0000208884527978e+17
;; irb(main):009:0> p 100002088845279768.0 + 4.0
;; 1.0000208884527978e+17
;; => 1.0000208884527978e+17
;; irb(main):010:0> p 100002088845279768.0 + 5.0
;; 1.0000208884527978e+17
;; => 1.0000208884527978e+17
;; irb(main):011:0> p 100002088845279768.0 + 6.0
;; 1.0000208884527978e+17
;; => 1.0000208884527978e+17
;; irb(main):012:0> p 100002088845279768.0 + 7.0
;; 1.0000208884527978e+17
;; => 1.0000208884527978e+17
;; irb(main):013:0> p 100002088845279768.0 + 8.0
;; 1.0000208884527978e+17
;; => 1.0000208884527978e+17
;; irb(main):014:0> p 100002088845279768.0 + 9.0
;; 1.0000208884527979e+17
;; => 1.0000208884527979e+17
;; irb(main):015:0>
;; 
;;
;; Also helpful pages I consulted:
;; http://eli.thegreenplace.net/2007/06/21/sicp-section-11/
;; http://www.billthelizard.com/2009/10/sicp-exercises-16-18.html
;; http://blogs.msdn.com/b/dwayneneed/archive/2010/05/07/fun-with-floating-point.aspx
;; http://www.serpentine.com/blog/2011/06/29/here-be-dragons-advances-in-problems-you-didnt-even-know-you-had/
;; http://www.cc.kyoto-su.ac.jp/~yamada/programming/float.html (in Japanese)
;;
