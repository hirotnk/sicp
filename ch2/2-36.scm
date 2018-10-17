(define (accumulate op init seq)
  (if
    (null? seq) init
    (op (car seq) (accumulate op init (cdr seq)))))

(define (flatmap op seq)
  (accumulate append '()
    (map op seq)))

(define (accumulate-n op init seqs)
  (if
    (null? (car seqs)) '() ;seqs would be '(() () () ..) here
    (cons
      (accumulate op init (map car seqs))
      (accumulate-n op init (map cdr seqs)))))

(define (nmap op seqs)
  (if
    (null? (car seqs)) '()
    (cons
      (apply op (map car seqs))
      (nmap op (map cdr seqs)))))


(define lol (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(define (test-accumulate-n) (accumulate-n + 0 lol))
;(22 26 30)
(define (test-nmap) (nmap + lol))
;(22 26 30)



