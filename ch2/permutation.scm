(include "./2-36")
(define (permutation set)
  (if
    (null? set) '(())
    (flatmap
        (lambda(i)
          (map
            (lambda(subset)
              (cons i subset))
            (permutation (remove (lambda(a) (= i a)) set)))) set)))

;; this version simply print permutations
(define (perm-print set acc)
  (if
    (null? set) (print (reverse acc))
    (for-each
      (lambda(i)
        (perm-print (remove (lambda(k) (= k i)) set) (cons i acc)))
      set)))

