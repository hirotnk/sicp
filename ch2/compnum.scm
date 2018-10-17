(load "./hash-tbl")
(load "./compnum-common")
(load "./2-4-3")

;; first install rectangular package here
(install-rectangular-package)
(install-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

;; testing
;;
;$ [/Users/yoshihiro.tanaka/gith/giggle/ch2%] rlwrap gosh
;gosh> (load "./compnum")
;#t
;gosh> (real-part (make-from-real-imag 1 2))
;1
;gosh> (imag-part (make-from-real-imag 1 2))
;2
;gosh> (magnitude (make-from-real-imag 1 2))
;1.5537739740300374
;gosh> (angle (make-from-real-imag 1 2))
;1.1071487177940904
;gosh>

