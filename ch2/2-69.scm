(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list
        left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

;; returns the symbols of leaf/tree (transparent)
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))  ;; 2nd element
        (caddr tree)))             ;; 3rd element

;; returns the weight of leaf/tree (transparent)
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

;; decodes bits using a given Huffman tree
(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits) '()
            (let ((next-branch
                    (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                        (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (choose-branch bit branch)
    (cond
        ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: choose-branch" bit))))

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))

;; makes ordered set (list) of pairs
;; from existing pairs.
;; this is actually sort function of pairs
(define (make-leaf-set pairs)
    (if (null? pairs) '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)   ;symbol
                                   (cadr pair)) ;frequency
                        (make-leaf-set (cdr pairs))))))

(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                        (make-leaf 'B 2)
                        (make-code-tree (make-leaf 'D 1)
                                        (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

(define (encode-symbol message tree)
    (cond
        ((null? tree) (error "encode-symbol-error-1"))
        ((and (leaf? tree) (eq? (symbol-leaf tree) message)) '())
        ((member message (symbols tree))
            (cond
                ((member message (symbols (left-branch tree)))
                    (cons 0 (encode-symbol message (left-branch tree))))
                ((member message (symbols (right-branch tree)))
                    (cons 1 (encode-symbol message (right-branch tree))))
                (else (error "encode-symbol-error-2"))))
        (else (error "encode-symbol-symbol-3"))))

; ex. 2.69
(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
    (cond
        ((null? leaf-set) '())
        ((null? (cdr leaf-set)) (car leaf-set))
        (else
            (successive-merge
                (adjoin-set
                    (make-code-tree (car leaf-set) 
                                    (cadr leaf-set))
                    (cddr leaf-set))))))


;;gosh> (decode sample-message sample-tree)
;;(A D A B B C A)
;;gosh> (encode (decode sample-message sample-tree) sample-tree)
;;(0 1 1 0 0 1 0 1 0 1 1 1 0)
;;gosh> sample-message
;;(0 1 1 0 0 1 0 1 0 1 1 1 0)
;;gosh>

;(define (generate-huffman-tree pairs)
;    (successive-merge (make-leaf-set pairs)))

;;
;; this is my 'sort'. notice that make-leaf-set is actually
;; nicely organized 'sort' function

;(define (find-min m l acc)
;    (cond
;        ((null? l) (list m acc))
;        ((< m (car l)) (find-min m (cdr l) (cons (car l) acc)))
;        ((> m (car l)) (find-min (car l) (cdr l) (cons m acc)))))
;
;(define (sort l)
;    (if (null? l) '()
;        (let ((temp (find-min (car l) (cdr l) '())))
;        (cons (car temp) (sort (cadr temp))))))
;
;
;(define (add-to-set a set)
;    (cond
;        ((null? set) (list a))
;        ((< a (car set)) (cons a set))
;        ((> a (car set))
;            (cons (car set) (add-to-set a (cdr set))))))
;
;(define (sort l)
;    (if (null? l) '()
;        (add-to-set (car l) (sort (cdr l)))))
;
