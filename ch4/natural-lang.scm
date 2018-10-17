(load "./repl-amb")

(amb-run)

(define (require p)
  (if (not p) (amb)))


(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
    (parse-word articles)
    (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
        (maybe-extend
          (list 'noun-phrase noun-phrase (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))



(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
    (parse-word prepositions)
    (parse-noun-phrase)))


(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
      (maybe-extend
        (list 'verb-phrase verb-phrase (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(parse '(the cat eats))
(parse '(the student with the cat sleeps in the class))
(parse '(the professor lectures to the student with the cat))


Execution:
;; [/Users/yoshi/gith/sicp/ch4%] rlwrap gosh
;; (load "./repl-amb")
;; 
;; (amb-run)
;; 
;; (define (require p)
;;   (if (not p) (amb)))
;; 
;; 
;; (define nouns '(noun student professor cat class))
;; (define verbs '(verb studies lectures eats sleeps))
;; (define articles '(article the a))
;; (define prepositions '(prep for to in by with))
;; 
;; (define (parse-sentence)
;;   (list 'sentence
;;         (parse-noun-phrase)
;;         (parse-verb-phrase)))
;; 
;; #t
;; gosh>
;; 
;; ;;; Amb-Eval input:
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; (define (parse-simple-noun-phrase)
;;   (list 'simple-noun-phrase
;;     (parse-word articles)
;;     (parse-word nouns)))
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; 
;; (define (parse-noun-phrase)
;;   (define (maybe-extend noun-phrase)
;;     (amb noun-phrase
;;         (maybe-extend
;;           (list 'noun-phrase noun-phrase (parse-prepositional-phrase)))))
;;   (maybe-extend (parse-simple-noun-phrase)))
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; 
;; 
;; 
;; (define (parse-word word-list)
;;   (require (not (null? *unparsed*)))
;;   (require (memq (car *unparsed*) (cdr word-list)))
;;   (let ((found-word (car *unparsed*)))
;;     (set! *unparsed* (cdr *unparsed*))
;;     (list (car word-list) found-word)))
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; 
;; (define (parse-prepositional-phrase)
;;   (list 'prep-phrase
;;     (parse-word prepositions)
;;     (parse-noun-phrase)))
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; 
;; 
;; (define (parse-verb-phrase)
;;   (define (maybe-extend verb-phrase)
;;     (amb verb-phrase
;;       (maybe-extend
;;         (list 'verb-phrase verb-phrase (parse-prepositional-phrase)))))
;;   (maybe-extend (parse-word verbs)))
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; 
;; (define *unparsed* '())
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; (define (parse input)
;;   (set! *unparsed* input)
;;   (let ((sent (parse-sentence)))
;;     (require (null? *unparsed*))
;;     sent))
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; ok
;; 
;; ;;; Amb-Eval input:
;; 
;; (parse '(the cat eats))
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; (sentence (simple-noun-phrase (article the) (noun cat)) (verb eats))
;; 
;; ;;; Amb-Eval input:
;; (parse '(the student with the cat sleeps in the class))
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; (sentence (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))) (verb-phrase (verb sleeps) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))))
;; 
;; ;;; Amb-Eval input:
;; (parse '(the professor lectures to the student with the cat))
;; 
;; ;;; Starting a new problem
;; ;;; Amb-Eval value:
;; (sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))
;; 
;; ;;; Amb-Eval input:
;; try-again
;; 
;; ;;; Amb-Eval value:
;; (sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))
;; 
;; ;;; Amb-Eval input:
;; try-again
;; 
;; ;;; There are no more values of
;; (parse '(the professor lectures to the student with the cat))
;; 
;; ;;; Amb-Eval input:
;; try-again
;; 
;; ;;; There is no current problem
;; 
;; ;;; Amb-Eval input:
;; 
;; 
