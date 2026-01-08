#lang racket450

(define lst1-9 (build-list 9 add1))
(define lst1-5 (build-list 5 add1))

;; args must be length 9
(define (p x1 x2 x3 x4 x5 x6 x7 x8 x9)
  (apply + (map * lst1-9 (list x1 x2 x3 x4 x5 x6 x7 x8 x9))))

(define (mk-racket-variable-name . strs)
  (string-join (map string-downcase strs) "-"))

;; args must be length 5
(define (p/lst lst5)
  (apply + (map * lst1-5 lst5)))
