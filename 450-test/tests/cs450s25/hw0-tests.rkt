#lang racket450/testing

(DECLARE-HW-FILE "hw0.rkt")

(define lst1-9 (list 1 2 3 4 5 6 7 8 9))
(define lst1-5 (list 1 2 3 4 5))
(define lst6-9 (list 6 7 8 9))

(define (mk-random-lst size [max 100])
  (build-list size (lambda (x) (random max))))

(check-equal? 0 #;(apply p lst1-9)
              (apply + (map * lst1-9 lst1-9)))

(let ([randlst (mk-random-lst 9)])
  (check-equal? (apply p randlst)
                (apply + (map * lst1-9 randlst))))

(check-equal? (p/lst lst1-5)
              (apply + (map * lst1-5 lst1-5)))

(let ([randlst (mk-random-lst 5)])
  (check-equal? (p/lst randlst)
                (apply + (map * lst1-5 randlst))))

(check-equal? (p/lst lst1-5)
              (- (apply p lst1-9) (apply + (map * lst6-9 lst6-9))))

(let ([randlst (mk-random-lst 9)])
  (check-equal? (p/lst (take randlst 5))
                (- (apply p randlst)
                   (apply + (map * lst6-9 (drop randlst 5))))))

(check-equal? (mk-racket-variable-name "racket" "variable" "name")
              "racket-variable-name")
