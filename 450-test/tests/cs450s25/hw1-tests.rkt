#lang racket450/testing

(DECLARE-HW-FILE "hw1.rkt")
;; ;; spring 2025 hw1 autograder

;; (provide TESTS)

;; (require 2htdp/image
;;          "testing-framework.rkt")

;; (define TESTS
;;   (test-suite
;;    "HW1 test-suite"

;;    #;(test-case
;;     "Checking that hw1.rkt file runs"
;;     (check-not-exn (lambda () (dynamic-require "hw1.rkt" #f))))

   (test-case
    "rad->deg / deg->rad: zero"
    (check-within (rad->deg 0) 0 .0001)
    (check-within (deg->rad 0) 0 .0001))
      
   (test-case
    "forum test: rad->deg pi/3 = ~60"
    (check-within (rad->deg (/ pi 3)) 60 .0001))

   (test-case
    "unrotated I-block"
    (check-equal? (mk-I-block 0)
                  (rectangle 160 40 "solid" "cyan")))

   (test-case
    "unrotated J-block"
    (check-equal? (mk-J-block 0)
                  (beside/align
                   "bottom"
                   (rectangle 40 80 "solid" "blue")
                   (rectangle 80 40 "solid" "blue")))
    (check-equal? (mk-J-block 0)
                  (above/align
                   "left"
                   (rectangle 40 40 "solid" "blue")
                   (rectangle 120 40 "solid" "blue"))))
   
   (test-case
    "I-block 90deg"
    (check-equal? (mk-I-block (/ pi 2))
                  (rectangle 40 160 "solid" "cyan")))

   (test-case
    "J-block 90deg"
    (check-equal? (mk-J-block (/ pi 2))
                  (beside/align
                   "bottom"
                   (rectangle 40 40 "solid" "blue")
                   (rectangle 40 120 "solid" "blue"))))

   (test-case
    "I-block 180 = same"
    (let ([x (random)])
      (check-equal? (mk-I-block (* x 2 pi))
                    (mk-I-block (+ (* x 2 pi) pi)))))

   (test-case
    "J-block 360 = same"
    (let ([x (random)])
      (check-equal? (mk-J-block (* x 2 pi))
                    (mk-J-block (+ (* x 2 pi) (* 2 pi))))))

   #;(test-case
    "I-block 0 = I-block pi"
    (check-equal? (mk-I-block 0)
                  (mk-I-block pi)))


;;    )) ; only place where closing parens allowed on separate line!

;; (module+ main
;;   (require rackunit/text-ui)
;;   (run-tests TESTS 'verbose))
