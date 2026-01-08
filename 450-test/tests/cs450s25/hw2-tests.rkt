#lang racket450/testing

(DECLARE-HW-FILE "hw2.rkt")

;; ;; spring 2025 hw2 autograder

;; (provide TESTS)

;; (require 2htdp/image
;;          "testing-framework.rkt")

(define SIDE 40)
(define UNIT SIDE)
(define SCENE-W (* 10 UNIT))
(define SCENE-H (* 20 UNIT))
(define CENTER-X (/ SCENE-W 2))
(define MAX-Y SCENE-H)
(define MT-SCENE
  (empty-scene SCENE-W SCENE-H))

(define (rad->deg rad) (* rad (/ 180 pi.f)))
(define J-IMG
  (beside/align
   "bottom"
   (rectangle SIDE (* 2 SIDE) 'solid 'blue)
   (rectangle (* 2 SIDE) SIDE 'solid 'blue)))
(define (mk-J-block rad)
  (rotate (rad->deg rad) J-IMG))

;; (define TESTS
;;   (test-suite
;;    "HW2 test-suite"

;;    #;(test-case
;;     "Checking that hw2.rkt file runs"
;;     (check-not-exn (lambda () (dynamic-require "hw2.rkt" #f))))

   (test-case
    "INIT-WORLDSTATE is a WorldState"
    (check-true (WorldState? INIT-WORLDSTATE)))

   (test-case
    "initial scene is empty"
    (check-equal?
     (WorldState->Image INIT-WORLDSTATE)
     MT-SCENE))

   (test-case
    "scene after 2 ticks has J-block on top"
    (let ([num-ticks 2])
      ;; ticks the given world w x number of times
      (define (do-ticks w x)
        (if (zero? x)
            w
            (do-ticks (next-WorldState w) (sub1 x))))
      (check-equal?
       (WorldState->Image (do-ticks INIT-WORLDSTATE num-ticks))
       (overlay/align "middle" "top" (mk-J-block 0) MT-SCENE))))
   
   (test-case
    "scene after rand [0,20] ticks is correct"
    (let ([num-ticks (random 20)])
      ;; ticks the given world w x number of times
      (define (do-ticks w x)
        (if (zero? x)
            w
            (do-ticks (next-WorldState w) (sub1 x))))
      (check-equal?
       (WorldState->Image (do-ticks INIT-WORLDSTATE num-ticks))
       (place-image (mk-J-block 0) CENTER-X (- (* num-ticks UNIT) UNIT) MT-SCENE))))

   (test-case
    "scene after 20 ticks has J-block on bottom"
    (let ([num-ticks 20])
      ;; ticks the given world w x number of times
      (define (do-ticks w x)
        (if (zero? x)
            w
            (do-ticks (next-WorldState w) (sub1 x))))
      (check-equal?
       (WorldState->Image (do-ticks INIT-WORLDSTATE num-ticks))
       (overlay/align "middle" "bottom" (mk-J-block 0) MT-SCENE))))


   (test-case
    "scene after 21 ticks has J-block on bottom"
    (let ([num-ticks 21])
      ;; ticks the given world w x number of times
      (define (do-ticks w x)
        (if (zero? x)
            w
            (do-ticks (next-WorldState w) (sub1 x))))
      (check-equal?
       (WorldState->Image (do-ticks INIT-WORLDSTATE num-ticks))
       (overlay/align "middle" "bottom" (mk-J-block 0) MT-SCENE))))

   (test-case
    "scene after 40 ticks has J-block on bottom"
    (let ([num-ticks 40])
      ;; ticks the given world w x number of times
      (define (do-ticks w x)
        (if (zero? x)
            w
            (do-ticks (next-WorldState w) (sub1 x))))
      (check-equal?
       (WorldState->Image (do-ticks INIT-WORLDSTATE num-ticks))
       (overlay/align "middle" "bottom" (mk-J-block 0) MT-SCENE))))

   (test-case
    "valid WorldState after 20 ticks"
    (let ([num-ticks 20])
      ;; ticks the given world w x number of times
      (define (do-ticks w x)
        (if (zero? x)
            w
            (do-ticks (next-WorldState w) (sub1 x))))
      (check-true (WorldState? (do-ticks INIT-WORLDSTATE num-ticks)))))

   (test-case
    "valid WorldState after 40 ticks"
    (let ([num-ticks 40])
      ;; ticks the given world w x number of times
      (define (do-ticks w x)
        (if (zero? x)
            w
            (do-ticks (next-WorldState w) (sub1 x))))
      (check-true (WorldState? (do-ticks INIT-WORLDSTATE num-ticks)))))

   (test-case
    "valid WorldState after rand [0,100] ticks"
    (let ([num-ticks (random 100)])
      ;; ticks the given world w x number of times
      (define (do-ticks w x)
        (if (zero? x)
            w
            (do-ticks (next-WorldState w) (sub1 x))))
      (check-true (WorldState? (do-ticks INIT-WORLDSTATE num-ticks)))))

   (test-case
    "valid WorldState after rand [0,200] ticks"
    (let ([num-ticks (random 200)])
      ;; ticks the given world w x number of times
      (define (do-ticks w x)
        (if (zero? x)
            w
            (do-ticks (next-WorldState w) (sub1 x))))
      (check-true (WorldState? (do-ticks INIT-WORLDSTATE num-ticks)))))

   (test-case
    "main fn is defined"
    (check-true (procedure? main)))
   
;;    )) ; only place where closing parens allowed on separate line!

;; (module+ main
;;   (require rackunit/text-ui)
;;   (run-tests TESTS 'verbose))
