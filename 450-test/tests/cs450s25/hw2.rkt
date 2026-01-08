#lang racket450

;; (provide (all-defined-out))

;; (require 2htdp/image
;;          2htdp/universe
;;          rackunit)

(define SIDE 40)
(define UNIT SIDE) ; pixels
(define SCENE-W (* 10 UNIT))
(define SCENE-H (* 20 UNIT))
(define CENTER-X (/ SCENE-W 2))
(define MAX-Y SCENE-H)
(define MT-SCENE
  (empty-scene SCENE-W SCENE-H))

(define I-IMG
  (rectangle (* 4 SIDE) SIDE 'solid 'cyan))
(define L-IMG
  (beside/align "bottom"
   (rectangle (* 2 SIDE) SIDE 'solid 'orange)
   (rectangle SIDE (* 2 SIDE) 'solid 'orange)))
(define J-IMG
  (beside/align
   "bottom"
   (rectangle SIDE (* 2 SIDE) 'solid 'blue)
   (rectangle (* 2 SIDE) SIDE 'solid 'blue)))
(define O-IMG
  (rectangle (* 2 SIDE) SIDE 'solid 'yellow))
(define T-IMG
  (rectangle (* 2 SIDE) SIDE 'solid 'purple))
(define S-IMG
  (rectangle (* 2 SIDE) SIDE 'solid 'green))

(define Z-IMG
  (rectangle (* 2 SIDE) SIDE 'solid 'red))

(define (rad->deg rad) (* rad (/ 180 pi.f)))
;; student examples
(check-within (rad->deg (/ pi 3)) 60 .0001)

(define (deg->rad d) 0)

(define (mk-I-block rad)
  (rotate (rad->deg rad) I-IMG))
(mk-I-block 0)

(define (mk-L-block rad)
  (rotate (rad->deg rad) L-IMG))
(mk-L-block 0)

(define (mk-J-block rad)
  (rotate (rad->deg rad) J-IMG))
(mk-J-block 0)

;; A WorldState is a non-negative integer in [0, MAX-Y]
;; Represents: bot-center of an unrotated "J" block "bounding block"
(define (WorldState? x) (<= 0 x MAX-Y))

(define INIT-WORLDSTATE 0)
(define INIT INIT-WORLDSTATE)
(define/contract (next-WorldState w)
  (-> WorldState? WorldState?)
  (min (+ w SIDE) MAX-Y))
(define/contract (WorldState->Image w)
  (-> WorldState? image?)
  (place-image (mk-J-block 0) CENTER-X (get-J-center w) MT-SCENE))

;; ticks the given world w x number of times
(define (do-ticks w x)
  (if (zero? x)
      w
      (do-ticks (next-WorldState w) (sub1 x))))

;; get-J-center : Y-coord -> Y-Coord
;; compute center Y coordinate for shape, given bot Y coordinate
(define (get-J-center bot)
  (- bot SIDE))

(define (main)
  (big-bang INIT-WORLDSTATE 
    [on-tick next-WorldState 1]
    [to-draw WorldState->Image]))
