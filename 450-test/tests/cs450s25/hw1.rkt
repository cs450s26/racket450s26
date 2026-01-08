#lang racket450

(define SIDE 40)
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
