#lang racket450/testing

(DECLARE-HW-FILE "dummy.rkt")

(check-exn
 (lambda (e)
   (and (exn:fail? e)))
 (lambda () not-defined-var))
