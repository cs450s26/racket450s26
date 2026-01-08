#lang racket

(require (prefix-in 450: "../../450-lib/racket450/testing/main.rkt")
         rackunit)

(check-exn
 (lambda (e)
   (and (exn:fail:syntax? e)
        (string-contains? (exn-message e) "HW DECLARATION FILE")))
 (lambda () (expand #'(450:#%module-begin 1))))
