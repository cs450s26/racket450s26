#lang racket/base

;; fns in this file should typically be required for-syntax
(provide test-form?
         maybe-wrap-test-form)

; need check- literals and other forms at phase 0 when this file required for-syntax
(require (for-meta -1
                   rackunit
                   racket/base
                   racket/format)
         syntax/parse)

(define test-form?
  (syntax-parser
    [((~or (~literal test-case)
           (~literal check-equal?)
           (~literal check-within)
           (~literal check-true)
           (~literal check-false)
           (~literal check-not-false)
           (~literal check-exn))
      . _)
     #t]
    [((~or (~literal let)
           (~literal let*)
           (~literal letrec))
      let-bindings
      maybe-tst)
     (test-form? #'maybe-tst)]
    [_ #f]))
  
(define maybe-wrap-test-form
  (syntax-parser
    [((~literal test-case) . _) this-syntax]
    [maybe-tst
     #:when (test-form? #'maybe-tst)
     (syntax/loc this-syntax
       (test-case (~a 'maybe-tst) maybe-tst))]))


