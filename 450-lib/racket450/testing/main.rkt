#lang racket/base

(provide (except-out
          (all-from-out
           rackunit
           racket450)
          #%module-begin)
         test-case
         (rename-out [testing-mb450 #%module-begin]))

(require rackunit
         rackunit/text-ui
         racket450
         racket/format
         (for-syntax racket/base
                     syntax/stx
                     syntax/parse
                     racket/syntax))

(require rackunit
         (only-in rackunit [test-case ru:test-case]))

(struct exn:fail:contract:dynamic-require exn:fail:contract ())

;; TODO: put these in common file?
(begin-for-syntax
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
      [_ #f]))
  
  (define maybe-wrap-test-form
    (syntax-parser
      [((~literal test-case) . _) this-syntax]
      [(~and ((~or (~literal check-equal?)
                   (~literal check-within)
                   (~literal check-true)
                   (~literal check-false)
                   (~literal check-not-false)
                   (~literal check-exn))
              . _)
             this-tst)
       (syntax/loc this-syntax
         (test-case (~a 'this-tst) this-tst))]
      ))
  )

(define-syntax testing-mb450
  (syntax-parser
    [(_ (~and (~seq forms ...)))

     #:with (non-test-forms ...)
     (filter
      (compose not test-form?)
      (stx->list #'(forms ...)))

     #:with (test-forms ...)
     (filter
      test-form?
      (stx->list #'(forms ...)))

     #:with (wrapped-test-forms ...)
     (stx-map
      maybe-wrap-test-form
      #'(test-forms ...))

     #'(#%module-begin
        non-test-forms ...
        (define TESTS
          (test-suite
           (string-append "HW TEST SUITE")
           (let () wrapped-test-forms ... (void))))
        (module+ main
          (require rackunit/text-ui)
          (run-tests TESTS 'verbose)))]))



