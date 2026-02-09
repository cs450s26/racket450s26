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
         (for-syntax "../450-stx-utils.rkt"
                     racket/base
                     syntax/stx
                     syntax/parse
                     racket/syntax))

(require rackunit
         (only-in rackunit [test-case ru:test-case]))

(struct exn:fail:contract:dynamic-require exn:fail:contract ())

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



