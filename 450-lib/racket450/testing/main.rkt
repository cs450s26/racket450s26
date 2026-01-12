#lang racket/base

(provide (except-out
          (all-from-out
           rackunit
           racket450)
          #;#%top
          #%module-begin)
         #;DECLARE-HW-FILE
         test-case
         (rename-out #;[top450 #%top]
                     [testing-mb450 #%module-begin]))

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

#;(define HW-FILE
  (make-parameter #f)
  #;(lambda ()
     (raise
      (exn:fail:contract
       "HW file not declared (are you using #lang racket/testing ?)"))))

#;(define-syntax DECLARE-HW-FILE
  (syntax-parser [(_ f) #'(HW-FILE f)]))

#;(define-syntax (HW stx)
  (syntax-parse stx
    [(_ symb)
     #'(dynamic-require
        (HW-FILE)
        'symb
        (lambda ()
          (raise
           (exn:fail:contract:dynamic-require
            (format
             "attempted to use an identifier that was not defined: ~a" 'symb)
            (current-continuation-marks)))))]))

#;(define-syntax (top450 stx)
  (syntax-parse stx
    [(_ . p)
;     #:do[(displayln (syntax->datum #'p))]
     #'(HW p)]))

#;(define-syntax (test-case stx)
  (syntax-parse stx
    [(_ nam chk ...)
     #'(ru:test-case nam
                     (with-check-info*
                         (list
;                          (make-check-location (list (HW-FILE) #f #f #f #f))
                          (make-check-name nam)
                          (make-check-expression 'chk))
                       (lambda ()
                         (with-handlers
                             ([exn:fail:contract:dynamic-require?
                               (lambda (e)
                                 (fail (exn-message e)))]
                              [exn:fail:contract?
                               (lambda (e)
                                 (fail (exn-message e)))])
                           chk))) ...)]))

;; format of a racket450/testing file
;; (DECLARE-HW-FILE ...)
;; (requires ...)
;; (defines ...)
;; (test-cases ...)
(define-syntax testing-mb450
  (syntax-parser
    [(_ #;(~and hw-decl
              (~describe
               "HW DECLARATION FILE (should be 1st line after #lang)"
               ((~literal DECLARE-HW-FILE) _)))
        (~and req ((~literal require) . _)) ...
        (~and def ((~literal define) . _)) ...
        ;(~and def-stx ((~literal define-syntax) . _)) ...
        tst
        #;(~and tst
              (~describe
               "test case (requires and defines must come before first test case)"
               ((~or (~literal check-equal?)
                     (~literal check-true)
                     (~literal check-false)
                     (~literal check-not-false)
                     (~literal check-within)
                     (~literal let))
               . _))) ...)
     #:with (tst-case ...)
            (stx-map
             (syntax-parser
               [((~literal test-case) . _)
                this-syntax]
               [this-tst
                (syntax/loc this-syntax
                  (test-case (~a 'this-tst) this-tst))])
             #'(tst ...))
     #'(#%module-begin
       ; hw-decl
        req ...
        def ...
        ;def-stx ...
        (define TESTS
          (test-suite
           (string-append "HW TEST SUITE")
           #;(test-case
               (string-append "!CRASH CHECK: " (HW-FILE) " (NOT PASSING = NO CREDIT)")
             (check-not-exn (lambda () (dynamic-require (HW-FILE) #f))))
           #;(test-case
               (string-append "!TESTS FILE CHECK: " (HW-FILE) "-tests.rkt (NOT PASSING = NO CREDIT)")
             (check-not-exn
              (lambda ()
                (dynamic-require
                 (string-append
                  (path->string (path-replace-extension (HW-FILE) #""))
                  "-tests.rkt") #f))))
           #;(test-suite
            (string-append "!TESTS CHECK: " (HW-FILE) "-tests.rkt (NOT PASSING = NO CREDIT)")
             (dynamic-require
              (string-append
               (path->string (path-replace-extension (HW-FILE) #""))
               "-tests.rkt")
              'TESTS))
           (let () tst ...)))
        (module+ main
          (require rackunit/text-ui)
          (run-tests TESTS 'verbose)))]))


