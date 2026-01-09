#lang racket/base

(provide (except-out
          (all-from-out
           rackunit
           racket450)
          #%top
          #%module-begin)
         DECLARE-HW-FILE
         test-case
         (rename-out [top450 #%top]
                     [testing-mb450 #%module-begin])
         #;(for-syntax
          (all-from-out racket/base)))

(require rackunit
         rackunit/text-ui
         racket450
         racket/format
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

(require rackunit
         (only-in rackunit [test-case ru:test-case]))

(struct exn:fail:contract:dynamic-require exn:fail:contract ())

(define HW-FILE
  (make-parameter
   (lambda ()
     (raise
      (exn:fail:contract
       "HW file not declared (are you using #lang racket/testing ?)")))))

(define-syntax DECLARE-HW-FILE
  (syntax-parser [(_ f) #'(HW-FILE f)]))

(define-syntax (HW stx)
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

(define-syntax (top450 stx)
  (syntax-parse stx
    [(_ . p)
;     #:do[(displayln (syntax->datum #'p))]
     #'(HW p)]))

(define-syntax (test-case stx)
  (syntax-parse stx
    [(_ nam chk ...)
     ;; #:with _p (format-id #'nam "p")
     ;; #:with _p/lst (format-id #'nam "p/lst")
     ;; #:with _mk-racket-variable-name (format-id #'nam "mk-racket-variable-name")
     #'(ru:test-case nam
                     (with-check-info*
                         (list
;                          (make-check-location (list (HW-FILE) #f #f #f #f))
;                          (make-check-name nam)
                          (make-check-expression 'chk))
                       (lambda ()
                         (with-handlers
                             ([exn:fail:contract:dynamic-require?
                               (lambda (e)
                                 (fail (exn-message e)))]
                              [exn:fail:contract?
                               (lambda (e)
                                 (fail (exn-message e)))])
                           chk
                           #;(let ([_p (HW p)]
                                 [_p/lst (HW p/lst)]
                                 [_mk-racket-variable-name (HW mk-racket-variable-name)])
                             chk)
                           ))) ...)]))

(define-syntax testing-mb450
  (syntax-parser
    #:literals (DECLARE-HW-FILE)
    [(_ (~and hw-decl
              (~describe
               "HW DECLARATION FILE (should be 1st line after #lang)"
               ((~literal DECLARE-HW-FILE) _)))
        (~and def ((~literal define) . _)) ...
        ru-tst ...)
;    #:do[(displayln #'(def ...))
;         (displayln #'(tst ...))]
     #'(#%module-begin
        hw-decl
        def ...
        (define TESTS
          (test-suite
           (string-append (HW-FILE) " TESTS")
           (test-case (~a 'ru-tst) ru-tst) ...))
        (module+ main
          (require rackunit/text-ui)
          (run-tests TESTS 'verbose)))]))


