#lang racket/base

(provide (except-out
          (all-from-out racket/base
                        racket/bool
                        racket/list
                        racket/math
                        racket/match
                        racket/format
                        racket/string
                        racket/contract
                        racket/function
                        rackunit)
          ;; excluded names:
          #%module-begin
          set!
          struct
          values
          define-values
;          match-define
;          match-let
;          let*
          letrec)
         local
         check-expect
         define-struct
         (rename-out
          [struct450 struct]
          [mb450 #%module-begin]))

(require racket/bool
         racket/list         
         racket/math
         racket/match
         racket/format
         racket/string
         racket/contract
         racket/function
         rackunit
         (for-syntax racket/base
                     syntax/stx
                     syntax/parse
                     racket/syntax))

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

(define-syntax mb450
  (syntax-parser
    [(_ forms ...)

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

     #:with do-provide-all (datum->syntax this-syntax '(provide (all-defined-out)))

     #'(#%module-begin
        do-provide-all
        non-test-forms ...
        test-forms ...
        #;(define HW-EXAMPLES
          (test-suite
           (string-append "HW Examples, run as extra Tests")
           (let () wrapped-test-forms ... (void))))
        #;(module+ main
          (require rackunit/text-ui)
          (run-tests HW-EXAMPLES 'verbose)))]))

;; override htdp forms with syntax error
(define-syntax define-struct
  (syntax-parser
    [(_ . _)
     #'(raise-syntax-error
        'define-struct "use struct instead of define-struct")]))

(define-syntax check-expect
  (syntax-parser
    [(_ . _)
     #'(raise-syntax-error
        'check-expect "use check-equal? instead of check-expect")]))

(define-syntax local
  (syntax-parser
    [(_ . _)
     #'(raise-syntax-error
        'local "use let or let* instead of local")]))

;; inserts #:transparent if not already present
(define-syntax struct450
  (syntax-parser
    [(_ (~and x (~not :keyword)) ...
        (~optional (~and #:transparent tr:keyword)
                   #:defaults ([tr #'#:transparent])))
     #'(struct x ... tr)]))
