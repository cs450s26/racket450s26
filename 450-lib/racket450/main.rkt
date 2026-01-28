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
                        ;; 2htdp/image
                        ;; 2htdp/universe)
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
         ;; 2htdp/image
         ;; 2htdp/universe
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

(define-syntax mb450
  (syntax-parser
    [(_ x ...)
     #:with do-provide-all (datum->syntax this-syntax '(provide (all-defined-out)))
     #'(#%module-begin
        do-provide-all
        x ...)]))

;; override htdp forms with stxerr

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
