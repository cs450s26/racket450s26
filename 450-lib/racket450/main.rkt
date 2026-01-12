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
                        rackunit
                        2htdp/image
                        2htdp/universe)
          ;; excluded names:
          #%module-begin
          set!
          define-values
          values
;          match-define
;          match-let
;          let*
          letrec)
         (rename-out
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
         2htdp/image
         2htdp/universe
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

;; override these htdp forms with err
;; - local
;; - check-expect
