#lang racket/base

(provide (except-out
          (all-from-out racket/base
                        racket/list
                        racket/string)
          #%module-begin
          set!)
         (rename-out
          [mb450 #%module-begin]))

(require racket/list
         racket/string
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
