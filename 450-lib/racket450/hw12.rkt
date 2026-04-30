#lang racket

(provide exn:fail:typecheck:cs450
         exn:fail:typecheck:cs450?
         exn:fail:syntax:cs450
         exn:fail:syntax:cs450?)
         
;; 450syntax errors
(struct exn:fail:typecheck:cs450  exn:fail:syntax [] )
(struct exn:fail:syntax:cs450     exn:fail:syntax [] )

