#lang racket/base
(provide
 random-expr
 )

(require
 racket/match
 (for-syntax
  racket/base
  syntax/parse
  ))

(define-syntax (random-expr stx)
  (syntax-parse stx
    [(_ e ...+)
     (define/syntax-parse count
       (datum->syntax #'here (length (syntax->list #'(e ...)))))
     (define/syntax-parse (index ...)
       (for/list ([e-stx (syntax->list #'(e ...))]
                  [i (in-naturals)])
         (datum->syntax #'here i)))
     #'(match (random count)
         [index e] ...)]))
