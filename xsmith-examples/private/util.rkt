#lang racket/base
(provide
 random-expr
 define-ag/one-arg
 Ectype
 )

(require
 xsmith
 racket/match
 syntax/parse/define
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



(define-syntax-parser define-ag/one-arg
  [(_ ag-name:id component-name:id racr-ize-id:expr
      no-exception-check-expr:expr
      type-default-stx
      mk-render-node:expr)
   #'(define-syntax-parser ag-name
       [(_ name:id
           (~or (~optional (~seq #:type type:expr)
                           #:defaults ([type type-default-stx]))
                (~optional (~seq #:ctype ctype:expr)
                           #:defaults ([ctype #'(λ (n t) (hash 'Expression t))]))
                (~optional (~seq #:racr-name racr-name:id)
                           #:defaults ([racr-name (racr-ize-id #'name)]))
                (~optional (~seq #:NE-name NE-name)
                           #:defaults ([NE-name #'name]))
                (~optional (~seq #:feature feature-arg)))
           (... ...))
        #'(add-to-grammar component-name
                          [racr-name Expression (Expression)
                                     #:prop type-info [type ctype]
                                     ((... ~?) ((... ~@) #:prop feature feature-arg))
                                     #:prop render-node-info
                                     (mk-render-node (λ () (if no-exception-check-expr
                                                               'NE-name
                                                               'name)))])])])


(define-syntax-parser Ectype
  [(_ etype:expr)
   #'(λ (n t) (hash 'Expression etype))])
