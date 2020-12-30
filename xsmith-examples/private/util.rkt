#lang clotho
(provide
 random-expr
 define-ag/one-arg
 define-ag/two-arg
 define-ag/three-arg
 Ectype
 E2ctype
 E3ctype
 define-ag/converter
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



;; These `ag` macros are a shorthand I started using when making the Racket kernel fuzzer as an add-to-grammar wrapper for quickly defining lots of builtin functions.
;; TODO - the ag macros should allow arbitraray #:prop arguments.
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
                           #:defaults ([ctype #'default-ctype/ag/one-arg]))
                (~optional (~seq #:racr-name racr-name:id)
                           #:defaults ([racr-name (racr-ize-id #'name)]))
                (~optional (~seq #:NE-name NE-name)
                           #:defaults ([NE-name #'name]))
                (~optional (~seq #:shallow shallow-arg))
                (~optional (~seq #:feature feature-arg)))
           (... ...))
        #`(add-to-grammar component-name
                          [racr-name Expression (Expression)
                                     #:prop type-info [type ctype]
                                     ((... ~?) ((... ~@) #:prop feature feature-arg))
                                     #,@(syntax-parse (or (attribute shallow-arg) #'#f)
                                          [#f #'()]
                                          [#t #'(#:prop wont-over-deepen #t
                                                 #:prop depth-increase 0)])
                                     #:prop render-node-info
                                     (mk-render-node (λ () (if no-exception-check-expr
                                                               'NE-name
                                                               'name)))])])])
(define default-ctype/ag/one-arg
  (λ (n t) (hash 'Expression t)))


;; TODO - if I could make Ectype somehow cache the resulting function I could
;; probably somewhat reduce code size (good for runtime) as well as memory
;; usage during compilation.
(define-syntax-parser Ectype
  [(_ etype:expr)
   #'(λ (n t) (hash 'Expression etype))])

(define-syntax-parser define-ag/two-arg
  [(_ ag-name:id component-name:id racr-ize-id:expr
      no-exception-check-expr:expr
      type-default-stx
      mk-render-node:expr)
   #'(define-syntax-parser ag-name
       [(_ name:id
           (~or (~optional (~seq #:type type:expr)
                           #:defaults ([type type-default-stx]))
                (~optional (~seq #:ctype ctype:expr)
                           #:defaults ([ctype #'default-ctype/ag/two-arg]))
                (~optional (~seq #:racr-name racr-name:id)
                           #:defaults ([racr-name (racr-ize-id #'name)]))
                (~optional (~seq #:NE-name NE-name)
                           #:defaults ([NE-name #'name]))
                (~optional (~seq #:feature feature-arg)))
           (... ...))
        #'(add-to-grammar component-name
                          [racr-name Expression ([l : Expression]
                                                 [r : Expression])
                                     #:prop type-info [type ctype]
                                     ((... ~?) ((... ~@) #:prop feature feature-arg))
                                     #:prop render-node-info
                                     (mk-render-node (λ () (if no-exception-check-expr
                                                               'NE-name
                                                               'name)))])])])
(define default-ctype/ag/two-arg
  (λ (n t) (hash 'l t 'r t)))
(define-syntax-parser E2ctype
  [(_ etypel:expr etyper:expr)
   #'(λ (n t) (hash 'l etypel 'r etyper))])

(define-syntax-parser define-ag/three-arg
  [(_ ag-name:id component-name:id racr-ize-id:expr
      no-exception-check-expr:expr
      type-default-stx
      mk-render-node:expr)
   #'(define-syntax-parser ag-name
       [(_ name:id
           (~or (~optional (~seq #:type type:expr)
                           #:defaults ([type type-default-stx]))
                (~optional (~seq #:ctype ctype:expr)
                           #:defaults ([ctype #'default-ctype/ag/three-arg]))
                (~optional (~seq #:racr-name racr-name:id)
                           #:defaults ([racr-name (racr-ize-id #'name)]))
                (~optional (~seq #:NE-name NE-name)
                           #:defaults ([NE-name #'name]))
                (~optional (~seq #:feature feature-arg)))
           (... ...))
        #'(add-to-grammar component-name
                          [racr-name Expression ([l : Expression]
                                                 [m : Expression]
                                                 [r : Expression])
                                     #:prop type-info [type ctype]
                                     ((... ~?) ((... ~@) #:prop feature feature-arg))
                                     #:prop render-node-info
                                     (mk-render-node (λ () (if no-exception-check-expr
                                                               'NE-name
                                                               'name)))])])])
(define default-ctype/ag/three-arg
  (λ (n t) (hash 'l t 'm t 'r t)))
(define-syntax-parser E3ctype
  [(_ etypel:expr etypem:expr etyper:expr)
   #'(λ (n t) (hash 'l etypel 'm etypem 'r etyper))])

(define-syntax-parser define-ag/converter
  [(_ ag-name:id ag/one-arg-name:id)
   #'(define-syntax-parser ag-name
       [(_ name:id from:expr to:expr
           (~or
            (~optional (~seq #:NE-name NE-name))
            (~optional (~seq #:racr-name racr-name))
            (~optional (~seq #:feature feature-arg))
            ) (... ...))
        #'(ag/one-arg-name name #:type to #:ctype (Ectype from) #:shallow #t
                           ((... ~?) ((... ~@) #:NE-name NE-name))
                           ((... ~?) ((... ~@) #:racr-name racr-name))
                           ((... ~?) ((... ~@) #:feature feature-arg)))])])
