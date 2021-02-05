#lang clotho
;; Providing functions like `function-type-arg-type!`.
(provide
 product-type-inner-type-list!
 function-type-arg-type!
 function-type-return-type!
 structural-record-type-known-field-dict!
 nominal-record-type-name!
 nominal-record-type-super-record!
 nominal-record-type-known-field-dict!
 )

(require
 "types.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax
  ))

(define-syntax define-mutating-accessors
  (syntax-parser
    [(_ constructor-expression:expr accessor:id ...+)
     (with-syntax ([(mutating-accessor! ...)
                    (map (λ (accessor) (format-id accessor "~a!" accessor))
                         (syntax->list #'(accessor ...)))])
       #'(begin
           (define (mutating-accessor! t)
             (define constructed-type constructor-expression)
             (unify! constructed-type t)
             (accessor constructed-type))
           ...))]))

(define-mutating-accessors (product-type #f) product-type-inner-type-list)
(define-mutating-accessors
  (function-type (fresh-type-variable)
                 (fresh-type-variable))
  function-type-arg-type
  function-type-return-type)
(define-mutating-accessors
  (fresh-structural-record-type)
  structural-record-type-known-field-dict)
(define-mutating-accessors
  (any-nominal-record-type)
  nominal-record-type-name
  nominal-record-type-super-record
  nominal-record-type-known-field-dict)


(module+ test
  (require rackunit)
  (let ([the-type (base-type 'the-type)])
    (define (the-type? x)
      (can-unify? x the-type))
    (check-pred the-type?
                (function-type-arg-type!
                 (fresh-type-variable
                  (function-type the-type (fresh-type-variable)))))
    (check-pred the-type?
                (function-type-return-type!
                 (fresh-type-variable
                  (function-type (fresh-type-variable) the-type))))
    (check-pred the-type?
                (function-type-return-type!
                 (fresh-type-variable
                  (function-type (fresh-type-variable) the-type))))
    (check-equal? (length (product-type-inner-type-list!
                           (fresh-type-variable
                            (product-type (list (fresh-type-variable)
                                                (fresh-type-variable)
                                                (fresh-type-variable))))))
                  3)
    (check-equal? (product-type-inner-type-list!
                   (fresh-type-variable
                    (product-type #f)))
                  #f)))
