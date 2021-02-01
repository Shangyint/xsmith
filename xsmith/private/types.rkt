#lang clotho
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2017-2020 The University of Utah
;; All rights reserved.
;;
;; This file is part of Xsmith, a generator of highly effective fuzz testers.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;   * Redistributions of source code must retain the above copyright notice,
;;     this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/contract)
(provide
 (rename-out [core-type-variable? type-variable?])

 ;(struct-out base-type)
 (contract-out
  [rename mk-base-type base-type (->* (any/c) (base-type? #:leaf? any/c)
                                      base-type?)])
 base-type-name
 base-type-supertype
 base-type-leaf?
 base-type?
 default-base-type

 ;(struct-out alias-type)
 ;; TODO - tuple types -- what should the API be?
 ;(rename-out [mk-record-type record-type])
 product-type?
 product-type-inner-type-list
 ;product-type-inner-type-list

 define-generic-type
 generic-type?
 generic-type-name
 generic-type-type-arguments

 ;nominal-record-type
 nominal-record-type-with
 any-nominal-record-type
 nominal-record-type?
 (rename-out
  [nominal-record-type-name/canonical nominal-record-type-name]
  [nominal-record-type-super-record/canonical nominal-record-type-super-record]
  [nominal-record-type-known-field-dict/canonical
   nominal-record-type-known-field-dict])

 structural-record-type?
 fresh-structural-record-type
 (rename-out
  [structural-record-type-known-field-dict/canonical
   structural-record-type-known-field-dict])

 (rename-out [make-nominal-record-definition nominal-record-definition-type])
 nominal-record-definition-type?
 nominal-record-definition-type-type
 ;nominal-record-definition-type?/with-field
 ;nominal-record-type?/with-field

 type?

 (contract-out
  [fresh-type-variable (->* () () #:rest (listof type?) type?)]
  [fresh-subtype-of (-> type? type?)]
  [fresh-supertype-of (-> type? type?)]
  [unify! (-> type? type? any/c)]
  [subtype-unify! (-> type? type? any/c)]
  [can-unify? (-> type? type? any/c)]
  [can-subtype-unify? (-> type? type? any/c)]
  [concretize-type (->* (type?)
                        (#:at-node ast-node?)
                        type?)]
  [rename settled? settled-type? (-> type? any/c)]
  [type-has-no-variables? (-> type? any/c)]
  [rename mk-product-type product-type (-> (or/c #f (listof type?)) type?)]
  [function-type (-> type? type? type?)]
  )
 function-type?
 function-type-arg-type
 function-type-return-type

 current-xsmith-type-constructor-thunks
 type-variable-subtype-default

 type->type-variable-list
 at-least-as-settled
 contains-type-variables?

 type-contains-function-type?

 replace-parametric-types-with-variables
 make-parametric-type-based-on
 )

(module+ for-private
  (provide
   type-variable->type
   record-type-max-fields
   type-max-concretization-depth
   ))

(require
 racket/match
 racket/dict
 racket/list
 racket/set
 "scope-graph.rkt"
 "xsmith-utils.rkt"
 racr
 (submod "xsmith-utils.rkt" for-private)
 (for-syntax
  clotho/racket/base
  syntax/parse
  racket/syntax
  ))
(module+ test (require rackunit))


#|
Type variables contain:

a "type", which is either:
• #f - unconstrained
• a list - constrained to be one of the types in the list
A type variable list may not contain type variables and may not contain more than one of each compound type.  Eg. it may contain any number of base types but only one function type or record type.  However, the function or record type contained my be only partially specified (eg. may contain type variables).

A forward field, which is #f when the type variable has not been forwarded to a different one, or points to another type variable.  IE, because multiple type variables can be unified together, they need to become one.  But I still have references to the old ones lying around.

And upper and lower bounds lists (for subtyping).  The bounds lists include any number of other type variables.

When type variables are subtype-unified, the variables are set in each other's upper/lower bound list.  Their type lists are filtered only to types that have a sub/super type in the other list.  This also affects transitive upper/lower bounds.
If a (transitive) upper bound is ever equal to a (transitive) lower bound, that part of the lattice is unified into one type-variable-innard.

|#

(struct core-type-variable
  ([forward #:mutable]
   [type #:mutable]
   [lower-bounds #:mutable]
   [upper-bounds #:mutable])
  #:methods gen:custom-write
  [(define (write-proc v output-port output-mode)
     (match v
       [(c-type-variable t _ _)
        (fprintf output-port
                 "#<type-variable ~a>"
                 t)]))])

(define-match-expander c-type-variable
  (syntax-parser
    [(_ type lowers uppers)
     #'(and (? core-type-variable?)
            (app (λ (v)
                   (let ([cv (variable-canonicalize v)])
                     (values
                      (core-type-variable-type cv)
                      (core-type-variable-lower-bounds cv)
                      (core-type-variable-upper-bounds cv))))
                 type lowers uppers))]))

(define (type-variable->type tv)
  (match tv
    [(c-type-variable (list the-type) _ _)
     the-type]
    [else #f]))
(define (c-type-variable-type tv)
  (core-type-variable-type (variable-canonicalize tv)))
(define (c-type-variable-lower-bounds tv)
  (core-type-variable-lower-bounds (variable-canonicalize tv)))
(define (c-type-variable-upper-bounds tv)
  (core-type-variable-upper-bounds (variable-canonicalize tv)))



(define (variable? v)
  ;; TODO - I should have a predicate for whether something is one of these types and a separate predicate for whether they are still variable or are finalized.
  (or (core-type-variable? v)
      (structural-record-type? v)
      (nominal-record-type? v)
      (product-type? v)))

(define (variable-lower-bounds v)
  (cond
    [(core-type-variable? v) (core-type-variable-lower-bounds v)]
    [(product-type? v) (product-type-lower-bounds v)]
    [(structural-record-type? v) (structural-record-type-lower-bounds v)]
    [(nominal-record-type? v) (nominal-record-type-lower-bounds v)]
    [else (error 'variable-lower-bounds! "received non-variable value: ~v" v)]))
(define (variable-upper-bounds v)
  (cond
    [(core-type-variable? v) (core-type-variable-upper-bounds v)]
    [(product-type? v) (product-type-upper-bounds v)]
    [(structural-record-type? v) (structural-record-type-upper-bounds v)]
    [(nominal-record-type? v) (nominal-record-type-upper-bounds v)]
    [else (error 'variable-upper-bounds! "received non-variable value: ~v" v)]))
(define ((variable-DIR-bounds! get-dir set-dir!) innard)
  ;; This version updates forwarded bounds.
  (define ret1 (get-dir innard))
  (define ret2 (map variable-canonicalize ret1))
  (define ret3 (remove-duplicates ret2))
  (set-dir! innard ret3)
  ret3)
(define (variable-lower-bounds! v)
  (cond
    [(core-type-variable? v)
     ((variable-DIR-bounds! core-type-variable-lower-bounds
                            set-core-type-variable-lower-bounds!)
      v)]
    [(product-type? v)
     ;; TODO - product-type is currently not treated like the others
     (product-type-lower-bounds v)]
    [(structural-record-type? v)
     ((variable-DIR-bounds! structural-record-type-lower-bounds
                            set-structural-record-type-lower-bounds!)
      v)]
    [(nominal-record-type? v)
     ((variable-DIR-bounds! nominal-record-type-lower-bounds
                            set-nominal-record-type-lower-bounds!)
      v)]
    [else (error 'variable-lower-bounds! "received non-variable value: ~v" v)]))
(define (variable-upper-bounds! v)
  (cond
    [(core-type-variable? v)
     ((variable-DIR-bounds! core-type-variable-upper-bounds
                            set-core-type-variable-upper-bounds!)
      v)]
    ;; TODO - product-type is currently not treated like the others
    [(product-type? v) (product-type-upper-bounds v)]
    [(structural-record-type? v)
     ((variable-DIR-bounds! structural-record-type-upper-bounds
                            set-structural-record-type-upper-bounds!)
      v)]
    [(nominal-record-type? v)
     ((variable-DIR-bounds! nominal-record-type-upper-bounds
                            set-nominal-record-type-upper-bounds!)
      v)]
    [else (error 'variable-upper-bounds! "received non-variable value: ~v" v)]))

(define ((variable->transitive-DIR-bounds dir) tvi)
  (define immediates (dir tvi))
  (let loop ([transitive-members immediates]
             [work-list immediates])
    (cond [(null? work-list) transitive-members]
          [else
           (define maybe-new-ones (dir (car work-list)))
           (define new-ones (listeq-subtract maybe-new-ones transitive-members))
           (define new-transitive-members (set-union transitive-members new-ones))
           (define new-work-list (append new-ones (cdr work-list)))
           (loop new-transitive-members
                 new-work-list)])))

(define (variable-transitive-lower-bounds v)
  (cond
    [(or (core-type-variable? v)
         (product-type? v)
         (structural-record-type? v)
         (nominal-record-type? v))
     ((variable->transitive-DIR-bounds variable-lower-bounds!) v)]
    [else (error 'variable-transitive-lower-bounds
                 "received non-variable value: ~v" v)]))
(define (variable-transitive-upper-bounds v)
  (cond
    [(or (core-type-variable? v)
         (product-type? v)
         (structural-record-type? v)
         (nominal-record-type? v))
     ((variable->transitive-DIR-bounds variable-upper-bounds!) v)]
    [else (error 'variable-transitive-upper-bounds
                 "received non-variable value: ~v" v)]))

(define (variable-canonicalize v)
  (define (canonicalize accessor thing)
    (define fwd (accessor thing))
    (if fwd
        (canonicalize accessor fwd)
        thing))
  (cond
    [(core-type-variable? v) (canonicalize core-type-variable-forward v)]
    [(structural-record-type? v)
     (canonicalize structural-record-type-canonical-forward v)]
    [(nominal-record-type? v)
     (canonicalize nominal-record-type-canonical-forward v)]
    [(product-type? v) v]
    [else (error 'variable-canonicalize "received non-variable value: ~v" v)]))

(define (canonicalize-if-variable t)
  (if (variable? t)
      (variable-canonicalize t)
      t))

(define type-variable-subtype-default
  (make-parameter #t))

(define (fresh-type-variable #:subtype? [subtype? (type-variable-subtype-default)]
                             . args)
  (when (memf core-type-variable? args)
    (error 'fresh-type-variable
           "partially constrained type variables can not include type variables in the constraint list.  Given ~a\n"
           args))
  (define (composite-error)
    (error 'fresh-type-variable
           "partially constrained type variables can not include multiple of any composite type in the constraint list.  Given ~a\n"
           args))
  (when (<= 2 (length (filter function-type? args))) (composite-error))
  (when (<= 2 (length (filter product-type? args))) (composite-error))
  ;(when (<= 2 (length (filter sum-type? args))) (composite-error))
  (when (<= 2 (length (filter nominal-record-type? args))) (composite-error))
  ;(when (<= 2 (length (filter record-type? args))) (composite-error))
  ;; TODO - I probably only want to allow one of each kind of generic type
  (define type (if (null? args)
                   #f
                   (map (λ (x) (if (base-type? x)
                                   (if subtype?
                                       (mk-base-type-range #f x)
                                       (mk-base-type-range x x))
                                   x))
                        args)))
  (core-type-variable #f type '() '()))

(define (fresh-subtype-of v)
  (define fv (fresh-type-variable))
  (subtype-unify! fv v)
  fv)
(define (fresh-supertype-of v)
  (define fv (fresh-type-variable))
  (subtype-unify! v fv)
  fv)

#|
Base types can be declared as subtypes of other base types.
Inside a type variable, they are always placed in a base-type-range, which gives a minimum and maximum type.
The minimum may be #f to mean any subtype of the maximum type.
|#

(struct base-type (name supertype leaf?) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "#<~a>" (base-type-name self)))])

;; a default base type, for when users don't specify any type rules.
(define default-base-type (base-type 'xsmith_default-base-type #f #t))

(module+ test
  ;; Define some base types for testing
  (define animal (mk-base-type 'animal #:leaf? #f))
  (define bird (mk-base-type 'bird animal #:leaf? #f))
  (define penguin (mk-base-type 'penguin bird #:leaf? #f))
  (define canine (mk-base-type 'canine animal #:leaf? #f))
  (define dog (mk-base-type 'dog canine #:leaf? #f))
  (define labradoodle (mk-base-type 'labradoodle dog #:leaf? #t))
  )

(define (base-type->parent-chain bt)
  (if (not (base-type-supertype bt))
      (list bt)
      (cons bt (base-type->parent-chain (base-type-supertype bt)))))

(define (mk-base-type name [parent #f] #:leaf? [leaf? #t])
  (when (and parent (base-type-leaf? parent))
    (error 'base-type "can't create a subtype of leaf type: ~v" parent))
  (base-type name parent leaf?))

(define (base-type->superest bt)
  (if (not (base-type-supertype bt))
      bt
      (base-type->superest (base-type-supertype bt))))

(define (base-type-least-upper-bound a b)
  (match (list a b)
    [(list #f _) b]
    [(list _ #f) a]
    [else
     (define chain-a (base-type->parent-chain a))
     (define chain-b (base-type->parent-chain b))
     (define len-a (length chain-a))
     (define len-b (length chain-b))
     (define chain-a* (if (< len-a len-b)
                          chain-a
                          (drop chain-a (- len-a len-b))))
     (define chain-b* (if (< len-b len-a)
                          chain-b
                          (drop chain-b (- len-b len-a))))
     (let loop ([as chain-a*]
                [bs chain-b*])
       (cond [(null? as) (error 'base-type-least-upper-bound
                                "incompatible base types: ~v and ~v" a b)]
             [(equal? as bs) (car as)]
             [else (loop (cdr as) (cdr bs))]))]))

(define (base-type-greatest-lower-bound a b)
  (match (list a b)
    [(list #f _) a]
    [(list _ #f) b]
    [else
     (define chain-a (base-type->parent-chain a))
     (define chain-b (base-type->parent-chain b))
     (define len-a (length chain-a))
     (define len-b (length chain-b))
     (define chain-a* (if (< len-a len-b)
                          chain-a
                          (drop chain-a (- len-a len-b))))
     (define chain-b* (if (< len-b len-a)
                          chain-b
                          (drop chain-b (- len-b len-a))))
     (if (equal? chain-a* chain-b*)
         (if (< len-a len-b)
             b
             a)
         (error 'base-type-greatest-lower-bound
                "incompatible base types: ~v and ~v" a b))]))

(struct base-type-range (sub super)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "#<range:~a-~a>"
              (base-type-range-sub self)
              (base-type-range-super self)))])
(define (mk-base-type-range min max)
  (if (base-type-leaf? max)
      (base-type-range max max)
      (base-type-range min max)))

(struct function-type (arg-type return-type)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "#<function-type ~a → ~a>"
              (function-type-arg-type self)
              (function-type-return-type self)))])


;; Product types may have #f as the inner list to specify that the length and inner types are yet unconstrained.
#|
inner-type-list may be:
• #f for unconstrained
• a list of types (which may contain type variables)
upper-bounds and lower-bounds are lists of other product types that a given one has been subtype-unified with.  Once any product type among these has a list (instead of #f) for its inner type list, all the others will have a list of the same length created and filled with type variables.  Then they are all subtype-unified.
|#

(struct product-type
  ([inner-type-list #:mutable]
   [lower-bounds #:mutable]
   [upper-bounds #:mutable])
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "#<product-type ~a>"
              (product-type-inner-type-list self)))])
(define (mk-product-type inners)
  (if (not inners)
      (product-type #f '() '())
      (product-type inners '() '())))
(define (product-type->all-transitive-bounds pt)
  (define (work dones todos)
    (cond [(null? todos) dones]
          [(memq (car todos) dones) (work dones (cdr todos))]
          [else
           (match (car todos)
             [(product-type _ lbs ubs)
              (work (cons (car todos) dones)
                    (append lbs ubs (cdr todos)))])]))
  (work '() (list pt)))
;(struct sum-type (inner-type-list) #:transparent)


#|
TODO - when generating a record ref, I'll need to compare something like (record-with-a-field-of-type t) to available record types.
|#
#;(struct record-type (name scope) #:transparent)
#;(define (mk-record-type #:name [name #f] name-type-dict)
    (record-type name (scope #f
                             (map (λ (k) (binding k
                                                  #f
                                                  (dict-ref name-type-dict k)
                                                  'definition))
                                  (dict-keys name-type-dict))
                             '())))

(struct nominal-record-type
  ;; If name is not #f, known-field-dict is an ordered dict of name to type.
  ;; If name is #f, then the type is variable and known-field-dict is still an ordered dict, but names may be #f to just specify that an int needs to be available.
  ;; By ordered dict, I specifically mean an alist.
  ;; TODO - right now *actually* ordered dict just means a dictionary with no actual order.  At some point I intended there to be an order.  But clearly I never did that.  Maybe I will?
  (canonical-forward name super-record known-field-dict lower-bounds upper-bounds)
  #:mutable
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc v output-port output-mode)
     (match v
       [(c-nominal-record-type name super kfd lb ub)
        (fprintf output-port
                 "#<nominal-record-type name/final?:~a, fields: ~s>"
                 name
                 kfd)]))])

(define (nominal-record-type-with field type)
  (nominal-record-type #f #f #f (hash field type) '() '()))
(define (any-nominal-record-type)
  (nominal-record-type #f #f #f (hash) '() '()))

(define (nominal-record-type-known-field-dict/canonical x)
  (nominal-record-type-known-field-dict (variable-canonicalize x)))
(define (nominal-record-type-name/canonical x)
  (nominal-record-type-name (variable-canonicalize x)))
(define (nominal-record-type-super-record/canonical x)
  (nominal-record-type-super-record (variable-canonicalize x)))

(define-match-expander c-nominal-record-type
  (syntax-parser
    [(_ name/final? super-record known-field-dict lowers uppers)
     #'(and (? nominal-record-type?)
            (app (λ (nrt)
                   (let ([cnrt (variable-canonicalize nrt)])
                     (values
                      (nominal-record-type-name cnrt)
                      (nominal-record-type-super-record cnrt)
                      (nominal-record-type-known-field-dict cnrt)
                      (nominal-record-type-lower-bounds cnrt)
                      (nominal-record-type-upper-bounds cnrt))))
                 name/final? super-record known-field-dict lowers uppers))]))

(struct nominal-record-definition-type
  ;; This is a wrapper to be the type for the definition site of a nominal-record-type.
  ;; IE instances of a nominal record use the `inner` type, but nominal types need
  ;; definitions themselves.  This is the type for those definitions...
  (type)
  #:transparent)
(define (make-nominal-record-definition nominal-record-type)
  ;; TODO - this should be verified to be actually a nominal-record-type
  (nominal-record-definition-type nominal-record-type))

(define (nominal-record-type?/with-field nrt)
  ;; predicate for when a NRT type has a named field but not a name overall.
  (and (nominal-record-type? nrt)
       (not (nominal-record-type-name nrt))
       (not (null? (filter (λ(x)x)
                           (dict-keys (nominal-record-type-known-field-dict nrt)))))))


(struct structural-record-type
  (canonical-forward finalized? known-field-dict lower-bounds upper-bounds)
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc v output-port output-mode)
     (match v
       [(c-structural-record-type finalized? kfd lb ub)
        (fprintf output-port
                 "#<structural-record-type final?:~a, fields: ~s>"
                 finalized?
                 kfd)]))])

(define (fresh-structural-record-type [field-dict (hash)]
                                      #:finalized? [finalized? #f])
  (structural-record-type #f finalized? field-dict '() '()))

(define-match-expander c-structural-record-type
  (syntax-parser
    [(_ final? known-field-dict lowers uppers)
     #'(and (? structural-record-type?)
            (app (λ (srt)
                   (let ([csrt (variable-canonicalize srt)])
                     (values
                      (structural-record-type-finalized? csrt)
                      (structural-record-type-known-field-dict csrt)
                      (structural-record-type-lower-bounds csrt)
                      (structural-record-type-upper-bounds csrt))))
                 final? known-field-dict lowers uppers))]))

(define (structural-record-type-known-field-dict/canonical x)
  (structural-record-type-known-field-dict (variable-canonicalize x)))

;; Generic types are given a name which is a symbol.  But it is just for printing.
;; They are compared with `eq?` on their constructor, which is bound to the name
;; by `define-generic-type`.
(struct generic-type (name constructor type-arguments subtype-variances)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc v output-port output-mode)
     (match v
       [(generic-type n ctor ts vs)
        (fprintf output-port "#<~a ~s>" n ts)]))])

(begin-for-syntax
  (define-syntax-class generic-field-spec
    (pattern field:id
             #:attr variance #'invariant)
    (pattern [field:id (~and variance (~or (~datum covariant)
                                           (~datum contravariant)
                                           (~datum invariant)))])))
(define-syntax (define-generic-type stx)
  (syntax-parse stx
    [(_ name:id (field-spec:generic-field-spec ...))
     (define field-length (length (syntax->list #'(field-spec ...))))
     (with-syntax ([(accessor-name ...) (map (λ (x) (format-id #'name
                                                               "~a-~a"
                                                               #'name
                                                               x))
                                             (syntax->list #'(field-spec.field ...)))]
                   [(accessor-index ...)
                    (map (λ (x) (datum->syntax
                                 #'name
                                 (- field-length
                                    (length
                                     (member x (syntax->list
                                                #'(field-spec.field ...)))))))
                         (syntax->list #'(field-spec.field ...)))]
                   [predicate-name (format-id #'name "~a?" #'name)]
                   [field-length-stx (datum->syntax #f field-length)])
       #'(begin
           (define (name field-spec.field ...)
             (generic-type 'name name
                           (list field-spec.field ...)
                           (list 'field-spec.variance ...)))
           (define (predicate-name x)
             (and (generic-type? x)
                  (eq? name (generic-type-constructor x))))
           (define (accessor-name x)
             (when (not (predicate-name x))
               (error 'accessor-name "not a ~a: ~a" 'predicate-name x))
             (list-ref (generic-type-type-arguments x) accessor-index))
           ...))]))

(struct parameter-type (symbol))
(define (make-fresh-parameter-type)
  (parameter-type (gensym 'param)))

;; TODO - maybe I should have a base struct with no fields called type, then allow the user to define their own new types with custom rules for subtyping (at least to specify which fields are covariant, contravariant, or invariant) and for where to recur during unification.
(define (type? x)
  (or
   (core-type-variable? x)
   (base-type? x)
   (base-type-range? x)
   (function-type? x)
   (product-type? x)
   ;(sum-type? x)
   (nominal-record-type? x)
   (nominal-record-definition-type? x)
   (structural-record-type? x)
   (generic-type? x)
   (parameter-type? x)
   ))


(define (type->skeleton-with-vars t)
  (match t
    [(generic-type name constructor inners variances)
     (apply constructor (map (λ(x) (fresh-type-variable))
                             inners))]
    [(? product-type?) (mk-product-type #f)]
    [(? nominal-record-type?) (any-nominal-record-type)]
    [(? nominal-record-definition-type?) (nominal-record-definition-type
                                          (any-nominal-record-type))]
    [(? structural-record-type?)
     (fresh-structural-record-type)]
    [(? function-type?) (function-type (fresh-type-variable) (fresh-type-variable))]))

(define (base-type-ranges->unified-versions sub super)
  ;; returns a list of a new sub-range and a new super-range if compatible,
  ;; otherwise return #f
  (match (list sub super)
    [(list (base-type-range lsub lsup) (base-type-range rsub rsup))
     (and (can-subtype-unify? sub super)
          (let ()
            ;; lsup must not be higher than rsup -- IE the upper bound of the supertype is also an upper bound on the subtype.
            (define new-lsup (base-type-greatest-lower-bound lsup rsup))
            ;; rsub must not be lower than lsub -- IE the lower bound of the subtype is also a lower bound on the supertype.
            (define new-rsub (base-type-least-upper-bound lsub rsub))
            (define new-l (mk-base-type-range lsub new-lsup))
            (define new-r (mk-base-type-range new-rsub rsup))
            (list new-l new-r)))]
    [else #f]))
(module+ test
  (check-equal? (base-type-ranges->unified-versions
                 (mk-base-type-range #f dog)
                 (mk-base-type-range labradoodle canine))
                (list (mk-base-type-range #f dog)
                      (mk-base-type-range labradoodle canine)))
  (check-equal? (base-type-ranges->unified-versions
                 (mk-base-type-range labradoodle canine)
                 (mk-base-type-range #f dog))
                (list (mk-base-type-range labradoodle dog)
                      (mk-base-type-range labradoodle dog)))
  (check-equal? (base-type-ranges->unified-versions
                 (mk-base-type-range dog canine)
                 (mk-base-type-range #f dog))
                (list (mk-base-type-range dog dog)
                      (mk-base-type-range dog dog)))
  (check-equal? (base-type-ranges->unified-versions
                 (mk-base-type-range penguin penguin)
                 (mk-base-type-range #f bird))
                (list (mk-base-type-range penguin penguin)
                      (mk-base-type-range penguin bird)))
  (check-equal? (base-type-ranges->unified-versions
                 (mk-base-type-range #f bird)
                 (mk-base-type-range penguin penguin))
                (list (mk-base-type-range #f penguin)
                      (mk-base-type-range penguin penguin)))
  )
(define (type-lists->unified-base-types sub-list super-list)
  (define result
    (filter (λ(x)x)
            (for*/list ([sub sub-list]
                        [sup super-list])
              (base-type-ranges->unified-versions sub sup))))
  (define new-subs (map first result))
  (define new-sups (map second result))
  ;; TODO - filter out any that are sub-ranges within each of these lists
  (list new-subs new-sups))

(struct exn:fail:subtype-unify exn:fail ())
(define (errsu message-format-string . args)
  (raise (exn:fail:subtype-unify
          (string-append "subtype-unify!: " (apply format message-format-string args))
          (current-continuation-marks))))

(define (subtype-unify! sub* super*)
  #|
  * This sets variables to be in each others' upper- and lower-bounds.
  * As variables are unified, possibilities that don't fit with variables they are unified with (or settled types they are unified with) are filtered out.
  * Unification transitively affects all upper and lower bounds of a variable.
  * Subtype-unified type variables form a lattice, and any time a lower bound becomes an upper bound (or vice-versa), the lattice between those two nodes is squashed to a single node.
  * recursion into inner type structures (function, generic, etc) will operate on type-specific meanings of subtyping -- generics will have a way of specifying per field whether the field is invariant (the default), covariant, or contravariant
  |#
  #;(define can-unify-result
      (can-subtype-unify? sub super))
  #;(when (not can-unify-result)
      (xd-printf "\nWarning: subtype-unify! called when can-subtype-unify? claims unification is impossible.  If this doesn't error, there's a problem.\n")
      (xd-printf "Called with subtype: ~v, supertype: ~v\n" sub super))
  (define sub (canonicalize-if-variable sub*))
  (define super (canonicalize-if-variable super*))
  (define (rec sub super)
    (subtype-unify! sub super))
  (with-handlers ([(λ(e)#t)
                   (λ(e)
                     #;(when can-unify-result
                         (xd-printf "\n\nsubtype-unify! errored even though can-subtype-unify? said it should pass!\n\n"))
                     (raise e))])
    (match (list sub super)
      ;; type variable x2
      [(list (c-type-variable _ _ _)
             (c-type-variable _ _ _))
       (subtype-unify!/two-type-variables/type-variable-innard sub super)]
      ;; type variable left
      [(list (c-type-variable inner-t lb ub)
             _)
       (match inner-t
         [(list possibilities ...)
          (define new-possibilities
            (filter (λ (p) (can-subtype-unify? p super))
                    possibilities))
          (set-core-type-variable-type!
           sub
           (match new-possibilities
             [(list) (errsu "can't unify these types: ~v and ~v"
                            sub super)]
             [(list (? base-type-range?) ...)
              (define super-range (match super
                                    [(base-type-range _ _) super]
                                    [(base-type _ _ _) (mk-base-type-range #f super)]))
              (define new-ranges
                (filter-map
                 (λ (sub)
                   (define x (base-type-ranges->unified-versions sub super-range))
                   (and x (car x)))
                 new-possibilities))
              (match new-ranges
                [(list) (errsu "can't unify types: ~v and ~v (this error hopefully is unreachable...)"
                               sub super)]
                [(list one) new-ranges]
                [else new-ranges])]
             [(list non-base)
              (rec non-base super)
              (list non-base)]))]
         [#f (match super
               [(base-type-range super-low super-high)
                (set-core-type-variable-type!
                 sub
                 (list (mk-base-type-range #f super-high)))]
               [(? base-type?)
                (set-core-type-variable-type!
                 sub
                 (list (mk-base-type-range #f super)))]
               [else
                (set-core-type-variable-type!
                 sub
                 (list (type->skeleton-with-vars super)))
                (rec (car (core-type-variable-type sub)) super)])])

       (when (not (equal? inner-t (core-type-variable-type sub)))
         (ripple-subtype-unify-changes/type-variable '() (list sub)))]

      ;; type variable right -- this code is basically the same as the above... maybe it could be unified better...
      [(list _
             (c-type-variable inner-t lb ub))

       (match inner-t
         [(list possibilities ...)
          (define new-possibilities
            (filter (λ (p) (can-subtype-unify? sub p))
                    possibilities))
          (set-core-type-variable-type!
           super
           (match new-possibilities
             [(list) (errsu "can't unify the following types: ~v and ~v"
                            sub super)]
             [(list (? base-type-range?) ...)
              (define sub-range
                (match sub
                  [(base-type-range _ _) sub]
                  [(base-type _ _ _) (mk-base-type-range sub sub)]))
              (define new-ranges
                (filter-map
                 (λ (super)
                   (define x (base-type-ranges->unified-versions sub-range super))
                   (and x (cadr x)))
                 new-possibilities))
              (match new-ranges
                [(list) (errsu "can't unify types: ~v and ~v (this error hopefully is unreachable...)"
                               sub super)]
                [(list one) new-ranges]
                [else new-ranges])]
             [(list non-base)
              (rec sub non-base)
              (list non-base)]))]
         [#f (match sub
               [(base-type-range sub-low sub-high)
                (set-core-type-variable-type!
                 super
                 (list (mk-base-type-range sub-high (base-type->superest sub-high))))]
               [(? base-type?)
                (set-core-type-variable-type!
                 super
                 (list (mk-base-type-range sub (base-type->superest sub))))]
               [else
                (set-core-type-variable-type!
                 super
                 (list (type->skeleton-with-vars sub)))
                (rec sub (car (core-type-variable-type super)))])])

       (when (not (equal? inner-t (core-type-variable-type super)))
         (ripple-subtype-unify-changes/type-variable '() (list super)))]


      ;; product type
      [(list (product-type inner1 lowers1 uppers1)
             (product-type inner2 lowers2 uppers2))

       (define (inner-unify! sub super)
         (for-each (λ (l r) (rec l r))
                   (product-type-inner-type-list sub)
                   (product-type-inner-type-list super)))

       (define (ripple-length! len pt done-list)
         ;; Propagate length to all related product types.
         ;; At each step, unify the lists.
         ;; This basically initializes all of the inner lists of a graph
         ;; of product-types that had been subtype-unified to each other
         ;; with none of them having a settled length yet.
         ;; Once they are initialized, the inner type variables can carry
         ;; all the info about subtype relations, and the outer product types
         ;; are free to be simple lists.
         (if (memq pt done-list)
             done-list
             (let ([supers (product-type-upper-bounds pt)]
                   [subs (product-type-lower-bounds pt)])
               (set-product-type-inner-type-list!
                pt
                (map (λ (x) (fresh-type-variable))
                     (make-list len #f)))
               (define done-list-1
                 (for/fold ([done-list (cons pt done-list)])
                           ([super supers])
                   (define new-list (ripple-length! len super done-list))
                   (inner-unify! pt super)
                   new-list))
               (define done-list-2
                 (for/fold ([done-list done-list-1])
                           ([sub subs])
                   (define new-list (ripple-length! len sub done-list))
                   (inner-unify! sub pt)
                   new-list))
               (set-product-type-upper-bounds! pt '())
               (set-product-type-lower-bounds! pt '())
               done-list-2)))

       (define l1 (and inner1 (length inner1)))
       (define l2 (and inner2 (length inner2)))

       (when (and l1 l2 (not (equal? l1 l2)))
         (errsu "Tried to unify two product types with unequal lengths (~v and ~v): ~v, ~v"
                l1 l2
                sub super))

       (match (list inner1 inner2)
         [(list #f #f)
          (when (not (memq super (product-type-upper-bounds sub)))
            (set-product-type-upper-bounds!
             sub
             (cons super (product-type-upper-bounds sub))))
          (when (not (memq sub (product-type-lower-bounds super)))
            (set-product-type-lower-bounds!
             super
             (cons sub (product-type-lower-bounds super))))]
         [(list #f _)
          (ripple-length! l2 sub '())
          (inner-unify! sub super)]
         [(list _ #f)
          (ripple-length! l1 super '())
          (inner-unify! sub super)]
         [else (inner-unify! sub super)])]


      ;; nominal record type
      [(list (nominal-record-type c-forward1 name1 super1 kfd1 lb1 ub1)
             (nominal-record-type c-forward2 name2 super2 kfd2 lb2 ub2))
       (subtype-unify!/two-type-variables/nominal-record-type sub super)
       ;; TODO - nominal record types for the first pass should not be subtypable.  It should be easy to later add a supertype field -- subtyping with nominal records should be easy compared to various other things.
       ;;(todo-code "If both are fully specified I can just check that they are equal, otherwise I need to check that the partial specification fits and mutate if the other is fully specified.")
       ;; TODO - the below is the implementation of non-subtype `unify!`.  For now, let's assume nominal records don't subtype unify, only normal unify.
       #;(define (fail)
           (errsu "can't unify types: ~v and ~v"
                  sub super))
       #;(define t1 sub)
       #;(define t2 super)

       #;(match (list sub super)
           [(list (nominal-record-type #f inners1) (nominal-record-type name2 inners2))
            ;; TODO - do a sanity check that the inners match up and error if they don't.
            (set-nominal-record-type-name! t1 name2)
            (set-nominal-record-type-inners! t1 inners2)]
           [(list (nominal-record-type name1 inners1) (nominal-record-type #f inners2))
            ;; TODO - do a sanity check that the inners match up and error if they don't.
            (set-nominal-record-type-name! t2 name1)
            (set-nominal-record-type-inners! t2 inners1)]
           [(list (nominal-record-type name1 inners1) (nominal-record-type name2 inners2))
            (when (not (equal? name1 name2))
              (fail))])]
      ;; nominal record definition type
      [(list (nominal-record-definition-type inner1)
             (nominal-record-definition-type inner2))
       (rec inner1 inner2)]
      ;; Structural records
      [(list (c-structural-record-type f?1 known-fields-1 lb-1 ub-1)
             (c-structural-record-type f?2 known-fields-2 lb-2 ub-2))
       (subtype-unify!/two-type-variables/structural-record-type sub super)]
      ;; function type
      [(list (function-type arg-l ret-l)
             (function-type arg-r ret-r))
       ;; covariant return
       (rec ret-l ret-r)
       ;; contravariant arguments
       (rec arg-r arg-l)]
      ;; generic type
      [(list (generic-type name1 constructor1 type-arguments1 variances1)
             (generic-type name2 constructor2 type-arguments2 variances2))
       (unless (eq? constructor1 constructor2)
         (errsu "TODO - better message -- tried to unify different generic types."))
       (for-each (λ (isub isuper variance)
                   (match variance
                     ['invariant (begin
                                   (rec isub isuper)
                                   (rec isuper isub))]
                     ['covariant (rec isub isuper)]
                     ['contravariant (rec isuper isub)]))
                 type-arguments1
                 type-arguments2
                 variances1)]

      ;; parameter type
      [(list (parameter-type sym-l) (parameter-type sym-r))
       (when (not (eq? sym-l sym-r))
         (errsu "Unification of different parameter types failed: ~v and ~v"
                sub super))]
      ;; base type
      [(list (or (? base-type?) (? base-type-range?))
             (or (? base-type?) (? base-type-range?)))
       (unless (can-subtype-unify? sub super)
         (errsu "Base types: type ~v is not a subtype of type ~v."
                sub super))]
      [else (errsu "case analysis reached end: can't unify types: ~v and ~v" sub super)])))

(define (mk-ripple-subtype-unify-changes subtype-unify!-func)
  (define (done-pair-list-remove-with done-list target)
    ;; filter the done list to elements that don't include the target
    ;; TODO - this could be really slow since it will be done frequently.  If so, I should change the representation to be a pair of hash tables, maybe?
    (filter
     (λ (pair) (and (not (equal? (car pair) target))
                    (not (equal? (cdr pair) target))))
     done-list))
  (define (ripple-subtype-unify-changes done-pair-list work-list)
    (if (null? work-list)
        (void)
        (let ([innard (variable-canonicalize (car work-list))]
              [work-list (cdr work-list)])
          (define (fold-body dones work lower upper)
            (cond
              [(member (cons lower upper)
                       dones
                       (λ (x y) (and (eq? (car x) (car y))
                                     (eq? (cdr x) (cdr y)))))
               (values dones work)]
              [else
               (match-define (list subchange superchange)
                 (subtype-unify!-func lower upper))
               (define new-dones1 (if subchange
                                      (done-pair-list-remove-with dones lower)
                                      dones))
               (define new-dones2 (if superchange
                                      (done-pair-list-remove-with new-dones1 upper)
                                      new-dones1))
               (define new-dones3 (cons (cons lower upper) new-dones2))
               (define new-work1 (if subchange
                                     (set-add work lower)
                                     work))
               (define new-work2 (if superchange
                                     (set-add new-work1 upper)
                                     new-work1))
               (values new-dones3
                       new-work2)]))
          (define-values (dones1 work1)
            (for/fold ([dones done-pair-list]
                       [work work-list])
                      ([lower (variable-lower-bounds! innard)])
              (fold-body dones work lower innard)))
          (define-values (dones2 work2)
            (for/fold ([dones dones1]
                       [work work1])
                      ([upper (variable-upper-bounds! innard)])
              (fold-body dones work innard upper)))
          (ripple-subtype-unify-changes dones2 work2))))
  ripple-subtype-unify-changes)

(define ((mk-subtype-unify!/two-type-variables
          set-lowers!
          set-uppers!
          squash!
          subtype-unify!-func
          ripple-changes)
         sub super)
  ;; TODO - check that one is not recursively contained in the structure of the other.
  (define tvi-sub sub)
  (define tvi-sup super)
  (define tvi-sub-uppers (variable-transitive-upper-bounds tvi-sub))
  (define tvi-sub-lowers (variable-transitive-lower-bounds tvi-sub))
  (define already-done?
    (or (eq? tvi-sub tvi-sup)
        (memq tvi-sup tvi-sub-uppers)))
  (define squash-case?
    ;; When a lower bound needs to become an upper bound, it means they need to be unified/squashed.
    (memq tvi-sup tvi-sub-lowers))

  (cond
    [already-done? (void)]
    [squash-case? (squash! tvi-sub tvi-sup)]
    [else
     (set-uppers!
      tvi-sub
      (cons tvi-sup (variable-upper-bounds! tvi-sub)))
     (set-lowers!
      tvi-sup
      (cons tvi-sub (variable-lower-bounds! tvi-sup)))

     (define dones (list (cons tvi-sub tvi-sup)))
     (match (subtype-unify!-func tvi-sub tvi-sup)
       [(list #f #f) (void)]
       [(list #f #t) (ripple-changes dones (list tvi-sup))]
       [(list #t #f) (ripple-changes dones (list tvi-sub))]
       [(list #t #t) (ripple-changes dones (list tvi-sub tvi-sup))])]))

(define (squash-helper/bounds sub super)
  ;; Returns list of new-lowers, new-uppers, upper/lower intersection (IE the range of the squash)
  (define sup-lowers (variable-lower-bounds super))
  (define sub-uppers (variable-upper-bounds sub))

  (define intersection
    (set-union (list sub super)
               (set-intersect sup-lowers sub-uppers)))

  (define new-lowers
    (listeq-subtract (apply set-union
                            (map variable-lower-bounds
                                 intersection))
                     intersection))
  (define new-uppers
    (listeq-subtract (apply set-union
                            (map variable-upper-bounds
                                 intersection))
                     intersection))
  (list new-lowers new-uppers intersection))

(define (squash-type-variables! sub super)
  (define tvi-sub (variable-canonicalize sub))
  (define tvi-sup (variable-canonicalize super))
  (match-define (list new-lowers new-uppers intersection)
    (squash-helper/bounds tvi-sub tvi-sup))
  (match-define (list lower-change upper-change)
    (subtype-unify!/type-variables tvi-sub tvi-sup))

  (define new-type
    (c-type-variable-type tvi-sub))
  (define new-type-variable
    (core-type-variable #f new-type new-lowers new-uppers))

  (for ([i intersection])
    (set-core-type-variable-forward! i new-type-variable))
  (when (or lower-change upper-change)
    (ripple-subtype-unify-changes/type-variable '() (list new-type-variable))))

(define (squash-structural-record-types! sub super)
  ;; `sub` must actually be a supertype of `super`, but I'm calling it `sub` because this function will only have one call site (inside `subtype-unify!`), and this keeps the same names.
  ;; TODO - this will probably be almost identical to squash-type-variable-innards, except for the type variable handle stuff.  They can probably be combined once I get rid of the TV/TVI split.
  ;; create a new SRT with appropriate known fields, make sub, super, and their upper/lower intersect all forward to this new record
  (match-define (list new-lowers new-uppers intersection)
    (squash-helper/bounds sub super))
  (match-define (list lower-change upper-change)
    (subtype-unify!/structural-record-types sub super))

  (define new-known-field-dict (structural-record-type-known-field-dict sub))
  (define new-final? (for/or ([srt intersection])
                       (structural-record-type-finalized? srt)))
  (define new-srt
    (structural-record-type #f new-final? new-known-field-dict new-lowers new-uppers))
  (for ([srt intersection])
    (set-structural-record-type-canonical-forward! srt new-srt))
  ;; TODO - should I do more sanity checking that everything is right here?  Eg. if something in the intersection was final, it should not have changed, so the final thing should be the same as the final one.
  (when (or lower-change upper-change
            ;; For now, let's just be safe and always ripple
            #t)
    (ripple-subtype-unify-changes/structural-record-type '() (list new-srt))))

(define (squash-nominal-record-types! sub super)
  ;; `sub` must actually be a supertype of `super`, but I'm calling it `sub` because this function will only have one call site (inside `subtype-unify!`), and this keeps the same names.
  ;; TODO - this will probably be almost identical to squash-type-variable-innards, except for the type variable handle stuff.  They can probably be combined once I get rid of the TV/TVI split.
  ;; create a new NRT with appropriate known fields, make sub, super, and their upper/lower intersect all forward to this new record
  (match-define (list new-lowers new-uppers intersection)
    (squash-helper/bounds sub super))
  (match-define (list lower-change upper-change)
    (subtype-unify!/nominal-record-types sub super))

  (define new-known-field-dict (nominal-record-type-known-field-dict sub))
  (define new-name/final? (for/or ([nrt intersection])
                            (nominal-record-type-name nrt)))
  (define new-super (for/or ([nrt intersection])
                      (nominal-record-type-super-record nrt)))
  (define new-nrt
    (nominal-record-type #f new-name/final? new-super new-known-field-dict new-lowers new-uppers))
  (for ([nrt intersection])
    (set-nominal-record-type-canonical-forward! nrt new-nrt))
  ;; TODO - should I do more sanity checking that everything is right here?  Eg. if something in the intersection was final, it should not have changed, so the final thing should be the same as the final one.
  (when (or lower-change upper-change
            ;; For now, let's just be safe and always ripple
            #t)
    (ripple-subtype-unify-changes/nominal-record-type '() (list new-nrt))))


(define (subtype-unify!/type-variables sub* super*)
  #|
  This is a helper function that takes only type-variables, and only updates their type lists.  IE it does not add them to each others' upper/lower bounds lists.
  It returns a list of two bools.  The first one is true iff the sub list was modified, the second one is true iff the super list was modified.
  |#

  ;; Type variables may have any number of base-type-ranges as possibilities.
  ;; When subtype-unifying, each base-type-range pair is tried for unification.
  ;; All successes then replace the old base-type-ranges.
  ;; (Except the new ranges are tested against each other -- any range that fits entirely within another is eliminated.)

  (define sub (variable-canonicalize sub*))
  (define super (variable-canonicalize super*))
  (match (list (flatten (list (core-type-variable-type sub)))
               (flatten (list (core-type-variable-type super))))
    [(list (list #f) (list #f)) (list #f #f)]
    [(list (list #f) r)
     (set-core-type-variable-type!
      sub
      (map (λ (t)
             (match t
               [(base-type-range low high) (mk-base-type-range #f high)]
               [else
                (define inner-sub (type->skeleton-with-vars t))
                (subtype-unify! inner-sub t)
                inner-sub]))
           r))
     (list #t #f)]
    [(list l (list #f))
     (set-core-type-variable-type!
      super
      (map (λ (t)
             (match t
               [(base-type-range low high)
                (mk-base-type-range low (base-type->superest high))]
               [else
                (define inner-sup (type->skeleton-with-vars t))
                (subtype-unify! t inner-sup)
                inner-sup]))
           l))
     (list #f #t)]
    [(list subtypes supertypes)

     (match-define (list sub-bases super-bases)
       (type-lists->unified-base-types subtypes supertypes))

     (define compound-type-pairs
       (filter
        (λ(x)x)
        ;; type variables can only ever have one of each compound type, so the inner loop can only ever have one match.
        (for*/list ([sub (filter (λ(x) (not (base-type-range? x)))
                                 subtypes)]
                    [sup (filter (λ(x) (not (base-type-range? x)))
                                 supertypes)])
          (and (can-subtype-unify? sub sup)
               (subtype-unify! sub sup)
               (list sub sup)))))
     (define sub-compounds (map first compound-type-pairs))
     (define super-compounds (map second compound-type-pairs))

     (define (->use t-list)
       (match t-list
         [(list)
          (let ([err-values
                 (with-handlers ([(λ(e)#t) (λ(e) (errsu "Internal error while raising subtyping error."))])
                   (list (variable-canonicalize sub)
                         (variable-canonicalize super)))])
            (errsu "can't unify types ~v and ~v (this one shouldn't happen...)"
                   (car err-values)
                   (cadr err-values)))]
         [(list ts ...) ts]))
     (define all-sub (append sub-bases sub-compounds))
     (define all-super (append super-bases super-compounds))

     (set-core-type-variable-type! sub (->use all-sub))
     (set-core-type-variable-type! super (->use all-super))

     (list (not (set=? subtypes all-sub))
           (not (set=? supertypes all-super)))]))

(define (subtype-unify!/structural-record-types sub* super*)
  #|
  This helper function updates the actual type fields of structural records, NOT upper/lower bounds.
  It returns a list of two bools.  The first one is true iff sub was modified, the second one is true iff super was modified.
  |#
  (define sub (variable-canonicalize sub*))
  (define super (variable-canonicalize super*))
  (match (list sub super)
    [(list (c-structural-record-type f?1 known-fields-1 lb-1 ub-1)
           (c-structural-record-type f?2 known-fields-2 lb-2 ub-2))
     (define subtype-changed? #f)
     (define new-kf-1
       ;; the subtype must have all the fields of the supertype, and may have more
       (for/hash ([field-name (set-union (dict-keys known-fields-1)
                                         (dict-keys known-fields-2))])
         (define fsub (dict-ref known-fields-1 field-name #f))
         (define fsup (dict-ref known-fields-2 field-name #f))
         (values
          field-name
          (cond [(and fsub fsup)
                 (subtype-unify! fsub fsup)
                 fsub]
                [fsup
                 (define new-var (fresh-type-variable))
                 (subtype-unify! new-var fsup)
                 (set! subtype-changed? #t)
                 new-var]
                [fsub fsub]
                [else (error 'this-should-be-impossible-but-cond-unhelpfully-returns-void-on-fall-through-so-im-adding-this-just-in-case)]))))

     (set-structural-record-type-known-field-dict! sub new-kf-1)
     (list subtype-changed? #f)]))

(define (subtype-unify!/nominal-record-types sub* super*)
  #|
  This helper function updates the actual type fields of nominal records, NOT upper/lower bounds.
  It returns a list of two bools.  The first one is true iff sub was modified, the second one is true iff super was modified.
  |#
  (define sub (variable-canonicalize sub*))
  (define super (variable-canonicalize super*))
  (match (list sub super)
    [(list (nominal-record-type fwd1 name1 super1 known-fields-1 lb-1 ub-1)
           (nominal-record-type fwd2 name2 super2 known-fields-2 lb-2 ub-2))
     (define subtype-changed? #f)
     (define merged-keys (set-union (dict-keys known-fields-1)
                                    (dict-keys known-fields-2)))
     ;; TODO - I should actually do a check that #f fields can-unify.  Maybe just by calling `can-unify?` on the things to be squashed...
     (define new-kf-1
       ;; The subtype must have all the fields of the supertype, and may have more.
       ;; HOWEVER, shared fields of the subtype and supertype must be the same, NOT subtypes -- a subtype can only add more fields, not change (even subtyping) the other fields.
       (for/hash ([field-name (if (or name1 name2)
                                  (set-remove merged-keys #f)
                                  merged-keys)])
         (define fsub (dict-ref known-fields-1 field-name #f))
         (define fsup (dict-ref known-fields-2 field-name #f))
         (values
          field-name
          (cond [(and fsub fsup)
                 (unify! fsub fsup)
                 fsub]
                [fsup
                 (define new-var (fresh-type-variable))
                 (unify! new-var fsup)
                 (set! subtype-changed? #t)
                 new-var]
                [fsub fsub]
                [else (error 'this-should-be-impossible-but-cond-unhelpfully-returns-void-on-fall-through-so-im-adding-this-just-in-case)]))))

     (set-nominal-record-type-known-field-dict! sub new-kf-1)
     (list subtype-changed? #f)]))

(define ripple-subtype-unify-changes/type-variable
  (mk-ripple-subtype-unify-changes subtype-unify!/type-variables))
(define ripple-subtype-unify-changes/structural-record-type
  (mk-ripple-subtype-unify-changes subtype-unify!/structural-record-types))
(define ripple-subtype-unify-changes/nominal-record-type
  (mk-ripple-subtype-unify-changes subtype-unify!/nominal-record-types))
(define subtype-unify!/two-type-variables/type-variable-innard
  (mk-subtype-unify!/two-type-variables
   set-core-type-variable-lower-bounds!
   set-core-type-variable-upper-bounds!
   squash-type-variables!
   subtype-unify!/type-variables
   ripple-subtype-unify-changes/type-variable))
(define subtype-unify!/two-type-variables/structural-record-type
  (mk-subtype-unify!/two-type-variables
   set-structural-record-type-lower-bounds!
   set-structural-record-type-upper-bounds!
   squash-structural-record-types!
   subtype-unify!/structural-record-types
   ripple-subtype-unify-changes/structural-record-type))
(define subtype-unify!/two-type-variables/nominal-record-type
  (mk-subtype-unify!/two-type-variables
   set-nominal-record-type-lower-bounds!
   set-nominal-record-type-upper-bounds!
   squash-nominal-record-types!
   subtype-unify!/nominal-record-types
   ripple-subtype-unify-changes/nominal-record-type))

(define (can-subtype-unify? sub super)
  (match-define (list basic-result variable-dict)
    (can-subtype-unify?/variable-dict sub super))
  (and basic-result
       (check-can-unify-variable-dict variable-dict)))
(define (can-subtype-unify?/variable-dict sub* super*)
  (define sub (canonicalize-if-variable sub*))
  (define super (canonicalize-if-variable super*))
  (define vd (hasheq))
  (define (rec* sub super)
    (match-define (list basic-result variable-dict)
      (can-subtype-unify?/variable-dict sub super))
    (when basic-result
      (set! vd (variable-dict-merge vd variable-dict)))
    basic-result)
  (define (rec/tail sub super)
    (match-define (list basic-result variable-dict)
      (can-subtype-unify?/variable-dict sub super))
    (list basic-result
          (if basic-result (variable-dict-merge vd variable-dict) #f)))
  (match (list sub super)
    ;; 2x type-variable
    [(list (c-type-variable inner-t-sub sub-lb sub-ub)
           (c-type-variable inner-t-sup sup-lb sup-ub))
     (match (list (flatten (list inner-t-sub))
                  (flatten (list inner-t-sup)))
       [(list (list #f) _) (list #t
                                 (variable-dict-merge vd
                                                      (hasheq sub (list super)
                                                              super (list sub))))]
       [(list _ (list #f)) (list #t
                                 (variable-dict-merge vd
                                                      (hasheq sub (list super)
                                                              super (list sub))))]
       [(list subs sups)
        (define basic
          (for*/or ([sub subs]
                    [sup sups])
            (match (list sub sup)
              [(list (base-type-range #f lsup) (base-type-range _ rsup))
               (and
                (eq? (base-type->superest lsup)
                     (base-type->superest rsup))
                (or (memq rsup (base-type->parent-chain lsup))
                    (memq lsup (base-type->parent-chain rsup))))]
              [(list (base-type-range lsub _) (base-type-range _ rsup))
               (rec* lsub rsup)]
              [else (rec* sub sup)])))
        (list basic (variable-dict-merge vd
                                         (hasheq sub (list super)
                                                 super (list sub))))])]
    ;; left type-variable
    [(list (c-type-variable _ _ _) _)
     (match-define (list basic variable-dict)
       (can-X-unify?/one-type-variable
        sub super (λ (l r) (rec* l r))))
     (list basic (variable-dict-merge vd variable-dict))]
    ;; right type-variable
    [(list _ (c-type-variable _ _ _))
     (match-define (list basic variable-dict)
       (can-X-unify?/one-type-variable
        super sub (λ (super sub)
                    (rec* sub super))))
     (list basic (variable-dict-merge vd variable-dict))]
    ;; function-type
    [(list (function-type arg-l ret-l)
           (function-type arg-r ret-r))
     (define basic
       (and
        ;; covariant return
        (rec* ret-l ret-r)
        ;; contravariant arguments
        (rec* arg-r arg-l)))
     (list basic vd)]
    ;; product-type
    [(list (product-type inner1 lowers1 uppers1)
           (product-type inner2 lowers2 uppers2))
     (match-define (list basic-result variable-dict)
       (can-X-unify?/product-type sub super rec*))
     (list basic-result (variable-dict-merge vd variable-dict))]
    ;; nominal-record-type
    [(list (? nominal-record-type?) (? nominal-record-type?))
     ;; TODO - make this actually subtypable
     (can-unify?/variable-dict sub super)]
    ;; nominal-record-definition-type
    [(list (nominal-record-definition-type inner1)
           (nominal-record-definition-type inner2))
     ;; TODO -for now this is symmetric, but later should be subtypable.
     (define basic-result
       (rec* inner1 inner2))
     (list basic-result vd)]
    ;; structural-record-type
    [(list (c-structural-record-type f?1 known-fields-1 lb-1 ub-1)
           (c-structural-record-type f?2 known-fields-2 lb-2 ub-2))
     ;; For each key in the supertype, the subtype either has the same key and it can subtype, or all transitive lower bounds either also don't have the key or their field can be a subtype.
     (define transitive-lb-1
       (variable-transitive-lower-bounds sub))
     (define basic-result
       (for/and ([k (dict-keys known-fields-2)])
         (if (dict-has-key? known-fields-1 k)
             (rec* (dict-ref known-fields-1 k) (dict-ref known-fields-2 k))
             (and (not (structural-record-type-finalized? sub))
                  (for/and ([lb transitive-lb-1])
                    (match-define (list basic-result variable-dict)
                      (can-subtype-unify?/structural-record-type-lower-bound/field/variable-dict
                       lb k (dict-ref known-fields-2 k)))
                    (set! vd (variable-dict-merge vd variable-dict))
                    basic-result)))))
     (list basic-result vd)]
    ;; generic-type
    [(list (generic-type name1 constructor1 type-arguments1 variances1)
           (generic-type name2 constructor2 type-arguments2 variances2))
     ;; TODO - generic types need to store the variance type for each field.
     ;;        For a start, let's assume all fields are invariant.
     (define basic-result
       (and (eq? constructor1 constructor2)
            (for/and ([l type-arguments1]
                      [r type-arguments2]
                      [v variances1])
              (match v
                ['invariant
                 (match-define (list basic-result variable-dict)
                   (can-unify?/variable-dict l r))
                 (set! vd (variable-dict-merge vd variable-dict))
                 basic-result]
                ['covariant (rec* l r)]
                ['contravariant (rec* r l)]))))
     (list basic-result vd)]
    ;; parameter type
    [(list (parameter-type sym-l) (parameter-type sym-r))
     (eq? sym-l sym-r)]
    ;; base-type
    [(list (base-type lname lsuper _) (base-type rname rsuper _))
     (list (->bool (memq super (base-type->parent-chain sub))) vd)]
    ;; While base-type-ranges can only be in type variables, it is convenient to recursively use this function to test them
    [(list (base-type-range l-low l-high) (base-type-range r-low r-high))
     (list
      (->bool
       (if l-low
           (memq r-high (base-type->parent-chain l-low))
           (or (memq r-high (base-type->parent-chain l-high))
               (memq l-high (base-type->parent-chain r-high)))))
      vd)]
    [(list (base-type _ _ _) (base-type-range _ _))
     (rec/tail (mk-base-type-range sub sub) super)]
    [(list (base-type-range _ _) (base-type _ _ _))
     (rec/tail sub (mk-base-type-range super super))]
    [else (list #f #f)]))

(define (can-X-unify?/one-type-variable tv rtype inner-can-unify?)
  (define vd (hasheq tv (list rtype)))
  (cond
    [(not (core-type-variable-type tv)) (list #t vd)]
    [else
     (define t (core-type-variable-type tv))
     (define (struct-rec predicate)
       (match (filter predicate t)
         [(list) (list #f (hasheq))]
         [(list one) (list (inner-can-unify? one rtype) vd)]))
     (match rtype
       [(or (? base-type?)
            (? base-type-range?))
        (define basic-result
          (for/or ([possibility t])
            (match possibility
              [(base-type-range low high)
               (inner-can-unify? possibility rtype)]
              [else #f])))
        (list basic-result vd)]
       [(? function-type?) (struct-rec function-type?)]
       [(? product-type?) (struct-rec product-type?)]
       [(? nominal-record-type?) (struct-rec nominal-record-type?)]
       [(? nominal-record-definition-type?)
        (struct-rec nominal-record-definition-type?)]
       [(? structural-record-type?)
        (struct-rec structural-record-type?)]
       [(? parameter-type?) (list #t vd)]
       [(generic-type name constructor type-arguments variances)
        (define inner-matched
          (filter (λ (x) (match x
                           [(generic-type _ iconstructor _ _)
                            (eq? constructor iconstructor)]
                           [else #f]))
                  t))
        (match inner-matched
          [(list) (list #f vd)]
          [(list one) (list (inner-can-unify? one rtype) vd)]
          [(list one more ...)
           (error 'xsmith
                  "A type variable seems to have two generic types of the same type.  This is currently an error.  Type:  ~v"
                  tv)])])]))

(define (can-X-unify?/product-type l r inner-can-unify?)
  (match (list l r)
    [(list (product-type inner1 lowers1 uppers1)
           (product-type inner2 lowers2 uppers2))
     (cond
       [(not inner1) (list #t (hasheq l (list r) r (list l)))]
       [(not inner2) (list #t (hasheq l (list r) r (list l)))]
       [(not (equal? (length inner1) (length inner2))) (list #f (hasheq))]
       [else
        (list (for/and ([li inner1]
                        [ri inner2])
                (inner-can-unify? li ri))
              (hasheq))])]))


(define (can-subtype-unify?/structural-record-type-lower-bound/field/variable-dict
         lower-bound-srt field-key super-field-value)
  (match lower-bound-srt
    [(c-structural-record-type lb-f? lb-kf _ _)
     (if (and (not (dict-has-key? lb-kf field-key)) (not lb-f?))
         (list #t (hasheq))
         (if (dict-has-key? lb-kf field-key)
             (can-subtype-unify?/variable-dict
              (dict-ref lb-kf field-key) super-field-value)
             (list #f (hasheq))))]))


(define (unify! t1* t2*)
  (define t1 (canonicalize-if-variable t1*))
  (define t2 (canonicalize-if-variable t2*))
  (with-handlers ([exn:fail:subtype-unify?
                   (λ (e) (error 'unify!
                                 (exn-message e)))])
    (begin (subtype-unify! t1 t2)
           (subtype-unify! t2 t1))))


;; When checking can-unify? (or can-subtype-unify?), we need to keep a mapping
;; from variables to the types they need to be able to unify with.
;; At the end of unification, even if each part of a compound type can be unified,
;; we need to check whether there were duplicate types for any given variable that
;; can't be unified.
;; Eg. (can-unify? (function-type tv1 tv1) (function-type int int)) should be #t,
;; but (can-unify? (function-type tv1 tv1) (function-type string int)) should be #f.
;; Here is a helper to merge dictionaries with this info.
(define (variable-dict-merge vd1 vd2)
  (cond [(or (not vd2) (hash-empty? vd2)) vd1]
        [(or (not vd1) (hash-empty? vd1)) vd2]
        [else
         (for/fold ([vd vd1])
                   ([(k v) (in-hash vd2)])
           (hash-set vd k (append (hash-ref vd k (list)) v)))]))
(define (check-can-unify-variable-dict vd)
  (for/and ([(var type-list) (in-hash vd)])
    (let loop ([type-list type-list])
      (match type-list
        [(list) #t]
        [(list t1) #t]
        [(list t1 more ...)
         (for/and ([t more])
           ;; Whether we are checking can-unify? or can-subtype-unify?, we will just
           ;; check whether the types to be unified for a duplicate type variable
           ;; can symmetric unify.  We don't have a way to tell whether they should
           ;; have a subtype relationship or which direction it should be.
           (match-define (list basic-result variable-dict)
             (can-unify?/variable-dict t1 t))
           ;; We just care about the basic result here.  If we try to do this
           ;; variable-dict thing recursively we may never terminate.
           ;; Ideally we could not collect the variable dicts the second time
           ;; around, but that would mean more code...
           basic-result)]))))

;; symmetric can-unify? can't just check if each can subtype-unify the other,
;; because there could be cases like:
;; (type-variable dog penguin) (type-variable labradoodle bird)
;; where each side can subtype-unify with the other, but they can't be subtype
;; unified in BOTH directions.
(define (can-unify? l r)
  (match-define (list basic-result variable-dict)
    (can-unify?/variable-dict l r))
  (and basic-result
       (check-can-unify-variable-dict variable-dict)))
(define (can-unify?/variable-dict l* r*)
  (define l (canonicalize-if-variable l*))
  (define r (canonicalize-if-variable r*))
  (define vd (hasheq))
  (define (rec/tail l r)
    (match-define (list basic-result variable-dict)
      (can-unify?/variable-dict l r))
    (list basic-result
          (if basic-result
              (variable-dict-merge vd variable-dict)
              #f)))
  (define (rec* l r)
    (match-define (list basic-result variable-dict)
      (can-unify?/variable-dict l r))
    (when basic-result
      (set! vd (variable-dict-merge vd variable-dict)))
    basic-result)
  (match (list l r)
    ;; 2x type-variable
    [(list (c-type-variable l-inner-t l-lb l-ub)
           (c-type-variable r-inner-t r-lb r-ub))
     (match (list (flatten (list l-inner-t))
                  (flatten (list r-inner-t)))
       [(list (list #f) _) (list #t (variable-dict-merge
                                     vd (hasheq l (list r) r (list l))))]
       [(list _ (list #f)) (list #t (variable-dict-merge
                                     vd (hasheq l (list r) r (list l))))]
       [(list ls rs)
        (define basic
          (for*/or ([ll ls]
                    [rr rs])
            (rec* ll rr)))
        (list basic (variable-dict-merge vd (hasheq l (list r) r (list l))))])]
    ;; left type-variable
    [(list (c-type-variable _ _ _) _)
     (match-define (list basic-result variable-dict)
       (can-X-unify?/one-type-variable l r (λ (l r) (rec* l r))))
     (list basic-result (variable-dict-merge vd variable-dict))]
    ;; right type-variable
    [(list _ (c-type-variable _ _ _))
     (match-define (list basic-result variable-dict)
       (can-X-unify?/one-type-variable r l (λ (l r) (rec* l r))))
     (list basic-result (variable-dict-merge vd variable-dict))]
    ;; function-type
    [(list (function-type arg-l ret-l)
           (function-type arg-r ret-r))
     (define basic
       (and
        (rec* ret-l ret-r)
        (rec* arg-r arg-l)))
     (list basic vd)]
    ;; product-type
    [(list (product-type inner1 lowers1 uppers1)
           (product-type inner2 lowers2 uppers2))
     (match-define (list basic-result variable-dict)
       (can-X-unify?/product-type l r rec*))
     (list basic-result (variable-dict-merge vd variable-dict))]
    ;; nominal-record-type
    [(list (? nominal-record-type?) (? nominal-record-type?))
     (define basic-result
       (match (list l r)
         [(list (c-nominal-record-type #f #f inners1 lb1 ub1)
                (c-nominal-record-type #f #f inners2 lb2 ub2))
          (->bool (and (for/and ([k (dict-keys inners1)])
                         (or (not (dict-has-key? inners2 k))
                             (rec* (dict-ref inners1 k) (dict-ref inners2 k))))
                       (for/and ([k (dict-keys inners2)])
                         (or (not (dict-has-key? inners1 k))
                             (rec* (dict-ref inners1 k) (dict-ref inners2 k))))))]
         [(list (c-nominal-record-type #f #f inners1 lb1 ub1)
                (c-nominal-record-type name2 super2 inners2 lb2 ub2))
          (define inner-vals (dict-values inners2))
          (for/and ([k (dict-keys inners1)])
            (cond [(not k) (let ([needed-type (dict-ref inners1 k)])
                             (->bool (ormap (λ (x) (rec* x needed-type))
                                            inner-vals)))]
                  [else (and (dict-has-key? inners2 k)
                             (rec* (dict-ref inners1 k) (dict-ref inners2 k)))]))]
         [(list (c-nominal-record-type name1 super1 kfd1 lb1 ub1)
                (c-nominal-record-type #f #f kfd2 lb2 ub2))
          (rec* r l)]
         [(list (c-nominal-record-type name1 super1 kfd1 lb1 ub1)
                (c-nominal-record-type name2 super2 kfd2 lb2 ub2))
          ;; TODO - verify that names are unique?
          (equal? name1 name2)]))
     (list basic-result (variable-dict-merge vd (hasheq l (list r) r (list l))))]
    ;; nominal-record-definition-type
    [(list (nominal-record-definition-type inner1)
           (nominal-record-definition-type inner2))
     (rec/tail inner1 inner2)]
    ;; structural-record-type
    [(list (c-structural-record-type f?1 known-fields-1 lb-1 ub-1)
           (c-structural-record-type f?2 known-fields-2 lb-2 ub-2))
     ;; All known fields that are in both must be compatible, and the union of known-fields must not conflict with any lower bounds.
     ;; There are 2 kinds of conflicts:
     ;; * Struct A has more fields than struct B, but struct B has a finalized lower bound that lacks the field.
     ;; * Struct A has more fields than struct B, but struct B has a lower bound that has one of those fields as an incompatible type.
     (define tlb1 (variable-transitive-lower-bounds l))
     (define tlb2 (variable-transitive-lower-bounds r))
     (define basic
       (for/and ([k (set-union (dict-keys known-fields-1) (dict-keys known-fields-2))])
         (define f1 (dict-ref known-fields-1 k #f))
         (define f2 (dict-ref known-fields-2 k #f))
         (cond
           [(and f1 f2) (rec* f1 f2)]
           [f1 (and (not (structural-record-type-finalized? r))
                    (for/and ([lb tlb2])
                      (match-define (list basic-result variable-dict)
                        (can-subtype-unify?/structural-record-type-lower-bound/field/variable-dict
                         lb k f1))
                      (when basic-result
                        (set! vd (variable-dict-merge vd variable-dict)))
                      basic-result))]
           [f2 (and (not (structural-record-type-finalized? l))
                    (for/and ([lb tlb1])
                      (match-define (list basic-result variable-dict)
                        (can-subtype-unify?/structural-record-type-lower-bound/field/variable-dict
                         lb k f2))
                      (when basic-result
                        (set! vd (variable-dict-merge vd variable-dict)))
                      basic-result))]
           [else (error 'this-should-be-impossible-but-cond-unhelpfully-returns-void-on-fall-through-so-im-adding-this-just-in-case/can-unify/srt)])))
     (list basic vd)]
    ;; generic-type
    [(list (generic-type name1 constructor1 type-arguments1 variances1)
           (generic-type name2 constructor2 type-arguments2 variances2))
     (define basic-result
       (and (eq? constructor1 constructor2)
            (for/and ([inner-l type-arguments1]
                      [inner-r type-arguments2])
              (rec* inner-l inner-r))))
     (list basic-result vd)]
    ;; parameter-type
    [(list (parameter-type sym-l) (parameter-type sym-r))
     (list (eq? sym-l sym-r) vd)]
    ;; base-type
    [(list (base-type _ _ _) (base-type _ _ _))
     (list (eq? l r) vd)]
    [(list (base-type-range l-low l-high) (base-type-range r-low r-high))
     ;; In this case we return the new range that the variables would occupy if symmetrically unified.
     (define new-high
       (cond [(memq r-high (base-type->parent-chain l-high))
              l-high]
             [(memq l-high (base-type->parent-chain r-high))
              r-high]
             [else 'bad]))
     (define basic
       (and (not (eq? new-high 'bad))
            (let ([new-low (match (list l-low r-low)
                             [(list #f _) r-low]
                             [(list _ #f) l-low]
                             [else
                              (cond [(memq r-low (base-type->parent-chain l-low))
                                     r-low]
                                    [(memq l-low (base-type->parent-chain r-low))
                                     l-low]
                                    [else 'bad])])])
              (and (not (eq? 'bad new-low))
                   (or (not new-low)
                       (->bool (memq new-high
                                     (base-type->parent-chain new-low))))))))
     (list basic vd)]
    [(list (base-type _ _ _) (base-type-range _ _))
     (rec/tail (mk-base-type-range l l) r)]
    [(list (base-type-range _ _) (base-type _ _ _))
     (rec/tail l (mk-base-type-range r r))]
    [else (list #f #f)]))


;; A parameter to hold the list of constructors for base or composite types (with minimally constrained type variables inside).
(define current-xsmith-type-constructor-thunks
  (make-parameter (list default-base-type)))
;; TODO - this should be configurable.
(define type-max-concretization-depth (make-parameter 5))
(define record-type-max-fields (make-parameter 5))
(define product-type-max-length 6)

(define (concretize-type t
                         #:at-node [node #f])
  (define (recur t depth)
    (define (r t) (recur t (add1 depth)))
    (match t
      ;; TODO - type generation needs some kind of depth limit if composite types can contain composite types.
      [(c-type-variable (list one-type) _ _)
       (r one-type)]
      [(c-type-variable (and maybe-options (or #f (list _ ...))) _ _)
       (define options (or maybe-options
                           (filter-map (λ (x) (and x (x)))
                                       (current-xsmith-type-constructor-thunks))))
       (define options-filtered
         (if (< depth (type-max-concretization-depth))
             options
             (filter base-type? options)))
       (define options-use (if (null? options-filtered) options options-filtered))
       (when (null? options-use)
         (error 'concretize-type "Received an empty list for options.  Was current-xsmith-type-constructor-thunks parameterized?"))
       ;; TODO - fresh-type-variable is invalid here as it causes infinite recursion.
       (when (not (empty? (filter (λ (tv) (and (core-type-variable? tv)
                                               (equal? #f (core-type-variable-type
                                                           (variable-canonicalize tv)))))
                                  options-use)))
         (error 'concretize-type (format "Received a typeless type variable in options.  Don't use (fresh-type-variable) when parameterizing current-xsmith-type-constructor-thunks." options-use)))
       (r (random-ref options-use))]
      [(base-type _ _ _) t]
      [(base-type-range low high)
       ;; TODO - this should be a random choice.  But I also need to deal with the #f low case and enumerate all possibilities.  For now I just want to get the code working again.
       high]
      [(parameter-type _) t]
      [(product-type inner lb ub)
       (define inner-types inner)
       (if inner-types
           (mk-product-type (map r inner-types))
           (mk-product-type (map (λ (x) (r (fresh-type-variable)))
                                 (make-list (random product-type-max-length) #f))))]
      [(and (? nominal-record-type?) (? settled?)) t]
      [(c-nominal-record-type name super known-fields lb ub)
       (when name
         (eprintf "nominal-record-type with name not settled! ~v\n" name))
       (cond
         [(and node (nominal-record-type?/with-field t))
          (let* ([d (nominal-record-definition-type t)]
                 [references
                  (filter (λ (b) (and b
                                      (nominal-record-definition-type?
                                       (binding-type b))
                                      (can-unify? d (binding-type b))))
                          (att-value '_xsmith_visible-bindings node))])
            (if (null? references)
                (error 'concretize-type
                       "can't find suitable definition for nominal-record-type: ~v\n"
                       t)
                (nominal-record-definition-type-type
                 (binding-type (random-ref references)))))]
         [(nominal-record-type?/with-field t)
          (error 'concretize-type
                 "can't concretize nominal-record-type with a named field but no record name unless a #:node argument is given: ~v\n"
                 t)]
         [else
          (let* ([needed (dict-ref known-fields #f (λ () (fresh-type-variable)))]
                 [n-random-fields (random (record-type-max-fields))]
                 [field-list (cons needed
                                   (map (λ (x) (fresh-type-variable))
                                        (make-list n-random-fields #f)))])
            (concretize-type
             (nominal-record-type #f
                                  (fresh-var-name "record_")
                                  #f
                                  (for/list ([f field-list])
                                    (cons (fresh-var-name "field_")
                                          (r f)))
                                  '() '())))])]
      [(nominal-record-definition-type inner)
       (nominal-record-definition-type (r inner))]
      [(c-structural-record-type finalized? known-fields lb ub)
       ;; TODO - if all immediate lower bounds have a conflicted name and an appropriate supertype for all of them can be found, it could be instantiated here.  For now, let's just ignore that.
       (fresh-structural-record-type
        #:finalized? #t
        (for/hash ([k (dict-keys known-fields)])
          (values k (r (dict-ref known-fields k)))))]
      [(function-type arg return) (function-type (r arg)
                                                 (r return))]
      [(generic-type name ctor inners vs)
       (generic-type name ctor (map r inners) vs)]
      [else (error 'concretize-type "internal error -- no case for type: ~a" t)]))
  (recur t 0))

(module+ test
  (define integer (mk-base-type 'integer))
  (define float (mk-base-type 'float))
  (define string (mk-base-type 'string))

  (check-true (can-unify? integer integer))
  (check-false (can-unify? integer float))

  (define int-int->int (function-type (mk-product-type (list integer integer))
                                      integer))
  (define int->int (function-type (mk-product-type (list integer)) integer))
  (define str->int (function-type (mk-product-type (list string)) integer))

  (check-true (can-unify? int->int int->int))
  (check-false (can-unify? int->int int-int->int))
  (check-false (can-unify? int->int str->int))
  (check-not-exn (λ () (unify! int->int int->int)))
  (check-exn exn? (λ () (unify! int->int str->int)))



  (define (or-int-str) (fresh-type-variable integer string))

  ;; Partially constrained type variables can not include variables in the constraint.
  (check-exn exn? (λ () (fresh-type-variable (fresh-type-variable integer))))
  ;; Though it can include composite types that include variables.
  (check-not-exn (λ () (fresh-type-variable
                        (function-type (mk-product-type (list (or-int-str)))
                                       (or-int-str)))))
  ;; But not more than one of a given composite.
  (check-exn exn? (λ () (fresh-type-variable
                         (function-type (mk-product-type (list (or-int-str)))
                                        (or-int-str))
                         int->int)))

  (define t1 (or-int-str))
  (check-true (can-unify? integer t1))
  (check-true (can-unify? string t1))
  (check-not-exn (λ () (unify! integer t1)))
  (check-false (can-unify? string t1))
  (check-exn exn? (λ () (unify! string t1)))

  (define (or1->int) (function-type (mk-product-type (list (or-int-str))) integer))
  (define (or2->int) (function-type (mk-product-type #f) integer))
  (define an-or1->int (or1->int))
  (define an-or2->int (or2->int))
  (define an-or3->int (fresh-type-variable))
  (check-true (can-unify? an-or1->int an-or2->int))
  (check-true (can-unify? an-or1->int int->int))
  (check-true (can-unify? an-or1->int str->int))
  (check-false (can-unify? an-or1->int int-int->int))
  (check-true (can-unify? an-or2->int int->int))
  (check-true (can-unify? an-or2->int str->int))
  (check-true (can-unify? an-or2->int int-int->int))
  (check-true (can-unify? an-or3->int int->int))
  (check-true (can-unify? an-or3->int str->int))
  (check-true (can-unify? an-or3->int int-int->int))
  (check-not-exn (λ () (unify! an-or1->int an-or3->int)))
  (check-true (can-unify? an-or3->int int->int))
  (check-true (can-unify? an-or3->int str->int))
  (check-false (can-unify? an-or3->int int-int->int))
  (check-not-exn (λ () (unify! an-or1->int an-or2->int)))
  (check-true (can-unify? an-or1->int int->int))
  (check-true (can-unify? an-or2->int int->int))
  (check-true (can-unify? an-or1->int str->int))
  (check-true (can-unify? an-or2->int str->int))
  (check-false (can-unify? an-or1->int int-int->int))
  (check-false (can-unify? an-or2->int int-int->int))
  (check-true (can-unify? an-or1->int an-or2->int))
  (check-not-exn (λ () (unify! an-or2->int int->int)))
  ;; Unifying 2 also affected 1 and 3
  (check-false (can-unify? an-or1->int str->int))
  (check-false (can-unify? an-or2->int str->int))
  (check-false (can-unify? an-or3->int str->int))
  (check-true (can-unify? an-or1->int int->int))
  (check-true (can-unify? an-or2->int int->int))
  (check-true (can-unify? an-or3->int int->int))


  (let ()
    ;; test multiple instances of the same type variable
    (define v1 (fresh-type-variable))
    (define f1 (function-type v1 v1))
    (check-true (can-unify? f1 (function-type integer integer)))
    (check-true (can-unify? (function-type integer integer) f1))
    (check-true (can-subtype-unify? f1 (function-type integer integer)))
    (check-true (can-subtype-unify? (function-type integer integer) f1))
    (check-false (can-unify? f1 (function-type string integer)))
    (check-false (can-unify? (function-type string integer) f1))
    (check-false (can-subtype-unify? f1 (function-type string integer)))
    (check-false (can-subtype-unify? (function-type string integer) f1))
    )

  )

;; TODO - It might make sense for Xsmith to track construction of
;; `base-types` or otherwise maintain a list of all base types so it
;; can know what the *actual* lowest types are. Right now a type can
;; not pass the `settled?` predicate because a variable has a base type
;; range with bottom `#f` when in reality the type IS settled because
;; the top type of the range has no subtypes.

(define (settled?/core t wrappers-allowed?)
  (define (rec t) (settled?/core t wrappers-allowed?))
  (match t
    [(c-type-variable (list one-type) _ _)
     (and wrappers-allowed? (rec one-type))]
    [(c-type-variable _ _ _) #f]
    [(base-type _ _ _) #t]
    [(base-type-range l r) (equal? l r)]
    [(parameter-type _) #t]
    [(function-type a r)
     (and (rec a) (rec r))]
    [(c-nominal-record-type name super fields lb ub)
     ;; If a name is set then it's settled.
     (->bool name)]
    [(nominal-record-definition-type inner) (rec inner)]
    [(c-structural-record-type finalized? fields lb ub)
     (and finalized?
          (for/and ([k (dict-keys fields)])
            (rec (dict-ref fields k))))]
    [(product-type itl lb ub)
     (and (list? itl) (andmap rec itl))]
    [(generic-type _ _ inners _)
     (andmap rec inners)]))
(define (settled? t)
  (settled?/core t #t))
(define (type-has-no-variables? t)
  (settled?/core t #f))

(define (highlight arg)
  (format "\033[31m~a\033[0m\n" arg))
(define (at-least-as-settled v constraint-type)
  #|
  Returns #t when:
  • they are different types
  • any case in a type variable of v is available in constraint-type
  This is a helper to tell how much of the tree needs to be walked to decide
  whether it is meaningful to test `can-unify?` when choosing a production
  to create.  A type may unify with a cousin node's type, constraining the
  nodes we can choose at a given point.  But we don't want to traverse more
  of the tree than we need to.
  |#
  (match (list v constraint-type)
    [(list (c-type-variable #f _ _) _) #f]
    [(list _ (c-type-variable #f _ _)) #t]
    [(list (c-type-variable (list one-type) _ _)
           _)
     (at-least-as-settled one-type constraint-type)]
    [(list _
           (c-type-variable (list one-type) _ _))
     (at-least-as-settled v one-type)]
    ;; two type variables with multiple options
    [(list (c-type-variable (list ts ...) _ _)
           (c-type-variable (list cs ...) _ _))
     (or
      ;; check if every case in ts is covered in cs
      (for/and ([t ts])
        (match t
          [(base-type _ _ _) (for/or ([c (filter base-type? cs)])
                               (or (can-subtype-unify? t c)
                                   (can-subtype-unify? c t)))]
          [(base-type-range _ _) (for/or ([c (filter base-type-range? cs)])
                                   (or (can-subtype-unify? t c)
                                       (can-subtype-unify? c t)))]
          [(? function-type?)
           (ormap (λ (c) (at-least-as-settled t c))
                  (filter function-type? cs))]
          [(? product-type?)
           (ormap (λ (c) (at-least-as-settled t c))
                  (filter product-type? cs))]
          [(? generic-type?)
           (ormap (λ (c) (at-least-as-settled t c))
                  (filter (λ (c) (and (generic-type? c)
                                      (eq? (generic-type-constructor c)
                                           (generic-type-constructor t))))
                          cs))]
          ;; TODO - more cases, and make `else` an error.
          [else #f]))
      ;; check if they have nothing in common
      (for/and ([c cs])
        (match c
          [(? (λ (x) (or (base-type? x)
                         (base-type-range? x))))
           (for/and ([t (filter (λ (x) (or (base-type? x)
                                           (base-type-range? x)))
                                ts)])
             (and (not (can-subtype-unify? c t))
                  (not (can-subtype-unify? t c))))]
          [(? function-type?)
           (null? (filter function-type? ts))]
          [(? product-type?)
           (null? (filter product-type? ts))]
          [(? generic-type?)
           (null? (filter generic-type? ts))]
          [(? nominal-record-type?)
           (null? (filter nominal-record-type? ts))]
          [(? nominal-record-definition-type?)
           (null? (filter nominal-record-definition-type? ts))])))]
    ;; left type variable with multiple options
    [(list (c-type-variable (list ts ...) _ _)
           _)
     (and (for/and ([t ts])
            (at-least-as-settled t constraint-type))
          (or (not (base-type? constraint-type))
              (let ([btrs (filter base-type-range? ts)])
                (or (for/and ([btr btrs])
                      (can-unify? btr constraint-type))
                    (for/and ([btr btrs])
                      (not (can-unify? btr constraint-type)))))))]
    ;; right type variable with multiple options
    [(list _
           (c-type-variable (list cs ...) _ _))
     (for/and ([c cs]) (at-least-as-settled v c))]
    ;; No more variables
    [(list (c-type-variable _ _ _) _) (error 'at-least-as-settled "internal error, shouldn't reach this point with a type variable, got l: ~v, r: ~v\n" v constraint-type)]
    [(list _ (c-type-variable _ _ _)) (error 'at-least-as-settled "internal error, shouldn't reach this point with a type variable, got l: ~v, r: ~v\n" v constraint-type)]
    [(list (parameter-type _) _) #t]
    [(list (base-type _ _ _) _) #t]
    [(list (base-type-range min max) _)
     (or (not (can-unify? v constraint-type))
         (eq? min max))]
    [(list (function-type v-arg v-ret) (function-type c-arg c-ret))
     (and (at-least-as-settled v-arg c-arg)
          (at-least-as-settled v-ret c-ret))]
    [(list (function-type _ _) rtype) #f]
    [(list (function-type _ _) _) #t]
    [(list (product-type v-inner-list _ _) (product-type c-inner-list _ _))
     (let ([ts v-inner-list]
           [cs c-inner-list])
       (match (list ts cs)
         [(list _ #f) #t]
         [(list #f _) #f]
         [else (if (equal? (length ts) (length cs))
                   (andmap at-least-as-settled ts cs)
                   #t)]))]
    [(list (product-type _ _ _) _) #t]
    [(list (c-nominal-record-type v-name v-super v-inners v-lb v-ub)
           (c-nominal-record-type c-name c-super c-inners c-lb c-ub))
     ;; For now be conservative.
     (->bool v-name)]
    [(list (c-nominal-record-type _ _ _ _ _) _) #t]
    [(list (nominal-record-definition-type inner1)
           (nominal-record-definition-type inner2))
     (at-least-as-settled inner1 inner2)]
    [(list (nominal-record-definition-type _) _) #t]
    [(list (c-structural-record-type f?1 known-fields-1 lb-1 ub-1)
           (c-structural-record-type f?2 known-fields-2 lb-2 ub-2))
     #;(error 'at-least-as-settled/structural-record-type-case "TODO - implement.  check finalized status, fail quickly if both are finalized and fields are clearly incompatible (each record has a field that the other doesn't), recur through known-fields.")
     ;; TODO - for now, let's just conservatively force maximal exploration.
     #f]
    [(list (c-structural-record-type _ _ _ _) _) #t]
    [(list (generic-type v-n v-ctor v-inners v-vars)
           (generic-type c-n c-ctor c-inners v-vars))
     (if (eq? v-ctor c-ctor)
         (andmap at-least-as-settled v-inners c-inners)
         #t)]
    [(list (generic-type _ _ _ _) _) #t]
    ;; No else, so we get an error if there are new types that we don't extend this with.
    ))

(module+ test
  (check-true (settled? (fresh-type-variable (base-type-range dog dog))))
  (check-true (at-least-as-settled (mk-base-type 'foo) (fresh-type-variable)))
  (check-true (at-least-as-settled (mk-base-type 'foo)
                                   (fresh-type-variable (mk-base-type 'foo)
                                                        (mk-base-type 'bar))))
  (check-true (at-least-as-settled (mk-base-type 'foo)
                                   (fresh-type-variable (mk-base-type 'foo)
                                                        (mk-base-type 'bar))))
  (check-false (at-least-as-settled (fresh-type-variable)
                                    (fresh-type-variable (mk-base-type 'foo)
                                                         (mk-base-type 'bar))))
  (check-false (at-least-as-settled (function-type (fresh-type-variable)
                                                   (mk-base-type 'foo))
                                    (function-type (mk-base-type 'bar)
                                                   (mk-base-type 'foo))))
  (check-true (at-least-as-settled (fresh-type-variable (mk-product-type #f))
                                   (fresh-type-variable (mk-base-type 'foo)
                                                        (mk-base-type 'bar))))
  (check-false (at-least-as-settled (fresh-type-variable (mk-product-type #f))
                                    (fresh-type-variable
                                     (mk-product-type (list (fresh-type-variable)))
                                     (mk-base-type 'bar))))
  ;; When multiple base types are possible and the constraint is one of them, it's not sufficiently settled.
  (check-false (at-least-as-settled (fresh-type-variable penguin dog) dog))
  ;; When multiple base types are possible but the constraint is out of the set, it's good.
  (check-true (at-least-as-settled (fresh-type-variable penguin dog)
                                   (mk-base-type 'unrelated-base-type)))
  )


;;; True if any of the variables is anywhere in the type.
(define (contains-type-variables? t vs)
  ;; Here "type variables" can be type variables, or product-type-inners boxes, or structural-type canonical instances
  ;; The variables must be in canonical form.
  (define (rec t) (contains-type-variables? t vs))
  (match t
    [(base-type _ _ _) #f]
    [(base-type-range _ _) #f]
    [(parameter-type _) #f]
    [(function-type arg ret) (or (rec arg) (rec ret))]
    [(product-type inners lb ub)
     (match inners
       [#f (->bool (memq t vs))]
       [(list ts ...) (ormap rec ts)])]
    ;[(sum-type)]
    [(nominal-record-definition-type inner) (rec inner)]
    [(generic-type name constructor inners variances)
     (ormap rec inners)]
    [(or
      (c-type-variable _ _ _)
      (c-structural-record-type _ _ _ _)
      (c-nominal-record-type _ _ _ _ _))
     (not (set-empty?
           (set-intersect (type->type-variable-list t)
                          vs)))]))

;;; Returns a list of every type variable contained in a type or related as an upper/lower bound.
(define (type->type-variable-list orig-type*)
  (define orig-type (canonicalize-if-variable orig-type*))
  (define (work vars init-todos init-dones)
    (cond
      [(null? init-todos) vars]
      [(memq (car init-todos) init-dones)
       (work vars (cdr init-todos) init-dones)]
      [else
       (let* ([t (canonicalize-if-variable (car init-todos))]
              [bounds (if (variable? t)
                          (map variable-canonicalize
                               (append (variable-lower-bounds! t)
                                       (variable-upper-bounds! t)))
                          (list))]
              [dones (cons (car init-todos) init-dones)]
              [todo-bounds (listeq-subtract bounds dones)]
              [todos (append todo-bounds (cdr init-todos))])
         (match t
           [(base-type _ _ _) (work vars todos dones)]
           [(base-type-range _ _) (work vars todos dones)]
           [(parameter-type _) (work vars todos dones)]
           [(function-type arg ret)
            (work vars (list* arg ret todos) dones)]
           [(product-type inners lb ub)
            (if inners
                (work vars (append inners todos) dones)
                (let ([all-bounds (product-type->all-transitive-bounds t)])
                  (work (set-union vars all-bounds)
                        todos
                        (append all-bounds init-dones))))]
           ;[(sum-type)]
           [(c-nominal-record-type name super known-fields lb ub)
            (work (cons t vars) (append (dict-values known-fields) todos) dones)]
           [(nominal-record-definition-type inner)
            (work vars (cons inner todos) dones)]
           [(c-structural-record-type f?1 known-fields-1 lb-1 ub-1)
            (work (cons t vars) (append (dict-values known-fields-1) todos) dones)]
           [(generic-type name constructor inners variances)
            (work vars (append inners todos) dones)]
           [(c-type-variable (list single-it) _ _)
            (define not-done-inners (if (memq single-it dones)
                                        (list)
                                        (list single-it)))
            (work (cons t vars)
                  (append not-done-inners todos)
                  dones)]

           [(c-type-variable (list its ...) _ _)
            (define not-done-inners (listeq-subtract its dones))
            (work (cons t vars)
                  (append not-done-inners todos)
                  dones)]
           [(c-type-variable #f _ _)
            (work (cons t vars)
                  todos
                  dones)]))]))
  (work '() (list orig-type) '()))

(define (type-contains-function-type? t #:potential? [potential? #f])
  ;; Recursively checks for function types within a given type.
  ;; This is for higher-order effect tracking
  (define (rec it)
    (type-contains-function-type? it #:potential? potential?))
  (match t
    [(base-type _ _ _) #f]
    [(base-type-range _ _) #f]
    ;; Though parameter-types should only be found inside function types...
    [(parameter-type _) #f]
    [(function-type arg ret) #t]
    [(product-type inners lb ub)
     (if potential?
         ;; TODO - I noticed this while working on something else, but I think
         ;;        this is wrong.  The test for the if should be
         ;;        (and potential (not inners)), so it doesn't return #t
         ;;        for concrete product types that don't have anything inside.
         ;;        But I want to actually fix this separately.
         #t
         (when (not inners)
           (error 'type-contains-function-type? "given non-settled type: ~v" t)))
     (for/or ([it inners])
       (rec it))]
    [(c-nominal-record-type name super known-fields lb ub)
     (if (and potential? (not (settled? t)))
         #t
         (for/or ([it (dict-values known-fields)])
           (rec it)))]
    [(nominal-record-definition-type inner) #f]
    [(c-structural-record-type f?1 known-fields-1 lb-1 ub-1)
     (if (and potential? (not (settled? t)))
         #t
         (for/or ([it (dict-values known-fields-1)])
           (rec it)))]
    [(generic-type name constructor inners variances)
     (for/or ([it inners])
       (rec it))]
    [(c-type-variable (list single-it) _ _)
     (rec single-it)]
    [(c-type-variable (list its ...) _ _)
     (for/or ([it its])
       (rec it))]
    [(c-type-variable #f _ _)
     (if potential?
         #t
         (error 'type-contains-function-type? "given non-settled type: ~v" t))]))


(define (structurally-recur-on-type/where-polymorphism-valid
         t transformer
         ;; if collect? is true, make a list of the result
         ;; of the transformer on the type and its children
         ;; instead of reconstructing the type.
         #:collect? [collect? #f])
  (define (rec t)
    (structurally-recur-on-type/where-polymorphism-valid t transformer
                                                         #:collect? collect?))
  ;; Do preorder traversal.
  (define t* (transformer t))
  (match (if collect? t t*)
    [(function-type arg ret)
     (if collect?
         (list t* (rec arg) (rec ret))
         (function-type (rec arg) (rec ret)))]
    [(product-type (list inners ...) lb ub)
     (if collect?
         (cons t* (map rec inners))
         (mk-product-type (map rec inners)))]
    [(generic-type name constructor inners variances)
     (if collect?
         (cons t* (map rec inners))
         (generic-type name constructor (map rec inners) variances))]
    [(c-type-variable (list single-it) _ _)
     (rec single-it)]
    ;; All other types are either atomic or are unsupported for
    ;; doing polymoprhic function replacement.
    [(or
      (? base-type?)
      (? base-type-range?)
      (? parameter-type?)
      (? nominal-record-type?)
      (? nominal-record-definition-type?)
      (? structural-record-type?)
      (? core-type-variable?))
     t*]))


;; TODO - I should make a more generic type tree walking function.
;; It could have an optional predicate for whether to recur, what
;; function to apply to child nodes, an option to reconstruct the same
;; type with the transformed children or collect the child results
;; (and results on the node itself) in some other way, a function to
;; apply to the type when not recurring into it, a way to potentially
;; short-circuit evaluation based on an intermediate result... It
;; could potentially replace several functions in this file with more
;; declarative specifications of how to do the traversal. But it has to
;; support a robust set of specifications to be able to subsume so many
;; other ad-hoc tree walking functions.


(define (type->function-type-list-for-polymorphism t)
  (define (transformer t)
    (if (function-type? t)
        t
        '()))
  (flatten
   (list
    (structurally-recur-on-type/where-polymorphism-valid
     t transformer #:collect? #t))))

(define (type-swap-for-polymorphism t old-t new-t)
  (define (transformer t)
    (if (can-unify? t old-t)
        new-t
        t))
  (structurally-recur-on-type/where-polymorphism-valid t transformer))

(define (replace-parametric-types-with-variables t)
  (define param-hash (make-hasheq))
  (define (transformer t)
    (match t
      [(parameter-type sym)
       (if (hash-has-key? param-hash sym)
           (hash-ref param-hash sym)
           (let ([ftv (fresh-type-variable)])
             (hash-set! param-hash sym ftv)
             ftv))]
      [else t]))
  (structurally-recur-on-type/where-polymorphism-valid t transformer))

(define (type->type-list-for-polymorphism t)
  ;; return a flattened list of all types in the tree t,
  ;; except not recurring into those where polymoprhic replacements
  ;; are invalid.
  (flatten
   (list
    (structurally-recur-on-type/where-polymorphism-valid t (λ(x)x) #:collect? #t))))

(define (make-parametric-type-based-on t)
  ;; Here we make a type that maybe has parameter types in it that
  ;; could unify with type t.
  ;; To make this simple, we are going to concretize the type first.
  (define ct (concretize-type t))
  ;; A parametric type needs to have the same parameter type in both the
  ;; input and output sides of a function.

  (define function-types (type->function-type-list-for-polymorphism ct))

  (define (function-type->replacement-candidate ft)
    (define arg-types (type->type-list-for-polymorphism
                       (function-type-arg-type ft)))
    (define ret-types (type->type-list-for-polymorphism
                       (function-type-return-type ft)))
    (define ret-types-shuffled (wrap-external-randomness
                                (shuffle ret-types)))
    (for/or ([ret ret-types-shuffled])
      (for/or ([arg arg-types])
        (and (can-unify? arg ret) ret))))

  (define replacement-choice-list
    (for/or ([ft (wrap-external-randomness
                  (shuffle function-types))])
      (define choice (function-type->replacement-candidate ft))
      (and choice (list ft choice))))

  (define result
    (and replacement-choice-list
         ;; Replace the type with a parameter just in the function type,
         ;; then replace that function type in the overall type.
         (type-swap-for-polymorphism
          t
          (car replacement-choice-list)
          (type-swap-for-polymorphism (car replacement-choice-list)
                                      (cadr replacement-choice-list)
                                      (make-fresh-parameter-type)))))

  (define (make-more-parametric-with-probability t)
    ;; TODO - make the probability configurable.
    (if (random-bool)
        (make-parametric-type-based-on t)
        t))

  (if result
      (make-more-parametric-with-probability result)
      t))

(module+ test
  (require
   quickcheck
   rackunit/quickcheck
   (prefix-in r: racket/base)
   )
  (let ()
    (define t1
      (replace-parametric-types-with-variables
       (function-type (parameter-type 'a) (parameter-type 'a))))
    (check-true (can-unify? t1 (function-type dog dog)))
    (check-false (can-unify? t1 (function-type dog bird)))

    ;; Quickcheck test for round-trip unification of
    ;; type -> parameterized type -> type with variables
    ;; conversion.
    (define-generic-type my-list-type ([type covariant]))
    (define function-type-generator
      (make-generator
       (λ (size random-generator)
         ;; TODO - this throws away the quickcheck random-generator, but I don't
         ;; think it really matters much.
         ;; TODO - it would be good to use the size argument
         (with-new-random-source #:seed (random-seed-value)
           (parameterize ([current-xsmith-type-constructor-thunks
                           (list
                            (λ() dog)
                            (λ() bird)
                            (λ() (my-list-type (fresh-type-variable)))
                            (λ() (function-type (fresh-type-variable)
                                                (fresh-type-variable)))
                            (λ() (function-type (mk-product-type #f)
                                                (fresh-type-variable)))
                            (λ() (mk-product-type #f))
                            (λ() (fresh-structural-record-type))
                            )])
             (concretize-type (function-type (fresh-type-variable)
                                             (fresh-type-variable))))))))

    (define replace-parametric-types-with-variables/round-trip-property
      (property ([ftype function-type-generator])
                (let ([parameterized (replace-parametric-types-with-variables
                                      (make-parametric-type-based-on
                                       ftype))])
                  (and (can-unify? ftype parameterized)
                       (void?
                        (with-handlers ([(λ (e) #t) (λ (e) e)])
                          (unify! ftype parameterized)))))))
    ;; TODO - I don't remember the real random seed max...
    (define random-max-int (expt 2 29))
    (with-new-random-source #:seed (r:random random-max-int)
      (check-property replace-parametric-types-with-variables/round-trip-property))
    ))


;; This is a copy/pasted version of list-subtract from the `set` generic implementation.  The generic uses `equal?`-based testing, but I'm only using it on things where I want `eq?`-based testing.
(define (listeq-subtract s . sets)
  #;(for ([s2 (in-list sets)] [i (in-naturals 1)])
      (unless (list? s2)
        (apply raise-argument-error 'set-subtract "list?" i s sets)))
  (for/fold
      ([s1 '()])
      ([x (in-list s)]
       #:unless (for/or ([s2 (in-list sets)])
                  (memq x s2)))
    (cons x s1)))


(module+ test
  (define v1 (fresh-type-variable))
  (define v2 (fresh-type-variable))
  (define v3 (fresh-type-variable (mk-base-type 'foo)
                                  (function-type v1 v2)))
  (define p1 (mk-product-type #f))
  (define p2 (mk-product-type #f))
  (define p3 (mk-product-type #f))
  (define v4 (fresh-type-variable (mk-base-type 'bar)
                                  (function-type p1 v3)))

  (check-not-false (contains-type-variables? v1 (type->type-variable-list v4)))
  (check-not-false (contains-type-variables? v1 (type->type-variable-list v3)))
  (check-false (contains-type-variables? v1 (type->type-variable-list v2)))

  (define (s= l r)
    (set=? (apply seteq l)
           (apply seteq r)))
  (check-true
   (s= (type->type-variable-list v3)
       (list v1 v2 v3)))
  (check-true
   (s= (type->type-variable-list v4)
       (flatten (list v1 v2 v3 v4 (type->type-variable-list p1)))))
  (unify! v1 v2)
  (check-eq? (variable-canonicalize v1)
             (variable-canonicalize v2))
  (check-true
   (s= (type->type-variable-list v1)
       (type->type-variable-list v2)))
  (check-true
   (s= (type->type-variable-list v3)
       (map canonicalize-if-variable (list v1 v3))))
  (check-not-false (contains-type-variables? v1 (type->type-variable-list v2)))

  (check-false (s= (product-type->all-transitive-bounds p1)
                   (product-type->all-transitive-bounds p2)))
  (unify! p1 p2)
  (check-true (s= (product-type->all-transitive-bounds p1)
                  (product-type->all-transitive-bounds p2)))
  (check-not-false (contains-type-variables?
                    v4 (list (canonicalize-if-variable v1))))
  (check-not-false (contains-type-variables? v4 (type->type-variable-list p1)))
  (check-not-false (contains-type-variables? v4 (type->type-variable-list p2)))
  (check-false (contains-type-variables? v4 (type->type-variable-list p3)))
  (unify! p2 (mk-product-type (list v1 v2 v3)))
  (check-true (contains-type-variables? v1 (type->type-variable-list p1)))
  (check-true (contains-type-variables? p1 (type->type-variable-list v1)))
  (check-true (contains-type-variables? v3 (type->type-variable-list p1)))
  (check-true (contains-type-variables? p1 (type->type-variable-list v3)))

  (unify! p3 p1)
  (check-true (s= (type->type-variable-list p1)
                  (type->type-variable-list p2)))
  (check-true (s= (type->type-variable-list p1)
                  (type->type-variable-list p3)))

  ;; check with subtype-unification
  (define sv1 (fresh-type-variable))
  (define sv2 (fresh-type-variable))
  (define sv3 (fresh-type-variable (mk-base-type 'foo)
                                   (function-type sv1 sv2)))
  (define sv4 (fresh-type-variable))
  (define sv5 (fresh-type-variable))
  (define sp1 (mk-product-type #f))
  (define sp2 (mk-product-type #f))
  (subtype-unify! sv1 sp1)
  (subtype-unify! sv4 sv3)
  (subtype-unify! sv3 sv5)

  (check-true (s= (type->type-variable-list sv4)
                  (type->type-variable-list sv3)))
  (check-true (contains-type-variables? sv1 (type->type-variable-list sp1)))
  (check-true (contains-type-variables? sp1 (type->type-variable-list sv1)))
  (check-true (subset? (type->type-variable-list sv1)
                       (type->type-variable-list sv5)))


  ;; Check differences between symmetric and subtype can-unify?
  (define birddog1 (fresh-type-variable (mk-base-type-range bird bird)
                                        (mk-base-type-range labradoodle labradoodle)))
  (define birddog2 (fresh-type-variable (mk-base-type-range dog dog)
                                        (mk-base-type-range penguin penguin)))
  (check-true (can-subtype-unify? birddog1 birddog2))
  (check-true (can-subtype-unify? birddog2 birddog1))
  (check-false (can-unify? birddog1 birddog2))
  (check-false (can-unify? birddog2 birddog1))
  (subtype-unify! birddog1 birddog2)
  (check-true (can-subtype-unify? birddog1 canine))
  (check-false (can-subtype-unify? birddog1 bird))
  (check-true (can-subtype-unify? labradoodle birddog2))
  (check-false (can-subtype-unify? birddog2 labradoodle))

  (check-equal? (base-type->parent-chain penguin)
                (list penguin bird animal))

  (define animal1 (fresh-subtype-of animal))
  (define bird1 (fresh-type-variable))
  (subtype-unify! bird1 animal)
  (subtype-unify! bird1 animal1)
  (subtype-unify! bird1 bird)
  (check-true (can-subtype-unify? bird1 penguin))
  (check-true (can-subtype-unify? penguin bird1))
  (define penguin1 (fresh-type-variable))
  (subtype-unify! penguin1 animal1)
  (subtype-unify! penguin1 penguin)
  (check-true (can-subtype-unify? penguin1 bird))
  (check-true (can-subtype-unify? penguin penguin1))
  (check-not-false (can-subtype-unify? penguin1 bird1))
  (check-not-false (can-subtype-unify? bird1 penguin1))

  (check-false (can-subtype-unify? bird penguin1))

  ;; Tests for my subtyping + function parameter bug
  (define bird-will-be-penguin (fresh-type-variable (mk-base-type-range #f bird)))
  (define bird-will-be-penguin/left (fresh-type-variable (mk-base-type-range #f bird)))
  (define bird-will-be-penguin/right (fresh-type-variable (mk-base-type-range #f bird)))
  (unify! bird-will-be-penguin penguin)
  (subtype-unify! bird-will-be-penguin/left penguin)
  (subtype-unify! penguin bird-will-be-penguin/right)
  (check-equal? (car (c-type-variable-type bird-will-be-penguin))
                (mk-base-type-range penguin penguin))
  (check-equal? (car (c-type-variable-type bird-will-be-penguin/left))
                (mk-base-type-range #f penguin))
  (check-equal? (car (c-type-variable-type bird-will-be-penguin/right))
                (mk-base-type-range penguin bird))


  ;; testing contains-type-variables over a graph
  (define v-a (fresh-type-variable))
  (define v-a+b (fresh-type-variable))
  (define v-a+a (fresh-type-variable))
  (define v-aa (fresh-type-variable))
  (define v-ab (fresh-type-variable))
  (define v-aaa (fresh-type-variable))
  (define v-aab (fresh-type-variable))
  (define v-aab+a (fresh-type-variable))
  (define v-aab+b (fresh-type-variable))

  (subtype-unify! v-aa v-a)
  (subtype-unify! v-ab v-a)
  (subtype-unify! v-aaa v-aa)
  (subtype-unify! v-a v-a+a)
  (subtype-unify! v-a v-a+b)
  (subtype-unify! v-aab v-aab+a)
  (subtype-unify! v-aab v-aab+b)

  ;; do some tests before connecting everything
  (define (ccv #:t [t #t] l r)
    (define c (if t check-true check-false))
    (c (contains-type-variables? l (type->type-variable-list r)))
    (c (contains-type-variables? r (type->type-variable-list l))))
  (ccv v-aa v-a+a)
  (ccv v-aaa v-a+b)
  (ccv #:t #f v-aab v-a+b)
  (ccv v-aab+a v-aab)

  ;; connect the rest of the graph.  Now they should all be connected.
  (subtype-unify! v-aab v-aa)
  (define graph-nodes (list v-a v-a+b v-a+a v-aa v-ab v-aaa v-aab v-aab+a v-aab+b))
  (for* ([n1 graph-nodes]
         [n2 graph-nodes])
    (ccv n1 n2))


  (define tv-nominal-record-type (fresh-type-variable (any-nominal-record-type)))
  (check-true (can-unify? tv-nominal-record-type (any-nominal-record-type)))
  (check-not-exn (λ () (unify! tv-nominal-record-type (any-nominal-record-type))))


  ;; tests for generic types and subtyping
  (let ()
    (define-generic-type g ([i invariant] [co covariant] [contra contravariant]))

    (define-syntax (subtype-check stx)
      (syntax-parse stx
        [(_ t/f sub super)
         #`(let ([bool t/f]
                 [b sub]
                 [p super])
             #,(syntax/loc stx (check-equal? (can-subtype-unify? b p) bool))
             (if bool
                 #,(syntax/loc stx (check-not-exn (λ () (subtype-unify! b p))))
                 #,(syntax/loc stx (check-exn exn? (λ () (subtype-unify! b p))))))]))

    (subtype-check #t (g dog dog dog) (g dog dog dog))
    (subtype-check #t (g dog labradoodle animal) (g dog dog dog))

    ;; check that invariance holds
    (subtype-check #f
                   (g labradoodle labradoodle animal)
                   (g dog dog dog))
    (subtype-check #f
                   (g animal labradoodle animal)
                   (g dog dog dog))
    ;; check that covariance holds
    (subtype-check #f
                   (g dog animal animal)
                   (g dog dog dog))
    ;; check that contravariance holds
    (subtype-check #f
                   (g dog labradoodle labradoodle)
                   (g dog dog dog))
    )

  ;; tests for structural records
  (let ()
    (define (ffsrt ht)
      (fresh-structural-record-type #:finalized? #t ht))
    (check-false (can-unify? (ffsrt (hash 'x dog))
                             (ffsrt (hash 'x dog 'y dog))))
    (check-false (can-subtype-unify? (ffsrt (hash 'x dog))
                                     (ffsrt (hash 'x dog 'y dog))))
    (check-true (can-subtype-unify? (ffsrt (hash 'x dog 'y dog))
                                    (ffsrt (hash 'x dog))))
    (check-true (can-subtype-unify? (ffsrt (hash 'x labradoodle
                                                 'y dog))
                                    (ffsrt (hash 'x dog))))
    (check-true (can-subtype-unify? (ffsrt (hash 'x labradoodle
                                                 'y dog))
                                    (fresh-subtype-of
                                     (ffsrt (hash 'x dog)))))
    (check-false (can-subtype-unify? (ffsrt (hash 'x labradoodle
                                                  'y dog))
                                     (fresh-subtype-of
                                      (ffsrt (hash 'x dog 'z bird)))))
    (check-true (can-unify? (ffsrt (hash 'x labradoodle
                                         'y dog))
                            (fresh-subtype-of
                             (ffsrt (hash)))))

    ;; non-finalized SRTs
    (define x-dog-1 (fresh-structural-record-type (hash 'x dog)))
    (check-true (can-unify? x-dog-1
                            (ffsrt (hash 'x dog 'y dog))))
    (check-true (can-subtype-unify? x-dog-1
                                    (ffsrt (hash 'x dog 'y dog))))
    (unify! x-dog-1 (ffsrt (hash 'x dog 'y dog)))
    (check-false (can-unify? x-dog-1 (ffsrt (hash 'x dog))))

    (define x-dog-2 (fresh-structural-record-type (hash 'x dog)))
    (check-true (can-subtype-unify? x-dog-2
                                    (ffsrt (hash 'x dog 'y dog))))
    (subtype-unify! x-dog-2 (ffsrt (hash 'x dog 'y dog)))
    (check-true (can-subtype-unify? x-dog-2
                                    (ffsrt (hash 'x dog 'y labradoodle))))

    )

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
