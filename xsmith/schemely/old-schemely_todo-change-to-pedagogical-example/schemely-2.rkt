#!/usr/bin/env racket
#lang racket/base
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2019 The University of Utah
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

(require xsmith racr racket/pretty racket/random)

(define-spec-component schemely-core)

(add-to-grammar
 schemely-core
 [Program #f ([expressions : Expression * = (add1 (random 5))])]

 [Expression #f ()
             #:prop may-be-generated #f]

 [LiteralBool Expression ([v = (even? (random 2))])]
 [Not Expression ([Expression])]
 ;; TODO - many of these I've defined as binary but they could be variadic instead.
 [And Expression ([l : Expression] [r : Expression])]
 [Or Expression ([l : Expression] [r : Expression])]

 [LiteralNumber Expression ([v = (random 100)])]
 [Plus Expression ([l : Expression] [r : Expression])]
 [Minus Expression ([l : Expression] [r : Expression])]
 [Times Expression ([l : Expression] [r : Expression])]
 [Divide Expression ([l : Expression] [r : Expression])]
 [LessThan Expression ([l : Expression] [r : Expression])]
 [GreaterThan Expression ([l : Expression] [r : Expression])]

 [LiteralString Expression ([v = (random-ref (list "foo" "bar" "baz" "quux"))])]
 [StringAppend Expression ([l : Expression] [r : Expression])]
 [StringLength Expression (Expression)]

 [LiteralEmptyList Expression ()]
 [List Expression ([arguments : Expression * = (add1 (random 8))])]
 [SafeCar Expression ([default : Expression] [list : Expression])]
 [SafeCdr Expression (Expression)]
 [EmptyP Expression (Expression)]

 [EqualP Expression ([l : Expression] [r : Expression])]

 [If Expression ([test : Expression] [then : Expression] [else : Expression])
     #:prop strict-child-order? #t]

 )


;; helper for to-s-exp
(define (->se sym . children-refs)
  (λ (n) `(,sym ,@(map (λ (x) (att-value 'to-s-exp (ast-child x n)))
                       children-refs))))
(define (->se* sym children-ref)
  (λ (n) `(,sym ,@(map (λ (x) (att-value 'to-s-exp x))
                       (ast-children (ast-child children-ref n))))))

(add-ag-rule
 schemely-core
 to-s-exp
 [Program (λ (n) `(begin ,@(map (λ (x) (att-value 'to-s-exp x))
                                (ast-children (ast-child 'expressions n)))))]

 [LiteralBool (λ (n) (ast-child 'v n))]
 [Not (->se 'not 'Expression)]
 [And (->se 'and 'l 'r)]
 [Or (->se 'or 'l 'r)]

 [LiteralNumber (λ (n) (ast-child 'v n))]
 [Plus (->se '+ 'l 'r)]
 [Minus (->se '- 'l 'r)]
 [Times (->se '* 'l 'r)]
 [Divide (->se '/ 'l 'r)]
 [LessThan (->se '< 'l 'r)]
 [GreaterThan (->se '> 'l 'r)]

 [LiteralString (λ (n) (ast-child 'v n))]
 [StringAppend (->se 'string-append 'l 'r)]
 [StringLength (->se 'string-length 'Expression)]

 [LiteralEmptyList (λ (n) ''())]
 [List (->se* 'list 'arguments)]
 [SafeCar (->se 'safe-car 'default 'list)]
 [SafeCdr (->se 'safe-cdr 'Expression)]
 [EmptyP (->se 'null? 'Expression)]

 [EqualP (->se 'equal? 'l 'r)]

 [If (->se 'if 'test 'then 'else)]
 )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types

(define number (base-type 'number))
(define (number-type? x) (can-unify? x number))
(define bool (base-type 'bool))
(define (bool-type? x) (can-unify? x bool))
(define string (base-type 'string))
(define (string-type? x) (can-unify? x string))

(define (type-thunks-for-concretization) (list (λ()number) (λ()bool) (λ()string)))

(define (list-type x)
  (generic-type 'list (list x)))
(define (list-type? x)
  (and (generic-type? x)
       (eq? (generic-type-name x)
            'list)))
(define (fresh-list-type) (list-type (fresh-type-variable)))
(define (list-type-type x)
  (when (not (list-type? (concretize-type x)))
    (error 'list-type-type "given ~a\n" x))
  (define fl (fresh-list-type))
  (unify! x fl)
  (car (generic-type-type-arguments fl)))

(define no-child-types (λ (n t) (hash)))


(define numeric-bin-op-type (λ (n t) (hash 'l number 'r number)))
;; TODO - specifying a default with #f seems broken at the moment.
(add-prop
 schemely-core type-info
 [Program [(fresh-type-variable) (λ (n t) (for/hash ([c (ast-children
                                                         (ast-child 'expressions n))])
                                            (values c (fresh-type-variable))))]]

 [LiteralBool [bool (no-child-types)]]
 [Not [bool (λ (n t) (hash 'Expression bool))]]
 [And [bool (λ (n t) (hash 'l bool 'r bool))]]
 [Or [bool (λ (n t) (hash 'l bool 'r bool))]]

 [LiteralNumber [number (no-child-types)]]
 [Plus [number numeric-bin-op-type]]
 [Minus [number numeric-bin-op-type]]
 [Times [number numeric-bin-op-type]]
 [Divide [number numeric-bin-op-type]]
 [LessThan [bool numeric-bin-op-type]]
 [GreaterThan [bool numeric-bin-op-type]]

 [LiteralString [string (no-child-types)]]
 [StringAppend [string (λ (n t) (hash 'l string 'r string))]]
 [StringLength [number (λ (n t) (hash 'Expression string))]]

 [LiteralEmptyList [(fresh-list-type) (no-child-types)]]
 [List [(fresh-list-type) (λ (n t)
                            (define lt (fresh-list-type))
                            (unify! t lt)
                            (define inner (list-type-type lt))
                            (for/hash ([c (ast-children (ast-child 'arguments n))])
                              (values c inner)))]]
 [SafeCar [(fresh-type-variable) (λ (n t) (hash 'default t 'list (list-type t)))]]
 [SafeCdr [(fresh-list-type) (λ (n t) (hash 'Expression t))]]
 [EmptyP [bool (λ (n t) (hash 'Expression (fresh-list-type)))]]

 [EqualP [bool (λ (n t)
                 (define arg-type (fresh-type-variable))
                 (hash 'l arg-type 'r arg-type))]]

 [If [(fresh-type-variable) (λ (n t) (hash 'test bool 'then t 'else t))]]
 )

(assemble-spec-components
 ;; TODO - have this macro check the name -- it can't have dashes or other things that RACR doesn't allow...
 schemely
 schemely-core)

(define (generate-and-print)
  (parameterize ([current-xsmith-type-constructor-thunks
                  (type-thunks-for-concretization)])
    (pretty-print
     (att-value 'to-s-exp (schemely-generate-ast 'Program))
     (current-output-port)
     1)))

(module+ main
  (xsmith-command-line generate-and-print)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.