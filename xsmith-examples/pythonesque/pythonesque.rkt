#!/usr/bin/env racket
#lang clotho
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2019-2020 The University of Utah
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

(require
 xsmith
 xsmith/app
 xsmith/racr-convenience
 racr
 pprint
 racket/class
 racket/dict
 (except-in racket/list empty)
 racket/string
 "../private/xsmith-examples-version.rkt"
 )

(define-spec-component pythonesque-grammar)

#|
Broad Future Goals:
  [x] basic types (int, bool)
  [x] functions
    [x] definitions
    [x] calls
    [x] as values
  [x] if/else
  [ ] arrays
  [ ] try/except
  [ ] loops
    [ ] while
    [ ] for ... in
  [ ] classes
    [ ] definitions
    [ ] instantiation
  [ ] runtime type modification
    [ ] grafting
    [ ] pruning
  [ ] union types (e.g., variable could be either string or integer)
  [ ] "uninitialized" variables (start as None and gain value later)
  [ ] standard library calls (e.g., itertools, functools, dataclasses, etc.)

Fixes:
  [ ] functions should always return the same "type" (no None fallthrough)
       - actually maybe functions should just *always* return something?
  [ ] so many pass statements omg
|#

;;;;
;; Grammar specification.

(define (pick-int)
  (random 100))

(define (pick-bool)
  (random-ref (list #t #f)))

(add-to-grammar
 pythonesque-grammar
 ; Special nodes.
 [Node #f ()]
 [Program Node ([decls : VarDecl * = (add1 (random 5))]
                [funcs : FuncDecl * = (random 3)]
                [main : FuncDecl])]
 ; Declarations.
 [Decl Node (name type)]
 [FuncDecl Decl ([params : ParamDecl *]
                 BlockOrStmt)]
 [ParamDecl Decl ()]
 [VarDecl Decl (Val)]
 ; BlockOrStmt super-node.
 [BlockOrStmt Node ()]
 ; Blocks.
 [Block BlockOrStmt ([decls : VarDecl * = (random 2)]
                     [stmts : Stmt * = (add1 (random 4))])]
 ; Statements.
 [Stmt BlockOrStmt ()]
 [AssignStmt Stmt (name Expr)]
 [PassStmt Stmt ()]
 [ReturnStmt Stmt ()]
 [ValReturnStmt ReturnStmt (Expr)]
 [ExprStmt Stmt (Expr)]
 [IfStmt Stmt ([test : Expr]
               [then : BlockOrStmt])]
 [IfElseStmt IfStmt ([else : BlockOrStmt])]
 ; Expressions.
 [Expr Node ()]
 [FuncAppExpr Expr ([func : VarRefExpr]
                    [args : Expr *])]
 [BinExpr Expr ([lhs : Expr]
                [rhs : Expr])]
 [AddExpr BinExpr ()]  ;; TODO - I would like to be able to combine all these.
 [SubExpr BinExpr ()]
 [MulExpr BinExpr ()]
 [DivExpr BinExpr ()]
 [VarRefExpr Expr (name)]
 ; Values.
 [Val Node ()]
 [NoneVal Val ()]
 [IntVal Val ([val = (pick-int)])]
 [BoolVal Val ([val = (pick-bool)])]
 )

;;;;
;; Properties.

(add-property
 pythonesque-grammar
 may-be-generated
 [Node #f]
 [Decl #f]
 [BlockOrStmt #f]
 [Stmt #f]
 [ReturnStmt #f]
 [Expr #f]
 [BinExpr #f]
 [Val #f]
 )

(add-property
 pythonesque-grammar
 strict-child-order?
 [Program #t]
 [Block #t]
 [IfStmt #t]
 )

(define (no-increase)
  (λ (n) 0))

(add-property
 pythonesque-grammar
 depth-increase
 [Program (no-increase)]
 [Decl (no-increase)]
 [Block (λ (n) (if (member (node-type (parent-node n))
                           '(IfStmt
                             IfElseStmt
                             FuncDecl))
                   (att-value 'xsmith_ast-depth (parent-node n))
                   (add1 (att-value 'xsmith_ast-depth (parent-node n)))))]
 [AssignStmt (no-increase)]
 [ExprStmt (no-increase)]
 )

(add-property
 pythonesque-grammar
 wont-over-deepen
 [VarDecl #t]
 [AssignStmt #t]
 [ExprStmt #t]
 [ValReturnStmt #t]
 )

; Binding and reference information.

(add-property
 pythonesque-grammar
 binder-info
 [Decl ()]
 [FuncDecl ()]
 [ParamDecl ()]
 [VarDecl ()]
 )

(add-property
 pythonesque-grammar
 reference-info
 [AssignStmt (write #:unifies Expr)]
 [VarRefExpr (read)]
 )

; Fresh generation.

(define (fresh-concrete-var-type)
  (concretize-type (fresh-type-variable)))
(define (fresh-concrete-function-type)
  (concretize-type (function-type (product-type #f)
                                  (fresh-type-variable))))

(add-property
 pythonesque-grammar
 fresh
 ; Declarations.
 [FuncDecl
  (λ (lift-fields)
    (let* ([parent (parent-node (current-hole))]
           [main? (and (eq? (node-type parent) 'Program)
                       (eq? (ast-child 'main parent) (current-hole)))]
           [name (or (dict-ref lift-fields 'name #f)
                     (if main?
                         "inner_main"
                         (fresh-var-name "func_")))]
           [type (or (dict-ref lift-fields 'type #f)
                     (if main?
                         (function-type (product-type '()) int)
                         (fresh-concrete-function-type)))])
      (hash 'name name
            'type type
            'params (map (λ (t) (make-fresh-node 'ParamDecl (hash 'type t)))
                         (product-type-inner-type-list
                          (function-type-arg-type
                           type))))))]
 [ParamDecl
  (hash 'name (fresh-var-name "arg_")
        'type (fresh-type-variable))]
 [VarDecl
  (hash 'name (if (equal? (top-ancestor-node (current-hole))
                          (parent-node (current-hole)))
                  (fresh-var-name "GLOBAL_")
                  (fresh-var-name "local_"))
        'type (fresh-concrete-var-type))]
 ; Expressions.
 [FuncAppExpr
  (hash 'func (make-hole 'VarRefExpr)
        ;; The empty 'args' list will be rewritten by 'fresh' for VarRefExpr.
        'args 0)]
 [VarRefExpr
  (hash 'name (λ ()
                (let* ([choice (send this xsmith_get-reference!)]
                       [parent (parent-node (current-hole))])
                  (when (and (ast-subtype? parent 'FuncAppExpr)
                             (eq? (current-hole) (ast-child 'func parent)))
                    (let ([arg-children (create-ast-list
                                         (map (λ (x) (make-hole 'Expr))
                                              (product-type-inner-type-list
                                               (function-type-arg-type
                                                (binding-type choice)))))])
                      (enqueue-inter-choice-transform
                       (λ ()
                         (rewrite-subtree
                          (ast-child 'args parent)
                          arg-children)))))
                  (binding-name choice))))]
 )

; Lifting.

(add-property
 pythonesque-grammar
 lift-predicate
 [Program (λ (n t) #t)]
 [Block (λ (n t) (and (not (function-type? t))
                      (not (nominal-record-definition-type? t))))]
 [FuncDecl #f]
 )

(add-property
 pythonesque-grammar
 lift-type->ast-binder-type
 [#f (λ (t) (cond
              [(function-type? t) 'FuncDecl]
              [else 'VarDecl]))]
 )

; Choice weights.

;; TODO - add function to get default (#f) weight
;; TODO - allow a separate default weight specification that #f relies on by default
;; TODO - investigate other properties that are intended for #f-only implementations and rewrite specification

(add-property
 pythonesque-grammar
 choice-weight
 [#f 100]
 [PassStmt 1]
 [ExprStmt (λ () (if (>= (att-value 'xsmith_ast-depth (current-hole))
                         (- (xsmith-max-depth) 1))
                     0
                     100))]
 [VarRefExpr (λ () (if (parent-node-has-type? 'ExprStmt (current-hole))
                       0
                       100))]
 [Val (λ () (if (parent-node-has-type? 'ExprStmt (current-hole))
                0
                100))]
 )

; Types.

(define unit (base-type 'unit))
(define stmt (base-type 'stmt))
(define int (base-type 'int))
(define bool (base-type 'bool))

(define-generic-type return-type (type))
(define-generic-type no-return-type (type))

(define (no-child-types)
  (λ (n t) (hash)))

(define (fresh-no-return)
  (no-return-type (fresh-type-variable)))

(define (fresh-maybe-return)
  (fresh-type-variable (return-type (fresh-type-variable))
                       (no-return-type (fresh-type-variable))))

(define (usable-types)
  (fresh-type-variable int bool))

(define (bin-expr-types)
  (λ (n t) (hash 'lhs t
                 'rhs t)))

(add-property
 pythonesque-grammar
 type-info
 ; Special nodes.
 [Program [(fresh-type-variable)
           (λ (n t)
             (hash 'decls (λ (c) (fresh-type-variable))
                   'funcs (λ (c) (function-type (product-type #f) (fresh-type-variable)))
                   'main (function-type (product-type '()) int)))]]
 ; Statements.
 [Block [(fresh-maybe-return)
         (λ (n t)
           (define stmts (ast-children (ast-child 'stmts n)))
           (define last-stmt (car (reverse stmts)))
           (define stmt-dict
             (for/hash ([s stmts])
               (values s
                       (if (eq? s last-stmt)
                           t
                           (fresh-no-return)))))
           (for/fold ([dict stmt-dict])
                     ([d (ast-children (ast-child 'decls n))])
             (dict-set dict d (fresh-type-variable))))]]
 [AssignStmt [stmt
               (λ (n t)
                 (hash 'Expr (usable-types)))]]
 [PassStmt [(fresh-no-return) (no-child-types)]]
 [ReturnStmt [(error 'typing-non-value-return-stmt) (no-child-types)]]
 [ValReturnStmt [(return-type (fresh-type-variable))
                 (λ (n t)
                   (define rt (return-type (fresh-type-variable)))
                   (unify! t rt)
                   (hash 'Expr (return-type-type rt)))]]
 [ExprStmt [(fresh-no-return)
             (λ (n t)
               (hash 'Expr (fresh-type-variable)))]]
 [IfStmt [(fresh-no-return)
          (λ (n t) (hash 'test bool
                         'then t))]]
 [IfElseStmt [(fresh-maybe-return)
              (λ (n t)
                (hash 'test bool
                      'then t
                      'else t))]]
 ; Declarations.
 [FuncDecl [(function-type (product-type #f) (fresh-type-variable))
            (λ (n t)
              (let ([decl-type-ann (ast-child 'type n)]
                    [f-type (function-type (product-type #f)
                                           (fresh-type-variable))])
                (unify! t decl-type-ann)
                (unify! t f-type)
                (define arg-types (product-type-inner-type-list
                                   (function-type-arg-type f-type)))
                (hash-set
                 (for/hash ([arg (ast-children (ast-child 'params n))]
                            [arg-type arg-types])
                   (values arg arg-type))
                 'BlockOrStmt (return-type (function-type-return-type f-type)))))]]
 [ParamDecl [(fresh-type-variable) (no-child-types)]]
 [VarDecl [(fresh-type-variable)
           (λ (n t)
             (hash 'Val t))]]
 ; Expressions.
 [FuncAppExpr [(fresh-type-variable)
               (λ (n t)
                 (define args-type (product-type #f))
                 (define arg-nodes (ast-children (ast-child 'args n)))
                 (define func-node (ast-child 'func n))
                 (define arg-types (map (λ (c) (fresh-type-variable)) arg-nodes))
                 (when (not (att-value 'xsmith_is-hole? func-node))
                   (unify! args-type (product-type arg-types)))
                 (for/fold ([dict (hash 'func (function-type args-type t))])
                           ([a arg-nodes]
                            [t arg-types])
                   (dict-set dict a t)))]]
 [AddExpr [(fresh-type-variable int) (bin-expr-types)]]
 [SubExpr [(fresh-type-variable int) (bin-expr-types)]]
 [MulExpr [(fresh-type-variable int) (bin-expr-types)]]
 [DivExpr [(fresh-type-variable int) (bin-expr-types)]]
 [VarRefExpr [(fresh-type-variable) (no-child-types)]]
 ; Values.
 [NoneVal [unit (no-child-types)]]
 [IntVal [int (no-child-types)]]
 [BoolVal [bool (no-child-types)]]
 )

;;;;
;; Pretty printing.

(define (tab d)
  (indent 4 d))

(define (bin-expr op pn)
  (hs-append
   ($xsmith_render-node (ast-child 'lhs pn))
   (text op)
   ($xsmith_render-node (ast-child 'rhs pn))))

(define (pretty-print-children cns)
  (map (λ (cn) ($xsmith_render-node cn)) cns))

(add-property
 pythonesque-grammar
 render-node-info
 ; Special nodes.
 [Program (λ (n)
            (define decls (ast-children (ast-child 'decls n)))
            (define funcs (ast-children (ast-child 'funcs n)))
            (define main (ast-child 'main n))
            (v-append
             (v-concat
              (append
               (pretty-print-children decls)
               (list (text ""))
               (add-between
                (pretty-print-children (reverse (cons main funcs)))
                (text ""))
               (list (text ""))))
             ;; Implement the program entry-point (like 'main' in C).
             (text "if __name__ == '__main__':")
             (tab
              (v-append
               (text "result = inner_main()")
               (text "print(result)")))))]
 ; Declarations.
 [FuncDecl (λ (n)
             (define params (ast-children (ast-child 'params n)))
             (v-append
              (h-append
               (text "def ")
               (text (ast-child 'name n))
               (text "(")
               (h-concat
                (add-between
                 (map (λ (param)
                        ($xsmith_render-node param))
                      params)
                 (text ", ")))
               (text "):"))
              (tab ($xsmith_render-node (ast-child 'BlockOrStmt n)))))]
 [ParamDecl (λ (n)
              (text (ast-child 'name n)))]
 [VarDecl (λ (n)
            (hs-append
             (text (ast-child 'name n))
             (text "=")
             ($xsmith_render-node (ast-child 'Val n))))]
 ; Statements.
 [Block (λ (n)
          (h-append
           (v-concat
            (append
             (map (λ (cn) ($xsmith_render-node cn))
                  (ast-children (ast-child 'decls n)))
             (map (λ (cn) ($xsmith_render-node cn))
                  (ast-children (ast-child 'stmts n)))))))]
 [AssignStmt (λ (n)
               (hs-append
                (text (ast-child 'name n))
                (text "=")
                ($xsmith_render-node (ast-child 'Expr n))))]
 [PassStmt (λ (n)
             (text "pass"))]
 [ValReturnStmt (λ (n)
                  (hs-append
                   (text "return")
                   ($xsmith_render-node (ast-child 'Expr n))))]
 [ExprStmt (λ (n)
             ($xsmith_render-node (ast-child 'Expr n)))]
 [IfStmt (λ (n)
           (v-append
            (h-append
             (text "if ")
             ($xsmith_render-node (ast-child 'test n))
             (text ":"))
            (tab ($xsmith_render-node (ast-child 'then n)))))]
 [IfElseStmt (λ (n)
               (v-append
                (h-append
                 (text "if ")
                 ($xsmith_render-node (ast-child 'test n))
                 (text ":"))
                (tab ($xsmith_render-node (ast-child 'then n)))
                (text "else:")
                (tab ($xsmith_render-node (ast-child 'else n)))))]
 ; Expressions.
 [FuncAppExpr (λ (n)
                (h-append
                 ($xsmith_render-node (ast-child 'func n))
                 (text "(")
                 (h-concat
                  (add-between
                   (map (λ (arg) ($xsmith_render-node arg))
                        (ast-children (ast-child 'args n)))
                   (text ", ")))
                 (text ")")))]
 [AddExpr (λ (n) (bin-expr "+" n))]
 [SubExpr (λ (n) (bin-expr "-" n))]
 [MulExpr (λ (n) (bin-expr "*" n))]
 [DivExpr (λ (n) (bin-expr "//" n))]
 [VarRefExpr (λ (n)
               (text (ast-child 'name n)))]
 ; Values.
 [NoneVal (λ (n) (text "None"))]
 [IntVal (λ (n)
           (text (number->string (ast-child 'val n))))]
 [BoolVal (λ (n)
            (if (ast-child 'val n)
                (text "True")
                (text "False")))]
 )

(add-property
 pythonesque-grammar
 render-hole-info
 [#f (λ (h) (h-append
             (text "<")
             (text (symbol->string (ast-node-type h)))
             (text ">")))])

;;;;
;; Assemble.

(define concretized-types
  (map (λ (t) (λ () t))
       (list int bool)))

(define-xsmith-interface-functions
  [pythonesque-grammar]
  #:fuzzer-name pythonesque
  #:fuzzer-version xsmith-examples-version-string/no-name
  #:default-max-depth 8
  #:comment-wrap (λ (lines) (string-join
                             (map (λ (l) (format "# ~a" l)) lines)
                             "\n"))
  #:format-render (λ (d) (pretty-format d 120))
  #:type-thunks concretized-types
  )

(module+ main (pythonesque-command-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
