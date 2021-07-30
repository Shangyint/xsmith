#lang clotho

#|
This is a demo of 3 ways of doing memory accesses:
* Literal addresses
* Pointers (that ultimately get their values from literal addresses or other pointers)
* “Fake Vars”, which are literal addresses but that are tracked in Xsmith as variables.  Fake Vars are basically variable references as far as Xsmith is concerned, but are rendered as repeated use of the same literal value.

For a language like C or C++ you probably want to model something like
`malloc` or `new`.  But this is just a small demo for what you might
do with an array of “global memory” that you can just write into.
|#

(require xsmith racr racket/pretty racket/string racket/port)

(define-spec-component arith)

(add-to-grammar
 arith
 [Definition #f (name type Expression)
   #:prop binder-info ()]
 [Expression #f ()
             #:prop may-be-generated #f]
 [LetStar Expression ([definitions : Definition *]
                      [sideEs : Expression * = (random 2)]
                      Expression)
          #:prop strict-child-order? #t]
 [VariableReference Expression (name)
                    #:prop reference-info (read)]
 [SetBangRet Expression (name Expression)
             #:prop reference-info (write)]
 [LiteralInt Expression ([v = (random 100)])]
 [Addition Expression ([es : Expression * = (+ 1 (random 5))])
           #:prop choice-weight 50]

 [GlobalMemoryRead Expression ([address : Expression])
                   #:prop mutable-container-access (read 'pointer)
                   #:prop type-info
                   [(fresh-type-variable)
                    (λ (n t) (hash 'address (pointer t)))]]
 [GlobalMemoryWrite Expression ([address : Expression] [newval : Expression])
                    ;; Note that this global-memory-write returns the same type
                    ;; that it writes.  Yours may be different.
                    #:prop mutable-container-access (write 'pointer)
                    #:prop type-info
                    [(fresh-type-variable)
                     (λ (n t) (hash 'address (pointer t)
                                    'newval t))]]
 ;; Note that memory addresses may be accessed through a literal or variable
 [LiteralMemoryAddress Expression ([address = (fresh-address!)])
                       #:prop type-info [(pointer (fresh-type-variable))
                                         (λ (n t) (hash))]]
 [FakeVarRef Expression ([VariableReference])
             #:prop depth-increase 0
             #:prop wont-over-deepen #t
             ;; I think this fake-var stuff should maybe be limited to a specific
             ;; set of types, rather than any type.
             #:prop type-info [(pointer int)
                               (λ (n t)
                                 (define inner (fresh-type-variable))
                                 (unify! t (pointer inner))
                                 (hash 'VariableReference (fake-var inner)))]]
 [FakeVarLiteral Expression ([address = (fresh-address!)])
                 #:prop type-info [(fake-var int)
                                   (λ (n t) (hash))]]
 )


(define fresh-address-counter 0)
(define (fresh-address!)
  ;; You could use any number of methods for keeping track of addresses already
  ;; usend and how to generate new ones.  Let's say they just increase by #x1000
  ;; (hexadecimal in Racket uses #x prefix).
  (set! fresh-address-counter (+ fresh-address-counter #x1000))
  fresh-address-counter)

(define int (base-type 'int))
(define-generic-type pointer ([type invariant]))
(define-generic-type fake-var ([type invariant]))

(add-property
 arith type-info
 [Definition [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
 [LetStar [(fresh-type-variable)
           (λ (n t) (hash 'definitions (λ (cn) (fresh-type-variable))
                          'sideEs (λ (cn) (fresh-type-variable))
                          'Expression t))]]
 [LiteralInt [int (λ (n t) (hash))]]
 [VariableReference [(fresh-type-variable) (λ (n t) (hash))]]
 [SetBangRet [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
 [Addition [int (λ (n t) (hash 'es t))]])

(add-property
 arith render-node-info
 #|
 We have 2 modifications to our printer to support fake-vars.
 (1) We filter out definitions that have the fake-var type and don't
 print them.
 (2) We have special handling in the VariableReference node to pass
 printing through to the RHS of the definition node when it is a fake-var type.
 |#
 [LetStar
  (λ (n)
    `(let* (,@(map (λ (d)
                     `[,(string->symbol (ast-child 'name d))
                       ,(att-value 'xsmith_render-node
                                   (ast-child 'Expression d))])
                   (filter
                    (λ (dn)
                      ;; This filter function is more complicated than necessary so I can note that there are fake-var definitions without them printing.
                      (if (can-unify? (att-value 'xsmith_type dn)
                                      (fake-var (fresh-type-variable)))
                          (begin (eprintf "Not printing fake-var definition: ~a\n"
                                          (ast-child 'name dn))
                                 #f)
                          #t))
                    (ast-children (ast-child 'definitions n)))))
       ,@(map (λ (c) (att-value 'xsmith_render-node c))
              (ast-children (ast-child 'sideEs n)))
       ,(att-value 'xsmith_render-node (ast-child 'Expression n))))]
 [LiteralInt (λ (n) (ast-child 'v n))]
 [SetBangRet (λ (n) `(begin (set! ,(string->symbol (ast-child 'name n))
                                  ,(att-value 'xsmith_render-node
                                              (ast-child 'Expression n)))
                            ,(string->symbol (ast-child 'name n))))]
 [Addition (λ (n) `(+ ,@(map (λ (c) (att-value 'xsmith_render-node c))
                             (ast-children (ast-child 'es n)))))]


 [VariableReference
  (λ (n) (if (can-unify? (att-value 'xsmith_type n)
                         (fake-var (fresh-type-variable)))
             (let ([def-node (binding-ast-node
                              (att-value 'xsmith_binding n))])
               (eprintf "Rendering fake-var reference as a literal: ~a\n"
                        (ast-child 'name n))
               (att-value 'xsmith_render-node (ast-child 'Expression def-node)))
             (string->symbol (ast-child 'name n))))]
 [GlobalMemoryRead
  (λ (n) `(global-memory-read
           ,(att-value 'xsmith_render-node (ast-child 'address n))))]
 [GlobalMemoryWrite
  (λ (n) `(global-memory-read
           ,(att-value 'xsmith_render-node (ast-child 'address n))
           ,(att-value 'xsmith_render-node (ast-child 'newval n))))]
 [LiteralMemoryAddress
  (λ (n) (ast-child 'address n))]
 [FakeVarLiteral
  (λ (n) (ast-child 'address n))]
 [FakeVarRef
  (λ (n)
    (define def-node
      (binding-ast-node
       (att-value 'xsmith_binding (ast-child 'VariableReference n))))
    (att-value 'xsmith_render-node (ast-child 'Expression def-node)))]
 )


(define-xsmith-interface-functions
  [arith]
  #:program-node LetStar
  #:type-thunks (list (λ () int))
  #:comment-wrap (λ (lines)
                   (string-join
                    (map (λ (x) (format ";; ~a" x)) lines)
                    "\n"))
  #:format-render (λ (ast)
                    (with-output-to-string
                      (λ ()
                        (pretty-print ast (current-output-port) 1)))))


(module+ main
  (arith-command-line))
