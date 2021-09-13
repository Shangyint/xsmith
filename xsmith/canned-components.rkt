#lang clotho

(provide
 define-basic-spec-component
 add-basic-expressions
 add-basic-statements
 add-loop-over-container
 ;; property to tell child blocks not to increase depth
 block-user?
 ;; Type system stuff
 return-type
 no-return-type
 fresh-maybe-return-type
 void-type
 number-type
 int-type
 float-type
 bool-type
 string-type
 mutable
 immutable
 array-type
 list-type
 dictionary-type

 depth-weight

 current-array-length
 current-num-effect-expressions
 current-arg-length

 random-string/ascii
 random-string/ascii-no-null
 random-string/ascii-no-control)

(require
 xsmith
 racr
 xsmith/racr-convenience
 racket/dict
 racket/list
 racket/port
 (for-syntax
  clotho/racket/base
  syntax/parse
  (for-syntax
   clotho/racket/base
   syntax/parse
   )))


;; Long arrays can sometimes create really long generation times, eg. for arrays of arrays of arrays of functions...
(define current-array-length
  (make-parameter
   (λ () (random 6))
   (λ (v)
     (cond
       [(and (procedure? v) (procedure-arity-includes? v 0)) v]
       [(exact-positive-integer? v) (λ () (random v))]
       [else (raise-argument-error
              'current-array-length
              "(or/c exact-positive-integer? (procedure-arity-includes/c 0))"
              v)]))))

(define current-num-effect-expressions
  (make-parameter
   (λ () (add1 (random 3)))
   (λ (v)
     (cond
       [(and (procedure? v) (procedure-arity-includes? v 0)) v]
       [(exact-positive-integer? v) (λ () (add1 (random v)))]
       [else (raise-argument-error
              'current-num-effect-expressions
              "(or/c exact-positive-integer? (procedure-arity-includes/c 0))"
              v)]))))

(define current-arg-length
  (make-parameter
   (λ () (random 6))
   (λ (v)
     (cond
       [(and (procedure? v) (procedure-arity-includes? v 0)) v]
       [(exact-positive-integer? v) (λ () (random v))]
       [else (raise-argument-error
              'current-arg-length
              "(or/c exact-positive-integer? (procedure-arity-includes/c 0))"
              v)]))))

(define fieldname-options
  '(a b c d e f g))
(define (random-field-name)
  (random-ref fieldname-options))

(define variable-reference-weight 15)

(define (depth-weight #:shallow [shallow-weight 1]
                      #:deep [deep-weight 3])
  (λ (n) (if (att-value 'xsmith_at-max-depth? n)
             deep-weight
             shallow-weight)))


(define (random-string/ascii)
  (random-string-from-char-producing-proc
   (λ () (random-char-in-range (range 0 128)))))
(define (random-string/ascii-no-null)
  (random-string-from-char-producing-proc
   (λ () (random-char-in-range (range 1 128)))))
(define (random-string/ascii-no-control)
  (random-string-from-char-producing-proc
   (λ () (random-char-in-range (range 32 127)))))

(define (random-char/ascii)
  (random-char-in-range (range 0 128)))
(define (random-char/ascii-no-null)
  (random-char-in-range (range 1 128)))
(define (random-char/ascii-no-control)
  (random-char-in-range (range 32 127)))




(define mutable-structural-record-assignment-type-rhs
  (λ (n t)
    (define newtype (fresh-type-variable))
    (hash 'record (mutable
                   (fresh-structural-record-type
                    (hash (ast-child 'fieldname n) newtype)))
          'newvalue newtype)))
(define (mutable-array-assignment-type-rhs index-and-length-type)
  (λ (n t)
    (define inner (fresh-type-variable))
    (hash 'array (mutable (array-type inner))
          'index index-and-length-type
          'newvalue inner)))
(define (mutable-dictionary-assignment-type-rhs index-and-length-type
                                                dictionary-key-type-thunk
                                                dictionary-value-type-thunk)
  (λ (n t)
    (define vt (dictionary-value-type-thunk))
    (define kt (dictionary-key-type-thunk))
    (hash 'dictionary (mutable (dictionary-type kt vt))
          ;; Depending on the type of assignment, it will either have a key
          ;; or an index, but not both.  However, extra keys are just ignored,
          ;; for better or worse, so we can just use both always...
          'index index-and-length-type
          'key kt
          'newvalue vt)))

(define (lambda-fresh-implementation cur-hole make-fresh-node-func)
  (let* ([type (att-value 'xsmith_type cur-hole)]
         [ftype (function-type
                 (product-type #f)
                 (fresh-type-variable))]
         [unification-dumb-return-value (unify! ftype type)]
         [force-exploration-return
          (force-type-exploration-for-node! cur-hole)]
         [parameters
          (map (λ (t)
                 (make-fresh-node-func 'FormalParameter
                                       (hash 'type t)))
               (or (product-type-inner-type-list
                    (function-type-arg-type ftype))
                   (build-list ((current-arg-length)) (λ (x) (fresh-type-variable)))))])
    (unify! (product-type (map (λ (x) (ast-child 'type x))
                               parameters))
            (function-type-arg-type ftype))
    (hash
     'type type
     'parameters parameters)))

(define (make-lambda-type-rhs finalize-return-type-function)
  (λ (n t)
    (define args-type (product-type
                       (map (λ(x)(fresh-type-variable))
                            (ast-children (ast-child 'parameters n)))))
    (define return-type (fresh-type-variable))
    (unify! (function-type args-type return-type)
            t)
    (define args-list (product-type-inner-type-list args-type))
    (hash-set
     (for/hash ([c (ast-children (ast-child 'parameters n))]
                [at args-list])
       (values c at))
     (ast-child 'body n)
     (finalize-return-type-function return-type))))


(begin-for-syntax
  (define-syntax (use? stx)
    (syntax-parse stx
      [(_ name (~optional default:boolean))
       #'(let ([att (attribute name)])
           (if att
               (syntax-parse att
                 [#t #t]
                 [#f #f])
               (~? default #f)))])))

(define-syntax (define-basic-spec-component stx)
  (syntax-parse stx
    [(_ component)
     #`(begin
         (define-spec-component component)
         (add-to-grammar
          component
          [Expression #f ()
                      #:prop may-be-generated #f
                      #:prop type-info
                      ;; TODO - this error message is dumb, because it doesn't say WHICH node is falling back like this.  It should be able to, but I would need to be able to access the current choice object, which is not available here.
                      [(error 'type-info "Trying to type check as an expression without a specialized implementation.  You probably forgot to add a type-info property for a subtype of Expression.")
                       no-child-types]]
          [Statement #f ()
                     #:prop may-be-generated #f
                     #:prop type-info
                     ;; TODO - this error message is dumb, because it doesn't say WHICH node is falling back like this.  It should be able to, but I would need to be able to access the current choice object, which is not available here.
                     [(error 'type-info "Trying to type check as a statement without a specialized implementation.  You probably forgot to add a type-info property for a subtype of Statement.")
                      no-child-types]]
          [Definition #f ([type]
                          [name = (fresh-var-name "b_")]
                          Expression)
            #:prop binder-info ()
            #:prop type-info [(fresh-type-variable) (λ (n t) (hash 'Expression t))]]
          [DefinitionNoRhs #f ([type]
                               [name])
            #:prop binder-info (#:lift-target? #f)
            #:prop type-info [(fresh-type-variable) no-child-types]]
          [FormalParameter #f (type [name = (fresh-var-name "arg_")])
                           #:prop binder-info (#:binder-style parameter)
                           #:prop type-info [(fresh-type-variable) no-child-types]]
          ))]))

(define-syntax (add-basic-expressions stx)
  (syntax-parse stx
    [(_ component
        (~or
         (~optional (~seq #:ProgramWithSequence use-program-with-sequence:boolean))
         (~optional (~seq #:VariableReference use-variable-reference:boolean))
         (~optional (~seq #:VoidExpression use-void-expression:boolean))
         (~optional (~seq #:AssignmentExpression use-assignment-expression:boolean))
         (~optional (~seq #:IfExpression use-if-expression:boolean))
         (~optional (~seq #:LambdaWithExpression use-LWE:boolean))
         (~optional (~seq #:LambdaWithBlock use-LWB:boolean))
         (~optional (~seq #:LambdaSingleWithExpression use-LSWE:boolean))
         (~optional (~seq #:ProcedureApplication use-procedure-application:boolean))
         (~optional (~seq #:ProcedureApplicationSingle
                          use-procedure-application-single:boolean))
         (~optional (~seq #:LetSequential use-let-sequential:boolean))
         (~optional (~seq #:ExpressionSequence use-expression-sequence:boolean))
         (~optional (~seq #:Numbers use-numbers:boolean))
         (~optional (~seq #:Booleans use-booleans:boolean))
         (~optional (~seq #:Strings use-strings:boolean))
         (~optional (~seq #:MutableArray use-mutable-array:boolean))
         (~optional (~seq #:MutableArraySafeAssignmentExpression
                          use-mutable-array-safe-assignment-expression:boolean))
         (~optional (~seq #:ImmutableArray use-immutable-array:boolean))
         (~optional (~seq #:ImmutableList use-immutable-list:boolean))
         (~optional (~seq #:MutableDictionary use-mutable-dictionary:boolean))
         ;; Dictionaries may be accessed/updated by key or by index.
         ;; When using index, you must ensure that the keys are in a deterministic
         ;; order, perhaps by sorting them in a wrapper function.
         ;; In either case, the dictionary may be empty.
         (~optional (~seq #:MutableDictionarySafeReferenceByIndex
                          use-mutable-dictionary-safe-reference-by-index:boolean))
         (~optional (~seq #:MutableDictionarySafeReferenceByKey
                          use-mutable-dictionary-safe-reference-by-key:boolean))
         (~optional (~seq #:MutableDictionarySafeAssignmentByIndexExpression
                          use-mutable-dictionary-safe-assignment-by-index-expression:boolean))
         (~optional (~seq #:MutableDictionarySafeAssignmentByKeyExpression
                          use-mutable-dictionary-safe-assignment-by-key-expression:boolean))
         (~optional (~seq #:MutableStructuralRecord
                          use-mutable-structural-record:boolean))
         (~optional (~seq #:MutableStructuralRecordAssignmentExpression
                          use-mutable-structural-record-assignment-expression:boolean))
         (~optional (~seq #:ImmutableStructuralRecord
                          use-immutable-structural-record:boolean))
         (~optional (~seq #:bool-type bool-type-e:expr)
                    #:defaults ([bool-type-e #'bool-type]))
         (~optional (~seq #:number-type number-type-e:expr)
                    #:defaults ([number-type-e #'number-type]))
         (~optional (~seq #:int-type int-type-e:expr)
                    #:defaults ([int-type-e #'int-type]))
         (~optional (~seq #:string-literal-value string-literal-value-e:expr)
                    #:defaults ([string-literal-value-e
                                 #'(random-string/ascii-no-control)]))
         (~optional (~seq #:int-literal-value int-literal-value-e:expr)
                    #:defaults ([int-literal-value-e
                                 #'(random-int)]))
         (~optional (~seq #:index-and-length-type index-and-length-type-e:expr))
         (~optional (~seq #:dictionary-key-type dictionary-key-type-e:expr))
         (~optional (~seq #:dictionary-value-type dictionary-value-type-e:expr))
         )
        ...
        )
     #`(begin
         (define bool bool-type-e)
         (define number number-type-e)
         (define int int-type-e)
         (define index-and-length-type (~? index-and-length-type-e int))
         (define dictionary-key-type
           (~? (λ () dictionary-key-type-e)
               (λ () (fresh-type-variable bool int))))
         (define dictionary-value-type
           (~? (λ () dictionary-value-type-e)
               (λ () (fresh-type-variable bool int))))
         ;;; Optional components
         #,@(if (use? use-variable-reference)
                #'((add-to-grammar
                    component
                    [VariableReference Expression (name)
                                       #:prop reference-info (read)
                                       #:prop type-info
                                       [(fresh-type-variable) no-child-types]
                                       #:prop choice-weight
                                       (λ (n) (if (ast-subtype? (parent-node n)
                                                                'Definition)
                                                  1
                                                  variable-reference-weight))]))
                #'())
         #,@(if (use? use-procedure-application)
                #'((add-to-grammar
                    component
                    [ProcedureApplication
                     Expression
                     ([procedure : Expression]
                      [arguments : Expression * = (create-ast-bud)])
                     #:prop reducible-list-fields #f
                     #:prop edit
                     (λ (n)
                       (and
                        (ast-bud-node? (ast-child 'arguments n))
                        (att-value 'xsmith_no-holes-in-subtree?
                                   (ast-child 'procedure n))
                        (λ () (rewrite-subtree
                               (ast-child 'arguments n)
                               (let* ([ft (att-value 'xsmith_type
                                                     (ast-child 'procedure n))]
                                      [ft-access (function-type
                                                  (product-type #f)
                                                  (fresh-type-variable))]
                                      [_ (begin (unify! ft ft-access)
                                                (force-type-exploration-for-node!
                                                 (ast-child 'procedure n)))]
                                      [arg-types (product-type-inner-type-list
                                                  (function-type-arg-type ft-access))])
                                 (create-ast-list
                                  (if (list? arg-types)
                                      (map (λ (x) (make-hole 'Expression)) arg-types)
                                      (build-list
                                       ((current-arg-length))
                                       (λ (x) (make-hole 'Expression))))))))))
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t)
                        (define proc (ast-child 'procedure n))
                        (define args-node (ast-child 'arguments n))
                        (define args-done? (not (ast-bud-node? args-node)))
                        (define args (and args-done? (ast-children args-node)))
                        (define args-type (if args-done?
                                              (product-type
                                               (map (λ(x)(fresh-type-variable))
                                                    args))
                                              (product-type #f)))
                        (hash-set
                         (if args-done?
                             (for/hash ([arg args]
                                        [arg-type (product-type-inner-type-list
                                                   args-type)])
                               (values arg arg-type))
                             (hash))
                         'procedure
                         (function-type args-type t)))]]))
                #'())
         #,@(if (use? use-procedure-application-single)
                #'((add-to-grammar
                    component
                    [ProcedureApplicationSingle
                     Expression
                     ([procedure : Expression]
                      [argument : Expression])
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t)
                        (define proc (ast-child 'procedure n))
                        (define args-node (ast-child 'argument n))
                        (define arg-type (fresh-type-variable))
                        (define procedure-type (function-type arg-type t))
                        (hash 'procedure procedure-type
                              'argument arg-type))]]))
                #'())

         #,@(if (use? use-numbers)
                #'((add-to-grammar
                    component
                    [NumberLiteral Expression (v)
                                   #:prop may-be-generated #f
                                   #:prop choice-weight (depth-weight)]
                    [IntLiteral NumberLiteral ()
                                #:prop fresh (hash 'v int-literal-value-e)]
                    [Plus Expression ([l : Expression] [r : Expression])]
                    [Minus Expression ([l : Expression] [r : Expression])]
                    [Times Expression ([l : Expression] [r : Expression])]
                    [SafeDivide Expression ([l : Expression] [r : Expression])])
                   (add-property
                    component type-info
                    [IntLiteral [int no-child-types]]
                    [Plus [number numeric-bin-op-subtype]]
                    [Minus [number numeric-bin-op-subtype]]
                    [Times [number numeric-bin-op-subtype]]
                    [SafeDivide [number numeric-bin-op-subtype]]))
                #'())
         #,@(if (and (use? use-numbers)
                     (use? use-booleans))
                #'((add-to-grammar
                    component
                    [LessThan Expression ([l : Expression] [r : Expression])]
                    [GreaterThan Expression ([l : Expression] [r : Expression])])
                   (add-property
                    component type-info
                    [LessThan [bool (numeric-bin-op/no-relation-to-return number)]]
                    [GreaterThan [bool (numeric-bin-op/no-relation-to-return number)]]))
                #'())

         #,@(if (use? use-void-expression)
                #'((add-to-grammar
                    component
                    [VoidExpression Expression ()
                                    #:prop type-info [void-type no-child-types]]))
                #'())

         #,@(if (use? use-program-with-sequence)
                #'((add-to-grammar
                    component
                    [ProgramWithSequence
                     #f ([definitions : Definition *]
                         [ExpressionSequence])
                     #:prop strict-child-order? #t
                     #:prop reducible-list-fields #t
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t)
                        (hash 'definitions (λ (c) (fresh-type-variable))
                              'ExpressionSequence t))]]))
                #'())

         #,@(if (use? use-assignment-expression)
                #'((add-to-grammar
                    component
                    [AssignmentExpression
                     Expression (name [newvalue : Expression])
                     #:prop reference-info (write #:unifies newvalue)
                     #:prop type-info
                     [void-type
                      (λ (n t) (hash 'newvalue (fresh-type-variable)))]]))
                #'())

         #,@(if (use? use-if-expression)
                #'((add-to-grammar
                    component
                    [IfExpression
                     Expression ([test : Expression]
                                 [then : Expression]
                                 [else : Expression])
                     #:prop strict-child-order? #t
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t) (hash 'test bool
                                     'then t
                                     'else t))]]))
                #'())

         #,@(if (use? use-booleans)
                #'((add-to-grammar
                    component
                    [BoolLiteral Expression ([v = (even? (random 2))])
                                 #:prop choice-weight (depth-weight)]
                    [Not Expression ([Expression])]
                    [And Expression ([l : Expression] [r : Expression])]
                    [Or Expression ([l : Expression] [r : Expression])])
                   (add-property
                    component
                    type-info
                    [BoolLiteral [bool no-child-types]]
                    [Not [bool (λ (n t) (hash 'Expression bool))]]
                    [And [bool (λ (n t) (hash 'l bool 'r bool))]]
                    [Or [bool (λ (n t) (hash 'l bool 'r bool))]]))
                #'())


         #,@(if (use? use-strings)
                #'((add-to-grammar
                    component
                    [StringLiteral
                     Expression
                     ([v = string-literal-value-e])
                     #:prop choice-weight (depth-weight)]
                    [StringAppend Expression ([l : Expression] [r : Expression])]
                    [StringLength Expression (Expression)])
                   (add-property
                    component
                    type-info
                    [StringLiteral [string-type no-child-types]]
                    [StringAppend [string-type (λ (n t) (hash 'l string-type 'r string-type))]]
                    [StringLength [index-and-length-type
                                   (λ (n t) (hash 'Expression string-type))]]))
                #'())


         #,@(if (use? use-LSWE)
                #'((add-to-grammar
                    component
                    [LambdaSingleWithExpression
                     Expression ([parameter : FormalParameter]
                                 [body : Expression])
                     #:prop wont-over-deepen #t
                     #:prop choice-weight 1
                     #:prop fresh
                     (let* ([type (att-value 'xsmith_type (current-hole))]
                            [arg-type (fresh-type-variable)]
                            [ftype (function-type
                                    arg-type
                                    (fresh-type-variable))]
                            [unification-dumb-return-value (unify! ftype type)]
                            [force-exploration-return
                             (force-type-exploration-for-node! (current-hole))]
                            [parameter
                             (make-fresh-node 'FormalParameter
                                              (hash 'type arg-type))])
                       (hash
                        'type type
                        'parameter parameter))
                     #:prop type-info
                     [(function-type (fresh-type-variable) (fresh-type-variable))
                      (λ (n t)
                        (define arg-type (fresh-type-variable))
                        (define return-type (fresh-type-variable))
                        (unify! t (function-type arg-type return-type))
                        (hash 'parameter arg-type
                              'body return-type))]]))
                #'())
         #,@(if (use? use-LWE)
                #'((add-to-grammar
                    component
                    [LambdaWithExpression
                     Expression ([parameters : FormalParameter * = ((current-arg-length))]
                                 [body : Expression])
                     #:prop reducible-list-fields #f
                     #:prop wont-over-deepen #t
                     #:prop choice-weight 1
                     #:prop fresh
                     (lambda-fresh-implementation (current-hole) make-fresh-node)
                     #:prop type-info
                     [(function-type (product-type #f) (fresh-type-variable))
                      (make-lambda-type-rhs (λ (rt) rt))]]))
                #'())
         #,@(if (use? use-LWB)
                #'((add-to-grammar
                    component
                    [LambdaWithBlock
                     Expression ([parameters : FormalParameter * = ((current-arg-length))]
                                 [body : Block])
                     #:prop block-user? #t
                     #:prop reducible-list-fields #f
                     #:prop wont-over-deepen #t
                     #:prop choice-weight 1
                     #:prop fresh
                     (lambda-fresh-implementation (current-hole) make-fresh-node)
                     #:prop type-info
                     [(function-type (product-type #f) (fresh-type-variable))
                      (make-lambda-type-rhs (λ (rt) (return-type rt)))]]))
                #'())

         #,@(if (use? use-let-sequential)
                #'((add-to-grammar
                    component
                    [LetSequential
                     Expression ([definitions : Definition *]
                                 [body : Expression])
                     #:prop strict-child-order? #t
                     #:prop reducible-list-fields #t
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t)
                        (hash 'definitions (λ (c) (fresh-type-variable))
                              'body t))]]))
                #'())

         #,@(if (or (use? use-expression-sequence) (use? use-program-with-sequence))
                #'((add-to-grammar
                    component
                    [ExpressionSequence
                     Expression
                     ([effectexpressions : Expression *
                                         = ((current-num-effect-expressions))]
                      [finalexpression : Expression])
                     #:prop strict-child-order? #t
                     #:prop reducible-list-fields #t
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t)
                        ;; TODO - the effect type could be any, but using void-type will force it to use a side-effectful expression
                        (hash 'effectexpressions (λ (c) void-type)
                              'finalexpression t))]]))
                #'())

         #,@(if (use? use-immutable-array)
                #'((add-to-grammar
                    component
                    [ImmutableArrayLiteral
                     Expression
                     ([expressions : Expression * = ((current-array-length))])
                     #:prop wont-over-deepen #t
                     #:prop reducible-list-fields #t
                     #:prop choice-weight (depth-weight)]
                    [ImmutableArraySafeReference
                     Expression ([array : Expression]
                                 [index : Expression]
                                 [fallback : Expression])]
                    [ImmutableArraySafeSet
                     Expression ([array : Expression]
                                 [index : Expression]
                                 [newvalue : Expression])])
                   (add-property
                    component
                    type-info
                    [ImmutableArrayLiteral [(immutable (fresh-array-type))
                                            (λ (n t)
                                              (define et (fresh-type-variable))
                                              (define at (immutable (array-type et)))
                                              (subtype-unify! at t)
                                              (hash 'expressions et))]]
                    [ImmutableArraySafeReference
                     [(fresh-type-variable)
                      (λ (n t) (hash 'index index-and-length-type
                                     'array (immutable
                                             (array-type t))
                                     'fallback t))]]
                    [ImmutableArraySafeSet
                     [(immutable (fresh-array-type))
                      (λ (n t)
                        (define inner-t (fresh-type-variable))
                        (unify! (immutable
                                 (array-type inner-t))
                                t)
                        (hash 'index index-and-length-type
                              'array t
                              'newvalue inner-t))]]))
                #'())

         #,@(if (use? use-immutable-list)
                #'((add-to-grammar
                    component
                    [ImmutableListLiteral
                     Expression
                     ([expressions : Expression * = ((current-array-length))])
                     #:prop wont-over-deepen #t
                     #:prop reducible-list-fields #t
                     #:prop choice-weight (depth-weight)
                     #:prop type-info
                     [(immutable (list-type (fresh-type-variable)))
                      (λ (n t)
                        (define inner-t (fresh-type-variable))
                        (unify! t (immutable (list-type inner-t)))
                        (hash 'expressions inner-t))]]
                    [ImmutableListSafeCar Expression ([list : Expression]
                                                      [fallback : Expression])
                                          #:prop type-info
                                          [(fresh-type-variable)
                                           (λ (n t)
                                             (hash 'list (immutable (list-type t))
                                                   'fallback t))]]
                    [ImmutableListSafeCdr Expression ([list : Expression]
                                                      [fallback : Expression])
                                          #:prop type-info
                                          [(immutable
                                            (list-type (fresh-type-variable)))
                                           (λ (n t)
                                             (hash 'list t
                                                   'fallback t))]]
                    [ImmutableListCons Expression ([list : Expression]
                                                   [newvalue : Expression])
                                       #:prop type-info
                                       [(immutable (list-type (fresh-type-variable)))
                                        (λ (n t)
                                          (define inner-t (fresh-type-variable))
                                          (unify! (immutable (list-type inner-t)) t)
                                          (hash 'list t
                                                'newvalue inner-t))]]))
                #'())

         #,@(if (use? use-mutable-array)
                #'((add-to-grammar
                    component
                    [MutableArrayLiteral
                     Expression
                     ([expressions : Expression * = ((current-array-length))])
                     #:prop wont-over-deepen #t
                     #:prop reducible-list-fields #t
                     #:prop choice-weight (depth-weight)
                     #:prop type-info
                     [(mutable (fresh-array-type))
                      (λ (n t)
                        (define et (fresh-type-variable))
                        (define at (mutable (array-type et)))
                        (subtype-unify! at t)
                        (hash 'expressions et))]]
                    [MutableArraySafeReference
                     Expression ([array : Expression]
                                 [index : Expression]
                                 [fallback : Expression])
                     #:prop mutable-container-access (read 'MutableArray)
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t) (hash 'index index-and-length-type
                                     'array (mutable
                                             (array-type t))
                                     'fallback t))]]))
                #'())
         #,@(if (use? use-mutable-array-safe-assignment-expression)
                #'((add-to-grammar
                    component
                    [MutableArraySafeAssignmentExpression
                     Expression
                     ([array : VariableReference]
                      [index : Expression]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableArray)
                     #:prop required-child-reference #t
                     #:prop type-info
                     [void-type (mutable-array-assignment-type-rhs
                                 index-and-length-type)]]))
                #'())


         #,@(if (use? use-mutable-dictionary)
                #'((add-to-grammar
                    component
                    [MutableDictionarySafeLiteral
                     Expression
                     ([keys : Expression *]
                      [vals : Expression *])
                     #:prop wont-over-deepen #t
                     ;; These ideally should be reducible, but the keys and values must be reduced together.
                     #:prop reducible-list-fields #f
                     #:prop choice-weight (depth-weight)
                     #:prop fresh
                     (let ([elem-count ((current-array-length))])
                       (hash 'keys elem-count
                             'vals elem-count))
                     #:prop type-info
                     [(mutable (dictionary-type (dictionary-key-type) (dictionary-value-type)))
                      (λ (n t)
                        (define kt (dictionary-key-type))
                        (define vt (dictionary-value-type))
                        (define dt (mutable (dictionary-type kt vt)))
                        (subtype-unify! dt t)
                        (hash 'keys kt
                              'vals vt))]]))
                #'())
         #,@(if (use? use-mutable-dictionary-safe-reference-by-key)
                #'((add-to-grammar
                    component
                    [MutableDictionarySafeReferenceByKey
                     Expression
                     ([dictionary : Expression]
                      [key : Expression]
                      [fallback : Expression])
                     #:prop choice-weight 10
                     #:prop mutable-container-access (read 'MutableDictionary)
                     #:prop type-info
                     [(dictionary-value-type)
                      (λ (n t)
                        (define key-type (dictionary-key-type))
                        (hash 'dictionary (mutable (dictionary-type key-type t))
                              'key key-type
                              'fallback t))]]))
                #'())
         #,@(if (use? use-mutable-dictionary-safe-reference-by-index)
                #'((add-to-grammar
                    component
                    [MutableDictionarySafeReferenceByIndex
                     Expression
                     ([dictionary : Expression]
                      [index : Expression]
                      [fallback : Expression])
                     #:prop choice-weight 10
                     #:prop mutable-container-access (read 'MutableDictionary)
                     #:prop type-info
                     [(dictionary-value-type)
                      (λ (n t)
                        (define key-type (dictionary-key-type))
                        (hash 'dictionary (mutable (dictionary-type key-type t))
                              'index index-and-length-type
                              'fallback t))]]))
                #'())
         #,@(if (use? use-mutable-dictionary-safe-assignment-by-key-expression)
                #'((add-to-grammar
                    component
                    [MutableDictionarySafeAssignmentByKeyExpression
                     Expression
                     ([dictionary : VariableReference]
                      [key : Expression]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableDictionary)
                     #:prop required-child-reference #t
                     #:prop type-info
                     [void-type
                      (mutable-dictionary-assignment-type-rhs
                       index-and-length-type
                       dictionary-key-type
                       dictionary-value-type)]]))
                #'())
         #,@(if (use? use-mutable-dictionary-safe-assignment-by-index-expression)
                #'((add-to-grammar
                    component
                    [MutableDictionarySafeAssignmentByIndexExpression
                     Expression
                     ([dictionary : VariableReference]
                      [index : Expression]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableDictionary)
                     #:prop required-child-reference #t
                     #:prop type-info
                     [void-type
                      (mutable-dictionary-assignment-type-rhs
                       index-and-length-type
                       dictionary-key-type
                       dictionary-value-type)]]))
                #'())

         #,@(if (use? use-immutable-structural-record)
                #'((add-to-grammar
                    component
                    [ImmutableStructuralRecordLiteral
                     Expression
                     (fieldnames [expressions : Expression *])
                     #:prop wont-over-deepen #t
                     #:prop reducible-list-fields #f
                     #:prop choice-weight (depth-weight)
                     #:prop fresh
                     (let* ([t (begin (force-type-exploration-for-node!
                                       (current-hole))
                                      (att-value 'xsmith_type (current-hole)))]
                            [srt (fresh-structural-record-type)]
                            [msrt (immutable srt)]
                            [_side-effect (unify! t msrt)]
                            [fd (structural-record-type-known-field-dict srt)]
                            [necessary-fields (dict-keys fd)]
                            ;; Let's inject extra fields.
                            [new-fields (map (λ(_) (random-field-name))
                                             (make-list (add1 (random 2)) #f))]
                            [all-fields (remove-duplicates
                                         (append necessary-fields new-fields))])
                       (hash 'fieldnames all-fields
                             'expressions (length all-fields)))
                     #:prop type-info
                     [(immutable (fresh-structural-record-type (hash)))
                      (λ (n t)
                        (define names (ast-child 'fieldnames n))
                        (define name-type-dict (for/hash ([fn names])
                                                 (values fn (fresh-type-variable))))
                        (define fsrt (fresh-structural-record-type name-type-dict))
                        (define mfsrt (immutable fsrt))
                        (subtype-unify! mfsrt t)
                        (define td (structural-record-type-known-field-dict fsrt))
                        (for/hash ([c (ast-children (ast-child 'expressions n))]
                                   [f (ast-child 'fieldnames n)])
                          (values c
                                  (dict-ref td f (λ () (fresh-type-variable))))))]]
                    [ImmutableStructuralRecordReference
                     Expression
                     ([fieldname = (random-field-name)]
                      [record : Expression])
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t) (hash 'record
                                     (immutable
                                      (fresh-structural-record-type
                                       (hash (ast-child 'fieldname n) t)))))]]
                    [ImmutableStructuralRecordSet
                     Expression
                     ([fieldname = (random-field-name)]
                      [record : Expression]
                      [newvalue : Expression])
                     #:prop type-info
                     [(immutable (fresh-structural-record-type (hash)))
                      (λ (n t)
                        (define inner-t (fresh-type-variable))
                        (define record-t
                          (immutable (fresh-structural-record-type
                                      (hash (ast-child 'fieldname n)
                                            inner-t))))
                        (subtype-unify! record-t t)
                        (hash 'record record-t
                              'newvalue inner-t))]]))
                #'())

         #,@(if (use? use-mutable-structural-record)
                #'((add-to-grammar
                    component
                    [MutableStructuralRecordLiteral
                     Expression
                     (fieldnames [expressions : Expression *])
                     #:prop reducible-list-fields #f
                     #:prop wont-over-deepen #t
                     #:prop choice-weight (depth-weight)
                     #:prop fresh
                     (let* ([t (begin (force-type-exploration-for-node!
                                       (current-hole))
                                      (att-value 'xsmith_type (current-hole)))]
                            [srt (fresh-structural-record-type)]
                            [msrt (mutable srt)]
                            [_side-effect (unify! t msrt)]
                            [fd (structural-record-type-known-field-dict srt)]
                            [necessary-fields (dict-keys fd)]
                            ;; Let's inject extra fields.
                            ;; Actually not, mutable structural records are invariant
                            ;; because they are mutable.
                            )
                       (hash 'fieldnames necessary-fields
                             'expressions (length necessary-fields)))
                     #:prop type-info
                     [(mutable (fresh-structural-record-type (hash)))
                      (λ (n t)
                        (define known-fields
                          (for/hash ([k (ast-child 'fieldnames n)])
                            (values k (fresh-type-variable))))
                        (define fsrt (fresh-structural-record-type known-fields
                                                                   #:finalized? #t))
                        (define mfsrt (mutable fsrt))
                        (unify! mfsrt t)
                        (define td (structural-record-type-known-field-dict fsrt))
                        (for/hash ([c (ast-children (ast-child 'expressions n))]
                                   [f (ast-child 'fieldnames n)])
                          (values c
                                  (dict-ref td f))))]]
                    [MutableStructuralRecordReference
                     Expression
                     ([fieldname = (random-field-name)]
                      [record : Expression])
                     #:prop mutable-container-access (read 'MutableStructuralRecord)
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t) (hash 'record
                                     (mutable
                                      (fresh-structural-record-type
                                       (hash (ast-child 'fieldname n) t)))))]]))
                #'())
         #,@(if (use? use-mutable-structural-record-assignment-expression)
                #'((add-to-grammar
                    component
                    [MutableStructuralRecordAssignmentExpression
                     Expression
                     ([fieldname = (random-field-name)]
                      [record : VariableReference]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableStructuralRecord)
                     #:prop required-child-reference #t
                     #:prop type-info
                     [void-type mutable-structural-record-assignment-type-rhs]]))
                #'())


         ;; end begin
         )]))


(define-simple-property
  block-user?
  attribute
  #:rule-name _xsmith_block-user?
  #:default #f
  #:transformer (syntax-parser [#t #'(λ (n) #t)] [#f #'(λ (n) #f)]))


(define-syntax (add-basic-statements stx)
  (syntax-parse stx
    [(_ component
        (~or
         (~optional (~seq #:NamedFunctionDefinition use-named-function-definition:boolean))
         (~optional (~seq #:ProgramWithBlock use-program-with-block:boolean))
         (~optional (~seq #:IfElseStatement use-if-else-statement:boolean))
         (~optional (~seq #:Block use-block-statement:boolean))
         (~optional (~seq #:ReturnStatement use-return-statement:boolean))
         (~optional (~seq #:AssignmentStatement use-assignment-statement:boolean))
         (~optional (~seq #:NullStatement use-null-statement:boolean))
         (~optional (~seq #:ExpressionStatement use-expression-statement:boolean))
         (~optional (~seq #:MutableArraySafeAssignmentStatement
                          use-mutable-array-safe-assignment-statement:boolean))
         (~optional (~seq #:MutableDictionarySafeAssignmentByKeyStatement
                          use-mutable-dictionary-safe-assignment-by-key-statement:boolean))
         (~optional (~seq #:MutableDictionarySafeAssignmentByIndexStatement
                          use-mutable-dictionary-safe-assignment-by-index-statement:boolean))
         (~optional (~seq #:MutableStructuralRecordAssignmentStatement
                          use-mutable-structural-record-assignment-statement:boolean))
         (~optional (~seq #:bool-type bool-type-e:expr)
                    #:defaults ([bool-type-e #'bool-type]))
         (~optional (~seq #:int-type int-type-e:expr)
                    #:defaults ([int-type-e #'int-type]))
         (~optional (~seq #:index-and-length-type index-and-length-type-e:expr))
         (~optional (~seq #:dictionary-key-type dictionary-key-type-e:expr))
         (~optional (~seq #:dictionary-value-type dictionary-value-type-e:expr))
         )
        ...
        )
     #`(begin
         (define bool bool-type-e)
         (define int int-type-e)
         (define index-and-length-type (~? index-and-length-type-e int))
         (define dictionary-key-type
           (~? (λ () dictionary-key-type-e)
               (λ () (fresh-type-variable bool int))))
         (define dictionary-value-type
           (~? (λ () dictionary-value-type-e)
               (λ () (fresh-type-variable bool int))))

         #,@(if (use? use-named-function-definition)
                #'((add-to-grammar
                    component
                    [NamedFunctionDefinition
                     Expression ([parameters : FormalParameter * = ((current-arg-length))]
                                 [body : Block])
                     #:prop block-user? #t
                     #:prop reducible-list-fields #f
                     #:prop wont-over-deepen #t
                     #:prop choice-weight 1
                     #:prop fresh
                     (lambda-fresh-implementation (current-hole) make-fresh-node)
                     #:prop type-info
                     [(function-type (product-type #f) (fresh-type-variable))
                      (make-lambda-type-rhs (λ (rt) (return-type rt)))]])
                   (add-choice-method
                    component
                    nfd-must-be-under-definition
                    [NamedFunctionDefinition
                     (λ () (let ([parent-node (ast-parent (current-hole))])
                             (and (not (ast-list-node? parent-node))
                                  (equal? (ast-node-type parent-node)
                                          'Definition))))])
                   (add-property
                    component
                    choice-filters-to-apply
                    [NamedFunctionDefinition (nfd-must-be-under-definition)]))
                #'())

         #,@(if (use? use-return-statement)
                #'((add-to-grammar
                    component
                    [ReturnStatement Statement (Expression)
                                     #:prop wont-over-deepen #t
                                     #:prop type-info
                                     [(return-type (fresh-type-variable))
                                      (λ (n t)
                                        (define inner (fresh-type-variable))
                                        (unify! (return-type inner) t)
                                        (hash 'Expression inner))]]))
                #'())
         #,@(if (use? use-if-else-statement)
                #'((add-to-grammar
                    component
                    [IfElseStatement Statement
                                     ([test : Expression]
                                      [then : Block]
                                      [else : Block])
                                     #:prop block-user? #t
                                     #:prop strict-child-order? #t
                                     #:prop type-info [(fresh-maybe-return-type)
                                                       (λ (n t) (hash 'test bool
                                                                      'then t
                                                                      'else t))]]))
                #'())
         #,@(if (or (use? use-block-statement)
                    (use? use-if-else-statement)
                    (use? use-program-with-block))
                #'((add-to-grammar
                    component
                    [Block Statement ([definitions : Definition *]
                                      [statements : Statement * = (add1 (random 5))])
                           #:prop strict-child-order? #t
                           ;; TODO - statements should be reducible.  I need to rearrange it so it has a final statement as a separate field.
                           #:prop reducible-list-fields (definitions)
                           #:prop depth-increase
                           ;; Many nodes have blocks as children to
                           ;; implicitly have multiple statements and
                           ;; allow definitions. Let's assume blocks
                           ;; are always used that way unless they are
                           ;; themselves under blocks.
                           (λ (n)
                             (if (and (parent-node n)
                                      (att-value '_xsmith_block-user?
                                                 (parent-node n)))
                                 0
                                 1))
                           #:prop type-info
                           [(fresh-maybe-return-type)
                            (λ (n t)
                              (define statements (ast-children
                                                  (ast-child 'statements n)))
                              (define last-statement (car (reverse statements)))
                              (define statement-dict
                                (for/hash ([s (ast-children
                                               (ast-child 'statements n))])
                                  (values s
                                          (if (eq? s last-statement)
                                              t
                                              no-return-type))))
                              (for/fold ([dict statement-dict])
                                        ([d (ast-children
                                             (ast-child 'definitions n))])
                                (dict-set dict d (fresh-type-variable))))]]))
                #'())
         #,@(if (use? use-program-with-block)
                #'((add-to-grammar
                    component
                    [ProgramWithBlock
                     #f ([definitions : Definition *]
                         [Block])
                     #:prop block-user? #t
                     #:prop reducible-list-fields #t
                     #:prop strict-child-order? #t
                     #:prop type-info
                     [(fresh-type-variable)
                      (λ (n t)
                        (hash 'definitions (λ (c) (fresh-type-variable))
                              'Block (λ (c) no-return-type)))]]))
                #'())

         #,@(if (use? use-expression-statement)
                #'((add-to-grammar
                    component
                    [ExpressionStatement
                     Statement (Expression)
                     #:prop wont-over-deepen #t
                     #:prop type-info
                     [no-return-type
                      (λ (n t) (hash 'Expression (fresh-type-variable)))]]))
                #'())

         #,@(if (use? use-assignment-statement)
                #'((add-to-grammar
                    component
                    [AssignmentStatement
                     Statement (name Expression)
                     #:prop reference-info (write #:unifies Expression)
                     #:prop wont-over-deepen #t
                     #:prop type-info
                     [no-return-type
                      (λ (n t) (hash 'Expression (fresh-type-variable)))]]))
                #'())
         #,@(if (use? use-null-statement)
                #'((add-to-grammar
                    component
                    [NullStatement
                     Statement ()
                     #:prop choice-weight 1
                     #:prop type-info [no-return-type no-child-types]]))
                #'())
         #,@(if (use? use-mutable-array-safe-assignment-statement)
                #'((add-to-grammar
                    component
                    [MutableArraySafeAssignmentStatement
                     Statement
                     ([array : VariableReference]
                      [index : Expression]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableArray)
                     #:prop type-info
                     [no-return-type
                      (λ (n t)
                        (define inner (fresh-type-variable))
                        (hash 'array (mutable (array-type inner))
                              'index index-and-length-type
                              'newvalue inner))]]))
                #'())
         #,@(if (use? use-mutable-dictionary-safe-assignment-by-key-statement)
                #'((add-to-grammar
                    component
                    [MutableDictionarySafeAssignmentByKeyStatement
                     Statement
                     ([dictionary : VariableReference]
                      [key : Expression]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableDictionary)
                     #:prop type-info
                     [no-return-type
                      (mutable-dictionary-assignment-type-rhs
                       index-and-length-type
                       dictionary-key-type
                       dictionary-value-type)]]))
                #'())
         #,@(if (use? use-mutable-dictionary-safe-assignment-by-index-statement)
                #'((add-to-grammar
                    component
                    [MutableDictionarySafeAssignmentByIndexStatement
                     Statement
                     ([dictionary : VariableReference]
                      [index : Expression]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableDictionary)
                     #:prop type-info
                     [no-return-type
                      (mutable-dictionary-assignment-type-rhs
                       index-and-length-type
                       dictionary-key-type
                       dictionary-value-type)]]))
                #'())
         #,@(if (use? use-mutable-structural-record-assignment-statement)
                #'((add-to-grammar
                    component
                    [MutableStructuralRecordAssignmentStatement
                     Statement
                     ([fieldname = (random-field-name)]
                      [record : VariableReference]
                      [newvalue : Expression])
                     #:prop mutable-container-access (write 'MutableStructuralRecord)
                     #:prop type-info
                     [no-return-type
                      mutable-structural-record-assignment-type-rhs]]))
                #'())


         )]))


(define-syntax (add-loop-over-container stx)
  (syntax-parse stx
    [(_ component
        (~or
         (~optional (~seq #:name loop-node-name))
         (~optional (~seq #:loop-ast-type loop-ast-type)
                    #:defaults ([loop-ast-type #'Expression]))
         (~optional (~seq #:body-ast-type loop-body-ast-type)
                    #:defaults ([loop-body-ast-type #'Expression]))
         (~optional (~seq #:mutable-container-access
                          mutable-container-access-prop-val))
         (~optional (~seq #:bind-whole-collection? bind-whole-collection?)
                    #:defaults ([bind-whole-collection? #'#f]))
         (~optional (~seq #:collection-type-constructor
                          collection-type-constructor-stx)
                    #:defaults ([collection-type-constructor-stx
                                 #'(λ (inner-type)
                                     (immutable (array-type inner-type)))]))
         (~optional (~seq #:loop-type-constructor loop-type-constructor-stx))
         (~optional (~seq #:body-type-constructor body-type-constructor-stx))
         (~optional (~seq #:loop-variable-type-constructor
                          loop-variable-type-constructor-stx)
                    #:defaults ([loop-variable-type-constructor-stx
                                 #'(λ (element-type) element-type)])))
        ...)
     (define/syntax-parse collection-node-ast-type
       (syntax-parse #'bind-whole-collection?
         [#t #'Definition]
         [#f #'Expression]))
     #'(begin
         (define collection-type-constructor collection-type-constructor-stx)
         (define loop-type-function (~? loop-type-constructor-stx
                                        collection-type-constructor-stx))
         (define body-type-function
           (~? body-type-constructor-stx
               (λ (loop-type elem-type)
                 (define return-inner (fresh-type-variable))
                 (define return-outer (loop-type-function return-inner))
                 (unify! return-outer loop-type)
                 return-inner)))
         (define loop-variable-type-constructor loop-variable-type-constructor-stx)
         (add-to-grammar
          component
          [loop-node-name
           loop-ast-type
           ([collection : collection-node-ast-type]
            [elemname : DefinitionNoRhs = (create-ast-bud)]
            [body : loop-body-ast-type = (create-ast-bud)])
           #:prop binding-structure 'serial/all
           #:prop strict-child-order? #t
           #:prop edit
           ;; Fill in elemname after collection
           (λ (n)
             (and (ast-bud-node? (ast-child 'elemname n))
                  (att-value 'xsmith_no-holes-in-subtree?
                             (ast-child 'collection n))
                  (let* ([collection-type (att-value 'xsmith_type
                                                     (ast-child 'collection n))]
                         [inner-type (fresh-type-variable)]
                         [_void (unify! (collection-type-constructor inner-type)
                                        collection-type)]
                         ;; We have to force type exploration so that the type
                         ;; field of the DefinitionNoRhs will match the
                         ;; collection type properly.
                         [_void (force-type-exploration-for-node!
                                 (ast-child 'collection n))]
                         [new-def (make-fresh-node
                                   'DefinitionNoRhs
                                   (hash 'name (fresh-var-name "loopvar_")
                                         'type (concretize-type
                                                (loop-variable-type-constructor
                                                 inner-type))))])
                    (λ () (rewrite-subtree (ast-child 'elemname n)
                                           new-def)))))
           #:prop edit
           ;; Fill in body after elemname
           (λ (n)
             (and (ast-bud-node? (ast-child 'body n))
                  (not (ast-bud-node? (ast-child 'elemname n)))
                  (not (att-value 'xsmith_is-hole? (ast-child 'elemname n)))
                  (λ () (rewrite-subtree (ast-child 'body n)
                                         (make-hole 'loop-body-ast-type)))))
           (~? (~@ #:prop mutable-container-access
                   mutable-container-access-prop-val))
           #:prop type-info
           [(λ (n) (loop-type-function (fresh-type-variable)))
            (λ (n t)
              (define elemtype (fresh-type-variable))
              (hash 'body (body-type-function t elemtype)
                    'elemname (loop-variable-type-constructor elemtype)
                    'collection (collection-type-constructor elemtype)))]
           ]))]))




;;;;;; Types

;; Statement types
(define-generic-type return-type (type))
(define no-return-type (base-type 'no-return-type))

(define (fresh-maybe-return-type)
  (fresh-type-variable (return-type (fresh-type-variable))
                       no-return-type))

;; Expression types
(type-variable-subtype-default #t)
;; TODO - these should be injectable.  They are used in the type specifications,
;; but sometimes I want to create a slightly different hierarchy while still using
;; the same canned components.
(define void-type (base-type 'void))
(define number-type (base-type 'number #:leaf? #f))
(define int-type (base-type 'int number-type))
(define float-type (base-type 'float number-type))
(define bool-type (base-type 'bool))
(define string-type (base-type 'string))

(define-generic-type mutable ([type invariant]))
(define-generic-type immutable ([type covariant]))

(define-generic-type array-type ([type covariant]))
(define (fresh-array-type) (array-type (fresh-type-variable)))
(define-generic-type list-type ([type covariant]))
(define (fresh-list-type) (list-type (fresh-type-variable)))
(define-generic-type dictionary-type ([key-type covariant] [value-type covariant]))

(define no-child-types (λ (n t) (hash)))

(define (fresh-concrete-var-type)
  (concretize-type (fresh-type-variable)))


(define (numeric-bin-op/no-relation-to-return number-type)
  (λ (n t) (hash 'l number-type 'r number-type)))
(define numeric-bin-op-subtype
  (λ (n t)
    (hash 'l t 'r t)))
