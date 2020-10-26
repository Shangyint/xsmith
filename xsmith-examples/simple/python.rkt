#lang clotho

(require
 xsmith
 xsmith/app
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 pprint
 racket/string
 )

(define-basic-spec-component python-comp)

(define dictionary-key-type
  (λ () (fresh-type-variable int-type bool-type string-type)))
(define dictionary-value-type
  (λ () (fresh-type-variable int-type bool-type string-type)))

(add-basic-expressions python-comp
                       #:VariableReference #t
                       #:ProcedureApplication #t
                       #:LambdaWithExpression #t
                       #:Numbers #t
                       #:Booleans #t
                       #:Strings #t
                       #:MutableArray #t
                       #:MutableDictionary #t
                       #:MutableStructuralRecord #t
                       #:dictionary-key-type (dictionary-key-type)
                       #:dictionary-value-type (dictionary-value-type)
                       )
(add-basic-statements python-comp
                      #:ProgramWithBlock #t
                      #:NamedFunctionDefinition #t
                      #:Block #t
                      #:ReturnStatement #t
                      #:IfElseStatement #t
                      #:ExpressionStatement #t
                      #:AssignmentStatement #t
                      #:NullStatement #t
                      #:MutableArraySafeAssignmentStatement #t
                      #:MutableDictionarySafeAssignmentStatement #t
                      #:MutableStructuralRecordAssignmentStatement #t
                      #:dictionary-key-type (dictionary-key-type)
                      #:dictionary-value-type (dictionary-value-type)
                      )

(define nest-step 4)
(define (binary-op-renderer op-rendered)
  (λ (n) (h-append lparen ($xsmith_render-node (ast-child 'l n))
                   space op-rendered space
                   ($xsmith_render-node (ast-child 'r n)) rparen)))
(add-property
 python-comp
 render-hole-info
 [#f (λ (h) (text "«HOLE»"))])

(define (comma-list doc-list)
  (apply h-append
         (apply-infix (h-append comma space)
                      doc-list)))


(add-loop-over-container
 python-comp
 ;; Sure, Python calls them lists, but my type system calls them arrays.
 #:name ArrayComprehension
 #:collection-type-constructor (λ (elem-type) (mutable (array-type elem-type))))
(add-property
 python-comp render-node-info
 [ArrayComprehension
  ;; [body for binder_name in collection]
  (λ (n) (h-append (text "[")
                   ($xsmith_render-node (ast-child 'body n))
                   (text " for ")
                   (text (ast-child 'name (ast-child 'elemname n)))
                   (text " in ")
                   ($xsmith_render-node (ast-child 'collection n))
                   (text "]")))])

(add-loop-over-container
 python-comp
 #:name LoopOverArray
 #:collection-type-constructor (λ (elem-type) (mutable (array-type elem-type)))
 #:loop-type-constructor (λ (elem-type) (fresh-maybe-return-type))
 #:body-type-constructor (λ (loop-type elem-type) loop-type)
 #:loop-ast-type Statement
 #:body-ast-type Block
 #:bind-whole-collection? #t
 )

(define (render-let varname rhs body)
  (h-append (text "(lambda ")
            varname
            (text ": ")
            body
            (text ")(")
            rhs
            (text ")")
            ))

(add-property
 python-comp
 render-node-info

 [LoopOverArray
  (λ (n)
    (define cd (ast-child 'collection n))
    (define collection-name (ast-child 'name cd))
    (define body (ast-child 'body n))
    (v-append
     (h-append (text collection-name)
               (text " = ")
               ($xsmith_render-node (ast-child 'Expression cd)))
     (h-append (text "for ")
               (text (ast-child 'name (ast-child 'elemname n)))
               (text " in ")
               (text collection-name)
               (text ":")
               (nest nest-step
                     (h-append
                      line
                      (v-concat
                       (append
                        (map (λ (cn) ($xsmith_render-node cn))
                             (ast-children (ast-child 'definitions body)))
                        (map (λ (cn) ($xsmith_render-node cn))
                             (ast-children (ast-child 'statements body))))))))
     line))]

 [ProgramWithBlock
  (λ (n)
    (define definitions (ast-children (ast-child 'definitions n)))
    (v-append
     (text "FAKEBLOCK = True")
     (text "safe_divide = lambda a, b: a if (b == 0) else (a / b)")
     (vb-concat
      (list*
       (text "")
       (text "")
       (map (λ (cn) ($xsmith_render-node cn))
            (append definitions
                    (list (ast-child 'Block n))))))
     (text "")
     (apply v-append
            (map (λ (v) (text (format "print(~a)\n"
                                      (ast-child 'name v))))
                 (filter (λ (x) (base-type? (ast-child 'type x)))
                         definitions)))
     ;; Hack to get a newline...
     (text "")))]

 [Definition
   (λ (n)
     (let ([expr-node (ast-child 'Expression n)])
       ;; TODO: I would like this to be handled more elegantly.
       (if (equal? (ast-node-type expr-node)
                   'NamedFunctionDefinition)
           (let ([name (ast-child 'name n)]
                 [parameters (ast-children (ast-child 'parameters expr-node))]
                 [body (ast-child 'body expr-node)])
             (nest nest-step
                   (v-append
                    (h-append (text "def ")
                              (text (format "~a" name))
                              lparen
                              (h-concat (apply-infix
                                         (text ", ")
                                         (map (λ (cn) ($xsmith_render-node cn)) parameters)))
                              rparen
                              colon)
                    ;; If any global variables are assigned to, they need a
                    ;; declaration "global <varname>".
                    (let* ([assignment-statements
                            (att-value
                             'xsmith_find-descendants
                             n
                             (λ (cn) (node-subtype? cn 'AssignmentStatement)))]
                           [global-var-nodes
                            (filter (λ (cn)
                                      (define binding (att-value 'xsmith_binding cn))
                                      (define def-node (binding-ast-node binding))
                                      (define ancestor-types
                                        (map ast-node-type
                                             (ancestor-nodes def-node)))
                                      (not (memq 'NamedFunctionDefinition
                                                 ancestor-types)))
                                    assignment-statements)]
                           [global-var-names
                            (map
                             (λ (assign-node) (ast-child 'name assign-node))
                             global-var-nodes)]
                           [nonlocal-var-nodes
                            (filter (λ (cn)
                                      (define binding (att-value 'xsmith_binding cn))
                                      (define def-node (binding-ast-node binding))
                                      (define ancestor-types
                                        (map ast-node-type
                                             (ancestor-nodes def-node)))
                                      (and (not (memq n (ancestor-nodes def-node)))
                                           (memq 'NamedFunctionDefinition
                                                 ancestor-types)))
                                    assignment-statements)]
                           [nonlocal-var-names
                            (map
                             (λ (assign-node) (ast-child 'name assign-node))
                             nonlocal-var-nodes)])
                      (apply
                       v-append
                       (append
                        (map (λ (name) (h-append (text "global ") (text name)))
                             global-var-names)
                        (map (λ (name) (h-append (text "nonlocal ") (text name)))
                             nonlocal-var-names))))
                    ($xsmith_render-node body))))
           (h-append (text (ast-child 'name n))
                     space
                     equals
                     space
                     ($xsmith_render-node (ast-child 'Expression n))))))]

 [Block
  ;; Python doesn't have a stand-alone block construct, so we'll fake it.
  (λ (n)
    (h-append
     (text "if FAKEBLOCK:")
     (nest nest-step
           (h-append
            line
            (v-concat
             (append
              (map (λ (cn) ($xsmith_render-node cn))
                   (ast-children (ast-child 'definitions n)))
              (map (λ (cn) ($xsmith_render-node cn))
                   (ast-children (ast-child 'statements n)))))))
     line))]

 [ExpressionStatement (λ (n) ($xsmith_render-node (ast-child 'Expression n)))]

 [ReturnStatement (λ (n) (h-append (text "return ")
                                   ($xsmith_render-node (ast-child 'Expression n))))]

 [NullStatement (λ (n) (text "pass"))]
 [AssignmentStatement
  (λ (n)
    (hs-append (text (format "~a" (ast-child 'name n)))
               equals
               ($xsmith_render-node (ast-child 'Expression n))))]

 [IfElseStatement
  (λ (n)
    (h-append
     (h-append (text "if") space lparen ($xsmith_render-node (ast-child 'test n)) rparen
               space
               (text ":"))
     (nest nest-step
           (h-append line
                     (v-concat
                      (let ([b (ast-child 'then n)])
                        (append
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'definitions b)))
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'statements b))))))))
     line
     (text "else:")
     (nest nest-step
           (h-append line
                     (v-concat
                      (let ([b (ast-child 'else n)])
                        (append
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'definitions b)))
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'statements b))))))))
     line))]



 [VariableReference (λ (n) (text (format "~a" (ast-child 'name n))))]

 [ProcedureApplication
  (λ (n) (h-append ($xsmith_render-node (ast-child 'procedure n))
                   lparen
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'arguments n))))
                   rparen))]
 [FormalParameter (λ (n) (text (format "~a" (ast-child 'name n))))]
 [LambdaWithExpression
  (λ (n) (h-append lparen
                   (text "lambda ")
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'parameters n))))
                   colon
                   space
                   ($xsmith_render-node (ast-child 'body n))
                   rparen))]

 [BoolLiteral (λ (n) (text (if (ast-child 'v n) "True" "False")))]
 [Not (λ (n) (h-append (text "not") lparen
                       ($xsmith_render-node (ast-child 'Expression n))
                       rparen))]
 [And (binary-op-renderer (text "and"))]
 [Or (binary-op-renderer (text "or"))]

 [IntLiteral (λ (n) (text (format "~a" (ast-child 'v n))))]
 [Plus (binary-op-renderer (text "+"))]
 [Minus (binary-op-renderer (text "-"))]
 [Times (binary-op-renderer (text "*"))]
 [LessThan (binary-op-renderer (text "<"))]
 [GreaterThan (binary-op-renderer (text ">"))]

 [SafeDivide (λ (n) (h-append (text "safe_divide") lparen
                              ($xsmith_render-node (ast-child 'l n))
                              (text ",") space
                              ($xsmith_render-node (ast-child 'r n))
                              rparen))]

 [StringLiteral (λ (n) (text (format "\"~a\"" (ast-child 'v n))))]
 [StringAppend (binary-op-renderer (text "+"))]
 [StringLength (λ (n) (h-append (text "len")
                                lparen
                                ($xsmith_render-node (ast-child 'Expression n))
                                rparen))]

 [MutableArrayLiteral
  (λ (n) (h-append lbracket
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'expressions n))))
                   rbracket))]
 [MutableArraySafeReference
  (λ (n)
    (define array-var (text (fresh-var-name "arr_")))
    (render-let array-var
                ($xsmith_render-node (ast-child 'array n))
                (h-append array-var
                          lbracket
                          ($xsmith_render-node (ast-child 'index n))
                          (text " % ")
                          (text "len") lparen array-var rparen
                          rbracket)))]
 [MutableArraySafeAssignmentStatement
  (λ (n)
    (define array-var (text (fresh-var-name "arr_")))
    (v-append
     (h-append array-var (text " = ") ($xsmith_render-node (ast-child 'array n)))
     (h-append  array-var
                lbracket
                ($xsmith_render-node (ast-child 'index n))
                (text " % ")
                (text "len") lparen array-var rparen
                rbracket
                space equals space ($xsmith_render-node (ast-child 'newvalue n)))))]

 [MutableDictionarySafeLiteral
  (λ (n)
    (h-append lbrace
              (comma-list (for/list ([key (ast-children (ast-child 'keys n))]
                                     [val (ast-children (ast-child 'vals n))])
                            (h-append ($xsmith_render-node key)
                                      colon
                                      space
                                      ($xsmith_render-node val))))
              rbrace))]
 [MutableDictionarySafeReferenceWithDefault
  (λ (n)
    (define dictionary-rendered ($xsmith_render-node (ast-child 'dictionary n)))
    (h-append dictionary-rendered
              dot
              (text "get")
              lparen
              ($xsmith_render-node (ast-child 'accessKey n))
              comma
              space
              ($xsmith_render-node (ast-child 'defaultValue n))
              rparen))]
 [MutableDictionarySafeAssignmentStatement
  (λ (n)
    (define dict-var (text (fresh-var-name "dict_")))
    (v-append
     (h-append dict-var
               (text " = ")
               ($xsmith_render-node (ast-child 'dictionary n)))
     (h-append dict-var
               lbracket
               (text "list") lparen dict-var (text ".keys()") rparen
               lbracket
               ($xsmith_render-node (ast-child 'index n))
               (text " % ")
               (text "len") lparen dict-var (text ".keys()") rparen
               rbracket
               rbracket)))]

 [MutableStructuralRecordLiteral
  (λ (n)
    (h-append lbrace
              (comma-list (map (λ (fieldname expression-node)
                                 (h-append dquote (text (format "~a" fieldname)) dquote
                                           colon
                                           space
                                           ($xsmith_render-node expression-node)))
                               (ast-child 'fieldnames n)
                               (ast-children (ast-child 'expressions n))))
              rbrace))]
 [MutableStructuralRecordReference
  (λ (n) (h-append ($xsmith_render-node (ast-child 'record n))
                   lbracket dquote
                   (text (format "~a" (ast-child 'fieldname n)))
                   dquote rbracket))]
 [MutableStructuralRecordAssignmentStatement
  (λ (n) (h-append ($xsmith_render-node (ast-child 'record n))
                   lbracket dquote
                   (text (format "~a" (ast-child 'fieldname n)))
                   dquote rbracket
                   space equals space
                   ($xsmith_render-node (ast-child 'newvalue n))))]
 )



(define (type-thunks-for-concretization)
  (list
   (λ()int-type)
   (λ()bool-type)
   (λ()string-type)
   (λ()(mutable (array-type (fresh-type-variable))))
   (λ()(mutable (dictionary-type (dictionary-key-type) (dictionary-value-type))))
   (λ()(mutable (fresh-structural-record-type)))
   ))

(define (python-format-render doc)
  (pretty-format doc 120))


(define-xsmith-interface-functions
  [python-comp]
  #:fuzzer-name python
  #:type-thunks type-thunks-for-concretization
  #:program-node ProgramWithBlock
  #:format-render python-format-render
  #:comment-wrap (λ (lines) (string-join (map (λ (l) (format "# ~a" l)) lines)
                                         "\n")))

(module+ main (python-command-line))
