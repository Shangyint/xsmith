#lang clotho

(require
 xsmith
 xsmith/app
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 (except-in pprint empty)
 racket/format
 racket/string
 racket/match
 racket/list
 syntax/parse/define
 "../private/util.rkt"
 "../private/xsmith-examples-version.rkt"
 (for-syntax
  racket/base
  syntax/parse
  ))

(define-basic-spec-component python-comp)

;; Helper parsers for easily adding elements to the grammar.
(define-syntax-parser ag [(_ arg ...) #'(add-to-grammar python-comp arg ...)])
(define-syntax-parser ap [(_ arg ...) #'(add-property python-comp arg ...)])

;; TODO - types - add characters, sets, iterables, what else?

(define number-type (base-type 'number #:leaf? #f))
(define real-type (base-type 'real number-type #:leaf? #f))
(define int-type (base-type 'int real-type))
(define float-type (base-type 'float real-type))
;; NOTE - Certain calls in Python can raise OverflowError or MemoryError when
;;        given numbers of a certain size.
(define ssize_t-max-size (expt 2 63))
(define overflow-safe-ssize_t-min-value (* -1 ssize_t-max-size))  ;; Minimum allowable value that does not cause OverflowError.
(define overflow-safe-ssize_t-max-value (sub1 ssize_t-max-size))  ;; Maximum allowable value that does not cause OverflowError.
(define overflow-safe-ssize_t-type (base-type 'overflow-safe-ssize_t #:leaf? #t))
(define int-max-size (expt 2 31))
(define overflow-safe-int-min-value (* -1 int-max-size))
(define overflow-safe-int-max-value (sub1 int-max-size))
(define overflow-safe-int-type (base-type 'overflow-safe-int #:leaf? #t))
(define string-safe-int-max-value (expt 2 16))  ;; Arbitrary max argument value for string-generating functions.
(define string-safe-int-type (base-type 'string-safe-int #:leaf? #t))
;; NOTE - The range function can often interact poorly with some other functions
;;        when its operands are very large integers. We restrict them to avoid
;;        this unfortunate situation.
(define reasonable-range-max-value (expt 2 24))
(define reasonable-range-half-value (/ reasonable-range-max-value 2))

;; TODO - This is a very primitive implementation of types.
;;
;; I think the type of types could be a generic that contains a tuple of
;; attributes (fields or methods) and their associated types. This would then
;; support functionality like:
;;   - hasattr()
;;   - getattr()
;;   - setattr()
;;   - instantiation
;;   - methods
(define type-type (base-type 'type #:leaf? #t))

(define char-type (base-type 'char))
(define dictionary-key-type
  (λ () (fresh-type-variable int-type bool-type string-type)))
(define dictionary-value-type
  (λ () (fresh-type-variable)))
(define (fresh-dictionary key-type value-type)
  (fresh-type-variable
   (mutable (dictionary-type key-type value-type))
   (immutable (dictionary-type key-type value-type))))
(define byte-string-type (base-type 'byte-string))
(define tuple-max-length 6)
(define-generic-type iterator-type ([type covariant]))
(define-generic-type iterable-type ([type covariant]))
(define-generic-type sequence-type ([type covariant]))
;; NOTE - The Python perspective on iterables, iterators, sequences, etc. is...
;;        difficult.
;; Sequences are not iterables, but the `iter` function is able to treat any
;; sequence as an iterable. However, whether a function uses `iter` to allow
;; sequences as arguments is an implementation detail and cannot be dictated by
;; the type system. Therefore, we must handle them separately and explicitly
;; allow either in those cases where it is permissible.
;;
;; iterable:
;;   - __iter__()
;; iterator:
;;   - __iter__() (so iterators are iterable)
;;   - __next__()
;; sequence:
;;   - __getitem__()
;;   - __len__()
;;
;; mutable sequence:
;;   - list, bytearray
;; immutable sequence:
;;   - tuple, range, str, bytes
;;   - everything in mutable sequences
;;   - __hash__() (if sub-elements are hashable)
(define (fresh-iterable-or-sequence-or-array inner)
  (fresh-type-variable
   (immutable (fresh-type-variable (iterable-type inner)
                                   (sequence-type inner)))
   (mutable (fresh-type-variable (iterable-type inner)
                                 (sequence-type inner)
                                 (array-type inner)))))
(define (fresh-iterator-or-iterable-or-sequence-or-array inner)
  (fresh-type-variable
   (immutable (fresh-type-variable (iterator-type inner)
                                   (iterable-type inner)
                                   (sequence-type inner)))
   (mutable (fresh-type-variable (iterable-type inner)
                                 (sequence-type inner)
                                 (array-type inner)))))
;; NOTE - Python only provides immutable iterators.
(define (fresh-immutable-iterator inner)
  (immutable (iterator-type inner)))
(define (fresh-iterable inner)
  (fresh-type-variable
   (immutable (fresh-type-variable (iterable-type inner)))
   (mutable (fresh-type-variable (iterable-type inner)
                                 (array-type inner)))))  ;; TODO - mark MutableArray as instance of MutableIterable
(define (fresh-sequence inner)
  (fresh-type-variable
   (immutable (sequence-type inner))
   (mutable (fresh-type-variable (sequence-type inner)
                                 (array-type inner)))))
(define (fresh-immutable-iterable inner-type)
  (immutable (iterable-type inner-type)))
(define (fresh-mutable-iterable inner-type)
  (mutable (iterable-type inner-type)))
(define (fresh-immutable-sequence inner-type)
  (immutable (sequence-type inner-type)))
(define (fresh-mutable-sequence inner-type)
  (mutable (sequence-type inner-type)))
(define (fresh-mutable-array inner-type)
  (mutable (array-type inner-type)))

(define (fresh-comparable-type)
  ;; TODO - lots of things are comparable (eg. lt/gt work on them) in python,
  ;; but I'm not sure which ones I should include for fuzzing.
  (fresh-type-variable real-type
                       string-type
                       byte-string-type
                       char-type
                       bool-type
                       ))
(define (fresh-summable-type)
  (fresh-type-variable int-type))

(define (biased-random-int)
  ;; The random function returns word-sized integers.
  ;; I want more variety, like bigints.
  (match (random 6)
    [0 (random-int)]
    [1 (+ (* (random-int) (random-int)) (random-int))]
    [2 (+ (* (random-int) (random-int) (random-int)) (random-int))]
    [3 (+ (* (random-int) (random-int) (random-int) (random-int)) (random-int))]
    [4 (random 255)]
    [5 (random 10)]
    ))

#;(define (biased-random-string)
  ;; NOTE: There appears to be an issue with Unicode handling across Python
  ;;       versions that leads to inconsistent behavior. For now, Unicode
  ;;       generation is being disabled and Python will only work with ASCII.
  ;;
  ;; FIXME: Find out why this behavior exists and fix it.
  (random-string-from-char-producing-proc random-ascii-char))
(define (biased-random-string)
  (match (random 3)
    [0 (random-string)]
    [1 (random-string-from-char-producing-proc random-ascii-char)]
    [2 (random-string-from-char-producing-proc biased-random-char)]))

#;(define (biased-random-char)
  ;; NOTE: There appears to be an issue with Unicode handling across Python
  ;;       versions that leads to inconsistent behavior. For now, Unicode
  ;;       generation is being disabled and Python will only work with ASCII.
  ;;
  ;; FIXME: Find out why this behavior exists and fix it.
  (random-ascii-char))
(define (biased-random-char)
  ;; Random-char very rarely generates ascii, which is more common.
  ;; More saliently, low-value characters are interned in Racket and
  ;; high-value characters are not.  So I want to be sure to generate
  ;; both to have some variety.
  (if (random-bool)
      (random-char)
      (random-ascii-char)))

(define (random-byte-list-length) (random 30))
(define (random-byte) (random 256))
(define (random-byte-string)
  (bytes->immutable-bytes (apply bytes
                                 (map (λ (x) (random-byte))
                                      (make-list (random-byte-list-length) #f)))))

(add-basic-expressions python-comp
                       #:VariableReference #t
                       #:ProcedureApplication #t
                       #:LambdaWithExpression #t
                       #:int-type int-type
                       #:number-type number-type
                       #:index-and-length-type int-type
                       #:int-literal-value (biased-random-int)
                       #:Booleans #t
                       #:Strings #t
                       #:string-literal-value (biased-random-string)
                       #:MutableArray #t
                       #:MutableDictionary #t
                       #:MutableDictionarySafeReferenceByKey #t
                       #:MutableDictionarySafeReferenceByIndex #t
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
                      #:MutableDictionarySafeAssignmentByKeyStatement #t
                      #:MutableDictionarySafeAssignmentByIndexStatement #t
                      #:MutableStructuralRecordAssignmentStatement #t
                      #:int-type int-type
                      #:index-and-length-type int-type
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
 #:collection-type-constructor (λ (elem-type) (fresh-iterator-or-iterable-or-sequence-or-array elem-type))
 #:loop-type-constructor (λ (elem-type) (mutable (array-type elem-type))))
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
 ;; This produces simple generator comprehensions.
 #:name SimpleGenerator
 #:collection-type-constructor (λ (elem-type) (fresh-iterator-or-iterable-or-sequence-or-array elem-type))
 #:loop-type-constructor (λ (elem-type) (fresh-immutable-iterator elem-type)))
(add-property
 python-comp render-node-info
 [SimpleGenerator
  ;; (body for binder_name in collection)
  (λ (n) (h-append lparen
                   ($xsmith_render-node (ast-child 'body n))
                   (text " for ")
                   (text (ast-child 'name (ast-child 'elemname n)))
                   (text " in ")
                   ($xsmith_render-node (ast-child 'collection n))
                   rparen))])

(add-loop-over-container
 python-comp
 #:name LoopOverArray
 #:collection-type-constructor (λ (elem-type) (fresh-iterator-or-iterable-or-sequence-or-array elem-type))
 #:loop-type-constructor (λ (elem-type) no-return-type)
 #:body-type-constructor (λ (loop-type elem-type) loop-type)
 #:loop-ast-type Statement
 #:body-ast-type Block
 #:bind-whole-collection? #t
 )
(add-property
 python-comp render-node-info
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
     line))])

(define no-child-types (λ (n t) (hash)))
(define render-expression-child
  (λ (n)
    ($xsmith_render-node (ast-child 'Expression n))))
(define (render-child-in func-name)
  (λ (n)
    (h-append (text func-name)
              lparen
              (render-expression-child n)
              rparen)))
(define render-child-in-iter
  (render-child-in "iter"))
(define render-child-in-tuple
  (render-child-in "tuple"))
(define render-child-in-list
  (render-child-in "list"))

(add-to-grammar
 python-comp
 [CharLiteral Expression ([v = (random-char)])
              #:prop type-info [char-type no-child-types]
              #:prop choice-weight (depth-weight)
              #:prop render-node-info
              (λ (n) (text (python-string-format (string (ast-child 'v n)))))]
 [StringToSequence Expression (Expression)
                   #:prop depth-increase 0
                   #:prop wont-over-deepen #t
                   #:prop type-info [(immutable (sequence-type char-type))
                                     (λ (n t) (hash 'Expression string-type))]
                   #:prop render-node-info render-expression-child]
 [CharsToString Expression (Expression)
                #:prop type-info
                [string-type (λ (n t) (hash 'Expression (fresh-sequence char-type)))]
                #:prop render-node-info
                (λ (n) (h-append (text "\"\".join(")
                                 ($xsmith_render-node (ast-child 'Expression n))
                                 (text ")")))]
 [ByteStringToSequence Expression (Expression)
                       #:prop depth-increase 0
                       #:prop wont-over-deepen #t
                       #:prop type-info
                       [(immutable (sequence-type int-type))
                        (λ (n t) (hash 'Expression byte-string-type))]
                       #:prop render-node-info render-expression-child]
 [ByteStringLiteral Expression ([v = (random-byte-string)])
                    #:prop type-info [byte-string-type no-child-types]
                    #:prop choice-weight (depth-weight)
                    #:prop render-node-info
                    (λ (n) (text (python-byte-string-format (ast-child 'v n))))]
 [ByteStringAppend Expression ([l : Expression] [r : Expression])
                   #:prop type-info [byte-string-type (λ (n t) (hash 'l t 'r t))]
                   #:prop render-node-info (binary-op-renderer (text "+"))]
 [ByteStringLength Expression (Expression)
                   #:prop type-info
                   [int-type (λ (n t) (hash 'Expression byte-string-type))]
                   #:prop render-node-info
                   (λ (n) (h-append (text "len") lparen
                                    ($xsmith_render-node (ast-child 'Expression n))
                                    rparen))]
 [StringToByteString Expression (Expression)
                     #:prop type-info
                     [byte-string-type (λ (n t) (hash 'Expression string-type))]
                     #:prop render-node-info
                     (λ (n) (h-append lparen
                                      ($xsmith_render-node (ast-child 'Expression n))
                                      rparen
                                      (text ".encode('UTF-8')")))]


 [TupleLiteral Expression ([values : Expression *])
               #:prop wont-over-deepen #t
               #:prop choice-weight (depth-weight)
               #:prop fresh (let* ([t (att-value 'xsmith_type (current-hole))]
                                   [pt (product-type #f)]
                                   [_ (unify! t pt)]
                                   [inners (product-type-inner-type-list pt)]
                                   [len (if inners
                                            (length inners)
                                            (random tuple-max-length))])
                              (hash 'values len))
               #:prop type-info
               [(product-type #f)
                (λ (n t)
                  (define children (ast-children (ast-child 'values n)))
                  (define child-types (map (λ (x) (fresh-type-variable)) children))
                  (unify! t (product-type child-types))
                  (for/hash ([c children] [ct child-types]) (values c ct)))]
               #:prop render-node-info
               (λ (n) (h-append
                       (text "(")
                       (h-concat (for/list ([c (ast-children
                                                (ast-child 'values n))])
                                   (h-append
                                    ($xsmith_render-node c)
                                    ;; Thankfully python allows a trailing comma.
                                    (text ", "))))
                       (text ")")))]
 [TupleReference Expression ([tuple : Expression]
                             [length]
                             [index])
                 #:prop fresh (let* ([length (add1 (random (sub1 tuple-max-length)))]
                                     [index (random length)])
                                (hash 'length length 'index index))
                 #:prop type-info
                 [(fresh-type-variable)
                  (λ (n t)
                    (define index (ast-child 'index n))
                    (define child-types
                      (for/list ([i (in-range (ast-child 'length n))])
                        (if (equal? i index)
                            t
                            (fresh-type-variable))))
                    (hash 'tuple (product-type child-types)))]
                 #:prop render-node-info
                 (λ (n) (h-append ($xsmith_render-node (ast-child 'tuple n))
                                  (text (format "[~a]" (ast-child 'index n)))))]
 [DictKeys Expression (Expression)
           #:prop type-info
           ;; I'm not sure about the mutability of this...
           [(immutable (sequence-type (dictionary-key-type)))
            (λ (n t)
              (define kt (dictionary-key-type))
              (unify! (immutable (sequence-type kt)) t)
              (hash 'Expression
                    (mutable (dictionary-type kt (dictionary-value-type)))))]
           #:prop render-node-info
           (λ (n) (h-append ($xsmith_render-node (ast-child 'Expression n))
                            (text ".keys()")))]
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Zero-Cost Converters
;;;;

(define-syntax-parser ag/zero-cost-simple-converter
  [(_ name:id
      ;; `consumes` and `produces` are expressions that resolve to simple types.
      (~seq #:consumes consumes:expr)
      (~seq #:produces produces:expr)
      ;; `renderer` is a function that takes a node as argument and renders it.
      (~seq #:renderer render-func:expr))
   #'(ag [name Expression ([Expression])
               #:prop depth-increase 0
               #:prop wont-over-deepen #t
               #:prop choice-weight (depth-weight)
               #:prop type-info
               [produces
                (λ (n t)
                  (hash 'Expression consumes))]
               #:prop render-node-info render-func])])

;; A syntax parser to make creating zero-cost container converters easier.
(define-syntax-parser ag/zero-cost-container-converter
  [(_ name:id
      ;; `consumes` and `produces` are functions that take an inner type as
      ;; argument and create a new wrapper type.
      (~seq #:consumes consumes:expr)
      (~seq #:produces produces:expr)
      ;; `renderer` is a function that takes a node as argument and renders it.
      (~seq #:renderer render-func:expr))
   #'(ag [name Expression ([Expression])
               #:prop depth-increase 0
               #:prop wont-over-deepen #t
               #:prop choice-weight 1
               #:prop type-info
               [(produces (fresh-type-variable))
                (λ (n t)
                  (define inner-type (fresh-type-variable))
                  (define produced-type (produces inner-type))
                  (unify! t produced-type)
                  (define consumed-type (consumes inner-type))
                  (hash 'Expression consumed-type))]
               #:prop render-node-info render-func])])

;;;;
;; Numerical Converters

(ag/zero-cost-simple-converter
 IntToOverflowSafeInt
 #:consumes int-type
 #:produces overflow-safe-int-type
 #:renderer (render-child-in "NE_safe_int"))
(ag/zero-cost-simple-converter
 IntToOverflowSafeSsizeT
 #:consumes int-type
 #:produces overflow-safe-ssize_t-type
 #:renderer (render-child-in "NE_safe_ssize_t"))
(ag/zero-cost-simple-converter
 IntToStringSafeInt
 #:consumes int-type
 #:produces string-safe-int-type
 #:renderer (render-child-in "NE_safe_string_int"))

;;;;
;; Iterator/Iterable/Sequence/Array Converters

;; ___->ImmutableIterator
(ag/zero-cost-container-converter
 ConvertToIterator
 #:consumes fresh-iterable-or-sequence-or-array
 #:produces fresh-immutable-iterator
 #:renderer render-child-in-iter)
;; ___->ImmutableSequence (Tuple)
(ag/zero-cost-container-converter
 ConvertToImmutableSequence
 #:consumes (λ (inner)
              (fresh-type-variable
               (immutable (fresh-type-variable (iterator-type inner)
                                               (iterable-type inner)))
               (mutable (fresh-type-variable (iterable-type inner)
                                             (sequence-type inner)
                                             (array-type inner)))))
 #:produces fresh-immutable-sequence
 #:renderer render-child-in-tuple)
;; ___->MutableArray (List)
(ag/zero-cost-container-converter
 ConvertToMutableArray
 #:consumes (λ (inner)
              (fresh-type-variable
               (immutable (fresh-type-variable (iterator-type inner)
                                               (iterable-type inner)
                                               (sequence-type inner)))
               (mutable (fresh-type-variable (iterable-type inner)))))
 #:produces fresh-mutable-array
 #:renderer render-child-in-list)
;; ImmutableIterator==ImmutableIterable
(ag/zero-cost-container-converter
 IteratorIsAnIterable
 #:consumes fresh-immutable-iterator
 #:produces fresh-immutable-iterable
 #:renderer render-expression-child)
;; ImmutableSequence==ImmutableIterable
(ag/zero-cost-container-converter
 ImmutableSequenceIsAnImmutableIterable
 #:consumes fresh-immutable-sequence
 #:produces fresh-immutable-iterable
 #:renderer render-expression-child)
;; MutableSequence==MutableIterable
(ag/zero-cost-container-converter
 MutableSequenceIsAMutableIterable
 #:consumes fresh-mutable-sequence
 #:produces fresh-mutable-iterable
 #:renderer render-expression-child)
;; MutableArray==MutableSequence
(ag/zero-cost-container-converter
 MutableArrayIsAMutableSequence
 #:consumes fresh-mutable-array
 #:produces fresh-mutable-sequence
 #:renderer render-expression-child)

;; Numbers.  The canned-components numbers aren't quite right if we allow complex numbers.
(add-to-grammar
 python-comp
 [NumberLiteral Expression (v)
                #:prop may-be-generated #f
                #:prop choice-weight (depth-weight)]
 [IntLiteral NumberLiteral ()
             #:prop fresh (hash 'v (biased-random-int))]
 [Plus Expression ([l : Expression] [r : Expression])]
 [Minus Expression ([l : Expression] [r : Expression])]
 [Times Expression ([l : Expression] [r : Expression])]
 [SafeDivide Expression ([l : Expression] [r : Expression])]
 [SafeIntDivide Expression ([l : Expression] [r : Expression])]
 [LessThan Expression ([l : Expression] [r : Expression])]
 [GreaterThan Expression ([l : Expression] [r : Expression])])
(define numeric-bin-op-subtype (λ (n t) (hash 'l t 'r t)))
(define (comparison-child-types n t)
  (define ct (fresh-comparable-type))
  (hash 'l ct 'r ct))
(add-property
 python-comp type-info
 [IntLiteral [int-type no-child-types]]
 [Plus [(fresh-subtype-of number-type) numeric-bin-op-subtype]]
 [Minus [(fresh-subtype-of number-type) numeric-bin-op-subtype]]
 [Times [(fresh-subtype-of number-type) numeric-bin-op-subtype]]
 [SafeDivide [real-type (λ (n t) (hash 'l real-type 'r real-type))]]
 [SafeIntDivide [int-type (λ (n t) (hash 'l int-type 'r int-type))]]
 [LessThan [bool-type comparison-child-types]]
 [GreaterThan [bool-type comparison-child-types]])

(define (render-let varname rhs body)
  (h-append (text "(lambda ")
            varname
            (text ": ")
            body
            (text ")(")
            rhs
            (text ")")
            ))

(define (python-string-format str)
  ;; IE to what Python's parser expects
  (format "\"~a\""
          (apply
           string-append
           (for/list ([c (string->list str)])
             (define ci (char->integer c))
             (cond [(equal? c #\\) "\\\\"]
                   [(equal? c #\") "\\\""]
                   [(< 31 ci 126) (string c)]
                   ;; For arbitrary unicode, you can use \Uxxxxxxxx with hex digits.
                   [else (format "\\U~a" (~r #:base 16
                                             #:min-width 8
                                             #:pad-string "0"
                                             ci))])))))
(define (python-byte-string-format str)
  ;; IE to what Python's parser expects
  (format "b\"~a\""
          (apply
           string-append
           (for/list ([c (bytes->list str)])
             (cond [(equal? c (char->integer #\\)) "\\\\"]
                   [(equal? c (char->integer #\")) "\\\""]
                   [(< 31 c 126) (string (integer->char c))]
                   ;; \xNN allows arbitrary hex characters
                   [else (format "\\x~a" (~r #:base 16
                                             #:min-width 2
                                             #:pad-string "0"
                                             c))])))))


;;;; Helpers to add a bunch of built-in functions.
;; NE stands for "no exceptions", IE safe versions of things.
;; It's shorter than the word "safe", and it will be repeated a lot...
(define NE? #t)
(define-for-syntax (racr-ize-id id)
  (datum->syntax id
                 (string->symbol
                  (string-titlecase (symbol->string (syntax->datum id))))))

(define-syntax-parser ag/zero-arg
  [(_ name:id
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'(fresh-subtype-of number-type)]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (racr-ize-id #'name)]))
           (~optional (~seq #:NE-name NE-name)
                      #:defaults ([NE-name #'name])))
      ...)
   #'(ag [racr-name Expression ()
                    #:prop type-info [type (λ (n t) (hash))]
                    #:prop render-node-info
                    (λ (n)
                      (define name (symbol->string (if NE? 'NE-name 'name)))
                      (h-append (text name)
                                lparen
                                rparen))])])

(define-ag/one-arg ag/one-arg python-comp racr-ize-id NE?
  #'(fresh-subtype-of number-type)
  (λ (name-thunk)
    (λ (n)
      (h-append (text (symbol->string (name-thunk)))
                lparen
                ($xsmith_render-node (ast-child 'Expression n))
                rparen))))
(define-ag/two-arg ag/two-arg python-comp racr-ize-id NE?
  #'(fresh-subtype-of number-type)
  (λ (name-thunk)
    (λ (n)
      (h-append (text (symbol->string (name-thunk)))
                lparen
                ($xsmith_render-node (ast-child 'l n))
                (text ", ")
                ($xsmith_render-node (ast-child 'r n))
                rparen))))
(define-ag/three-arg ag/three-arg python-comp racr-ize-id NE?
  #'(fresh-subtype-of number-type)
  (λ (name-thunk)
    (λ (n)
      (h-append (text (symbol->string (name-thunk)))
                lparen
                ($xsmith_render-node (ast-child 'l n))
                (text ", ")
                ($xsmith_render-node (ast-child 'm n))
                (text ", ")
                ($xsmith_render-node (ast-child 'r n))
                rparen))))


;;;; built-in functions from https://docs.python.org/3/library/functions.html
(ag/one-arg abs)
(ag/one-arg all #:type bool-type #:ctype (Ectype (fresh-iterable-or-sequence-or-array (fresh-type-variable))))
(ag/one-arg any #:type bool-type #:ctype (Ectype (fresh-iterable-or-sequence-or-array (fresh-type-variable))))
(ag/one-arg ascii #:type string-type #:ctype (Ectype (fresh-type-variable)))
(ag/one-arg bin #:type string-type #:ctype (Ectype int-type))
(ag/one-arg bool #:type bool-type #:ctype (Ectype (fresh-type-variable)))
;; TODO - breakpoint()  ;; XXX - we may not want to implement this, as it initializes the PDB debugger.
;; TODO - bytearray()  ;; XXX - returns a mutable byte-string, so we may need to update the byte-string-type stuff to accommodate.
;; TODO - bytes actually takes arguments of a few varieties, but they need to be constrained to avoid obvious errors.
;;          - string (requires encoding and error handling)
;;          - buffer (requires us implementing buffers)
;;          - iterable (all contents must be integers in the range [0, 255])
;;          - no argument (produces an empty byte string)
(ag/zero-arg bytes
             #:racr-name BytesZero
             #:type byte-string-type)
(ag/one-arg bytes
            #:racr-name BytesOne
            #:type byte-string-type
            #:ctype (Ectype (fresh-type-variable string-safe-int-type)))
(ag/one-arg callable #:type bool-type #:ctype (Ectype (fresh-type-variable)))
(ag/one-arg chr #:NE-name NE_chr #:type char-type #:ctype (Ectype int-type))
;; TODO - classmethod()  ;; XXX - this is to be used as a decorator prior to a class's method declaration.
;; TODO - compile()  ;; XXX we may not want to implement this, as it produces code objects that may lead to arbitrary execution.
;; TODO - complex actually allows floats as args, but I need to expand the numeric tower before I do that.
(ag/two-arg complex #:type number-type #:ctype (E2ctype int-type int-type))
;; TODO - delattr()
;; TODO - dict can also take some other configurations of arguments.
(ag/zero-arg dict
             #:racr-name DictZero
             #:type (fresh-dictionary (dictionary-key-type) (dictionary-value-type)))
(ag/one-arg dict
            #:racr-name DictOne
            #:type (fresh-dictionary (dictionary-key-type) (dictionary-value-type))
            ;; TODO - This is actually incorrect. The note on Python's sequences:
            ;;          https://docs.python.org/3/glossary.html#term-sequence
            ;;        explains that while dictionaries support getitem and len,
            ;;        they are actually considered 'mappings' instead of
            ;;        'sequences' due to the necessity of the key type being
            ;;        immutable.
            #:ctype (λ (n t)
                      (define key-type (fresh-type-variable))
                      (define value-type (fresh-type-variable))
                      (define return-dict (fresh-dictionary key-type value-type))
                      (unify! t return-dict)
                      (hash 'Expression (fresh-sequence (product-type (list key-type value-type))))))
;; Zero-arg dir returns a list of names bound in the current scope.
(ag/zero-arg dir
             #:racr-name DirZero
             #:type (array-type string-type))
;; One-arg dir returns a list of attributes of the passed-in object.
;; TODO - Figure a way to use the returned strings in, e.g., `getattr`, `delattr`.
(ag/one-arg dir
            #:racr-name DirOne
            #:type (array-type string-type)
            #:ctype (Ectype (fresh-type-variable)))
(ag/two-arg divmod #:NE-name NE_divmod
            #:type (product-type (list int-type int-type))
            #:ctype (E2ctype int-type int-type))
(ag/one-arg enumerate
            #:type (fresh-immutable-iterator (product-type (list int-type (fresh-type-variable))))
            #:ctype (λ (n t)
                      (define arg-elem (fresh-type-variable))
                      (define return-iterator (fresh-immutable-iterator arg-elem))
                      (unify! t return-iterator)
                      (define arg-iterable (fresh-iterable-or-sequence-or-array arg-elem))
                      (hash 'Expression arg-iterable)))
;; TODO - eval()
;; TODO - exec()
(ag/two-arg filter
            #:type (fresh-immutable-iterator (fresh-type-variable))
            #:ctype (λ (n t)
                      (define arg-elem (fresh-type-variable))
                      (define return-iterator (fresh-immutable-iterator arg-elem))
                      (unify! t return-iterator)
                      (define arg-array (fresh-iterable-or-sequence-or-array arg-elem))
                      (hash 'l (function-type (product-type (list arg-elem))
                                              bool-type)
                            'r arg-array)))
;; NOTE: The float function can actually take a string or an int, but the string has to be a number string...
(ag/one-arg float #:type number-type #:ctype (Ectype int-type))
;; TODO - format() -- this will probably be fine if limited to default (empty) format spec and given a value in a limited set of types.  Arbitrary types will raise problems of eg. how function X is printed.
;; TODO - frozenset()
;; TODO - getattr()
;; TODO - globals()
;; TODO - hasattr really should be using strings that represent attributes, but this is technically correct.
(ag/two-arg hasattr #:type bool-type #:ctype (λ (n t)
                                               (hash 'l (fresh-type-variable)
                                                     'r string-type)))
;; TODO - hash()
;; TODO - help()  ;; XXX - don't know if we should implement this, since it's meant for interactive use.
(ag/one-arg hex #:type string-type #:ctype (Ectype int-type))
;; NOTE - id is commented out because its return value is both implementation- and run-dependent.
;; (ag/one-arg id #:type int-type #:ctype (Ectype (fresh-type-variable)))
;; TODO - __import__()
;; TODO - input()  ;; XXX - don't know if we should implement this, since it waits for external input.
;; TODO - int()
(ag/two-arg isinstance
            #:type bool-type
            #:ctype (E2ctype (fresh-type-variable) type-type))
;; TODO - issubclass()
;; NOTE - iter() is handled in the zero-cost converters.
(ag/one-arg len #:type int-type
            #:ctype (Ectype (fresh-sequence (fresh-type-variable))))
;; NOTE - list() is handled in the zero-cost converters.
;; TODO - locals()
;; NOTE - map() is actually variadic, but xsmith doesn't really support variadic
;;        types. We define a few instances, though.
(ag/two-arg map
            #:racr-name MapTwo
            #:type (fresh-immutable-iterator (fresh-type-variable))
            #:ctype (λ (n t)
                      (define return-elem (fresh-type-variable))
                      (define return-iterator (fresh-immutable-iterator return-elem))
                      (unify! t return-iterator)
                      (define arg-elem (fresh-type-variable))
                      (define arg-array (fresh-iterable-or-sequence-or-array arg-elem))
                      (hash 'l (function-type (product-type (list arg-elem))
                                              return-elem)
                            'r arg-array)))
(ag/three-arg map
              #:racr-name MapThree
              #:type (fresh-immutable-iterator (fresh-type-variable))
              #:ctype (λ (n t)
                        (define return-elem (fresh-type-variable))
                        (define return-iterator (fresh-immutable-iterator return-elem))
                        (unify! t return-iterator)
                        (define arg1-elem (fresh-type-variable))
                        (define arg1-array (fresh-iterable-or-sequence-or-array arg1-elem))
                        (define arg2-elem (fresh-type-variable))
                        (define arg2-array (fresh-iterable-or-sequence-or-array arg2-elem))
                        (hash 'l (function-type (product-type (list arg1-elem arg2-elem))
                                                return-elem)
                              'm arg1-array
                              'r arg2-array)))
(ag/two-arg max #:NE-name NE_max
            ;; with fallback for when it gets an empty sequence
            #:type (fresh-comparable-type)
            #:ctype (λ (n t) (hash 'l (fresh-sequence t)
                                   'r t)))
;; TODO - memoryview()
(ag/two-arg min #:NE-name NE_min
            ;; with fallback for when it gets an empty sequence
            #:type (fresh-comparable-type)
            #:ctype (λ (n t) (hash 'l (fresh-sequence t)
                                   'r t)))
;; NOTE - next() takes an iterator and returns the next element in that
;;        iterator. However, this can frequently lead to Python raising
;;        StopIteration exceptions when the iterator becomes exhausted. We wrap
;;        calls to next() in an exception-avoiding function with a default value
;;        for when the exception would have been raised.
(ag/two-arg next
            #:NE-name NE_next
            #:type (fresh-type-variable)
            #:ctype (λ (n t)
                      (hash 'l (fresh-immutable-iterator t)
                            'r t)))
;; TODO - object()
(ag/one-arg oct #:type string-type #:ctype (Ectype int-type))
;; TODO - open()
(ag/one-arg ord #:type int-type #:ctype (Ectype char-type))
;; The pow function can take a long time and a lot of memory
;; (IE enough to hang the process) when given very large numbers...
#;(ag/two-arg pow #:racr-name PowTwo
              #:type int-type
              #:ctype (E2ctype int-type int-type))
#;(ag/three-arg pow #:racr-name PowThree
                #:type int-type
                #:ctype (E3ctype int-type int-type int-type))
;; TODO - print()
;; TODO - property()
(ag/one-arg range
            #:racr-name RangeOne
            #:NE-name reasonable_range
            #:type (fresh-immutable-sequence int-type)
            #:ctype (Ectype int-type))
(ag/two-arg range
            #:racr-name RangeTwo
            #:NE-name reasonable_range
            #:type (fresh-immutable-sequence int-type)
            #:ctype (E2ctype int-type int-type))
(ag/three-arg range
              #:racr-name RangeThree
              #:NE-name reasonable_range
              #:type (fresh-immutable-sequence int-type)
              #:ctype (E3ctype int-type int-type int-type))
(ag/one-arg repr #:type string-type #:ctype (Ectype (fresh-type-variable)))
(ag/one-arg reversed
            #:type (fresh-immutable-iterator (fresh-type-variable))
            #:ctype (λ (n t)
                      (define return-elem (fresh-type-variable))
                      (define return-iterator (fresh-immutable-iterator return-elem))
                      (unify! t return-iterator)
                      (define arg-elem (fresh-type-variable))
                      (define arg-array (fresh-sequence arg-elem))
                      (hash 'Expression arg-array)))
(ag/one-arg round #:type int-type #:ctype (Ectype real-type))
;; TODO - set()
;; TODO - setattr()
;; TODO - slice()
(ag/one-arg sorted
            #:type (mutable (array-type (fresh-comparable-type)))
            #:ctype (λ (n t)
                      (define return-elem (fresh-type-variable))
                      (define return-array (mutable (array-type return-elem)))
                      (unify! t return-array)
                      (define arg-iterable (fresh-iterable return-elem))
                      (hash 'Expression arg-iterable)))
;; TODO - staticmethod()
(ag/one-arg str #:type string-type #:ctype (Ectype (fresh-type-variable)))
;; NOTE - sum() can also take an optional 'start' index argument, but it is left
;;        out here to avoid index-out-of-bounds issues.
;; NOTE - Technically, sum() is happy to take any iterable/sequence over objects
;;        which implement __add__ for one another except for strings. However,
;;        problems can crop up, so we restrict it to integers here.
(ag/one-arg sum
            #:type (fresh-summable-type)
            #:ctype (λ (n t)
                      (hash 'Expression (fresh-iterable-or-sequence-or-array t))))
;; TODO - super()
;; NOTE - tuple() is handled in the zero-cost converters.
(ag/one-arg type
            #:shallow #t
            #:type type-type
            #:ctype (Ectype (fresh-type-variable)))
;; TODO - vars()
;; NOTE - zip() can also take zero arguments, but we omit this for interest.
(ag/one-arg zip
            #:racr-name ZipOne
            #:type (fresh-immutable-iterator (product-type (list (fresh-type-variable))))
            #:ctype (λ (n t)
                      (define return-elem (fresh-type-variable))
                      (define return-iterator (fresh-immutable-iterator (product-type (list return-elem))))
                      (unify! t return-iterator)
                      (define arg-iterable (fresh-iterable return-elem))
                      (hash 'Expression arg-iterable)))
(ag/two-arg zip
            #:racr-name ZipTwo
            #:type (fresh-immutable-iterator (product-type (list (fresh-type-variable)
                                                                 (fresh-type-variable))))
            #:ctype (λ (n t)
                      (define return-elem1 (fresh-type-variable))
                      (define return-elem2 (fresh-type-variable))
                      (define return-iterator (fresh-immutable-iterator (product-type (list return-elem1
                                                                                            return-elem2))))
                      (unify! t return-iterator)
                      (define arg-iterable1 (fresh-iterable return-elem1))
                      (define arg-iterable2 (fresh-iterable return-elem2))
                      (hash 'l arg-iterable1
                            'r arg-iterable2)))
(ag/three-arg zip
              #:racr-name ZipThree
              #:type (fresh-immutable-iterator (product-type (list (fresh-type-variable)
                                                                   (fresh-type-variable)
                                                                   (fresh-type-variable))))
              #:ctype (λ (n t)
                        (define return-elem1 (fresh-type-variable))
                        (define return-elem2 (fresh-type-variable))
                        (define return-elem3 (fresh-type-variable))
                        (define return-iterator (fresh-immutable-iterator (product-type (list return-elem1
                                                                                              return-elem2
                                                                                              return-elem3))))
                        (unify! t return-iterator)
                        (define arg-iterable1 (fresh-iterable return-elem1))
                        (define arg-iterable2 (fresh-iterable return-elem2))
                        (define arg-iterable3 (fresh-iterable return-elem3))
                        (hash 'l arg-iterable1
                              'm arg-iterable2
                              'r arg-iterable3)))

;;;;;;
;; NOTE: methods on instances of `str`, documented at: https://docs.python.org/3/library/stdtypes.html#string-methods

;; Concatenates documents *ds*, inserting *d-sep* between each pair of documents.
(define (h-intercalate ds d-sep)
  (h-concat
   (add-between
    ds
    d-sep)))

(define-for-syntax (racr-ize-str-method-id id suffix)
  (datum->syntax id
                 (string->symbol
                  (string-append
                   "StrMethod"
                   (string-titlecase (symbol->string (syntax->datum id)))
                   suffix))))

(define (render-str-method-node n name . args)
  (h-append ($xsmith_render-node (ast-child 'str n))
            dot
            (text name)
            lparen
            (h-intercalate args
                           (text ", "))
            rparen))

(define-syntax-parser ag/str-method/zero-arg
  [(_ name:id
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'string-type]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (racr-ize-str-method-id #'name "Zero")])))
      ...)
   #'(ag [racr-name Expression ([str : Expression])
                    #:prop type-info
                    [type
                     (λ (n t)
                       (hash 'str string-type))]
                    #:prop render-node-info
                    (λ (n)
                      (render-str-method-node n (symbol->string 'name)))])])

(define-syntax-parser ag/str-method/one-arg
  [(_ name:id
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'string-type]))
           (~optional (~seq #:ctype ctype:expr)
                      #:defaults ([ctype #'(λ (n t) (hash 'str string-type 'arg string-type))]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (racr-ize-str-method-id #'name "One")])))
      ...)
   #'(ag [racr-name Expression ([str : Expression]
                                [arg : Expression])
                    #:prop type-info
                    [type ctype]
                    #:prop render-node-info
                    (λ (n)
                      (render-str-method-node n
                                              (symbol->string 'name)
                                              ($xsmith_render-node (ast-child 'arg n))))])])
(define-syntax-parser SM1ctype
  [(_ etype-arg:expr)
   #'(λ (n t) (hash 'str string-type
                    'arg etype-arg))])

(define-syntax-parser ag/str-method/two-arg
  [(_ name:id
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'string-type]))
           (~optional (~seq #:ctype ctype:expr)
                      #:defaults ([ctype #'(λ (n t) (hash 'str string-type 'argOne string-type 'argTwo string-type))]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (racr-ize-str-method-id #'name "Two")])))
      ...)
   #'(ag [racr-name Expression ([str : Expression]
                                [argOne : Expression]
                                [argTwo : Expression])
                    #:prop type-info
                    [type ctype]
                    #:prop render-node-info
                    (λ (n)
                      (render-str-method-node n
                                              (symbol->string 'name)
                                              ($xsmith_render-node (ast-child 'argOne n))
                                              ($xsmith_render-node (ast-child 'argTwo n))))])])
(define-syntax-parser SM2ctype
  [(_ etype-arg1:expr etype-arg2:expr)
   #'(λ (n t) (hash 'str string-type
                    'argOne etype-arg1
                    'argTwo etype-arg2))])

(define-syntax-parser ag/str-method/three-arg
  [(_ name:id
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'string-type]))
           (~optional (~seq #:ctype ctype:expr)
                      #:defaults ([ctype #'(λ (n t) (hash 'str string-type
                                                          'argOne string-type
                                                          'argTwo string-type
                                                          'argThree string-type))]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (racr-ize-str-method-id #'name "Three")])))
      ...)
   #'(ag [racr-name Expression ([str : Expression]
                                [argOne : Expression]
                                [argTwo : Expression]
                                [argThree : Expression])
                    #:prop type-info
                    [type ctype]
                    #:prop render-node-info
                    (λ (n)
                      (render-str-method-node n
                                              (symbol->string 'name)
                                              ($xsmith_render-node (ast-child 'argOne n))
                                              ($xsmith_render-node (ast-child 'argTwo n))
                                              ($xsmith_render-node (ast-child 'argThree n))))])])
(define-syntax-parser SM3ctype
  [(_ etype-arg1:expr etype-arg2:expr etype-arg3:expr)
   #'(λ (n t) (hash 'str string-type
                    'argOne etype-arg1
                    'argTwo etype-arg2
                    'argThree etype-arg3))])

;; NOTE - capitalize - changed in 3.8
(ag/str-method/zero-arg capitalize)
(ag/str-method/zero-arg casefold)
(ag/str-method/one-arg center #:ctype (SM1ctype string-safe-int-type))
(ag/str-method/two-arg center #:ctype (SM2ctype string-safe-int-type char-type))
(ag/str-method/one-arg count #:type int-type)
(ag/str-method/two-arg count #:type int-type #:ctype (SM2ctype string-type int-type))
(ag/str-method/three-arg count #:type int-type #:ctype (SM3ctype string-type int-type int-type))
;; NOTE - These encodings are enumerated at: https://docs.python.org/3/library/codecs.html#standard-encodings
;;        This list includes only each main codec name and none of the aliases.
(define encoding-codecs
  (list "ascii"
        "big5" "big5hkscs"
        "cp037" "cp273" "cp424" "cp437" "cp500" "cp720" "cp737" "cp775"
        "cp850" "cp852" "cp855" "cp856" "cp857" "cp858" "cp860" "cp861"
        "cp862" "cp863" "cp864" "cp865" "cp866" "cp869" "cp874" "cp875"
        "cp932" "cp949" "cp950" "cp1006" "cp1026" "cp1125" "cp1140" "cp1250"
        "cp1251" "cp1252" "cp1253" "cp1254" "cp1255" "cp1256" "cp1257" "cp1258"
        "euc_jp" "euc_jis_2004" "euc_jisx0213" "euc_kr"
        "gb2312" "gbk" "gb18030"
        "hz"
        "iso2022_jp" "iso2022_jp_1" "iso2022_jp_2" "iso2022_jp_2004"
        "iso2022_jp_3" "iso2022_jp_ext" "iso2022_kr"
        "latin_1"
        "iso8859_2" "iso8859_3" "iso8859_4" "iso8859_5" "iso8859_6" "iso8859_7"
        "iso8859_8" "iso8859_9" "iso8859_10" "iso8859_11"
        "iso8859_13" "iso8859_14" "iso8859_15" "iso8859_16"
        "johab"
        "koi8_r" "koi8_t" "koi8_u"
        "kz1048"
        "mac_cyrillic" "mac_greek" "mac_iceland" "mac_latin2" "mac_roman" "mac_turkish"
        "ptcp154"
        "shitf_jis" "shift_jis_2004" "shift_jisx0213"
        "utf_32" "utf_32_be" "utf_32_le"
        "utf_16" "utf_16_be" "utf_16_le"
        "utf_7" "utf_8" "utf_8_sig"))
;; We define a smaller set of codecs for default use, and use the large set only when it is enabled via a
;; command-line feature switch.
(define simple-encoding-codecs
  (list "ascii" "utf_16" "utf_8"))
;; We define a single-element set of codecs for use as needed.
(define ascii-encoding-codec
  (list "ascii"))
;; NOTE - These error handling mechanisms are enumerated under: https://docs.python.org/3/library/codecs.html#codecs.register_error
;;        Aside from 'strict' (which is accordingly commented out), they should prevent runtime errors.
;; TODO - Wrapping this in a custom function in the header could accommodate implementing 'strict' and
;;        simply checking for errors, but I don't know if that's worth bothering with since any such
;;        error should be handled adequately by the error-avoiding mechanisms enabled here.
(define encoding-error-handling-schemes
  (list #;"strict"
        "ignore"
        "replace"
        "xmlcharrefreplace"
        "backslashreplace"
        "namereplace"))
(define decoding-error-handling-schemes
  (list #;"strict"
        "ignore"
        "replace"
        "backslashreplace"))
(add-to-grammar
 python-comp
 ;; NOTE - While we provide a version of `encode` that uses the default encoding, we do not use the default
 ;;        error handling option of 'strict'. This is to avoid runtime exceptions.
 [StrMethodEncodeOne Expression
                     ([str : Expression]
                      [errors = (random-ref encoding-error-handling-schemes)])
                     #:prop type-info
                     [byte-string-type
                      (λ (n t)
                        (hash 'str string-type))]
                     #:prop render-node-info
                     (λ (n)
                       (h-append ($xsmith_render-node (ast-child 'str n))
                                 dot
                                 (text "encode")
                                 lparen
                                 (text "errors=") squote (text (ast-child 'errors n)) squote
                                 rparen))]
 [StrMethodEncodeTwo Expression
                     ([str : Expression]
                      [encoding = (random-ref (cond
                                                [(xsmith-feature-enabled? 'simple-encoding)
                                                 simple-encoding-codecs]
                                                [(xsmith-feature-enabled? 'advanced-encoding)
                                                 encoding-codecs]
                                                [else
                                                 ascii-encoding-codec]))]
                      [errors = (random-ref encoding-error-handling-schemes)])
                     #:prop type-info
                     [byte-string-type
                      (λ (n t)
                        (hash 'str string-type))]
                     #:prop render-node-info
                     (λ (n)
                       (render-str-method-node n
                                               "encode"
                                               (h-append squote (text (ast-child 'encoding n)) squote)
                                               (h-append squote (text (ast-child 'errors n)) squote)))]
 ;; We also provide the byte string decoding methods implementations here since
 ;; they are the dual of the string encoding methods.
 [BytesMethodDecodeOne Expression
                       ([bts : Expression]
                        [errors = (random-ref decoding-error-handling-schemes)])
                       #:prop type-info
                       [string-type
                        (λ (n t)
                          (hash 'bts byte-string-type))]
                       #:prop render-node-info
                       (λ (n)
                         (h-append ($xsmith_render-node (ast-child 'bts n))
                                   dot
                                   (text "decode")
                                   lparen
                                   (text "errors=") squote (text (ast-child 'errors n)) squote
                                   rparen))]
 [BytesMethodDecodeTwo Expression
                       ([bts : Expression]
                        [encoding = (random-ref (cond
                                                  [(xsmith-feature-enabled? 'simple-encoding)
                                                   simple-encoding-codecs]
                                                  [(xsmith-feature-enabled? 'advanced-encoding)
                                                   encoding-codecs]
                                                  [else
                                                   ascii-encoding-codec]))]
                        [errors = (random-ref decoding-error-handling-schemes)])
                       #:prop type-info
                       [string-type
                        (λ (n t)
                          (hash 'bts byte-string-type))]
                       #:prop render-node-info
                       (λ (n)
                         (h-append ($xsmith_render-node (ast-child 'bts n))
                                   dot
                                   (text "decode")
                                   lparen
                                   squote (text (ast-child 'encoding n)) squote
                                   comma space
                                   squote (text (ast-child 'errors n)) squote
                                   rparen))])
(ag/str-method/one-arg endswith #:type bool-type)
(ag/str-method/two-arg endswith #:type bool-type #:ctype (SM2ctype string-type int-type))
(ag/str-method/three-arg endswith #:type bool-type #:ctype (SM3ctype string-type int-type int-type))
(ag/str-method/zero-arg expandtabs)
(ag/str-method/one-arg expandtabs #:ctype (SM1ctype overflow-safe-int-type))
(ag/str-method/one-arg find #:type int-type)
(ag/str-method/two-arg find #:type int-type #:ctype (SM2ctype string-type int-type))
(ag/str-method/three-arg find #:type int-type #:ctype (SM3ctype string-type int-type int-type))
;; TODO - format()  ;; XXX - changed in 3.7
;; TODO - format_map()
;; TODO - index()  ;; XXX - like str.find, but raises ValueError when substring not found
(ag/str-method/zero-arg isalnum #:type bool-type)
(ag/str-method/zero-arg isalpha #:type bool-type)
;; TODO - isascii()  ;; XXX - new in 3.7
(ag/str-method/zero-arg isdecimal #:type bool-type)
(ag/str-method/zero-arg isdigit #:type bool-type)
(ag/str-method/zero-arg isidentifier #:type bool-type)
(ag/str-method/zero-arg islower #:type bool-type)
(ag/str-method/zero-arg isnumeric #:type bool-type)
(ag/str-method/zero-arg isprintable #:type bool-type)
(ag/str-method/zero-arg isspace #:type bool-type)
(ag/str-method/zero-arg istitle #:type bool-type)
(ag/str-method/zero-arg isupper #:type bool-type)
(ag/str-method/one-arg join #:ctype (SM1ctype (fresh-iterable string-type)))
(ag/str-method/one-arg ljust #:ctype (SM1ctype string-safe-int-type))
(ag/str-method/two-arg ljust #:ctype (SM2ctype string-safe-int-type char-type))
(ag/str-method/zero-arg lower)
(ag/str-method/zero-arg lstrip)
(ag/str-method/one-arg lstrip)
;; TODO - maketrans()  ;; XXX - static
(ag/str-method/one-arg partition #:type (product-type (list string-type string-type string-type)))
;; TODO - removeprefix()  ;; XXX - new in 3.9
;; TODO - removesuffix()  ;; XXX - new in 3.9
(ag/str-method/two-arg replace)
(ag/str-method/three-arg replace #:ctype (SM3ctype string-type string-type overflow-safe-ssize_t-type))
(ag/str-method/one-arg rfind #:type int-type)
(ag/str-method/two-arg rfind #:type int-type #:ctype (SM2ctype string-type int-type))
(ag/str-method/three-arg rfind #:type int-type #:ctype (SM3ctype string-type int-type int-type))
;; TODO - rindex()  ;; XXX - like str.rfind, but raises ValueError when substring not found
(ag/str-method/one-arg rjust #:ctype (SM1ctype string-safe-int-type))
(ag/str-method/two-arg rjust #:ctype (SM2ctype string-safe-int-type char-type))
(ag/str-method/one-arg rpartition #:type (product-type (list string-type string-type string-type)))
(ag/str-method/zero-arg rsplit #:type (fresh-iterable string-type))
(ag/str-method/one-arg rsplit #:type (fresh-iterable string-type))
(ag/str-method/two-arg rsplit #:type (fresh-iterable string-type) #:ctype (SM2ctype string-type overflow-safe-ssize_t-type))
(ag/str-method/zero-arg rstrip)
(ag/str-method/one-arg rstrip)
(ag/str-method/zero-arg split #:type (fresh-iterable string-type))
(ag/str-method/one-arg split #:type (fresh-iterable string-type))
(ag/str-method/two-arg split #:type (fresh-iterable string-type) #:ctype (SM2ctype string-type overflow-safe-ssize_t-type))
(ag/str-method/zero-arg splitlines #:type (fresh-iterable string-type))
(ag/str-method/one-arg splitlines #:type (fresh-iterable string-type) #:ctype (SM1ctype bool-type))
(ag/str-method/one-arg startswith #:type bool-type)
(ag/str-method/two-arg startswith #:type bool-type #:ctype (SM2ctype string-type int-type))
(ag/str-method/three-arg startswith #:type bool-type #:ctype (SM3ctype string-type int-type int-type))
(ag/str-method/zero-arg strip)
(ag/str-method/one-arg strip)
(ag/str-method/zero-arg swapcase)
(ag/str-method/zero-arg title)
;; TODO - translate()
(ag/str-method/zero-arg upper)
(ag/str-method/one-arg zfill #:ctype (SM1ctype string-safe-int-type))

(define header-definitions-block
  (string-append
   "
from inspect import signature

FAKEBLOCK = True


def safe_divide(a, b):
    return a if (b == 0) else (a / b)


def safe_int_divide(a, b):
    return a if (b == 0) else (a // b)


def NE_chr(x):
    return chr(abs(x) % 0x10FFFF)


def NE_max(seq, fallback):
    if 0 == len(seq):
        return fallback
    else:
        return max(seq)


def NE_min(seq, fallback):
    if 0 == len(seq):
        return fallback
    else:
        return min(seq)


def NE_divmod(x, y):
    if 0 == y:
        return (x, y)
    else:
        return divmod(x, y)


def NE_next(iterator, default_value):
    try:
        return next(iterator)
    except:
        return default_value


def NE_safe_int(x):
    if x < 0:
        " (format "return -(x % ~a)" overflow-safe-int-min-value) "
    else:
        " (format "return x % ~a" overflow-safe-int-max-value) "


def NE_safe_ssize_t(x):
    if x < 0:
        " (format "return -(x % ~a)" overflow-safe-ssize_t-min-value) "
    else:
        " (format "return x % ~a" overflow-safe-ssize_t-max-value) "


def NE_safe_string_int(x):
     " (format "return x % ~a" string-safe-int-max-value) "


def bound(value, lo, hi):
    diff = hi - lo
    return (((value - lo) % diff) + lo)


def reasonable_range(*args):
    if len(args) == 1:
        (hi, ) = args
        if hi < 0:
            " (format "hi = -(hi % ~a)" reasonable-range-max-value) "
        else:
            " (format "hi %= ~a" reasonable-range-max-value) "
        return range(hi)
    elif len(args) == 2:
        (lo, hi) = args
        " (format "if (hi - lo) > ~a:" reasonable-range-max-value) "
            midpoint = (hi + lo) // 2
            " (format "lo_bound = midpoint - ~a" reasonable-range-half-value) "
            " (format "hi_bound = midpoint + ~a" reasonable-range-half-value) "
            lo = bound(lo, lo_bound, hi_bound)
            hi = bound(hi, lo_bound, hi_bound)
        return range(lo, hi)
    elif len(args) == 3:
        (lo, hi, step) = args
        r = reasonable_range(lo, hi)
        return range(r.start, r.stop, step)
    else:
        return range(pow(2, 10))


def list_safe_reference(array, index, fallback):
    if not (len(array) == 0):
        return array[index % len(array)]
    else:
        return fallback


def list_safe_assignment(array, index, newvalue):
    if not (len(array) == 0):
        array[index % len(array)] = newvalue


def dict_safe_reference_by_index(dict, index, fallback):
    if not (len(dict) == 0):
        return dict[list(dict.keys())[index % len(dict.keys())]]
    else:
        return fallback


def dict_safe_assignment_by_index(dict, index, newvalue):
    if not (len(dict) == 0):
        dict[list(dict.keys())[index % len(dict.keys())]] = newvalue


def is_iterator(x):
    return hasattr(x, '__next__')


def is_iterable(x):
    return hasattr(x, '__iter__')


def is_sequence(x):
    return hasattr(x, '__getitem__') and hasattr(x, '__len__')


def to_string(x):
    if any(map(lambda t: isinstance(x, t), (bool, int, float, complex, bytes, bytearray, memoryview))):
        # Primitive types with known-good `__repr__` implementations.
        return repr(x)
    elif isinstance(x, str):
        # Strings can be problematic due to Unicode stuff.
        try:
            output = x.encode('ascii').decode('ascii')
        except UnicodeEncodeError as _:
            output = repr([ord(c) for c in x])
        return output
    elif isinstance(x, tuple):
        return '(' + ', '.join(map(to_string, x)) + ')'
    elif isinstance(x, list):
        return '[' + ', '.join(map(to_string, x)) + ']'
    elif isinstance(x, dict):
        return '{' + ', '.join(': '.join(map(to_string, pair)) for pair in x.items()) + '}'
    elif callable(x):
        func_name = getattr(x, '__name__', '#NO_NAME#')
        parameters = tuple(sorted(signature(x).parameters.keys()))
        return '#<FUNCTION: ' + func_name + repr(parameters) + '>'
    elif is_iterator(x):
        return '#<ITERATOR: ' + getattr(type(x), '__name__', '#ITERATOR') + '>'
    elif is_iterable(x):
        return '#<ITERABLE: ' + getattr(type(x), '__name__', '#ITERABLE') + '>'
    elif is_sequence(x):
        return '#<SEQUENCE: ' + getattr(type(x), '__name__', '#SEQUENCE') + '>'
    else:
        return repr(x)"))

;;;; Render nodes from add-basic-statements/expressions
(add-property
 python-comp
 render-node-info

 [ProgramWithBlock
  (λ (n)
    (define definitions (ast-children (ast-child 'definitions n)))
    (v-append
     (text header-definitions-block)
     (text "\n\n\n\n")
     (text "###### Randomly generated program starts here ######")
     (text "\n\n\n\n")
     (vb-concat
      (list*
       (text "")
       (text "")
       (map (λ (cn) ($xsmith_render-node cn))
            (append definitions
                    (list (ast-child 'Block n))))))
     (text "")
     (apply v-append
            (map (λ (v) (text (format "print(to_string(~a))\n"
                                      (ast-child 'name v))))
                 definitions
                 #;(filter (λ (x) (base-type? (ast-child 'type x)))
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
     (h-append (text "if") space lparen ($xsmith_render-node (ast-child 'test n)) rparen colon)
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
 [Not (λ (n) (h-append lparen (text "not") lparen
                       ($xsmith_render-node (ast-child 'Expression n))
                       rparen rparen))]
 [And (binary-op-renderer (text "and"))]
 [Or (binary-op-renderer (text "or"))]

 [IntLiteral (λ (n) (text (format "~a" (ast-child 'v n))))]
 [Plus (binary-op-renderer (text "+"))]
 [Minus (binary-op-renderer (text "-"))]
 [Times (binary-op-renderer (text "*"))]
 [LessThan (binary-op-renderer (text "<"))]
 [GreaterThan (binary-op-renderer (text ">"))]

 [SafeIntDivide (λ (n) (h-append (text "safe_int_divide") lparen
                                 ($xsmith_render-node (ast-child 'l n))
                                 (text ",") space
                                 ($xsmith_render-node (ast-child 'r n))
                                 rparen))]
 [SafeDivide (λ (n) (h-append (text "safe_divide") lparen
                              ($xsmith_render-node (ast-child 'l n))
                              (text ",") space
                              ($xsmith_render-node (ast-child 'r n))
                              rparen))]

 [StringLiteral (λ (n) (text (python-string-format (ast-child 'v n))))]
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
    (h-append (text "list_safe_reference(")
              ($xsmith_render-node (ast-child 'array n))
              (text ", ")
              ($xsmith_render-node (ast-child 'index n))
              (text ", ")
              ($xsmith_render-node (ast-child 'fallback n))
              (text ")")))]
 [MutableArraySafeAssignmentStatement
  (λ (n)
    (h-append (text "list_safe_assignment(")
              ($xsmith_render-node (ast-child 'array n))
              (text ", ")
              ($xsmith_render-node (ast-child 'index n))
              (text ", ")
              ($xsmith_render-node (ast-child 'newvalue n))
              (text ")")))]

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
 [MutableDictionarySafeReferenceByKey
  (λ (n)
    (h-append ($xsmith_render-node (ast-child 'dictionary n))
              dot
              (text "get")
              lparen
              ($xsmith_render-node (ast-child 'key n))
              comma
              space
              ($xsmith_render-node (ast-child 'fallback n))
              rparen))]
 [MutableDictionarySafeReferenceByIndex
  (λ (n)
    (h-append (text "dict_safe_reference_by_index(")
              ($xsmith_render-node (ast-child 'dictionary n))
              (text ", ")
              ($xsmith_render-node (ast-child 'index n))
              (text ", ")
              ($xsmith_render-node (ast-child 'fallback n))
              (text ")")))]
 [MutableDictionarySafeAssignmentByKeyStatement
  (λ (n)
    (h-append ($xsmith_render-node (ast-child 'dictionary n))
              (text "[")
              ($xsmith_render-node (ast-child 'key n))
              (text "]")
              (text " = ")
              ($xsmith_render-node (ast-child 'newvalue n))))]
 [MutableDictionarySafeAssignmentByIndexStatement
  (λ (n)
    (h-append (text "dict_safe_assignment_by_index(")
              ($xsmith_render-node (ast-child 'dictionary n))
              (text ", ")
              ($xsmith_render-node (ast-child 'index n))
              (text ", ")
              ($xsmith_render-node (ast-child 'newvalue n))
              (text ")")))]

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
   (λ()(immutable (iterator-type (fresh-type-variable))))
   (λ()(mutable (array-type (fresh-type-variable))))
   (λ()(mutable (dictionary-type (dictionary-key-type) (dictionary-value-type))))
   (λ()(mutable (fresh-structural-record-type)))
   ))

(define (python-format-render doc)
  (pretty-format doc 120))


(define-xsmith-interface-functions
  [python-comp]
  #:fuzzer-name python3
  #:fuzzer-version xsmith-examples-version-string/no-name
  #:type-thunks type-thunks-for-concretization
  #:program-node ProgramWithBlock
  #:format-render python-format-render
  #:comment-wrap (λ (lines) (string-join (map (λ (l) (format "# ~a" l)) lines)
                                         "\n"))
  #:features ([advanced-encoding #f]
              [simple-encoding #f]))

(module+ main (python3-command-line))
