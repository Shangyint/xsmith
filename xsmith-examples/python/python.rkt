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
 "../private/xsmith-examples-version.rkt"
 (for-syntax
  racket/base
  syntax/parse
  ))

(define-basic-spec-component python-comp)

;; TODO - types - add characters, sets, iterables, what else?

(define number-type (base-type 'number #:leaf? #f))
(define real-type (base-type 'real number-type #:leaf? #f))
(define int-type (base-type 'int real-type))
(define float-type (base-type 'float real-type))

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
(define-generic-type sequence-type ([type covariant]))
(define-generic-type iterable-type ([type covariant]))
(define (fresh-iterable inner)
  (fresh-type-variable
   (mutable (iterable-type inner))
   (immutable (iterable-type inner))))
(define (fresh-sequence inner)
  (fresh-type-variable
   (mutable (sequence-type inner))
   (immutable (sequence-type inner))))
;;
;; NOTE: These are just some notes about the strange lazy iterable-esque data
;;       types in Python. Most of this is written in a pseudo-Haskell notation.
;;
;; iterable:
;;   - __iter__()
;; iterator:
;;   - __iter__()
;;   - __next__()
;; sequence:
;;   - __getitem__()
;; mutable sequence:
;;   - list, bytearray
;; immutable sequence:
;;   - tuple, range, str, bytes
;;   - everything in mutable sequences
;;   - __hash__() (if sub-elements are hashable)
;;
;; all       ::                  Either (Iterable a, Sequence a) -> Bool
;; any       ::                  Either (Iterable a, Sequence a) -> Bool
;; dict      ::                  Maybe (Either Iterable (a, b), Sequence (a, b)) -> Dictionary a b
;; enumerate ::                  Either (Iterable a, Sequence a) -> Iterator (Int, a)
;; filter    :: Boolifiable b => (a -> b) -> Either (Iterable a, Sequence a) -> Iterator a
;; hash      :: Hashable a    => a -> Int
;; iter      ::                  Either (Iterable a, Sequence a) -> Iterator a
;; len       ::                  Lengthy a -> Int
;; list      ::                  Maybe (Either (Iterable a, Sequence a)) -> List a
;; map       ::                  (a -> b) -> Either (Iterable a, Sequence a) -> Iterator b
;; max       :: Comparable a  => Either (Iterable a, Sequence a) -> a
;; min       :: Comparable a  => Either (Iterable a, Sequence a) -> a
;; next      ::                  Iterator a -> a
;; range     ::                  Int -> Maybe Int -> Maybe Int -> Immutable Sequence Int
;; reversed  ::                  Either (Reversible a, Sequence a) -> Iterator a
;; set       :: Hashable a    => Either (Iterable a, Sequence a) -> Set a
;; sorted    :: Comparable a  => Either (Iterable a, Sequence a) -> List a
;; sum       :: Summable a    => Either (Iterable a, Sequence a) -> a
;; tuple     ::                  Either (Iterable a, Sequence a) -> Immutable Sequence a
;; zip       ::                  Either (Iterable a, Sequence a) -> Either (Iterable b, Sequence b) -> Iterator (a, b)
;;
;; class Iterable a where
;;     __iter__ :: () -> Iterator a
;; class Iterable a => Iterator a where
;;     __next__ :: () -> Maybe a
;; class Sequence a where
;;     __getitem__ :: Int -> a  ;; NOTE: the index is most often an Int, though not always
;; class Hashable a where
;;     __hash__ :: a -> Int
;; class Boolifiable a where
;;     __bool__ :: a -> Bool
;; class Comparable a where
;;     __lt__ :: a -> a -> Bool
;; class Summable a where
;;     __add__ :: a -> a -> a
;;
;;
;;
(define (fresh-comparable-type)
  ;; TODO - lots of things are comparable (eg. lt/gt work on them) in python,
  ;; but I'm not sure which ones I should include for fuzzing.
  (fresh-type-variable real-type
                       string-type
                       byte-string-type
                       char-type
                       bool-type
                       ))

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

(define (biased-random-string)
  (match (random 3)
    [0 (random-string)]
    [1 (random-string-from-char-producing-proc
        (λ () (random-char-in-range (range 0 128))))]
    [2 (random-string-from-char-producing-proc biased-random-char)]))

(define (biased-random-char)
  ;; Random-char very rarely generates ascii, which is more common.
  ;; More saliently, low-value characters are interned in Racket and
  ;; high-value characters are not.  So I want to be sure to generate
  ;; both to have some variety.
  (if (random-bool)
      (random-char)
      (random-char-in-range (range 0 128))))

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
(define pass-through-render (λ (n) ($xsmith_render-node (ast-child 'Expression n))))

(add-to-grammar
 python-comp
 [CharLiteral Expression ([v = (random-char)])
              #:prop type-info [char-type no-child-types]
              #:prop choice-weight 1
              #:prop render-node-info
              (λ (n) (text (python-string-format (string (ast-child 'v n)))))]
 [StringToSequence Expression (Expression)
                   #:prop depth-increase 0
                   #:prop wont-over-deepen #t
                   #:prop type-info [(immutable (sequence-type char-type))
                                     (λ (n t) (hash 'Expression string-type))]
                   #:prop render-node-info pass-through-render]
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
                       #:prop render-node-info pass-through-render]
 [ByteStringLiteral Expression ([v = (random-byte-string)])
                    #:prop type-info [byte-string-type no-child-types]
                    #:prop choice-weight 1
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
               #:prop choice-weight 1
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

 [ISequenceToIterable Expression ([Expression])
                      #:prop depth-increase 0
                      #:prop wont-over-deepen #t
                      #:prop type-info
                      [(immutable (iterable-type (fresh-type-variable)))
                       (λ (n t)
                         (define inner (fresh-type-variable))
                         (unify! t (immutable (iterable-type inner)))
                         (hash 'Expression (immutable (sequence-type inner))))]
                      #:prop render-node-info pass-through-render]
 [MSequenceToIterable Expression ([Expression])
                      #:prop depth-increase 0
                      #:prop wont-over-deepen #t
                      #:prop type-info
                      [(mutable (iterable-type (fresh-type-variable)))
                       (λ (n t)
                         (define inner (fresh-type-variable))
                         (unify! t (mutable (iterable-type inner)))
                         (hash 'Expression (mutable (sequence-type inner))))]
                      #:prop render-node-info pass-through-render]
 [IterableToImmutableSequence Expression (Expression)
                              #:prop depth-increase 0
                              #:prop wont-over-deepen #t
                              #:prop type-info
                              [(immutable (sequence-type (fresh-type-variable)))
                               (λ (n t)
                                 (define inner (fresh-type-variable))
                                 (unify! t (immutable (sequence-type inner)))
                                 (hash 'Expression (fresh-iterable inner)))]
                              #:prop render-node-info
                              (λ (n) (h-append
                                      (text "tuple(")
                                      ($xsmith_render-node (ast-child 'Expression n))
                                      (text ")")))]
 [SequenceToImmutableSequence Expression (Expression)
                              #:prop depth-increase 0
                              #:prop wont-over-deepen #t
                              #:prop type-info
                              [(immutable (sequence-type (fresh-type-variable)))
                               (λ (n t)
                                 (define inner (fresh-type-variable))
                                 (unify! t (immutable (sequence-type inner)))
                                 (hash 'Expression (fresh-sequence inner)))]
                              #:prop render-node-info
                              (λ (n) (h-append
                                      (text "tuple(")
                                      ($xsmith_render-node (ast-child 'Expression n))
                                      (text ")")))]
 [IterableToMutableSequence Expression (Expression)
                            #:prop depth-increase 0
                            #:prop wont-over-deepen #t
                            #:prop type-info
                            [(mutable (sequence-type (fresh-type-variable)))
                             (λ (n t)
                               (define inner (fresh-type-variable))
                               (unify! t (mutable (sequence-type inner)))
                               (hash 'Expression (fresh-iterable inner)))]
                            #:prop render-node-info
                            (λ (n) (h-append
                                    (text "list(")
                                    ($xsmith_render-node (ast-child 'Expression n))
                                    (text ")")))]
 [MutableArrayToSequence Expression (Expression)
                         #:prop depth-increase 0
                         #:prop wont-over-deepen #t
                         #:prop type-info
                         [(mutable (sequence-type (fresh-type-variable)))
                          (λ (n t)
                            (define inner (fresh-type-variable))
                            (unify! t (mutable (sequence-type inner)))
                            (hash 'Expression (mutable (array-type inner))))]
                         #:prop render-node-info pass-through-render]
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

;; Numbers.  The canned-components numbers aren't quite right if we allow complex numbers.
(add-to-grammar
 python-comp
 [NumberLiteral Expression (v)
                #:prop may-be-generated #f
                #:prop choice-weight 1]
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
(define-syntax-parser ag [(_ arg ...) #'(add-to-grammar python-comp arg ...)])
(define-syntax-parser ap [(_ arg ...) #'(add-property python-comp arg ...)])

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

(define-syntax-parser ag/one-arg
  [(_ name:id
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'(fresh-subtype-of number-type)]))
           (~optional (~seq #:ctype ctype:expr)
                      #:defaults ([ctype #'(λ (n t) (hash 'Expression t))]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (racr-ize-id #'name)]))
           (~optional (~seq #:NE-name NE-name)
                      #:defaults ([NE-name #'name])))
      ...)
   #'(ag [racr-name Expression (Expression)
                    #:prop type-info [type ctype]
                    #:prop render-node-info
                    (λ (n)
                      (define name (symbol->string (if NE? 'NE-name 'name)))
                      (h-append (text name)
                                (text "(")
                                ($xsmith_render-node (ast-child 'Expression n))
                                (text ")")))])])
(define-syntax-parser Ectype
  [(_ etype:expr)
   #'(λ (n t) (hash 'Expression etype))])

(define-syntax-parser ag/two-arg
  [(_ name:id
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'(fresh-subtype-of number-type)]))
           (~optional (~seq #:ctype ctype:expr)
                      #:defaults ([ctype #'(λ (n t) (hash 'l t 'r t))]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (racr-ize-id #'name)]))
           (~optional (~seq #:NE-name NE-name)
                      #:defaults ([NE-name #'name])))
      ...)
   #'(ag [racr-name Expression ([l : Expression]
                                [r : Expression])
                    #:prop type-info [type ctype]
                    #:prop render-node-info
                    (λ (n)
                      (define name (symbol->string (if NE? 'NE-name 'name)))
                      (h-append (text name)
                                (text "(")
                                ($xsmith_render-node (ast-child 'l n))
                                (text ", ")
                                ($xsmith_render-node (ast-child 'r n))
                                (text ")")))])])
(define-syntax-parser E2ctype
  [(_ etypel:expr etyper:expr)
   #'(λ (n t) (hash 'l etypel 'r etyper))])

(define-syntax-parser ag/three-arg
  [(_ name:id
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'(fresh-subtype-of number-type)]))
           (~optional (~seq #:ctype ctype:expr)
                      #:defaults ([ctype #'(λ (n t) (hash 'l t 'm t 'r t))]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (racr-ize-id #'name)]))
           (~optional (~seq #:NE-name NE-name)
                      #:defaults ([NE-name #'name])))
      ...)
   #'(ag [racr-name Expression ([l : Expression]
                                [m : Expression]
                                [r : Expression])
                    #:prop type-info [type ctype]
                    #:prop render-node-info
                    (λ (n)
                      (define name (symbol->string (if NE? 'NE-name 'name)))
                      (h-append (text name)
                                (text "(")
                                ($xsmith_render-node (ast-child 'l n))
                                (text ", ")
                                ($xsmith_render-node (ast-child 'm n))
                                (text ", ")
                                ($xsmith_render-node (ast-child 'r n))
                                (text ")")))])])
(define-syntax-parser E3ctype
  [(_ etypel:expr etypem:expr etyper:expr)
   #'(λ (n t) (hash 'l etypel 'm etypem 'r etyper))])

;;;; built-in functions from https://docs.python.org/3/library/functions.html
(ag/one-arg abs)
(ag/one-arg all #:type bool-type #:ctype (Ectype (fresh-iterable (fresh-type-variable))))
(ag/one-arg any #:type bool-type #:ctype (Ectype (fresh-iterable (fresh-type-variable))))
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
            #:ctype (Ectype (fresh-type-variable int-type )))
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
            #:ctype (Ectype (fresh-iterable (product-type (list (dictionary-key-type) (dictionary-value-type))))))
;; Zero-arg dir returns a list of names bound in the current scope.
(ag/zero-arg dir
             #:racr-name DirZero
             #:type (fresh-iterable string-type))
;; One-arg dir returns a list of attributes of the passed-in object.
;; TODO - Figure a way to use the returned strings in, e.g., `getattr`, `delattr`.
(ag/one-arg dir
            #:racr-name DirOne
            #:type (fresh-iterable string-type)
            #:ctype (Ectype (fresh-type-variable)))
(ag/two-arg divmod #:NE-name NE_divmod
            #:type (product-type (list int-type int-type))
            #:ctype (E2ctype int-type int-type))
;; TODO - enumerate returns an 'enumerate object', which is not reversible but is fairly similar to a list.
;;        Also, their should be a constraint that the returned tuples' second elements are of the same type as the elements of the passed-in iterable.
(ag/one-arg enumerate
            #:type (immutable (fresh-iterable (product-type (list int-type (fresh-type-variable)))))
            #:ctype (λ (n t)
                      ;; The inner type we will tell the child to take.
                      (define inner-type (fresh-type-variable))
                      ;; The inner type as dictated by the parent's type.
                      (define inner-type-to-return
                        ;; Get the list of type variables. The innermost is listed first.
                        ;; TODO - Is the return order of `type->type-variable-list` guaranteed?
                        (car
                         (type->type-variable-list t)))
                      ;; Unify the two types.
                      (unify! inner-type inner-type-to-return)
                      ;; Assign the child's type. It must be an iterable of the chosen type.
                      (hash 'Expression (fresh-iterable inner-type-to-return))))
;; TODO - eval()
;; TODO - exec()
(ag/two-arg filter
            #:type (immutable (iterable-type (fresh-type-variable)))
            #:ctype (λ (n t)
                      (define arg-elem (fresh-type-variable))
                      (define return-array (immutable (iterable-type arg-elem)))
                      (unify! t return-array)
                      (define arg-array (fresh-iterable arg-elem))
                      (hash 'l (function-type (product-type (list arg-elem))
                                              bool-type)
                            'r arg-array)))
;; The float function can actually take a string or an int, but the string has to be a number string...
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
;; TODO - id is commented out because its return value is both implementation- and run-dependent.
;; (ag/one-arg id #:type int-type #:ctype (Ectype (fresh-type-variable)))
;; TODO - __import__()
;; TODO - input()  ;; XXX - don't know if we should implement this, since it waits for external input.
;; TODO - int()
;; TODO - isinstance()
;; TODO - issubclass()
;; TODO - iter()
(ag/one-arg len #:type int-type
            #:ctype (Ectype (fresh-sequence (fresh-type-variable))))
(ag/one-arg list #:type (mutable (array-type (fresh-type-variable)))
            #:ctype (λ (n t)
                      (define inner (fresh-type-variable))
                      (unify! (mutable (array-type inner)) t)
                      (hash 'Expression (fresh-iterable inner))))
;; TODO - locals()

;; Map is actually variadic, but xsmith doesn't really support variadic types.
;; I could define multiple instances, though.
;; TODO - the map interface does not implement the len interface.  So I guess I need to make it a different type.  But out of expedience for now I'm just commenting it out.  Also filter.
(ag/two-arg map
            #:racr-name MapTwo
            #:type (immutable (iterable-type (fresh-type-variable)))
            #:ctype (λ (n t)
                      (define return-elem (fresh-type-variable))
                      (define return-array (immutable (iterable-type return-elem)))
                      (unify! t return-array)
                      (define arg-elem (fresh-type-variable))
                      (define arg-array (fresh-iterable arg-elem))
                      (hash 'l (function-type (product-type (list arg-elem))
                                              return-elem)
                            'r arg-array)))
(ag/three-arg map
              #:racr-name MapThree
              #:type (immutable (iterable-type (fresh-type-variable)))
              #:ctype (λ (n t)
                        (define return-elem (fresh-type-variable))
                        (define return-array (immutable (iterable-type return-elem)))
                        (unify! t return-array)
                        (define arg1-elem (fresh-type-variable))
                        (define arg1-array (fresh-iterable arg1-elem))
                        (define arg2-elem (fresh-type-variable))
                        (define arg2-array (fresh-iterable arg2-elem))
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
;; TODO - next()
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
;; TODO - range()
;; TODO - repr()
;; TODO - a reversed object is not reversible!  What kind of nonsense is that?!
;;        I'm not going to muck up my fuzzer's type system even more right now
;;        for the sake of fuzzing this single wacko python type.
;;        So let's just turn off `reversed` for the time being.
;;        Also the return of `map` is not reversible.  What a load of hogwash.
;(ag/one-arg reversed #:type (immutable (iterable-type (fresh-type-variable))))
(ag/one-arg round #:type int-type #:ctype (Ectype real-type))
;; TODO - set()
;; TODO - setattr()
;; TODO - slice()
(ag/one-arg sorted #:type (immutable (iterable-type (fresh-comparable-type))))
;; TODO - staticmethod()
;; TODO - str()
;; TODO - sum()
;; TODO - super()
;; TODO - tuple()
;; TODO - type()
;; TODO - vars()
;; TODO - zip()

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
(ag/str-method/one-arg center #:ctype (SM1ctype int-type))
(ag/str-method/two-arg center #:ctype (SM2ctype int-type char-type))
(ag/str-method/one-arg count #:type int-type)
(ag/str-method/two-arg count #:type int-type #:ctype (SM2ctype string-type int-type))
(ag/str-method/three-arg count #:type int-type #:ctype (SM3ctype string-type int-type int-type))
;; TODO - encode()  ;; XXX - changed in 3.9
(ag/str-method/one-arg endswith #:type bool-type)
(ag/str-method/two-arg endswith #:type bool-type #:ctype (SM2ctype string-type int-type))
(ag/str-method/three-arg endswith #:type bool-type #:ctype (SM3ctype string-type int-type int-type))
(ag/str-method/zero-arg expandtabs)
(ag/str-method/one-arg expandtabs #:ctype (SM1ctype int-type))
(ag/str-method/one-arg find #:type int-type)
(ag/str-method/two-arg find #:type int-type #:ctype (SM2ctype string-type int-type))
(ag/str-method/three-arg find #:type int-type #:ctype (SM3ctype string-type int-type int-type))
;; TODO - format()  ;; XXX - changed in 3.7
;; TODO - format_map()
;; TODO - index()  ;; XXX - like str.find, but raises ValueError when substring not found
(ag/str-method/zero-arg isalnum)
(ag/str-method/zero-arg isalpha)
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
(ag/str-method/one-arg ljust #:ctype (SM1ctype int-type))
(ag/str-method/two-arg ljust #:ctype (SM2ctype int-type char-type))
(ag/str-method/zero-arg lower)
(ag/str-method/zero-arg lstrip)
(ag/str-method/one-arg lstrip)
;; TODO - maketrans()  ;; XXX - static
(ag/str-method/one-arg partition #:type (product-type (list string-type string-type string-type)))
;; TODO - removeprefix()  ;; XXX - new in 3.9
;; TODO - removesuffix()  ;; XXX - new in 3.9
(ag/str-method/two-arg replace)
(ag/str-method/three-arg replace #:ctype (SM3ctype string-type string-type int-type))
(ag/str-method/one-arg rfind #:type int-type)
(ag/str-method/two-arg rfind #:type int-type #:ctype (SM2ctype string-type int-type))
(ag/str-method/three-arg rfind #:type int-type #:ctype (SM3ctype string-type int-type int-type))
;; TODO - rindex()  ;; XXX - like str.rfind, but raises ValueError when substring not found
(ag/str-method/one-arg rjust #:ctype (SM1ctype int-type))
(ag/str-method/two-arg rjust #:ctype (SM2ctype int-type char-type))
(ag/str-method/one-arg rpartition #:type (product-type (list string-type string-type string-type)))
(ag/str-method/zero-arg rsplit #:type (fresh-iterable string-type))
(ag/str-method/one-arg rsplit #:type (fresh-iterable string-type))
(ag/str-method/two-arg rsplit #:type (fresh-iterable string-type) #:ctype (SM2ctype string-type int-type))
(ag/str-method/zero-arg rstrip)
(ag/str-method/one-arg rstrip)
(ag/str-method/zero-arg split #:type (fresh-iterable string-type))
(ag/str-method/one-arg split #:type (fresh-iterable string-type))
(ag/str-method/two-arg split #:type (fresh-iterable string-type) #:ctype (SM2ctype string-type int-type))
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
(ag/str-method/one-arg zfill #:ctype (SM1ctype int-type))

(define header-definitions-block
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
def to_string(x) -> str:
    if any(map(lambda t: isinstance(x, t), (bool, int, float, complex, str, bytes, bytearray, memoryview))):
        # Primitive types with known-good `__repr__` implementations.
        return repr(x)
    elif isinstance(x, tuple):
        return '(' + ', '.join(map(to_string, x)) + ')'
    elif isinstance(x, list):
        return '[' + ', '.join(map(to_string, x)) + ']'
    elif isinstance(x, dict):
        return '{' + ', '.join(': '.join(map(to_string, pair)) for pair in x.items()) + '}'
    elif callable(x):
        func_name = getattr(x, '__name__', \"#<FUNCTION>\")
        parameters = tuple(sorted(signature(x).parameters.keys()))
        return func_name + repr(parameters)
    else:
        return repr(x)

")

;;;; Render nodes from add-basic-statements/expressions
(add-property
 python-comp
 render-node-info

 [ProgramWithBlock
  (λ (n)
    (define definitions (ast-children (ast-child 'definitions n)))
    (v-append
     (text header-definitions-block)
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
                                         "\n")))

(module+ main (python3-command-line))
