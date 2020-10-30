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
 (for-syntax
  racket/base
  syntax/parse
  ))

(define-basic-spec-component python-comp)

;; TODO - types - add characters, sets, iterables, what else?

(define dictionary-key-type
  (λ () (fresh-type-variable int-type bool-type string-type)))
(define dictionary-value-type
  (λ () (fresh-type-variable int-type bool-type string-type)))
(define byte-string-type (base-type 'btye-string))
(define tuple-max-length 6)
(define-generic-type sequence-type ([type covariant]))
(define (fresh-sequence inner)
  (fresh-type-variable
   (mutable (sequence-type inner))
   (immutable (sequence-type inner))))

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
                       #:Numbers #t
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

(add-to-grammar
 python-comp
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

 [MutableArrayToSequence Expression ([arr : Expression])
                         #:prop depth-increase 0
                         #:prop wont-over-deepen #t
                         #:prop type-info
                         [(mutable (sequence-type (fresh-type-variable)))
                          (λ (n t)
                            (define inner (fresh-type-variable))
                            (unify! (mutable (sequence-type inner)) t)
                            (hash 'arr (mutable (array-type inner))))]
                         #:prop render-node-info
                         (λ (n) ($xsmith_render-node (ast-child 'arr n)))]
 [DictKeys Expression ([dict : Expression])
           #:prop type-info
           ;; I'm not sure about the mutability of this...
           [(immutable (sequence-type (dictionary-key-type)))
            (λ (n t)
              (define kt (dictionary-key-type))
              (unify! (immutable (sequence-type kt)) t)
              (hash 'dict (mutable (dictionary-type kt (dictionary-value-type)))))]
           #:prop render-node-info
           (λ (n) (h-append ($xsmith_render-node (ast-child 'dict n))
                            (text ".keys()")))]
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
(define-syntax-parser ag/single-arg
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
(ag/single-arg abs)
;; TODO - all()
;; TODO - any()
;; TODO - ascii()
(ag/single-arg bin #:type string-type #:ctype (Ectype int-type))
(ag/single-arg bool #:type bool-type #:ctype (Ectype (fresh-type-variable)))
;; TODO - breakpoint()
;; TODO - bytearray()
;; TODO - bytes()
(ag/single-arg callable #:type bool-type #:ctype (Ectype (fresh-type-variable)))
;; TODO - if I add a char type (as strings of length 1), chr should return that type
(ag/single-arg chr #:NE-name NE_chr #:type string-type #:ctype (Ectype int-type))
;; TODO - classmethod()
;; TODO - compile()
;; TODO - complex actually allows floats as args, but I need to expand the numeric tower before I do that.
(ag/two-arg complex #:type number-type #:ctype (E2ctype int-type int-type))
;; TODO - delattr()
;; TODO - dict()
;; TODO - dir()
(ag/two-arg divmod #:NE-name NE_divmod
            #:type (product-type (list int-type int-type))
            #:ctype (E2ctype int-type int-type))
;; TODO - enumerate()
;; TODO - eval()
;; TODO - exec()
(ag/two-arg filter
            #:type (immutable (sequence-type (fresh-type-variable)))
            #:ctype (λ (n t)
                      (define arg-elem (fresh-type-variable))
                      (define return-array (immutable (sequence-type arg-elem)))
                      (unify! t return-array)
                      (define arg-array (fresh-sequence arg-elem))
                      (hash 'l (function-type (product-type (list arg-elem))
                                              bool-type)
                            'r arg-array)))
;; The float function can actually take a string or an int, but the string has to be a number string...
(ag/single-arg float #:type number-type #:ctype (Ectype int-type))
;; TODO - format() -- this will probably be fine if limited to default (empty) format spec and given a value in a limited set of types.  Arbitrary types will raise problems of eg. how function X is printed.
;; TODO - frozenset()
;; TODO - getattr()
;; TODO - globals()
;; TODO - hasattr()
;; TODO - hash()
;; TODO - help()
(ag/single-arg hex #:type string-type #:ctype (Ectype int-type))
;; TODO - id()
;; TODO - __import__()
;; TODO - input()
;; TODO - int()
;; TODO - isinstance()
;; TODO - issubclass()
;; TODO - iter()
(ag/single-arg len #:type int-type
               #:ctype (Ectype (fresh-sequence (fresh-type-variable))))
(ag/single-arg list #:type (mutable (array-type (fresh-type-variable)))
               #:ctype (λ (n t)
                         (define inner (fresh-type-variable))
                         (unify! (mutable (array-type inner)) t)
                         (hash 'seq (fresh-sequence inner))))
;; TODO - locals()

;; Map is actually variadic, but xsmith doesn't really support variadic types.
;; I could define multiple instances, though.
(ag/two-arg map
            #:type (immutable (sequence-type (fresh-type-variable)))
            #:ctype (λ (n t)
                      (define return-elem (fresh-type-variable))
                      (define return-array (immutable (sequence-type return-elem)))
                      (unify! t return-array)
                      (define arg-elem (fresh-type-variable))
                      (define arg-array (fresh-sequence arg-elem))
                      (hash 'l (function-type (product-type (list arg-elem))
                                              return-elem)
                            'r arg-array)))
;; TODO - max()
;; TODO - memoryview()
;; TODO - min()
;; TODO - next()
;; TODO - object()
(ag/single-arg oct #:type string-type #:ctype (Ectype int-type))
;; TODO - open()
;; TODO - if I add a char type (as strings of length 1), chr should accept that type
(ag/single-arg ord #:NE-name NE_ord #:type int-type #:ctype (Ectype string-type))
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
;; TODO - reversed()
;; TODO - round()
;; TODO - set()
;; TODO - setattr()
;; TODO - slice()
;; TODO - sorted()
;; TODO - staticmethod()
;; TODO - str()
;; TODO - sum()
;; TODO - super()
;; TODO - tuple()
;; TODO - type()
;; TODO - vars()
;; TODO - zip()

(define header-definitions-block
  "
FAKEBLOCK = True
def safe_divide(a,b):
  return a if (b == 0) else (a / b)
def NE_chr(x):
  return chr(abs(x) % 0x10FFFF)
def NE_ord(x):
  if 0 == len(x):
    return 0
  else:
    return ord(x[0])
def NE_divmod(x,y):
  if 0 == y:
    return (x,y)
  else:
    return divmod(x,y)
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
  #:fuzzer-name python
  #:type-thunks type-thunks-for-concretization
  #:program-node ProgramWithBlock
  #:format-render python-format-render
  #:comment-wrap (λ (lines) (string-join (map (λ (l) (format "# ~a" l)) lines)
                                         "\n")))

(module+ main (python-command-line))
