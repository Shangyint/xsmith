#lang clotho

(require
 xsmith
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 pprint
 racket/string
 "../private/xsmith-examples-version.rkt"
 )

(define-basic-spec-component javascript-comp)

(add-basic-expressions javascript-comp
                       #:VariableReference #t
                       #:ProcedureApplication #t
                       #:LambdaWithBlock #t
                       #:Numbers #t
                       #:Booleans #t
                       #:Strings #t
                       #:MutableArray #t
                       #:MutableStructuralRecord #t
                       )
(add-basic-statements javascript-comp
                      #:ProgramWithBlock #t
                      #:Block #t
                      #:ReturnStatement #t
                      #:IfElseStatement #t
                      #:ExpressionStatement #t
                      #:AssignmentStatement #t
                      #:NullStatement #t
                      #:MutableArraySafeAssignmentStatement #t
                      #:MutableStructuralRecordAssignmentStatement #t
                      )


(define nest-step 4)

(add-property
 javascript-comp
 render-hole-info
 [#f (λ (h) (text "«HOLE»"))])

(define (comma-list doc-list)
  (apply h-append
         (apply-infix (h-append comma space)
                      doc-list)))

(define max-safe-int (- (expt 2 53) 1))
(define min-safe-int (- (- (expt 2 53) 1)))
;; I'm not sure what a good boundary for string size is, but 1k seems like a reasonable starting place.
(define max-safe-string-size (expt 2 10))
(define header-boundary-numbers
  (string-append
   (format "MAX_SAFE_INT = ~a\n" max-safe-int)
   (format "MIN_SAFE_INT = ~a\n" min-safe-int)
   (format "MAX_SAFE_STRING_SIZE = ~a\n" max-safe-string-size)))
(define header-definitions-block
  ;; Javascript, being stupid, uses double precision floating point representation for all numbers, including integers.
  ;; Thus, there is a maximum size for integers before they start becoming approximate, losing mathematical properties like reflexivity and associativity for basic operators, etc.
  ;; Here is a nice web site that discusses the boundaries: https://www.avioconsulting.com/blog/overcoming-javascript-numeric-precision-issues
  ;;
  ;; I want to use Math.trunc for safe_divide, but mujs apparently doesn't have it.
  ;; So let's use Math.floor instead.
  "
safe_int = function(x){
  if (x > MAX_SAFE_INT) {
    return MAX_SAFE_INT;
  } else if (x < MIN_SAFE_INT){
    return MIN_SAFE_INT;
  } else {
    return x;
  }
}
safe_divide = function(a,b){return safe_int(Math.floor(b == 0 ? a : a / b))}
safe_plus = function(a,b){return safe_int(a + b)}
safe_minus = function(a,b){return safe_int(a - b)}
safe_times = function(a,b){return safe_int(a * b)}
array_safe_reference = function(array, index, fallback){
  if (array.length == 0) {
    return fallback;
  } else {
    return array[Math.abs(index) % array.length];
  }
}
array_safe_assignment = function(array, index, newvalue){
  if (array.length == 0) {
    return;
  } else {
    array[Math.abs(index) % array.length] = newvalue
    return;
  }
}
safe_string_append = function(a, b){
  // If strings grow too big, JS implementations either
  // run out of memory, crash saying that a string was too big,
  // or throw an exception.
  // Let's keep strings from growing too big via appending.
  if (a.length + b.length < MAX_SAFE_STRING_SIZE) {
    return a + b;
  } else {
    return a;
  }
}

// Polyfill print
if (typeof print == \"undefined\") {
  print = console.log;
}
")

(define (render-child fieldname n)
  (att-value 'xsmith_render-node (ast-child fieldname n)))
(define (binary-function-renderer name)
  (λ (n) (h-append (text name) (text "(")
                   (render-child 'l n) (text ", ") (render-child 'r n)
                   (text ")"))))
(define (binary-op-renderer op-rendered)
  (λ (n) (h-append lparen (render-child 'l n)
                   space op-rendered space
                   (render-child 'r n) rparen)))

(add-property
 javascript-comp
 render-node-info

 [ProgramWithBlock
  (λ (n)
    (define definitions (ast-children (ast-child 'definitions n)))
    (v-append
     (text header-boundary-numbers)
     (text header-definitions-block)
     (text "\n\n\n\n")
     (text "////////  Randomly generated program starts here ////////")
     (text "\n\n\n\n")
     (vb-concat
      (list*
       (text "")
       (text "")
       (map (λ (cn) (att-value 'xsmith_render-node cn))
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

 [Definition (λ (n)
               (h-append (text (ast-child 'name n))
                         space
                         equals
                         space
                         (att-value 'xsmith_render-node (ast-child 'Expression n))
                         semi))]

 [Block
  (λ (n)
    (h-append
     lbrace
     (nest nest-step
           (h-append
            line
            (v-concat
             (append
              (map (λ (cn) (att-value 'xsmith_render-node cn))
                   (ast-children (ast-child 'definitions n)))
              (map (λ (cn) (att-value 'xsmith_render-node cn))
                   (ast-children (ast-child 'statements n)))))))
     line
     rbrace))]

 [ExpressionStatement (λ (n) (h-append (att-value 'xsmith_render-node (ast-child 'Expression n))
                                       semi))]

 [ReturnStatement (λ (n) (h-append (text "return ")
                                   (att-value 'xsmith_render-node (ast-child 'Expression n))))]
 [NullStatement (λ (n) (text ";"))]
 [AssignmentStatement
  (λ (n)
    (h-append (text (format "~a" (ast-child 'name n)))
              space
              equals
              space
              (att-value 'xsmith_render-node (ast-child 'Expression n))
              semi))]

 [IfElseStatement
  (λ (n)
    (h-append
     (h-append (text "if") space lparen (att-value 'xsmith_render-node (ast-child 'test n)) rparen)
     (att-value 'xsmith_render-node (ast-child 'then n))
     (text "else")
     (att-value 'xsmith_render-node (ast-child 'else n))))]



 [VariableReference (λ (n) (text (format "~a" (ast-child 'name n))))]

 [ProcedureApplication
  (λ (n) (h-append (att-value 'xsmith_render-node (ast-child 'procedure n))
                   lparen
                   (comma-list (map (λ (cn) (att-value 'xsmith_render-node cn))
                                    (ast-children (ast-child 'arguments n))))
                   rparen))]
 [FormalParameter (λ (n) (text (format "~a" (ast-child 'name n))))]
 [LambdaWithBlock
  ;; Wrap the function in parentheses, so it always counts as an expression.
  ;; If you try to put a function expression in statement position, it complains that it needs a name.
  (λ (n) (h-append lparen (text "function") lparen
                   (comma-list (map (λ (cn) (att-value 'xsmith_render-node cn))
                                    (ast-children (ast-child 'parameters n))))
                   rparen
                   (att-value 'xsmith_render-node (ast-child 'body n))
                   rparen))]

 [BoolLiteral (λ (n) (text (if (ast-child 'v n) "true" "false")))]
 [Not (λ (n) (h-append (text "!") lparen
                       (att-value 'xsmith_render-node (ast-child 'Expression n))
                       rparen))]
 [And (binary-op-renderer (text "&&"))]
 [Or (binary-op-renderer (text "||"))]

 [IntLiteral (λ (n) (text (format "~a" (ast-child 'v n))))]
 [Plus (binary-function-renderer "safe_plus")]
 [Minus (binary-function-renderer "safe_minus")]
 [Times (binary-function-renderer "safe_times")]
 [SafeDivide (binary-function-renderer "safe_divide")]
 [LessThan (binary-op-renderer (text "<"))]
 [GreaterThan (binary-op-renderer (text ">"))]


 [StringLiteral (λ (n) (text (format "~v" (ast-child 'v n))))]
 ;; TODO - I've generated some programs where strings get too large.  I should wrap this with a safe wrapper to stay within some boundary.
 [StringAppend (binary-function-renderer "safe_string_append")]
 [StringLength (λ (n) (h-append lparen
                                (att-value 'xsmith_render-node (ast-child 'Expression n))
                                rparen
                                (text ".length")))]

 [MutableArrayLiteral
  (λ (n) (h-append lbracket
                   (comma-list (map (λ (cn) (att-value 'xsmith_render-node cn))
                                    (ast-children (ast-child 'expressions n))))
                   rbracket))]
 [MutableArraySafeReference
  (λ (n)
    (h-append (text "array_safe_reference(")
              (att-value 'xsmith_render-node (ast-child 'array n))
              (text ", ")
              (att-value 'xsmith_render-node (ast-child 'index n))
              (text ", ")
              (att-value 'xsmith_render-node (ast-child 'fallback n))
              (text ")")))]
 [MutableArraySafeAssignmentStatement
  (λ (n)
    (h-append (text "array_safe_assignment(")
              (att-value 'xsmith_render-node (ast-child 'array n))
              (text ", ")
              (att-value 'xsmith_render-node (ast-child 'index n))
              (text ", ")
              (att-value 'xsmith_render-node (ast-child 'newvalue n))
              (text ");")))]

 [MutableStructuralRecordLiteral
  (λ (n)
    ;; We need to wrap it in parentheses in statement contexts.
    (h-append lparen lbrace
              (comma-list (map (λ (fieldname expression-node)
                                 (h-append (text (format "~a" fieldname))
                                           colon
                                           space
                                           (att-value 'xsmith_render-node expression-node)))
                               (ast-child 'fieldnames n)
                               (ast-children (ast-child 'expressions n))))
              rbrace rparen))]
 [MutableStructuralRecordReference
  (λ (n) (h-append (att-value 'xsmith_render-node (ast-child 'record n))
                   (text ".")
                   (text (format "~a" (ast-child 'fieldname n)))))]
 [MutableStructuralRecordAssignmentStatement
  (λ (n) (h-append (att-value 'xsmith_render-node (ast-child 'record n))
                   (text ".")
                   (text (format "~a" (ast-child 'fieldname n)))
                   space equals space
                   (att-value 'xsmith_render-node (ast-child 'newvalue n))
                   semi))]
 )



(add-loop-over-container
 javascript-comp
 #:name ForLoopOverArray
 #:collection-type-constructor (λ (elem-type) (mutable (array-type elem-type)))
 #:loop-type-constructor (λ (elem-type) no-return-type)
 #:body-type-constructor (λ (loop-type elem-type) loop-type)
 #:mutable-container-access (read 'MutableArray)
 #:loop-ast-type Statement
 #:body-ast-type Block
 #:bind-whole-collection? #t
 )
(add-property
 javascript-comp render-node-info
 [ForLoopOverArray
  (λ (n)
    (define cd (ast-child 'collection n))
    (define collection-name (ast-child 'name cd))
    (define index-name (fresh-var-name "index_"))
    (define elem-name (ast-child 'name (ast-child 'elemname n)))
    (define body (ast-child 'body n))
    (v-append
     (h-append (text collection-name)
               (text " = ")
               (att-value 'xsmith_render-node (ast-child 'Expression cd)))
     (h-append (text (format "for (~a = 0 ; ~a < ~a.length ; ~a ++){"
                             index-name index-name collection-name index-name))
               (nest nest-step
                     (v-append
                      (text "")
                      (text (format "~a = ~a[~a];"
                                    elem-name
                                    collection-name
                                    index-name))
                      (v-concat
                       (append
                        (map (λ (cn) (att-value 'xsmith_render-node cn))
                             (ast-children (ast-child 'definitions body)))
                        (map (λ (cn) (att-value 'xsmith_render-node cn))
                             (ast-children (ast-child 'statements body)))))))
               (text "}"))
     line))])

(add-loop-over-container
 javascript-comp
 #:name ForLoopToIndex
 #:collection-type-constructor (λ (elem-type)
                                 (unify! int-type elem-type)
                                 int-type)
 #:loop-type-constructor (λ (elem-type) no-return-type)
 #:body-type-constructor (λ (loop-type elem-type) loop-type)
 #:loop-ast-type Statement
 #:body-ast-type Block
 #:bind-whole-collection? #t
 )
(add-property
 javascript-comp render-node-info
 [ForLoopToIndex
  (λ (n)
    (define cd (ast-child 'collection n))
    (define collection-name (ast-child 'name cd))
    (define index-name (fresh-var-name "index_"))
    (define guard-name (fresh-var-name "guard_"))
    (define elem-name (ast-child 'name (ast-child 'elemname n)))
    (define body (ast-child 'body n))
    (v-append
     (h-append (text collection-name)
               (text " = ")
               (att-value 'xsmith_render-node (ast-child 'Expression cd)))
     (h-append (text guard-name)
               (text " = ")
               ;; Let's limit these loops to 100 iterations.
               (text (format "((Math.abs(~a) > 100) ? 100 : Math.abs(~a))"
                             collection-name collection-name)))
     (h-append (text (format "for (~a = 0 ; ~a < ~a ; ~a ++){"
                             index-name index-name
                             guard-name
                             index-name))
               (nest nest-step
                     (v-append
                      (text "")
                      (text (format "~a = ~a;"
                                    elem-name
                                    index-name))
                      (v-concat
                       (append
                        (map (λ (cn) (att-value 'xsmith_render-node cn))
                             (ast-children (ast-child 'definitions body)))
                        (map (λ (cn) (att-value 'xsmith_render-node cn))
                             (ast-children (ast-child 'statements body)))))))
               (text "}"))
     line))])


(define (type-thunks-for-concretization)
  (list
   (λ()int-type)
   (λ()bool-type)
   (λ()string-type)
   (λ()(mutable (array-type (fresh-type-variable))))
   (λ()(mutable (fresh-structural-record-type)))
   ))

(define (javascript-format-render doc)
  (pretty-format doc 120))

(define-xsmith-interface-functions
  [javascript-comp]
  #:fuzzer-name simple-javascript
  #:fuzzer-version xsmith-examples-version-string/no-name
  #:type-thunks type-thunks-for-concretization
  #:program-node ProgramWithBlock
  #:format-render javascript-format-render
  #:default-max-depth 8
  #:comment-wrap (λ (lines) (string-join (map (λ (l) (format "// ~a" l)) lines)
                                         "\n")))

(module+ main (simple-javascript-command-line))
