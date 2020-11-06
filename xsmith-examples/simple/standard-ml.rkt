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

(define-basic-spec-component comp)

(define int-type (base-type 'int))
(define-generic-type box-type ([type covariant]))

(add-basic-expressions comp
                       ;#:ProgramWithSequence #t
                       ;#:ExpressionSequence #t
                       #:VariableReference #t
                       #:ProcedureApplication #t
                       #:LambdaWithExpression #t
                       #:Numbers #t
                       #:int-type int-type
                       #:number-type int-type
                       ;; TODO - make an int generator that fits within SML bounds and produces interesting numbers.
                       #:int-literal-value (random 100)
                       #:Booleans #t
                       #:Strings #t
                       ;; TODO - make a string generator that fits within SML bounds and produces interesting strings.
                       #:string-literal-value (random-ref (list "foo" "bar" "baz"))
                       )


;; TODO - here is a primer on basic syntax: http://rigaux.org/language-study/syntax-across-languages-per-language/SML.html
;; TODO - bring in a bunch of stuff from “basis” library: https://www.cs.princeton.edu/~appel/smlnj/basis/string.html


(add-to-grammar
 comp
 [BoxLiteral Expression (Expression)
             #:prop wont-over-deepen #t
             #:prop type-info
             [(box-type (fresh-type-variable)) (λ (n t)
                                                 (define ct (fresh-type-variable))
                                                 (unify! t (box-type ct))
                                                 (hash 'Expression ct))]
             #:prop render-node-info
             (λ (n) (h-append lparen (text "ref ") (render-child 'Expression n) rparen))]
 [Unbox Expression (Expression)
        #:prop mutable-container-access (read 'box)
        #:prop type-info
        [(fresh-type-variable) (λ (n t) (hash 'Expression (box-type t)))]
        #:prop render-node-info
        (λ (n) (h-append lparen (text "!") (render-child 'Expression n) rparen))]
 [SetBox Expression ([box : Expression]
                     [newval : Expression])
        #:prop mutable-container-access (write 'box)
        #:prop type-info
        [void-type (λ (n t)
                     (define inner (fresh-type-variable))
                     (hash 'box (box-type inner) 'newval inner))]
        #:prop render-node-info
        (λ (n) (h-append
                lparen
                (render-child 'box n) (text " := ") (render-child 'newval n)
                rparen))]
 )




(define nest-step 4)

(define (binary-op-renderer op-rendered)
  (λ (n) (h-append lparen (att-value 'xsmith_render-node (ast-child 'l n))
                   space op-rendered space
                   (att-value 'xsmith_render-node (ast-child 'r n)) rparen)))

;; TODO - I haven't quite got procedure (lambda) printing right, so I'm turning functions off for now.
(add-property comp choice-weight [ProcedureApplication 0])

(add-property
 comp
 render-hole-info
 [#f (λ (h) (text "«HOLE»"))])

(define (comma-list doc-list)
  (apply h-append
         (apply-infix (h-append comma space)
                      doc-list)))
(define header-definitions-block
  "
fun safe_divide(a, b) = if 0 = b then a else a div b

")

(define (render-child cname node)
  (att-value 'xsmith_render-node (ast-child cname node)))





(add-to-grammar
 comp
 [ProgramWithExpression #f ([definitions : Definition *]
                            [Expression])
                        #:prop strict-child-order? #t
                        #:prop type-info
                        [(fresh-type-variable int-type bool-type string-type)
                         (λ (n t)
                           (hash 'definitions (λ (c) (fresh-type-variable))
                                 'Expression t))]])


(add-property
 comp
 render-node-info

 [ProgramWithExpression
  ;; Some compilers (polyml's polyc) expect a main function.
  ;; Others make a binary that executes the top level.
  ;; So we'll make a main function but also call it.
  (λ (n)
    (define definitions (ast-children (ast-child 'definitions n)))
    (define (print-value pp-obj type)
      (v-append
       (h-append (text "val _ = print")
                 lparen
                 (cond
                   [(or (can-unify? type int-type)
                        (can-unify? type number-type))
                    (h-append (text "Int.toString ")
                              pp-obj)]
                   [(can-unify? type bool-type)
                    (h-append (text "Bool.toString ")
                              pp-obj)]
                   [(can-unify? type string-type) pp-obj]
                   [else (error 'type-printing "no rule for type ~v\n" type)]
                   )
                 rparen)
       (text "val _ = print \"\\n\"")))
    (v-append
     (text header-definitions-block)
     (text "fun main() = let\n")
     (vb-concat
      `(,(text "")
        ,(text "")
        ,@(map (λ (cn) (att-value 'xsmith_render-node cn))
               definitions)
        ,(h-append (text "val mainresult = ") (att-value 'xsmith_render-node (ast-child 'Expression n)))))
     (text "")
     (print-value (text "mainresult") (att-value 'xsmith_type (ast-child 'Expression n)))
     (apply v-append
            (map (λ (v)
                   (print-value (text (ast-child 'name v)) (ast-child 'type v)))
                 (filter (λ (x) (base-type? (ast-child 'type x)))
                         definitions)))
     (text "in print \"\\n\" end")
     (text "val _ = main()")
     ;; Hack to get a newline...
     (text "")))]
 #;[ProgramWithSequence
  (λ (n)
    (define definitions (ast-children (ast-child 'definitions n)))
    (v-append
     (text header-definitions-block)
     (vb-concat
      (list*
       (text "")
       (text "")
       (map (λ (cn) (att-value 'xsmith_render-node cn))
            (append definitions
                    (list (ast-child 'ExpressionSequence n))))))
     (text "")
     (apply v-append
            (map (λ (v) (text (format "print ~a\n"
                                      (ast-child 'name v))))
                 #;(filter (λ (x) (base-type? (ast-child 'type x)))
                         definitions)
                 definitions))
     ;; Hack to get a newline...
     (text "")))]
 #;[ExpressionSequence
  (λ (n)
    (v-append
     (apply v-append
            (map (λ (c) (att-value 'xsmith_render-node c))
                 (ast-children (ast-child 'effectexpressions n))))
     (att-value 'xsmith_render-node (ast-child 'finalexpression n))))]

 ;; TODO - maybe type annotations.
 [Definition (λ (n) (h-append (text "val ")
                              (text (ast-child 'name n))
                              space
                              equals
                              space
                              (att-value 'xsmith_render-node (ast-child 'Expression n))))]


 [VariableReference (λ (n) (text (format "~a" (ast-child 'name n))))]

 ;; TODO - I could *actually* use my single-argument function type for SML and
 ;; have it take tuple objects, I think.
 [ProcedureApplication
  (λ (n) (h-append (att-value 'xsmith_render-node (ast-child 'procedure n))
                   lparen
                   (comma-list (map (λ (cn) (att-value 'xsmith_render-node cn))
                                    (ast-children (ast-child 'arguments n))))
                   rparen))]
 ;; TODO - maybe type annotations.
 [FormalParameter
  (λ (n) (text (format "~a" (ast-child 'name n))))
  #;(λ (n) (h-append
          (text (format "~a" (ast-child 'name n)))
          space colon space
          (text (type->string (ast-child 'type n)))))
  ]
 [LambdaWithExpression
  (λ (n) (h-append lparen (text "fn") lparen
                   (comma-list (map (λ (cn) (att-value 'xsmith_render-node cn))
                                    (ast-children (ast-child 'parameters n))))
                   rparen
                   (text " => ")
                   (att-value 'xsmith_render-node (ast-child 'body n))
                   rparen))]

 [BoolLiteral (λ (n) (text (if (ast-child 'v n) "true" "false")))]
 [Not (λ (n) (h-append (text "not") lparen
                       (att-value 'xsmith_render-node (ast-child 'Expression n))
                       rparen))]
 [And (binary-op-renderer (text "andalso"))]
 [Or (binary-op-renderer (text "orelse"))]

 [IntLiteral (λ (n) (text (format "~a" (ast-child 'v n))))]
 [Plus (binary-op-renderer (text "+"))]
 ;; TODO - unary negation with tilde
 ;; TODO - real division uses /, integer division uses `div`, modulus is `mod`
 [Minus (binary-op-renderer (text "-"))]
 [Times (binary-op-renderer (text "*"))]
 [LessThan (binary-op-renderer (text "<"))]
 [GreaterThan (binary-op-renderer (text ">"))]

 [SafeDivide (λ (n) (h-append (text "safe_divide") lparen
                              (att-value 'xsmith_render-node (ast-child 'l n))
                              (text ",") space
                              (att-value 'xsmith_render-node (ast-child 'r n))
                              rparen))]

 [StringLiteral (λ (n) (text (format "~v" (ast-child 'v n))))]
 ;; TODO - SML strings have concat which is [string] -> string.
 ;; I should define my own string stuff rather than bringing in canned components here.
 [StringAppend (λ (n) (h-append lparen
                                (text "concat ") lbracket
                                (att-value 'xsmith_render-node (ast-child 'l n))
                                comma space
                                (att-value 'xsmith_render-node (ast-child 'r n))
                                rbracket rparen))]
 [StringLength (λ (n) (h-append lparen (text "size ")
                                (att-value 'xsmith_render-node (ast-child 'Expression n))
                                rparen
                                ))]


 )



(define (type-thunks-for-concretization)
  (list
   (λ()int-type)
   (λ()bool-type)
   (λ()string-type)
   ;(λ()(mutable (array-type (fresh-type-variable))))
   ;(λ()(mutable (fresh-structural-record-type)))
   ))

(define (sml-format-render doc)
  (pretty-format doc 120))

(define-xsmith-interface-functions
  [comp]
  #:fuzzer-name simple-sml
  #:fuzzer-version xsmith-examples-version-string/no-name
  #:type-thunks type-thunks-for-concretization
  ;#:program-node ProgramWithSequence
  #:program-node ProgramWithExpression
  #:format-render sml-format-render
  #:comment-wrap (λ (lines) (format "(*\n~a\n*)" (string-join lines "\n"))))

(module+ main (simple-sml-command-line))
