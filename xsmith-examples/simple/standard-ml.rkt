#lang clotho

(require
 xsmith
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 pprint
 racket/string
 racket/format
 (except-in racket/list empty)
 "../private/xsmith-examples-version.rkt"
 )

(define-basic-spec-component comp)

(define int-type (base-type 'int))
(define-generic-type box-type ([type covariant]))
(define no-child-types (λ (n t) (hash)))

;; TODO - strings -- I'm left somewhat unsure whether SML supports unicode strings
;; or just ascii strings.  Perhaps it's inconsistent.  I would like to know if
;; there is a standard way for both...

(define random-string-length-max 50)
(define (random-ascii-string)
  (define l (random random-string-length-max))
  (apply string
         (map (λ (_) (integer->char (random 128)))
              (make-list l #f))))
(define (byte-char->decimal-escape c)
  (format "\\~a" (~r #:min-width 3 #:pad-string "0" (char->integer c))))
(define (char->unicode-escape c)
  (format "\\x~a" (~r #:min-width 4 #:pad-string "0" #:base 16 (char->integer c))))
(define (sml-string-format str)
  (apply
   string-append
   (flatten
    (list
     "\""
     (for/list ([c (string->list str)])
       (cond [(or (char<=? #\a c #\z)
                  (char<=? #\A c #\Z)
                  (char<=? #\0 c #\9)
                  )
              (string c)]
             [(< (char->integer c) 256)
              (byte-char->decimal-escape c)]
             [else
              (char->unicode-escape c)
              ]))
     "\""))))

(add-basic-expressions comp
                       #:ProgramWithSequence #t
                       #:ExpressionSequence #t
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
                       #:string-literal-value (random-ascii-string)
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
        ;; to always have an expression of void-type available
        #:prop wont-over-deepen #t
        #:prop type-info
        [void-type (λ (n t)
                     (define inner (fresh-type-variable))
                     (hash 'box (box-type inner) 'newval inner))]
        #:prop render-node-info
        (λ (n) (h-append
                lparen
                (render-child 'box n) (text " := ") (render-child 'newval n)
                rparen))]
 [DumbVoid Expression ()
           #:prop choice-weight 1
           #:prop type-info [void-type no-child-types]
           #:prop render-node-info (λ (n) (text "()"))]
 )




(define nest-step 2)

(define (binary-op-renderer op-rendered)
  (λ (n) (h-append lparen (att-value 'xsmith_render-node (ast-child 'l n))
                   space op-rendered space
                   (att-value 'xsmith_render-node (ast-child 'r n)) rparen)))


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


(define (type->string t*)
  ;; concretize and unify, just in case.
  (define t (concretize-type t*))
  (unify! t t*)
  (cond
    [(can-unify? t int-type) "int"]
    [(can-unify? t string-type) "string"]
    [(can-unify? t bool-type) "bool"]
    [(can-unify? t void-type) "unit"]
    [(can-unify? t (box-type (fresh-type-variable)))
     (define inner (fresh-type-variable))
     (unify! (box-type inner) t)
     (format "(~a) ref" (type->string inner))]
    [(can-unify? t (product-type #f))
     (define inners (product-type-inner-type-list t))
     (if (null? inners)
         "unit"
         (format "(~a)" (string-join (map type->string inners) " * ")))]
    [(can-unify? t (function-type (product-type #f) (fresh-type-variable)))
     (define ret (fresh-type-variable))
     (define arg (fresh-type-variable))
     (unify! t (function-type arg ret))
     (format "(~a -> ~a)" (type->string arg) (type->string ret))]
    [else (error 'standard-ml_type->string
                 "Type not implemented yet: ~v" t)]))

(add-property
 comp
 render-node-info

 [ProgramWithSequence
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
     (nest nest-step
           (v-append
            (text "fun main() = let\n")
            (vb-concat
             `(,(text "")
               ,(text "")
               ,@(map (λ (cn) (att-value 'xsmith_render-node cn))
                      definitions)
               ,(h-append (text "val mainresult = ") (att-value 'xsmith_render-node (ast-child 'ExpressionSequence n)))))
            (text "")
            (print-value (text "mainresult") (att-value 'xsmith_type (ast-child 'ExpressionSequence n)))
            (apply v-append
                   (map (λ (v)
                          (print-value (text (ast-child 'name v)) (ast-child 'type v)))
                        (filter (λ (x)
                                  (let ([t (ast-child 'type x)])
                                    (and (base-type? t) (not (can-unify? void-type t)))))
                                definitions)))))
     (text "in print \"\\n\"")
     (text "end")
     (text "val _ = main()")
     ;; Hack to get a newline...
     (text "")))]

 [ExpressionSequence
  (λ (n)
    (v-append
     (nest nest-step
           (v-append
            (text "let")
            (apply v-append
                   (map (λ (c) (h-append (text "val _ = ")
                                         (att-value 'xsmith_render-node c)))
                        (ast-children (ast-child 'effectexpressions n))))))
     (nest nest-step
           (v-append
            (text "in")
            (att-value 'xsmith_render-node (ast-child 'finalexpression n))))
     (text "end")))]

 [Definition (λ (n) (h-append (text "val ")
                              (text (ast-child 'name n))
                              space
                              colon
                              space
                              (text (type->string (ast-child 'type n)))
                              space
                              equals
                              space
                              (att-value 'xsmith_render-node (ast-child 'Expression n))))]


 [VariableReference (λ (n) (text (format "~a" (ast-child 'name n))))]

 ;; TODO - I could *actually* use my single-argument function type for SML and
 ;; have it take tuple objects, I think.
 [ProcedureApplication
  (λ (n) (h-append lparen
                   (att-value 'xsmith_render-node (ast-child 'procedure n))
                   lparen
                   (comma-list (map (λ (cn) (att-value 'xsmith_render-node cn))
                                    (ast-children (ast-child 'arguments n))))
                   rparen
                   rparen))]
 [FormalParameter
  (λ (n) (h-append
          (text (format "~a" (ast-child 'name n)))
          space colon space
          (text (type->string (ast-child 'type n)))))]
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

 [StringLiteral (λ (n) (text (sml-string-format (ast-child 'v n))))]
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
  #:program-node ProgramWithSequence
  #:format-render sml-format-render
  #:comment-wrap (λ (lines) (format "(*\n~a\n*)" (string-join lines "\n"))))

(module+ main (simple-sml-command-line))
