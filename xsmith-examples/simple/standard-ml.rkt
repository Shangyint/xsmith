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
 "../private/util.rkt"
 "../private/xsmith-examples-version.rkt"
 syntax/parse/define
 (for-syntax
  racket/base
  syntax/parse
  ))

#|
TODO - running / compiling SML

* poly/ML - run with `poly --script <file.sml>` or compile with `polyc -o <executable> <file.sml>`
  Note that `polyc` seems to run the program while compiling...
  The polyc compiler expects a main function, while poly just executes top-level stuff.
* mlton - compile with `mlton -output <executable> <file.sml>
  No main function required, it runs top-level code.
* smlsharp - compile with `smlsharp -o <executable> <file.sml>`
  !! requires an “interface file” with the same name but extension `.smi`.
  For my purposes, it probably just needs to contain the line:
  _require "basis.smi"
  However, at the time of writing, I have yet to successfully compile anything
  with smlsharp...
* sml/NJ - its interface is really weird, and it prints lots of extra crap.
  I'm not immediately sure how I can run it in a way to be able to diff its output
  with the other systems.
|#

(define-basic-spec-component comp)

(define large-int-type (base-type 'large-int))
(define small-int-type (base-type 'small-int))
(define-generic-type box-type ([type covariant]))
(define no-child-types (λ (n t) (hash)))

;; TODO - strings are required to include ascii, and may support unicode.
;; But it's implementation-specific.  PolyML doesn't seem to support unicode strings.
;; TODO - there is also a `text` type, but I'm not sure what the difference is.
;; A quick glance doesn't seem to me like it supports unicode.

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


;; TODO - integers -- SML has int/Int (int is the type Int is the structure
;; with functions for operating on them) as fixed-width, LargeInt as some kind
;; of larger integer that may have a max size, and IntInf as arbitrary precision.
;; When IntInf is available, LargeInt must be the same type as IntInf.
;; So not all implementations have arbitrary precision ints, but if they do then
;; LargeInt is always that size.
;; I think it's possible that different implementations have different sizes for
;; the basic int type.  I could maybe implement a “safe-math” set of operations for
;; int type, but it would have to use the smallest actual int size among the implementations.
;; So for starters I think I should stick to using IntInf, which has all the functions here:
;; https://smlfamily.github.io/Basis/int-inf.html#IntInf:STR:SPEC
;; as well as here:
;; https://smlfamily.github.io/Basis/integer.html#LargeInt:STR:SPEC

;; max/min ints according to Int.maxInt and Int.minInt
(define polyml-max-int 4611686018427387903)
(define polyml-min-int -4611686018427387904)
(define mlton-max-int 2147483647)
(define mlton-min-int -2147483648)
(define max-small-int (min polyml-max-int mlton-max-int))
(define min-small-int (max polyml-min-int mlton-min-int))

(define (random-byte) (random 256))
(define (biased-random-int)
  ;; The random function returns word-sized integers.
  ;; I want more variety, like bigints.
  (random-expr
   (random-int)
   (+ (* (random-int) (random-int)) (random-int))
   (+ (* (random-int) (random-int) (random-int) (random-int)) (random-int))
   (random-byte)
   ;(random 10)
   (random-expr
    ;; interesting integers
    0
    -1
    1
    polyml-max-int
    polyml-min-int
    mlton-max-int
    mlton-min-int
    (add1 polyml-max-int)
    (sub1 polyml-min-int)
    (add1 mlton-max-int)
    (sub1 mlton-min-int))
   ))

(define (biased-random-small-int)
  (random-expr
   (random (add1 max-small-int))
   (- (random (add1 (abs min-small-int))))
   (random-byte)
   (random-expr
    ;; interesting numbers
    0
    -1
    1
    max-small-int
    min-small-int)))


(add-basic-expressions comp
                       #:ProgramWithSequence #t
                       #:ExpressionSequence #t
                       #:VariableReference #t
                       #:ProcedureApplication #t
                       #:LambdaWithExpression #t
                       #:ImmutableList #t
                       #:Booleans #t
                       #:index-and-length-type small-int-type
                       ;#:Numbers #t
                       ;#:int-type large-int-type
                       ;#:number-type large-int-type
                       ;#:int-literal-value (biased-random-int)
                       ;#:Strings #t
                       ;#:string-literal-value (random-ascii-string)
                       )


;; TODO - here is a primer on basic syntax: http://rigaux.org/language-study/syntax-across-languages-per-language/SML.html
;; TODO - here is an actual intro to standard ML, though it says that it's out of date, and is copyright 1998: https://www.cs.cmu.edu/~rwh/introsml/contents.htm
;; TODO - bring in a bunch of stuff from “basis” library: https://www.cs.princeton.edu/~appel/smlnj/basis/string.html

;; No exceptions.
(define NE? #t)

(define-for-syntax (racr-ize-id id)
  (datum->syntax id
                 (string->symbol
                  (string-titlecase (symbol->string (syntax->datum id))))))
(define-ag/one-arg ag/one-arg comp racr-ize-id NE?
  #'large-int-type
  (λ (name-thunk)
    (λ (n)
      (h-append (text (symbol->string (name-thunk)))
                (text "(") (render-child 'Expression n) (text ")")))))
(define-ag/two-arg ag/two-arg comp racr-ize-id NE?
  #'large-int-type
  (λ (name-thunk)
    (λ (n)
      (h-append (text (symbol->string (name-thunk)))
                (text "(")
                (render-child 'l n)
                (text ", ")
                (render-child 'r n)
                (text ")")))))
(define-ag/three-arg ag/three-arg comp racr-ize-id NE?
  #'large-int-type
  (λ (name-thunk)
    (λ (n)
      (h-append (text (symbol->string (name-thunk)))
                (text "(")
                (render-child 'l n)
                (text ", ")
                (render-child 'm n)
                (text ", ")
                (render-child 'r n)
                (text ")")))))

(add-to-grammar
 comp
 [BoxLiteral Expression (Expression)
             #:prop wont-over-deepen #t
             #:prop choice-weight 1
             #:prop type-info
             [(box-type (fresh-type-variable)) (λ (n t)
                                                 (define ct (fresh-type-variable))
                                                 (unify! t (box-type ct))
                                                 (hash 'Expression ct))]
             #:prop render-node-info
             (λ (n) (h-append lparen (text "ref ") lparen (render-child 'Expression n) rparen rparen))]
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



(define-syntax-parser ag/atomic-literal
  [(_ name:id type:expr fresh-expr:expr render-expr:expr)
   #'(add-to-grammar comp [name Expression ([v = fresh-expr])
                                #:prop choice-weight 1
                                #:prop type-info [type no-child-types]
                                #:prop render-node-info render-expr])])
(ag/atomic-literal LargeIntLiteral large-int-type (biased-random-int)
                   render-int-literal)
(ag/atomic-literal SmallIntLiteral small-int-type (biased-random-small-int)
                   render-int-literal)
(define render-int-literal
  (λ (n) (let ([v (ast-child 'v n)])
           ;; SML uses tilde instead of dash for negative numbers.  Weird.
           (text (format "~a~a" (if (< v 0) "~" "") (abs v))))))
(ag/atomic-literal StringLiteral string-type (random-ascii-string)
                   (λ (n) (text (sml-string-format (ast-child 'v n)))))



(ag/two-arg safeSmallAdd #:type small-int-type)
(ag/two-arg safeSmallSubtract #:type small-int-type)
(ag/two-arg safeSmallMultiply #:type small-int-type)
(ag/two-arg safeSmallDivide #:type small-int-type)
(ag/two-arg safeSmallModulo #:type small-int-type)
;(ag/two-arg safeSmallQuotient #:type small-int-type)
;(ag/two-arg safeSmallRemainder #:type small-int-type)
(ag/one-arg safeSmallNegation #:type small-int-type)
(ag/one-arg safeSmallAbs #:type small-int-type)
(ag/two-arg Int.min #:racr-name SmallMin #:type small-int-type)
(ag/two-arg Int.max #:racr-name SmallMax #:type small-int-type)
(ag/one-arg Int.sign #:racr-name SmallSign #:type small-int-type)
(ag/two-arg Int.sameSign #:racr-name SmallSameSign
            #:type bool-type #:ctype (E2ctype small-int-type small-int-type))
;(ag/two-arg safeSmallFmt #:type string-type #:ctype (E2ctype small-int-type small-int-type))
(ag/one-arg Int.toString #:racr-name SmallToString
            #:type string-type #:ctype (Ectype small-int-type))

(define-syntax-parser ag/two-infix
       [(_ name:id
           (~or (~optional (~seq #:type type:expr)
                           #:defaults ([type #'large-int-type]))
                (~optional (~seq #:ctype ctype:expr)
                           #:defaults ([ctype #'(λ (n t) (hash 'l t 'r t))]))
                (~optional (~seq #:racr-name racr-name:id)
                           #:defaults ([racr-name (racr-ize-id #'name)]))
                (~optional (~seq #:NE-name NE-name)
                           #:defaults ([NE-name #'name]))
                (~optional (~seq #:feature feature-arg)))
           ...)
        #'(add-to-grammar comp
                          [racr-name Expression ([l : Expression]
                                                 [r : Expression])
                                     #:prop type-info [type ctype]
                                     (~? (~@ #:prop feature feature-arg))
                                     #:prop render-node-info
                                     (λ (n) (h-append lparen (render-child 'l n)
                                                      (text (symbol->string
                                                             (if NE? 'NE-name 'name)))
                                                      (render-child 'r n) rparen))])])

(ag/two-infix + #:racr-name LargeAdd #:type large-int-type)
(ag/two-infix - #:racr-name LargeSubtract #:type large-int-type)
(ag/two-infix * #:racr-name LargeMultiply #:type large-int-type)
(ag/two-arg safeLargeDivide #:type large-int-type)
(ag/two-arg safeLargeModulo #:type large-int-type)
;(ag/two-arg safeLargeRemainder #:type large-int-type)
;(ag/two-arg safeLargeQuotient #:type large-int-type)
(ag/two-arg LargeInt.min #:racr-name LargeMin #:type large-int-type)
(ag/two-arg LargeInt.max #:racr-name LargeMax #:type large-int-type)
(ag/one-arg ~ #:racr-name LargeNegate #:type large-int-type)
(ag/one-arg LargeInt.abs #:racr-name LargeAbs #:type large-int-type)
(ag/one-arg LargeInt.sign #:racr-name LargeSign #:type large-int-type)
(ag/two-arg LargeInt.sameSign #:racr-name LargeSameSign
            #:type bool-type #:ctype (E2ctype large-int-type large-int-type))
;(ag/two-arg safeLargeFmt #:type string-type #:ctype (E2ctype large-int-type large-int-type))
(ag/one-arg LargeInt.toString #:racr-name LargeToString
            #:type string-type #:ctype (Ectype large-int-type))


(define-syntax-parser ag/comparison
  [(_ name racr-name type)
   #'(ag/two-infix name
                   #:racr-name racr-name #:type bool-type #:ctype (E2ctype type type))])
(ag/comparison = SmallIntEqual small-int-type)
(ag/comparison <> SmallIntNotEqual small-int-type)
(ag/comparison < SmallIntLess small-int-type)
(ag/comparison <= SmallIntLessEqual small-int-type)
(ag/comparison > SmallIntGreater small-int-type)
(ag/comparison >= SmallIntGreaterEqual small-int-type)
(ag/comparison = LargeIntEqual large-int-type)
(ag/comparison <> LargeIntNotEqual large-int-type)
(ag/comparison < LargeIntLess large-int-type)
(ag/comparison <= LargeIntLessEqual large-int-type)
(ag/comparison > LargeIntGreater large-int-type)
(ag/comparison >= LargeIntGreaterEqual large-int-type)
(ag/comparison = StringEqual string-type)
(ag/comparison <> StringNotEqual string-type)
(ag/comparison < StringLess string-type)
(ag/comparison <= StringLessEqual string-type)
(ag/comparison > StringGreater string-type)
(ag/comparison >= StringGreaterEqual string-type)

(define-ag/converter ag/converter ag/one-arg)
(ag/converter Int.toLarge
              small-int-type large-int-type
              #:racr-name SmallIntTolargeInt)
(ag/converter safeLargeIntToSmallInt large-int-type small-int-type)
(ag/converter size
              string-type small-int-type
              #:racr-name StringSize)



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


(define header-definitions-block*
  "
fun safeLargeDivide(a, b) = if 0 = b then a else a div b
fun safeLargeModulo(a, b) = if 0 = b then a else a mod b
(*fun safeLargeRemainder(a, b) = if 0 = b then a else a rem b*)
(*fun safeLargeQuotient(a, b) = if 0 = b then a else quot(a, b)*)
fun safe_car(l, fallback) = if (null l) then fallback else (hd l)
fun safe_cdr(l, fallback) = if (null l) then fallback else (tl l)
fun safeLargeIntToSmallInt(x : LargeInt.int) =
  if x > 0 then LargeInt.toInt(x mod max_as_large)
     else LargeInt.toInt(x mod min_as_large)

")
(define (make-safe-math-infix/non-div name op)
  (format "
fun ~a(a, b) = let
  val largeResult : LargeInt.int = Int.toLarge(a) ~a Int.toLarge(b)
  in if ((largeResult > max_as_large) orelse (largeResult < min_as_large))
     then a else a ~a b
  end
"
          name op op))
(define (make-safe-math-infix/div name op)
  (format "
fun ~a(a, b) = if 0 = b then a
  else let
  val largeResult : LargeInt.int = Int.toLarge(a) ~a Int.toLarge(b)
  in if largeResult > max_as_large orelse largeResult < min_as_large
     then a else a ~a b
  end
"
          name op op))
(define (make-safe-math-prefix/div name op)
  (format "
fun ~a(a, b) = if 0 = b then a
  else let
  val largeResult : LargeInt.int = ~a(Int.toLarge(a), Int.toLarge(b))
  in if largeResult > max_as_large orelse largeResult < min_as_large
     then a else ~a(a, b)
  end
"
          name op op))
(define (make-safe-math-prefix/single name op)
  (format "
fun ~a(a) = let
  val largeResult : LargeInt.int = ~a(Int.toLarge(a))
  in if largeResult > max_as_large orelse largeResult < min_as_large
     then a else ~a(a)
  end
"
          name op op))
(define header-definitions-block
  (string-join
   (list
    (format "val max_as_large : LargeInt.int = ~a" max-small-int)
    (format "val min_as_large : LargeInt.int = ~~~a" (abs min-small-int))
    (make-safe-math-infix/non-div "safeSmallAdd" "+")
    (make-safe-math-infix/non-div "safeSmallSubtract" "-")
    (make-safe-math-infix/non-div "safeSmallMultiply" "*")
    (make-safe-math-infix/div "safeSmallDivide" "div")
    (make-safe-math-infix/div "safeSmallModulo" "mod")
    ;(make-safe-math-infix/div "safeSmallRemainder" "rem")
    ;(make-safe-math-prefix/div "safeSmallQuotient" "quot")
    (make-safe-math-prefix/single "safeSmallNegate" "~")
    (make-safe-math-prefix/single "safeSmallAbs" "abs")
    header-definitions-block*
    )
   "\n"))

(define (render-child cname node)
  (att-value 'xsmith_render-node (ast-child cname node)))





(add-to-grammar
 comp
 [ProgramWithExpression #f ([definitions : Definition *]
                            [Expression])
                        #:prop strict-child-order? #t
                        #:prop type-info
                        [(fresh-type-variable large-int-type bool-type string-type)
                         (λ (n t)
                           (hash 'definitions (λ (c) (fresh-type-variable))
                                 'Expression t))]])

(define (list-add-between ls between)
  (cond [(null? ls) ls]
        [(null? (cdr ls)) ls]
        [else (cons (car ls)
                    (cons between
                          (list-add-between (cdr ls) between)))]))

(define (type->string t*)
  ;; concretize and unify, just in case.
  (define t (concretize-type t*))
  (unify! t t*)
  (cond
    [(can-unify? t large-int-type) "LargeInt.int"]
    [(can-unify? t small-int-type) "int"]
    [(can-unify? t string-type) "string"]
    [(can-unify? t bool-type) "bool"]
    [(can-unify? t void-type) "unit"]
    [(can-unify? t (box-type (fresh-type-variable)))
     (define inner (fresh-type-variable))
     (unify! (box-type inner) t)
     (format "(~a ref)" (type->string inner))]
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
    [(can-unify? t (immutable (list-type (fresh-type-variable))))
     (define inner (fresh-type-variable))
     (unify! t (immutable (list-type inner)))
     (format "(~a list)" (type->string inner))]
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
      (define (get-string-converter type)
        (h-append
         lparen
         (cond
           [(can-unify? type large-int-type)
            (text "LargeInt.toString")]
           [(can-unify? type small-int-type)
            (text "Int.toString")]
           [(can-unify? type bool-type)
            (text "Bool.toString")]
           [(can-unify? type string-type)
            (text "fn x : string => x")]
           [(can-unify? type void-type)
            (text "fn x : unit => \"\"")]
           [(can-unify? type (box-type (fresh-type-variable)))
            (define inner (fresh-type-variable))
            (unify! (box-type inner) type)
            (h-append (text (format "fn x : ~a => " (type->string type)))
                      lparen (get-string-converter inner) (text " (!x)") rparen)]
           [(can-unify? type (function-type (fresh-type-variable)
                                            (fresh-type-variable)))
            (text (format "fn x : ~a => \"procedure\"" (type->string type)))]
           [(can-unify? type (immutable (list-type (fresh-type-variable))))
            (define inner (fresh-type-variable))
            (unify! type (immutable (list-type inner)))
            (h-append (text (format "fn x : ~a => concat(map " (type->string type)))
                      (get-string-converter inner)
                      (text " x)"))]
           [else (error 'type-printing "no rule for type ~v\n" type)]
           )
         rparen))
      (v-append
       (h-append (text "val _ = print")
                 lparen
                 (get-string-converter type)
                 lparen
                 pp-obj
                 rparen
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
                                    (and #;(base-type? t) (not (can-unify? void-type t)))))
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
 [Not (λ (n) (h-append lparen (text "not") lparen
                       (att-value 'xsmith_render-node (ast-child 'Expression n))
                       rparen rparen))]
 [And (binary-op-renderer (text "andalso"))]
 [Or (binary-op-renderer (text "orelse"))]

 #;[IntLiteral render-int-literal]
 #;[Plus (binary-op-renderer (text "+"))]
 ;; TODO - unary negation with tilde
 ;; TODO - real division uses /, integer division uses `div`, modulus is `mod`
 #;[Minus (binary-op-renderer (text "-"))]
 #;[Times (binary-op-renderer (text "*"))]
 #;[LessThan (binary-op-renderer (text "<"))]
 #;[GreaterThan (binary-op-renderer (text ">"))]

 #;[SafeDivide (λ (n) (h-append (text "safe_divide") lparen
                              (att-value 'xsmith_render-node (ast-child 'l n))
                              (text ",") space
                              (att-value 'xsmith_render-node (ast-child 'r n))
                              rparen))]

 #;[StringLiteral (λ (n) (text (sml-string-format (ast-child 'v n))))]
 ;; TODO - SML strings have concat which is [string] -> string.
 ;; I should define my own string stuff rather than bringing in canned components here.
 #;[StringAppend (λ (n) (h-append lparen
                                (text "concat ") lbracket
                                (att-value 'xsmith_render-node (ast-child 'l n))
                                comma space
                                (att-value 'xsmith_render-node (ast-child 'r n))
                                rbracket rparen))]
 #;[StringLength (λ (n) (h-append lparen (text "Int.toLarge(size(")
                                (att-value 'xsmith_render-node (ast-child 'Expression n))
                                (text "))")
                                rparen
                                ))]


 [ImmutableListLiteral
  (λ (n) (h-append lbracket
                   (apply h-append
                          (list-add-between
                           (map (λ (c) (att-value 'xsmith_render-node c))
                                (ast-children (ast-child 'expressions n)))
                           (text ", ")))
                   rbracket))]
 [ImmutableListSafeCar
  (λ (n) (h-append lparen (text "safe_car")
                   lparen (render-child 'list n) (text ", ")
                   (render-child 'fallback n) rparen rparen))]
 [ImmutableListSafeCdr
  (λ (n) (h-append lparen (text "safe_cdr")
                   lparen (render-child 'list n) (text ", ")
                   (render-child 'fallback n) rparen rparen))]
 [ImmutableListCons
  (λ (n) (h-append lparen lbracket (render-child 'newvalue n) rbracket
                   (text " @ ") (render-child 'list n) rparen))]


 )



(define (type-thunks-for-concretization)
  (list
   (λ()small-int-type)
   (λ()large-int-type)
   (λ()bool-type)
   (λ()string-type)
   (λ()(box-type (fresh-type-variable)))
   (λ()(immutable (list-type (fresh-type-variable))))
   ))

(define (sml-format-render doc)
  (pretty-format doc 120))

(define-xsmith-interface-functions
  [comp]
  #:fuzzer-name simple-sml
  #:fuzzer-version xsmith-examples-version-string/no-name
  #:type-thunks type-thunks-for-concretization
  #:program-node ProgramWithSequence
  #:format-render sml-format-render
  #:comment-wrap (λ (lines) (format "(*\n~a\n*)" (string-join lines "\n"))))

(module+ main (simple-sml-command-line))
