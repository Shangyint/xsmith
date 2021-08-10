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
  racket/string
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

;;;; Types

;; Integers -- SML has int/Int (int is the type Int is the structure
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

;; Strings - the string type is essentially (extended) ascii strings,
;; presumably a byte string.  It's a vector of char type.
;; the WideString.string type is a vector of widechar type, and
;; is not a byte string.  It's not guaranteed to be unicode, but
;; probably any sane implementation uses unicode.
;; HOWEVER - poly/ML doesn't seem to implement WideString or WideChar.
;; So maybe I'll not use them for now.

(define large-int-type (base-type 'large-int))
(define small-int-type (base-type 'small-int))
(define byte-char-type (base-type 'byte-char))
(define byte-string-type (base-type 'byte-string))
(define wide-char-type (base-type 'wide-char))
(define wide-string-type (base-type 'wide-string))
(define-generic-type box-type ([type covariant]))
(define no-child-types (λ (n t) (hash)))

(define tuple-max-length 6)


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



;; max/min ints according to Int.maxInt and Int.minInt
(define polyml-max-int 4611686018427387903)
(define polyml-min-int -4611686018427387904)
(define mlton-max-int 2147483647)
(define mlton-min-int -2147483648)
;; mosml and mlkit have the same max/min values as polyml
;; hamlet inherits its max/min values from whatever SML compiles it.
(define max-small-int (min polyml-max-int mlton-max-int))
(define min-small-int (max polyml-min-int mlton-min-int))

(define polyml-max-char 255)
(define polyml-min-char 0)
(define mlton-max-char 255)
(define mlton-min-char 0)
(define max-byte-char (max polyml-max-char mlton-max-char))
(define min-byte-char (min polyml-min-char mlton-min-char))

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
    (sub1 mlton-min-int)
    )))

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
                       #:ProcedureApplicationSingle #t
                       #:LambdaSingleWithExpression #t
                       #:VoidExpression #t
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

(add-loop-over-container
 comp
 #:name IntegerForLoop
 #:loop-type-constructor (λ (elem-type) void-type)
 #:body-type-constructor (λ (loop-type elem-type) void-type)
 #:loop-variable-type-constructor (λ (elem-type) small-int-type)
 #:collection-type-constructor (λ (elem-type) small-int-type)
 )
(add-property
 comp
 render-node-info
 [IntegerForLoop
  ;; For this IntegerForLoop, we will have the loop index in a box, loop using
  ;; the box, and have an integer definition within the loop body based on that box.
  (λ (n)
    (let ([box-var (text (fresh-var-name "loopbox"))])
      (v-append
       (h-append (text "let val ")
                 box-var
                 (text " = ref ")
                 (render-child 'collection n)
                 )
       (nest nest-step
             (v-append
              (text "in")
              (h-append
               (text "while !")
               box-var
               (nest nest-step
                     (v-append
                      (text " > 0 do")
                      (nest nest-step
                            (v-append
                             (h-append (text "let val ") (render-child 'elemname n)
                                       (text " = !") box-var (text " in ("))
                             (h-append box-var (text " := !") box-var (text " - 1;"))
                             (render-child 'body n)))
                      (text ") end"))))))
       (h-append (text "end")))))])


;; No exceptions.
(define NE? #t)

(define-for-syntax (racr-ize-id id)
  (datum->syntax id
                 (string->symbol
                  (string-replace
                   (string-titlecase (symbol->string (syntax->datum id)))
                   "." "Dot"))))
(define-ag/one-arg ag/one-arg comp racr-ize-id NE?
  #'int-type
  (λ (name-thunk)
    (λ (n)
      (h-append lparen
                (text (symbol->string (name-thunk)))
                lparen
                (render-child 'Expression n)
                rparen
                rparen))))
(define-ag/two-arg ag/two-arg comp racr-ize-id NE?
  #'int-type
  (λ (name-thunk)
    (λ (n)
      (h-append lparen
                (text (symbol->string (name-thunk)))
                lparen
                (render-child 'l n)
                (text ", ")
                (render-child 'r n)
                rparen
                rparen))))
(define-ag/three-arg ag/three-arg comp racr-ize-id NE?
  #'int-type
  (λ (name-thunk)
    (λ (n)
      (h-append lparen
                (text (symbol->string (name-thunk)))
                lparen
                (render-child 'l n)
                (text ", ")
                (render-child 'm n)
                (text ", ")
                (render-child 'r n)
                rparen
                rparen))))
(define-ag/converter ag/converter ag/one-arg)

(add-to-grammar
 comp
 [BoxLiteral Expression (Expression)
             #:prop wont-over-deepen #t
             #:prop choice-weight (depth-weight)
             #:prop type-info
             [(box-type (fresh-type-variable))
              (λ (n t)
                (define ct (fresh-type-variable))
                (unify! t (box-type ct))
                (hash 'Expression ct))]
             #:prop render-node-info
             (λ (n) (h-append lparen (text "ref ")
                              lparen (render-child 'Expression n) rparen
                              rparen))]
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


 [TupleLiteral Expression ([values : Expression *])
               ;; TODO - SML supports tuple projection with the `#N` operator, where
               ;; you get out the Nth value, index starting at 1.
               ;; HOWEVER, it does not support tuples of length 1!
               ;; I think I'll just allow tuples for function arguments.
               #:prop wont-over-deepen #t
               #:prop reducible-list-fields #f
               ;; Because it's going to just be a wrapper for function arguments,
               ;; make the depth increase 0.
               #:prop depth-increase 0
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
                       (h-concat
                        (list-add-between (for/list ([c (ast-children
                                                         (ast-child 'values n))])
                                            (att-value 'xsmith_render-node c))
                                          (text ", ")))
                       (text ")")))]
 [PolymorphicFunction
  Expression ([f : Expression] [parametrictype])
  #:prop render-node-info (λ (n) (h-append (text "(* It's Polymorphin time! *) ")
                                           (render-child 'f n)))
  #:prop wont-over-deepen #t
  #:prop feature parametric-types
  #:prop choice-weight (λ (n)
                         (if (ast-subtype? (parent-node n) 'PolymorphicFunction)
                             0
                             1000))
  #:prop type-info
  [(function-type (fresh-type-variable) (fresh-type-variable))
   (λ (n t)
     (define pt (ast-child 'parametrictype n))
     (if pt
         (begin
           (unify! (replace-parametric-types-with-variables pt) t)
           (hash 'f pt))
         (let ([new-pt (make-parametric-type-based-on t #:sml-hack #t)])
           (enqueue-inter-choice-transform
            (λ () (rewrite-terminal 'parametrictype n new-pt)))
           (hash 'f new-pt))))]]
 )



(define-syntax-parser ag/atomic-literal
  [(_ name:id type:expr fresh-expr:expr render-expr:expr
      (~or
       (~optional (~seq #:feature feature-arg)))
      ...)
   #'(add-to-grammar comp [name Expression ([v = fresh-expr])
                                (~? (~@ #:prop feature feature-arg))
                                #:prop choice-weight
                                (depth-weight)
                                #:prop type-info [type no-child-types]
                                #:prop render-node-info render-expr])])
(ag/atomic-literal LargeIntLiteral large-int-type (biased-random-int)
                   render-int-literal
                   #:feature large-int)
(ag/atomic-literal SmallIntLiteral small-int-type (biased-random-small-int)
                   render-int-literal)
(define render-int-literal
  (λ (n)
    (define (format-too-large x)
      (define-values (q r) (quotient/remainder x max-small-int))
      (if (equal? q 0)
          r
          (format "((~a * ~a) + ~a)" (format-too-large q) max-small-int r)))
    (let ([v (ast-child 'v n)])
      ;; SML uses tilde instead of dash for negative numbers.  Weird.
      (text (format "~a~a"
                    (if (< v 0) "~" "")
                    ;; Some implementations only allow literals that are as large as
                    ;; the small int type.  So we need to make them small enough.
                    ;; Actually, I don't think this is really the case, it
                    ;; looks like those versions perhaps just don't support
                    ;; IntInf.int or LargeInt.int...
                    (abs v))))))
(ag/atomic-literal ByteCharLiteral byte-char-type (random max-byte-char)
                   (λ (n) (text (format "(#~a)"
                                        (sml-string-format
                                         (string (integer->char (ast-child 'v n))))))))
(ag/atomic-literal ByteStringLiteral byte-string-type (random-ascii-string)
                   (λ (n) (text (sml-string-format (ast-child 'v n)))))



(ag/two-arg safeSmallAdd #:type small-int-type)
(ag/two-arg safeSmallSubtract #:type small-int-type)
(ag/two-arg safeSmallMultiply #:type small-int-type)
(ag/two-arg safeSmallDivide #:type small-int-type)
(ag/two-arg safeSmallModulo #:type small-int-type)
(ag/two-arg safeSmallQuotient #:type small-int-type)
(ag/two-arg safeSmallRemainder #:type small-int-type)
(ag/one-arg safeSmallNegate #:type small-int-type)
(ag/one-arg safeSmallAbs #:type small-int-type)
(ag/two-arg Int.min #:racr-name SmallMin #:type small-int-type)
(ag/two-arg Int.max #:racr-name SmallMax #:type small-int-type)
(ag/one-arg Int.sign #:racr-name SmallSign #:type small-int-type)
(ag/two-arg Int.sameSign #:racr-name SmallSameSign
            #:type bool-type #:ctype (E2ctype small-int-type small-int-type))
;(ag/two-arg safeSmallFmt #:type byte-string-type #:ctype (E2ctype small-int-type small-int-type))

(define-syntax-parser ag/two-infix
       [(_ name:id
           (~or (~optional (~seq #:type type:expr)
                           #:defaults ([type #'int-type]))
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
                                                      space
                                                      (text (symbol->string
                                                             (if NE? 'NE-name 'name)))
                                                      space
                                                      (render-child 'r n) rparen))])])

(ag/two-infix + #:racr-name LargeAdd #:type large-int-type #:feature large-int)
(ag/two-infix - #:racr-name LargeSubtract #:type large-int-type #:feature large-int)
(ag/two-infix * #:racr-name LargeMultiply #:type large-int-type #:feature large-int)
(ag/two-arg safeLargeDivide #:type large-int-type #:feature large-int)
(ag/two-arg safeLargeModulo #:type large-int-type #:feature large-int)
;(ag/two-arg safeLargeRemainder #:type large-int-type #:feature large-int)
;(ag/two-arg safeLargeQuotient #:type large-int-type #:feature large-int)
(ag/two-arg LargeInt.min #:racr-name LargeMin #:type large-int-type #:feature large-int)
(ag/two-arg LargeInt.max #:racr-name LargeMax #:type large-int-type #:feature large-int)
(ag/one-arg ~ #:racr-name LargeNegate #:type large-int-type #:feature large-int)
(ag/one-arg LargeInt.abs #:racr-name LargeAbs #:type large-int-type #:feature large-int)
(ag/one-arg LargeInt.sign #:racr-name LargeSign #:type large-int-type #:feature large-int)
(ag/two-arg LargeInt.sameSign #:racr-name LargeSameSign #:feature large-int
            #:type bool-type #:ctype (E2ctype large-int-type large-int-type))
;(ag/two-arg safeLargeFmt #:type byte-string-type #:ctype (E2ctype large-int-type large-int-type))

(ag/one-arg safeCharSucc #:type byte-char-type)
(ag/one-arg safeCharPred #:type byte-char-type)
(ag/one-arg Char.contains
            #:type (function-type byte-char-type bool-type)
            #:ctype (Ectype byte-string-type))
(ag/one-arg Char.notContains
            #:type (function-type byte-char-type bool-type)
            #:ctype (Ectype byte-string-type))
(ag/one-arg Char.toLower #:type byte-char-type)
(ag/one-arg Char.toUpper #:type byte-char-type)
(ag/one-arg Char.isAlpha #:type bool-type #:ctype (Ectype byte-char-type))
(ag/one-arg Char.isAlphaNum #:type bool-type #:ctype (Ectype byte-char-type))
(ag/one-arg Char.isAscii #:type bool-type #:ctype (Ectype byte-char-type))
;; Some implementations treat all characters above 127 as control characters
;; and return true for Char.isCntrl, while others return false...
;(ag/one-arg Char.isCntrl #:type bool-type #:ctype (Ectype byte-char-type))
(ag/one-arg Char.isDigit #:type bool-type #:ctype (Ectype byte-char-type))
(ag/one-arg Char.isGraph #:type bool-type #:ctype (Ectype byte-char-type))
(ag/one-arg Char.isHexDigit #:type bool-type #:ctype (Ectype byte-char-type))
(ag/one-arg Char.isLower #:type bool-type #:ctype (Ectype byte-char-type))
(ag/one-arg Char.isUpper #:type bool-type #:ctype (Ectype byte-char-type))
(ag/one-arg Char.isPrint #:type bool-type #:ctype (Ectype byte-char-type))
(ag/one-arg Char.isSpace #:type bool-type #:ctype (Ectype byte-char-type))
(ag/one-arg Char.isPunct #:type bool-type #:ctype (Ectype byte-char-type))
;; fromString - probably don't bother
;; fromCString - probably don't bother
;; toCString - ?

(ag/two-arg safeStringSub #:type byte-char-type
            #:ctype (E2ctype byte-string-type small-int-type))
;; TODO - extract NONE
;; TODO - extract SOME
;; TODO - substring
(ag/two-infix ^ #:racr-name StringConcatTwo #:type byte-string-type
              #:ctype (E2ctype byte-string-type byte-string-type))
(ag/one-arg String.concat #:type byte-string-type
            #:ctype (Ectype (immutable (list-type byte-string-type))))
;; TODO - concatWith
(ag/converter String.str byte-char-type byte-string-type)
(ag/one-arg String.implode #:type byte-string-type
            #:ctype (Ectype (immutable (list-type byte-char-type))))
(ag/one-arg String.explode #:type (immutable (list-type byte-char-type))
            #:ctype (Ectype byte-string-type))
(ag/one-arg String.map #:type (function-type byte-string-type
                                             byte-string-type)
            #:ctype (Ectype (function-type byte-char-type byte-char-type)))
(ag/one-arg String.translate #:type (function-type byte-string-type byte-string-type)
            #:ctype (Ectype (function-type byte-char-type
                                           byte-string-type)))
(ag/one-arg String.tokens #:type (function-type byte-string-type
                                                (immutable (list-type byte-string-type)))
            #:ctype (Ectype (function-type byte-char-type
                                           bool-type)))
(ag/one-arg String.fields #:type (function-type byte-string-type
                                                (immutable (list-type byte-string-type)))
            #:ctype (Ectype (function-type byte-char-type
                                           bool-type)))
;; TODO - isPrefix
;; TODO - compare
;; TODO - collate

(define-syntax-parser ag/comparison
  [(_ name racr-name type (~or (~optional (~seq #:feature feature-arg))) ...)
   #'(ag/two-infix name
                   #:racr-name racr-name #:type bool-type #:ctype (E2ctype type type)
                   (~? (~@ #:feature feature-arg)))])
(ag/comparison = SmallIntEqual small-int-type)
(ag/comparison <> SmallIntNotEqual small-int-type)
(ag/comparison < SmallIntLess small-int-type)
(ag/comparison <= SmallIntLessEqual small-int-type)
(ag/comparison > SmallIntGreater small-int-type)
(ag/comparison >= SmallIntGreaterEqual small-int-type)

(ag/comparison = LargeIntEqual large-int-type #:feature large-int)
(ag/comparison <> LargeIntNotEqual large-int-type #:feature large-int)
(ag/comparison < LargeIntLess large-int-type #:feature large-int)
(ag/comparison <= LargeIntLessEqual large-int-type #:feature large-int)
(ag/comparison > LargeIntGreater large-int-type #:feature large-int)
(ag/comparison >= LargeIntGreaterEqual large-int-type #:feature large-int)

(ag/comparison = CharEqual byte-char-type)
(ag/comparison <> CharNotEqual byte-char-type)
(ag/comparison < CharLess byte-char-type)
(ag/comparison <= CharLessEqual byte-char-type)
(ag/comparison > CharGreater byte-char-type)
(ag/comparison >= CharGreaterEqual byte-char-type)

(ag/comparison = StringEqual byte-string-type)
(ag/comparison <> StringNotEqual byte-string-type)
(ag/comparison < StringLess byte-string-type)
(ag/comparison <= StringLessEqual byte-string-type)
(ag/comparison > StringGreater byte-string-type)
(ag/comparison >= StringGreaterEqual byte-string-type)

(ag/converter Int.toLarge
              small-int-type large-int-type
              #:feature large-int)
(ag/converter safeLargeIntToSmallInt large-int-type small-int-type #:feature large-int)
(ag/converter size byte-string-type small-int-type
              #:racr-name StringSize)
(ag/converter Char.toString byte-char-type byte-string-type)
(ag/converter LargeInt.toString large-int-type byte-string-type #:feature large-int)
(ag/converter Int.toString small-int-type byte-string-type)
(ag/converter Char.ord byte-char-type small-int-type)
(ag/converter safeChr small-int-type byte-char-type)



(ag/one-arg List.null #:type bool-type
            #:ctype (Ectype (immutable (list-type (fresh-type-variable))))
            #:racr-name ListNull)
(ag/one-arg List.length
            #:type small-int-type
            #:ctype (Ectype (immutable (list-type (fresh-type-variable)))))
(ag/two-infix @ #:racr-name ListAppend
                #:type (immutable (list-type (fresh-type-variable))))
(ag/two-arg safeLast #:type (fresh-type-variable)
            #:ctype (λ (n t) (hash 'l (immutable (list-type t))
                                   'r t)))
;; TODO List.getItem
(ag/three-arg safeListNth #:type (fresh-type-variable)
              #:ctype (λ (n t) (hash 'l (immutable (list-type t))
                                     'm small-int-type
                                     'r t)))
(ag/two-arg safeListTake #:type (immutable (list-type (fresh-type-variable)))
            #:ctype (λ (n t) (hash 'l t
                                   'r small-int-type)))
(ag/two-arg safeListDrop #:type (immutable (list-type (fresh-type-variable)))
            #:ctype (λ (n t) (hash 'l t
                                   'r small-int-type)))
(ag/one-arg List.rev #:type (immutable (list-type (fresh-type-variable))))
(ag/one-arg List.concat #:type (immutable (list-type (fresh-type-variable)))
            #:ctype (λ (n t) (hash 'Expression (immutable (list-type t)))))
(ag/two-arg List.revAppend #:type (immutable (list-type (fresh-type-variable))))
(ag/one-arg List.app
            #:type (function-type
                    (immutable (list-type (fresh-type-variable)))
                    void-type)
            #:ctype (λ (n t)
                      (define inner (fresh-type-variable))
                      (unify! (function-type
                               (immutable (list-type inner))
                               void-type)
                              t)
                      (hash 'Expression (function-type inner
                                                       void-type))))
(ag/one-arg List.map
            #:type (function-type
                    (immutable (list-type (fresh-type-variable)))
                    (immutable (list-type (fresh-type-variable))))
            #:ctype (λ (n t)
                      (define inner-l (fresh-type-variable))
                      (define inner-r (fresh-type-variable))
                      (unify! (function-type
                               (immutable (list-type inner-l))
                               (immutable (list-type inner-r)))
                              t)
                      (hash 'Expression (function-type inner-l inner-r))))
(add-property comp choice-weight [ListDotmap 200])
;; TODO - List.mapPartial (needs option type)
;; TODO - List.find (needs option type)
(ag/one-arg List.filter
            #:type (let ([inner (fresh-type-variable)])
                     (function-type
                      (immutable (list-type inner))
                      (immutable (list-type inner))))
            #:ctype (λ (n t)
                      (define inner (fresh-type-variable))
                      (unify! (function-type
                               (immutable (list-type inner))
                               (immutable (list-type inner)))
                              t)
                      (hash 'Expression (function-type inner
                                                       bool-type))))
;; TODO - List.partition
;; TODO - List.foldl
;; TODO - List.foldr
;; TODO - List.exists
;; TODO - List.all
;; TODO - List.tabulate


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
(* Begin \"safe math\" boilerplate definitions *)

fun safeSmallAdd(a, b) =
if Int.sameSign(a, b)
then
  (if ((0 < a) andalso ((max_as_small - a) < b))
   then a
   else if (0 > a) andalso ((min_as_small - a) > b) then a else a + b)
else a + b

fun safeSmallSubtract(a, b) =
if Int.sameSign(a, b)
then a - b
else
  (if ((b <= 0) andalso ((max_as_small + b) < a))
   then a
   else if (a <= 0) andalso ((max_as_small + a) < b) then a else a - b)

fun safeSmallMultiply(a, b) =
if (a = 0 orelse b = 0 orelse a = 1 orelse b = 1) then a * b else
(* just rule out max values because they aren't safe in the abs function *)
if (a = max_as_small orelse
    a = min_as_small orelse
    b = max_as_small orelse
    b = min_as_small)
  then a else
(* Now that max/min are out, let's short circuit on -1 so we can divide. *)
if (a = ~1) orelse (b = ~1) then a * b else
if Int.sameSign(a, b)
then
  if (a > 0) andalso ((max_as_small div b) > a) then a * b else
  if (max_as_small div (abs b) > (abs a)) then a * b else a
else
  if (a > 0) andalso ((min_as_small div b) > (abs a)) then a * b else
  if (a < 0) andalso (max_as_small div a) > b then a * b else a

fun safeSmallNegate(x) =
if x = min_as_small then min_as_small else ~x
fun safeSmallAbs(x) =
(* I want to use safeSmallAbs in other functions to ensure I have a non-negative *)
if x = min_as_small then 0 else abs x

fun safeSmallDivide(a, b) =
if (b = 0 orelse (a = min_as_small andalso b = ~1)) then a else a div b

fun safeSmallModulo(a, b) =
(* Return 0 when b is 0 so I can use it in other safe functions. *)
if b = 0 then 0 else a mod b

fun safeSmallRemainder(a, b) =
if b = 0 then a else Int.rem(a, b)

fun safeSmallQuotient(a, b) =
if (b = 0 orelse (a = min_as_small andalso b = ~1)) then a else Int.quot(a, b)

fun safe_car(l, fallback) = if (null l) then fallback else (List.hd l)
fun safe_cdr(l, fallback) = if (null l) then fallback else (List.tl l)
fun safeLast(l, fallback) = if (null l) then fallback else (List.last l)
fun safeListNth(l, index, fallback) = if (null l) then fallback
  else let val i = index mod (List.length l) in List.nth(l, i) end
fun safeListTake(l, amount) = List.take(l, safeSmallModulo(amount, (List.length l)))
fun safeListDrop(l, amount) = List.drop(l, safeSmallModulo(amount, (List.length l)))

fun safeStringSub(s, i) =
if (String.size s) = 0 then #\"a\"
  else String.sub(s, (safeSmallModulo(safeSmallAbs(i), (String.size s))))

")

;; These make safe math wrappers for fixed-width integers that rely on LargeInt
;; to do the tests.  Easier to write, but I can't use them for implementations
;; that don't support LargeInt...
;(define (make-safe-math-infix/non-div name op)
;  (format "
;fun ~a(a, b) = let
;  val largeResult : LargeInt.int = Int.toLarge(a) ~a Int.toLarge(b)
;  in if ((largeResult > max_as_large) orelse (largeResult < min_as_large))
;     then a else a ~a b
;  end
;"
;          name op op))
;(define (make-safe-math-infix/div name op)
;  (format "
;fun ~a(a, b) = if 0 = b then a
;  else let
;  val largeResult : LargeInt.int = Int.toLarge(a) ~a Int.toLarge(b)
;  in if largeResult > max_as_large orelse largeResult < min_as_large
;     then a else a ~a b
;  end
;"
;          name op op))
;(define (make-safe-math-prefix/div name op)
;  (format "
;fun ~a(a, b) = if 0 = b then a
;  else let
;  val largeResult : LargeInt.int = ~a(Int.toLarge(a), Int.toLarge(b))
;  in if largeResult > max_as_large orelse largeResult < min_as_large
;     then a else ~a(a, b)
;  end
;"
;          name op op))
;(define (make-safe-math-prefix/single name op)
;  (format "
;fun ~a(a) = let
;  val largeResult : LargeInt.int = ~a(Int.toLarge(a))
;  in if largeResult > max_as_large orelse largeResult < min_as_large
;     then 0 else ~a(a)
;  end
;"
;          name op op))

(define safe-math-tests
  ;; These should cause no exceptions.
  (let ([test-constants (list "max_as_small"
                              "min_as_small"
                              "0"
                              "~1"
                              "1"
                              "10"
                              "~10"
                              "(max_as_small - 1000)"
                              "(min_as_small + 1000)")])
    (string-join
     (flatten
      (list
       (for/list ([i test-constants])
         (format "val _ = safeSmallAbs(~a)" i)
         (format "val _ = safeSmallNegate(~a)" i)
         (for/list ([j test-constants])
           (list
            (format "val _ = safeSmallAdd(~a, ~a)" i j)
            (format "val _ = safeSmallSubtract(~a, ~a)" i j)
            (format "val _ = safeSmallMultiply(~a, ~a)" i j)
            (format "val _ = safeSmallDivide(~a, ~a)" i j)
            (format "val _ = safeSmallModulo(~a, ~a)" i j)
            (format "val _ = safeSmallRemainder(~a, ~a)" i j)
            (format "val _ = safeSmallQuotient(~a, ~a)" i j)
            )))))
     "\n")))

(define (header-definitions-block)
  (string-join
   `(,(format "val max_as_small : int = ~a" max-small-int)
     ,(format "val min_as_small : int = ~~~a" (abs min-small-int))
     ,@(if (xsmith-feature-enabled? 'large-int)
           (list
            (format "val max_as_large : LargeInt.int = ~a" max-small-int)
            (format "val min_as_large : LargeInt.int = ~~~a" (abs min-small-int))
            "fun safeLargeDivide(a, b) = if 0 = b then a else a div b"
            "fun safeLargeModulo(a, b) = if 0 = b then a else a mod b"
            ;"fun safeLargeRemainder(a, b) = if 0 = b then a else a rem b"
            ;"fun safeLargeQuotient(a, b) = if 0 = b then a else quot(a, b)"
            "
fun safeLargeIntToSmallInt(x : LargeInt.int) =
  if x > 0 then LargeInt.toInt(x mod max_as_large)
     else LargeInt.toInt(x mod min_as_large)
"
            )
           '())
     ;,(make-safe-math-infix/non-div "safeSmallAdd" "+")
     ;,(make-safe-math-infix/non-div "safeSmallSubtract" "-")
     ;,(make-safe-math-infix/non-div "safeSmallMultiply" "*")
     ;,(make-safe-math-infix/div "safeSmallDivide" "div")
     ;,(make-safe-math-prefix/div "safeSmallQuotient" "quot")
     ;,(make-safe-math-prefix/single "safeSmallNegate" "~")
     ;,(make-safe-math-prefix/single "safeSmallAbs" "abs")

     ;,(make-safe-math-infix/div "safeSmallModulo" "mod")
     ;,(make-safe-math-infix/div "safeSmallRemainder" "rem")

     ,header-definitions-block*

     ,(format "fun safeChr(x) = let val ax = safeSmallAbs(x) in chr(ax mod ~a) end"
              max-byte-char)
     ,(format "fun safeCharSucc(c) = if c = chr(~a) then c else Char.succ(c)"
              max-byte-char)
     ,(format "fun safeCharPred(c) = if c = chr(0) then c else Char.pred(c)")
     ;,safe-math-tests
     "\n\n\n\n\n"
     "(* The randomly generated program starts here! *)"
     "\n\n\n\n\n"
     )
   "\n"))

(define (render-child cname node)
  (att-value 'xsmith_render-node (ast-child cname node)))







(define (list-add-between ls between)
  (cond [(null? ls) ls]
        [(null? (cdr ls)) ls]
        [else (cons (car ls)
                    (cons between
                          (list-add-between (cdr ls) between)))]))

(define (type->string t)
  ;; concretize and unify, just in case.
  (define parametric-type-hash (make-hasheq))
  ;; 26 names should be enough.
  (define parametric-type-names
    '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
  (define parametric-name-index 0)
  (define (rec t*)
    (define t (concretize-type t*))
    (unify! t t*)
    (cond
      [(can-unify? t large-int-type) "LargeInt.int"]
      [(can-unify? t small-int-type) "int"]
      [(can-unify? t byte-string-type) "string"]
      [(can-unify? t byte-char-type) "char"]
      [(can-unify? t wide-string-type) "WideString.string"]
      [(can-unify? t byte-char-type) "WideChar.char"]
      [(can-unify? t bool-type) "bool"]
      [(can-unify? t void-type) "unit"]
      [(parameter-type? t)
       (hash-ref! parametric-type-hash t
                  (λ () (begin0
                            (format "'~a"
                                    (symbol->string
                                     (list-ref parametric-type-names
                                               parametric-name-index)))
                          (set! parametric-name-index
                                (add1 parametric-name-index)))))]
      [(can-unify? t (box-type (fresh-type-variable)))
       (define inner (fresh-type-variable))
       (unify! (box-type inner) t)
       (format "(~a ref)" (rec inner))]
      [(can-unify? t (product-type #f))
       (define inners (product-type-inner-type-list t))
       (if (null? inners)
           "unit"
           (format "(~a)" (string-join (map rec inners) " * ")))]
      [(can-unify? t (function-type (fresh-type-variable) (fresh-type-variable)))
       (define ret (fresh-type-variable))
       (define arg (fresh-type-variable))
       (unify! t (function-type arg ret))
       (format "(~a -> ~a)" (rec arg) (rec ret))]
      [(can-unify? t (immutable (list-type (fresh-type-variable))))
       (define inner (fresh-type-variable))
       (unify! t (immutable (list-type inner)))
       (format "(~a list)" (rec inner))]
      [else (error 'standard-ml_type->string
                   "Type not implemented yet: ~v" t)]))
  (rec t))

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
           [(can-unify? type byte-char-type)
            (text "Char.toString")]
           [(can-unify? type wide-char-type)
            (text "WideChar.toString")]
           [(can-unify? type byte-string-type)
            ;; we could just do the identity function, but
            ;; String.toString escapes non-printing characters.
            (text "String.toString")]
           [(can-unify? type wide-string-type)
            (text "WideString.toString")]
           [(can-unify? type (box-type (fresh-type-variable)))
            (define inner (fresh-type-variable))
            (unify! (box-type inner) type)
            (h-append (text (format "fn x : ~a => " (type->string type)))
                      lparen (get-string-converter inner) (text " (!x)") rparen)]
           [(or (can-unify? type void-type)
                (can-unify? type (product-type (list))))
            (text "fn x : unit => \"\"")]
           [(can-unify? type (product-type (list (fresh-type-variable))))
            ;; This is just an artifact of the Xsmith type system, SML doesn't
            ;; support products with a single element.
            (define inner (fresh-type-variable))
            (unify! type (product-type (list inner)))
            (get-string-converter inner)]
           [(can-unify? type (product-type #f))
            (define ct (concretize-type type))
            (define inners (product-type-inner-type-list ct))
            ;; Well, there's a way to get them out.
            ;; But now I'm feeling lazy.  It's late.  I just want to get this going.
            (text (format "fn x : ~a => \"Tuples should be printed better than this.\""
                          (type->string type)))]
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
     (nest nest-step
           (v-append
            (text "fun main() = let\n")
            (text (header-definitions-block))
            (vb-concat
             `(,(text "")
               ,(text "")
               ,@(map (λ (cn) (att-value 'xsmith_render-node cn))
                      definitions)
               ,(h-append (text "val mainresult = ")
                          (render-child 'ExpressionSequence n))))
            (text "")
            (print-value (text "mainresult")
                         (att-value 'xsmith_type
                                    (ast-child 'ExpressionSequence n)))
            (apply v-append
                   (map (λ (v)
                          (print-value (text (ast-child 'name v))
                                       (ast-child 'type v)))
                        definitions))))
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
            (render-child 'finalexpression n)))
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
                              (render-child 'Expression n)))]
 [DefinitionNoRhs (λ (n) (text (ast-child 'name n)))]


 [VariableReference (λ (n) (text (format "~a" (ast-child 'name n))))]

 [VoidExpression (λ (n) (text "((ref 0) := 0)"))]

 [ProcedureApplicationSingle
  (λ (n) (h-append lparen
                   (render-child 'procedure n)
                   space
                   ;; I can get rid of these extra parens if I am more careful
                   ;; about making sure everything is outer-parenthesized elsewhere.
                   lparen
                   (render-child 'argument n)
                   rparen
                   rparen))]
 [FormalParameter
  (λ (n) (h-append
          (text (format "~a" (ast-child 'name n)))
          space colon space
          (text (type->string (ast-child 'type n)))))]
 [LambdaSingleWithExpression
  (λ (n) (h-append lparen (text "fn ")
                   (render-child 'parameter n)
                   (text " => ")
                   (render-child 'body n)
                   rparen))]

 [BoolLiteral (λ (n) (text (if (ast-child 'v n) "true" "false")))]
 [Not (λ (n) (h-append lparen (text "not") lparen
                       (att-value 'xsmith_render-node (ast-child 'Expression n))
                       rparen rparen))]
 [And (binary-op-renderer (text "andalso"))]
 [Or (binary-op-renderer (text "orelse"))]


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
   (if (xsmith-feature-enabled? 'large-int)
       (λ()large-int-type)
       #f)
   (λ()bool-type)
   (λ()byte-char-type)
   (λ()byte-string-type)
   ;; poly/ML doesn't seem to support wide chars and wide strings
   ;(λ()wide-char-type)
   ;(λ()wide-string-type)
   (λ()(box-type (fresh-type-variable)))
   (λ()(immutable (list-type (fresh-type-variable))))
   ))

(define (sml-format-render doc)
  (pretty-format doc 120)
  ;"program here"
  )

(define-xsmith-interface-functions
  [comp]
  #:fuzzer-name simple-sml
  #:fuzzer-version xsmith-examples-version-string/no-name
  #:type-thunks type-thunks-for-concretization
  #:program-node ProgramWithSequence
  #:format-render sml-format-render
  #:comment-wrap (λ (lines) (format "(*\n~a\n*)" (string-join lines "\n")))
  #:features ([large-int #f] [parametric-types #f])
  #:default-max-depth 7
  )

(module+ main (simple-sml-command-line))
