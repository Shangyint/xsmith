#lang clotho

(require
 (except-in xsmith module)
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 racket/string
 racket/list
 racket/pretty
 racket/match
 syntax/parse/define
 "../private/util.rkt"
 "../private/xsmith-examples-version.rkt"
 (for-syntax
  clotho/racket/base
  syntax/parse
  racket/match
  ))

;; Likely I'll want to fuzz both with and without exceptions.  Let's make it toggleable.
;; TODO - this should be backed by a parameter or something so I can set it as a command-line option.
(define NE? #t)

(define random-max 4294967087)

(define (moreargs-default)
  (random 5))

(define bc-max-fixnum (- (expt 2 62) 1))
(define bc-min-fixnum (- (expt 2 62)))
(define cs-max-fixnum (- (expt 2 60) 1))
(define cs-min-fixnum (- (expt 2 60)))
(define interesting-integers
  (list bc-max-fixnum bc-min-fixnum cs-max-fixnum cs-min-fixnum
        ;; are the smallest bignums interesting, or just the biggest fixnums?
        (add1 bc-max-fixnum) (sub1 bc-min-fixnum)
        (add1 cs-max-fixnum) (sub1 cs-min-fixnum)
        0
        -1
        1
        ))


(define (biased-random-char*)
  ;; Random-char very rarely generates ascii, which is more common.
  ;; More saliently, low-value characters are interned in Racket and
  ;; high-value characters are not.  So I want to be sure to generate
  ;; both to have some variety.
  (if (random-bool)
      (random-char)
      (random-char-in-range (range 0 128))))
(define (biased-random-string*)
  (random-expr
   (random-string)
   (random-string-from-char-producing-proc
    (λ () (random-char-in-range (range 0 128))))
   (random-string-from-char-producing-proc biased-random-char)))

;; For now, to stop getting the same bug all the time, don't produce
;; characters that trip up bugs we've already found.
(define (biased-random-char)
  (let ([c (biased-random-char*)])
    (if (or (eq? c #\;) (eq? c #\,))
        (biased-random-char)
        c)))
(define (biased-random-string)
  ;; Semicolon and backtick had printing issues in racketCS.
  ;; Period had printing issues in racketBC.
  ;; The uFEFF character caused a read/write bug in 7.9.
  ;; Hash doesn't have a bug per se, but racketCS quotes symbols
  ;; that start with # by either putting them in pipes or using backslashes
  ;; (it seems to use backslashes when the symbol starts with #%, otherwise
  ;; pipes).  So it gives spurious output mismatches.
  (string-replace (biased-random-string*) #px";|`|#|\\.|\uFEFF" "a"))

(define (biased-random-int)
  ;; The random function returns word-sized integers.
  ;; I want more variety, like bigints.
  (random-expr
   (random-int)
   (+ (* (random-int) (random-int)) (random-int))
   (+ (* (random-int) (random-int) (random-int) (random-int)) (random-int))
   (random-byte)
   (random 10)
   (random-ref interesting-integers)
   ))
(define (biased-random-nat)
  (abs (biased-random-int)))
(define (random-byte) (random 256))
(define (random-byte-string-length) (random 30))
(define (random-byte-string)
  (bytes->immutable-bytes
   (list->bytes
    (for/list ([i (in-range (random-byte-string-length))])
      (random-byte)))))

(define-generic-type box-type ([type covariant]))
(define bool (base-type 'bool #:leaf? #f))

(define number (base-type 'number bool #:leaf? #f))
(define complex (base-type 'complex number #:leaf? #f))
(define real (base-type 'real complex #:leaf? #f))
(define rational (base-type 'rational real #:leaf? #f))
(define int (base-type 'int rational #:leaf? #f))
(define nat (base-type 'nat int #:leaf? #f))
(define byte (base-type 'byte nat #:leaf? #t))

(define char (base-type 'char bool))
(define string (base-type 'string bool #:leaf? #f))
(define mutable-string (base-type 'mutable-string string))
(define immutable-string (base-type 'immutable-string string))
(define bytes (base-type 'bytes bool #:leaf? #f))
(define mutable-bytes (base-type 'mutable-bytes bytes))
(define immutable-bytes (base-type 'immutable-bytes bytes))
(define symbol (base-type 'symbol bool))
(define keyword (base-type 'keyword bool))
(define date (base-type 'date bool #:leaf? #f))
(define date* (base-type 'date* date))
;; for literal #t and #f
(define exact-bool (base-type 'exact-bool bool))



(define-basic-spec-component racket-comp)

(add-basic-expressions racket-comp
                       #:ProgramWithSequence #t
                       #:VariableReference #t
                       #:ProcedureApplication #t
                       #:VoidExpression #t
                       #:AssignmentExpression #t
                       #:IfExpression #t
                       #:ExpressionSequence #t
                       #:LetSequential #t
                       #:LambdaWithExpression #t
                       ;#:Numbers #t
                       ;#:Booleans #t
                       ;#:Strings #t
                       #:ImmutableList #t
                       #:MutableArray #t
                       #:MutableArraySafeAssignmentExpression #t
                       #:ImmutableArray #t
                       #:MutableStructuralRecord #t
                       #:MutableStructuralRecordAssignmentExpression #t
                       #:ImmutableStructuralRecord #t
                       #:int-type int
                       #:bool-type exact-bool
                       )

(add-property
 racket-comp
 strict-child-order?
 ;; Racket enforces strict left-to-right evaluation for everything.
 ;; Well, unless you are using concurrency, but that's another can of worms.
 [#f #t])

(add-property
 racket-comp
 render-hole-info
 [#f (λ (h) '_HOLE_)])


(define-for-syntax (racr-ize-symbol sym)
  ;; Turn common racket identifiers into something RACR can deal with.
  (define split-chars (string->list "-<>?!*+/=0123456789"))

  (define-values (parts-rev dividers-rev)
    (let ()
      (define-values (parts dividers current-thing current-divider?)
        (for/fold ([parts '()]
                   [dividers '()]
                   [current ""]
                   [current-divider? #f])
                  ([c (string->list (symbol->string sym))])
          (define divider? (member c split-chars))
          (cond [(and divider? current-divider?)
                 (values parts dividers (string-append current (string c)) #t)]
                [divider?
                 (values (cons current parts) dividers (string c) #t)]
                [current-divider?
                 (values parts (cons current dividers) (string c) #f)]
                [else
                 (values parts dividers (string-append current (string c)) #f)])))
      (cond [current-divider? (values parts (cons current-thing dividers))]
            [else (values (cons current-thing parts) dividers)])))

  (define part-strings
    (map string-titlecase (reverse parts-rev)))
  (define divider-strings
    (map (λ (divider)
           (match divider
             ["->" "To"]
             ["/" "With"]
             ["?" "P"]
             ["!" "Bang"]
             ["0" "Zero"]
             ["1" "One"]
             ["2" "Two"]
             ["3" "Three"]
             ["4" "Four"]
             ["5" "Five"]
             ["6" "Six"]
             ["7" "Seven"]
             ["8" "Eight"]
             ["9" "Nine"]
             ["224-" "TwoTwentyFour"]
             ["256-" "TwoFiftySix"]
             ["*" "Star"]
             ["*?" "StarP"]
             ["*-" "Star"]
             [_ ""]))
         (reverse dividers-rev)))

  (define (merge-parts accum ps ds)
    (match (list ps ds)
      [(list (list) (list)) accum]
      [(list (list p ps ...) (list d ds ...))
       (merge-parts (string-append accum p d) ps ds)]
      [(list (list p ps ...) (list))
       (merge-parts (string-append accum p) ps null)]))

  (define converted-string (merge-parts "" part-strings divider-strings))
  (string->symbol converted-string))
(define-for-syntax (racr-ize-id id)
  (datum->syntax id
                 (racr-ize-symbol (syntax-e id))
                 id))

(define (render-child sym n)
  (att-value 'xsmith_render-node (ast-child sym n)))
(define (render-children sym n)
  (map (λ (cn) (att-value 'xsmith_render-node cn)) (ast-children (ast-child sym n))))
(define ((binary-op-renderer op) n)
  `(,op ,(render-child 'l n) ,(render-child 'r n)))

(define ((render-variadic variadic-function-name) n)
  `(,variadic-function-name ,@(render-children 'minargs n)
                            ,@(render-children 'moreargs n)))
(define no-child-types (λ (n t) (hash)))

(define-syntax-parser ag [(_ arg ...) #'(add-to-grammar racket-comp arg ...)])
(define-syntax-parser ap [(_ arg ...) #'(add-property racket-comp arg ...)])

(define-ag/one-arg ag/one-arg racket-comp racr-ize-id NE?
  #'(fresh-subtype-of number)
  (λ (name-thunk)
    (λ (n) `(,(name-thunk)
             ,(render-child 'Expression n)))))
(define-ag/two-arg ag/two-arg racket-comp racr-ize-id NE?
  #'(fresh-subtype-of number)
  (λ (name-thunk)
    (λ (n) `(,(name-thunk)
             ,(render-child 'l n)
             ,(render-child 'r n)))))
(define-ag/three-arg ag/three-arg racket-comp racr-ize-id NE?
  #'(fresh-subtype-of number)
  (λ (name-thunk)
    (λ (n) `(,(name-thunk)
             ,(render-child 'l n)
             ,(render-child 'm n)
             ,(render-child 'r n)))))


(ag
 [EmptyListLiteral Expression ()
                   #:prop choice-weight (depth-weight)
                   #:prop type-info [(immutable (list-type (fresh-type-variable)))
                                     no-child-types]
                   #:prop render-node-info (λ (n) 'null)]
 [MutableStringLiteral Expression ([v = (biased-random-string)])
                       #:prop choice-weight (depth-weight)
                       #:prop type-info [mutable-string no-child-types]
                       #:prop render-node-info (λ (n)
                                                 `(string-copy ,(ast-child 'v n)))]
 [MutableBytesLiteral Expression ([v = (random-byte-string)])
                      #:prop choice-weight (depth-weight)
                      #:prop type-info [mutable-bytes no-child-types]
                      #:prop render-node-info (λ (n)
                                                `(bytes-copy ,(ast-child 'v n)))]
 [DateLiteral Expression ([v = (random-int)])
              #:prop choice-weight (depth-weight)
              #:prop type-info [date* no-child-types]
              ;; OK, so I'm just using seconds->date.  Not literally a date literal.
              #:prop render-node-info (λ (n) `(NE/seconds->date ,(ast-child 'v n)))]
 [VariadicExpression Expression ([minargs : Expression *]
                                 [moreargs : Expression * = moreargs-default])
                     #:prop may-be-generated #f])


(define-syntax-parser ag/atomic-literal
  [(_ name:id type:expr fresh-expr:expr)
   #'(ag [name Expression ([v = fresh-expr])
               #:prop choice-weight (depth-weight)
               #:prop type-info [type no-child-types]
               #:prop render-node-info (λ (n) `(quote ,(ast-child 'v n)))])])
(ag/atomic-literal IntLiteral int (biased-random-int))
(ag/atomic-literal NatLiteral nat (biased-random-nat))
(ag/atomic-literal ByteLiteral byte (random-byte))
;; TODO - rational, real, complex literals.  Also, be sure I'm sometimes generating +/- infinity and NAN.
;; TODO - maybe also bias generation to make some special values more common, such as size boundary or bignum boundary numbers, null our boundary characters, NAN, etc, to be more likely for them to collide in weird ways.
(ag/atomic-literal CharLiteral char (biased-random-char))
(ag/atomic-literal BoolLiteral exact-bool (random-bool))
(ag/atomic-literal ImmutableStringLiteral immutable-string (biased-random-string))
(ag/atomic-literal ImmutableBytesLiteral immutable-bytes (random-byte-string))
(ag/atomic-literal SymbolLiteral symbol (string->symbol (biased-random-string)))
(ag/atomic-literal KeywordLiteral keyword (string->keyword (biased-random-string)))

;; Limit make-string length to a byte to prevent making ginormous strings.
(ag/one-arg make-string #:racr-name MakeStringOne
            #:type mutable-string
            #:ctype (Ectype byte))
(ag/two-arg make-string #:racr-name MakeStringTwo
            #:type mutable-string
            #:ctype (E2ctype byte char))
(ag/one-arg make-bytes #:racr-name MakeBytesOne
            #:type mutable-bytes
            #:ctype (Ectype byte))
(ag/two-arg make-bytes #:racr-name MakeBytesTwo
            #:type mutable-bytes
            #:ctype (E2ctype byte byte))

(define-syntax-parser ag/variadic
  [(_ racket-name:id min-args:expr
      (~or (~optional (~seq #:type type:expr)
                      #:defaults ([type #'(fresh-subtype-of number)]))
           (~optional (~seq #:ctype ctype:expr)
                      #:defaults ([ctype #'(λ (n t) (hash 'minargs t 'moreargs t))]))
           (~optional (~seq #:racr-name racr-name:id)
                      #:defaults ([racr-name (datum->syntax
                                              #'racket-name
                                              (racr-ize-symbol
                                               (syntax-e #'racket-name))
                                              #'racket-name)]))
           (~optional (~seq #:NE-name NE-name:id)
                      #:defaults ([NE-name #'racket-name]))
           (~optional (~seq #:feature feature-arg)))
      ...)
   #'(ag [racr-name VariadicExpression ()
                    #:prop fresh (hash 'minargs min-args)
                    #:prop type-info [type ctype]
                    (~? (~@ #:prop feature feature-arg))
                    #:prop render-node-info (render-variadic
                                             (if NE? 'NE-name 'racket-name))])])
(ag/variadic * 0 #:racr-name TimesNum #:type number)
(ag/variadic * 0 #:racr-name TimesReal #:type real)
(ag/variadic * 0 #:racr-name TimesInt #:type int)
(ag/variadic * 0 #:racr-name TimesNat #:type nat)
(ag/variadic + 0 #:racr-name PlusNum #:type number)
(ag/variadic + 0 #:racr-name PlusReal #:type real)
(ag/variadic + 0 #:racr-name PlusInt #:type int)
(ag/variadic + 0 #:racr-name PlusNat #:type nat)
(ag/variadic - 1 #:racr-name MinusNum #:type number)
(ag/variadic - 1 #:racr-name MinusReal #:type real)
(ag/variadic - 1 #:racr-name MinusInt #:type int)
(ag/variadic / 1 #:racr-name Divide #:NE-name NE/ #:type real)
(ag/variadic bitwise-and 0 #:type int)
(ag/variadic bitwise-ior 0 #:type int)
(ag/variadic bitwise-xor 0 #:type int)
(ag/variadic lcm 0 #:type int)
;; TODO - re-enable gcd when I start fuzzing versions where the bug is fixed.
;;        Turning it off for now to stop getting duplicates.
;(ag/variadic gcd 0 #:type int)
(ag/variadic min 1 #:racr-name MinReal #:type real)
(ag/variadic min 1 #:racr-name MinInt #:type int)
(ag/variadic min 1 #:racr-name MinNat #:type nat)
(ag/variadic min 1 #:racr-name MinByte #:type byte)
(ag/variadic max 1 #:racr-name MaxReal #:type real)
(ag/variadic max 1 #:racr-name MaxInt #:type int)
(ag/variadic max 1 #:racr-name MaxNat #:type nat)
(ag/variadic max 1 #:racr-name MaxByte #:type byte)
(ag/variadic append 0 #:type (immutable (list-type (fresh-type-variable))))
(ag/variadic string 0 #:type string
             #:ctype (λ (n t) (hash 'minargs char 'moreargs char)))
(ag/variadic bytes 0 #:type bytes
             #:ctype (λ (n t) (hash 'minargs byte 'moreargs byte)))
(ag/variadic string-append 0 #:type mutable-string
             #:ctype (λ (n t) (hash 'minargs string 'moreargs string)))
(ag/variadic bytes-append 0 #:type mutable-bytes
             #:ctype (λ (n t) (hash 'minargs bytes 'moreargs bytes)))

;; The numerical comparison operators require at least 1 argument.  I'm not sure why they don't accept 0 args -- eg. as a predicate that an empty list is sorted.
(define-syntax-parser ag/number-compare
  [(_ name:id symbol:expr)
   #'(ag [name VariadicExpression ()
               #:prop fresh (hash 'minargs 1)
               #:prop type-info
               [bool (λ (n t) (hash 'minargs real 'moreargs real))]
               #:prop render-node-info (render-variadic symbol)])])
(ag/number-compare LessThan '<)
(ag/number-compare LessThanEqual '<=)
(ag/number-compare NumericEqual '=)
(ag/number-compare GreaterThan '>)
(ag/number-compare GreaterThanEqual '>=)
(define-syntax-parser ag/char-compare
  [(_ name:id symbol:expr)
   #'(ag [name VariadicExpression ()
               #:prop fresh (hash 'minargs 1)
               #:prop type-info
               [bool (λ (n t) (hash 'minargs char 'moreargs char))]
               #:prop render-node-info (render-variadic symbol)])])
(ag/char-compare CharLessThan 'char<?)
(ag/char-compare CharLessThanEqual 'char<=?)
(ag/char-compare CharEqual 'char=?)
(ag/char-compare CharGreaterThan 'char>?)
(ag/char-compare CharGreaterThanEqual 'char>=?)
(ag/char-compare CharCILessThan 'char-ci<?)
(ag/char-compare CharCILessThanEqual 'char-ci<=?)
(ag/char-compare CharCIEqual 'char-ci=?)
(ag/char-compare CharCIGreaterThan 'char-ci>?)
(ag/char-compare CharCIGreaterThanEqual 'char-ci>=?)
(ag [SymbolLessThan VariadicExpression ()
                    #:prop fresh (hash 'minargs 1)
                    #:prop type-info
                    [bool (λ (n t) (hash 'minargs symbol 'moreargs symbol))]
                    #:prop render-node-info (render-variadic 'symbol<?)])
(define-syntax-parser ag/string-compare
  [(_ name:id symbol:expr)
   #'(ag [name VariadicExpression ()
               #:prop fresh (hash 'minargs 1)
               #:prop type-info
               [bool (λ (n t) (hash 'minargs string 'moreargs string))]
               #:prop render-node-info (render-variadic symbol)])])
(ag/string-compare StringLessThan 'string<?)
(ag/string-compare StringLessThanEqual 'string<=?)
(ag/string-compare StringEqual 'string=?)
(ag/string-compare StringGreaterThan 'string>?)
(ag/string-compare StringGreaterThanEqual 'string>=?)
(ag/string-compare StringCILessThan 'string-ci<?)
(ag/string-compare StringCILessThanEqual 'string-ci<=?)
(ag/string-compare StringCIEqual 'string-ci=?)
(ag/string-compare StringCIGreaterThan 'string-ci>?)
(ag/string-compare StringCIGreaterThanEqual 'string-ci>=?)
(define-syntax-parser ag/bytes-compare
  [(_ name:id symbol:expr)
   #'(ag [name VariadicExpression ()
               #:prop fresh (hash 'minargs 1)
               #:prop type-info
               [bool (λ (n t) (hash 'minargs bytes 'moreargs bytes))]
               #:prop render-node-info (render-variadic symbol)])])
(ag/bytes-compare BytesLessThan 'bytes<?)
(ag/bytes-compare BytesEqual 'bytes=?)
(ag/bytes-compare BytesGreaterThan 'bytes>?)


(ag/one-arg abs #:type real)
(ag/one-arg cos #:type number #:ctype (Ectype real) #:feature float)
(ag/one-arg acos #:type number #:ctype (Ectype real) #:feature float)
(ag/one-arg sin #:type number #:ctype (Ectype real) #:feature float)
(ag/one-arg asin #:type number #:ctype (Ectype real) #:feature float)
(ag/one-arg tan #:type number #:ctype (Ectype real) #:feature float)
(ag/one-arg atan #:racr-name AtanOne #:NE-name NE/atan-1 #:type number
            #:feature float
            #:ctype (Ectype real))
(ag/one-arg add1 #:racr-name AddOneNum #:type number)
(ag/one-arg add1 #:racr-name AddOneInt #:type int)
(ag/one-arg add1 #:racr-name AddOneNat #:type nat)
(ag/one-arg sub1 #:racr-name SubOneNum #:type number)
(ag/one-arg sub1 #:racr-name SubOneInt #:type int)
(ag/one-arg angle #:type real #:NE-name NE/angle #:feature float)
(ag/one-arg ceiling #:type real)
(ag/one-arg floor #:type real)
(ag/one-arg round #:type real)
(ag/one-arg truncate #:type real)
(ag/one-arg imag-part)
(ag/one-arg real-part)
;; TODO - magnitude requires the float feature if complex numbers are allowed.
(ag/one-arg magnitude #:feature float)
;; TODO - numerator and denominator take rational reals (not imaginaries or irrationals)
(ag/one-arg numerator #:type int)
(ag/one-arg denominator #:type int)
;; TODO - what should I do about exp, expt, arithmetic-shift, and other functions that potentially need a limited domain?  Make NE versions?

(ag/one-arg not #:type bool)
(ag/one-arg bitwise-not #:type int)
(ag/one-arg zero? #:type bool #:ctype (Ectype number))
(ag/one-arg null? #:type bool
            #:ctype (Ectype (immutable (list-type (fresh-type-variable)))))
(ag/one-arg symbol-interned? #:type bool #:ctype (Ectype symbol))
(ag/one-arg symbol-unreadable?
            #:type bool #:ctype (Ectype symbol))
(ag/one-arg integer-length #:type int)
(ag/one-arg even? #:type bool #:ctype (Ectype int))
(ag/one-arg odd? #:type bool #:ctype (Ectype int))
(ag/one-arg exact? #:type bool #:ctype (Ectype number))
(ag/one-arg inexact? #:type bool #:ctype (Ectype number))
(ag/one-arg exact-integer? #:type bool #:ctype (Ectype number))
(ag/one-arg exact-positive-integer? #:type bool #:ctype (Ectype number))
(ag/one-arg exact-nonnegative-integer? #:type bool #:ctype (Ectype number))
(ag/one-arg inexact-real? #:type bool #:ctype (Ectype number))
(ag/one-arg positive? #:type bool #:ctype (Ectype real))
(ag/one-arg negative? #:type bool #:ctype (Ectype real))
(ag/one-arg exact->inexact #:type number #:feature float)
(ag/one-arg inexact->exact #:type number)

(ag/one-arg char-downcase #:type char)
(ag/one-arg char-foldcase #:type char)
;; char-titlecase presumably has the same issue as string-titlecase, in that it is
;; a wont-fix issue in RacketBC.
;(ag/one-arg char-titlecase #:type char)
(ag/one-arg char-upcase #:type char)
(ag/one-arg char-utf-8-length #:type int #:ctype (Ectype char))
(ag/one-arg char-general-category #:type symbol
            #:ctype (Ectype char))

(ag/one-arg date*-nanosecond #:type number #:ctype (Ectype date*))
(ag/one-arg date*-time-zone-name #:type immutable-string #:ctype (Ectype date*))
(ag/one-arg date-day #:type int #:ctype (Ectype date))
(ag/one-arg date-dst? #:type bool #:ctype (Ectype date))
(ag/one-arg date-hour #:type int #:ctype (Ectype date))
(ag/one-arg date-minute #:type int #:ctype (Ectype date))
(ag/one-arg date-month #:type int #:ctype (Ectype date))
(ag/one-arg date-second #:type int #:ctype (Ectype date))
(ag/one-arg date-time-zone-offset #:type int #:ctype (Ectype date))
(ag/one-arg date-week-day #:type int #:ctype (Ectype date))
(ag/one-arg date-year #:type int #:ctype (Ectype date))
(ag/one-arg date-year-day #:type int #:ctype (Ectype date))

(define-syntax-parser ag/char-pred
  [(_ name:id)
   #'(ag/one-arg name #:type bool #:ctype (Ectype char))])
(ag/char-pred char-alphabetic?)
(ag/char-pred char-blank?)
(ag/char-pred char-graphic?)
(ag/char-pred char-iso-control?)
(ag/char-pred char-lower-case?)
(ag/char-pred char-numeric?)
(ag/char-pred char-punctuation?)
(ag/char-pred char-symbolic?)
(ag/char-pred char-title-case?)
(ag/char-pred char-upper-case?)
(ag/char-pred char-whitespace?)

(ag/one-arg string-length #:type int #:ctype (Ectype string))
(ag/one-arg bytes-length #:type int #:ctype (Ectype bytes))
(ag/one-arg string-utf-8-length
            #:type int #:ctype (Ectype string))
(ag/one-arg string-copy #:type mutable-string #:ctype (Ectype string))
(ag/one-arg bytes-copy #:type mutable-bytes #:ctype (Ectype bytes))

(ag/one-arg sha1-bytes #:type bytes #:ctype (Ectype bytes))
(ag/one-arg sha224-bytes #:type bytes #:ctype (Ectype bytes))
(ag/one-arg sha256-bytes #:type bytes #:ctype (Ectype bytes))

(ag/two-arg bytes-ref #:NE-name NE/bytes-ref #:type byte #:ctype (E2ctype bytes int))
(ag/two-arg string-ref #:NE-name NE/string-ref #:type char #:ctype (E2ctype string int))
;; For most languages I would probably need read/write of mutable strings to be annotated with effect-read-mutable-container (or write), but because Racket guarantees order of evaluation, I'm not going to worry about it here.
(ag/three-arg bytes-set! #:NE-name NE/bytes-set!
              #:type void-type
              #:ctype (E3ctype mutable-bytes int byte))
(ag/three-arg string-set! #:NE-name NE/string-set!
              #:type void-type
              #:ctype (E3ctype mutable-string int char))

;; It seems to be inconsistent whether these return mutable or immutable strings.
(ag/one-arg string-downcase #:type immutable-string #:ctype (Ectype string))
(ag/one-arg string-foldcase #:type immutable-string #:ctype (Ectype string))
;; String titlecase is buggy in RacketBC, but the fix is just to use CS instead, so I'm commenting it out of the fuzzer so I stop getting a deluge of string-titlecase bug reports.
;(ag/one-arg string-titlecase #:type immutable-string #:ctype (Ectype string))
(ag/one-arg string-upcase #:type immutable-string #:ctype (Ectype string))

;; In BC these may return a MUTABLE string that is EQ to the original.
;; In CS these (rightfully) return immutable strings.
(ag/one-arg string-normalize-nfc/wrap
            #:type immutable-string #:ctype (Ectype string))
(ag/one-arg string-normalize-nfd/wrap
            #:type immutable-string #:ctype (Ectype string))
(ag/one-arg string-normalize-nfkc/wrap
            #:type immutable-string #:ctype (Ectype string))
(ag/one-arg string-normalize-nfkd/wrap
            #:type immutable-string #:ctype (Ectype string))

(define-ag/converter ag/converter ag/one-arg)
(ag/converter char->integer char int)
(ag/converter number->string number string)
(ag/converter string->symbol string symbol)
(ag/converter string->uninterned-symbol string symbol)
(ag/converter string->unreadable-symbol string symbol)
(ag/converter symbol->string symbol string)
(ag/converter string->keyword string keyword)
(ag/converter keyword->string keyword string)
(ag/converter string->list string (immutable (list-type char)))
(ag/converter bytes->list bytes (immutable (list-type byte)))
(ag/converter list->string (immutable (list-type char)) string)
(ag/converter list->bytes (immutable (list-type byte)) bytes)
(ag/converter string->bytes/utf-8 string bytes)
(ag/converter string->immutable-string string immutable-string)
(ag/converter bytes->immutable-bytes bytes immutable-bytes)
;; TODO - should be real instead of int
;; TODO - needs a second boolean arg for whether it's local time (the default #t is local) -- or maybe I should always use UTC?
(ag/converter seconds->date real date* #:NE-name NE/seconds->date)
(ag/one-arg vector->list
            #:racr-name ImmutableVectorToList
            #:type (immutable (list-type (fresh-type-variable)))
            #:ctype (λ (n t)
                      (define inner-type (fresh-type-variable))
                      (unify! t (immutable (list-type inner-type)))
                      (hash 'Expression (immutable (array-type inner-type)))))
(ag/one-arg vector->list
            #:racr-name MutableVectorToList
            #:type (immutable (list-type (fresh-type-variable)))
            #:ctype (λ (n t)
                      (define inner-type (fresh-type-variable))
                      (unify! t (immutable (list-type inner-type)))
                      (hash 'Expression (mutable (array-type inner-type)))))
;; This one is kinda dumb, it probably checks and is just identity.
(ag/one-arg vector->immutable-vector
            #:racr-name ImmutableVectorToImmutableVector
            #:type (immutable (array-type (fresh-type-variable))))
(ag/one-arg vector->immutable-vector
            #:racr-name MutableVectorToImmutableVector
            #:type (immutable (array-type (fresh-type-variable)))
            #:ctype (λ (n t)
                      (define inner-type (fresh-type-variable))
                      (unify! t (immutable (array-type inner-type)))
                      (hash 'Expression (mutable (array-type inner-type)))))

(define ag/type-predicate-ctype (Ectype (fresh-type-variable)))
(define-syntax-parser ag/type-predicate
  [(_ name:id)
   #'(ag/one-arg name
                 #:type bool
                 #:ctype ag/type-predicate-ctype)])
(ag/type-predicate boolean?)
(ag/type-predicate box?)
(ag/type-predicate byte?)
(ag/type-predicate char?)
(ag/type-predicate complex?)
(ag/type-predicate date?)
(ag/type-predicate date*?)
(ag/type-predicate evt?)
(ag/type-predicate exn?)
(ag/type-predicate hash?)
;; some string functions apparently don't guarantee whether their return is mutable or immutable.  Eg. string-normalize-* return mutable strings in BC but immutable strings in CS.  Or it may be something more subtle, like sometimes returning the original string.  Anyway, this gives spurious differences.
;(ag/type-predicate immutable?)
(ag/type-predicate integer?)
(ag/type-predicate interned-char?)
(ag/type-predicate keyword?)
(ag/type-predicate list?)
(ag/type-predicate list-pair?)
(ag/type-predicate mpair?)
(ag/type-predicate number?)
(ag/type-predicate pair?)
(ag/type-predicate parameter?)
(ag/type-predicate parameterization?)
;; the path? predicate depends on the current system type, path-for-some-system? is system independent.
;(ag/type-predicate path?)
(ag/type-predicate path-for-some-system?)
(ag/type-predicate pregexp?)
(ag/type-predicate procedure?)
(ag/type-predicate rational?)
(ag/type-predicate real?)
(ag/type-predicate regexp?)
(ag/type-predicate string?)
(ag/type-predicate struct-type?)
(ag/type-predicate struct-type-property?)
(ag/type-predicate struct?)
(ag/type-predicate symbol?)
(ag/type-predicate syntax?)
(ag/type-predicate true-object?)
(ag/type-predicate vector?)
(ag/type-predicate void?)

(ag/one-arg box
            #:racr-name MutableBoxLiteral
            ;; TODO -- add weight arg to ag macros
            #:type (mutable (box-type (fresh-type-variable)))
            #:ctype (λ (n t)
                      (define inner-type (fresh-type-variable))
                      (unify! (mutable (box-type inner-type)) t)
                      (hash 'Expression inner-type)))
(ap wont-over-deepen [MutableBoxLiteral #t])
(ag/one-arg box-immutable #:racr-name ImmutableBoxLiteral
            ;; TODO -- add weight arg to ag macros
            #:type (immutable (box-type (fresh-type-variable)))
            #:ctype (λ (n t)
                      (define inner-type (fresh-type-variable))
                      (unify! (immutable (box-type inner-type)) t)
                      (hash 'Expression inner-type)))
(ap wont-over-deepen [ImmutableBoxLiteral #t])
(ag/one-arg unbox
            #:type (fresh-type-variable)
            #:ctype (λ (n t) (hash 'Expression (fresh-type-variable
                                                (immutable (box-type t))
                                                (mutable (box-type t))))))
(ap mutable-container-access [Unbox (read 'box)])
(ag [SetBoxBang Expression ([box : Expression] [newval : Expression])
                #:prop mutable-container-access (write 'box)
                #:prop type-info [void-type
                                  (λ (n t)
                                    (define inner-type (fresh-type-variable))
                                    (hash 'box (mutable (box-type inner-type))
                                          'newval inner-type))]
                #:prop render-node-info (λ (n) `(set-box! ,(render-child 'box n)
                                                          ,(render-child 'newval n)))])


(ag/two-arg equal?
            #:type bool
            #:ctype (λ (n t) (hash 'l (fresh-type-variable) 'r (fresh-type-variable))))
;; Don't fuzz `eqv?` or `eq?`, because various operations may return fresh or cached/interned versions of the result -- eg. sometimes string functions may return a fresh string or the same one as a previous operation.  These aren't intended to be stable between implementations or versions for the return values of many functions.
;(ag/two-arg eqv?
;            #:type bool
;            #:ctype (λ (n t) (hash 'l (fresh-type-variable) 'r (fresh-type-variable))))
;(ag/two-arg eq?
;            #:type bool
;            #:ctype (λ (n t) (hash 'l (fresh-type-variable) 'r (fresh-type-variable))))
(ag/two-arg make-polar
            #:type number
            ;; TODO - should be real real, once I fix the numeric tower
            #:ctype (E2ctype int int)
            #:feature float)
(ag/two-arg make-rectangular
            #:type number
            ;; TODO - should be real real, once I fix the numeric tower
            #:ctype (E2ctype int int))


;;;;; Paths
;; TODO - path related functions may accept path objects, strings or bytestrings that can be converted to paths, 'up or 'same symbols corresponding to ".." and "." paths on unix, or path components, which are paths with only one part.  Paths may be unix paths or windows paths.
;; You can tell if a path is for unix or windows with `path-convention-type`.
;; To build windows and unix paths explicitly, use bytes->path or string->some-system-path, both take 'windows or 'unix as the second argument.  Then combine paths with `build-path/convention-type`.  Converting from bytes/strings will need some safe wrapper to avoid exceptions from invalid paths.
;; I need a safe string/byte -> path function and safe relative-absolute converters.
;; TODO - should I allow path-strings?  Just paths is perhaps an easier place to start.
;; paths can be for windows or unix, and can be absolute or relative.
;; I'll do relative/absolute as base type subtypes, and OS type as a wrapper.
;; TODO - maybe I should not bother with subtyping of paths, since I basically always need to strictly separate windows and unix paths.
(define path (base-type 'path #:leaf? #f))
(define windows-path (base-type 'windows-path path))
(define unix-path (base-type 'unix-path path))
(define (fresh-path)
  (fresh-type-variable windows-path unix-path))
;; path-symbol is for APIs that accept 'up or 'same symbols.
(define path-symbol (base-type 'path-symbol))
(ag/atomic-literal PathSymbol path-symbol (random-expr 'up 'same))

;; build-path also allows 'up and 'same
;; build-path does the convention of the machine it's run on.  Maybe I'll ignore it for now?
(ag/variadic build-path/windows 1
             #:type windows-path
             #:ctype (λ (n t) (hash 'minargs windows-path
                                    'moreargs (fresh-type-variable
                                               path-symbol
                                               windows-path))))
(ag/variadic build-path/unix 1
             #:type unix-path
             #:ctype (λ (n t) (hash 'minargs unix-path
                                    'moreargs (fresh-type-variable
                                               path-symbol
                                               unix-path))))
(ag/one-arg bytes->path/windows #:type windows-path #:ctype (Ectype bytes) #:shallow #t)
(ag/one-arg bytes->path/unix #:type unix-path #:ctype (Ectype bytes) #:shallow #t)
(ag/one-arg string->path/windows #:type windows-path #:ctype (Ectype string))
(ag/one-arg string->path/unix #:type unix-path #:ctype (Ectype string))
(ag/one-arg cleanse-path #:type (fresh-path))
(ag/one-arg simplify-path/no-fs #:type (fresh-path))
(ag/one-arg relative-path? #:type bool #:ctype (Ectype (fresh-path)))
(ag/one-arg absolute-path? #:type bool #:ctype (Ectype (fresh-path)))
(ag/one-arg complete-path? #:type bool #:ctype (Ectype (fresh-path)))
(ag/one-arg explode-path
            #:type (immutable (list-type (fresh-path)))
            #:ctype (λ (n t)
                      (define inner (fresh-type-variable))
                      (unify! t (immutable (list-type inner)))
                      (hash 'Expression inner)))
(ag/one-arg path->bytes #:type bytes #:ctype (Ectype (fresh-path)))
(ag/one-arg path->directory-path #:type (fresh-path))
(ag/one-arg path-convention-type #:type symbol #:ctype (Ectype (fresh-path)))
;; TODO - actually this is variadic and requires 1 arg.
;; path<? only allows `path?`, which means a path for the current system.  Most path functions actually take a `path-for-some-system?` instead.
;(ag/two-arg path<? #:type bool #:ctype (E2ctype (fresh-path) (fresh-path)))
(ag/one-arg path->complete-path/single #:type (fresh-path))
(ag/two-arg path->complete-path/double #:type (fresh-path))
;; TODO - split-path -- it returns 3 values, it's fairly complicated.
;; TODO - bytes->path-element, string->path-element -- these require that the path only be a single element.  Somewhat more difficult.
;; TODO path-element->bytes
;; TODO path-element->string


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Rendering for basic expressions (IE add-basic-expression)
;;;;; Mostly this is from the simple/racket fuzzer.

(define-syntax (define-quoted-and-pass-through stx)
  ;; To be able to test that my submodule of wrappers compiles at compile time,
  ;; rather than waiting until I compile a generated test.
  (syntax-parse stx
    [(_ name inner)
     #'(begin
         (define name 'inner)
         inner)]))

(define-quoted-and-pass-through wrapper-submodule
  (module wrappers-and-helpers racket/base
    (provide (all-defined-out))
    (require
     racket/match
     racket/format
     racket/string
     racket/path
     )
    (define-values (NE/)
      (λ (arg1 . args)
        (if (null? args)
            (if (eq? arg1 0)
                0
                (/ arg1))
            (apply / arg1 (map (λ (x) (if (eq? 0 x) 1 x)) args)))))
    ;; TODO - add safe/NoException wrappers
    (define-values (NE/atan-2)
      (λ (x y)
        (if (equal? x 0)
            0
            (if (equal? y 0)
                0
                (atan x y)))))
    (define-values (NE/atan-1)
      (λ (x)
        (if (equal? x 0+1i)
            0
            (if (equal? x 0-1i)
                0
                (atan x)))))
    (define (NE/angle x)
      (if (equal? 0 x)
          0
          (angle x)))
    (define (NE/bytes-ref bytes index)
      (define l (bytes-length bytes))
      (if (zero? l)
          0
          (bytes-ref bytes (modulo index l))))
    (define (NE/string-ref string index)
      (define l (string-length string))
      (if (zero? l)
          #\a
          (string-ref string (modulo index l))))
    (define (NE/bytes-set! bytes index new)
      (define l (bytes-length bytes))
      (if (zero? l)
          (void)
          (bytes-set! bytes (modulo index l) new)))
    (define (NE/string-set! string index new)
      (define l (string-length string))
      (if (zero? l)
          (void)
          (string-set! string (modulo index l) new)))
    (define (NE/seconds->date x)
      (define (sd x)
        ;; Don't use local time.
        (seconds->date x #f))
      ;; The range is apparently OS-dependent.  I binary searched the bounds on my machine, but that was probably not very useful.
      ;; The range is -67768040609715604 to 67768036191701999 inclusive, at least in racket 7.6-bc.
      ;(define min-second -67768040609715604)
      ;(define max-second 67768036191701999)
      ;; Hopefully these bounds should work for any setup, as it's only a few hundred years in either direction.
      (define min-second -10000000000)
      (define max-second 10000000000)
      (cond [(< x min-second) (sd (modulo (truncate x) min-second))]
            [(> x max-second) (sd (modulo (truncate x) max-second))]
            [else (sd x)])
      )
    (define (string-normalize-nfd/wrap str)
      (string-normalize-nfd (string-copy str)))
    (define (string-normalize-nfkd/wrap str)
      (string-normalize-nfkd (string-copy str)))
    (define (string-normalize-nfc/wrap str)
      (string-normalize-nfc (string-copy str)))
    (define (string-normalize-nfkc/wrap str)
      (string-normalize-nfkc (string-copy str)))
    (define-values (safe-car)
      (λ (list fallback)
        (if (null? list)
            fallback
            (car list))))
    (define-values (safe-cdr)
      (λ (list fallback)
        (if (null? list)
            fallback
            (cdr list))))
    (define (NE/vector-ref vec index fallback)
      (if (eq? 0 (vector-length vec))
          fallback
          (vector-ref vec (modulo index (vector-length vec)))))
    (define (immutable-vector-set vec index-raw val)
      (if (eq? 0 (vector-length vec))
          vec
          (let ([index (modulo index-raw (vector-length vec))])
            (vector->immutable-vector
             (build-vector (vector-length vec)
                           (λ (i) (if (equal? i index)
                                      val
                                      (vector-ref vec i))))))))
    (define (NE/vector-set! vec index val)
      (if (eq? 0 (vector-length vec))
          (void)
          (vector-set! vec (modulo index (vector-length vec)) val)))

    (define (string-path-prep s)
      (define (string-filter-nulls s)
        (string-replace s "\0" ""))
      (define (string->non-empty s)
        (if (equal? s "")
            "a"
            s))
      (string->non-empty (string-filter-nulls s)))
    (define (bytes-path-prep s)
      (define (bytes-filter-nulls bs)
        (list->bytes (filter (λ (b) (not (eq? b 0))) (bytes->list bs))))
      (define (bytes->non-empty s)
        (if (equal? s #"")
            #"a"
            s))
      (bytes->non-empty (bytes-filter-nulls s)))

    (define (string->path/windows s)
      (string->some-system-path (string-path-prep s) 'windows))
    (define (string->path/unix s)
      (string->some-system-path (string-path-prep s) 'unix))

    (define (path-relativize p [backup #f])
      (if (or (symbol? p)
              (relative-path? p))
          p
          (let ([ps (explode-path p)]
                [convention (path-convention-type p)])
            (if (null? (cdr ps))
                (or backup
                    (if (eq? convention 'unix)
                        (build-path/convention-type 'unix "a-relative-path")
                        (build-path/convention-type 'windows "a-relative-path")))
                (apply build-path (cdr ps))))))

    (define (bytes->path/windows s)
      (bytes->path (bytes-path-prep s) 'windows))
    (define (bytes->path/unix s)
      (bytes->path (bytes-path-prep s) 'unix))

    (define (build-path/windows one . others)
      (apply build-path/convention-type 'windows one (map path-relativize others)))
    (define (build-path/unix one . others)
      (apply build-path/convention-type 'unix one (map path-relativize others)))
    (define (simplify-path/no-fs p)
      (simplify-path p #f))

    (define (path->complete-path/single p)
      (define convention (path-convention-type p))
      (define base (if (eq? convention 'unix)
                       (bytes->path #"/" 'unix)
                       (bytes->path #"C:\\" 'windows)))
      (path->complete-path p base))
    (define (path->complete-path/double p base)
      (path->complete-path p (path->complete-path/single base)))

    (define (format-round x)
      (if (or (equal? x +inf.0)
              (equal? x -inf.0)
              (equal? x +nan.0))
          x
          (~a #:max-width +inf.0 (inexact->exact (round x)))))

    (define (my-format val #:mutables [mutables '()])
      (define (rec x)
        (my-format x #:mutables mutables))
      (define (my-format/hash-inner the-hash)
        (define (hash-sort-lt l r)
          (match (list l r)
            [(list (? number?) (? number?)) (< l r)]
            [(list (? string?) (? string?)) (string<? l r)]
            [(list (? symbol?) (? symbol?)) (string<? (symbol->string l)
                                                      (symbol->string r))]
            [(list (? keyword?) (? keyword?)) (string<? (keyword->string l)
                                                        (keyword->string r))]
            [_ (error 'fuzzer-format "TODO - add more ways to sort for hash tables.  Given: ~v and ~v" l r)]))
        (string-join
         (for/list ([k (sort (hash-keys the-hash) hash-sort-lt)])
           (format "[~a . ~a]"
                   (rec k)
                   (rec (hash-ref the-hash k))))))
      (define (mutable? x)
        (not (immutable? x)))
      (define (mutable-container? x)
        (and (mutable? x)
             (or (box? x)
                 (vector? x)
                 (hash? x))))
      (define mutable-index
        (and (mutable-container? val)
             (let ([memq-result (memq val mutables)])
               (and memq-result (length memq-result)))))
      (when (and (mutable? val) (not mutable-index))
        (set! mutables (cons val mutables)))
      (match val
        ;; The type system ought to prevent any cyclic data structures...
        ;; Yet I got a couple in a big fuzz campaign, so to guard against
        ;; programs failing to terminate while printing due to cyclic
        ;; data, I'm adding this mutable container guard.
        [(? (λ (x) mutable-index))
         (format "#{mutable-object-already-seen: ~a}" mutable-index)]
        [(list v ...) (format "(~a)" (string-join (map rec v)))]
        [(and (? immutable?) (vector v ...))
         (format "#{vector-immutable ~a}" (string-join (map rec v)))]
        [(vector v ...)
         (format "#{vector-mutable ~a}" (string-join (map rec v)))]
        [(and (? immutable?) (? hash?) (? hash-eq?))
         (format "#{immutable-hasheq ~a}" (my-format/hash-inner val))]
        [(and (? immutable?) (? hash?) (? hash-eqv?))
         (format "#{immutable-hasheqv ~a}" (my-format/hash-inner val))]
        [(and (? immutable?) (? hash?) (? hash-equal?))
         (format "#{immutable-hashequal ~a}" (my-format/hash-inner val))]
        [(and (? mutable?) (? hash?) (? hash-eq?))
         (format "#{mutable-hasheq ~a}" (my-format/hash-inner val))]
        [(and (? mutable?) (? hash?) (? hash-eqv?))
         (format "#{mutable-hasheqv ~a}" (my-format/hash-inner val))]
        [(and (? mutable?) (? hash?) (? hash-equal?))
         (format "#{mutable-hashequal ~a}" (my-format/hash-inner val))]
        [(and (? immutable?) (box v))
         (format "#{immutable-box ~a}" (rec v))]
        [(and (? mutable?) (box v))
         (format "#{mutable-box ~a}" (rec v))]
        [(? procedure?)
         ;; Different versions of Racket can have different results for
         ;; `object-name`, so let's just print all procedures equally.
         (format "#<procedure>")]
        [(and (? number?)
              (? exact?)
              (or (? integer?) (? rational?)))
         (~a val)]
        ;; For floating point numbers, let's round them to ameliorate minor
        ;; differences...
        [(? real?) (format-round val)]
        ;; For complex numbers, let's round both parts.
        [(? number?) (format "#{complex ~a ~a}"
                             (format-round (real-part val))
                             (format-round (imag-part val)))]
        [(? path-for-some-system?)
         (define pc (path-convention-type val))
         (format "#<~a-path ~v>" pc (path->bytes val))]
        [(or (? void?)
             (? string?)
             (? bytes?)
             (? symbol?)
             (? keyword?)
             #t #f
             (? char?)
             ;; Dates are structs, but they can only contain atomic data.
             ;; So we don't need to worry about applying a specialized
             ;; printer recursively.
             (? date?)
             )
         (~v val)]))
    (define (my-print x)
      (println (my-format x)))
    ))

(add-property
 racket-comp
 render-node-info

 [ProgramWithSequence
  (λ (n)
    `(
      ;; I'm sick of trying to write wrappers in #lang kernel.
      ;; Let's use a submodule in racket/base instead.
      ,wrapper-submodule
      (#%require (submod "." wrappers-and-helpers))
      ,@(render-children 'definitions n)
      (define-values (program-result) ,(render-child 'ExpressionSequence n))
      (begin
        ,(if #;(base-type? (att-value 'xsmith_type
                                      (ast-child 'ExpressionSequence n)))
             #t
             '(printf "Program body result: ~v\n" (my-format program-result))
             '(void))
        ,@(for/list ([c (ast-children (ast-child 'definitions n))]
                     #:when #t #;(base-type? (concretize-type
                                              (att-value 'xsmith_type c)))
                     )
            `(printf "Var ~a: ~a\n"
                     ',(string->symbol (ast-child 'name c))
                     (my-format ,(string->symbol (ast-child 'name c))))))))]

 [Definition (λ (n)
               `(define-values (,(string->symbol (ast-child 'name n)))
                  ,(render-child 'Expression n)))]
 [AssignmentExpression
  (λ (n) `(set! ,(string->symbol (ast-child 'name n))
                ,(render-child 'newvalue n)))]
 [ExpressionSequence
  (λ (n)
    `(begin
       ,@(render-children 'effectexpressions n)
       ,(render-child 'finalexpression n)))]
 [LetSequential
  (λ (n)
    (define let-pairs
      (map (λ (dn) `[(,(string->symbol (ast-child 'name dn)))
                     ,(render-child 'Expression dn)])
           (ast-children (ast-child 'definitions n))))
    (foldr (λ (v accum)
             `(let-values (,v) ,accum))
           (render-child 'body n)
           let-pairs))]

 [IfExpression
  (λ (n)
    `(if ,(render-child 'test n)
         ,(render-child 'then n)
         ,(render-child 'else n)))]

 [VariableReference (λ (n) (string->symbol (ast-child 'name n)))]

 [ProcedureApplication
  (λ (n)
    `(#%app ,(render-child 'procedure n)
            ,@(render-children 'arguments n)))]
 [FormalParameter (λ (n) (string->symbol (ast-child 'name n)))]
 [LambdaWithExpression
  (λ (n)
    `(lambda (,@(render-children 'parameters n))
       ,(render-child 'body n)))]

 ;[BoolLiteral (λ (n) (not (not (ast-child 'v n))))]
 ;[Not (λ (n) `(not ,(render-child 'Expression n)))]
 ;[And (binary-op-renderer 'and)]
 ;[Or (binary-op-renderer 'or)]

 ;[IntLiteral (λ (n) (ast-child 'v n))]
 ;[Plus (binary-op-renderer '+)]
 ;[Minus (binary-op-renderer '-)]
 ;[Times (binary-op-renderer '*)]
 ;[LessThan (binary-op-renderer '<)]
 ;[GreaterThan (binary-op-renderer '>)]
 ;[SafeDivide (binary-op-renderer 'safe-divide)]

 ;[StringLiteral (λ (n) (ast-child 'v n))]
 ;[StringAppend (binary-op-renderer 'string-append)]
 ;[StringLength (λ (n) `(string-length ,(render-child 'Expression n)))]

 [ImmutableListLiteral
  (λ (n) `(list ,@(render-children 'expressions n)))]
 [ImmutableListSafeCar
  (λ (n) `(safe-car ,(render-child 'list n)
                    ,(render-child 'fallback n)))]
 [ImmutableListSafeCdr
  (λ (n) `(safe-cdr ,(render-child 'list n)
                    ,(render-child 'fallback n)))]
 [ImmutableListCons
  (λ (n) `(cons ,(render-child 'newvalue n)
                ,(render-child 'list n)))]
 [MutableArrayLiteral
  (λ (n) `(vector ,@(render-children 'expressions n)))]
 [ImmutableArrayLiteral
  (λ (n) `(vector->immutable-vector (vector ,@(render-children 'expressions n))))]
 [MutableArraySafeReference
  (λ (n) `(NE/vector-ref ,(render-child 'array n)
                         ,(render-child 'index n)
                         ,(render-child 'fallback n)))]
 [ImmutableArraySafeReference
  (λ (n) `(NE/vector-ref ,(render-child 'array n)
                         ,(render-child 'index n)
                         ,(render-child 'fallback n)))]
 [MutableArraySafeAssignmentExpression
  (λ (n)
    `(NE/vector-set! ,(render-child 'array n)
                     ,(render-child 'index n)
                     ,(render-child 'newvalue n)))]
 [ImmutableArraySafeSet
  (λ (n)
    `(immutable-vector-set ,(render-child 'array n)
                           ,(render-child 'index n)
                           ,(render-child 'newvalue n)))]

 [MutableStructuralRecordLiteral
  (λ (n)
    `(make-hash (list ,@(map (λ (name val)
                               `(cons ',name
                                      ,(att-value 'xsmith_render-node val)))
                             (ast-child 'fieldnames n)
                             (ast-children (ast-child 'expressions n))))))]
 [ImmutableStructuralRecordLiteral
  (λ (n)
    `(hash ,@(apply append
                    (map (λ (name val)
                           `(',name
                             ,(att-value 'xsmith_render-node val)))
                         (ast-child 'fieldnames n)
                         (ast-children (ast-child 'expressions n))))))]
 [MutableStructuralRecordReference
  (λ (n)
    `(hash-ref ,(render-child 'record n)
               ',(ast-child 'fieldname n)))]
 [ImmutableStructuralRecordReference
  (λ (n)
    `(hash-ref ,(render-child 'record n)
               ',(ast-child 'fieldname n)))]
 [MutableStructuralRecordAssignmentExpression
  (λ (n)
    `(hash-set! ,(render-child 'record n)
                ',(ast-child 'fieldname n)
                ,(render-child 'newvalue n)))]
 [ImmutableStructuralRecordSet
  (λ (n)
    `(hash-set ,(render-child 'record n)
               ',(ast-child 'fieldname n)
               ,(render-child 'newvalue n)))]
 [VoidExpression (λ (n) '(void))]
 )


;; This is a quick hack that removes lift chains.
;; I'm not sure that I want to commit to this generally.
;; We could add it to canned-components or something.
;; But maybe we occasionally DO want a definition to have an immediate reference as its RHS.
;; So I would rather try to do something that limits lift chains to a certain (small) depth.
(add-choice-method
 racket-comp
 no-ref-on-def-rhs
 [#f (λ () #t)]
 [VariableReference
  (λ ()
    (not (equal? (ast-node-type (parent-node (current-hole)))
                 'Definition)))])
(add-property racket-comp
              choice-filters-to-apply
              [#f (no-ref-on-def-rhs)])



(define (type-thunks-for-concretization)
  (list
   (λ()bool)
   (λ()number)
   (λ()int)
   (λ()char)
   (λ()string)
   (λ()bytes)
   (λ()symbol)
   (λ()keyword)
   (λ() (immutable (list-type (fresh-type-variable))))
   (λ() (immutable (box-type (fresh-type-variable))))
   (λ() (mutable (box-type (fresh-type-variable))))
   ))

(define (racket-format-render s-exps)
  (define out (open-output-string))
  (for ([symex s-exps])
    (pretty-print symex out 1))
  (format "\n(module random-fuzzing-module '#%kernel\n~a\n)\n"
          (get-output-string out)))

(define-xsmith-interface-functions
  [racket-comp]
  #:fuzzer-name racket-kernel
  #:fuzzer-version xsmith-examples-version-string/no-name
  #:program-node ProgramWithSequence
  #:type-thunks type-thunks-for-concretization
  #:format-render racket-format-render
  #:features ([float #f])
  #:comment-wrap (λ (lines) (string-join (map (λ (l) (format ";; ~a" l)) lines)
                                         "\n"))

  )

(module+ main (racket-kernel-command-line))
