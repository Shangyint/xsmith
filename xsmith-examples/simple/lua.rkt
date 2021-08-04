#lang clotho

(require
 xsmith
 xsmith/app
 racr
 xsmith/racr-convenience
 xsmith/canned-components
 (except-in pprint empty)
 racket/string
 racket/list
 racket/match
 racket/format
 "../private/xsmith-examples-version.rkt"
 "../private/util.rkt"
 )

(define-basic-spec-component lua-comp)

(add-basic-expressions lua-comp
                       #:LambdaWithBlock #t
                       #:VariableReference #t
                       #:ProcedureApplication #t
                       #:Numbers #t
                       #:int-literal-value (biased-random-int)
                       #:Booleans #t
                       #:Strings #t
                       #:string-literal-value (random-byte-string)
                       #:MutableArray #t
                       #:MutableStructuralRecord #t
                       )
(add-basic-statements lua-comp
                      #:ProgramWithBlock #t
                      #:Block #t
                      #:ReturnStatement #t
                      #:IfElseStatement #t
                      #:AssignmentStatement #t
                      #:NullStatement #t
                      #:MutableArraySafeAssignmentStatement #t
                      #:MutableStructuralRecordAssignmentStatement #t
                      )


(define (random-byte-list-length) (random 30))
(define (random-byte) (random 256))
(define (random-byte-string)
  (bytes->immutable-bytes (apply bytes
                                 (map (λ (x) (random-byte))
                                      (make-list (random-byte-list-length) #f)))))

;; TODO - find the right numbers for these.
(define float-int-min (- (expt 2 30)))
(define float-int-max (expt 2 30))

(define (random-int-in-bounds)
  (let ([x (random-int)])
    (if (< x 0)
        (modulo x float-int-min)
        (modulo x float-int-max))))
(define (biased-random-int)
  (random-expr
   (random-int-in-bounds)
   (random 255)
   (random-expr 0 -1 1 float-int-min float-int-max)
   ))


(add-property
 lua-comp
 choice-weight
 [ProcedureApplication 50])

(define nest-step 4)
(define (binary-op-renderer op-rendered)
  (λ (n) (h-append lparen ($xsmith_render-node (ast-child 'l n))
                   space op-rendered space
                   ($xsmith_render-node (ast-child 'r n)) rparen)))
(define (binary-func-renderer op-rendered)
  (λ (n) (h-append
          lparen
          op-rendered lparen ($xsmith_render-node (ast-child 'l n))
          comma space
          ($xsmith_render-node (ast-child 'r n)) rparen
          rparen)))
(add-property
 lua-comp
 render-hole-info
 [#f (λ (h) (text "«HOLE»"))])

(define (comma-list doc-list)
  (apply h-append
         (apply-infix (h-append comma space)
                      doc-list)))

(define (lua-render-let varname binding-expr body-expr)
  (h-append lparen lparen (text (format "function(~a) return " varname))
            body-expr
            (text " end") rparen
            lparen binding-expr rparen rparen))
(define (lua-render-let-return-void varname binding-expr body-expr)
  (h-append lparen lparen (text (format "function(~a) " varname))
            body-expr
            (text " end") rparen
            lparen binding-expr rparen rparen))

(define (lua-string-format byte-string)
  (format "\"~a\""
          (apply string-append
                 (for/list ([b byte-string])
                   (if (and (< 31 b 127)
                            (not (string-contains? "\"'[]\\"
                                                   (string (integer->char b)))))
                       (string (integer->char b))
                       (format "\\x~a" (~r #:base 16
                                           #:min-width 2
                                           #:pad-string "0"
                                           b)))))))

(define header-definitions-block-1
  (format
   "
our_int_min = ~a
our_int_max = ~a
"
   float-int-min
   float-int-max
   ))
(define header-definitions-block-2
  ;; TODO - the modulo definition maybe needs to change based on lua version.  Is there a way to do conditional evaluation based on which lua implementation I'm in?
  ;; This modulo definition is from the Lua reference manual:
  ;; http://www.lua.org/manual/5.2/manual.html#3.4.1
  "

safe_add = function(a, b)
  local result = a + b
  if (result > our_int_max) or (result < our_int_min) then
    return a
  else
    return result
  end
end
safe_subtract = function(a, b)
  local result = a - b
  if (result > our_int_max) or (result < our_int_min) then
    return a
  else
    return result
  end
end
safe_multiply = function(a, b)
  local result = a * b
  if (result > our_int_max) or (result < our_int_min) then
    return a
  else
    return result
  end
end
safe_divide = function(a, b)
  if b == 0 then
    return a
  else
    local result = a / b
    if (result > our_int_max) or (result < our_int_min) then
      return a
    else
      return result
    end
  end
end
safe_modulo = function(a, b)
  if b == 0 then
    return 0
  else
    return a % b
  end
end
--modulo = function(a, b) return a - math.floor(a/b)*b end

array_modulo = function(a, b)
  return math.abs(safe_modulo(a,b))
end

function mutable_array_safe_reference(array, index, fallback)
  if (#array == 0) then
    return fallback
  else
    return array[array_modulo(index, #array) + 1]
  end
end
function mutable_array_safe_assignment(array, index, newvalue)
  if (#array == 0) then
    return
  else
    array[array_modulo(index, #array) + 1] = newvalue
    return
  end
end

function form(x)
  local t = type(x)
  if \"nil\" == t then
    return \"nil\"
  elseif \"function\" == t then
    return t
  elseif \"string\" == t then
    return string.format(\"%q\", x)
  elseif \"number\" == t then
    -- Large numbers can't be printed by %d,
    -- but also will print differently in different implementations with %q.
    -- TODO I'm not sure where the cutoff is, or how to effectively print big numbers.
    if math.abs(x) < 10 ^ 14 then return tostring(x) else return \"BIGnumber\" end
  elseif \"boolean\" == t then
    if x then return \"true\" else return \"false\" end
  elseif \"table\" == t then
    local acc = \"{\"
    for index, value in pairs(x) do
      acc = acc .. \"[\" .. form(index) .. \"] = \" .. form(value) .. \", \"
    end
    acc = acc .. \"}\"
    return acc
  else
    return t
  end
end

expression_statement_dummy_var = 0;

")

(add-property
 lua-comp
 render-node-info

 [ProgramWithBlock
  (λ (n)
    (define definitions (ast-children (ast-child 'definitions n)))
    (v-append
     (text (string-append
            header-definitions-block-1
            header-definitions-block-2
            ))
     (text "\n\n\n\n")
     (text "-------- Randomly generated program starts here --------")
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
            (map (λ (v) (text (format "print(form(~a))\n"
                                      (ast-child 'name v))))
                 definitions
                 #;(filter (λ (x) (base-type? (ast-child 'type x)))
                         definitions)))
     ;; Hack to get a newline...
     (text "")))]

 [Definition (λ (n)
               (h-append (text (ast-child 'name n))
                         space
                         equals
                         space
                         ($xsmith_render-node (ast-child 'Expression n))))]

 [Block
  (λ (n)
    (h-append
     (text "do")
     (nest nest-step
           (h-append
            line
            (v-concat
             (append
              (map (λ (cn) ($xsmith_render-node cn))
                   (ast-children (ast-child 'definitions n)))
              (map (λ (cn) ($xsmith_render-node cn))
                   (ast-children (ast-child 'statements n)))))))
     line
     (text "end")))]

 ;; Lua doesn't actually allow expression statements.  Let's hack around that with an assignment to a dummy variable that is never read.
 #;[ExpressionStatement (λ (n) (h-append (text "expression_statement_dummy_var = ")
                                       ($xsmith_render-node (ast-child 'Expression n))
                                       semi))]

 [ReturnStatement (λ (n) (h-append (text "return ")
                                   ($xsmith_render-node (ast-child 'Expression n))))]

 ;; TODO - I'm not sure how to do a null statement in lua offhand.
 [NullStatement (λ (n) (text "expression_statement_dummy_var = 0;"))]
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
               (text "then"))
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
     (text "else")
     (nest nest-step
           (h-append line
                     (v-concat
                      (let ([b (ast-child 'else n)])
                        (append
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'definitions b)))
                         (map (λ (cn) ($xsmith_render-node cn))
                              (ast-children (ast-child 'statements b))))))))
     line
     (text "end")))]



 [VariableReference (λ (n) (text (format "~a" (ast-child 'name n))))]

 [ProcedureApplication
  (λ (n) (h-append ($xsmith_render-node (ast-child 'procedure n))
                   lparen
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'arguments n))))
                   rparen))]
 [FormalParameter (λ (n) (text (format "~a" (ast-child 'name n))))]
 [LambdaWithBlock
  (λ (n) (h-append lparen
                   (text "function") lparen
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'parameters n))))
                   rparen
                   space
                   ($xsmith_render-node (ast-child 'body n))
                   space
                   (text "end")
                   rparen))]

 [BoolLiteral (λ (n) (text (if (ast-child 'v n) "true" "false")))]
 [Not (λ (n) (h-append (text "not") lparen
                       ($xsmith_render-node (ast-child 'Expression n))
                       rparen))]
 [And (binary-op-renderer (text "and"))]
 [Or (binary-op-renderer (text "or"))]

 [IntLiteral (λ (n) (text (format "~a" (ast-child 'v n))))]
 [Plus (binary-func-renderer (text "safe_add"))]
 [Minus (binary-func-renderer (text "safe_subtract"))]
 [Times (binary-func-renderer (text "safe_multiply"))]
 [LessThan (binary-op-renderer (text "<"))]
 [GreaterThan (binary-op-renderer (text ">"))]

 [SafeDivide (binary-func-renderer (text "safe_divide"))]

 [StringLiteral (λ (n) (text (lua-string-format (ast-child 'v n))))]
 [StringAppend (binary-op-renderer (text ".."))]
 [StringLength (λ (n) (h-append (text "string.len")
                                lparen
                                ($xsmith_render-node (ast-child 'Expression n))
                                rparen))]

 [MutableArrayLiteral
  (λ (n) (h-append lparen lbrace
                   (comma-list (map (λ (cn) ($xsmith_render-node cn))
                                    (ast-children (ast-child 'expressions n))))
                   rbrace rparen))]
 [MutableArraySafeReference
  ;; Lua's array index should start at 1.  And we should define a modulus function, one of the many... frustrating parts of lua is that there wasn't a built-in modulus operator until recently.  So to fuzz older versions we need to define a function for it.
  (λ (n)
    (h-append (text "mutable_array_safe_reference(")
              ($xsmith_render-node (ast-child 'array n))
              (text ", ")
              ($xsmith_render-node (ast-child 'index n))
              (text ", ")
              ($xsmith_render-node (ast-child 'fallback n))
              (text ")")))]
 [MutableArraySafeAssignmentStatement
  (λ (n)
    (h-append (text "mutable_array_safe_assignment(")
              ($xsmith_render-node (ast-child 'array n))
              (text ", ")
              ($xsmith_render-node (ast-child 'index n))
              (text ", ")
              ($xsmith_render-node (ast-child 'newvalue n))
              (text ")")))]

 [MutableStructuralRecordLiteral
  (λ (n)
    (h-append lparen lbrace
              (comma-list (map (λ (fieldname expression-node)
                                 (h-append (text (format "~a" fieldname))
                                           equals
                                           ($xsmith_render-node expression-node)))
                               (ast-child 'fieldnames n)
                               (ast-children (ast-child 'expressions n))))
              rbrace rparen))]
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
   (λ()(mutable (fresh-structural-record-type)))
   ))


(define (lua-format-render doc)
  (pretty-format doc 120))

(define-xsmith-interface-functions
  [lua-comp]
  #:fuzzer-name simple-lua
  #:fuzzer-version xsmith-examples-version-string/no-name
  #:type-thunks type-thunks-for-concretization
  #:program-node ProgramWithBlock
  #:format-render lua-format-render
  #:comment-wrap (λ (lines) (string-join (map (λ (l) (format "-- ~a" l)) lines)
                                         "\n")))

(module+ main (simple-lua-command-line))
