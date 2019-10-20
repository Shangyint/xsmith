
#lang racket/base

(require
 xsmith
 pprint)

(add-to-grammar
 pycalc-grammar
 [Program #f (Block Return)]
 [Block #f ([decls : Decl *]
            [calcs : Calc *])]
 [Decl #f (name type Expr)]
 [Calc #f ()]
 [Stmt Calc ()]
 [Assign Stmt (name Val)]
 [CalcAssign Assign ([op : Op])]
 [ForLoop Stmt (name
                [low : Expr]
                [high : Expr]
                [body : Block])]
 [IfElse Stmt ([test : Expr]
               [then : Block]
               [else : Block])]
 [Expr Calc ()]
 [OpExpr Expr ([lhs : Expr]
               [rhs : Expr]
               [op : Op])]
 [Op #f ()]
 [Add Op ()]
 [Sub Op ()]
 [Mul Op ()]
 [Div Op ()]
 ;; Values
 [Val Expr ()]
 [BoolVal Val ()]
 [IntVal Val ()]
 [Return #f (Expr)]
 )

(add-prop
 pycalc-grammar
 render-hole
 [#f (λ (h) (node-type h))])  ;; TODO - is this valid?

(define (tab d)
  (indent 4 d))

(add-prop
 pycalc-grammar
 render-node
 #:render-hole (λ (h) (text (pycalc-render-hole h)))
 [Program (λ (n)
            (v-append
             (pycalc-render (ast-child 'Block n))
             (pycalc-render (ast-child 'Return n))))]
 [Block (λ (n)
          (v-concat
           (append
            (map pycalc-render (ast-children (ast-child 'decls n)))
            (map pycalc-render (ast-children (ast-child 'calcs n))))))]
 [Decl (λ (n)
         (hs-append
          (text (ast-child 'name n))
          (text "=")
          (pycalc-render (ast-child 'Val n))))]
 [Assign (λ (n)
           (hs-append
            (text (ast-child 'name n))
            (text "=")
            (pycalc-render (ast-child 'Val n))))]
 [CalcAssign (λ (n)
               (hs-append
                (text (ast-child 'name n))
                (pycalc-render (ast-child 'op n))
                (pycalc-render (ast-child 'Val n))))]
 [ForLoop (λ (n)
            (v-append
             (h-append
              (text "for ")
              (text (ast-child 'name n))
              (text " in range(")
              (pycalc-render (ast-child 'low n))
              (text ", ")
              (pycalc-render (ast-child 'high n))
              (text "):"))
             (tab
              (pycalc-render (ast-child 'body n)))))]
 [IfElse (λ (n)
           (v-append
            (h-append
             (text "if ")
             (pycalc-render (ast-child 'test n))
             (text ":"))
            (tab
             (pycalc-render (ast-child 'then n)))
            (text "else:")
            (tab
             (pycalc-render (ast-child 'else n)))))]
 [OpExpr (λ (n)
           (hs-append
            (pycalc-render (ast-child 'lhs n))
            (pycalc-render (ast-child 'op n))
            (pycalc-render (ast-child 'rhs n))))]
 [Add (λ (n) (text "+"))]
 [Sub (λ (n) (text "-"))]
 [Mul (λ (n) (text "*"))]
 [Div (λ (n) (text "//"))]
 [BoolVal (λ (n)
            (if (random 2)
                (text "True")
                (text "False")))]
 [IntVal (λ (n)
           (number->string (random 1000)))]
 [Return (λ (n)
           (hs-append
            (text "return")
            (pycalc-render (ast-child 'Expr n))))]
 )

;; Assemble.

(assemble-spec-components pycalc pycalc-grammar)

(define concretized-types
  ...)

(define (pycalc-generate-and-render)
  (parameterize ([current-xsmith-type-constructor-thunks concretized-types])
    (let ([ast (pycalc-generate-ast 'Program)])
      (pretty-print (pycalc-render ast)
                    (current-output-port)
                    120))))

(xsmith-command-line
 pycalc-generate-and-render
 #:fuzzer-name "pycalc-pretty-printing")