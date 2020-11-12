#lang clotho
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2017-2020 The University of Utah
;; All rights reserved.
;;
;; This file is part of Xsmith, a generator of highly effective fuzz testers.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;   * Redistributions of source code must retain the above copyright notice,
;;     this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require
 racket/contract
 racket/contract/base
 (only-in racr ast-node? att-value)
 (only-in racket/base [random racket:random])
 racket/string
 racket/pretty
 (submod "types.rkt" for-private)
 "core-macros-and-properties.rkt"
 "xsmith-utils.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax
  ))

(define (dash-dash-string? s)
  (and (string? s)
       (string-prefix? s "--")))

(provide
 (contract-out
  #;[xsmith-command-line
   (->* ((-> ast-node?))
        (#:comment-wrap (-> (listof string?) string?)
         #:fuzzer-name (or/c #f string?)
         #:fuzzer-version (or/c #f string?)
         #:features (listof
                     (or/c (list/c symbol? boolean?)
                           (list/c symbol? boolean? (listof string?))))
         #:extra-parameters (listof (list/c dash-dash-string?
                                            string?
                                            parameter?
                                            (or/c procedure? #f)))
         #:default-max-depth number?
         #:default-type-max-depth number?
         #:format-render (-> any/c string?))
        void?)]
  )
 xsmith-feature-enabled?
 define-xsmith-interface-functions
 )


(require
 racket/dict
 racket/match
 racket/cmdline
 racket/string
 racket/exn
 racket/port
 racket/pretty
 racket/list
 raco/command-name
 "xsmith-parameters.rkt"
 "xsmith-utils.rkt"
 (submod "xsmith-utils.rkt" for-private)
 "xsmith-version.rkt"
 "core-properties.rkt"
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define true-strings  '("true"  "t" "#t" "yes" "y"))
(define false-strings '("false" "f" "#f" "no"  "n"))
(define (string->bool bool-string var-name [default #f])
  (define s (string-downcase bool-string))
  (cond [(member s true-strings) #t]
        [(member s false-strings) #f]
        [default default]
        [else (error
               'string->bool
               (string-append
                "While parsing argument for ~a,\n"
                "expected “true”, “t”, “#t”, “false”, “f”, or “#f”.\n"
                "Got ~a.\n")
               var-name
               bool-string)]))
(define (bool->string b)
  (if b "true" "false"))

(define (generate-random-seed) (racket:random (sub1 (expt 2 31))))


(begin-for-syntax
  (define-syntax-class feature-class
    (pattern [name:id default:boolean
                      (~optional [docstring ...]
                                 #:defaults ([(docstring 1) '()]))]
             #:attr keyword (datum->syntax #'name
                                           (string->keyword
                                            (format "with-~a"
                                                    (syntax->datum #'name))))
             #:attr argname (car (generate-temporaries #'(name))))))


(define server-port-default 8080)
(define server-path-default "/")
(define server-ip-default "127.0.0.1")
(define given-seed-default #f)
(define server-default #f)
(define netstring-server-default #f)
(define netstring-ignore-input-default #f)
(define render-on-error-default #f)
(define s-exp-on-error-default #f)
(define s-exp-print-override-default #f)
(define s-exp-show-base-fields-default #f)
(define seq-to-file-default #f)
(define seq-from-file-default #f)
(define generation-timeout-default #f)
(define print-debug-default #f)
(define verbose-default #f)
(define output-file-default #f)



(define-syntax (define-xsmith-interface-functions stx)
  (syntax-parse stx
    [(_ [component1 component ...]
        (~or
         (~optional (~seq #:fuzzer-name fuzzer-name-given:id))
         (~optional (~seq #:program-node program-node:id)
                    #:defaults ([program-node #'Program]))
         (~optional (~seq #:properties [prop-name:id ...])
                    #:defaults ([(prop-name 1) '()]))
         (~optional (~seq #:command-line-name command-line-name-given:id))
         (~optional (~seq #:function-name function-name-given:id))
         (~optional (~seq #:fuzzer-version fuzzer-version-given))
         (~optional (~seq #:comment-wrap comment-wrap))
         (~optional (~seq #:features [feature:feature-class ...]))
         (~optional (~seq #:extra-parameters ([extra-param-name:id
                                               extra-param-docstring
                                               extra-param-param-expression
                                               extra-param-converter-expression]
                                              ...))
                    #:defaults ([(extra-param-name 1) '()]
                                [(extra-param-docstring 1) '()]
                                [(extra-param-param-expression 1) '()]
                                [(extra-param-converter-expression 1) '()]))
         (~optional (~seq #:default-max-depth default-max-depth)
                    #:defaults ([default-max-depth #'5]))
         (~optional (~seq #:default-type-max-depth default-type-max-depth)
                    #:defaults ([default-type-max-depth #'4]))
         (~optional (~seq #:format-render format-render-func))
         (~optional (~seq #:type-thunks type-constructor-thunks))
         )
        ...)


     (define/syntax-parse fuzzer-name
       (or (attribute fuzzer-name-given)
           #'component1))
     (define/syntax-parse command-line-function-name
       (or (attribute command-line-name-given)
           (format-id stx "~a-command-line" #'fuzzer-name)))
     (define/syntax-parse procedural-generation-function-name
       (or (attribute function-name-given)
           (format-id stx "~a-generate" #'fuzzer-name)))

     ;; I am having issues with optional lists, so I'm just going to do this
     ;; verbose method of defining things.
     (define/syntax-parse (feature-name ...)
       (if (attribute feature) #'(feature.name ...) #'()))
     (define/syntax-parse (feature-default ...)
       (if (attribute feature) #'(feature.default ...) #'()))
     (define/syntax-parse ((feature-docstring ...) ...)
       (if (attribute feature) #'((feature.docstring ...) ...) #'()))
     (define/syntax-parse (feature-keyword ...)
       (if (attribute feature) #'(feature.keyword ...) #'()))
     (define/syntax-parse (feature-argname ...)
       (if (attribute feature) #'(feature.argname ...) #'()))

     (define/syntax-parse (extra-param-defname ...)
       (generate-temporaries #'(extra-param-name ...)))
     (define/syntax-parse (extra-param-argname ...)
       (generate-temporaries #'(extra-param-name ...)))
     (define/syntax-parse (extra-param-converter ...)
       (generate-temporaries #'(extra-param-name ...)))
     (define/syntax-parse (extra-param-keyword ...)
       (map (λ (x) (datum->syntax #'stx (string->keyword
                                         (symbol->string
                                          (syntax->datum x)))))
            (syntax->list #'(extra-param-name ...))))

     #'(begin
         (assemble-spec-components fuzzer component1 component ...)
         (define fuzzer-version (~? fuzzer-version-given #f))
         (define extra-param-defname extra-param-param-expression) ...
         (define extra-param-converter extra-param-converter-expression) ...

         ;; TODO - I don't know about this default.
         (define comment-func (~? comment-wrap (λ (lines)
                                                 (string-join
                                                  (map (λ (l) (format ";; ~a" l))
                                                       lines)
                                                  "\n"))))

         (define type-constructor-thunks-func/list
           (~? type-constructor-thunks (current-xsmith-type-constructor-thunks)))

         (define (generation-thunk)
           (parameterize ([current-xsmith-type-constructor-thunks
                           (if (procedure? type-constructor-thunks-func/list)
                               (type-constructor-thunks-func/list)
                               type-constructor-thunks-func/list)])
             (fuzzer-generate-ast 'program-node)))

         (define extra-param-box-default (gensym))


         (define version-info
           (format "~a~a, in Racket ~a (vm-type ~a)"
                   (cond [(and 'fuzzer-name fuzzer-version)
                          (format "~a ~a, " 'fuzzer-name fuzzer-version)]
                         ['fuzzer-name (format "~a, " 'fuzzer-name)]
                         [else ""])
                   xsmith-version-string
                   (version)
                   (system-type 'vm)))


         ;; This is the old format the command-line function took it in.
         (define extra-parameters-old-format
           (list (list 'extra-param-name
                       extra-param-docstring
                       extra-param-defname
                       extra-param-converter)
                 ...))

         ;; Check uniqueness of extra-param details.
         ;; This would be good as a static check, but a check at module
         ;; load time is good too.
         (let ([extra-param-list (map third extra-parameters-old-format)]
               [extra-param-names (map first extra-parameters-old-format)])
           (when (not (equal? extra-param-list
                              (remove-duplicates extra-param-list)))
             (error 'xsmith-command-line
                    "extra parameters must be unique"))
           (when (not (equal? extra-param-names
                              (remove-duplicates extra-param-names)))
             (error 'xsmith-command-line
                    "extra-parameter names must be unique")))



         (define not-given (gensym))
         (define (arg given default)
           (if (eq? given not-given)
               default
               given))


         ;; Function that reads the current-command-line
         (define (command-line-function-name)

           (define features (hash (~@ 'feature-name feature-default) ...))
           (define (set-feature! k v)
             (set! features (dict-set features k v)))
           (define server-port server-port-default)
           (define server-path server-path-default)
           (define server-ip server-ip-default)
           (define given-seed given-seed-default)
           (define server? server-default)
           (define netstring-server-path netstring-server-default)
           (define netstring-ignore-input? netstring-ignore-input-default)
           (define render-on-error? render-on-error-default)
           (define s-exp-on-error? s-exp-on-error-default)
           (define s-exp-print-override s-exp-print-override-default)
           (define s-exp-show-base-fields s-exp-show-base-fields-default)
           (define seq-to-file seq-to-file-default)
           (define seq-from-file seq-from-file-default)
           (define max-depth default-max-depth)
           (define type-max-depth default-type-max-depth)
           (define generation-timeout generation-timeout-default)
           (define print-debug? print-debug-default)
           (define verbose? verbose-default)
           (define output-file-name output-file-default)
           (define inspection-serials '())
           (define options (xsmith-options-defaults))

           (define command-line-to-print (current-command-line-arguments))


           (define features-list (list (~@ (list 'feature-name feature-default
                                                 (list feature-docstring ...)))
                                       ...))
           (define features-command-line-segment
             (if (null? features-list)
                 '()
                 `((help-labels "")
                   (help-labels "[[LANGUAGE-SPECIFIC FEATURES]]")
                   (help-labels "Default to true unless otherwise specified.")
                   (once-each
                    ,@(map
                       (λ (spec)
                         (define-values (name default docstrings)
                           (match spec
                             [(list n d (list)) (values n d '(""))]
                             [(list n d (list doc (... ...))) (values n d doc)]))
                         `[(,(string-append "--with-" (symbol->string name)))
                           ,(λ (flag v) (set-feature! name (string->bool v name)))
                           ([,@(append docstrings
                                       (filter
                                        (λ(x)x)
                                        (list
                                         (and (not default)
                                              (format "Defaults to ~a"
                                                      (bool->string default))))))]
                            "bool")])
                       features-list)))))

           (define extra-param-box-default (gensym))
           (match-define (list (list extra-param-boxes
                                     extra-param-handlers
                                     extra-param-params)
                               (... ...))
             (map (λ (p)
                    (match p
                      [(list name docstring param normalizer)
                       (let* ([b (box extra-param-box-default)]
                              [handler (λ (v) (set-box! b
                                                        (if normalizer
                                                            (normalizer v)
                                                            v)))])
                         (list b handler param))]))
                  extra-parameters-old-format))

           (define extra-parameters-command-line-segment
             (if (null? extra-parameters-old-format)
                 '()
                 `((help-labels "")
                   (help-labels "[[LANGUAGE-SPECIFIC EXTRA PARAMETERS]]")
                   (once-each
                    ,@(for/list ([param-spec extra-parameters-old-format]
                                 [handler extra-param-handlers])
                        (match param-spec
                          [(list name docstring param normalizer)
                           `[(,(format "--~a" name))
                             ,(λ (flag v) (handler v))
                             [,docstring "arg"]]]))))))


           (parse-command-line
            (short-program+command-name)
            (current-command-line-arguments)
            `((help-labels "[[GENERAL OPTIONS]]")
              (once-each
               [("--seed" "-s")
                ,(λ (flag seed)
                   (set! given-seed (string->number seed)))
                ("Set the random seed" "seed")]
               [("--output-file" "-o")
                ,(λ (flag filename) (set! output-file-name filename))
                ("Output generated program to <filename>" "filename")]
               [("--server")
                ,(λ (flag run-as-server?)
                   (set! server? (string->bool run-as-server? 'run-as-server?)))
                ("Run as a web server instead of generating a single program."
                 "run-as-server?")]
               [("--netstring-server")
                ,(λ (flag socket-path) (set! netstring-server-path socket-path))
                ("Run as a netstring server on a Unix domain socket at given path."
                 "socket-path")]
               [("--netstring-ignore-input")
                ,(λ (flag bool) (set! netstring-ignore-input?
                                      (string->bool bool 'netstring-ignore-input)))
                (["Whether to ignore netstring input and just use random seeds"
                  "(For netstring server mode)"]
                 "netstring-ignore-input")]
               [("--server-port")
                ,(λ (flag n) (set! server-port (string->number n)))
                ("Use port n instead of 8080 (when running as server)." "n")]

               [("--server-ip")
                ,(λ (flag ip) (set! server-ip (if (member (string-downcase ip)
                                                          false-strings)
                                                  #f ip)))
                (["Make the server listen on given IP address."
                  "Use `false` to listen on all IP addresses."
                  "Defaults to 127.0.0.1"]
                 "ip")]
               [("--server-path")
                ,(λ (flag path) (set! server-path (if (not (string-prefix? path "/"))
                                                      (string-append "/" path)
                                                      path)))
                (["Run the fuzzer with the given path."
                  "Defaults to /"]
                 "path")]
               [("--render-on-error")
                ,(λ (flag show-render-on-error?)
                   (set! render-on-error? (string->bool show-render-on-error? 'show-render-on-error?)))
                (["Print the partial tree (using the render-node-info property) if an"
                  "error is encountered."
                  "Defaults to false."]
                 "show-render-on-error?")]
               [("--s-exp-on-error")
                ,(λ (flag show-s-exp-on-error)
                   (set! s-exp-on-error?
                         (string->bool show-s-exp-on-error 'show-s-exp-on-error?)))
                (["Print an s-expression representation of the tree if an error is encountered"
                  "Defaults to false."]
                 "show-s-exp-on-error?")]
               [("--s-exp-print-override")
                ,(λ (flag s-exp-override)
                   (set! s-exp-print-override
                         (string->bool s-exp-override 's-exp-override)))
                ["Print an s-expression representation of the tree instead of using the normal renderer."
                 "Defaults to false."]]
               [("--s-exp-show-base-fields")
                ,(λ (flag s-exp-show-base-fields-arg)
                   (set! s-exp-show-base-fields
                         (let* ([v-bool (string->bool s-exp-show-base-fields-arg
                                                      's-exp-show-base-fields
                                                      'no)]
                                [v-sym (and (eq? 'no v-bool)
                                            (string->symbol
                                             s-exp-show-base-fields-arg))])
                           (or v-sym v-bool))))
                [["Show (hidden) base fields when printing s-exp representation (for debugging)"
                  "t to show all fields, f for none, xsmithserialnumber for serial numbers"]
                 "Defaults to false."]]
               [("--inspection-serials")
                ,(λ (flag serials)
                   (set! inspection-serials
                         (map string->number
                              (string-split serials ","))))
                (["Comma separated serial numbers for extra debug printing."]
                 "Defaults to none.")]
               [("--print-debug")
                ,(λ (flag print-debug)
                   (set! print-debug? (string->bool print-debug 'print-debug?)))
                (["Print debug info even when generation is successful."
                  "Defaults to false."]
                 "print-debug?")]
               [("--seq-to-file")
                ,(λ (flag filename) (set! seq-to-file filename))
                (["Output the generated randomness sequence to a file at the given path."
                  "If not given, no file will be saved."]
                 "seq-to-file")]
               [("--seq-from-file")
                ,(λ (flag filename) (set! seq-from-file filename))
                (["Use the bytes in the given file as the source of randomness."
                  "Supersedes any other options that define the random source."]
                 "seq-from-file")]
               [("--timeout")
                ,(λ (flag timeout-seconds) (set! generation-timeout
                                                 (string->number timeout-seconds)))
                (["Timeout for generation, in seconds."]
                 "timout")]
               )
              (help-labels "")
              (help-labels "[[LANGUAGE-GENERATION OPTIONS]]")
              (once-each
               [("--max-depth")
                ,(λ (flag n) (set! max-depth (string->number n)))
                (,(format "Set maximum tree depth (default ~a)" default-max-depth)
                 "number")]
               [("--type-max-depth")
                ,(λ (flag n) (set! max-depth (string->number n)))
                (,(format "Set maximum depth for type concretization (default ~a)"
                          default-type-max-depth)
                 "number")]
               )
              ,@features-command-line-segment
              ,@extra-parameters-command-line-segment

              (help-labels "")
              (help-labels "[[INFORMATION OPTIONS]]")
              (once-each
               [("--version" "-v")
                ,(λ (flag)
                   (displayln version-info)
                   (exit 0))
                ("Show program version information and exit")]
               [("--verbose", "-V")
                ,(λ (flag verbose-mode?)
                   (set! verbose? (string->bool verbose-mode? 'verbose-mode?)))
                (["Whether to activate verbose mode, which prints extra information during generation."
                  "Defaults to false."]
                 "verbose-mode?")])
              )
            ;; Finish-proc
            (λ (flag-accum)
              (void))
            ;; arg-help-strs
            '()
            ;; help-proc
            ;; unknown-proc
            )


           (define param-pairs
             (filter
              (λ(x)x)
              (map (λ (p b) (and (not (eq? (unbox b) extra-param-box-default))
                                 (cons p (unbox b))))
                   extra-param-params
                   extra-param-boxes)))

           (core-generate-function
            #:param-pairs param-pairs
            #:features features
            #:server-port server-port
            #:server-path server-path
            #:server-ip server-ip
            #:given-seed given-seed
            #:server? server?
            #:netstring-server-path netstring-server-path
            #:netstring-ignore-input? netstring-ignore-input?
            #:render-on-error? render-on-error?
            #:s-exp-on-error? s-exp-on-error?
            #:s-exp-print-override s-exp-print-override
            #:s-exp-show-base-fields s-exp-show-base-fields
            #:seq-to-file seq-to-file
            #:seq-from-file seq-from-file
            #:max-depth max-depth
            #:type-max-depth type-max-depth
            #:generation-timeout generation-timeout
            #:print-debug print-debug?
            #:verbose verbose?
            #:output-file output-file-name
            #:inspection-serials inspection-serials

            #:command-line-to-print command-line-to-print))

         ;; Programatic interface function
         (define (procedural-generation-function-name
                  ;; Keyword arguments witt all default to `not-given`.
                  ;; We will use a default instead for each of those, but
                  ;; we can track not-given to know which ones were given
                  ;; and which are just a default value.
                  (~@ feature-keyword [feature-argname not-given])
                  ...
                  (~@ extra-param-keyword [extra-param-argname not-given])
                  ...
                  ;; Normal arguments
                  #:seed [seed-arg not-given]
                  #:output-file [output-file-arg not-given]
                  #:server [server-arg not-given]
                  #:netstring-server [netstring-server-arg not-given]
                  #:netstring-ignore-input [netstring-ignore-input-arg not-given]
                  #:server-port [server-port-arg not-given]
                  #:server-ip [server-ip-arg not-given]
                  #:server-path [server-path-arg not-given]
                  #:render-on-error [render-on-error-arg not-given]
                  #:s-exp-on-error [s-exp-on-error-arg not-given]
                  #:s-exp-print-override [s-exp-print-override-arg not-given]
                  #:s-exp-show-base-fields [s-exp-show-base-fields-arg not-given]
                  #:seq-to-file [seq-to-file-arg not-given]
                  #:seq-from-file [seq-from-file-arg not-given]
                  #:max-depth [max-depth-arg not-given]
                  #:type-max-depth [type-max-depth-arg not-given]
                  #:timeout [timeout-arg not-given]
                  #:print-debug [print-debug not-given]
                  #:verbose [verbose? not-given]
                  #:inspection-serials [inspection-serials not-given]
                  )



           (define features
             (hash (~@ 'feature-name (arg feature-argname feature-default)) ...))

           (define param-pairs
             (filter-map
              (λ (l) (and (not (eq? (cadr l)
                                    not-given))
                          (cons (car l)
                                ;; apply the converter to the value if the value
                                ;; is a string.
                                (if (string? (cadr l))
                                    ({caddr l} (cadr l))
                                    (cadr l)))))
              (list (~@ (list extra-param-defname
                              extra-param-argname
                              extra-param-converter))
                    ...)))

           (define command-line-to-print
             (apply
              append
              (filter (λ (l) (not (eq? (cadr l)
                                       not-given)))
                      (list
                       (~@ (list 'feature-keyword feature-argname)) ...
                       (~@ (list 'extra-param-keyword extra-param-argname)) ...
                       (list '#:server-port server-port-arg)
                       (list '#:server-path server-path-arg)
                       (list '#:server-ip server-ip-arg)
                       (list '#:seed seed-arg)
                       (list '#:server server-arg)
                       (list '#:netstring-server netstring-server-arg)
                       (list '#:netstring-ignore-input netstring-ignore-input-arg)
                       (list '#:render-on-error render-on-error-arg)
                       (list '#:s-exp-on-error s-exp-on-error-arg)
                       (list '#:s-exp-print-override s-exp-print-override-arg)
                       (list '#:s-exp-show-base-fields s-exp-show-base-fields-arg)
                       (list '#:seq-to-file seq-to-file-arg)
                       (list '#:seq-from-file seq-from-file-arg)
                       (list '#:max-depth max-depth-arg)
                       (list '#:type-max-depth type-max-depth-arg)
                       (list '#:timeout timeout-arg)
                       (list '#:output-file output-file-arg)
                       ))))

           (core-generate-function
            #:param-pairs param-pairs
            #:features features
            #:server-port (arg server-port-arg server-port-default)
            #:server-path (arg server-path-arg server-path-default)
            #:server-ip (arg server-ip-arg server-ip-default)
            #:given-seed (arg seed-arg given-seed-default)
            #:server? (arg server-arg server-default)
            #:netstring-server-path (arg netstring-server-arg
                                         netstring-server-default)
            #:netstring-ignore-input? (arg netstring-ignore-input-arg
                                           netstring-ignore-input-default)
            #:render-on-error? (arg render-on-error-arg render-on-error-default)
            #:s-exp-on-error? (arg s-exp-on-error-arg s-exp-on-error-default)
            #:s-exp-print-override (arg s-exp-print-override-arg
                                        s-exp-print-override-default)
            #:s-exp-show-base-fields (arg s-exp-show-base-fields-arg
                                          s-exp-show-base-fields-default)
            #:seq-to-file (arg seq-to-file-arg seq-to-file-default)
            #:seq-from-file (arg seq-from-file-arg seq-from-file-default)
            #:max-depth (arg max-depth-arg default-max-depth)
            #:type-max-depth (arg type-max-depth-arg default-type-max-depth)
            #:generation-timeout (arg timeout-arg
                                      generation-timeout-default)
            #:output-file (arg output-file-arg output-file-default)

            #:command-line-to-print command-line-to-print
            #:print-debug (arg print-debug print-debug-default)
            #:verbose (arg verbose? verbose-default)
            #:inspection-serials (arg inspection-serials '())
            )
           )



         (define (core-generate-function
                  ;; param-pairs is a list of pairs where the car is a
                  ;; racket parameter and the cdr is the value to parameterize it to.
                  #:param-pairs param-pairs
                  ;; features is a hash table of feature name to value
                  #:features features
                  #:server-port server-port
                  #:server-path server-path
                  #:server-ip server-ip
                  #:given-seed given-seed
                  #:server? server?
                  #:netstring-server-path netstring-server-path
                  #:netstring-ignore-input? netstring-ignore-input?
                  #:render-on-error? render-on-error?
                  #:s-exp-on-error? s-exp-on-error?
                  #:s-exp-print-override s-exp-print-override
                  #:s-exp-show-base-fields s-exp-show-base-fields
                  #:seq-to-file seq-to-file
                  #:seq-from-file seq-from-file
                  #:max-depth max-depth
                  #:type-max-depth type-max-depth
                  #:generation-timeout generation-timeout
                  #:output-file output-file
                  #:command-line-to-print command-line-to-print
                  #:print-debug print-debug-with-no-error?
                  #:verbose verbose?
                  #:inspection-serials inspection-serials
                  )


           #;(match-define (list (list extra-param-boxes
                                       extra-param-handlers
                                       extra-param-params)
                                 ...)
               (map (λ (p)
                      (match p
                        [(list name docstring param normalizer)
                         (let* ([b (box extra-param-box-default)]
                                [handler (λ (v) (set-box! b
                                                          (if normalizer
                                                              (normalizer v)
                                                              v)))])
                           (list b handler param))]))
                    extra-parameters))

           (define (verbose-eprintf . args)
             (if verbose?
                 (apply eprintf args)
                 #f))

           (define options (xsmith-options-defaults))

           (define initial-random-source
             (if seq-from-file
                 (port->bytes (open-input-file seq-from-file))
                 (or given-seed (generate-random-seed))))

           (define (generate-and-print!/xsmith-parameterized
                    #:random-source [random-input initial-random-source])
             (parameterize ([current-xsmith-max-depth max-depth]
                            [type-max-concretization-depth type-max-depth]
                            [current-xsmith-features features]
                            [xsmith-options options]
                            [xsmith-state (make-generator-state)]
                            [current-inspection-serials inspection-serials]
                            [current-random-source (make-random-source random-input)])
               (let/ec abort
                 (define option-lines
                   (append
                    (if 'fuzzer-name
                        (list (format "Fuzzer: ~a" 'fuzzer-name))
                        (list))
                    (list (format "Version: ~a" version-info)
                          (format "Options: ~a"
                                  (if (vector? command-line-to-print)
                                      (string-join
                                       (map (λ (x) (format "~a" x))
                                            (vector->list
                                             command-line-to-print)))
                                      command-line-to-print))
                          (if (number? random-input)
                              (format "Seed: ~a" random-input)
                              (format "Random-input: ~v" random-input)))))
                 (define captured-output "")
                 (define (output-error err msg [partial-prog #f])
                   (let* ([output
                           (format
                            (string-join
                             '("!!! Xsmith Error !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                               "~a"  ;; Error message.
                               ""
                               "Options:"
                               "~a"  ;; Option lines.
                               "Debug Log:"
                               "~a"  ;; Debug log.
                               ""
                               "Exception:"
                               "~a"  ;; Exception.
                               )
                             "\n")
                            msg
                            (string-join
                             option-lines
                             "\n")
                            (get-xsmith-debug-log!)
                            (exn->string err))]
                          [output (if (not (eq? captured-output ""))
                                      (string-append
                                       output
                                       (format "\nProgram output captured:\n~a\n"
                                               captured-output))
                                      output)]
                          [output (if partial-prog
                                      (string-append
                                       output
                                       (format "\nPartially generated program:\n~a\n"
                                               partial-prog))
                                      output)])
                     (set! captured-output "")
                     (display output)))
                 ;; Compute the result of a procedure, capturing all output to
                 ;; `captured-output` and returning the procedure's result.
                 (define (capture-output! proc)
                   (let* ([result #f]
                          [out (with-output-to-string
                                 (λ () (set! result (proc))))])
                     #;(set! captured-output out)
                     (when (not (eq? out ""))
                       (set! captured-output (string-append captured-output out)))
                     result))
                 ;;;;;;;;;;;;;;;;
                 ;; Actual generation and printing starts here.
                 ;;;;
                 ;; Convert an AST to a string.
                 (define (ast->string root)
                   (verbose-eprintf "Converting AST to string...\n")
                   (let ([ppr (att-value 'xsmith_render-node root)]
                         [fmt (~? format-render-func #f)])
                     (if fmt
                         (fmt ppr)
                         (format "~a\n" ppr))))
                 ;; Notify beginning of generation with seed.
                 (verbose-eprintf "Generating from seed: ~a.\n" random-input)
                 ;; Attempt to generate the AST.
                 (define error? #f)
                 (define error-root #f)
                 (verbose-eprintf "Generating the AST...\n")
                 (define ast
                   (capture-output!
                    (λ () (with-handlers
                            ([(match-lambda [(list 'ast-gen-error e root) #t]
                                            [_ #f])
                              (λ (l)
                                (set! error? (second l))
                                (set! error-root (third l)))]
                             [(λ (e) #t)
                              (λ (e) (set! error? e))])
                            (generation-thunk)))))
                 (define (do-error-printing)
                   (when error?
                     (verbose-eprintf "Encountered an error during program generation.\n")
                     (output-error
                      error?
                      "Error encountered while generating program!")
                     ;; If the user asked for it (and if any AST was salvaged from the
                     ;; generation stage), attempt to convert the partially completed AST
                     ;; to pre-print representation (PPR).
                     (when (and s-exp-on-error?
                                (or error-root ast))
                       (printf "S-expression representation of program:\n")
                       (parameterize ([current-s-exp-show-base-fields
                                       s-exp-show-base-fields])
                         (pretty-print
                          (att-value '_xsmith_to-s-expression (or error-root ast))
                          (current-output-port)
                          1))
                       (printf "\n\n"))
                     (when (and render-on-error?
                                error-root)
                       (let* ([ppr-error? #f]
                              [partial-prog (capture-output!
                                             (λ () (with-handlers ([(λ (e) #t)
                                                                    (λ (e) (set! ppr-error? e))])
                                                     (ast->string error-root))))])
                         (if ppr-error?
                             (begin
                               (verbose-eprintf "Encountered error while intercepting another error.\n")
                               (output-error
                                ppr-error?
                                "Error encountered in printing while intercepting another error in AST generation."))
                             (printf "Partially generated program render:\n~a\n\n"
                                     partial-prog))))
                     ;; Quit further execution.
                     (abort)))
                 (do-error-printing)
                 ;; Convert the AST to PPR.
                 (define program
                   (capture-output!
                    (λ () (with-handlers ([(λ (e) #t)
                                           (λ (e) (set! error? e))])
                            (if s-exp-print-override
                                (with-output-to-string
                                  (λ ()
                                    (parameterize
                                        ([current-s-exp-show-base-fields
                                          s-exp-show-base-fields])
                                      (pretty-print
                                       (att-value '_xsmith_to-s-expression ast)
                                       (current-output-port)
                                       1))))
                                (ast->string ast))))))
                 (if error?
                     (begin
                       ;; Something went wrong during printing.
                       (verbose-eprintf "Encountered error while printing program.\n")
                       (printf "Error encountered while printing program.\n")
                       (do-error-printing)
                       (abort))
                     (when print-debug-with-no-error?
                       (eprintf "~a\n\n" (get-xsmith-debug-log!))))
                 ;; Everything was successful!
                 (verbose-eprintf "Program successfully generated.\n")
                 (display (comment-func (cons "This is a RANDOMLY GENERATED PROGRAM."
                                              option-lines)))
                 (display (format "\n\n~a\n" program))
                 (when (non-empty-string? captured-output)
                   (display "\n")
                   (display (comment-func (flatten
                                           (list "!!! The following output was captured during execution:"
                                                 ""
                                                 (string-split captured-output "\n")))))
                   (display "\n"))

                 (display "\n"))
               ;; If the flag was set, output the random-source's byte sequence to file.
               (when seq-to-file
                 (verbose-eprintf "Writing random-source bytes to sequence file.\n")
                 (write-bytes (get-current-random-source-byte-string)
                              (open-output-file seq-to-file #:exists 'replace)))
               ;; Update the seed. (This is used in server mode.)
               (verbose-eprintf "Updating seed.\n")
               (dict-set! (xsmith-options)
                          'random-seed
                          (modulo (add1 (xsmith-option 'random-seed))
                                  random-seed-max))))

           (define (generate-and-print! #:random-source [random-source initial-random-source]
                                        #:timeout [timeout generation-timeout])
             (define (do-parameterized param-pairs thunk)
               (match param-pairs
                 ['() (thunk)]
                 [(list (cons param param-val) rest (... ...))
                  (parameterize ([param param-val])
                    (do-parameterized rest thunk))]))
             (define out (open-output-string))
             (define work-thread
               (thread (λ ()
                         (parameterize ([current-output-port out])
                           (do-parameterized param-pairs
                                             (λ () (generate-and-print!/xsmith-parameterized
                                                    #:random-source random-source)))))))
             (define on-time?
               (sync/timeout timeout work-thread))
             (if on-time?
                 (display (get-output-string out))
                 (begin
                   ;; Instead of killing the work thread, we send a break, which will
                   ;; cause an exception that will then give us some debugging info.
                   ;; This is important if we think our fuzzer is in an infinite loop somewhere.
                   (break-thread work-thread)
                   (displayln
                    (comment-func (list "Generation failed because timeout was exceeded.")))
                   (sync work-thread)
                   (displayln (comment-func (string-split (get-output-string out) "\n")))
                   (displayln (comment-func (string-split (get-xsmith-debug-log!) "\n"))))))

           (cond [(and server? netstring-server-path)
                  (error 'server? "--server and --netstring-server are mutually exclusive")]
                 [server?
                  (let ([serve/servlet (dynamic-require 'web-server/servlet-env 'serve/servlet)]
                        [response (dynamic-require 'web-server/http/response-structs 'response)]
                        [random-source (or initial-random-source
                                           (generate-random-seed))])
                    (define (servlet-start req)
                      (let ((out (open-output-string)))
                        (parameterize ((current-output-port out))
                          (generate-and-print! #:random-source random-source))
                        (set! random-source (generate-random-seed))
                        (response 200
                                  #"OK"
                                  (current-seconds)
                                  #"text/plain"
                                  '()
                                  (λ (op)
                                    (write-bytes
                                     (get-output-bytes out)
                                     op)))))
                    (eprintf "Starting server...\n")
                    (eprintf "Visit: http://localhost:~a~a\n" server-port server-path)
                    (serve/servlet servlet-start
                                   #:port server-port
                                   #:command-line? #t
                                   #:listen-ip server-ip
                                   #:servlet-path server-path))]
                 [netstring-server-path
                  (define make-parent-directory*
                    (dynamic-require 'racket/file 'make-parent-directory*))
                  (define (dr-us name)
                    (dynamic-require 'racket/unix-socket name))
                  (define unix-socket-listen (dr-us 'unix-socket-listen))
                  (define unix-socket-accept (dr-us 'unix-socket-accept))
                  (define unix-socket-close-listener (dr-us 'unix-socket-close-listener))

                  (make-parent-directory* netstring-server-path)
                  (define listener (unix-socket-listen netstring-server-path))
                  (define (close)
                    (unix-socket-close-listener listener)
                    (delete-file netstring-server-path))
                  (define (read-netstring port)
                    (define m (regexp-match (pregexp "^(\\d+):") port))
                    (when (not m)
                      (error 'read-netstring "Input port not at netstring"))
                    (define len (string->number (bytes->string/utf-8 (cadr m))))
                    (define bytes (read-bytes len port))
                    (define closing-comma (read-char port))
                    (when (not (equal? #\, closing-comma))
                      (error 'read-netstring
                             "netstring did not end in a closing comma, got :~v"
                             closing-comma))
                    (when (not (equal? (bytes-length bytes) len))
                      (error 'read-netstring
                             "Result length did not match.  Expected: ~v, Actual: ~v"
                             len (bytes-length bytes)))
                    bytes)
                  (define (write-netstring port netstring)
                    ;(eprintf "writing netstring:\n~v" netstring)
                    (display (bytes-length netstring) port)
                    (display #":" port)
                    (display netstring port)
                    (display #"," port)
                    (flush-output port))
                  (define (server-loop)
                    (let-values ([(in-port out-port) (unix-socket-accept listener)])
                      (define (connection-loop)
                        (define maybe-eof (peek-char in-port))
                        (if (eof-object? maybe-eof)
                            (close-input-port in-port)
                            (let ()
                              (define bytes-in (read-netstring in-port))
                              (define bytes-out
                                (with-output-to-bytes
                                  (λ () (generate-and-print!
                                         #:random-source (if netstring-ignore-input?
                                                             (generate-random-seed)
                                                             bytes-in)))))
                              (write-netstring out-port bytes-out)
                              (connection-loop))))
                      (connection-loop)
                      (close-output-port out-port)
                      (server-loop)))
                  (with-handlers ([(λ(x)x) (λ(e)
                                             (close)
                                             (raise e))])
                    (server-loop))]
                 [else
                  (if output-file
                      (call-with-output-file output-file
                        #:exists 'replace
                        (lambda (out)
                          (parameterize ([current-output-port out])
                            (generate-and-print!))))
                      (generate-and-print!))]))


         )]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
