#lang clotho/racket/base
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

(provide
 (struct-out scope)
 (struct-out reference)
 (struct-out binding)
 (struct-out module)
 resolve-reference
 visible-bindings
 current-well-formedness-regexp
 current-path-greater-than
 )

(require
 racket/list
 racket/match
 )

(struct scope (parent bindings imports)
  ;; parent is a scope, bindings are binding structs,
  ;; imports are just names (that should resolve to module structs)
  #:transparent)

(struct reference (name parent-scope)
  #:transparent)

;; Bindings may be to a module
(struct binding (name ast-node type def-or-param)
  #:transparent)

(struct module (scope)
  #:transparent)



;; This one is just for convenience in implementing the functions in this module.
(struct resolution (binding path)
  #:transparent)

;; reference -> binding
(define (resolve-reference reference)
  (define (resolve* name scope path-so-far)
    (if (not scope)
        '()
        (let* ([decl (findf (λ (b) (equal? (binding-name b) name))
                            (scope-bindings scope))]
               [parent-decls (resolve* name
                                       (scope-parent scope)
                                       (cons 'parent path-so-far))]
               [import-decls (flatten
                              (for/list ([import (scope-imports scope)])
                                (resolve* name
                                          (module-scope (resolve-reference import))
                                          (cons 'import path-so-far))))]
               [bindings (append parent-decls import-decls)])
          (if decl
              (cons (resolution decl (reverse (cons 'declaration path-so-far)))
                    bindings)
              bindings))))

  (let* ([resolutions (resolve* (reference-name reference)
                                (reference-parent-scope reference)
                                (list 'reference))]
         [well-formed-rs (filter well-formed?
                                 resolutions)]
         [err (when (null? well-formed-rs)
                (error '_xsmith_resolve-reference
                       (format "Unbound reference: ~a\n(With visible bindings: ~a)\n"
                               (reference-name reference)
                               (visible-bindings
                                (reference-parent-scope reference)))))]
         [best-r (apply generic-max #:gt-comparator greater-visibility
                        well-formed-rs)])
    (resolution-binding best-r)))

;; scope -> (listof binding)
(define (visible-bindings scope)
  ;; I wrote this originally with hashes, but hash ordering is not deterministic.
  ;; So I replaced every instance of "hash" with "alist" and nudged it into working.
  (define (alist-values x) (map cdr x))
  (define (alist-ref alist key fallback)
    (define found (assq key alist))
    (if found
        (cdr found)
        fallback))
  (define (alist-set alist key newval)
    (cons (cons key newval) alist))
  (define alist list)
  ;; Now that alist functions are out of the way, the actual implementation.
  ;; Do I immediately remember what I was doing here?  Not really, that's why
  ;; I just wanted to drop in "alist" instead of "hash".
  (define (resolution-alist-fold lalist ralist)
    (for/fold ([falist lalist])
              ([r (alist-values ralist)])
              (let* ([name (binding-name (resolution-binding r))]
                     [old-r (alist-ref falist name #f)])
                (if (and old-r
                         (greater-visibility old-r r))
                    falist
                    (alist-set falist name r)))))

  (define (visible-names-alist scope path-so-far)
    (if (not scope)
        (alist)
        (let* ([parent-rs (visible-names-alist
                           (scope-parent scope) (cons 'parent path-so-far))]
               [import-rss (for/list ([import (scope-imports scope)])
                             (visible-names-alist (module-scope
                                                   (resolve-reference import))
                                                  (cons 'import path-so-far)))]
               [resolutions-here
                (map (λ (b) (resolution b (reverse
                                           (cons 'declaration path-so-far))))
                     (scope-bindings scope))]
               [valid-resolutions-here (filter well-formed? resolutions-here)]
               [valid-here-alist (for/list ([r valid-resolutions-here])
                                   (cons (binding-name (resolution-binding r))
                                         r))])
          (for/fold ([falist (alist)])
                    ([ralist (list* valid-here-alist parent-rs import-rss)])
            (resolution-alist-fold falist ralist)))))

  (map resolution-binding
       (alist-values (visible-names-alist scope (list 'reference)))))


(define current-well-formedness-regexp (make-parameter #px"rp*i?d"))

(define (well-formed? resolution)
  (define (step->char step)
    (match step
      ['reference #\r]
      ['declaration #\d]
      ['parent #\p]
      ['import #\i]
      [_ (error 'scope-graph "Internal error - bad step, this shouldn't happen.")]))
  (regexp-match (current-well-formedness-regexp)
                (apply string (map step->char (resolution-path resolution)))))


(define (default-path-greater-than l r)
  ;; This assumes paths are well formed (IE they start with reference
  ;; and end in declaration), and that they are not the same.
  (define (path-part->n pp)
    (match pp
      ['reference 0]
      ['parent 1]
      ['import 2]
      ['declaration 3]
      [_ (error 'scope-graph "Internal error - bad step, this shouldn't happen.")]))
  (cond
    [(equal? (car l) (car r)) (default-path-greater-than (cdr l) (cdr r))]
    [else (> (path-part->n (car l)) (path-part->n (car r)))]))

(define current-path-greater-than (make-parameter default-path-greater-than))

(define (greater-visibility l r)
  ({current-path-greater-than} (resolution-path l)
                               (resolution-path r)))

(define (generic-max #:gt-comparator gt arg1 . args)
  (define (gmax arg1 . args)
    (cond [(empty? args) arg1]
          [(gt arg1 (car args))
           (apply gmax arg1 (cdr args))]
          [else (apply gmax (car args) (cdr args))]))
  (apply gmax arg1 args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
