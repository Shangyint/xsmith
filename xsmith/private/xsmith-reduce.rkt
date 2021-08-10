#lang clotho
;; -*- mode: Racket -*-
;;
;; Copyright (c) 2021 The University of Utah
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
 racket/system
 racket/port
 racket/file
 racket/class
 racket/match
 racr
 (submod "types.rkt" for-private)
 "core-macros-and-properties.rkt"
 "xsmith-utils.rkt"
 (submod "xsmith-utils.rkt" for-private)
 (submod "grammar-macros.rkt" for-private_reduction)
 "choice.rkt"
 "scope-graph.rkt"
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax
  ))

(provide reduce-ast)


(define (ast-size n)
  (apply + 1 (map ast-size
                  (filter ast-node?
                          (ast-children/flat n)))))

(define (distance-from-root n)
  (if (ast-has-parent? n)
      0
      (+ 1 (distance-from-root (ast-parent n)))))

(define (reduce-ast ast-original
                    ast->string
                    minimization-script
                    minimization-directory)
  (define script-full-path (path->string (path->complete-path minimization-script)))
  (make-directory* minimization-directory)
  (define min-file (build-path minimization-directory "reduction-candidate"))
  (define last-good-min-file (build-path minimization-directory "last-good-reduction-candidate"))

  ;; Minimization unfortunately works via mutation, so let's just always refer to root
  (define root ast-original)

  (define (write-file! f program-string)
    (with-output-to-file f
      #:mode 'text
      #:exists 'replace
      (λ () (display program-string))))
  (define (run-test-script!)
    (parameterize ([current-directory minimization-directory]
                   ;; It would be better to send the output somewhere useful,
                   ;; but without doing something the output gets put in
                   ;; the program source of the final output...
                   [current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      ;; Returns true if the script exits successfully, which is our test
      (system script-full-path)))
  (define (version-successful!? message)
    (define out-string (ast->string root))
    (write-file! min-file out-string)
    (define success? (run-test-script!))
    (when (and success? message)
      (write-file! last-good-min-file out-string)
      ;; TODO - it would be nice to print updates to stderr
      (xd-printf message))
    success?)


  ;; First check that the minimization script is successful on the original AST.
  (define original-success? (version-successful!? #f))
  (when (not original-success?)
    (error 'reduce-ast "Reduction script failed for original program."))
  ;; TODO - check that the interestingness test fails on an empty file as well (or some minimal always uninteresting file)


  #|
  Tactics:
  * Delete (non-binder) list node member
  * Delete unused binder (still not parameter) list node member
  * Replace node with (non-reference) atomic choice
  * Replace reference with most global, first declared definition of same type
  * Recur to children
  * TODO Climbing up after the recursion, search for descendant nodes of the same type, replace node with descendant (IE unwrap the critical section)

  Probably I want to just run each of those down the tree, then re-run the delete unused binder tactic until it fails to make a change.
  |#

  (define (reduce-node n)
    ;; Works by side effect on the AST.
    (let/ec return!


      ;; Tactic: replace (non-binder) node with atomic
      (define is-binder? (att-value '_xsmith_binder-type-field n))
      (define hole-type (ast-child 'xsmithholetype n))
      (define reference? (att-value '_xsmith_is-reference-node? n))
      (define already-atomic? (null? (filter (λ (cn) (ast-node? cn))
                                             (ast-children/flat n))))
      (when (and (not is-binder?)
                 hole-type
                 (or reference? (not already-atomic?)))
        (define p (ast-parent n))
        (define new-hole (make-hole/hole-name hole-type))
        (rewrite-subtree n new-hole)

        (define choices (filter (λ (c) (and (send c _xsmith_atomic?)
                                            (not (send c _xsmith_is-reference?))))
                                (att-value '_xsmith_hole->choice-list new-hole)))
        (define choices-or-reasons (map (λ (c) (send c _xsmith_apply-choice-filters))
                                        choices))
        (define choices-left (filter (λ (x) (is-a? x ast-choice%)) choices-or-reasons))

        (if (null? choices-left)
            ;; Fail, undo change
            (rewrite-subtree new-hole n)
            (let ([new-node (send (car choices-left) _xsmith_fresh)])
              (rewrite-subtree new-hole new-node)
              (if (version-successful!? (format "Reduced node to atomic (was size ~a).\n" (ast-size n)))
                  (return! #t)
                  (rewrite-subtree new-node n)))))


      ;; Tactic: replace reference node with most global, first declared definition of same type
      (when (and reference? hole-type)
        ;; TODO - the is-reference? property actually returns the name of the field.  I should maybe improve the name...
        (define reference-name-field reference?)
        (define p (ast-parent n))
        (define new-hole (make-hole-normal (ast-node-type n)))
        (rewrite-subtree n new-hole)

        (define choices (att-value '_xsmith_hole->choice-list new-hole))
        (define choices-or-reasons (map (λ (c) (send c _xsmith_apply-choice-filters))
                                        choices))
        (define choices-left (filter (λ (x) (is-a? x ast-choice%)) choices-or-reasons))
        (if (not (equal? 1 (length choices-left)))
            ;; Abort
            (rewrite-subtree new-hole n)
            (let* ([choice (car choices)]
                   [reference-options (send choice _xsmith_reference-options!)]
                   [non-lift (filter binding? reference-options)]
                   [definitions (filter (λ (x) (eq? 'definition (binding-def-or-param x)))
                                        non-lift)]
                   [distances-from-root (map (λ (x) (distance-from-root (binding-ast-node x)))
                                             definitions)]
                   [min-distance-from-root (and (not (null? distances-from-root))
                                                (apply min distances-from-root))]
                   [closest-to-root (and min-distance-from-root
                                         (filter (λ (x) (equal? min-distance-from-root
                                                                (distance-from-root (binding-ast-node x))))
                                                 definitions))]
                   [sorted-by-lift-numbers
                    (and min-distance-from-root
                         (sort closest-to-root
                               >
                               #:key (λ (x)
                                       (define m (regexp-match #px"\\d+" (binding-name x)))
                                       (or (and m (string->number (car m)))
                                           +inf.0))))]
                   [best-binding (and sorted-by-lift-numbers
                                     (car sorted-by-lift-numbers))]
                   [new-name (and best-binding (binding-name best-binding))]
                   [new-node (and new-name
                                  (not (equal?
                                        new-name
                                        (ast-child reference-name-field n)))
                                  (send choice _xsmith_fresh
                                        (hash reference-name-field new-name)))])
              (if (not new-node)
                  ;; Abort
                  (rewrite-subtree new-hole n)
                  (begin
                    (rewrite-subtree new-hole new-node)
                    (if (version-successful!? (format "Reduced: replaced reference with top lift\n"))
                        (return! #t)
                        (rewrite-subtree new-node n)))))))


      ;; Tactic: delete list node children
      (delete-list-node-children n (λ (x) (not (att-value '_xsmith_binder-type-field x))))


      ;; Tactic: recur to children
      (for ([cn (filter ast-node? (ast-children/flat n))])
        ;; We just ignore the return.
        (reduce-node cn))))

  (define (delete-list-node-children n remove-predicate)
    (define had-success? #f)
    (define reducibles (att-value '_xsmith_reducible-list-fields n))
    (let ([list-nodes
           (filter (λ (cn) (and (ast-node? cn)
                                (ast-list-node? cn)))
                   (match reducibles
                     [#t (ast-children n)]
                     [#f '()]
                     [(list field ...) (map (λ (f) (ast-child f n)) field)]))])
      (for ([list-node list-nodes])
        (let list-delete-loop ([i 1])
          ;; RACR uses 1-based indexing
          (when (<= i (ast-num-children list-node))
            (define removed-child (ast-child i list-node))
            (if (remove-predicate removed-child)
                (begin
                  (rewrite-delete removed-child)
                  (if (version-successful!? (format "Reduced: deleted node of size ~a\n" (ast-size removed-child)))
                      (begin
                        (set! had-success? #t)
                        (list-delete-loop i))
                      (begin (rewrite-insert list-node i removed-child)
                             (list-delete-loop (add1 i)))))
                (list-delete-loop (add1 i)))))))
    had-success?)

  (define (remove-unused-binders n)
    (define success?
      (delete-list-node-children
       n
       ;; Only delete if it's a definition node that has no references.
       (λ (def-node)
         (and (att-value '_xsmith_binder-type-field def-node)
              (let ([refs (att-value
                           'xsmith_find-descendants
                           root
                           (λ (cn)
                             (define binding
                               (and (att-value '_xsmith_is-reference-node? cn)
                                    (att-value '_xsmith_resolve-reference cn)))
                             (and binding (eq? (binding-ast-node binding)
                                               def-node))))])
                (null? refs))))))

    (define descendant-successes
      (for/list ([cn (filter ast-node? (ast-children/flat n))])
        (remove-unused-binders cn)))

    (or success? (ormap (λ(x)x) descendant-successes)))


  ;;; GO!

  (define orig-size (ast-size root))

  (reduce-node root)
  (let loop ()
    (when (remove-unused-binders root)
      (loop)))
  (define final-size (ast-size root))
  (define size-diff (- orig-size final-size))
  (xd-printf "reduction reduced from ~a to ~a nodes, a reduction of ~a (~a%)"
             orig-size final-size size-diff (* 100 (- 1 (/ (* 1.0 final-size) orig-size))))
  root)
