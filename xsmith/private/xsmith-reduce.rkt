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
 racket/system
 racket/file
 racket/class
 racr
 (submod "types.rkt" for-private)
 "core-macros-and-properties.rkt"
 "xsmith-utils.rkt"
 (submod "xsmith-utils.rkt" for-private)
 (submod "grammar-macros.rkt" for-private_reduction)
 "choice.rkt"
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

(define (reduce-ast ast-original
                    ast->string
                    minimization-script
                    minimization-directory)
  (define script-full-path (path->string (path->complete-path minimization-script)))
  (make-directory* minimization-directory)
  (define min-file (build-path minimization-directory "reduction-candidate"))

  ;; Minimization unfortunately works via mutation, so let's just always refer to root
  (define root ast-original)

  (define (write-min-file! program-string)
    (with-output-to-file min-file
      #:mode 'text
      #:exists 'replace
      (λ () (display program-string))))
  (define (run-test-script!)
    (parameterize ([current-directory minimization-directory])
      ;; Returns true if the script exits successfully, which is our test
      (system script-full-path)))
  (define (version-successful!? message)
    (write-min-file! (ast->string root))
    (define success? (run-test-script!))
    (when (and success? message)
      (xd-printf message))
    success?)


  ;; First check that the minimization script is successful on the original AST.
  (define original-success? (version-successful!? #f))
  (when (not original-success?)
    (error 'reduce-ast "Reduction script failed for original program."))


  #|
  Tactics:
  * Delete (non-binder) list node member
  * Delete unused binder (still not parameter) list node member
  * Replace node with (non-reference) atomic choice
  * Replace reference with most global, first declared definition of same type
  * Recur to children
  * Climbing up after the recursion, search for descendant nodes of the same type, replace node with descendant (IE unwrap the critical section)

  Probably I want to just run each of those down the tree, then re-run the delete unused binder tactic until it fails to make a change.
  |#

  (define (reduce-node n)
    ;; Works by side effect on the AST.
    (let/ec return!


      ;; Tactic: replace (non-binder) node with atomic
      (define is-binder? (att-value '_xsmith_binder-type-field n))
      (define hole-type (ast-child 'xsmithholetype n))
      (define reference? (att-value '_xsmith_is-reference-node? n))
      (define already-atomic? (not (null? (filter (λ (cn) (ast-node? cn))
                                                  (ast-children/flat n)))))
      (when (and is-binder? hole-type (or reference? already-atomic?))
        (define p (ast-parent n))
        (define new-hole (make-hole-direct hole-type (ast-node-type n)))
        (rewrite-subtree n new-hole)

        (define choices (att-value '_xsmith_hole->choice-list new-hole))
        (define choices-or-reasons (map (λ (c) (send c _xsmith_apply-choice-filters))
                                        choices))
        (define choices-left (filter (λ (x) (is-a? x ast-choice%)) choices-or-reasons))
        (define good-choices (filter (λ (c) (and (send c _xsmith_atomic?)
                                                 (send c _xsmith_is-reference?)))
                                     choices-left))
        (if (null? good-choices)
            ;; Fail, undo change
            (rewrite-subtree new-hole n)
            (let ([new-node (send (car good-choices) _xsmith_fresh)])
              (rewrite-subtree new-hole new-node)
              (if (version-successful!? (format "Reduced node to atomic (was size ~a).\n" (ast-size n)))
                  (return! #t)
                  (rewrite-subtree new-node n)))))


      ;; Tactic: replace reference node with most global, first declared definition of same type
      (when reference?
        ;; TODO - implement
        (void))


      ;; Tactic: delete list node children (when not binders)
      (let ([list-nodes (filter (λ (cn) (and (ast-node? cn)
                                             (ast-list-node? cn)))
                                (ast-children n))])
        (for ([list-node list-nodes])
          (let list-delete-loop ([i 1])
            ;; RACR uses 1-based indexing
            (when (<= i (ast-num-children list-node))
              (define removed-child (ast-child i list-node))
              (rewrite-delete removed-child)
              (if (version-successful!? (format "Reduced: deleted node of size ~a\n" (ast-size n)))
                  (list-delete-loop i)
                  (begin (rewrite-insert list-node i removed-child)
                         (list-delete-loop (add1 i))))))))



      ;; Tactic: delete unused binders in list nodes
      ;; TODO - this should probably be done after I do the other transformations to the whole tree, then run to fixpoint


      ;; Tactic: recur to children
      (for ([cn (filter ast-node? (ast-children/flat n))])
        ;; We just ignore the return.
        (reduce-node cn))))

  (reduce-node root)
  root)
