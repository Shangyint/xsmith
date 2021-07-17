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

(provide
 ast-choice%
 choose-ast
 current-hole

 print-choice-log
 )

(require
 racr
 racket/class
 racket/list
 (for-syntax
  clotho/racket/base
  ))

#|
Choices for AST growth should be some sort of object.

* They should have some "fresh-me" method that generates a fresh ast-node with appropriate holes, etc.  What exactly it generates could be altered by constraints available.
* They should have some weight score that affects their chances of being chosen.
* They should have an associated set of features that affects (enables/disables)
*   their availability to be chosen.
* They should be able to be refined based on new constraints, and should detect when the choice is no longer possible due to conflicting constraints.
** For some things this could be done by choices having a list of sub-choices that can be filtered, but for others (eg. with large choice spaces) this should happen in some other way.

|#

(define-syntax (current-hole stx)
  (syntax-case stx ()
    [(_) #'(get-field hole this)]
    [current-hole
     (raise-syntax-error 'current-hole
                         "Previously current-hole was allowed as an identifier macro, but now it needs to be wrapped in parentheses like a normal macro.  This is technically backwards-incompatible, but it's a really small change, and xsmith is research software."
                         stx)]))

(define ast-choice%
  (class object%
    (init-field hole)
    ;; Choice objects all will have an _xsmith_choice-weight method.
    ;; But they will also have a bunch of other methods defined by other properties.
    ;; So to treat them all with define/public uniformly,
    ;; let's just not define any here on the base class.
    (super-new)
    ))

(define (choose-ast ast-choice-list hole-symbol)
  ;; Weights are integers.
  ;; A random number should be generated between 0 and the sum of the weights.
  ;; Make a list of lists where sublists have the low-value for the bucket
  ;; and the value when the random number falls in that bucket.
  (when (null? ast-choice-list)
    (error 'choose-ast "given empty ast choice list"))
  (define-values (total-weight choice-list)
    (for/fold ([sum 0]
               [clist '()])
              ([c ast-choice-list])
      (define c-weight (send c _xsmith_choice-weight))
      (values (+ sum c-weight)
              (cons (list sum c) clist))))
  (define r (random total-weight))
  (define choice
    (let loop ([choices choice-list])
      (if (>= r (first (first choices)))
          (second (first choices))
          (loop (rest choices)))))
  (log-choice (send choice _xsmith_node-symbol)
              (map (λ (c) (send c _xsmith_node-symbol)) ast-choice-list)
              hole-symbol)
  choice)

(define (make-log-dict) (hasheq))
(define log/hole->choice->valid-freq (make-log-dict))
(define log/hole->num-choices->freq (make-log-dict))
(define log/hole->choice->freq (make-log-dict))
(define log/hole->freq (make-log-dict))
(define log/choice->freq (make-log-dict))

(define (log-choice choice choices-possible hole)
  (set! log/hole->choice->valid-freq
        (hash-update log/hole->choice->valid-freq
                     hole
                     (λ (choice->valid-freq)
                       (for/fold ([c->vf choice->valid-freq])
                                 ([c choices-possible])
                         (hash-update c->vf c add1 0)))
                     (make-log-dict)))
  (define (inc-2-level dict k1 k2)
    (hash-update dict
                 k1
                 (λ (d2) (hash-update d2 k2 add1 0))
                 (make-log-dict)))
  (set! log/hole->num-choices->freq
        (inc-2-level log/hole->num-choices->freq hole (length choices-possible)))
  (set! log/hole->choice->freq
        (inc-2-level log/hole->choice->freq hole choice))
  (set! log/hole->freq
        (hash-update log/hole->freq hole add1 0))
  (set! log/choice->freq
        (hash-update log/hole->freq choice add1 0))
  )

(define (print-choice-log)
  (define (header label)
    (printf "\n\n~a\n" label)
    (printf "==================================================\n"))

  (define all-keys
    (remove-duplicates
     (flatten
      (append (hash-keys log/hole->choice->valid-freq)
              (for/list ([(k v) (in-hash log/hole->choice->valid-freq)])
                (hash-keys v))))))
  (define key-max-len
    (apply max (map (λ (sym) (string-length (symbol->string sym)))
                    all-keys)))
  (define (cpad x)
    (define this-len (string-length (symbol->string x)))
    (define pad-string (make-string (max 0 (- key-max-len this-len)) (string-ref "." 0)))
    (string-append (symbol->string x) pad-string))
  (define (dpad n)
    (define ns (number->string n))
    (define pad-string (make-string (max 0 (- 3 (string-length ns))) (string-ref " " 0)))
    (string-append pad-string ns))

  (header "Given a hole, frequency a choice is (1) valid (2) chosen")
  (for ([h (remove-duplicates (append (hash-keys log/hole->choice->valid-freq)
                                      (hash-keys log/hole->choice->freq)))])
    (printf "~a hole:\n" h)
    (define c->vf (hash-ref log/hole->choice->valid-freq h (hash)))
    (define c->f (hash-ref log/hole->choice->freq h (hash)))
    (for ([c (hash-keys c->f)])
      (printf "\t~a\t~a\t~a\n"
              (cpad c) (dpad (hash-ref c->vf c 0)) (dpad (hash-ref c->f c 0)))))

  (header "Frequency a given number of choices is valid for a hole")
  (for ([h (hash-keys log/hole->num-choices->freq)])
    (printf "~a hole:\n" h)
    (define nc->f (hash-ref log/hole->num-choices->freq h))
    (for ([nc (hash-keys nc->f)])
      (printf "\t~a\t~a\n" (dpad nc) (dpad (hash-ref nc->f nc)))))

  (header "Frequency of hole")
  (for ([h (hash-keys log/hole->freq)])
    (printf "~a hole:\t~a\n" (cpad h) (dpad (hash-ref log/hole->freq h))))

  (header "Frequency of choice")
  (for ([h (hash-keys log/choice->freq)])
    (printf "~a choice:\t~a\n" (cpad h) (dpad (hash-ref log/choice->freq h))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; End of file.
