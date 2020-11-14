#!/usr/bin/env racket
#lang rash
(require file/glob)

(define n-trials 3)

(for ([trial (in-range n-trials)])
  (for ([i (in-range 2)])
    {
     racket -l xsmith-examples/racket-kernel-fuzzer/racket-kernel-fuzzer.rkt -- --max-depth 5 -s $trial &>! /tmp/repro-check-racket-$i
     racket -l xsmith-examples/python/python.rkt -- --max-depth 5 -s $trial &>! /tmp/repro-check-python-$i
     racket -l xsmith-examples/standard-ml/standard-ml.rkt -- --max-depth 5 -s $trial &>! /tmp/repro-check-sml-$i
     })
  (for ([lang '(racket python sml)])
    (define file0 (format "/tmp/repro-check-~a-~a" lang 0))
    (define file1 (format "/tmp/repro-check-~a-~a" lang 1))
    {diff $file0 $file1}))

echo
echo no diffs
