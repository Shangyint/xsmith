#lang info
(define name "version-string-with-git-hash")
(define version "2.0.3")
(define git-commit "$Format:%h$")

(define scribblings '(("scribblings/version-string-with-git-hash.scrbl" () (library))))
(define deps '("base"
               ))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     ))
