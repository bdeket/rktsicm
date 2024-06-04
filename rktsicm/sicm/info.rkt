#lang info

(define collection "sicm")

(define categories '(scientific))
(define pkg-desc "Port of scmutils to racket")

(define primary-file "main.rkt")

(define deps '("base" "compatibility-lib"))
(define build-deps '("base" "racket-doc" "at-exp-lib" "scribble-lib" "sandbox-lib"))

(define compile-omit-paths '("tests"))
(define test-omit-paths '(#rx"sicm[/|\\\\]calculus"
                          #rx"sicm[/|\\\\]display"
                          #rx"sicm[/|\\\\]doc"
                          #rx"sicm[/|\\\\]enclose"
                          #rx"sicm[/|\\\\]general"
                          #rx"sicm[/|\\\\]info"
                          #rx"sicm[/|\\\\]kernel"
                          #rx"sicm[/|\\\\]lang"
                          #rx"sicm[/|\\\\]mechanics"
                          #rx"sicm[/|\\\\]numerics"
                          #rx"sicm[/|\\\\]poly"
                          #rx"sicm[/|\\\\]parameters"
                          #rx"sicm[/|\\\\]rkt[/|\\\\]"
                          #rx"sicm[/|\\\\]simplify"
                          #rx"sicm[/|\\\\]solve"
                          #rx"sicm[/|\\\\]units"
                          "tests/0course"
                          "tests/compiled"
                          #rx".*\\.bak"
                          ))
(define test-include-paths '("tests"))

(define scribblings '(["scribblings/rktsicm.scrbl" (multi-page)]))
