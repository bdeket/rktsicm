#lang info

(define collection "sicm")
(define compile-omit-paths '("tests"))
(define test-omit-paths '(#rx"sicm[/|\\\\]calculus"
                          #rx"sicm[/|\\\\]display"
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
                          ;#rx"compiled"
                          ;#rx".*\\.bak"
                          ))
(define test-include-paths '("test"))

(define primary-file "main.rkt")
(define categories '(math))
