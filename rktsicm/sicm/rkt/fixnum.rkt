#lang racket/base

(require (rename-in racket/fixnum
                    [fx=  fix:=]
                    [fx<  fix:<]
                    [fx<= fix:<=]
                    [fx>  fix:>]
                    [fx>= fix:>=]
                    [fx+  fix:+]
                    [fx-  fix:-]
                    [fx*  fix:*]
                    [fxquotient fix:quotient]))
(provide (all-from-out racket/fixnum))