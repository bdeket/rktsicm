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
(provide (all-from-out racket/fixnum)
         (all-defined-out))

(define (fix:zero? x) (fix:= 0 x))
(define (fix:negate x) (fix:- x))
(define (fix:1+ x) (fix:+ 1 x))
(define (fix:-1+ x) (fix:- x 1))
(define fix:fixnum? fixnum?)
(define (fix:negative? x) (fix:< x 0))
(define fix:lsh arithmetic-shift)