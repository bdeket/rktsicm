#lang racket/base

(require (rename-in racket/flonum
                    [fl=  flo:=]
                    [fl<  flo:<]
                    [fl<= flo:<=]
                    [fl>  flo:>]
                    [fl>= flo:>=]
                    [fl+  flo:+]
                    [fl-  flo:-]
                    [fl*  flo:*]
                    [fl/  flo:/]
                    [flabs flo:abs]
                    [flexpt flo:expt]
                    [flsqrt flo:sqrt]
                    [fllog flo:log]
                    [flcos flo:cos]
                    [flsin flo:sin]
                    [fltan flo:tan]
                    [flacos flo:acos]
                    [flasin flo:asin]
                    [flatan flo:atan]))
(provide (all-from-out racket/flonum)
         (all-defined-out))

(define (flo:zero? x) (flo:= 0. x))
(define (flo:negate x) (flo:- x))
(define flo:flonum? flonum?)
(define (flo:negative? x) (flo:< x 0.))
(define (flo:atan2 y x) (atan y x))
(define (flo:random-unit state) (random-seed state) (random))
(define (flo:truncate->exact x) (inexact->exact (truncate x)))