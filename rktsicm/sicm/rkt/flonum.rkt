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
                    [flcos flo:cos]
                    [flsin flo:sin]
                    [fltan flo:tan]))
(provide (all-from-out racket/flonum)
         (all-defined-out))

(define (flo:zero? x) (flo:= 0. x))
(define (flo:negate x) (flo:- x))
(define flo:flonum? flonum?)
(define (flo:negative? x) (flo:< x 0.))