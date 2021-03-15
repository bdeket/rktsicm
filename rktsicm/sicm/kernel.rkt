#lang racket/base

(require (except-in racket/base
                    inexact? zero?
                    = < <= > >=
                    + - * /
                    sqrt exp log abs expt gcd
                    sin cos tan asin acos atan
                    make-rectangular make-polar real-part imag-part magnitude angle
                    apply compose object-name filter raise
                    #%top #%app)
         "kernel-gnrc.rkt"
         (only-in "kernel/extapply.rkt" #%top #%app))

(provide
 ;from racket/base
  (all-from-out racket/base)

 ;new top&app
 #%top
 #%app

 ;from litfun
 (all-from-out "kernel-gnrc.rkt"))

