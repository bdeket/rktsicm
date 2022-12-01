#lang racket/base

(require (except-in "kernel.rkt"
                    one? negate invert
                    square cube exp2 exp10 log2 log10
                    cot sec csc sinh cosh tanh sech csch
                    conjugate sigma compose)
         "kernel-gnrc.rkt"
         "simplify.rkt" ;;for side effect!
         "display/print.rkt"
         )

(provide
 ;from kernel
  (all-from-out "kernel.rkt")

 ;from generic
 (all-from-out "kernel-gnrc.rkt")
 simplify)

