#lang racket/base

(require "simplify/default.rkt"
         "simplify/expand-nary.rkt"
         "simplify/fpf.rkt"
         "simplify/matcher.rkt"
         "simplify/pcf.rkt"
         "simplify/pcf-fpf.rkt"
         "simplify/rcf.rkt"
         "simplify/rules.rkt"
         (only-in "simplify/rule-simplifier.rkt")
         "simplify/rule-syntax.rkt"
         "simplify/simplify.rkt"
         "simplify/sparse.rkt"
         "simplify/sparse-gcd.rkt"
         "simplify/sparse-interpolate.rkt"
         "simplify/split-poly.rkt"
         "simplify/symbenv.rkt"
         "simplify/syntax.rkt"
         )

(provide
 (except-out
  (all-from-out "simplify/default.rkt"
                "simplify/expand-nary.rkt"
                "simplify/fpf.rkt"
                "simplify/matcher.rkt"
                "simplify/pcf.rkt"
                "simplify/pcf-fpf.rkt"
                "simplify/rcf.rkt"
                "simplify/rules.rkt"
                "simplify/rule-simplifier.rkt"
                "simplify/rule-syntax.rkt"
                "simplify/simplify.rkt"
                "simplify/sparse.rkt"
                "simplify/sparse-gcd.rkt"
                "simplify/sparse-interpolate.rkt"
                "simplify/split-poly.rkt"
                "simplify/symbenv.rkt"
                "simplify/syntax.rkt"
                )
  simplify:assign-operations))
(void 'INSTALL-GENERICS
      (simplify:assign-operations))
