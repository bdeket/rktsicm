#lang racket/base

(require "simplify/pcf.rkt"
         "simplify/rcf.rkt"
         "simplify/fpf.rkt"
         "simplify/simplify.rkt"
         "simplify/split-poly.rkt"
         "simplify/symbenv.rkt"
         "simplify/syntax.rkt"
         "simplify/rule-syntax.rkt"
         "simplify/matcher.rkt"
         "simplify/rule-simplifier.rkt"
         "simplify/rules.rkt"
         "simplify/expand-nary.rkt"
         "simplify/default.rkt"
         "simplify/sparse.rkt"
         "simplify/sparse-gcd.rkt"
         "simplify/sparse-interpolate.rkt"
         "simplify/pcf-fpf.rkt"
         )

(provide
 (except-out
  (all-from-out "simplify/pcf.rkt"
                "simplify/rcf.rkt"
                "simplify/fpf.rkt"
                "simplify/simplify.rkt"
                "simplify/split-poly.rkt"
                "simplify/syntax.rkt"
                "simplify/rule-syntax.rkt"
                "simplify/matcher.rkt"
                "simplify/rule-simplifier.rkt"
                "simplify/default.rkt"
                "simplify/sparse.rkt"
                "simplify/sparse-gcd.rkt"
                "simplify/sparse-interpolate.rkt"
                "simplify/pcf-fpf.rkt"
                )
  simplify:assign-operations))
(require (only-in "rkt/environment.rkt" extend-environment
                  scmutils-base-environment symbolic-environment rule-environment)
         racket/runtime-path)
(define-runtime-path here ".")
(define (mkpath x) `(file ,(path->string (build-path here x))))
(void 'INSTALL-GENERICS-&-SETUP-ENVIRONMENT
      (simplify:assign-operations)
      (eval `(require ,@(map mkpath
                             (list "simplify/pcf.rkt"
                                   "simplify/rcf.rkt"
                                   "simplify/fpf.rkt"
                                   "simplify/simplify.rkt"
                                   "simplify/split-poly.rkt")))
            scmutils-base-environment)
      (symbolic-environment-maker symbolic-environment scmutils-base-environment)
      (extend-environment rule-environment symbolic-environment)
      (eval `(require ,@(map mkpath
                             (list "simplify/rules.rkt"
                                   "simplify/expand-nary.rkt")))
            rule-environment)
      (eval `(require ,@(map mkpath
                             (list "simplify/syntax.rkt"
                                   "simplify/rule-syntax.rkt"
                                   "simplify/matcher.rkt"
                                   "simplify/rule-simplifier.rkt"
                                   "simplify/default.rkt"
                                   "simplify/sparse.rkt"
                                   "simplify/sparse-gcd.rkt"
                                   "simplify/sparse-interpolate.rkt"
                                   "simplify/pcf-fpf.rkt")))
            scmutils-base-environment))
