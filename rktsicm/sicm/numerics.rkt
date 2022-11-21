#lang racket/base

(require "numerics/functions.rkt"
         "numerics/linear.rkt"
         "numerics/ode.rkt"
         "numerics/optimize.rkt"
         "numerics/quad.rkt"
         "numerics/quadrature.rkt"
         "numerics/roots.rkt"
         "numerics/signals.rkt"
         "numerics/statistics.rkt"
         )

(provide
 (except-out
  (all-from-out "numerics/functions.rkt"
                "numerics/linear.rkt"
                "numerics/ode.rkt"
                "numerics/optimize.rkt"
                "numerics/quad.rkt"
                "numerics/quadrature.rkt"
                "numerics/roots.rkt"
                "numerics/signals.rkt"
                "numerics/statistics.rkt"
                )
  sigfun:assign-operations
  signals:assign-operations))
(void 'INSTALL-GENERICS
      (sigfun:assign-operations)
      (signals:assign-operations))
