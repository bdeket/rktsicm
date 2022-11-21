#lang racket/base

(require "kernel-intr.rkt"
         "calculus/basis.rkt"
         "calculus/connection.rkt"
         "calculus/coord.rkt"
         "calculus/covariant-derivative.rkt"
         "calculus/curvature.rkt"
         "calculus/dgutils.rkt"
         "calculus/exterior-derivative.rkt"
         "calculus/form-fields.rkt"
         "calculus/frame-maker.rkt"
         "calculus/gram-schmidt.rkt"
         "calculus/hodge-star.rkt"
         "calculus/indexed.rkt"
         "calculus/interior-product.rkt"
         "calculus/Lie.rkt"
         "calculus/manifold.rkt"
         "calculus/maps.rkt"
         "calculus/metric.rkt"
         "calculus/ode.rkt"
         "calculus/SR-boost.rkt"
         "calculus/SR-frames.rkt"
         "calculus/tensors.rkt"
         "calculus/vector-calculus.rkt"
         "calculus/vector-fields.rkt"
         "calculus/wedge.rkt"
         )

(provide
 (except-out
  (all-from-out "calculus/basis.rkt"
                "calculus/connection.rkt"
                "calculus/coord.rkt"
                "calculus/covariant-derivative.rkt"
                "calculus/curvature.rkt"
                "calculus/dgutils.rkt"
                "calculus/exterior-derivative.rkt"
                "calculus/form-fields.rkt"
                "calculus/frame-maker.rkt"
                "calculus/gram-schmidt.rkt"
                "calculus/hodge-star.rkt"
                "calculus/indexed.rkt"
                "calculus/interior-product.rkt"
                "calculus/Lie.rkt"
                "calculus/manifold.rkt"
                "calculus/maps.rkt"
                "calculus/metric.rkt"
                "calculus/ode.rkt"
                "calculus/SR-boost.rkt"
                "calculus/SR-frames.rkt"
                "calculus/tensors.rkt"
                "calculus/vector-calculus.rkt"
                "calculus/vector-fields.rkt"
                "calculus/wedge.rkt"
                )
  indexed:assign-operations
  manifold:assign-operations
  form-fields:assign-operations
  vector-fields:assign-operations))
(void 'INSTALL-GENERICS
      (indexed:assign-operations)
      (manifold:assign-operations)
      (form-fields:assign-operations)
      (vector-fields:assign-operations))
