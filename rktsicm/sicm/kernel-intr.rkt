#lang racket/base

(require "kernel/deriv.rkt"
         "kernel/diff.rkt"
         "kernel/express.rkt"
         (only-in "kernel/extapply.rkt"
                  with-literal-apply-enabled
                  with-literal-reconstruction-enabled
                  with-self-evaluating-unbound-variables)
         "kernel/function.rkt"
         "kernel/generic.rkt"
         "kernel/heuristic.rkt"
         "kernel/iterat.rkt"
         "kernel/litfun.rkt"
         "kernel/mathutil.rkt"
         "kernel/matrices.rkt"
         "kernel/modarith.rkt"
         "kernel/numbers.rkt"
         "kernel/numeric.rkt"
         "kernel/numsymb.rkt"
         "kernel/operator.rkt"
         "kernel/pseries.rkt"
         "kernel/quaternion.rkt"
         "kernel/structs.rkt"
         "kernel/strutl.rkt"
         "kernel/types.rkt"
         "kernel/utils.rkt"
         "kernel/vectors.rkt"
         )

(provide
 (except-out
  (all-from-out "kernel/deriv.rkt"
                "kernel/diff.rkt"
                "kernel/express.rkt"
                "kernel/extapply.rkt"
                "kernel/function.rkt"
                "kernel/generic.rkt"
                "kernel/heuristic.rkt"
                "kernel/iterat.rkt"
                "kernel/litfun.rkt"
                "kernel/mathutil.rkt"
                "kernel/matrices.rkt"
                "kernel/modarith.rkt"
                "kernel/numbers.rkt"
                "kernel/numeric.rkt"
                "kernel/numsymb.rkt"
                "kernel/operator.rkt"
                "kernel/pseries.rkt"
                "kernel/quaternion.rkt"
                "kernel/structs.rkt"
                "kernel/strutl.rkt"
                "kernel/types.rkt"
                "kernel/utils.rkt"
                "kernel/vectors.rkt")
  modarith:assign-operations
  diff:assign-operations
  deriv:assign-operations
  operator:assign-operations
  function:assign-operations
  numbers:assign-operations
  vectors:assign-operations
  structs:assign-operations
  matrices:assign-operations
  quaternion:assign-operations
  pseries:assign-operations))
(void 'INSTALL-GENERICS
      (modarith:assign-operations)
      (diff:assign-operations)
      (deriv:assign-operations)
      (operator:assign-operations)
      (function:assign-operations)
      (numbers:assign-operations)
      (vectors:assign-operations)
      (structs:assign-operations)
      (matrices:assign-operations)
      (quaternion:assign-operations)
      (pseries:assign-operations))
