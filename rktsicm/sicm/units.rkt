#lang racket/base

(require "units/hms-dms-radians.rkt"
         "units/SI-units.rkt"
         "units/system.rkt"
         "units/units.rkt"
         "units/with-units.rkt"
         )

(provide
 (except-out
  (all-from-out "units/hms-dms-radians.rkt"
                "units/SI-units.rkt"
                "units/system.rkt"
                "units/units.rkt"
                "units/with-units.rkt")
  units:assign-operations
  with-units:assign-operations))
(void 'INSTALL-GENERICS
      (units:assign-operations #t)
      (with-units:assign-operations #t))