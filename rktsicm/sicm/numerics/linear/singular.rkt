#lang racket/base

(provide (all-defined-out))

;;; We define the standard singular-matrix failure continuations as follows.

(define (barf-on-zero-pivot dismiss)
  ((singular-matrix-error)))

(define (allow-zero-pivot dismiss)
  (dismiss))

;;; Rebind this to catch errors
(define singular-matrix-error (make-parameter (λ () error)))

;;; default value
(define (default-singular-matrix-error)
  (error "Singular matrix - zero pivot"))

(singular-matrix-error default-singular-matrix-error)

(define (with-singular-matrix-handler handler thunk)
  (parameterize ([singular-matrix-error handler])
    (thunk)))

(define (handle-singularity-errors-with error-handler)
  (singular-matrix-error error-handler))
