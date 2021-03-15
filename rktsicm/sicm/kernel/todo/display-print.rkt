#lang racket

(provide (except-out (all-defined-out) mk))

(require (for-syntax racket/base
                     racket/syntax))

;print-display needs a lot of things from kernel
;to avoid restructuring everything at this point
;I just defined stumps for the procedures and setters
;that will be overwritten in display/print
(define-syntax (mk stx)
  (syntax-case stx ()
    [(_ id _ ...)
     (with-syntax ([setter (format-id #'id "set-~a!" #'id)])
       #'(begin
           (define (id . rst) (error "needs to come from display/print"))
           (define (setter fct) (set! id fct))))]))

;for quaternions
(mk careful-simplify "display/print")


;for pseries
(mk print-expression "display/print")

;for makenumber
(mk simplify "display/print")