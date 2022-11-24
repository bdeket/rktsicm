#lang s-exp "extapply.rkt"

(provide (all-defined-out)
         (all-from-out "cstm/pseries.rkt"))

(require "cstm/pseries.rkt"
         "cstm/mathutil.rkt"
         "todo/display-print.rkt"
         )

;;;; Power-series arithmetic using infinite streams.

(define (series:print s . optionals)
  (apply series:for-each
	 print-expression
	 s
	 optionals))

(define (series:sum series order)
  (g:ref (partial-sums series) order))

