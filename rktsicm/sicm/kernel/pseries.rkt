#lang s-exp "extapply.rkt"

(provide (all-defined-out)
         (all-from-out "cstm/pseries.rkt"))

(require "cstm/pseries.rkt"
         "cstm/mathutil.rkt"
         "todo/display-print.rkt"
         )

;;bdk;; start original file

;;;; Power-series arithmetic using infinite streams.


;;bdk;; moved to cstm/pseries 1

(define (series:print s . optionals)
  (apply series:for-each
	 print-expression
	 s
	 optionals))

;;bdk;; moved to cstm/pseries 2

(define (series:sum series order)
  (g:ref (partial-sums series) order))

;;bdk;; moved to cstm/pseries 3

