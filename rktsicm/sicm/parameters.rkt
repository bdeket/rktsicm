#lang racket/base

(provide (all-defined-out))
(require (only-in "rkt/glue.rkt" true))

;***************************************************************************************************
;* kernel/extapply                                                                                 *
;***************************************************************************************************
;;bdk;; insert 1
(define *enable-generic-apply* (make-parameter true))
;;bdk;; insert 1 end

;;; *enable-literal-apply* is modulated by with-literal-apply-enabled.
;;; This procedure is defined in kernel/extapply.scm.
;;; This feature is used explicitly in ode/interface.scm.
;;bdk;; insert 2
(define *enable-literal-apply* (make-parameter #f))
;;bdk;; insert 2 end

;***************************************************************************************************
;* kernel/litfun                                                                                   *
;***************************************************************************************************
;;bdk;; insert 3
(define *literal-reconstruction* (make-parameter #f))
;;bdk;; insert 3 end

;***************************************************************************************************
;* kernel/numsymb                                                                                  *
;***************************************************************************************************
;;bdk;; insert 4
(define enable-constructor-simplifications? (make-parameter #t))
;;bdk;; insert 4 end
;;bdk;; insert 5
(define incremental-simplifier (make-parameter #f))
;;bdk;; insert 5 end

;***************************************************************************************************
;* for calculus/SR-tools                                                                           *
;***************************************************************************************************
(define *c* (make-parameter 299792458))
