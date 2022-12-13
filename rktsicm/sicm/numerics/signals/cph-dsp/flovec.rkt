#lang racket/base

(provide (all-from-out "flovec-scm.rkt"))

(require "flovec-scm.rkt")

(module flo:vector racket/base
  (provide (all-from-out (submod "flovec-scm.rkt" flo:vector)))

  (require (submod "flovec-scm.rkt" flo:vector)))

;;bdk;; start original file

