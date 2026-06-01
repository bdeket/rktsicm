#lang racket/base

(require "solve/solve.rkt"
         "solve/solve-utils.rkt"
         )

(provide (struct-out solution) residual-equations residual-variables substitutions tough-equations
         (struct-out equation) make-equation
         solve-incremental
         solve-equations
         simple-solve)