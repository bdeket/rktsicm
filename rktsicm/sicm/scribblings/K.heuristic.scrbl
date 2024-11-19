#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/heuristic))

@title[#:tag "kernel/heuristic"]{Heuristic}
@defmodule[sicm/kernel/heuristic #:packages ("rktsicm")]

@deftempproc*[
 *important-numbers*
 h-c-c
 h-c-r
 heuristic-canonicalize-complex
 heuristic-canonicalize-real
 heuristic-one-part-insignificant
 heuristic-round-complex
 heuristic-round-real
 heuristic-rounding-denominator
 heuristic-rounding-tiny
 heuristic-rounding-tolerance
 heuristic-symbolize?]
