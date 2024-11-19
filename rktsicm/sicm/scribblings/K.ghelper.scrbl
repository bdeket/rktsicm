#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/ghelper))

@title[#:tag "kernel/ghelper"]{Ghelper}
@defmodule[sicm/kernel/ghelper #:packages ("rktsicm")]

@deftempproc*[
 assign-operation
 get-operator-record
 get-operator-record-for
 make-assign-operations
 make-generic-operator]
