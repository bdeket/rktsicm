#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/extapply))

@title[#:tag "kernel/extapply"]{Extapply}
@defmodule[sicm/kernel/extapply #:packages ("rktsicm")]

@deftempproc*[
 *enable-generic-apply*
 *enable-literal-apply*
 myapp
 with-literal-apply-enabled
 with-literal-reconstruction-enabled
 with-self-evaluating-unbound-variables
 with-underflow->zero]
