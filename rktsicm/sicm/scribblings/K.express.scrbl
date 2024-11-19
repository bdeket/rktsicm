#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/express))

@title[#:tag "kernel/express"]{Express}
@defmodule[sicm/kernel/express #:packages ("rktsicm")]

@deftempproc*[
 add-property!
 compound-data-constructor?
 down-constructor-name
 down-maker?
 expr:<
 expr:=
 expression
 expression-of
 expression-walker
 first-operand
 generate-list-of-symbols
 get-property
 has-property?
 make-combination
 make-literal
 make-numerical-literal
 make-real-literal
 matrix-by-columns-maker?
 matrix-by-rows-maker?
 matrix-maker?
 operands
 operator
 pair-up
 procedure-expression
 procedure-name
 quaternion-maker?
 rest-operands
 second-operand
 substitute
 up-constructor-name
 up-maker?
 variables-in
 vector-maker?]
