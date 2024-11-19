#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/numeric))

@title[#:tag "kernel/numeric"]{Numeric}
@defmodule[sicm/kernel/numeric #:packages ("rktsicm")]

@(defthing*[(*no-rationals-in-divide* boolean?)])
variable that controls the behavior of @racket[scheme-number-divide]

@deftype*[real?
          [zero :zero
           one :one -one :-one
           two :two three :three
           pi :pi :+pi :-pi -pi
           2pi :2pi :+2pi -2pi :-2pi
           pi/2 :pi/2 :+pi/2 -pi/2 :-pi/2
           pi/3 :pi/3 :+pi/3 -pi/3 :-pi/3
           pi/4 :pi/4 :+pi/4 -pi/4 :-pi/4
           pi/6 :pi/6 :+pi/6 -pi/6 :-pi/6
           :euler :ln10 :ln2 :minlog :phi]]
some predefined constants

@deftype*[flonum? [*machine-epsilon* *sqrt-machine-epsilon*]]
smallest step from 1.0 that can be represented

@deftempproc*[
 ulp
 ulpr

 binomial-coefficient
 close-enuf?
 cubic
 exact-complex?
 exact-one?
 exact-quotient
 exact-rational?
 exact-zero?
 factorial
 gcd-complex
 gcd-rational
 integer-divide
 integer-divide-quotient
 integer-divide-remainder
 make-rational
 principal-range
 principal-value
 principal-value-minus-pi-to-pi
 principal-value-zero-to-2pi
 quadratic
 round-complex
 safelog
 scheme-number-divide
 scheme-number-gcd
 sgn
 sigma-KahanBabushkaNeumaier
 stirling-first-kind
 stirling-second-kind
  
 ~0?]
