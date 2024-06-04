#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm))

@title[#:tag "kernel/generic"]{Generic}
@defmodule[sicm/generic #:packages ("rktsicm") #:lang]
The @racket[sicm/generic] language provides all generic @racketid[g:function]s as @racketid[function]. The original racket @racketid[function]s are exported as @racketid[rkt:function]

@defsameproc*[[([v value?] ...) value? (+ g:+)]
              [([v1 value?] [v2 value?]) value? (g:+:bin generic:+)]
              [([vs (listof value?)]) value? (g:+:n)]]
Generic version of @racket[rkt:+]. The result for 0 arguments is the additive identity, if it exists. For numbers this is 0.

@defsameproc*[[([v value?] ...) value? (- g:-)]
              [([v1 value?] [v2 value?]) value? (g:-:bin generic:-)]
              [([vs (listof value?)]) value? (g:-:n)]]
Generic version of @racket[rkt:-]. The result for 0 arguments is the additive identity, if it exists. For numbers this is 0.

@defsameproc*[[([v value?] ...) value? (* g:*)]
              [([v1 value?] [v2 value?]) value? (g:*:bin generic:*)]
              [([vs (listof value?)]) value? (g:*:n)]]
Generic version of @racket[rkt:*]. The result for 0 arguments is the multiplicative identity, if it exists. For numbers this is 1.

@defsameproc*[[([v value?] ...) value? (/ g:/)]
              [([v1 value?] [v2 value?]) value? (g:/:bin generic:/)]
              [([vs (listof value?)]) value? (g:/:n)]]
Generic version of @racket[rkt:/]. The result for 0 arguments is the multiplicative identity, if it exists. For numbers this is 1.

@defsameproc*[[([v value?] ...) value? (gcd g:gcd)]
              [([v1 value?] [v2 value?]) value? (g:gcd:bin generic:gcd)]
              [([vs (listof value?)]) value? (g:gcd:n)]]
Generic version of @racket[rkt:gcd].

@defsameproc*[[([e value?]) value? (exp g:exp generic:exp)]]
Generic versions of @racket[rkt:exp]
@defsameproc*[[([b value?] [e value?]) value? (expt g:expt generic:expt)]]
Generic versions of @racket[rkt:expt]
@defsameproc*[[([v value?]) value? (square g:square generic:square)]]
Generic versions of @racket[(expt v 2)]
@defsameproc*[[([v value?]) value? (sqrt g:sqrt generic:sqrt)]]
Generic versions of @racket[rkt:sqrt]

@defsameproc*[[([v value?]) value? (log g:log generic:log)]]
Generic versions of @racket[rkt:log] with 1 argument (natural logarithm).

@defsameproc*[[([v value?]) value? (exp2 g:exp2 exp10 g:exp10 log2 g:log2 log10 g:log10)]]
Generic versions of @racket[rkt:exp] and @racket[rkt:log] but with base 2 or 10 respectively.

@defsameproc*[[([v value?]) value? (cos g:cos generic:cos
                                    sin g:sin generic:sin
                                    tan g:tan
                                    sec g:sec
                                    csc g:csc
                                    cot g:cot
                                    acos g:acos generic:acos
                                    asin g:asin generic:asin
                                    g:atan1 generic:atan1)]
              [([y value?] [x value?]) value?  (g:atan2 generic:atan2)]
              [([y value?] [x value? onle-like]) value? (atan g:atan)]
              [([v value?]) value? (cosh g:cosh generic:cosh
                                    sinh g:sinh generic:sinh
                                    tanh g:tanh
                                    sech g:sech
                                    csch g:csch
                                    acosh g:acosh
                                    asinh g:asinh
                                    atanh g:atanh)]]
Generic goniometric functions, hyperbolic goniometric functions and their inverses

@deftempproc*[
 g:abs generic:abs

 g:<
 g:<:bin generic:<
 g:<:n

 g:<=
 g:<=:bin generic:<=
 g:<=:n

 g:=
 g:=:bin generic:=
 g:=:n

 g:>
 g:>:bin generic:>
 g:>:n

 g:>=
 g:>=:bin generic:>=
 g:>=:n

 g:angle generic:angle


 g:apply generic:apply
 g:arity generic:arity
 g:conjugate generic:conjugate
 g:cross-product generic:cross-product
 g:derivative
 g:determinant generic:determinant
 g:dimension generic:dimension
 g:dot-product generic:dot-product
 g:identity 
 g:identity-like generic:identity-like
 g:identity? generic:identity?
 g:imag-part generic:imag-part
 g:inexact? generic:inexact?
 g:invert generic:invert
 g:magnitude generic:magnitude
 g:make-polar generic:make-polar
 g:make-rectangular generic:make-rectangular
 g:negate generic:negate
 g:one-like generic:one-like
 g:one? generic:one?
 g:outer-product generic:outer-product
 g:partial generic:partial-derivative
 g:partial-derivative
 g:real-part generic:real-part
 g:sigma
 g:simplify generic:simplify
 g:solve-linear generic:solve-linear
 g:solve-linear-left generic:solve-linear-left
 g:solve-linear-right generic:solve-linear-right
 g:trace generic:trace
 g:transpose generic:transpose
 g:type generic:type
 g:type-predicate generic:type-predicate
 g:zero-like generic:zero-like
 g:zero? generic:zero?

 applicable-literal?
 install-g:apply-case]
