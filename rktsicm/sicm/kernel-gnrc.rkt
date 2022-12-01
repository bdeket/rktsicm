#lang racket/base

(require "kernel-intr.rkt"
         )

(provide
 (except-out (all-from-out "kernel-intr.rkt")
             one? negate invert
             square cube exp2 exp10 log2 log10
             cot sec csc sinh cosh tanh sech csch
             conjugate sigma compose)

 (rename-out
  ;unary operators from generic
  [g:type type]
  [g:type-predicate type-predicate]
  [g:arity arity]

  [g:inexact? inexact?]

  [g:zero-like zero-like]
  [g:one-like one-like]
  [g:identity-like identity-like]

  [g:zero? zero?]
  [g:one? one?]
  [g:identity? identity?]

  [g:negate negate]
  [g:invert invert]

  [g:square square]
  [g:cube cube]
          
  [g:sqrt sqrt]

  [g:exp exp]
  [g:exp2 exp2]
  [g:exp10 exp10]
  [g:log log]
  [g:log2 log2]
  [g:log10 log10]

  [g:sin sin]
  [g:cos cos]
  [g:tan tan]
  [g:cot cot]
  [g:sec sec]
  [g:csc csc]

  [g:asin asin]
  [g:acos acos]

  [g:sinh sinh]
  [g:cosh cosh]
  [g:tanh tanh]
  [g:sech sech]
  [g:csch csch]

  [g:asinh asinh]
  [g:acosh acosh]
  [g:atanh atanh]

  [g:abs abs]

  [g:determinant determinant]
  [g:trace trace]
  [g:transpose transpose]
  [g:dimension dimension]

  [g:solve-linear-left solve-linear-left]
  [g:solve-linear-right solve-linear-right]
  [g:solve-linear solve-linear]

  [g:derivative derivative]

  ;; Binary (and nary) operators from generic

  [g:= =]
  [g:< <]
  [g:<= <=]
  [g:> >]
  [g:>= >=]

  [g:+ +]
  [g:- -]
  [g:* *]
  [g:/ /]

  [g:dot-product dot-product]
  [g:cross-product cross-product]

  [g:outer-product outer-product]

  [g:expt expt]
  [g:gcd gcd]

  ;; Complex operators from generic

  [g:make-rectangular make-rectangular]
  [g:make-polar make-polar]
  [g:real-part real-part]
  [g:imag-part imag-part]
  [g:magnitude magnitude]
  [g:angle angle]
  [g:conjugate conjugate]

  ;; Wierd operators from generic.scm

  [g:atan atan]

  [g:partial-derivative partial-derivative]
  [g:partial partial]

  [g:apply apply]

  ;; Compound operators from mathutil.scm

  [g:arg-scale arg-scale]
  [g:arg-shift arg-shift]

  [g:sigma sigma]

  [g:ref ref]
  [g:size size]

  [g:compose compose]
  ))
