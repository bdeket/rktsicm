#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/numsymb))

@title[#:tag "kernel/numsymb"]{Numsymb}
@defmodule[sicm/kernel/numsymb #:packages ("rktsicm")]

@deftempproc*[
 *conjugate-transparent-operators*
 abs?
 absolute-integer-tolerance
 acos?
 addto-symbolic-operator-table
 addup-args
 allow-nary-difference-quotient
 almost-integer?
 angle?
 asin?
 atan?
 conjugate?
 cos?
 cosh?
 csc?
 cube?
 derivative?
 difference?
 ederivative?
 enable-constructor-simplifications
 enable-constructor-simplifications?
 equality?
 exp?
 expt?
 heuristic-number-canonicalizer
 heuristic-sin-cos-simplify
 imag-part?
 incremental-simplifier
 invert?
 log?
 magnitude?
 make-numsymb-expression
 make-polar?
 make-rectangular?
 max?
 min?
 mulup-args
 n:-pi/2-mod-2pi?
 n:-pi/4-mod-pi?
 n:2pi
 n:pi
 n:pi-mod-2pi?
 n:pi/2
 n:pi/2-mod-2pi?
 n:pi/2-mod-pi?
 n:pi/3
 n:pi/4
 n:pi/4-mod-pi?
 n:pi/6
 n:zero-mod-2pi?
 n:zero-mod-pi?
 negate?
 numerical-expression
 numerical-expression-canonicalizer
 product?
 quotient?
 real-part?
 relative-integer-tolerance
 sec?
 sin?
 sinh?
 sqrt?
 square?
 sum?
 symb1:*
 symb1:+
 symb1:-
 symb1:/
 symb:&
 symb:*
 symb:+
 symb:-
 symb:-pi/2-mod-2pi?
 symb:-pi/4-mod-pi?
 symb:/
 symb:=
 symb:=:bin
 symb:abs
 symb:acos
 symb:add
 symb:add:n
 symb:addends
 symb:and
 symb:angle
 symb:anglexpr
 symb:asin
 symb:atan
 symb:conjugate
 symb:cos
 symb:cosh
 symb:csc
 symb:cube
 symb:denominator
 symb:derivative
 symb:dif
 symb:dif:n
 symb:difference
 symb:dividend
 symb:divisor
 symb:elementary-access?
 symb:exp
 symb:expt
 symb:if
 symb:imag-part
 symb:invert
 symb:log
 symb:magexpr
 symb:magnitude
 symb:make-polar
 symb:make-rectangular
 symb:max
 symb:min
 symb:minuend
 symb:mul
 symb:mul:n
 symb:multiplicands
 symb:negate
 symb:not
 symb:numerator
 symb:one?
 symb:or
 symb:pi-mod-2pi?
 symb:pi/2-mod-2pi?
 symb:pi/2-mod-pi?
 symb:pi/4-mod-pi?
 symb:product
 symb:quo
 symb:quo:n
 symb:quotient
 symb:real-part
 symb:sec
 symb:sin
 symb:sinh
 symb:sqrt
 symb:square
 symb:subtrahend
 symb:sum
 symb:tan
 symb:zero-mod-2pi?
 symb:zero-mod-pi?
 symb:zero?
 symbolic-operator-table
 tan?]
