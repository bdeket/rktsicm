#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm))

@title[#:tag "kernel"]{Kernel}
@defmodule[sicm/kernel #:packages ("rktsicm") #:lang]

@local-table-of-contents[#:style 'immediate-only]

@include-section{04-01-generic.scrbl}
@|#|
@section[#:tag "kernel/generic"]{Generic}
@defmodule[sicm/generic #:packages ("rktsicm") #:lang]
The @racket[sicm/generic] language provides all generic @racketid[g:function]s as @racketid[function]. The original racket @racketid[function]s are exported as @racketid[rkt:function]

@defsameproc*[[([v value?] ...) value? (+ g:+)]
              [([v1 value?] [v2 value?]) value? (g:bin:+ generic:+)]
              [([vs (listof value?)]) value? (g:+:n)]]
Generic version of @racket[rkt:+]. The result for 0 arguments is the additive identity, if it exists. For numbers this is 0.

@defsameproc*[[([v value?] ...) value? (- g:-)]
              [([v1 value?] [v2 value?]) value? (g:bin:- generic:-)]
              [([vs (listof value?)]) value? (g:-:n)]]
Generic version of @racket[rkt:-]. The result for 0 arguments is the additive identity, if it exists. For numbers this is 0.

@defsameproc*[[([v value?] ...) value? (* g:*)]
              [([v1 value?] [v2 value?]) value? (g:bin:* generic:*)]
              [([vs (listof value?)]) value? (g:*:n)]]
Generic version of @racket[rkt:*]. The result for 0 arguments is the multiplicative identity, if it exists. For numbers this is 1.

@defsameproc*[[([v value?] ...) value? (/ g:/)]
              [([v1 value?] [v2 value?]) value? (g:bin:/ generic:/)]
              [([vs (listof value?)]) value? (g:/:n)]]
Generic version of @racket[rkt:/]. The result for 0 arguments is the multiplicative identity, if it exists. For numbers this is 1.

@defsameproc*[[([v value?] ...) value? (gcd g:gcd)]
              [([v1 value?] [v2 value?]) value? (g:bin:gcd generic:gcd)]
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
Generic goneometric functions, hyperbolic goneometric functions and their inverses

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

@section[#:tag "kernel/mathutil"]{Mathutil}
@deftempproc*[
 adjust-end
 adjust-index
 component
 g:arg-scale
 g:arg-shift
 g:compose
 g:compose-2
 g:compose-bin
 g:cube
 g:ref
 g:size
 ratnum?
 ref-internal]

@section[#:tag "kernel/numeric"]{Numeric}
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

@section[#:tag "kernel/utils"]{Utils}
@deftempproc*[
 *birkholz*
 *last-notes*
 *last-notes-shown*
 *notes*
 *showing-notes*
 *taking-notes*
 a-reduce
 accumulation
 all-equal?
 alphaless?
 any?
 apply-to-all
 binary-combine
 bracket
 clear-notes!
 compose-2
 compose-bin
 compose-n
 concatenate-names
 concatenate-names-maker
 constant
 cpp
 defer-application
 display-note
 do-down
 do-up
 identity
 inverse-accumulation
 iterate-until-stable
 iterated
 left-circular-shift
 make-function-of-arguments
 make-function-of-vector
 make-map
 make-pairwise-test
 nary-combine
 none?
 note-that!
 pp-it
 print-breadth
 print-depth
 right-circular-shift
 show-notes
 sign
 the-null-symbol
 unary-combine
 wallp-pp
 watch-it]

@subsection[#:tag "kernel/utils/arity"]{Arity}
@deftype*[arity? [*exactly-zero* *at-least-zero*
                  *exactly-one* *at-least-one*
                  *exactly-two* *at-least-two*
                  *one-or-two*
                  *exactly-three* *at-least-three*]]
Fixed definitions of procedure arities
@deftempproc*[
 any-number?
 arity-includes?
 arity-intersect
 arity-min
 combine-arity
 exact-arity
 exactly-n?
 joint-arity
 normalize-arity]

@section[#:tag "kernel/iterat"]{Iterat}
@deftempproc*[
 array-by-cols
 array-by-rows
 array-copy
 array-elementwise
 array-ref
 array-set!
 array-with-substituted-col
 array-with-substituted-row
 generate-array
 generate-list
 generate-vector
 list-with-substituted-coord
 list:generate
 nth-col
 nth-row
 num-cols
 num-rows
 transpose-array
 vector-accumulate
 vector-elementwise
 vector-exists
 vector-forall
 vector-with-substituted-coord]

@section[#:tag "kernel/express"]{Express}
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

@section[#:tag "kernel/ghelper"]{Ghelper}
@deftempproc*[
 assign-operation
 get-operator-record
 get-operator-record-for
 make-assign-operations
 make-generic-operator]

@section[#:tag "kernel/strutl"]{Strutl}
@deftempproc*[
 combiner-padded-streams
 cons-stream
 empty-stream?
 factorial-stream
 head
 infinite-stream-of
 integers-starting-from
 map-stream
 map-streams
 merge-streams
 natural-number-stream
 one-stream
 prime-numbers-stream
 print-stream
 shorten-stream
 stream->list
 stream-accumulate
 stream-append
 stream-apply
 stream-car
 stream-cdr
 stream-evaluate
 stream-filter
 stream-for-each
 stream-head
 stream-of-iterates
 stream-of-powers
 stream-pair?
 stream-ref
 stream-tail
 stream-take
 stream:for-each
 stream:inflate
 stream:list-append
 tail
 the-empty-stream
 zero-stream]

@section[#:tag "kernel/extapply"]{Extapply}
@deftempproc*[
 *enable-generic-apply*
 *enable-literal-apply*
 myapp
 with-literal-apply-enabled
 with-literal-reconstruction-enabled
 with-self-evaluating-unbound-variables
 with-underflow->zero]

@section[#:tag "kernel/types"]{Types}
@deftempproc*[
 *down*
 *function*
 *matrix*
 *number*
 *up*
 *vector*
 abstract-down-type-tag
 abstract-down?
 abstract-function-type-tag
 abstract-function?
 abstract-matrix-type-tag
 abstract-matrix?
 abstract-number?
 abstract-predicate
 abstract-quantity?
 abstract-structure?
 abstract-type-tag
 abstract-type-tags
 abstract-up-type-tag
 abstract-up?
 abstract-vector-type-tag
 abstract-vector?
 add-to-numerical-quantity?
 cofunction?
 compound-type-tag?
 compound-type-tags
 concrete-predicate
 differential-type-tag
 differential?
 down-quantity?
 down-type-tag
 down?
 f:expression
 function-quantity?
 function-type-tag
 function?
 literal-number?
 literal-real?
 make-type
 matrix-quantity?
 matrix-type-tag
 matrix?
 not-compound?
 not-d-c-u?
 not-differential-or-compound?
 not-differential?
 not-operator?
 not-series?
 number-type-tag
 numerical-quantity?
 operator-type-tag
 operator?
 quantity-predicate
 quaternion-quantity?
 quaternion-type-tag
 quaternion?
 scalar?
 series-type-tag
 series?
 square-abstract-matrix?
 square-matrix?
 structure?
 type-tag
 type-tags
 typed-function?
 typed-or-abstract-function?
 unit-type-tag
 units?
 up-quantity?
 up-type-tag
 up?
 vector-quantity?
 vector-type-tag
 with-units-type-tag
 with-units?]

@section[#:tag "kernel/modarith"]{Modarith}
@deftempproc*[
 mod:*
 mod:+
 mod:-
 mod:/
 mod:=
 mod:binary-combine
 mod:chinese-remainder
 mod:expt
 mod:invert
 mod:make
 mod:make-internal
 mod:modulus
 mod:reduce
 mod:residue
 mod:unary-combine
 modarith:assign-operations
 modint:*
 modint:+
 modint:-
 modint:/
 modint:chinese-remainder
 modint:expt
 modint:invert
 modint?
 modular-type-tag]

@section[#:tag "kernel/diff"]{Diff}
@deftempproc*[
 *active-tags*
 <differential-tags?
 <dt
 <dts
 =dt
 d:*
 d:+
 diff:*
 diff:+
 diff:-
 diff:/
 diff:abs
 diff:acos
 diff:apply
 diff:arity
 diff:asin
 diff:assign-operations
 diff:atan1
 diff:atan2
 diff:binary-comparator
 diff:binary-op
 diff:cos
 diff:cosh
 diff:derivative
 diff:exp
 diff:expt
 diff:identity
 diff:invert
 diff:log
 diff:negate
 diff:one-like
 diff:one?
 diff:power
 diff:sin
 diff:sinh
 diff:sqrt
 diff:square
 diff:type
 diff:type-predicate
 diff:unary-op
 diff:zero-like
 diff:zero?
 differential->terms
 differential-coefficient
 differential-object
 differential-of
 differential-tag-count
 differential-tags
 differential-term-list
 dtl:*
 dtl:+
 extract-dx-differential
 extract-dx-function
 extract-dx-operator
 extract-dx-part
 finite-part
 infinitesimal-part
 insert-differential-tag
 intersect-differential-tags
 make-differential-quantity
 make-differential-tag
 make-differential-term
 make-x+dx
 max-order-tag
 remove-differential-tag
 replace-differential-tag
 replace-dx-function
 replace-dx-operator
 same-differential-tags?
 simple-derivative-internal
 tag-active?
 tdtl:*
 terms->differential
 terms->differential-collapse
 union-differential-tags
 with-active-tag
 with-tag
 without-tag]

@section[#:tag "kernel/deriv"]{Deriv}
@deftempproc*[
 deriv:assign-operations
 deriv:euclidean-structure
 deriv:multivariate-derivative]

@section[#:tag "kernel/operator"]{Operator}
@deftempproc*[
 expn
 make-op
 make-operator
 o:*
 o:+
 o:-
 o:arity
 o:cos
 o:exp
 o:expt
 o:f*o
 o:f+o
 o:f-o
 o:identity
 o:negate
 o:o*f
 o:o+f
 o:o-f
 o:o/n
 o:one-like
 o:sin
 o:type
 o:type-predicate
 o:zero-like
 operator-arity
 operator-merge-arities
 operator-merge-optionals
 operator-merge-subtypes
 operator-name
 operator-optionals
 operator-procedure
 operator-subtype
 operator:assign-operations
 set-operator-optionals!
 simple-operator?]

@section[#:tag "kernel/function"]{Function}
@deftempproc*[
 coerce-to-function
 f:arity
 f:binary
 f:identity-like
 f:one-like
 f:transpose
 f:type
 f:type-predicate
 f:unary
 f:zero-like
 function:assign-operations
 p-rename]

@section[#:tag "kernel/numbers"]{Numbers}
@deftempproc*[
 *known-reals*
 *numbers-are-constant-functions*
 abn:=
 abn:one?
 abn:zero?
 an:=
 an:one-like
 an:one?
 an:zero-like
 an:zero?
 declare-known-reals
 declare-unknown-reals
 known-real?
 literal-number
 make-numerical-combination
 n:*
 n:+
 n:-
 n:/
 n:<
 n:<=
 n:=
 n:>
 n:>=
 n:abs
 n:acos
 n:angle
 n:arity
 n:asin
 n:atan
 n:conjugate
 n:cos
 n:cosh
 n:csc
 n:csch
 n:cube
 n:deriv
 n:exp
 n:exp10
 n:exp2
 n:expt
 n:gcd
 n:imag-part
 n:inexact?
 n:invert
 n:log
 n:log10
 n:log2
 n:magnitude
 n:make-polar
 n:make-rectangular
 n:negate
 n:one-like
 n:one?
 n:real-part
 n:revdivide
 n:sec
 n:sech
 n:self
 n:sigma
 n:sin
 n:sinh
 n:sqrt
 n:square
 n:tan
 n:tanh
 n:type
 n:type-predicate
 n:zero-like
 n:zero?
 numbers:assign-operations
 with-known-reals]

@section[#:tag "kernel/vectors"]{Vectors}
@deftempproc*[
 abstract-vector
 av:arity
 av:zero-like
 complex-norm
 euclidean-norm
 general-inner-product
 literal-vector
 make-vector-combination
 maxnorm
 scalar*vector
 v:apply
 v:arity
 v:basis-unit?
 v:conjugate
 v:cross-product
 v:cube
 v:dimension
 v:dot-product
 v:elementwise
 v:generate
 v:inexact?
 v:inner-product
 v:make-basis-unit
 v:make-unit
 v:make-zero
 v:negate
 v:partial-derivative
 v:scale
 v:square
 v:type
 v:type-predicate
 v:unit?
 v:zero-like
 v:zero?
 vector*scalar
 vector+vector
 vector-vector
 vector/scalar
 vector:elementwise
 vector:generate
 vector=vector
 vectors:assign-operations]

@section[#:tag "kernel/structs"]{Structs}
@deftempproc*[
 *allowing-incompatible-multiplication*
 *careful-conversion*
 2-down?
 2-tensor?
 2-up?
 A^m_n->Mmn
 A^mn->Mmn
 A_m^n->Mnm
 A_mn->Mnm
 Mmn->A^m_n
 Mmn->A^mn
 Mnm->A_m^n
 Mnm->A_mn
 abstract-down
 abstract-up
 ac:zero-like
 ar:zero-like
 as-matrix
 as:arity
 compatible-shape
 compatible-zero
 down
 down->vector
 down-of-ups?
 dual-zero
 flip-indices
 flip-outer-index
 list->up-structure
 literal-down
 literal-up
 m->s
 make-down-combination
 make-up-combination
 matrix->structure
 rexists
 s->m
 s:->vector
 s:abs
 s:apply
 s:arity
 s:binary
 s:compatible-elements?
 s:compatible-for-contraction?
 s:conjugate
 s:contract
 s:determinant
 s:dimension
 s:divide-by-structure
 s:dot-product
 s:elementwise
 s:forall
 s:foreach
 s:fringe
 s:generate
 s:inexact?
 s:inverse
 s:inverse1
 s:invert
 s:length
 s:magnitude
 s:map
 s:map-chain
 s:map/l
 s:map/r
 s:map/r/l
 s:multiply
 s:negate
 s:opposite
 s:outer-product
 s:partial-derivative
 s:ref
 s:same
 s:select
 s:solve-linear-left
 s:solve-linear-right
 s:square
 s:structure
 s:subst
 s:subst-internal
 s:trace
 s:transpose
 s:transpose-outer
 s:transpose1
 s:type
 s:unary
 s:with-substituted-coord
 s:zero-like
 s:zero?
 sc:type-predicate
 scalar*structure
 scalar/tensor
 single-layer-down?
 single-layer-up?
 sr:type-predicate
 structs:assign-operations
 structure*scalar
 structure+structure
 structure->access-chains
 structure->matrix
 structure->prototype
 structure-structure
 structure/scalar
 structure:elementwise
 structure:expt
 structure=structure
 submatrix
 typical-object
 ultra-flatten
 ultra-unflatten
 up
 up->vector
 up-of-downs?
 up-structure->list
 vector->down
 vector->up]

@section[#:tag "kernel/matrices"]{Matrices}
@deftempproc*[
 Cramers-rule
 abstract-matrix
 am:arity
 am:id-like
 am:one-like
 am:zero-like
 array->matrix
 classical-adjoint-formula
 column-matrix
 column-matrix->up
 column-matrix->vector
 column-matrix?
 determinant-general
 determinant-numerical
 diagonal?
 down*matrix
 down->row-matrix
 easy-zero?
 general-determinant
 literal-column-matrix
 literal-matrix
 literal-row-matrix
 m:apply
 m:arity
 m:conjugate
 m:cos
 m:cross-product-column
 m:cross-product-row
 m:determinant
 m:diagonal
 m:dimension
 m:dot-product-column
 m:dot-product-row
 m:elementwise
 m:exp
 m:expt
 m:generate
 m:identity-like
 m:identity?
 m:inexact?
 m:inner-product
 m:invert
 m:make-diagonal
 m:make-identity
 m:make-zero
 m:minor
 m:negate
 m:nth-col
 m:nth-row
 m:num-cols
 m:num-rows
 m:one-like
 m:outer-product
 m:partial-derivative
 m:ref
 m:rsolve
 m:scale
 m:sin
 m:solve
 m:solve-linear
 m:square
 m:submatrix
 m:trace
 m:transpose
 m:type
 m:type-predicate
 m:zero-like
 m:zero?
 make-matrix-combination
 matinv-general
 matinv-numerical
 matrices:assign-operations
 matrix*matrix
 matrix*scalar
 matrix*up
 matrix*vector
 matrix+matrix
 matrix+scalar
 matrix->array
 matrix-binary-componentwise
 matrix-by-col-list
 matrix-by-cols
 matrix-by-row-list
 matrix-by-rows
 matrix-matrix
 matrix-ref
 matrix-scalar
 matrix-size
 matrix-with-substituted-row
 matrix/matrix
 matrix/scalar
 matrix:elementwise
 matrix:generate
 matrix=matrix
 matrix=scalar
 numerical?
 row-matrix
 row-matrix->down
 row-matrix->vector
 row-matrix?
 scalar*matrix
 scalar+matrix
 scalar-matrix
 scalar/matrix
 scalar=matrix
 set-numerical!
 set-symbolic!
 solve-general
 solve-numerical
 tag-matrix
 up->column-matrix
 vector*matrix
 vector->column-matrix
 vector->row-matrix]

@section[#:tag "kernel/quaternion"]{Quaternion}
@deftempproc*[
 4x4->quaternion
 angle-axis->quaternion
 make-quaternion
 q:*
 q:+
 q:-
 q:->4x4
 q:->angle-axis
 q:->rotation-matrix
 q:->vector
 q:/
 q:1
 q:3vector
 q:4x4->
 q:=
 q:angle-axis->
 q:apply
 q:arity
 q:conjugate
 q:exp
 q:i
 q:inexact?
 q:invert
 q:j
 q:k
 q:log
 q:magnitude
 q:make
 q:make-unit
 q:negate
 q:partial-derivative
 q:real&3vector->
 q:real-part
 q:ref
 q:rotate
 q:rotation-matrix->
 q:type
 q:type-predicate
 q:unit?
 q:zero-like
 q:zero?
 quaternion
 quaternion*quaternion
 quaternion*scalar
 quaternion+quaternion
 quaternion->3vector
 quaternion->4x4
 quaternion->angle-axis
 quaternion->real-part
 quaternion->rotation-matrix
 quaternion->vector
 quaternion-quaternion
 quaternion-ref
 quaternion/quaternion
 quaternion/scalar
 quaternion:assign-operations
 real&3vector->quaternion
 rotation-matrix->quaternion
 s:1
 s:i
 s:j
 s:k
 scalar*quaternion]

@section[#:tag "kernel/pseries"]{Pseries}
@deftempproc*[
 *integrate-series
 add-series
 atan-series
 binomial-series
 coefficient*series
 coefficient+series
 coefficient-series
 coefficient/series
 constant-series
 cos-series
 cosh-series
 div$series
 exp-series
 integral-series-tail
 integrate-helper
 invert-series
 make-series
 mul$series
 mul-series
 negate-stream
 partial-sums
 partial-sums-stream
 power-series
 pseries:assign-operations
 series
 series*coefficient
 series+coefficient
 series->stream
 series-coefficient
 series-wrapper
 series/coefficient
 series:->function
 series:add
 series:arity
 series:derivative
 series:div
 series:elementwise
 series:expt
 series:for-each
 series:function->
 series:generate
 series:identity
 series:inflate
 series:invert
 series:mul
 series:negate
 series:one
 series:one-like
 series:print
 series:promote-arity
 series:ref
 series:same-arity
 series:sub
 series:sum
 series:type
 series:type-predicate
 series:value
 series:zero
 series:zero-like
 sin-series
 sinh-series
 stream:c*s
 stream:s/c
 sub-series
 tan-series]

@section[#:tag "kernel/numsymb"]{Numsymb}
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

@section[#:tag "kernel/heuristic"]{Heuristic}
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

@section[#:tag "kernel/litfun"]{Litfun}
@deftempproc*[
 *literal-reconstruction*
 ->
 Any
 DOWN
 DOWN*
 Hamiltonian
 Lagrangian
 Real
 UP
 UP*
 X
 X*
 ^
 all-satisfied
 default-function-type
 f:domain-types
 f:range-type
 length->exact-arity
 litderiv
 literal-apply
 literal-function
 literal-function?
 literal-partial
 litfun
 permissive-function-type
 starify
 type->arity
 type->domain
 type->domain-types
 type->range-type
 type-expression->predicate
 type-expression->type-tag
 typed-function]
|#|