#lang scribble/manual

@(require "helpers.rkt"
          scribble/examples
          (for-label racket/base
                     racket/math
                     racket/stream
                     racket/contract
                     plot
                     sicm/poly))

@title[]{Polynomials}
@defmodule[sicm/poly #:packages ("rktsicm")]

@racket[sicm/poly] provides functions that work together with or specializes polynomials as defined in @racket[sicm/simplify/pcf]. Although @racket[pcf]s can have more than 1 variable, the functions below work on only 1 variable.

@;*************************************************************************************************

@; helpers - polyinterp
@defproc[((polynomial-function [p pcf?]) [x number?]) pcf?]{
 Creates a function in 1 variable from the @racket[p]. If @racket[(poly/arity p)] is at most 1 then the return value will be a @racket[number?] instead of a @racket[pcf?].
}
@defproc[(roots->poly [roots (listof number?)]) pcf?]{
 Creates an (unscaled) polynomial from its roots.
}
@;domain
@defproc[(poly-domain->canonical [p pcf?] [a real?] [b real?]) pcf?]{
 Create a new polynomial with the domain @racket[[a b]] mapped to @racket[[-1 1]]
}
@defproc[(poly-domain->general [p pcf?] [a real?] [b real?]) pcf?]{
 Create a new polynomial with the domain @racket[[-1 1]] mapped to @racket[[a b]]
}

@; Roots
@defproc[(poly->roots [p pcf?]) (listof number?)]{
 Get the roots of a @racket[pcf?] (in 1 variable).
}

@section{Special polynomials}
@;;Legendre
@defproc[(legendre-polynomial [n nonnegative-integer?]) pcf?]{
 Returns the nth Legendre polynomial. See @url{https://en.wikipedia.org/wiki/Legendre_polynomials}.
}
@;;Chebyshev
@defproc[(chebyshev-polynomial[n nonnegative-integer?]) pcf?]{
 Returns the nth Chebyshev polynomial of the first kind. See @url{https://en.wikipedia.org/wiki/Chebyshev_polynomials}.}


@section{Interpolating polynomials}
@defproc[((make-hermite-interpolator [n nonnegative-integer?]) [a (listof number? number? ...)]
                                                               [b (listof number? number? ...)]) pcf?]{
 Generate an Hermite interpolating @racket[pcf?] based on the two points @racket[a] and @racket[b], the function value in this points, and the values of the 1sth till the nth derivative of the function in this point. The polynomial has at most degree @racket[(+ (* 2 n) 1)]
}
@deftogether[(@defproc[(make-cubic-interpolant [ax number?] [ay number?] [ady number?]
                                               [bx number?] [by number?] [bdy number?]) pcf?]
              @defproc[(make-quintic-interpolant [ax number?] [ay number?] [ady number?] [addy number?]
                                                 [bx number?] [by number?] [bdy number?] [bddy number?]) pcf?])]{
 Specialized functions for @racket[(make-hermite-interpolator 1)] and @racket[(make-hermite-interpolator 2)].
}

@;; Lagrange
@;;; interp
@deftogether[(@defproc[(lagrange-interpolation-function [ys (listof number?)] [xs (listof number?)]) (-> number? number?)]
              @defproc[(Lagrange-interpolation-function [ys (listof any?)] [xs (listof any?)]) (-> any? any?)]
              @defproc[(lagrange [ys (listof any?)] [xs (listof any?)]) expression?]
              @defproc[(make-interp-poly [ys (listof any?)] [xs (listof any?)]) pcf?])]{
 Generate a Lagrange interpolation of the points @racket[(map cons xs ys)]. The polynomial goes through all points and will have a degree of at most @racket[(- (length xs) 1)].
}

@;; Chebyshev
@;;; polyinterp
@defproc[(get-poly-and-errors [fct (-> real? real?)] [low real?] [high real?] [order positive-integer?]) (List pcf? positive-integer? positive-integer?)]{
 Generate an interpolating @racket[pcf?] of function @racket[fct] on the interval @racket[[low high]] of degree @racket[(- order 1)] using Chebyshev roots. The return list includes the generated @racket[pcf?] and the maximum and minimum error observed at the bumps.}
@defproc[(generate-approx-poly [fct (-> real? real?)] [low real?] [high real?] [order positive-integer?] [eps #f (U #f real?)]) pcf?]{
 Generate an interpolating @racket[pcf?] of function @racket[fct] on the interval @racket[[low high]] using @racket[order] Chebyshev points. If @racket[eps] is supplied the degree of the resulting @racket[pcf?] will be reduced as much as possible to keep the error on the interval just below @racket[eps]}

@examples[#:eval (make-sicm/plot-eval)
          #:once
          (define fct sqrt)
          (define xs '(2 3 5 11))
          (define ys (map fct xs))
          (define (Δ clr lbl P+)
            (define-values (P eps) (if (pcf? P+)
                                       (values P+ #f)
                                       (values (car P+) (cadr P+))))
            (define F (polynomial-function P))
            (function (λ (x) (- (fct x) (F x))) #:color clr
                      #:label (string-append lbl (format " ~a°" (poly:degree P))
                                             (if eps (format " ε<~a" (/ (round (* eps 1000)) 1000)) ""))))
          (plot (list (Δ 1 "Lagrange" (make-interp-poly ys xs))
                      (Δ 2 "Chebyshev" (get-poly-and-errors fct 2 11 4))
                      (Δ 3 "Chebyshev" (get-poly-and-errors fct 2 11 6))
                      (Δ 4 "Chebyshev" (list (generate-approx-poly fct 2 11 8 0.1) 0.1))
                      (points (map (λ (x) (list x 0)) xs)))
                #:x-min 1 #:x-max 12 #:y-min -0.1 #:legend-anchor 'bottom-right)]


@section{Chebyshev expansions}
Chebyshev expansions are givven by a list of coefficients so that:
@racketblock[(for/sum ([a (in-list cheb-exp)]
                       [i (in-naturals)])
               (* a (chebyshev-polynomial i)))]
@deftogether[(@defthing[scaled-chebyshev-expansions (streamof (listof integer?))]
              @defthing[chebyshev-expansions (streamof (listof integer?))])]{
 Chebyshev expansions for the (scaled) power series @racketblock[(stream 1 x (* 2 (expt x 2)) … (* (expt 2 (+ n 1)) (expt x n)))] and for the power series @racketblock[(stream 1 x (expt x 2) … (expt x n))]
}
@deftogether[(@defproc[(poly->cheb-exp [p pcf?]) (listof real?)]
              @defproc[(cheb-exp->poly [cheb-exp (listof real?)]) pcf?])]{
 Convert a polynomial to a cheb-expansion and back. This only works for real-valued @racket[pcf?]s.
}
@defproc[(trim-cheb-exp [cheb-exp (listof real?)] [eps positive-real?]) (listof real?)]{
 Given a @racket[cheb-exp] and an error criterion @racket[eps], trim the tail of those coefficients whose absolute sum is <= @racket[eps].
}
@defproc[(cheb-econ [p pcf?] [low real?] [high real?] [eps positive-real?]) pcf?]{
 Reduce the degree of polynomial @racket[p] keeping the maximum error on the interval @racket[[low high]] smaller than @racket[eps] (if possible).
}
@defproc[(cheb-root-list [n nonnegative-integer?]) (listof real?)]{
 Get the roots of the nth @racket[chebyshev-polynomial]
}
@defproc[(first-n-cheb-values [n nonnegative-integer?] [x real?] [half? #f (U #f 'HALF)]) (listof real?)]{
 For the first n @racket[chebyshev-polynomial]'s, return @racket[(poly:value (chebyshev-polynomial i) x)].
 If the optional argument @racket[half?] is @racket['HALF] the first term is @racket[1/2] in stead of @racket[1]
}
@defproc[(cheb-exp-value [cheb-exp (listof real?)] [x real?]) real?]{
 Evaluate the cheb-expansion at @racket[x].
}
@defproc[(generate-cheb-exp [fct (-> real? real?)] [low real?] [high real?] [order nonnegative-integer?]) (listof real?)]{
 Generate a chebyshev-expansion of @racket[order] to approximate the function @racket[fct]. The expansion is mapped to interval @racket[[-1 1]] to behave as @racket[fct] on interval @racket[[low high]].
}

@section{Piecewise polynomials}
@; PPA - piecewise polynomials
@;; construction and fields
@deftogether[(@defproc[(ppa-make-from-poly [low real?] [high real?] [poly pcf?]) ppa]
              @defproc[(ppa-low-bound [ppa ppa]) real?]
              @defproc[(ppa-high-bound [ppa ppa]) real?]
              @defproc[(ppa-body [ppa ppa]) ppa-body]
              @defproc[(ppa-poly [ppa-body ppa-body]) pcf?])]{
 Basic constructor and data extractors for a piecewise polynomial (@racket[ppa]). The polynomials will always be evaluated on the interval @racket[[-1 1]] so before evaluation an @racket[x] ϵ @racket[[low high]] will be remaped to @racket[[-1 1]] for the relevant piece of the ppa.
}

@deftogether[(@defproc[(ppa-terminal? [ppa-body ppa-body]) boolean?]
              @defproc[(ppa-split? [ppa-body ppa-body]) boolean?])]{
 Check if the @racket[ppa-body] is a terminal or if it is split into more pieces.
}
@defproc[(ppa-split [ppa-body ppa-body]) real?]{
 Get the value at which to go from the low to the high side of the split @racket[ppa-body]}
@deftogether[(@defproc[(ppa-low-side [ppa-body ppa-body]) ppa-body]
              @defproc[(ppa-high-side [ppa-body ppa-body]) ppa-body])]{
 Get the body at the low or high side of the @racket[ppa-split].}

@defproc[(ppa-max-degree [ppa ppa]) nonnegative-integer?]{
 Get the highest degree of the pieces of the @racket[ppa]
}
@defproc[(ppa-size [ppa ppa]) nonnegative-integer?]{
 Get the number of pieces used in the @racket[ppa]
}

@defproc[(ppa-adjoin [ppa0 ppa] [ppa1 ppa]) ppa]{
 Connect two @racket[ppa]s. This will fail if @racket[(not (= (ppa-high-bound pp0) (ppa-low-bound pp1)))].
}

@;; evaluate
@defproc[(ppa-value [ppa0 ppa] [x real?]) real?]{
 Evaluate the @racket[ppa] in @racket[x].
}

@;; generate from function
@deftogether[(@defproc[(make-ppa [fct (-> real? real?)] [low real?] [high real?] [order positive-integer?] [esp positive-real?]) ppa]
              @defproc[(ppa-memo [fct (-> real? real?)] [low real?] [high real?] [order positive-integer?] [esp positive-real?]) (-> real? real?)])]{
 Generate a piecewise approximation for function @racket[fct] on interval @racket[[low high]]. Each piece is generated with a Chebyshev polynomial of at most degree @racket[(- order 1)], ensuring the error stays below @racket[eps], but in general this will not not guarantee continuity of the function at the splits. If continuity is required consider using @racket[make-smooth-ppa].
}

@deftogether[(@defproc[(make-smooth-ppa [fct (listof (-> real? real?))] [low real?] [high real?] [esp positive-real?]) ppa]
              @defproc[(smooth-ppa-memo [fct (listof (-> real? real?))] [low real?] [high real?] [esp positive-real?]) (-> real? real?)])]{
 Generate a piecewise approximation for function @racket[(car fcts)], each next function in @racket[fcts] should be the next derivative of the first function. Interpolation is done using Herite fitting.
}
@examples[#:eval (make-sicm/plot-eval)
          #:once
          (plot (list (function sin #:color 0)
                      (function (ppa-memo sin 0 4 3 .2) #:color 1 #:label "Chebyshev")
                      (function (smooth-ppa-memo (list sin) 0 4 .2) #:color 2 #:label "Hermite"))
                #:x-min 1 #:x-max 3 #:legend-anchor 'bottom)]