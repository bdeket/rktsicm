#lang scribble/manual

@(require (for-label (only-in racket lambda λ quote)
                     sicm))

@title[#:tag "intro"]{Introduction}

The @racket[sicm] language defines generic mathematical functions that can operate on a wide variaty
of data. For example the following multiplies, adds and applies numbers, symbols, functions and tuples:
@codeblock{
#lang sicm
(simplify ((up (* (+ 4 'a) 'b (λ (x) (/ 1 x)))
               (/ (λ (t) (exp t)) (λ (q) (+ 4 'a q))))
           't))}


Standard the output of mathematical functions will be some internal representation of the data.
Usually it is more interesting to see the result as a list of symbols and numbers. To this end
you could set up the repl to automatically print the simplified expressions:
@codeblock{
 (let ([P (current-print)]) (current-print (λ (a) (P (simplify a)))))
}
However, some simplifications can take a long time. Functions are opaque to the simplifier, so
unless applied to a symbol, the output will not be any more informative.

@para{}
Representing literal mathematical objects is possible: for example @racket['f] could represent
a number, a vector, a function, etc. To make it clear that it represents a function it should be
defined with @racket[(literal-function 'f)]. If just the quoted symbol @racket['f]
is used the default is to treat it as @racket[(literal-number 'f)].

@para{}
All @racket[sicm] generic functions can work with units. So for example:
@codeblock{(with-units->expression SI (/ (* :G earth-mass) (expt earth-radius 2)))}
calculates the acceleration due to gravity at sealevel. Important constants such as @racket[:G] have
been predefined.

@para{}
Solvers for linear equations, linear least square, polynomial roots and other can be found in
@racket[sicm/solve]. Some nonlinear can be solved:
@codeblock{
(solve-incremental
  (list (make-equation '(-  3 (+ x y))  (list 'A))
        (make-equation '(-  5 (- x y))  (list 'B))
        (make-equation '(-  3 (+ (* (sqrt x) z) (square y)))  (list 'C)))
  '(x y z))}

@para{}
For numeric calculations different tools can be found in @racket[sicm/numerics].

Finding a local minima:
@codeblock{
 (multidimensional-minimize (λ (v)
                              (+ (expt (- (ref v 0) 2) 2)
                                 (expt (+ (ref v 1) 1) 2)))
                            '(4 3))}

Calculating a define integral
@codeblock{((make-definite-integrator (λ (x)(/ x)) 1. 2.) 'integral)}
