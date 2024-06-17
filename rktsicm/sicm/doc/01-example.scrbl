#lang scribble/manual

@(require scribble/examples
          racket/sandbox
          (for-label (only-in racket lambda λ quote)
                     sicm))

@(define SICMEVAL (parameterize ([sandbox-memory-limit 50]
                                 [sandbox-eval-limits '(15 30)]
                                 [sandbox-output 'string]
                                 [sandbox-error-output 'string])
                    (make-evaluator 'sicm)))

@title[#:tag "intro"]{Introduction}

The @racket[sicm] language defines generic mathematical functions that can operate on a wide variaty of data. For example the following multiplies, adds and applies numbers, symbols, functions and tuples:
@examples[#:eval SICMEVAL
          (simplify ((up (* (+ 4 'a) 'b (λ (x) (/ 1 x)))
                         (/ (λ (t) (exp t)) (λ (q) (+ 4 'a q))))
                     't))]


Standard the output will be some internal representation of the data.
@examples[#:eval SICMEVAL
          (+ 4 'a)]
Usually it is more interesting to see the result as a list of symbols and numbers. To this end you could set up the repl to automatically print the simplified expressions:
@codeblock{
 (let ([P (current-print)]) (current-print (λ (a) (P (simplify a)))))
}
However, some simplifications can take a long time. Functions are opaque to the simplifier, so unless applied to a symbol, the output will not be any more informative.

@para{}
Representing literal mathematical objects is possible: for example @racket['f] could represent a number, a vector, a function, etc. To make it clear that it represents a function it should be defined with @racket[(literal-function 'f)]. If just the quoted symbol @racket['f] is used the default is to treat it as @racket[(literal-number 'f)].

@para{}
All @racket[sicm] generic functions can work with units. So for example:
@examples[#:eval SICMEVAL
          (with-units->expression SI (/ (* :G earth-mass) (expt earth-radius 2)))]
calculates the acceleration due to gravity at sealevel. Important constants such as @racket[:G] have been predefined.

@para{}
Solvers for linear equations, linear least square, polynomial roots and other can be found in @racket[sicm/solve]. Some slightly-nonlinear systems also can also be solved:
@examples[#:eval SICMEVAL
          (solve-incremental
           (list (make-equation '(-  3 (+ x y))  (list 'A))
                 (make-equation '(-  5 (- x y))  (list 'B))
                 (make-equation '(-  3 (+ (* (sqrt x) z) (square y)))  (list 'C)))
           '(x y z))]

@para{}
For numeric calculations different tools can be found in @racket[sicm/numerics].

@examples[#:eval SICMEVAL
          (multidimensional-minimize (λ (v)
                                       (+ (expt (- (ref v 0) 2) 2)
                                          (expt (+ (ref v 1) 1) 2)))
                                     '(4 3))]

@examples[#:eval SICMEVAL
          ((make-definite-integrator (λ (x)(/ x)) 1. 2.) 'integral)]
