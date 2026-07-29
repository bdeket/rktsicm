#lang scribble/manual

@(require "helpers.rkt"
          scribble/examples
          racket/sandbox
          (for-label racket/base
                     racket/contract))

@title[#:style 'toc]{Display}
Modules under @racket[sicm/display] hold functions that are not exported by @racket[sicm]. They can be divided in two categories: Displaying expression or Simplifying expressions.
For displaying, you should maybe consider also the packages 'latex-pict' and 'Text block'.
Regarding the expression simplifier, @racket[simplify] as exported by @racket[sicm] has the necessary functionality. The functions here can be used to tie the simplifier to the @racket[current-print]er

@;*************************************************************************************************
@defmodule[sicm/display #:packages ("rktsicm")]
@(require (for-label sicm/display))

@defproc[(2d-show-expression [expr any/c]) void?]
Print a multi-line ASCII-representation of the expression of @racket[expr]. Similar to @racket[$formula] from 'Text Block'.
@examples[#:eval (parameterize ([sandbox-memory-limit 50]
                                [sandbox-eval-limits '(15 30)]
                                [sandbox-output 'string]
                                [sandbox-error-output 'string])
                   (make-evaluator 'racket/base #:requires '(sicm/display)))
          #:once
          (2d-show-expression '(/ (+ alpha (/ ((derivative f) b) (+ alpha beta)))
                                  (+ (/ (+ x y) 2)
                                     (expt (/ (+ a c (/ 2 x)) (* d e))
                                           (+ f (/ g h))))))]

@defproc[(expression->tex-string [expr any/c]) string?]
Generate a Latex-formula for the expression of @racket[expr]. The original scmutils provides also a way to display these. For similar functionality please use @racket[tex-display-math] from 'latex-pict'.

@;*************************************************************************************************
@;PP
@defproc[(pp [obj any/c] [port output-port? (current-output-port)] [as-code? boolean? #f]) void?]{
 An alias for @racket[println]. The original @racket[pp] would print the definition of a function if @racket[as-code?] is @racket[#t]. Currently, unfortunateley, this is not implemented.
}
@defproc[(pp-it [obj any/c]) any/c]{ Prints the @racket[obj]ect and returns it. }
@defproc[(cpp [obj any/c] [port output-port? (current-output-port)]) void?]{
 Prints the @racket[obj]ect to the port with some extra decoration.
}
@defproc[(wall-pp [print? boolean?] [obj any/c] ...) void?]{
 Prints each @racket[obj]ect on a new line, if @racket[print?] is @racket[#t]. Otherwise nothing is printed.
}
@defproc[((wall-it [param parameter?] [message any/c]) [obj any/c]) any/c]{
 Creates a function that will print the fixed @racket[message] and supplied @racket[obj]ect if @racket[param] does not evalutate to @racket[#f]. It will always return the @racket[obj]ect.
}

@;*************************************************************************************************
@;PRINT
@deftempproc*[*divide-out-terms* *heuristic-numbers* canonicalize-numbers ham:simplify
              divide-out-terms-simplify eqn:simplify flush-derivative
              flush-literal-function-constructors *factoring* simplify careful-simplify
              *only-printing* *last-expression-printed* system-environments prepare-for-printing
              unsimplifiable? improper-expression? show-expression print-expression pe se
              print-expression-comment pec]

@;*************************************************************************************************
@;SUPRESS-ARGS
@deftempproc*[*suppressed-argument-list* *suppressed-argument-list-counter* *rename-list*
              suppress-arguments show-suppressed-arguments clear-arguments arg-suppressor
              arg-suppressor+ rename-part rename-expression]
