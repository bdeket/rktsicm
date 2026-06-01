#lang scribble/manual

@(require "helpers.rkt"
          scribble/examples
          (for-syntax racket/base)
          (for-label (only-in racket/base number? symbol? real? boolean?)
                     (except-in racket/math radians->degrees degrees->radians)
                     racket/contract
                     sicm/kernel/constants
                     sicm/solve
                     )
          sicm/solve)

@title[]{Solve}
@defmodule[sicm/solve #:packages ("rktsicm")]

@racket[sicm/solve] provides utilities to solve some basic system of equations.
@examples[#:eval (make-sicm-eval)
          (solve-equations
           (list (make-equation '(-  3 (+ x y))  (list 'A))
                 (make-equation '(-  5 (- x y))  (list 'B))
                 (make-equation '(-  3 (+ (* (sqrt x) z) (square y)))  (list 'C)))
           '(x y z))]
@;*************************************************************************************************

@defproc[(solve-equations [equations (listof equation?)] [unknowns (listof symbol?)])
         (or/c (list (or/c 'full-solutions 'underdetermined 'parameters-constrained 'tough-equations 'extra-equations)
                     solution?)
               (cons 'contradictions (listof equation?)))]{
 Tries to solve a system of equations for all possible solutions. When a solution is found the first symbol is:
 @(itemlist
   @item{@racket['full-solutions]}
   @item{@racket['underdetermined]: remaining unknowns (more unknowns than equations).}
   @item{@racket['parameters-constrained]: overdetermined solutions (more equations than unknowns) where the remaining equations can only be 0 depening on some variable/parameter}
   #;(item @{@racket['extra-equations]: ¿example?})
   #;(item @{@racket['tough-equations]: ¿example?})
   @item{@racket['contradictions]: equations that reduced to non-zero numbers.})
}
@defproc[(solve-incremental [equations (listof equation)]
                            [unknowns (listof symbol?)]
                            [succeed (-> solution? fail any/c) (λ (x y) x)]
                            [fail (-> any/c) (-> (cons (or/c 'failed 'contradictions) (listof any/c)))])
         (or/c (succeed s f) (fail))]{
Tries to solve a system of equations, the first valid solution, if found, is supplied to the succeed continuation. Otherwise the fail continuation is called.
}
@defproc[(simple-solve [equations strutl?] [unknowns (listof symbol?)])
         (or/c solution? (cons 'failed #f) (cons 'contradictions (listof equations?)))]{
 Tries to solve a system of equations. The equations are provided as a structure (@racket[up] or @racket[down] where each element is solved to be @racket[n:zero]. The first valid solution, if found, will be returned.
}
@examples[#:eval (make-sicm-eval)
          (simple-solve
           (up '(-  3 (+ x y))
               '(-  5 (- x y))
               '(-  3 (+ (* (sqrt x) z) (square y))))
           '(x y z))]

@;*************************************************************************************************
@deftogether[[@defstruct[equation ([expression expression?]
                                   [justifications (listof symbol?)]
                                   [variables (listof symbol?)])
                         #:omit-constructor]
              @defproc[(make-equation [expr expression?]
                                      [just (listof symbol?) (list (gensym 'eq))]) equation?]]]{
 Create a new equation to be used in solve. Equations are only given by an expression, the implied part is that they are equal to @racket[n:zero].
 On creation the justifications should be a list of just one symbol, to help identify the equation. If nothing is provided a new symbol wil be generated. The variables are all variables present in the expression, not just the unknowns.
 
}

@deftogether[[@defstruct[solution ([resid-eqs  (listof equation?)]
                                   [resid-vars (listof symbol?)]
                                   [substs     (listof (list = symbol? expression? (listof symbol?)))]
                                   [tough      (listof equation?)])]
              @defproc[(residual-equations [sol solution?]) (listof equation)]
              @defproc[(residual-variables [sol solution?]) (listof symbol?)]
              @defproc[(substitutions      [sol solution?]) (listof (list '= symbol? expression? (listof symbol?)))]
              @defproc[(tough-equations    [sol solution?]) (listof equation)]]]{
 The return structure of a solve. A solution will have @racket[residual-equations] if there are more equations than @racket[unknowns]. It will have @racket[residual-variables] if ther are more @racket[unknowns] than equations. It might be that there are enough equations defined but that the solver is not able to find correct answers, then the unused equations are found in @racket[tough-equations].
All valid solutions can be found in the @racket[substitutions]. For each substitution the @racket[justifications] that were used are given.
}
