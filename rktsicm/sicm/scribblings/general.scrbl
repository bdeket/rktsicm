#lang scribble/manual

@(require "helpers.rkt"
          scribble/examples
          racket/sandbox
          (for-label racket/base
                     racket/contract))

@title[#:style 'toc]{General}
Modules under @racket[sicm/general] hold functions that are not exported by @racket[sicm]. Mostly they are used for internal purposes, but they might be handy.

@local-table-of-contents[#:style 'immediate-only]

@;*************************************************************************************************
@section{eq-properties}
@defmodule[sicm/general/eq-properties #:packages ("rktsicm")]
@(require (for-label sicm/general/eq-properties))
This module provides methods for anotating objects. It manages a master record where objects are linked (weakly) to some properties. One of the ways this is used is in the solver to annotate that an abstract-number is expected to be real, or positive etc...
 
Objects that are @racket[eq?] will share the same properties. Property keys are tested with @racket[eq?]
  
@defproc[(eq-get [obj any/c] [key any/c]) any/c]
Retrieve the property @racket[key] from object @racket[obj].
@defproc[(eq-put! [obj any/c] [key any/c] [val any/c]) obj]
Sets property @racket[key] for object @racket[obj] to value @racket[val].
@defproc[(eq-label! [obj any/c] [key any/c] [val any/c] ... ...) obj]
Sets property @racket[key], value @racket[val] pairs for object @racket[obj].
@defproc[(eq-rem! [obj any/c] [key any/c] ...) obj]
Removes properties @racket[key] from object @racket[obj].
@defproc[(eq-plist [obj any?]) (listof (cons/c any/c any/c))]
Retrieve the properties from @racket[obj] as an association list.

@defproc[(eq-adjoin! [obj any/c] [key any/c] [val any/c]) obj]
For property @racket[key] of object @racket[obj] where the property points to a set, add value @racket[val] to this set. If property @racket[key] is not yet defined, create it.
@defproc[(eq-delete! [obj any/c] [key any/c] [val any/c]) obj]
For property @racket[key] of object @racket[obj] where the property points to a set, remove value @racket[val] from this set.

@defproc[(eq-clone! [source any/c] [target any/c]) target]
Copy all properties defined on @racket[source] to @racket[target], removing any previously defined properties of @racket[target].

@;*************************************************************************************************
@section{equals}
@defmodule[sicm/general/equals #:packages ("rktsicm")]
@(require (for-label sicm/general/equals))
This module defines some new equality tests that where introduced after MIT-Scheme 11.2 updated its definition of @racket[equal?]. While in @racket[rktsicm] no improvements where noted when changing to the below equality tests, they are implemented and used to keep the source code in line with the original as much as possible.
                                                                                                                   
@defproc[(pair:eq? [x pair?] [y pair?]) boolean?]
Check that the @racket[car] and @racket[cdr] of two pairs ar @racket[eq?].
@defproc[(vector:equal? [x vector?] [y vector?]) boolean?]
Check that two @racket[vector]s are @racket[simple:equal?]
@defproc[(simple:equal? [x any/c] [y any/c]) boolean?]
Equality check that returns @racket[#t] if both objects are one of:
@itemlist[
 @item{@racket[eq?]}
 @item{@racket[pair?] and both @racket[car] and @racket[cdr] are @racket[simple:equal?]}
 @item{@racket[vector?] and @racket[vector:equal?]}
 @item{@racket[number?] and @racket[eqv?]}
 @item{@racket[string?] and @racket[string=?]}]

@;*************************************************************************************************
@section{equation-style-unifier}
@defmodule[sicm/general/equation-style-unifier #:packages ("rktsicm")]
@(require (for-label sicm/general/equation-style-unifier))

@defproc[(unify [template x-expr] [expr x-expr]) (or/c #f (listof (list symbol? any/c)))]
This module provides match-like @racket[unify] procedure. It check if the (symbolic) expression can be matched against the template.
Template variables can match a single item: @racket[(? id)]. Or they can match multiple items @racket[(?? id)]. If the same @racket[id] is used more than once, it needs to match objects that are equal?
A third option can be given to the template variables that acts as a guard: @racket[(? id guard)].
@examples[#:eval (parameterize ([sandbox-memory-limit 50]
                                [sandbox-eval-limits '(15 30)]
                                [sandbox-output 'string]
                                [sandbox-error-output 'string])
                   (make-evaluator 'racket/base #:requires '(sicm/general/equation-style-unifier)))
          #:once
          (unify '(1) '(2))
          (unify '(1 (vector 2)) '(1 (vector 2)))
          (unify '(+ (? a) (? a)) '(+ 2 3))
          (unify '(+ (? a) (? a)) '(+ 2 2))
          (unify '(+ (? a) (* (?? b) (? a))) '(+ 2 (* 3 4 5 6 2)))
          (unify '(+ (? a) (? a)) (list '+ (vector 2) (vector 2)))
          (unify `(+ (? a ,even?) (?? b ,(λ (l) (andmap odd? l)))) '(+ 2 3 5))]

@|#|
@deftempproc*[unify:internal unify:value unify:occurs-in? unify:bind unify:lookup
              unify:content unify:element? unify:segment? unify:variable? unify:name
              unify:restricted? unify:restriction unify:type]
|#|

@;*************************************************************************************************
@section{gjs-cselim}
@defmodule[sicm/general/gjs-cselim #:packages ("rktsicm")]
@(require (for-label sicm/general/gjs-cselim))
Some utilities for finding common subexpressions
@defproc[(gjs/cselim [expression any/c] [not-worth-subdividing? predicate/c (λ (x) #f)]) any/c]
Given a (symbolic) expression, find the common subexpressions and rewrite the expression inserting @racket[let]'s at the highest possible level. Any expression that is @racket[not-worth-subdividing?] will be left as is.
@racket[gjs/cselim] recognizes @racket[lambda], @racket[λ] and plain @racket[let] as introducing new scope. Named @racket[let], @racket[let-values], definitions, or other mechanisms that introduce new bindings will probably give the wrong result.
@examples[#:eval (parameterize ([sandbox-memory-limit 50]
                                [sandbox-eval-limits '(15 30)]
                                [sandbox-output 'string]
                                [sandbox-error-output 'string])
                   (make-evaluator 'racket/base #:requires '(sicm/general/gjs-cselim)))
          #:once
          (gjs/cselim '(lambda (x)
                         (/ (+ (* x 3) (- y z) (- x y) (* x 3))
                            (- y z))))
          (gjs/cselim '(list (+ 4 x)
                             (let* ([x 4]
                                    [y (+ 4 x)])
                               (+ (* x y) (/ (* x y) (+ 4 x))))))
          (gjs/cselim '(list (+ 4 x)
                             (let ([x 4]
                                   [y (+ 4 x)])
                               (+ (* x y) (/ (* x y) (+ 4 x))))))]

@defproc[(occurs-in? [variables any/c] [expression any/c]) (or/c #f #t list?)]
Check if any of the @racket[variables] occur in the @racket[expression], but also works if @racket[variables] or @racket[expression] is an object instead of a list.

@;*************************************************************************************************
@section{hashcons}
@defmodule[sicm/general/hashcons #:packages ("rktsicm")]
@(require (for-label sicm/general/hashcons))
@defproc[(canonical-copy [expression any/c]) any/c]
Given an expression, @racket[canonical-copy] will create a copy that will always be @racket[eq?] to any other @racket[canonical-copy] of an expression that has the same structure (created from @racket[cons]es and @racket[eqv?] objects).
@examples[#:eval (parameterize ([sandbox-memory-limit 50]
                                [sandbox-eval-limits '(15 30)]
                                [sandbox-output 'string]
                                [sandbox-error-output 'string])
                   (make-evaluator 'racket/base #:requires '(sicm/general/hashcons)))
          #:once
          (eq? (canonical-copy (list 'define 'x 4))
               (canonical-copy (list 'define 'x 4)))]

@;*************************************************************************************************
@section{list-utils}
This module contains functions that operate on list structures.

@defmodule[sicm/general/list-utils #:packages ("rktsicm")]
@(require (for-label sicm/general/list-utils))
@subsection{list sections}
@defproc[(last-pair [lst pair?]) pair?]
Return the last @racket[cons] for which the @racket[cdr] is not a @racket[pair?].
@defproc[(except-last-pair [lst pair?]) list?]
Return a new @racket[list] consisting of every element of @racket[lst] except the last @racket[cons].
@defproc[(last [lst list?]) any/c]
Retern the last element of @racket[lst].
@defproc[(butlast [lst list?]) list?]
Return a new @racket[list] consisting of every element of @racket[lst] except the last element.
@defproc[(split-list [lst list?] [pred? predicate/c] [receiver (-> any/c any/c any/c)]) any/c]
Equivalent to
@racketblock[(let-values ([(yes no) (partition pred? lst)]) (receiver yes no))]
@defproc[(sublist [lst list?] [start integer?] [end integer?]) list?]
Create a @racket[list] starting from element at index @racket[start] till @racket[end] (non inclusive).
@defproc[(delete-nth [lst list?] [n integer?]) list?]
Create a new @racket[list] without the nth element.
@defproc[(delq-once [lst list?] [element any/c]) list?]
Create a new @racket[list] with the first item for which item is @racket[eq?] to @racket[element] removed.

@subsection{substitution}
@defproc[(subst [new any/c] [old any/c] [expression any/c]) any/c]
In a nested list structure, replace any element that is @racket[eq?] to @racket[old] with @racket[new].
@defproc[(substitute-multiple [expression any/c] [substitutions (listof (list/c any/c any/c))]) any/c]
In a nested list structre, replace any element that is @racket[simple:equal?] to a @racket[car] in the substitution dictionary with its @racket[cadr].
Care is taken to keep subexpression that did not change @racket[eq?] to the original.

@subsection{Count and search}
@defproc[(count-elements [pred? predicate/c] [lst list?]) integer?]
Count the elements in the list for which @racket[pred?] returns @racket[#t].
@defproc[(countsymbols [expression any/c]) integer?]
Count all the elements in the expression and subexpressions for which @racket[symbol?] returns @racket[#t].
@defproc[(find-first [pred? predicate/c] [lst list?]) any/c]
Returns the first element for which @racket[pred?] returns @racket[#t].
@defproc[(find-infimum [lst list?] [cmpr (-> any/c any/c any/c)]) any/c]
Returns the first element @racket[e] for which @racket[(cmpr e l)] returns @racket[#t], where @racket[l] are all other elements after @racket[e] in @racket[lst].
@defproc[(list-index-of [elmnt any/c] [lst list?]) (or/c #f integer?)]
Returns the index of the first element that is @racket[eqv?] to @racket[elmnt].

@subsection{map and fold}
@defproc[(safe-map [fct (-> any/c any/c)] [lst pair?]) pair?]
@racket[map] that works on a single, possible improper lists.
@deftogether[[@defproc[(reduce [combine (-> any/c any/c any/c)] [def any/c] [lst list?]) any/c]
              @defproc[(reduce-left [combine (-> any/c any/c any/c)] [def any/c] [lst list?]) any/c]]]
Similar as @racket[foldl] but if @racket[lst] is @racket[null?], @racket[def] is returned. If @racket[lst] is a singleton, the first/only element is returned. If the @racket[lst] is longer than 1, the first two elements to which fct is applied are the first two elements of the list.
@defproc[(reduce-right [combine (-> any/c any/c any/c)] [def any/c] [lst list?]) any/c]
Similar as @racket[foldr] but if @racket[lst] is @racket[null?], @racket[def] is returned. If @racket[lst] is a singleton, the first/only element is returned. If the @racket[lst] is longer than 1, the first two elements to which fct is applied are the last two elements of the list.
@examples[#:eval (parameterize ([sandbox-memory-limit 50]
                                [sandbox-eval-limits '(15 30)]
                                [sandbox-output 'string]
                                [sandbox-error-output 'string])
                   (make-evaluator 'racket/base #:requires '(sicm/general/list-utils)))
          #:once
          (reduce (λ (a b) `(fct ,a ,b)) 'nil '(1))
          (reduce-left  (λ (a b) `(fct ,a ,b)) 'nil '(1 2 3))
          (reduce-right (λ (a b) `(fct ,a ,b)) 'nil '(1 2 3))]
@defproc[((list:elementwise [fct procedure?]) [lists list?] ...) list?]
Equal to
@codeblock{(apply map proc lists)}
@defproc[(map&reduce [fct procedure?] [combine (-> any/c any/c any/c)] [init any/c] [lsts list?] ...) an]
Equivalent to:
@codeblock{(foldl combine init (apply map fct lsts))}
without constructing the intermediate list. Note works like @racket[foldl] and not like the @racket[reduce-left] as defined above.

@subsection{combinations of 2 elements}
@defproc[(distinct-pairs [lst list?]) (listof (cons/c any/c any/c))]
Creates a list containing all distinct pairs of 2 elements in the list.
@deftogether[[@defproc[(map-distinct-pairs [fct (-> any/c any/c any/c)] [lst list?]) list?]
              @defproc[(for-each-distinct-pair[fct (-> any/c any/c any/c)] [lst list?]) void?]]]
Equivalent to using @racket[map] or @racket[for-each] as in
@codeblock{(map (λ (p) (f (car p) (cdr p))) (distinct-pairs lst))}
without constructing the intermediate list.

@subsection{set operations on list}
Operations on lists that are treated as sets. Due to the implementation, if set1 has duplicates, the result still might have duplicates.
@defproc[(lset= [is-equal? (-> any/c any/c any/c)] [set1 list?] [set2 list?]) boolean?]
Checks if two sets are the same, duplicates are ignored.
@defproc[(lset-adjoin [is-equal? (-> any/c any/c any/c)] [e any/c] [set1 list?]) list?]
Add an element to the set, if not yet present.
@defproc[(lset-union [is-equal? (-> any/c any/c any/c)] [set1 list?] [set2 list?]) list?]
Add all unique items from set2, to set1.
@defproc[(lset-difference [is-equal? (-> any/c any/c any/c)] [set1 list?] [set2 list?]) list?]
Remove all items from set2 from set1.
@defproc[(lset-intersection [is-equal? (-> any/c any/c any/c)] [set1 list?] [set2 list?]) list?]
Keep only the items that are both in set1 and set2.

@subsection{other}
@defproc[((fringe-smaller-than? [n integer?]) [expression any/c]) (or/c #f integer?)]
Equivalent to:
@codeblock{
 (let ([len (length (flatten expr))])
   (if (< len n) len #f))}
But breaking early if the result is @racket[#f]
@defproc[(list-transpose [lsts (listof list?)]) (listof list?)]
For lsts a list of n lists with (same) length m, return a new list T of m lists with length n so that
@racket[(eq? (list-ref (list-ref lsts i) j)
             (list-ref (list-ref T j) i))].
@defproc[(cons-if-necessary [a any/c] [b any/c] [c (cons/c any/c any/c)]) (cons/c any/c any/c)]
Returns a @racket[cons] of a and b. If however @racket[(and (eq? a (car c)) (eq? b (cdr c)))] it returns c.
@defproc[(variable<? [a symbol?] [b symbol?]) boolean?]
Equal to @racket[symbol<?]

@;*************************************************************************************************
@section{logic-utils}
@defmodule[sicm/general/logic-utils #:packages ("rktsicm")]
@(require (for-label sicm/general/logic-utils
                     (only-in sicm/kernel/express operator operands)
                     (only-in sicm/rkt/environment scmutils-base-environment)))

@defform[(assert test [msg id args ...])
         #:contracts ([test any?]
                      [msg string?])]
Helper for raising test failures. Shorthand for
@codeblock{(unless test (error "some message ..."))}

@deftogether[[@defproc[(true?  [val any/c]) boolean?]
              @defproc[(false? [val any/c]) boolean?]]]
Check if @racket[val] is @racket[#t] or @racket[#f] respectively.

@deftogether[[@defproc[(*or [lst list?]) boolean?]
              @defproc[(&or [val any/c] ...) boolean?]]]
Check if at least one item of @racket[lst] or of @racket[val] is @racket[#t]

@deftogether[[@defproc[(*and [lst list?]) boolean?]
              @defproc[(&and [val any/c] ...) boolean?]]]
Check if all items of @racket[lst] or of @racket[val] are @racket[#t]

@defproc[(conjunction [pred1? predicate/c] [pred2? predicate/c]) predicate/c]
Creates a new predicate equal to:
@codeblock{(λ (x) (and (pred1? x) (pred2? x)))}

@defproc[(disjunction [pred1? predicate/c] [pred2? predicate/c]) predicate/c]
Creates a new predicate equal to:
@codeblock{(λ (x) (or (pred1? x) (pred2? x)))}

@defproc[(implication [pred1? predicate/c] [pred2? predicate/c]) predicate/c]
Creates a new predicate equal to:
@codeblock{(λ (x) (or (not (pred1? x)) (pred2? x)))}

@defproc[(negation [pred? predicate/c]) predicate/c]
Creates a new predicate equal to:
@codeblock{(λ (x) (not (pred? x)))}

@defproc[(assume! [assumption any/c] [responsible any/c] [if-false (-> any/c) take-note!])
         (or/c 'OK 'noted any/c)]
If @racket[assumption] is an expression, and all the @racket[operands] are @racket[number?]s, try to evaluate it using the @racket[procedure?] bound to the @racket[operator] in @racket[scmutils-base-environment]. If the @racket[assumption] is @racket[#t], return @racket['OK]. If it is false, evaluate @racket[if-false]. The default is to add a note about the @racket['false!] assumption, returning @racket['noted].
In all other cases that the assumption was not tested, a note is added to @racket[*notes*], with extra info about the @racket[responsible] rule as an @racket[eq-property] and the result of the call is @racket['noted].

@;*************************************************************************************************
@section{memoize}
@(require (for-label sicm/general/memoize))
@defmodule[sicm/general/memoize #:packages ("rktsicm")]

Memoize has functions for momoization. It also exports some control functions to get statistics on the memoized functions.

The memoized functions generated have the same @racket[arity] as the original. If the original function was an @racket[apply-hook], the memoized function will also be so, with the same @racket[apply-hook-extra]'s. And any eq-properties that the original function had will also be copied to the new memoized function.
None of the memoizers here defined work out of the box for functions that take keyword arguments.

@defproc[(samritchie-memoizer [func procedure?]) procedure?]
Multiple argument memoizer that guarantees that memorizing two functions that are @racket[eq?] result in the same memoized function.

@defproc[(linear-memoize [func procedure?] [table-size integer? 12] [finder (-> any/c any/c any/c) weak-find-equal-args?]) procedure?]
Multiple argument memoizer that keeps at most @racket[table-size] elements in memory. Finding previously calculated items is done with @racket[finder]. A function constructed with @racket[weak-finder].

@deftogether[[@defthing[weak-find-equal-args? (-> any/c any/c any/c)]
              @defthing[weak-find-eqv-args?  (-> any/c any/c any/c)]
              @defthing[weak-find-eq-args? (-> any/c any/c any/c)]]]
Finder functions to be used together with @racket[linear-memoize].

@defproc[(hash-memoize-1arg [func (-> any/c any/c)]) (-> any/c any/c)]
Multiple argument memoizer for functions of 1 argument.

@defproc[(clear-memoizer-tables) void]
Clear all the memoized values from the memoized functions.

@defproc[(the-memoizers) (listof memoizer)]
A list of all @racket[memoizer]'s currently known.

@defstruct[memoizer ([fun procedure?]
                     [org procedure?]
                     [type symbol?]
                     [max-table-size integer?]
                     [info (-> (list integer? integer? any/c))]
                     [reset (-> void)])
           #:omit-constructor]
Combines information about a memoized function. @racket[memoizer-fun] and @racket[memoizer-org] hold the memoized and original function. The @racket[type] is a symbol that shows wich memoizing function created the memoized function.

@racket[memoizer-info] returns a list that consist of two integers, showing how many hits and misses the memoized function has had, and a third argument that holds info about the internal memory of the memoized function.

@racket[memoizer-reset] returns a function to reset the memory of the memoized function.

@;*************************************************************************************************
@section{notes}
@(require (for-label sicm/general/notes))
@defmodule[sicm/general/notes #:packages ("rktsicm")]

@defthing[*notes* (listof any/c)]
List of notes. Notes are used in logic utils and simplifier to note down assumptions used.

@defproc[(note-that! [note any/c]) any/c]
Add a [note] to the @racket[*notes*] list. 

@defproc[(clear-notes!) void]
Reset the @racket[*notes*] list.

@defproc[(show-notes) void]
Show the notes that were in the @racket[*notes*] list before the last call to @racket[(clear-notes!)]

@;*************************************************************************************************
@section{permute}
@(require (for-label sicm/general/permute))
@defmodule[sicm/general/permute #:packages ("rktsicm")]
@deftogether[[@defproc[(permutations [lst list?]) (listof list?)]
              @defproc[(combinations [lst list?] [n integer?]) (listof list?)]]]
Generates all permutations or combinations of @racket[n] elements from a list of elements.

@defproc[(permute [permutation (listof integer?) [lst list?]]) list?]
Generate the permutation of @racket[lst] acccording to the permutation indexes @racket[permutation].

@defproc[(sort-and-permute [lst list?] [<? less-than?] [cont (-> list? list? (-> list? list?) (-> list? list?) any/c)]) any/c]
Based on a list of elements and a sorting function, generate the permutation that changes @racket[lst] into the sorted list according to @racket[<?]. Finally the @racket[cont] function is called with the original @racket[lst] sorted list, a permutation procedure that would convert the original @racket[lst] into the sorted list, and the inverse permutation procedure that would permute a sorted list into the original @racket[lst].

@defproc[(subpermute [psteps (listof (cons integer? integer?))] [lst list?]) list?]
Permute part of a @racket[lst] based on the steps of @racket[psteps], where each element of @racket[pstep] is the @racket[cons] of the new position with the old position.
@examples[#:eval (parameterize ([sandbox-memory-limit 50]
                                [sandbox-eval-limits '(15 30)]
                                [sandbox-output 'string]
                                [sandbox-error-output 'string])
                   (make-evaluator 'racket/base #:requires '(sicm/general/permute)))
          #:once
          (subpermute '((1 . 4) (4 . 2) (2 . 3) (3 . 1)) '(a b c d e f))]

@deftogether[[@defproc[(list-interchanges [p-lst list?] [o-lst list?]) integer?]
              @defproc[(permutation-interchanges [p-lst (listof real?)]) integer?]]]
Count how many swaps between neighbouring elements are necessary to go from the permuted @racket[p-lst] to the original @racket[o-lst]. For @racket[permutation-interchanges] the original list is defined as @racket[(sort p-lst <)].

@defproc[(split-permutations [o-lst list?] [p-lsts (listof list?)] [cont (-> (listof list?) (listof list?) any/c)]) any/c]
Split the list of permutated lists @racket[p-lsts] according the fact if @racket[(even? (list-interchange p-lst o-lst))] and call @racket[cont] on the even and odd lists.

@defproc[(permutation-parity [p-lst list?] [o-lst list?]) (or/c 1 0 -1)]
Returns @racket[1] if @racket[p-lst] needs an @racket[even?] number of interchanges to go to @racket[o-lst]. @racket[-1] if it needs an @racket[odd?] number of interchanges, and @racket[0] if it is not a permutation of @racket[o-lst].

@defproc[(exact-quotient [n integer?] [m integer?]) integer?]
Returns the quotient of @racket[n] and @racket[m], but only if the remainder is @racket[0]. Otherwise raising an error.

@deftogether[[@defproc[(factorial [n integer?]) integer?]
              @defproc[(number-of-permutations [n integer?]) integer?]]]
Return the factorial of @racket[n]: @racket[(* n (- n 1) (- n 2) ... 1)]

@deftogether[[@defproc[(binomial-coefficient [n integer?] [k integer?]) integer?]
              @defproc[(number-of-combinations [n integer?] [k integer?]) integer?]]]
Return the binomial of @racket[n] and @racket[k]:
@codeblock{(/ (* n (- n 1) (- n 2) ... (- n k -1))
              (* k (- k 1) (- k 2) ... 1))}


@;*************************************************************************************************
@section{resource-limit}
@(require (for-label sicm/general/resource-limit))
@defmodule[sicm/general/resource-limit #:packages ("rktsicm")]
@deftogether[[@defproc[(with-limited-time [max-time real?] [thunk (-> any/c)]) any/c]
              @defproc[(allocated-time-expired?) boolean?]]]
Helper for timekeeping. @racket[with-limited-time] calls the @racket[thunk] in which @racket[allocated-time-expired?] will return @racket[#t] when more time has expired than @racket[max-time] (in seconds).
Outside of @racket[with-limited-time], @racket[allocated-time-expired?] will always return @racket[#f].

@;*************************************************************************************************
@section{sets}
@defmodule[sicm/general/sets #:packages ("rktsicm")]
@(let ()
   (local-require (for-label sicm/general/sets))
   @deftempproc*[<numbers adjoin-set difference-sets duplications? element-set? empty-set empty-set?
                 eq-set/adjoin eq-set/difference eq-set/empty? eq-set/equal? eq-set/intersection
                 eq-set/make-empty eq-set/member? eq-set/remove eq-set/subset? eq-set/union
                 intersect-sets list->set list-adjoin list-difference list-intersection list-union
                 make-sets-package multi-set/adjoin multi-set/difference multi-set/element?
                 multi-set/empty multi-set/empty? multi-set/first multi-set/intersection
                 multi-set/remove multi-set/rest multi-set/union numbers real-numbers remove-duplicates
                 remove-set same-set? set->list singleton-set singleton-set? subset-sets? subset?
                 symbols union-sets])

@;*************************************************************************************************
@section{stack-queue}
@(require (for-label sicm/general/stack-queue))
@defmodule[sicm/general/stack-queue #:packages ("rktsicm")]
This module provides a simple stack-queue. It is used internally by the ambigous operator. As a data structure, it is not recomended.

@defproc[(make-stack&queue) stack&queue?]
Creates a new empty stack&queue.

@defproc[(stack&queue? [obj any/c]) boolean?]
Check if on object is a @racket[stack&queue?].

@defproc[(stack&queue-empty? [sq stack&queue?]) boolean?]
Check if @racket[sq] is an empty @racket[stack&queue?].

@deftogether[[@defproc[(push! [sq stack&queue?] [val any/c]) void]
              @defproc[(add-to-end! [sq stack&queue?] [val any/c]) void]]]
Adds @racket[val] to the front or end of the @racket[stack&queue?].

@defproc[(pop! [sq stack&queue?]) any/c]
Get the first element of the @racket[stack&queue?], leaving the @racket[stack&queue?] that element shorter.

@;*************************************************************************************************
@section{table}
@(require (for-label sicm/general/table))
@defmodule[sicm/general/table #:packages ("rktsicm")]

@subsection{Tables}
@defproc[(make-table [name any/c] [assoc (-> any/c (listof (pair? any/c any/c)) (or/c #f (pair? any/c any/c)))]) table]
Create an empty (multidimensional) table. A table is defined as an object-like function.

@deftogether[[@defproc[(get [table table] [keys any/c] ...) any/c]
              @defproc[(put! [table table] [val any/c] [keys any/c] ...) void]
              @defproc[(add-to-list! [obj any/c] [table table] [keys any/c] ...) any/c]
              @defproc[(adjoin-to-list! [obj any/c] [table table] [keys any/c] ...) any/c]]]
Retrieve and put objects in the table.

@subsection{AList tables}
@defproc[(lookup [key any/c] [table (listof (list any/c any/c))]) any/c]
Traverse the association-list @racket[table] returning the @racket[val] from the @racket[(list key val)] item that has @racket[eq?] @racket[key]

@subsection{PList tables}
@defproc[(default-lookup [key any/c] [default any/c] [table (listof any/c)]) any/c]
Traverse the the property-list @racket[table], which is a list where the even-numbered items are keys, and odd-numbered items are the values.
For the first item that is @racket[eq?] to the @racket[key], return the next item in the list. If no corresponding item is found, the @racket[default] argument is returned.

@defproc[(table-of [same? (-> any/c any/c any/c)] [keys (listof any/c)] [vals (listof any/c)]) (-> any/c any/c)]
From a list of @racket[keys] and a list of @racket[vals] generate a procedure that takes a @racket[key] and gives the corresponding val. The @racket[vals] list should be at least as long as the @racket[keys] list.
The first item in the @racket[keys] list that is the same according to @racket[same?] will give the corresponding item from the @racket[vals] list.

@;*************************************************************************************************
@section{weak}
@(require (for-label sicm/general/weak))
@defmodule[sicm/general/weak #:packages ("rktsicm")]

@deftogether[[@defproc[(weak-cons [car any/c] [cdr any/c]) weak-pair?]
              @defproc[(weak-pair? [obj any/c]) boolean?] 
              @defproc[(weak-car [wp weak-pair?]) any/c]
              @defproc[(weak-pair/car? [wp weak-pair?]) boolean?]
              @defproc[(weak-cdr [wp weak-pair?]) any/c]
              @defproc[(set-weak-cdr! [wp weak-pair?] [val any/c]) void]]]
Weak pairs, as in the value in the car of the weak-cons is retained weakly.
If the car was garbage-collected, the value returned will be a value that is @racket[gc-reclaimed-object?]. The @racket[weak-pair/car?] will check that the car is not yet garbage-collected.

@deftogether[[@defproc[(list->weak-list [lst list?]) weak-pair?]
              @defproc[(weak-list-length [lst weak-pair?]) integer?]
              @defproc[(weak-list-intact? [lst list?]) boolean?]]]
Turn a list into a weak-list, ie a set of @racket[weak-cons]es up to a final @racket['()]. A procedure for getting the length of the weak list.
A weak-list is intact if non of the @racket[weak-car]'s of the list are garbage-collected.

@deftempproc*[clean-expression-table clean-weak-alist clean-weak-list gc-reclaimed-object
              gc-reclaimed-object? get-weak-member purge-list
              set-weak-pair-cdr!
              weak-find weak-find-eq? weak-find-equal? weak-find-eqv? weak-finder
              set-weak-pair-car! weak-pair-car weak-pair-cdr set-car!]