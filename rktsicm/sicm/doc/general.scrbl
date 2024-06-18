#lang scribble/manual

@(require "helpers.rkt"
          scribble/examples
          racket/sandbox
          (for-label racket/base
                     racket/contract))

@title[#:style 'toc]{General}
Modules under @racket[sicm/general] hold functions that are not exported by @racket[sicm]. Mostly they are used for internal purposes, but they might be handy.

@local-table-of-contents[]

@;*************************************************************************************************
@section{assert}
@defmodule[sicm/general/assert #:packages ("rktsicm")]
@(require (for-label sicm/general/assert))
@defform[(assert test [msg id args ...])
         #:contracts ([test any?]
                      [msg string?])]
Helper for raising test failures. Shorthand for
@codeblock{(unless test (error "some message ..."))}

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
@|#|
Only used by unifier-rule-simplifier, which is not used
@section{equation-style-unifier}
@(require (for-label sicm/general/equation-style-unifier))
@defmodule[sicm/general/equation-style-unifier #:packages ("rktsicm")]
@deftempproc*[unify unify:internal unify:value unify:occurs-in? unify:bind unify:lookup
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
@examples[#:eval (parameterize ([sandbox-memory-limit 50]
                                [sandbox-eval-limits '(15 30)]
                                [sandbox-output 'string]
                                [sandbox-error-output 'string])
                   (make-evaluator 'racket/base #:requires '(sicm/general/gjs-cselim)))
          #:once
          (gjs/cselim '(lambda (x)
                         (/ (+ (* x 3) (- y z) (- x y) (* x 3))
                            (- y z))))]
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
@(require (for-label sicm/general/list-utils))
@defmodule[sicm/general/list-utils #:packages ("rktsicm")]
@deftempproc*[%append %map %map-1 %map-2 %reverse %reverse! append-map butlast cons-if-necessary
              count-elements countsymbols delete-nth delq-once distinct-pairs drop except-last-pair
              find-first find-infimum for-each-distinct-pair fringe-smaller-than? last last-pair
              list-index-of list-transpose list:elementwise lset-adjoin lset-difference
              lset-intersection lset-union lset= make-list map&reduce map-distinct-pairs partition
              reduce reduce-left reduce-right safe-map split-at split-list sublist subst
              substitute-multiple take variable<?]

@;*************************************************************************************************
@section{logic-utils}
@(require (for-label sicm/general/logic-utils))
@defmodule[sicm/general/logic-utils #:packages ("rktsicm")]
@deftempproc*[&and &or *and *assumption-tolerance-multiplier* *or add-assumption! assume!
              conjunction disjunction false? implication negation true? with-protection assert]

@;*************************************************************************************************
@section{memoize}
@(require (for-label sicm/general/memoize))
@defmodule[sicm/general/memoize #:packages ("rktsicm")]
@deftempproc*[*auditing-memoizers* *memoizers* *not-seen* *samritchie-memoizing* add-memoizer!
              clear-memoizer-tables eq-args? equal-args? eqv-args? hash-memoize hash-memoize-1arg
              linear-memoize linear-memoize-1arg make-scmutils-memoizer memoize-multi-arg-eq
              memoizer-f memoizer-gc-daemon memoizer-info memoizer-max-table-size memoizer-memo
              memoizer-reset memoizer? n-dimensional-table same-args? samritchie-memoizer
              scmutils-memoize-multi-arg-eq simple-memoize-multi-arg-eq struct:memoizer
              weak-find-eq-args? weak-find-equal-args? weak-find-eqv-args? memoizer]

@;*************************************************************************************************
@section{notes}
@(require (for-label sicm/general/notes))
@defmodule[sicm/general/note #:packages ("rktsicm")]
@deftempproc*[*notes* note-that! clear-notes! display-note show-notes]


@;*************************************************************************************************
@section{permute}
@(require (for-label sicm/general/permute))
@defmodule[sicm/general/permute #:packages ("rktsicm")]
@deftempproc*[binomial-coefficient combinations exact-quotient factorial list-interchanges
              number-of-combinations number-of-permutations permutation-interchanges
              permutation-parity permutations permute sort-and-permute split-permutations
              subpermute]

@;*************************************************************************************************
@section{resource-limit}
@(require (for-label sicm/general/resource-limit))
@defmodule[sicm/general/resource-limit #:packages ("rktsicm")]
@deftempproc*[allocated-time-expired? with-limited-time]

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
@deftempproc*[stack&queue? stack&queue-front stack&queue-back make-stack&queue stack&queue-empty?
              stack&queued? push! add-to-end! pop!]

@;*************************************************************************************************
@section{table}
@(require (for-label sicm/general/table))
@defmodule[sicm/general/table #:packages ("rktsicm")]
@deftempproc*[adjoin-to-list! default-lookup lookup make-table put! table-of]

@;*************************************************************************************************
@section{weak}
@(require (for-label sicm/general/weak))
@defmodule[sicm/general/weak #:packages ("rktsicm")]
@deftempproc*[clean-expression-table clean-weak-alist clean-weak-list gc-reclaimed-object
              gc-reclaimed-object? get-weak-member list->weak-list purge-list set-car!
              set-weak-pair-car! set-weak-pair-cdr! struct:weak-pair weak-car weak-cdr weak-cons
              weak-find weak-find-eq? weak-find-equal? weak-find-eqv? weak-finder weak-length
              weak-list-intact? weak-pair-car weak-pair-cdr weak-pair/car? weak-pair? weak-set-cdr!
              weak-pair]