#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          )

@title{General}


@section{assert}
@(require (for-label sicm/general/assert))
@defmodule[sicm/general/assert #:packages ("rktsicm")]
@deftempproc*[assert]
Helper for raising test failures.

@section{eq-properties}
@(require (for-label sicm/general/eq-properties))
@defmodule[sicm/general/eq-properties #:packages ("rktsicm")]
@deftempproc*[eq-adjoin! eq-clone! eq-delete! eq-get eq-label! eq-path eq-plist eq-properties
              eq-put! eq-rem!]
Master record for defining/adding/removing properties on objects.

@section{equals}
@(require (for-label sicm/general/equals))
@defmodule[sicm/general/equals #:packages ("rktsicm")]
@deftempproc*[pair:eq? simple:equal? vector:equal?]
Fast variants of @racket[equal?] for specific data.

@|#|
Only used by unifier-rule-simplifier, which is not used
@section{equation-style-unifier}
@(require (for-label sicm/general/equation-style-unifier))
@defmodule[sicm/general/equation-style-unifier #:packages ("rktsicm")]
@deftempproc*[unify unify:internal unify:value unify:occurs-in? unify:bind unify:lookup
              unify:content unify:element? unify:segment? unify:variable? unify:name
              unify:restricted? unify:restriction unify:type]
|#|

@section{gjs-cselim}
@(require (for-label sicm/general/gjs-cselim))
@defmodule[sicm/general/gjs-cselim #:packages ("rktsicm")]
@deftempproc*[entry-cntr entry-expr entry-name entry? expressions-seen gjs/cselim
              make-canonical-lets make-expression-recorder make-let-expression occurs-in?
              record-expression! set-entry-cntr! set-entry-expr! set-entry-name! struct:entry
              variable->expression entry]
common-subexpression eliminator

@section{hashcons}
@(require (for-label sicm/general/hashcons))
@defmodule[sicm/general/hashcons #:packages ("rktsicm")]
@deftempproc*[append-unique canonical-copy clean cons-unique hash-cons list-unique
              map-unique the-cons-table]
Unique consing

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

@section{logic-utils}
@(require (for-label sicm/general/logic-utils))
@defmodule[sicm/general/logic-utils #:packages ("rktsicm")]
@deftempproc*[&and &or *and *assumption-tolerance-multiplier* *or add-assumption! assume!
              conjunction disjunction false? implication negation true? with-protection assert]

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

@section{permute}
@(require (for-label sicm/general/permute))
@defmodule[sicm/general/permute #:packages ("rktsicm")]
@deftempproc*[binomial-coefficient combinations exact-quotient factorial list-interchanges
              number-of-combinations number-of-permutations permutation-interchanges
              permutation-parity permutations permute sort-and-permute split-permutations
              subpermute]

@section{resource-limit}
@(require (for-label sicm/general/resource-limit))
@defmodule[sicm/general/resource-limit #:packages ("rktsicm")]
@deftempproc*[allocated-time-expired? with-limited-time]

@section{sets}
@(require (for-label sicm/general/sets))
@defmodule[sicm/general/sets #:packages ("rktsicm")]
@deftempproc*[<numbers adjoin-set difference-sets duplications? element-set? empty-set empty-set?
              eq-set/adjoin eq-set/difference eq-set/empty? eq-set/equal? eq-set/intersection
              eq-set/make-empty eq-set/member? eq-set/remove eq-set/subset? eq-set/union
              intersect-sets list->set list-adjoin list-difference list-intersection list-union
              make-sets-package multi-set/adjoin multi-set/difference multi-set/element?
              multi-set/empty multi-set/empty? multi-set/first multi-set/intersection
              multi-set/remove multi-set/rest multi-set/union numbers real-numbers remove-duplicates
              remove-set same-set? set->list singleton-set singleton-set? subset-sets? subset?
              symbols union-sets]

@section{stack-queue}
@(require (for-label sicm/general/stack-queue))
@defmodule[sicm/general/stack-queue #:packages ("rktsicm")]
@deftempproc*[stack&queue? stack&queue-front stack&queue-back make-stack&queue stack&queue-empty?
              stack&queued? push! add-to-end! pop!]

@section{table}
@(require (for-label sicm/general/table))
@defmodule[sicm/general/table #:packages ("rktsicm")]
@deftempproc*[adjoin-to-list! default-lookup lookup make-table put! table-of]

@section{weak}
@(require (for-label sicm/general/weak))
@defmodule[sicm/general/weak #:packages ("rktsicm")]
@deftempproc*[clean-expression-table clean-weak-alist clean-weak-list gc-reclaimed-object
              gc-reclaimed-object? get-weak-member list->weak-list purge-list set-car!
              set-weak-pair-car! set-weak-pair-cdr! struct:weak-pair weak-car weak-cdr weak-cons
              weak-find weak-find-eq? weak-find-equal? weak-find-eqv? weak-finder weak-length
              weak-list-intact? weak-pair-car weak-pair-cdr weak-pair/car? weak-pair? weak-set-cdr!
              weak-pair]