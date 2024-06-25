#lang scribble/manual

@(require (for-label (except-in racket +)
                     (only-in sicm simplify g:simplify g:+ +)))

@title[#:tag "SICM"]{SICM}

@defmodule[sicm #:packages ("rktsicm") #:lang]

This library provides a language that is almost equivalent with scmutils as found at @hyperlink["http://groups.csail.mit.edu/mac/users/gjs/6946/installation.html" "http://groups.csail.mit.edu/mac/users/gjs/6946/installation.html"].

The original manual for scmutils is located at @hyperlink["http://groups.csail.mit.edu/mac/users/gjs/6946/refman.pdf" "http://groups.csail.mit.edu/mac/users/gjs/6946/refman.pdf"].

@para{}
The following main language is provided:
@codeblock{#lang sicm}

@para{}
Additionaly some other intermediate languages are defined.

@para{}
@codeblock{#lang s-exp sicm/kernel}
The kernel functions, with applicable structures and with racket's base mathematical functions prepended with "rkt:". Note that some functions in this language will not work since not all generics have been loaded. Most importantly @racket[g:simplify].

@para{}
Lastly, a minimum working language on top of the kernel language:
@codeblock{#lang s-exp sicm/generic}
This loads @racket[simplify] and binds all mathematical symbols to their generic function. ie: @racket[+] will be bound to @racket[g:+] etc.
The mechanics and calculus modules are defined in this language.

@local-table-of-contents[#:style 'immediate-only]


@;{==================================================================================================}

@include-section{01-example.scrbl}
@;@include-section{mit.scrbl}

@include-section{general.scrbl}
@;@include-section{generics.scrbl}
@include-section{kernel.scrbl}
@;@include-section{simplify.scrbl}
@;@include-section{display.scrbl}
@;@include-section{numerics.scrbl}
@;@include-section{poly.scrbl}
@;@include-section{solve.scrbl}
@;@include-section{units.scrbl}
@;@include-section{mechanics.scrbl}
@;@include-section{calculus.scrbl}
