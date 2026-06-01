#lang scribble/manual

@(require (for-syntax racket/base)
          "helpers.rkt"
          (for-label sicm/kernel/strutl))

@title[#:tag "kernel/strutl"]{Strutl}
@defmodule[sicm/kernel/strutl #:packages ("rktsicm")]

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