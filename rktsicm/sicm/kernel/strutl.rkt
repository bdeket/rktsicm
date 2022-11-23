#lang racket/base

(provide (all-defined-out)
         stream-ref stream-cons stream-first stream-rest stream-tail stream-take
         stream-filter stream-append stream-for-each stream->list
         empty-stream)

(require racket/stream
         "../rkt/fixnum.rkt"
         (only-in racket/list make-list)
         "cstm/generic.rkt"
         "numeric.rkt"
         "../rkt/int.rkt"
         (only-in "../rkt/todo.rkt" access ->environment))

;;;; STRUTL.SCM -- Stream utilities

(define (stream-pair? x) (and (stream? x) (not (stream-empty? x))))

(define (stream-head s n) (stream->list (stream-take s n)))

(define stream:for-each
 (let ()
   (define (loop p s n)
     (cond ((stream-empty? s)
	    'done)
	   ((int:= n 1)
	    (p (stream-first s))
	    '...)
	   (else
	    (p (stream-first s))
	    (loop p (stream-rest s) (int:- n 1)))))
   (lambda (p s . optionals)
     (loop p s
	   (if (not (null? optionals))
	       (car optionals)
	       -1)))))


(define print-stream
  (lambda (s . optionals)
     (apply stream:for-each writeln s optionals)))

(define (combiner-padded-streams f pad)
  (define (lp ss)
    (stream-cons (apply f
			(map (lambda (s)
			       (if (null? s)
				   pad
				   (stream-first s)))
			     ss))
		 (lp (map (lambda (s)
			    (if (null? s)
				s
				(stream-rest s)))
			  ss))))
  (lambda args (lp args)))


(define (stream-of-iterates next value)
  (stream-cons value
	       (stream-of-iterates next (next value))))


(define (infinite-stream-of x)
  (stream-cons x
	       (infinite-stream-of x)))


(define (stream-evaluate s x)
  (map-stream (lambda (f) (f x)) s))

(define (stream-apply s x)
  (map-stream (lambda (f) (apply f x)) s))


(define (map-stream f s)
  (if (stream-empty? s)
      empty-stream
      (stream-cons (f (stream-first s))
		   (map-stream f (stream-rest s)))))

(define (map-streams f . s)
  (let lp ([s s])
    (cond
      [(andmap stream-empty? s) empty-stream]
      [(ormap stream-empty? s) (error "map-streams: unequal length streams")]
      [else
       (stream-cons (apply f (map stream-first s))
                    (lp (map stream-rest s)))])))


(define (merge-streams s1 s2)
  (stream-cons (stream-first s1)
	       (stream-cons (stream-first s2)
			    (merge-streams (stream-rest s1)
					   (stream-rest s2)))))

(define (shorten-stream n s)
  (if (or (fix:= n 0) (stream-empty? s))
      empty-stream
      (stream-cons (stream-first s)
		   (shorten-stream (fix:- n 1)
				   (stream-rest s)))))


(define (stream:+ s1 s2)
  (map-streams g:+ s1 s2))

(define (stream:- s1 s2)
  (map-streams g:- s1 s2))

(define (stream:* s1 s2)
  (map-streams g:* s1 s2))

(define (stream:/ s1 s2)
  (map-streams g:/ s1 s2))



(define zero-stream 
  (stream-cons :zero zero-stream))

(define one-stream
  (stream-cons :one one-stream))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (int:+ n 1))))


(define natural-number-stream
  (stream-cons :one
	       (stream:+ one-stream
			 natural-number-stream)))

(define factorial-stream
  (let ()
    (define (fact-helper n! n+1)
      (stream-cons n!
		   (fact-helper (g:* n+1 n!) (g:+ n+1 1))))
    (fact-helper 1 1)))

(define (stream-of-powers x unity)
  (stream-of-iterates (lambda (y) (g:* x y))
		      unity))


(define (stream-accumulate str [zero :zero])
  (define (helper n str)
    (define nxt (g:+ n (stream-first str)))
    (stream-cons nxt
                 (helper nxt (stream-rest str))))
  (helper zero str))

;;; MIT Scheme system provides 
;;;  PRIME-NUMBERS-STREAM


;;; expands a stream with zeros interpolated between given values.

(define (stream:inflate stream n-interpolated-zeros)
  (stream-cons (stream-first stream)
	       (stream:list-append (make-list n-interpolated-zeros
					      (g:zero-like (stream-first stream)))
				   (stream:inflate (stream-rest stream)
						   n-interpolated-zeros))))

(define (stream:list-append list stream)
  (if (null? list)
      stream
      (stream-cons (car list)
		   (stream:list-append (cdr list)
				       stream))))

(define prime-numbers-stream
  (let ()
    (define (helper str)
      (define n (stream-first str))
      (stream-cons n
                   (helper (stream-filter (λ (x) (> (remainder x n) 0)) (stream-rest str)))))
    (helper (stream-rest natural-number-stream))))