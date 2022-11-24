#lang racket/base

(provide (all-defined-out)
         stream-ref stream-tail stream-take
         stream-filter stream-append stream-for-each stream->list)

(require racket/stream
         (only-in "../rkt/glue.rkt" make-list write-line
                  int:= int:- int:+ fix:= fix:-)
         "numeric.rkt"
         "cstm/generic.rkt"
         )

(define empty-stream? stream-empty?)
(define-syntax-rule (cons-stream a b) (stream-cons a b))
(define head stream-first)
(define stream-car stream-first)
(define tail stream-rest)
(define stream-cdr stream-rest)
(define the-empty-stream empty-stream)
(define (stream-pair? x) (and (stream? x) (not (stream-empty? x))))
(define (stream-head s n) (stream->list (stream-take s n)))

;;;; STRUTL.SCM -- Stream utilities

(define stream:for-each
 (let ()
   (define (loop p s n)
     (cond ((empty-stream? s)
	    'done)
	   ((int:= n 1)
	    (p (head s))
	    '...)
	   (else
	    (p (head s))
	    (loop p (tail s) (int:- n 1)))))
   (lambda (p s . optionals)
     (loop p s
	   (if (not (null? optionals))
	       (car optionals)
	       -1)))))


(define print-stream
  (lambda (s . optionals)
     (apply stream:for-each write-line s optionals)))

(define (combiner-padded-streams f pad)
  (define (lp ss)
    (cons-stream (apply f
			(map (lambda (s)
			       (if (null? s)
				   pad
				   (head s)))
			     ss))
		 (lp (map (lambda (s)
			    (if (null? s)
				s
				(tail s)))
			  ss))))
  (lambda args (lp args)))


(define (stream-of-iterates next value)
  (cons-stream value
	       (stream-of-iterates next (next value))))


(define (infinite-stream-of x)
  (cons-stream x
	       (infinite-stream-of x)))


(define (stream-evaluate s x)
  (map-stream (lambda (f) (f x)) s))

(define (stream-apply s x)
  (map-stream (lambda (f) (apply f x)) s))


(define (map-stream f s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (f (head s))
		   (map-stream f (tail s)))))

#; ;works only on 1 extra stream
(define map-streams stream-map)
(define (map-streams f . s)
  (let lp ([s s])
    (cond
      [(andmap empty-stream? s) empty-stream]
      [(ormap empty-stream? s) (error "map-streams: unequal length streams")]
      [else
       (cons-stream (apply f (map head s))
                    (lp (map tail s)))])))


(define (merge-streams s1 s2)
  (cons-stream (stream-car s1)
	       (cons-stream (stream-car s2)
			    (merge-streams (stream-cdr s1)
					   (stream-cdr s2)))))

(define (shorten-stream n s)
  (if (or (fix:= n 0) (empty-stream? s))
      the-empty-stream
      (cons-stream (head s)
		   (shorten-stream (fix:- n 1)
				   (tail s)))))

(define (stream:+ s1 s2)
  (map-streams g:+ s1 s2))

(define (stream:- s1 s2)
  (map-streams g:- s1 s2))

(define (stream:* s1 s2)
  (map-streams g:* s1 s2))

(define (stream:/ s1 s2)
  (map-streams g:/ s1 s2))



(define zero-stream 
  (cons-stream :zero zero-stream))

(define one-stream
  (cons-stream :one one-stream))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (int:+ n 1))))


(define natural-number-stream
  (cons-stream :one
	       (stream:+ one-stream
			 natural-number-stream)))

(define factorial-stream
  (let ()
    (define (fact-helper n! n+1)
      (cons-stream n!
		   (fact-helper (g:* n+1 n!) (g:+ n+1 1))))
    (fact-helper 1 1)))

(define (stream-of-powers x unity)
  (stream-of-iterates (lambda (y) (g:* x y))
		      unity))

#; #; #; #; ;first 3 defined in racket/stream
(define stream-for-each
  (access stream-for-each (->environment '(runtime stream))))

(define stream-append
  (access stream-append (->environment '(runtime stream))))

(define stream-filter
  (access stream-filter (->environment '(runtime stream))))

(define stream-accumulate
  (access stream-accumulate (->environment '(runtime stream))))

(define (stream-accumulate str [zero :zero])
  (define (helper n str)
    (define nxt (g:+ n (head str)))
    (cons-stream nxt
                 (helper nxt (tail str))))
  (helper zero str))

;;; MIT Scheme system provides 
;;;  PRIME-NUMBERS-STREAM

;;; expands a stream with zeros interpolated between given values.

(define (stream:inflate stream n-interpolated-zeros)
  (cons-stream (head stream)
	       (stream:list-append (make-list n-interpolated-zeros
					      (g:zero-like (head stream)))
				   (stream:inflate (tail stream)
						   n-interpolated-zeros))))

(define (stream:list-append list stream)
  (if (null? list)
      stream
      (cons-stream (car list)
		   (stream:list-append (cdr list)
				       stream))))

;;bdk;; not defined anywhere...
(define prime-numbers-stream
  (let ()
    (define (helper str)
      (define n (head str))
      (cons-stream n
                   (helper (stream-filter (λ (x) (> (remainder x n) 0)) (tail str)))))
    (helper (tail natural-number-stream))))