#lang racket/base

(provide (all-defined-out) pp)

(require (only-in "../rkt/glue.rkt" if true)
         (only-in "../rkt/define.rkt" define default-object?)
         (only-in "../rkt/todo.rkt" pp))

;;bdk;; insert 1
;;;for printing things out

(define (wallp-pp p? . objs)
  (if p? (for-each pp objs)))

(define (pp-it x)
  (pp x)
  x)

(define (watch-it wallp message)
  (lambda (e)
    (if wallp
	(begin (newline)
	       (display message)
           (pp e)))
    e))

(define (cpp x #:optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     port)))
    (display "#|\n" port)
    (pp x port true)			; as code 
    (display "|#\n" port)))

;;bdk;; insert 1 end