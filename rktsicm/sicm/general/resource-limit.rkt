#lang racket/base

(provide allocated-time-expired?
         with-limited-time)

;;bdk;; start original file

(define *time-upper-limit* (make-parameter #f))

(define (allocated-time-expired?)
  (define t (*time-upper-limit*))
  (and t (< t (current-seconds))))
      
(define (with-limited-time allocated-time thunk)
  (parameterize ([*time-upper-limit* (+ allocated-time (current-seconds))])
    (thunk)))


