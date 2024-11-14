#lang racket/base

(provide allocated-time-expired?
         with-limited-time)

(define (runtime) (/ (current-milliseconds) 1000.))
;;bdk;; start original file

(define *time-upper-limit* (make-parameter #f))

(define (allocated-time-expired?)
  (and (*time-upper-limit*)
       (> (runtime) (*time-upper-limit*))))
      
(define (with-limited-time allocated-time thunk)
  (parameterize ((*time-upper-limit* (+ allocated-time (runtime))))
    (thunk)))


