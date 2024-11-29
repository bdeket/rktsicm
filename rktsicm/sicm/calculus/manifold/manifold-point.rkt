#lang s-exp "../../generic.rkt"

(provide make-manifold-point
         manifold-point?
         get-coordinates
         transfer-point
         zero-manifold-function
         one-manifold-function
         constant-manifold-function
         (rename-out
          [manifold-point-spec manifold-point-representation]
          [manifold-point-manifold point->manifold]
          [manifold-point-coordinate-representations coordinate-reps]))

(require "../../kernel-gnrc.rkt"
         "../../general/assert.rkt"
         "../dgutils.rkt"
         )

;;bdk;; insert 1
(struct manifold-point (spec manifold [coordinate-representations #:mutable]))

(define (make-manifold-point spec manifold coordinate-system coordinate-rep)
  (manifold-point spec manifold (list (list coordinate-system coordinate-rep))))

(define (transfer-point embedded embedding)
  (lambda (point)
    (assert (eq? (embedded 'manifold) (point->manifold point)))
    (assert (= ((embedded 'manifold) 'embedding-dimension)
               ((embedding 'manifold) 'embedding-dimension)))		 
    (manifold-point (manifold-point-spec point)
                    (embedding 'manifold)
                    '())))
;;bdk;; insert 1 end

(define point->manifold manifold-point-manifold)
(define coordinate-reps manifold-point-coordinate-representations)
(define set-coordinate-reps! set-manifold-point-coordinate-representations!)

;;bdk;; insert 2
(define (get-coordinates point coordinate-system thunk)
  (let ((entry (assq coordinate-system (coordinate-reps point))))
    (if entry
        (cadr entry)
        (let ((val (s:map/r simplify-numerical-expression (thunk))))
          (set-coordinate-reps! point
                                (cons (list coordinate-system val)
                                      (coordinate-reps point)))
          val))))
;;bdk;; insert 2 end

;;bdk;; insert 3
(define ((constant-manifold-function c) m)
  (assert (manifold-point? m))
  c)

(define zero-manifold-function (constant-manifold-function 0))

(define one-manifold-function (constant-manifold-function 1))
;;bdk;; insert 3 end
