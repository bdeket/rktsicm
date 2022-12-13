#lang racket/base

(provide (all-defined-out))

(require (only-in "../../rkt/glue.rkt" if))

;;bdk;; start original file

;;;; Cluster Analysis of a set of objects with a distance measure


(define (cluster objects cluster-separation distance)
  (if (null? objects)
      '()
      (let merge-lp ((clusters (map make-singleton-cluster objects)))
        (if (null? (cdr clusters))
            clusters
            (let ((candidates '()) (min-d 1e307)) ;+infinity
              (let scan-lp-1 ((c1 clusters))
                (if (null? (cdr c1))
                    (merge-lp (cons (merge-2-clusters candidates distance)
                                    (multiset-difference clusters candidates)))
                    (let scan-lp-2 ((c2 (cdr c1)))
                      (let ((d (cluster-separation (car c1) (car c2))))
                        (if (< d min-d)
                          (begin (set! candidates (list (car c1) (car c2)))
                                 (set! min-d d)))
                        (if (null? (cdr c2))
                            (scan-lp-1 (cdr c1))
                            (scan-lp-2 (cdr c2))))))))))))

(define (multiset-difference s1 s2)
  (if (null? s2)
      s1
      (multiset-difference (remove-one (car s2) s1)
                           (cdr s2))))

(define (remove-one x s)
  (cond ((null? s) '())
        ((eq? x (car s)) (cdr s))
        (else
         (cons (car s)
               (remove-one x (cdr s))))))


;;; A cluster has: elements, a diameter, the subclusters it was made from.

(define (merge-2-clusters clusters distance)
  (let ((c1s (cluster-elements (car clusters)))
        (c2s (cluster-elements (cadr clusters))))
    (make-a-cluster (append c1s c2s)
                    (max (apply max
                                (apply append
                                       (map (lambda (c1)
                                              (map (lambda (c2)
                                                     (distance c1 c2))
                                                   c2s))
                                            c1s)))
                         (cluster-diameter (car clusters))
                         (cluster-diameter (cadr clusters)))
                    clusters)))

(define (make-a-cluster elements diameter subclusters)
  (list elements diameter subclusters))

(define (cluster-elements cluster) (car cluster))
(define (cluster-diameter cluster) (cadr cluster))
(define (cluster-subclusters cluster) (caddr cluster))

(define (make-singleton-cluster el)
  (make-a-cluster (list el) 0 '()))


(define (set-separation element-distance)
  (lambda (cl1 cl2)
    (let ((c1s (cluster-elements cl1))
          (c2s (cluster-elements cl2)))
      (apply min
             (apply append
                    (map (lambda (c1)
                           (map (lambda (c2)
                                  (element-distance c1 c2))
                                c2s))
                         c1s))))))
