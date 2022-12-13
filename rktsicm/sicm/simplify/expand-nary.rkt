#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" cons*)
         (only-in "../general/list-utils.rkt" reduce)
         "syntax.rkt"
         "rule-syntax.rkt")

;;bdk;; start original file

#|
;;; The logical ordering
(define expand-nary
  (rule-system
   ( (+ (? a) (? b) (? c) (?? d))
     none
     (+ (+ (: a) (: b)) (: c) (:: d)) )
   ( (* (? a) (? b) (? c) (?? d))
     none
     (* (* (: a) (: b)) (: c) (:: d)) )
   ( (- (? a) (? b) (? c) (?? d))
     none
     (- (- (: a) (: b)) (: c) (:: d)) )
   ( (/ (? a) (? b) (? c) (?? d))
     none
     (/ (/ (: a) (: b)) (: c) (:: d)))
   ))
|#

;;; MIT/GNU Scheme ordering
(define expand-nary
  (rule-system
   ( (+ (? a) (? b) (? c) (?? d))
     (reduce (lambda (x y) `(+ ,x ,y))
             0
             (cons* a b c d)) )
  ( (* (? a) (? b) (? c) (?? d))
    (reduce (lambda (x y) `(* ,x ,y))
            0
            (cons* a b c d)) )
   ( (- (? a) (? b) (? c) (?? d))
     none
     (- (: a) (+ (: b) (: c) (:: d))) )
   ( (/ (? a) (? b) (? c) (?? d))
     none
     (/ (: a) (* (: b) (: c) (:: d))) )
   ))

#|

(pp 
 (expand-nary
  '(+ a b c d e f)))
(+ f (+ e (+ d (+ c (+ b a)))))



(pp 
 (expand-nary
  '(let* ((G44125 (expt dt 3)) (G44128 (* (expt m 3) (expt r_0 4))))
     (up
      (+ (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
         (/ (* -1/2 G44125 (expt p_phi_0 2) p_r_0) G44128))
      (+ (/ (* G44125 p_phi_0 (expt p_r_0 2)) G44128)
         (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
         (/ (* -1/3 G44125 (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6))))))))
|#