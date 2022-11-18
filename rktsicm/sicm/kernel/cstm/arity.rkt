#lang racket/base

(provide (all-defined-out)
         arity-includes? normalize-arity)


(require (only-in racket/function arity-includes? normalize-arity)
         racket/fixnum)

(module+ test
  (require rackunit))

;;; Arity is important to special case.
(define (exact-arity n)
  (unless (and (exact-integer? n) (<= 0 n))
    (raise-argument-error 'exact-arity "exact-positive-integer?" n))
  n)

(define *at-least-zero* (arity-at-least 0))
(define *exactly-zero* 0)
(define *at-least-one* (arity-at-least 1))
(define *exactly-one* 1)
(define *at-least-two* (arity-at-least 2))
(define *exactly-two* 2)
(define *at-least-three* (arity-at-least 3))
(define *exactly-three* 3)
(define *one-or-two*    '(1 2))

(define (exactly-n? arity)
  (and (integer? arity)
       (fx<= 0 arity)))

(define (any-number? arity)
  (and (arity-at-least? arity)
       (fx= (arity-at-least-value arity) 0)))

(define (arity-min ar)
  (cond
    [(integer? ar) ar]
    [(arity-at-least? ar) (arity-at-least-value ar)]
    [else (apply min (map arity-min ar))]))

(define (combine-arity A B)
  (unless (procedure-arity? A) (error))
  (unless (procedure-arity? B) (error))
  (normalize-arity (append (if (list? A) A (list A))
                           (if (list? B) B (list B)))))

(define (arity-intersect A B)
  (define (swap v) (vector (vector-ref v 2) (vector-ref v 1) (vector-ref v 0)))
  (define (vcombine v v*) (vector (vector-ref v* 0)
                                  (combine-arity (vector-ref v 1) (vector-ref v* 1))
                                  (combine-arity (vector-ref v 2) (vector-ref v* 2))))
  (define (inner-n? A B)
      (cond
        [(number? B) (if (= A B) (vector '() A '()) (vector A '() B))]
        [(arity-at-least? B)
         (define b (arity-at-least-value B))
         (if (<= b A)
             (vector '() A (append (for/list ([i (in-range b A)]) i)
                                   (list (arity-at-least (+ A 1)))))
             (vector A '() B))]
        [(list? B)
         (for/fold ([v (vector A '() '())])
                   ([b (in-list B)])
           (vcombine v (inner (vector-ref v 0) b)))]
        [else (error "huhB?" B)]))
  (define (inner-+l A B)
    (for/fold ([v (vector A '() '())])
              ([b (in-list B)])
      (vcombine v (swap (inner b (vector-ref v 0))))))
  (define (inner A B)
    (cond
      [(null? A) (vector '() '() B)]
      [(number? A) (inner-n? A B)]
      [(number? B) (swap (inner-n? B A))]
      [(arity-at-least? A)
       (cond
         [(arity-at-least? B)
          (define a (arity-at-least-value A))
          (define b (arity-at-least-value B))
          (if (< a b)
              (vector (normalize-arity (for/list ([i (in-range a b)]) i)) B '())
              (vector '() B (normalize-arity (for/list ([i (in-range b a)]) i))))]
         [(list? B)
          (inner-+l A B)]
         [else (error "huhB?" B)])]
      [(arity-at-least? B) (swap (inner B A))]
      [(list? A) (inner-+l A B)]
      [else (error "huhA?" A)]))
  (inner (normalize-arity A) (normalize-arity B)))