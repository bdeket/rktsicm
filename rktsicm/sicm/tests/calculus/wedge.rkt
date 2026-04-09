#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../general/eq-properties.rkt"
         "../helper+scm.rkt"
         )

(provide the-tests)

(define (check-nf nf rank [name 'unnamed-nform-field]
                  #:fields [fields (build-list rank (λ (_) vector-field?))])
  (check-true (form-field? nf))
  (check-true (operator? nf))
  (check-equal? (operator-subtype nf) wedge)
  (check-equal? (operator-arity nf) (exact-arity rank))
  (when name (check-equal? (expression (operator-name nf)) name))
  (when fields (check-equal? (eq-get nf 'argument-types) fields)))
(define the-tests
  (test-suite
   "calculus/wedge"
   (test-case
    "wedge/2"
    ;; one of the arguments has rank 0
    (check-simplified? (((wedge2 (λ (x) x) D) (λ (x) (expt x 3))) 't)
                       '(* 3 (expt t 3)))
    (check-simplified? (((wedge2 D (λ (x) x)) (λ (x) (expt x 3))) 't)
                       '(* 4 (expt t 3)))
    (define w1 (wedge2 D (* D D)))
    (check-nf w1 2 '(wedge derivative (* derivative derivative)))
    (check-simplified? ((w1 (λ (x) (expt x 3)) (λ (y) (* 2 y))) 't)
                       '(* -12 t))
    (check-simplified? (((wedge2 D D) (lambda (x y) (expt x 3)) (lambda (y z) (* 2 y))) 'u 'v)
                       '(down (down 0 0) (down 0 0)))
    (check-exn #px"Wrong number of args to wedge product:\n\tassertion failed: "
               (λ () ((w1 (lambda (x) (expt x 3))) 't)))
    (define w2 (wedge D (* D D) (* D D D)))
    (check-nf w2 3 '(wedge derivative (wedge (* derivative derivative) (* (* derivative derivative) derivative))))
    (check-simplified? ((w2 (λ (x) (expt x 3)) (λ (y) (* 2 y 'q)) (λ (z) (* z z))) 't)
                       '(* 24 q)))
   (test-case
    "rank"
    (check-equal? (get-rank +) 0)
    (check-equal? (get-rank D) 1)
    (check-exn #px"Unknown rank operator" (λ () (get-rank (make-operator * '* 'test *at-least-one*))))
    (check-exn #px"Bad rank " (λ () (get-rank (up 3 4))))
    (check-equal? (rank->arity 2) *exactly-two*))
   (test-case
    "procedure->nform"
    (define nf (procedure->nform-field (λ (x) 'proc) 1))
    (check-nf nf 1)
    (check-equal? (nf 'any) 'proc)
    (define nf2 (procedure->nform-field (λ (x y) (list 'proc x y)) 2))
    (check-nf nf2 2)
    (check-equal? (nf2 'a 'b) '(proc a b))
    (define nf0 (procedure->nform-field (λ () 'proc) 0))
    ;; TODO -- it seems this is never called, but is this wat is wanted?
    (check-equal? nf0 'proc))
   (test-case
    "tensor-product2"
    (check-simplified? (((tensor-product2 (λ (x) x) D) (λ (x) (expt x 3))) 't)
                       '(* 3 (expt t 3)))
    (check-simplified? (((tensor-product2 D (λ (x) x)) (λ (x) (expt x 3))) 't)
                       '(* 4 (expt t 3)))
    (define t1 (tensor-product2 D (* D D)))
    (check-nf t1 2 '(tensor-product derivative (* derivative derivative)))
    (check-simplified? ((t1 (λ (x) (expt x 3)) (λ (y) (* 2 y))) 't)
                       0)
    (check-simplified? (((tensor-product2 D D) (lambda (x y) (expt x 3)) (lambda (y z) (* 2 y))) 'u 'v)
                       '(down (down (* 6 (expt u 2)) 0) (down 0 0)))
    (check-exn #px"Wrong number of args to tensor product:\n\tassertion failed: "
               (λ () ((t1 (lambda (x) (expt x 3))) 't))))
   (test-case
    "Alt-form"
    (define a1 (Alt (λ (x) 'proc)))
    (check-equal? (a1 'any) 'proc)
    (define a2 (Alt D))
    (check-nf a2 1 '(Alt derivative))
    (check-simplified? ((a2 (λ (t) (* t t))) 't)
                       '(* 2 t))
    (check-exn #px"Wrong number of args to alternation:\n\tassertion failed: "
               (λ () (a2 (λ (t) (* t t)) (λ (t) t))))
    (check-simplified? (((Alt (tensor-product2 D D)) (λ (t) (* t t)) (λ (t) (* 3 t))) 't)
                       0))
   (test-case
    "w2"
    (check-simplified? (((w2 (λ (x) x) D) (λ (x) (expt x 3))) 't)
                       '(* 3 (expt t 3)))
    (check-simplified? (((w2 D (λ (x) x)) (λ (x) (expt x 3))) 't)
                       '(* 4 (expt t 3)))
    (define W1 (w2 D (* D D)))
    (check-nf W1 2 #f #:fields #f)
    (check-simplified? ((W1 (λ (x) (expt x 3)) (λ (y) (* 2 y))) 't)
                       '(* -12 t))
    (check-simplified? (((w2 D D) (lambda (x y) (expt x 3)) (lambda (y z) (* 2 y))) 'u 'v)
                       '(down (down 0 0) (down 0 0)))
    (check-exn #px"Wrong number of args to alternation:\n\tassertion failed: "
               (λ () ((W1 (lambda (x) (expt x 3))) 't)))
    (define W2 (w2 D (w2 (* D D) (* D D D))))
    (check-nf W2 3 #f #:fields #f)
    (check-simplified? ((W2 (λ (x) (expt x 3)) (λ (y) (* 2 y 'q)) (λ (z) (* z z))) 't)
                       '(* 24 q)))
   (test-case
    "R3-rect"
    (define-coordinates (up x y z) R3-rect)
    (define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
    (define w (literal-1form-field 'w R3-rect))
    (define u (literal-1form-field 'u R3-rect))
    (define v (literal-1form-field 'v R3-rect))
    (define X (literal-vector-field 'X R3-rect))
    (define Y (literal-vector-field 'Y R3-rect))
    (define Z (literal-vector-field 'Z R3-rect))
    (define W (literal-vector-field 'W R3-rect))
    ;;; Just checking that everything is working...
    (check-simplified? ((w X) R3-point)
                       '(+ (* (X^0 (up x0 y0 z0)) (w_0 (up x0 y0 z0)))
                           (* (X^1 (up x0 y0 z0)) (w_1 (up x0 y0 z0)))
                           (* (X^2 (up x0 y0 z0)) (w_2 (up x0 y0 z0)))))
    ;;; A few theorems
    (check-simplified? (((- (wedge (wedge w u) v) (wedge w (wedge u v))) X Y Z)
                        R3-point)
                       0)
    (check-simplified? (((- (wedge (+ w u) v) (+ (wedge w v) (wedge u v))) X Y)
                        R3-point)
                       0)
    ;;; Note, a product of forms is their wedge!
    (check-simplified? (((- (wedge u v) (* u v)) X Y)
                        R3-point)
                       0))
   (test-case
    "dx^dy"
    (define-coordinates (up x y z) R3-rect)
    (define R3-point ((R3-rect '->point) (up 'x0 'y0 'z0)))
    (define dx^dy (wedge dx dy))
    (check-simplified? ((dx^dy d/dx d/dy) R3-point) 1)
    (check-simplified? ((dx^dy d/dx d/dx) R3-point) 0)
    (check-simplified? ((dx^dy d/dy d/dx) R3-point) -1))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))