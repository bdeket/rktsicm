#lang racket/base

(require rackunit
         (submod "../../simplify/symbenv.rkt" ALL)
         "../../general/list-utils.rkt"
         "../../kernel.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/symbenv"
   (test-case
    "symbolic-operator"
    (check-equal? (symbolic-operator '+) symb:sum)
    (check-exn #px"Undefined symbolic operator"
               (λ () (symbolic-operator (gensym 'unknown)))))
   (test-case
    "find in symbolic environment"
    (define env0 (symbolic-environment-maker))
    (define env1 (symbolic-environment-maker (make-base-namespace) (make-empty-namespace)))
    (define env2 (symbolic-environment-maker (make-empty-namespace) (make-empty-namespace)))

    ;; we can get a value in env2
    (check-equal? (eval 'zero? env2) symb:zero?)
    ;; but not much more
    (check-exn #px"function application is not allowed" (λ () (eval '(zero? 1) env2)))

    ;; in env1 we have racket/base available + the symbolics
    (check-equal? (eval 'one? env1) symb:one?)
    (check-true (eval '(one? 1) env1))

    ;; in 0 we have everything from scmutils-base-environment (if "kernel.rkt" is loaded)
    (check-equal? (eval 'g:simplify env0) g:simplify)

    ;; check for presence:
    (define itms
      '(zero? one? negate invert square cube sqrt exp log sin cos tan sec csc asin acos sinh cosh
              abs expt make-rectangular make-polar real-part imag-part magnitude angle conjugate
              atan = + * - /))
    (define dfnd (namespace-mapped-symbols env2))
    (define A (lset-difference eq? dfnd (cons '*environment* itms)))
    (define B (lset-difference eq? (cons '*environment*  itms) dfnd))
    (check-equal? '() B)
    (check-equal? '() A)
    (for ([s (in-list itms)]) (check-equal? (symbolic-operator s) (eval s env2)))
    )
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))