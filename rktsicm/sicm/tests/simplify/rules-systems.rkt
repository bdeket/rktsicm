#lang racket/base

(require rackunit
         "../../simplify/rules.rkt"
         "../../general/notes.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "simplify/rule-simplifier"
   (test-case
    "logexp"
    (check-equal? (logexp '(exp (* 3 (log x))))
                  '(expt x 3))
    (check-equal? (logexp '(exp (log x)))
                  'x)
    (check-equal? (logexp '(log (exp x)))
                  'x)
    (check-equal? (out->string (clear-notes!)(show-notes))
                  "\n#| \n'(assuming (= (log (exp x)) x))\n'(logexp1)\n|#")
    (check-equal? (logexp '(log (exp (/ (* x y) (* 1 y)))))
                  '(/ (* x y) (* 1 y)))
    ;; TODO ; this internal simplify seems redundant, especially since it's only for notes - investigate
    (check-equal? (out->string (clear-notes!)(show-notes))
                  "\n#| \n'(assuming (= (log (exp x)) x))\n'(logexp1)\n|#")
    (check-equal? (logexp '(sqrt (exp x)))
                  '(exp (/ x 2)))
    (check-equal? (out->string (clear-notes!)(show-notes))
                  "\n#| \n'(assuming (= (sqrt (exp x)) (exp (/ x 2))))\n'(logexp2)\n|#")
    (check-equal? (logexp '(log (sqrt x)))
                  '(* 1/2 (log x))))
   
   (test-case
    "todo"
    magsimp
    miscsimp
    simsqrt
    sqrt-expand
    sqrt-contract
    specfun->logexp
    logexp->specfun
    log-contract
    log-expand
    log-extra
    canonicalize-partials
    trig->sincos
    sincos->trig
    triginv
    special-trig
    angular-parity
    expand-multiangle
    trig-sum-to-product
    trig-product-to-sum
    contract-expt-trig
    half-angle
    sin^2->cos^2
    cos^2->sin^2
    split-high-degree-cosines
    split-high-degree-sines
    flush-obvious-ones
    sincos-random
    sincos->exp1
    sincos->exp2
    exp->sincos
    exp-contract
    exp-expand
    complex-rules
    divide-numbers-through
    clean-differentials)
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))