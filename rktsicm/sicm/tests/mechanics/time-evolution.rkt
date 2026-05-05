#lang s-exp "../../main.rkt"

(require rackunit
         "../helper+scm.rkt")

(rename-part 'derivative 'D)

(provide the-tests)
(define the-tests
  (test-suite
   "mechanics/time-evolution"
   (test-case
    "shift-t"
    (check-simplified? ((shift-t 'dt) (->H-state 0 'q 'p))
                       '(up dt q p)))
   (test-case
    "C->Cp"
    (define ((C dt) state) state)
    (check-simplified? (((C->Cp C) 'dt) (->H-state 't 'q 'p))
                       '(up (+ (* -1 dt) t) q p)))
   (test-case
    "H->Hp"
    (check-simplified? (((H->Hp 'dt) (literal-function 'H (Hamiltonian 1))) (->H-state 't 'q 'p))
                       '(H (up (+ (* -1 dt) t) q p))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))