#lang racket/base

(require rackunit
         racket/list
         "../../../numerics/optimize/unimin.rkt"
         "../../../kernel-intr.rkt"
         )


(provide the-tests)
(define the-tests
  (test-suite
   "numerics/optimize/unimin"
   (test-case
    "golden-section-min/max"
    (check-equal? (golden-section-min abs -20 90
                                      (λ (a minx b fa fminx fb count)
                                        (or (= a minx b)
                                            (= fa fminx fb)
                                            (<= 0 count))))
                  '(-20 20 0))
    (check-within (golden-section-min abs -20 90
                                      (λ (a minx b fa fminx fb count)
                                        (or (< (- b a) 1e-16)
                                            (= fa fminx fb)
                                            (<= 75 count))))
                  '(0 0 75) 1e-10)
    (check-within (golden-section-min abs -90 1e-300
                                      (λ (a minx b fa fminx fb count)
                                        (or (< (- b a) 1e-16)
                                            (= fa fminx fb)
                                            (<= 75 count))))
                  '(0 0 75) 1e-10)
    (check-within (golden-section-max (λ (x) (* -1 x (- x 2))) 0 2
                                      (λ (a minx b fa fminx fb count)
                                        (<= 20 count)))
                  '(1 1 20) 1e-5))
   (test-case
    "gsmin/max"
    (check-equal? (gsmin abs -20 90)
                  (golden-section-min abs -20 90
                                      (λ (a minx b fa fminx fb count)
                                        (close-enuf? (max fa fb) fminx (* 10 *machine-epsilon*)))))
    (check-equal? (gsmin abs -20 90 'function-tol .3)
                  (golden-section-min abs -20 90
                                      (λ (a minx b fa fminx fb count)
                                        (close-enuf? (max fa fb) fminx .3))))
    (check-equal? (gsmin abs -20 90 'arg-tol .3)
                  (golden-section-min abs -20 90
                                      (λ (a minx b fa fminx fb count)
                                        (close-enuf? a b .3))))
    (check-equal? (gsmin abs -20 90 'count 5)
                  (golden-section-min abs -20 90
                                      (λ (a minx b fa fminx fb count)
                                        (<= 5 count))))
    (check-equal? (gsmax abs -20 90)
                  (golden-section-max abs -20 90
                                      (λ (a minx b fa fminx fb count)
                                        (close-enuf? (max fa fb) fminx (* 10 *machine-epsilon*)))))
    (check-equal? (gsmax abs -20 90 'function-tol .3)
                  (golden-section-max abs -20 90
                                      (λ (a minx b fa fminx fb count)
                                        (close-enuf? (max fa fb) fminx .3))))
    (check-equal? (gsmax abs -20 90 'arg-tol .3)
                  (golden-section-max abs -20 90
                                      (λ (a minx b fa fminx fb count)
                                        (close-enuf? a b .3))))
    (check-equal? (gsmax abs -20 90 'count 5)
                  (golden-section-max abs -20 90
                                      (λ (a minx b fa fminx fb count)
                                        (<= 5 count)))))
   (test-case
    "brent-min"
    (check-within (take (brent-min abs -20 90 1e-4) 2)
                  '(0 0) 1e-4)
    (check-within (take (brent-max (λ (x) (* -1 x (- x 2))) -20 1.005 1e-4) 2)
                  '(1 1) 1e-4))
   (test-case
    "bracket-min"
    (check-equal? (bracket-min abs -20 100 5)
                  '(okay -120 -20 80 120 20 80 0))
    (check-equal? (bracket-max abs -20 100 5)
                  '(maxcount 1980 3280 5380 -1980 -3280 -5380 5)))
   (test-case
    "local-min"
    (check-within (map (λ (l) (take l 2))
                       (local-minima abs -20 90 10 1e-10))
                  '((0 0)) 1e-10)
    (check-within (sort (map (λ (l) (take l 2))
                             (local-maxima (λ (x) (* -1 x (- x 1) (- x 2) (- x 3))) -1 4 13 1e-10))
                        < #:key car)
                  `((,(/ (- 3 (sqrt 5)) 2) 1)
                    (,(/ (+ 3 (sqrt 5)) 2) 1)) 1e-5)
    (check-within (map (λ (l) (take l 2))
                       (local-maxima (λ (x) x) -1 1 4 1e-10))
                  '((1 1)) 1e-10)
    (check-within (map (λ (l) (take l 2))
                       (local-maxima (λ (x) (- x)) -1 1 4 1e-10))
                  '((-1 1)) 1e-10))
   (test-case
    "global-min"
    (check-within (take (estimate-global-min abs -20 90 13 1e-10) 2)
                  '(0 0) 1e-10)
    (check-within (take (estimate-global-max (λ (x) (* -1 x (- x 1) (- x 2) (- x 4))) -1 6 5 1e-10) 2)
                  '(3.326 6.914) 1e-3)
    (check-within (take (estimate-global-min (λ (x) (* 1 x (- x 1) (- x 2) (- x 4))) -1 6 6 1e-10) 2)
                  '(3.326 -6.914) 1e-3)
    (check-within (take (estimate-global-max (λ (x) (* -1 x (+ x 1) (+ x 2) (+ x 4))) -6 1 5 1e-10) 2)
                  '(-3.326 6.914) 1e-3)
    (check-within (take (estimate-global-min (λ (x) (* 1 x (+ x 1) (+ x 2) (+ x 4))) -6 1 6 1e-10) 2)
                  '(-3.326 -6.914) 1e-3))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))