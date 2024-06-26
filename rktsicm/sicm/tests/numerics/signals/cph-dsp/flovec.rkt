#lang racket/base

(require rackunit
         "../../../../numerics/signals/cph-dsp/flovec.rkt"
         racket/flonum
         )

(define F (flo:make-initialized-vector 5 values))
(define G (flo:make-vector 8 3))
   
(provide the-tests)
(define the-tests
  (test-suite
   "numerics/signals/cph-dsp/flovec"
   
   (check-equal? (flonum-vector->vector F)
                 #(0. 1. 2. 3. 4.))
   (check-equal? (flonum-vector->list F)
                 '(0. 1. 2. 3. 4.))
   (check-equal? (vector->flonum-vector #(0 1 2 3 4))
                 F)
   (check-equal? (list->flonum-vector '(0 1 2 3 4))
                 F)
   (check-equal? (flo:make-vector 2 2)
                 (flo:vector 2. 2.))
   (check-equal? (flo:make-vector 2)
                 (flo:vector 0. 0.))
   (check-equal? (flo:make-initialized-vector 2 (λ (i) (expt (+ i 2) 2)))
                 (flo:vector 4. 9.))
   (check-equal? (flo:subvector F 1 3)
                 (flo:make-initialized-vector 2 add1))
   (check-equal? (flo:vector-grow F 8 5)
                 (flo:vector 0. 1. 2. 3. 4. 5. 5. 5.))
   (check-equal? (flo:vector-grow F 8)
                 (flo:vector 0. 1. 2. 3. 4. 0. 0. 0.))
   (check-equal? F (flo:vector 0. 1. 2. 3. 4.))

   (flo:subvector-move! F 0 3 F 1)
   (check-equal? F (flo:vector 0. 0. 1. 2. 4.))

   (flo:subvector-move! F 2 5 F 0)
   (check-equal? F (flo:vector 1. 2. 4. 2. 4.))

   (flo:subvector-move! F 0 5 G 2)
   (check-equal? F (flo:vector 1. 2. 4. 2. 4.))
   (check-equal? G (flo:vector 3. 3. 1. 2. 4. 2. 4. 3.))

   (check-equal? (flo:vector-map F (λ (x) (fl+ 2. x)))
                 (flo:vector 3. 4. 6. 4. 6.))

   (void (flo:vector-map! F (λ (x) (fl* 2. x))))
   (check-equal? F (flo:vector 2. 4. 8. 4. 8.))

   (check-equal? (let ([s 0.0]) (flo:vector-for-each F (λ (v) (set! s (+ s v)))) s)
                 26.)

   (check-equal? (let ([s 0.0]) (flo:subvector-for-each F 1 3 (λ (v) (set! s (+ s v)))) s)
                 12.)

   (flo:vector-fill! F 0)
   (check-equal? F (flo:vector 0. 0. 0. 0. 0.))

   (flo:subvector-fill! G 3 5 0)
   (check-equal? G (flo:vector 3. 3. 1. 0. 0. 2. 4. 3.))

   (flo:set-vector-length! G 5)
   (check-equal? G (flo:vector 3. 3. 1. 0. 0.))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))