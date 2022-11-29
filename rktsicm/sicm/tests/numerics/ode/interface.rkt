#lang s-exp "../../../main.rkt"

(require rackunit
         "../../../numerics/ode/interface.rkt"
         "../../helper.rkt"
         )


(define the-tests
  (test-suite
   "numerics/ode/ode-advancer"
;;; Takes a step in the direction of the specified dt.  Returns the
;;; state achieved, the step taken, which may be smaller than
;;; requested, and a suggested next step.
   (check-within ((free-run-state-advancer
                   (lambda (a)
                     (lambda (s)
                       (up 1 (up (* a (ref s 1 0)) (/ (ref s 1 1) a)))))
                   2.0)
                  (up -1 (up 1 1))
                  1.0
                  1.e-12
                  list)
                 (list (up 0. (up (exp 2) (exp 1/2))) 1.0 .8999999999999999)
                 1e-14)
;;; Takes a step of the size specified.  Returns the target state, the
;;; last step size, and a suggested further step.  The last two may
;;; not be very useful.
   (check-within (car ((state-advancer
                   (lambda (a)
                     (lambda (s)
                       (up 1 (* a (ref s 1)))))
                   2)
                  (up 0 1)
                  10
                  1.e-12
                  list))
                 (up 10. (exp 20))
                 #;(list (up 10. (exp 20)) 2.1316282072803006e-14 3.197442310920451e-14)
                 1e-4)
   (test-case
    "3"
    (set-ode-integration-method! 'gear)
    (set!-*compiling-sysder? #f)
    (check-within (car ((free-run-state-advancer
                    (lambda ()
                      (lambda (x)
                        (vector 1.0 (vector-ref x 1)))))
                   #(0.0 1.0)				;initial conditions
                   1.0					;target advance
                   .000000000001				;lte
                   list))
                  #(.999999999999987 2.7182818284594377)
                  #;(list #(.999999999999987 2.7182818284594377) 1. 5.010573747095126e-4)
                  1e-12))
   (test-case
    "3*"
    (set-ode-integration-method! 'gear)
    (set!-*compiling-sysder? #t)
    ;; => problem with environments 'matrix-from-rows is not installed in numerical-environment
    (check-within (car ((free-run-state-advancer
                    (lambda ()
                      (lambda (x)
                        (vector 1.0 (vector-ref x 1)))))
                   #(0.0 1.0)				;initial conditions
                   1.0					;target advance
                   .000000000001				;lte
                   list))
                  #(.999999999999987 2.7182818284594377)
                  #;(list #(.999999999999987 2.7182818284594377) 1. 5.010573747095126e-4)
                  1e-12))
   (test-case
    "4"
    (set-ode-integration-method! 'BULIRSCH-STOER)
    (set!-*compiling-sysder? #t)
    (define ((L-coupled-harmonic m k) state)
      (let ((q (coordinate state))
            (qdot (velocity state)))
        (- (* 1/2 qdot m qdot)
           (* 1/2 q k q))))
    (check-simplified? ((phase-space-derivative
                         (Lagrangian->Hamiltonian
                          (L-coupled-harmonic (down (down 'm_1 0)
                                                    (down 0 'm_2))
                                              (down (down 'k_1 'c)
                                                    (down 'c 'k_2)))))
                        (->H-state 't
                                   (coordinate-tuple 'x_1 'x_2)
                                   (momentum-tuple 'p_1 'p_2)))
                       '(up 1
                            (up (/ p_1 m_1)
                                (/ p_2 m_2))
                            (down (+ (* -1 c x_2) (* -1 k_1 x_1))
                                  (+ (* -1 c x_1) (* -1 k_2 x_2)))))
    (define (sysder-HO m1 m2 k1 k2 c)
      (phase-space-derivative
       (Lagrangian->Hamiltonian
        (L-coupled-harmonic (down (down m1 0)
                                  (down 0  m2))
                            (down (down k1 c)
                                  (down c  k2))))))
    (check-within ((state-advancer sysder-HO 1. 1. 1. 1. 0.)
                   (up 0. (up 1. 2.) (down 3. 4.))
                   10
                   1.e-12)
                  '#(10.000000000000004 #(-2.4711348617445603 -3.854227501710379)
                                        (*down* #(-1.9731934763399812 -2.2682438945270498)))
                  1e-10)
    (check-within ((evolve sysder-HO 1. 1. 1. 1. 0.)
                   (up 0 (up 1 2) (down 3 4)) void 1 10)
                  #(10. #(-2.47113486174456 -3.8542275017103753)
                        (*down* #(-1.9731934763399899 -2.2682438945270835)))
                  1e-12)
    (check-within (accumulate pe ((evolve sysder-HO 1. 1. 1. 1. 0.)
                                  (up 0 (up 1 2) (down 3 4)) pe 1 10))
                  (list (up 0 (up 1 2) (down 3 4))
                        (up 1. (up 3.064715260291832 4.4464885509678655) (down .7794359327965212 .47826725385676705))
                        (up 2. (up 2.3117454439299054 2.8048960342084457) (down -2.157737936467112 -3.483182199839933))
                        (up 3. (up -.5666324724208439 -1.4155049609614183) (down -3.111097497861209 -4.24221000252152))
                        (up 4. (up -2.9240511067874 -4.3344972229589365) (down -1.2041283672829095 -1.100969492838598))
                        (up 5. (up -2.593110638526191 -3.2683727277261077) (down 1.8099108310528178 3.052497291179177))
                        (up 6. (up .1219237920535883 .8026785805050183) (down 3.159926358150026 4.39951214299932))
                        (up 7. (up 2.724862050499671 4.13575090356176) (down 1.6047201643111262 1.701635819935653))
                        (up 8. (up 2.8225747060615296 3.666432918876308) (down -1.425858348049221 -2.5607166284812077))
                        (up 9. (up .3252251938405927 -.17378658280231318) (down -3.1455092708957855 -4.468758018022222))
                        (up 10. (up -2.47113486174456 -3.8542275017103753) (down -1.9731934763399899 -2.2682438945270835))
                        (up 10. (up -2.47113486174456 -3.8542275017103753) (down -1.9731934763399899 -2.2682438945270835)))
                  1e-12))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))