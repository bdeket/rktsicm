#lang s-exp "../../main.rkt"

(require "../../numerics/extrapolate/re.rkt")

(require rackunit
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "mechanics/pendulum"
   (test-case
    "in file tests"
    (define (der-qq f state)
      ((richardson-derivative 
        (lambda (q)
          (state->q 
           (f (->H-state (state->t state) q (state->p state)))))
        1.e-8
        .01)
       (state->q state)))

    (define (der-qp f state)
      ((richardson-derivative 
        (lambda (p)
          (state->q 
           (f (->H-state (state->t state) (state->q state) p))))
        1.e-8
        .01)
       (state->p state)))

    (define (der-pq f state)
      ((richardson-derivative 
        (lambda (q)
          (state->p 
           (f (->H-state (state->t state) q (state->p state)))))
        1.e-8
        .01)
       (state->q state)))

    (define (der-pp f state)
      ((richardson-derivative 
        (lambda (p)
          (state->p 
           (f (->H-state (state->t state) (state->q state) p))))
        1.e-8
        .01)
       (state->p state)))

    (skip "from original file: VERY different accuracy")

    (check-= (let ((f (pendulum-circulating-aa-state-to-state 2.0 9.8))
                   (g (pendulum-circulating-state-to-aa-state 2.0 9.8))) 
               (let* ((state (->H-state 1. 1. 15.))
                      (aa-state (g state)))
                 (- (* (der-qq f aa-state) (der-pp f aa-state))
                    (* (der-pq f aa-state) (der-qp f aa-state)))))
             ;Value: 1.0000000000003484
             1.0000000000003484 1e-6)

    (check-= (let ((f (pendulum-circulating-aa-state-to-state 2.0 9.8))
                   (g (pendulum-circulating-state-to-aa-state 2.0 9.8))) 
               (let* ((state (->H-state 1. 1. 15.))
                      (aa-state (g state)))
                 (- (* (der-qq g state) (der-pp g state))
                    (* (der-pq g state) (der-qp g state)))))
             ;Value: .9999999999986688
             .9999999999986688 1e-15)

    (check-= (let ((f (pendulum-oscillating-aa-state-to-state 2.0 9.8))
                   (g (pendulum-oscillating-state-to-aa-state 2.0 9.8))) 
               (let* ((state (->H-state 1. 1. 1.))
                      (aa-state (g state)))
                 (- (* (der-qq g state) (der-pp g state))
                    (* (der-pq g state) (der-qp g state)))))
             ;Value: 1.000000000000521
             1.000000000000521 1e-15)

    (check-= (let ((f (pendulum-oscillating-aa-state-to-state 2.0 9.8))
                   (g (pendulum-oscillating-state-to-aa-state 2.0 9.8))) 
               (let* ((state (->H-state 1. 1. 1.))
                      (aa-state (g state)))
                 (- (* (der-qq f aa-state) (der-pp f aa-state))
                    (* (der-pq f aa-state) (der-qp f aa-state)))))
             ;Value: 1.000000000000406
             1.000000000000406 1e-6)
    )
   (test-case
    "pendululm-solution-series"
    (define (((pendulum-solution-series alpha beta) state) time)
      (let ((E ((Hpendulum alpha beta) state)))
        (let ((omega (pendulum-frequency alpha beta E))
              (beta (abs beta)))    
          (if (< E beta)
              (let ((k (sqrt (/ (+ E beta) (* 2 beta))))
                    (omega-0 (sqrt (abs (/ beta alpha)))))
                (let ((Kp (first-elliptic-integral (sqrt (- 1 (square k))))))
                  (define (term n)
                    (let ((omega-n (* omega (- (* 2 n) 1))))
                      (/ (sin (* omega-n time))
                         (* omega-n (cosh (/ (* omega-n Kp) omega-0))))))
                  (* 4 omega (series:generate (lambda (i) (term (+ i 1)))))))
              (let ((k (sqrt (/ (* 2 beta) (+ E beta))))
                    (omega-R (sqrt (abs (/ (+ E beta) (* 2 alpha))))))
                (let ((Kp (first-elliptic-integral (sqrt (- 1 (square k))))))
                  (define ((term time) n)
                    (let ((omega-n (* omega n)))
                      (/ (sin (* omega-n time))
                         (* omega-n (cosh (/ (* omega-n Kp) omega-R))))))
                  (+ (* omega time)
                     (* 2 omega (series:generate (lambda (i) ((term time) (+ i 1))))))))))))
    #|
    (series:print
     (((pendulum-solution-series 1. 9.8)
       (->H-state 0. 0. 4.9006733894348145)) 't))
    (* 1.8349993630564594 (sin (* 2.5043962735932013 t)))
    (* .03821300344597103 (sin (* 7.513188820779604 t)))
    (* .00135312864251141 (sin (* 12.521981367966006 t)))
    (* 5.702944261999213e-5 (sin (* 17.53077391515241 t)))
    (* 2.617233223741749e-6 (sin (* 22.53956646233881 t)))
    (* 1.2635138738869227e-7 (sin (* 27.548359009525214 t)))
    (* 6.308369363000512e-9 (sin (* 32.55715155671162 t)))
    (* 3.225945107424557e-10 (sin (* 37.56594410389802 t)))
    (* 1.679527336266625e-11 (sin (* 42.57473665108442 t)))
    (* 8.866866369088442e-13 (sin (* 47.583529198270824 t)))
    ;|#
    (check-within (map simplify
                       (stream->list
                        (stream-take
                         (series->stream
                          (((pendulum-solution-series 1. 9.8)
                            (->H-state 0. 0. 4.9006733894348145)) 't))
                         10)))
                  '((* 1.8349993630564594 (sin (* 2.5043962735932013 t)))
                    (* .03821300344597103 (sin (* 7.513188820779604 t)))
                    (* .00135312864251141 (sin (* 12.521981367966006 t)))
                    (* 5.702944261999213e-5 (sin (* 17.53077391515241 t)))
                    (* 2.617233223741749e-6 (sin (* 22.53956646233881 t)))
                    (* 1.2635138738869227e-7 (sin (* 27.548359009525214 t)))
                    (* 6.308369363000512e-9 (sin (* 32.55715155671162 t)))
                    (* 3.225945107424557e-10 (sin (* 37.56594410389802 t)))
                    (* 1.679527336266625e-11 (sin (* 42.57473665108442 t)))
                    (* 8.866866369088442e-13 (sin (* 47.583529198270824 t))))
                  1e-10))
   ;;******************************
   (test-case
    "pendulum-integration"
    (let ([alpha 1]
          [beta 2]
          [eps 0.01]
          [state (up 0 0 0)])
      (check-within (for/list ([time (in-range 5)])
                      (((pendulum-integration alpha beta eps) state) time))
                    (for/list ([time (in-range 5)])
                      (up time 0 0))
                    1e-300))
    (let ([alpha 1/2]
          [beta 1.24]
          [eps 0.01]
          [state (up 0 0.3 0)])
      (check-within (for/list ([time (in-range 5)])
                      (((pendulum-integration alpha beta eps) state) time))
                    (list (up 0 .3 0) (up 1 0 -.24) (up 2 -.3 0) (up 3 0 .24) (up 4 0.3 0))
                    .01)))
   (test-case
    "pendulum-oscilating-action"
    (check-= (pendulum-oscillating-action 1 1 1) (/ 8 :pi) 1e-15))
   (test-case
    "pendulum-oscillating-phase"
    (check-within ((pendulum-oscillating-phase 1/2 1.24) (up 0 0 -.24))
                  :-pi 0.01))
   (test-case
    "pendulum-advance"
    (check-exn #px"at the fixed point the phase is undefined"
               (λ () (((pendulum-advance 1/2 1.24) (up 0 0 0)) 1)))
    (let ([alpha 1/2]
          [beta 1.24]
          [state (up 0 0.3 0)])
      (check-within (for/list ([time (in-range 5)])
                      (((pendulum-advance alpha beta) state) time))
                    (list (up 0 .3 0) (up 1 0 -.24) (up 2 -.3 0) (up 3 0 .24) (up 4 0.3 0))
                    .01)
      (check-within (for/fold ([ans (list state)]
                               #:result (reverse ans))
                              ([time (in-range 1 5)])
                      (cons (((pendulum-advance alpha beta) (car ans)) time)
                            ans))
                    (list (up 0 .3 0) (up 1 0 -.24) (up 2 -.3 0) (up 3 0 .24) (up 4 0.3 0))
                    .01))
    (let ([alpha 1/2]
          [beta 1.24]
          [state (up 0 2 1)])
      (check-within (((pendulum-advance alpha beta) state) 1)
                    (up 1 -2.968 0.543)
                    .001))
    (let ([alpha 1/2]
          [beta 1.24]
          [state (up 0 2 -1)])
      (check-within (((pendulum-advance alpha beta) state) 1)
                    (up 1 -0.913 -1.508)
                    .001))
    (let ([alpha 1]
          [beta 1]
          [state (up 0 0 2)])
      (check-equal? (((pendulum-advance alpha beta) state) 1)
                    (up 1 +nan.0 +nan.0))))
   (test-case
    "pstate->gstate"
    (let ([alpha 1/2]
          [beta 1.24]
          [pstate (up 0 0.3 0)])
      (define gstate ((pendulum-state-to-global-aa-state alpha beta) pstate))
      (check-within ((pendulum-global-aa-state-to-state alpha beta) gstate)
                    pstate
                    1e-7))
    (let ([alpha 1/2]
          [beta 1.24]
          [pstate (up 0 2 1)])
      (define gstate ((pendulum-state-to-global-aa-state alpha beta) pstate))
      (check-within ((pendulum-global-aa-state-to-state alpha beta) gstate)
                    pstate
                    1e-7))
    (let ([alpha 1/2]
          [beta 1.24]
          [pstate (up 0 2 -1)])
      (define gstate ((pendulum-state-to-global-aa-state alpha beta) pstate))
      (check-within ((pendulum-global-aa-state-to-state alpha beta) gstate)
                    pstate
                    1e-7))
    (let ([alpha 1]
          [beta 1]
          [pstate (up 0 0 2)]
          [gstate (up 0 0 2.5464790894703255)])
      (check-equal? ((pendulum-state-to-global-aa-state alpha beta) pstate) 'go-figure)
      (check-equal? ((pendulum-global-aa-state-to-state alpha beta) gstate) 'oh-well)))
   (test-case
    "separatrix case"
    (check-= ((pendulum-separatrix-angular-momentum 1 1) 0) 2. 1e-10)
    (check-= (inverse-gudermannian (gudermannian 1)) 1 1e-10)
    )
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))