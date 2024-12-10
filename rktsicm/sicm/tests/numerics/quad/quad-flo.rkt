#lang racket/base

(require rackunit
         racket/port
         "../../../numerics/quad/quad-flo.rkt"
         )


(provide the-tests)
(define the-tests
  (test-suite
   "numerics/quad/quad-flo"
   ;; solutions checked with fl_/error and fl2_
   ;; or with bf_
   (test-case
    "+B / -B (fl2+/error fl2-/error)"
    (check-equal? (+B 2. 3e-20 list) (list 2. 3e-20))
    (check-equal? (+B #i27/11 #i10/9e-10 list)
                  (list 2.4545454546565657 -9.193374560022929e-18))
    (check-equal? (-B 2. 3e-20 list) (list 2. -3e-20))
    (check-equal? (-B #i27/11 #i10/9e-10 list)
                  (list 2.4545454544343435 9.193374560022929e-18)))
   (test-case
    "*T / /T (fl2*/error fl2//error)"
    (check-equal? (*T 2. 3e-20 list) (list 6e-20 0.0))
    (check-equal? (*T #i27/11 1.11111111111111111111e-10 list)
                  (list 2.7272727272727273e-10 -9.613919937708804e-27))
    (check-equal? (/T 2. 3e-20 list) (list 6.666666666666666e+19 -3643.769890630824))
    (check-equal? (/T #i27/11 #i10/9e-10 list)
                  (list 22090909090.909092 2.3126322489393405e-07)))
   (test-case
    "quad:+ / -"
    (define A (+B 2. 3e-20 cons))
    (define B (+B #i27/11 #i10/9e-10 cons))
    (check-equal? (quad:+ A B) '(4.454545454656565 . 4.349258352900397e-16))
    (check-equal? (quad:- A B) '(-0.4545454546565657 . 9.22337456002293e-18)))
   (test-case
    "quad:* / /"
    (define A (+B 2. 3e-20 cons))
    (define B (+B #i27/11 #i10/9e-10 cons))
    (check-equal? (quad:* A B) '(4.909090909313131 . -1.8313112756406162e-17))
    (check-equal? (quad:/ A B) '(0.8148148147779302 . 1.7076784174845263e-17)))
   (test-case
    "quad:square sqrt"
    (define B (+B #i27/11 #i10/9e-10 cons))
    (check-equal? (quad:square B) '(6.0247933889752066 . 2.5583425245700465e-16))
    (check-equal? (quad:sqrt B)   '(1.566698903636741 . -9.409527550149118e-17))
    (check-equal? (quad:sqrt      '(1.234567890123456e-4 . 7.8901234567890123e-20))
                  ;; fl2sqrt => '(0.011111111061111111 . -3.2472832006706835e-19)
                  ;; but input is not a wellmade quad (overlapping...)
                  ;; (quad:sqrt (+B 1.234567890123456e-4 7.8901234567890123e-20))
                  '(0.011111111061111111 . -3.2472832006706815e-19)))
   (test-case
    "quad:gonio"
    (define B (+B #i27/11 #i10/9e-10 cons))
    (parameterize ([current-output-port (open-output-nowhere)])
    ;; pi.bf:                          3.141592653589793      1.2246467991473532e-16
      (check-equal? (compute-quad:pi)'(3.141592653589793   .  1.224646799147354e-16)))
    ;; BFsin:                          0.6342570744828055    -5.479368300310054e-18
    (check-equal? (quad:sin B)       '(0.6342570744828055  . -5.479368300309996e-18))
    (check-equal? (alternate-sine B) '(0.6342570744828055  . -5.479368300310042e-18))
    ;; BFcos:                          -0.7731222176787528   -4.392084361674799e-17
    (check-equal? (quad:cos B)       '(-0.7731222176787528 . -4.392084361674801e-17))
    ;; BFtan:                          -0.8203839703211732   -1.7549486968203398e-17
    (check-equal? (quad:tan B)       '(-0.8203839703211732 . -1.754948696820345e-17))
    ;; BFatan:                         1.1839206090796854     1.0386447408892389e-16
    (check-equal? (quad:atan B)      '(1.1839206090796854  .  1.0386447408892368e-16)))
   (test-case
    "complex-quad"
    (define A (+B 2. 3e-20 cons))
    (define Bc 27/11+10/9i)
    (define Ar (q->cq A))
    (define Bcq (c->cq Bc))
    (define Z0 (cquad:+ Ar Bcq))
    (check-equal? Z0 '((4.454545454545455  . -4.440592098500626e-16)  (1.1111111111111112  .  0.0)))
    (define Z1 (cquad:* Z0 Z0))
    (check-equal? Z1 '((18.608407305377003 . -7.853279441261489e-16)  (9.8989898989899     . -1.5244881954446592e-16)))
    (define Z2 (cquad:- Z1 Z0))
    (check-equal? Z2 '((14.153861850831548 . -3.4126873427608626e-16) (8.787878787878789   . -5.965380293945285e-16)))
    (define Z3 (cquad:/ Z2 Z1))
    (check-equal? Z3 '((0.7886591700107948 .  2.5548555261476136e-17) (0.05271539977055005 .  1.3120091679257676e-18)))
    (check-equal? (cquad:/ (cquad:+ (cquad:* Z3 Z1) Z0) Z0)
                  ;;;'((4.454545454545455  . -4.440592098500626e-16)  (1.1111111111111112  .  0.0))
                     '((4.454545454545455  . -4.440592098500625e-16)  (1.1111111111111112  . -1.8488927466117464e-32)))
    (check-equal? (quad:magnitude Z2)
                  '(16.660090602480967 . -1.4626082127539948e-16))
    )
   
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))