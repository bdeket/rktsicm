#lang s-exp "../../../main.rkt"

(require rackunit
         "../../helper.rkt"
         )

(define the-tests
  (test-suite
   "numerics/signals/fourier"
   (test-case
    "fdelta"
    (define fdelta
      (signal->frequency-function
       (time-function->signal
        (sigfun:make (constant 1)
                     (sigfun:make-span -10 10)))))
    (define mfdelta (magnitude fdelta))
    (check-= ((sigfun:procedure mfdelta) 0) 20 1e-15)
    (check-= ((sigfun:procedure mfdelta) .1) 0 1e-15)
    (check-= (definite-integral (sigfun:procedure mfdelta) -.1 +.1 .001 #f)
             1. 1e-12)
    )
   (test-case
    "tdelta"
    (define tdelta
      (signal->time-function
       (frequency-function->signal
        (sigfun:make (constant 1)
                     (sigfun:make-span -25.6 25.6)))))

    (define mtdelta (magnitude tdelta))
    (check-= ((sigfun:procedure mtdelta) 0) 51.2 1e-15)
    (check-= (definite-integral (sigfun:procedure mtdelta) -.03 +.03 .001 #f)
             .9997816051629261 1e-4))
   (test-case
    "test-cos"
    (define ((test-cos a f phi) t)
      (* a (cos (+ (* :2pi f t) phi))))
    (define tc
      (sigfun:make (test-cos 2 8.2 1) (sigfun:make-span -2 +2)))
    (check-= (time-domain-energy tc) 16.027322899638975 1e-1)
    (define fc (Fourier-transform tc))
    (check-= (frequency-domain-energy fc) 16.027322899639 1e-1))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))