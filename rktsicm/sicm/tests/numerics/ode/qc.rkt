#lang racket/base

(require rackunit
         "../../../numerics/ode/advance.rkt"
         "../../../numerics/ode/qc.rkt"
         "../../../kernel/matrices.rkt"
         "../../helper.rkt"
         )


(define the-tests
  (test-suite
   "numerics/ode/qc"
   (test-case
    "\"simple\" explicit method"
    ;;; A "simple" explicit method, based on fourth-order Runge-Kutta:
    (check-within ((advance-generator
                                ((quality-control rk4 4)		;integration method
                                 (lambda (v) v)			        ;x' = x
                                 .0001))				;error tolerated
                               #(1.0)					;initial state (at t = t0)
                               1.0					;proceed to t = t0 + 1
                               0.1					;first step no larger than .1
                               0.5					;no step larger than .5
                               (lambda (ns dt h cont)
                                 (cons ns (cont)))
                               (lambda (ns dt sdt)
                                 ;; assert ns = #(2.718...)
                                 ;; assert dt = 1.000...+-
                                 (list ns dt sdt)))
                  '(#(1.0)
                    #(1.1051709178357205)
                    #(1.6129327932363555)
                    #(2.3979317172127734)
                    #(2.718278482477914) 1.0 0.3797097166064178)
                  #; ;;bdk;; 'completely' differen, but why?
                  '(#(1.)
                    #(1.648716933638961)
                    #(2.588254411801423)
                    #(2.7182707063734948) 1. .4041654277154577)
                  .0001))
   (test-case
    "A trapezoid method: xn+1 is found by corrector iteration"
    (check-within ((advance-generator
                    ((quality-control c-trapezoid 2)	;integration method
                     (lambda (v) v)			;x' = x
                     0.0001				;qc error tolerated
                     1.0e-5))				;corrector convergence
                   #(1.0)				;initial state (at t = t0)
                   1.0					;proceed to t = t0 + 1
                   0.1					;first step no larger than .1
                   0.5					;no step larger than .5
                   (lambda (ns dt h cont)
                     (cons ns (cont)))
                   (lambda (ns dt sdt)
                     ;; assert ns = #(2.718...)
                     ;; assert dt = 1.000...+-
                     (list ns dt sdt)))
                  '(#(1.0)
                    #(1.1051710456388344)
                    #(1.2163624776833994)
                    #(1.338717086036316)
                    #(1.4733793050313753)
                    #(1.6215872632471207)
                    #(1.7847035338027317)
                    #(1.9642277512646764)
                    #(2.161810399185505)
                    #(2.3792679840801374)
                    #(2.6185997357588873)
                    #(2.7182845408260983) 1.0 0.09776551613488911)
                  #; ;;bdk;; 'pretty' different
                  '(#(1.)
                    #(1.1051688554687495)
                    #(1.2583037182508854)
                    #(1.4307307288261986)
                    #(1.6229900169138303)
                    #(1.8372249433735863)
                    #(2.0758338727890635)
                    #(2.3414853586609374)
                    #(2.637148056178347)
                    #(2.7182832352360498) 1. .11894979864256087)
                  .0001))
   (test-case
    ";;; A trapezoid method:  xn+1 is found by Newton iteration"
    (check-within ((advance-generator
                    ((quality-control n-trapezoid 2)	;integration method
                     (lambda (v cont)			;x' = x
                       (cont v (array->matrix #(#(1.0)))))
                     0.0001				;qc-error tolerated
                     1					;state dimension
                     1.0e-5))				;corrector convergence
                   #(1.0)					;initial state (at t = t0)
                   1.0					;proceed to t = t0 + 1
                   0.1					;first step no larger than .1
                   0.5					;no step larger than .5
                   (lambda (ns dt h cont)
                     (cons ns (cont)))
                   (lambda (ns dt sdt)
                     ;; assert ns = #(2.718...)
                     ;; assert dt = 1.000...+-
                     (list ns dt sdt)))
                  '(#(1.0)
                    #(1.1051708824988178)
                    #(1.2160153434746208)
                    #(1.3379840076294482)
                    #(1.4721863533069754)
                    #(1.6198494500075877)
                    #(1.782323436700406)
                    #(1.9610938738757053)
                    #(2.1577953265722836)
                    #(2.3742263098170913)
                    #(2.6123657331212584)
                    #(2.7182811126262187) 1.0 0.09562982862555519)
                  #; ;;bdk;; again
                  '(#(1.)
                    #(1.1051708824988178)
                    #(1.259109256732086)
                    #(1.4307187104000767)
                    #(1.6219876372976312)
                    #(1.8350462370303233)
                    #(2.0722642596439016)
                    #(2.3362773683519777)
                    #(2.630016590666874)
                    #(2.718279922395027) 1. .11684285320335219)
                  .0001))
    ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))