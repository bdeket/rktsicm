#lang racket/base

(require rackunit
         "../../poly/polyroot.rkt"
         "../../rkt/default-object.rkt"
         (only-in "../../kernel-intr.rkt" *machine-epsilon*)
         "../../simplify/pcf.rkt"
         "../helper.rkt")

(define (poly:from-roots #:a [a 1] . lst)
  (for/fold ([P (poly:dense-> (list a))])
            ([r (in-list lst)])
    (poly:* P (poly:dense-> (list (- r) 1)))))

(provide the-tests)
(define the-tests
  (test-suite
   "poly/polyroot"
   (test-case
    "complex-random"
    (check-within (magnitude (complex-random 1)) 1 1e-15)
    (check-within (magnitude (complex-random 4)) 4 1e-15)
    (check-within (magnitude (complex-random 0)) 0 1e-15)
    (check-true (complex? (complex-random 3))))
   (test-case
    "horners-rule"
    (check-within (horners-rule (poly:dense-> '(3 5 2 8)) 1) '(18 33 52 1.687538997430238e-14) 1e-15)
    (check-within (horners-rule (poly:dense-> '(3 5 2 8)) -1/2) '(0 9 -20 1.7763568394002505e-15) 1e-15))
   (test-case
    "roots->poly"
    (check-equal? (roots->poly '(0 1)) (poly:dense-> '(0 -1 1)))
    (check-equal? (roots->poly '(4 10 12)) (poly:dense-> '(-480 208 -26 1)))
    (check-equal? (poly:value (roots->poly '(2 4 9 12)) 9) 0))
   (test-case
    "expand-multiplicities"
    (check-equal? (expand-multiplicities '()) '())
    (check-equal? (expand-multiplicities '((3 . 2))) '(2 2 2))
    (check-equal? (expand-multiplicities '((3 . 2) (1 . 7))) '(2 2 2 7)))
   (test-case
    "cluster-multiple-roots"
    (check-equal? (cluster-multiple-roots '(1) #t) '(1))
    (check-equal? (cluster-multiple-roots '(1) #f) '((1 . 1)))
    (check-equal? (cluster-multiple-roots '(1 +i -i) default-object) '(+i -i 1))
    (check-equal? (cluster-multiple-roots '(1 +i -i) #f) '((1 . +i) (1 . -i) (1 . 1)))
    (check-equal? (cluster-multiple-roots `(1 ,(+ 1 1e-15) 2 ,(+ 2 1e-15) ,(- 2 1e-15)) #t) '(1. 1. 2. 2. 2.))
    (check-equal? (cluster-multiple-roots `(1 ,(+ 1 1e-15) 2 ,(+ 2 1e-15) ,(- 2 +1e-15i)) #f) '((2 . 1.0) (3 . 2.0))))
   (test-case
    "bring-to-real"
    (define eps (* imaginary-part-tolerance *machine-epsilon*))
    (check-equal? (bring-to-real (+ 1 (* +i  99/100 eps))) 1.)
    (check-equal? (bring-to-real (+ 1 (* +i 101/100 eps))) (+ 1 (* +i 101/100 eps))))
   (test-case
    "ensure-real"
    (check-equal? (ensure-real (poly:dense-> '(1 2+1e-20i 3))) (poly:dense-> '(1 2. 3))))
   (test-case
    "obviously-complex?"
    (check-true (obviously-complex? +i))
    (check-false (obviously-complex? 1.))
    (check-true (obviously-complex? +1+1e200i))
    (check-true (obviously-complex? +1+1i))
    (check-false (obviously-complex? +1+0.0001i)))
   (test-case
    "clean-up-root"
    (check-equal? (clean-up-root 0.0) 0.)
    (check-equal? (clean-up-root (+ 0.5 1e-15)) 0.5)
    (check-equal? (clean-up-root (* +i (+ 0.5 1e-15))) +0.5i)
    (check-equal? (clean-up-root (+ 0.5 (* +i (+ 0.5 1e-15)))) 0.5+0.5i)
    (check-equal? (clean-up-root (+ 1e-15 (* +i (+ 0.5 1e-15)))) 0.0+0.5i)
    (check-equal? (clean-up-root (+ (+ 0.5 1e-15) (* +i 1e-15))) 0.5))
   (test-case
    "deflate-poly"
    (check-equal? (deflate-poly (roots->poly '(1 5 8)) '(1 8)) (roots->poly '(5)))
    (check-equal? (deflate-poly (roots->poly '(1 3 3 3 4 9)) '(3 3 9)) (roots->poly '(1 3 4)))
    (set-polyroot-settings! #:wallp? #t)
    (check-equal? (out->string (deflate-poly (roots->poly '(1 5 8)) '(1 8))) "((*dense* 1 1 -5) 0)\n")
    (set-polyroot-settings! #:wallp? #f))
   (test-case
    "identify-multiple-roots"
    (check-equal? (identify-multiple-roots '()) '())
    (check-equal? (identify-multiple-roots '(1)) '((1 . 1)))
    (check-equal? (identify-multiple-roots `(1 ,(+ 1 1e-15) 2 ,(+ 2 1e-15) ,(- 2 1e-15) ,(- 2 +1e-15i))) '((2 . 1.0) (4 . 2.0)))
    (check-equal? (identify-multiple-roots `(1 ,(- 2 1e-15) ,(+ 1 1e-15) ,(+ 2 1e-15) ,(+ 2 +1e-10i) ,(- 2 +1e-10i))) '((4 . 2.0) (2 . 1.0)))
    (set-polyroot-settings! #:clustering? #f)
    (check-equal? (identify-multiple-roots `(1 ,(- 2 1e-15) ,(+ 1 1e-15) ,(+ 2 1e-15) ,(+ 2 +1e-10i) ,(- 2 +1e-10i)))
                  `((1 . 1) (1 . ,(- 2 1e-15)) (1 . ,(+ 1 1e-15)) (1 . ,(+ 2 1e-15)) (1 . ,(+ 2 +1e-10i)) (1 .  ,(- 2 +1e-10i))))
    (set-polyroot-settings! #:clustering? #t))

   (test-case
    "start-search"
    (define (TRY . rst) (cons 'try rst))
    (define (IMPROVE a b c d) (list `improve a b (c 'x1) (d)))
    (check-within (start-search (poly:dense-> '(0 -1 1)) 1/2 TRY IMPROVE)
                  '(improve 1/2
                            (-1/4 0 2 0)
                            (try x1 1/2 -1/4 0 1 0)
                            1/2)
                  1e-15)
    (check-within (start-search (poly:dense-> '(0 -1 1)) 1 TRY IMPROVE)
                  1
                  1e-15)
    (set-polyroot-settings! #:wallp? #t)
    (check-equal? (out->string (start-search (poly:dense-> '(0 -1 1)) 1/2 TRY IMPROVE))
                  "(hunting-starting-at 1/2 (*dense* 1 1 -1 0))\n(zero-divide-at 1/2 in startup of searcher)\n")
    (check-equal? (out->string (start-search (poly:dense-> '(0 -1 1)) 1 TRY IMPROVE))
                  "(hunting-starting-at 1 (*dense* 1 1 -1 0))\n(won-immediately-at 1 0 2.220446049250313e-16)\n")
    (set-polyroot-settings! #:wallp? #f))
   (test-case
    "kahan-secant-method"
    (define (TRY . rst) (cons 'try rst))
    ;(kahan-secant-method xn xn-1 vxn vxn-1 dvxn dvxn-1 try iter-count)
    (check-equal? (kahan-secant-method 2 3 5 5 9 9 TRY 'cntr) #f)
    (check-equal? (kahan-secant-method 3.01 3 5 7 9 11 TRY 'cntr) #f)
    (check-equal? (kahan-secant-method 2 3 5 7 9 11 TRY 'cntr) #f)
    (check-within (kahan-secant-method 2.19 3 5 7 9 11 TRY 'cntr)
                  '(try -3029/900 2.19 5 9 cntr 0) 1e-15)
    (set-polyroot-settings! #:wallp? #t)
    (check-equal? (out->string (kahan-secant-method 2.19 3 5 7 9 11 TRY 'cntr))
                  "(trying-kahan-trick 10.0)\n")
    (set-polyroot-settings! #:wallp? #f))
   (test-case
    "wrong-way"
    (define (TRY . rst) (cons 'try rst))
    ;(wrong-way xn xn-1 vxn-1 dvxn-1 try iter-count shrink-count)
    (check-equal? (wrong-way 2 3 5 7 TRY 'iter 0) '(try 11/4 3 5 7 iter 1))
    (check-within (wrong-way 2 5e-15 5 7 TRY 1 root-searcher-max-shrink)
                  '(try 5e-15 5e-15 5 7 1 0) 1e-15))
   (test-case
    "root-searcher"
    ;; branch 1
    (check-within (root-searcher (poly:dense-> '(-309 -81))) -3811/999 1e-15)
    ;; branch 2
    (check-within (root-searcher (poly:dense-> '(2 3))) -2/3 1e-15)
    (check-within (root-searcher (roots->poly '(-2/3))) -2/3 1e-15)
    (check-within (root-searcher (roots->poly '(5/8 6/8))) 5/8 5e-15)
    (check-within (root-searcher (roots->poly '(5+1i 5+2i))) 5+1i 5e-15)
    (check-within (root-searcher (roots->poly '(5+1e-20i 5+2e-20i))) 4.999999983549557-1.5159694527477896e-8i 5e-15)
    ;; branch 3
    (check-within (root-searcher (poly:dense-> '(-322 -156))) -161/78 1e-15)
    ;; branch 4
    (check-within (root-searcher (poly:dense-> '(240 303 172 281))) -0.7323002190917894 1e-15)
    ;; branch 5
    (check-within (root-searcher (poly:dense-> '(33 290))) -33/290 1e-15)
    ;; branch 6
    (check-within (root-searcher (poly:dense-> '(133 308))) -133/308 1e-15)

    ;; TODO: is this worth it?
    (set-polyroot-settings! #:wallp? #t)
    (check-equal? (out->string (root-searcher (poly:dense-> '(-223 -146 -247))))
                  "(hunting-starting-at 0.1+0.1i (*dense* 1 -247 -146 -223))\n(trying -0.29554655870445345+0.9030427673898512i -2.842170943040401e-14+0.0i 1.4854784069484595e-13)\n(found-winner-at -0.29554655870445345+0.9030427673898512i -2.842170943040401e-14+0.0i 1.4854784069484595e-13)\n")
    (check-equal? (out->string (root-searcher (poly:dense-> '(146 -279))))
                  "(hunting-starting-at 0.1+0.1i (*dense* 1 -279 146))\n(trying 0.5232974910394265 2.842170943040401e-14 3.241851231905457e-14)\n(trying 0.5232974910394266 -2.842170943040401e-14 3.2418512319054584e-14)\n(found-lazy-winner-at 0.5232974910394266 -2.842170943040401e-14 3.2418512319054584e-14)\n")
    (check-equal? (out->string (root-searcher (poly:dense-> '(0 -97))))
                  "(hunting-starting-at 0.1+0.1i (*dense* 1 -97 0))\n(trying -1.3877787807814457e-17-1.3877787807814457e-17i 1.3461454173580023e-15+1.3461454173580023e-15i 8.454291072342091e-31)\n(trying 0.0+0.0i -0.0-0.0i 0.0)\n(zero-divide-at 0.0+0.0i in searcher)\n")
    (set-polyroot-settings! #:wallp? #f)

    (check-exn #px"Search failed" (λ () (root-searcher '(*dense* 1 +inf.0 -inf.0 +inf.0 -inf.0 +inf.0 1.0))))
    )
   (test-case
    "rescale-poly-roots"
    (define (SEARCHER x) (cons 'searcher x))
    (check-equal? (rescale-poly-roots (poly:dense-> '(0 0 0 1)) SEARCHER) 0)
    (check-equal? (rescale-poly-roots (poly:dense-> '(1 2 -3)) SEARCHER) '(searcher *dense* 1 -3 2 1))
    (check-equal? (rescale-poly-roots (poly:dense-> '(1 2 -3e40)) root-searcher) -5.7735026918962575e-21+3.3031298511041507e-38i)
    (check-equal? (rescale-poly-roots (poly:dense-> '(1 2 -3e-40)) root-searcher)  -0.5+3.061616997868383e-17i))

   (test-case
    "poly-newton-method"
    (define (SUCCEED x) (cons 'succeed x))
    (define (FAIL) 'fail)
    (check-equal? ((poly-newton-method (poly:dense-> '(0 -1 1))) 1/2 '(3 4 5) SUCCEED FAIL)
                  '(succeed . -1/4))
    (check-equal? ((poly-newton-method (poly:dense-> '(0 -1 1))) 1/2 '(2 0 5) SUCCEED FAIL)
                  'fail)
    (check-equal? ((poly-newton-method (poly:dense-> '(0 -1 1))) 1/2 '(0 4 5) SUCCEED FAIL)
                  '(succeed . 1/2))
    (check-equal? ((poly-newton-method (poly:dense-> '(0 -1 1))) 1/2 '(3 1 -5) SUCCEED FAIL)
                  '(succeed . -5/2)))
   (test-case
    "poly-laguerre-method"
    (define (SUCCEED x) (cons 'succeed x))
    (define (FAIL) 'fail)
    (check-equal? ((poly-laguerre-method (poly:dense-> '(0 -1 1))) 1/2 '(3 4 5) SUCCEED FAIL)
                  '(succeed . -0.30000000000000004-0.7483314773547882i))
    (check-equal? ((poly-laguerre-method (poly:dense-> '(0 -1 1))) 1/2 '(2 0 5) SUCCEED FAIL)
                  '(succeed . 0.5-0.8944271909999159i))
    (check-equal? ((poly-laguerre-method (poly:dense-> '(0 -1 1))) 1/2 '(0 4 5) SUCCEED FAIL)
                  'fail)
    (check-equal? ((poly-laguerre-method (poly:dense-> '(0 -1 1))) 1/2 '(3 1 -5) SUCCEED FAIL)
                  '(succeed . -0.41355287256600437))
    (check-equal? ((poly-laguerre-method (poly:dense-> '(0 -1 1))) 5e-101 '(3 1e-100 -5e-210) SUCCEED FAIL)
                  '(succeed . -1.7320508075688772)))
   (test-case
    "root-polisher"
    (check-equal? ((root-polisher (poly:dense-> '(0 -1 1))) .25) 0.)
    (check-equal? ((root-polisher (poly:dense-> '(0 -1 1))) .75) 1.)
    (check-equal? ((root-polisher (poly:dense-> '(0 -1 1))) 100.) 1.)
    (check-equal? ((root-polisher (poly:dense-> '(295 52 -42))) 4.0) 3.340633457939725)
    (check-equal? ((root-polisher (poly:dense-> '(471 -365 341))) 13.0) 0.7811268919939889)
    (check-equal? ((root-polisher (poly:dense-> '(-8.417388536670931e+119 -9.00755011313127e-296))) -7.934925000125151e-90) -7.934925000125151e-90)
    (set-polyroot-settings! #:wallp? #t)
    (check-equal? (out->string ((root-polisher (poly:dense-> '(0 -1 1))) 1e-200))
                  "(polishing root 1e-200 -1e-200 6.661338147750939e-216)\n(good-enuf-at 0.0)\n")
    (check-equal? (out->string ((root-polisher (poly:dense-> '(295 52 -42))) 3.340633457939725))
                  "(polishing root 3.340633457939725 5.684341886080802e-14 2.3508149644976626e-13)\n(win-at 3.340633457939725)\n")
    (check-equal? (out->string ((root-polisher (poly:dense-> '(471 -365 341))) 0.7811268919939889))
                  "(polishing root 0.7811268919939889 393.95297891829443 1.6789043834681962e-13)\n(got-worse-at -1.5676265051318206)\n")
    (check-equal? (out->string ((root-polisher (poly:dense-> '(-8.417388536670931e+119 -9.00755011313127e-296))) -7.934925000125151e-90))
                  "(polishing root -7.934925000125151e-90 -8.417388536670931e+119 1.8690357121255844e+104)\n(zero-divide-at -7.934925000125151e-90 in polisher)\n")
    (set-polyroot-settings! #:wallp? #f))
   
   (test-case
    "poly->roots"
    (define (<< x y) (or (< (real-part x) (real-part y))
                         (and (= (real-part x) (real-part y))
                              (< (imag-part x) (imag-part y)))))
    (check-equal? (poly->roots 5) '())
    (check-within (sort (poly->roots (roots->poly '(1 -1))) <<) '(-1. 1.) 1e-15)
    (check-within (sort (poly->roots (roots->poly '(4 2 -8))) <<) '(-8. 2. 4.) 1e-15)
    (check-within (sort (poly->roots (poly:dense-> '(4 -4 2))) <<) '(1-1i 1+1i) 1e-15)
    (check-within (sort (poly->roots (poly:dense-> '(0 4 -4 2))) <<) '(0 1-1i 1+1i) 1e-15)
    (set-polyroot-settings! #:wallp? #t)
    (check-not-false (regexp-match #px"\\(finder-loop \\(\\*dense\\* 1 1 0 -1\\) \\(\\)\\)"
                                   (out->string (poly->roots (roots->poly '(1 -1))))))
    (set-polyroot-settings! #:wallp? #f)
    (check-exn #px"Root finder failed"
               (λ () (poly->roots (poly:dense-> '(1.9457912621905344e-185 -6.344980807340924e-175))))))
   (skip
    (test-case
     "failing polys"
     ;; division by zero
     (poly->roots (poly:dense-> '(-8.965902202076102e-257 6.561498053106826e+299 1.2996897591359907e-67 -4.0837559670208424e+229 3.1893542523535725e+31 -3.141414099744984e+148)))
     (poly->roots (poly:dense-> '(-2.3339490843586745e-127 -5.6829771071862026e-89 2.3450841676429883e-207 -8.636081663977338e+80 -2.622588673092726e+165)))
     (poly->roots (poly:dense-> '(1.0658731516898746e-188 -5.254015980722965e+164)))
     ))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))