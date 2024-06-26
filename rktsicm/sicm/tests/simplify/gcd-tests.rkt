#lang s-exp "../../generic.rkt"

(require rackunit
         "../../rkt/glue.rkt"
         "../../general/assert.rkt"
         "../../simplify.rkt"
         "../helper.rkt")

(define (try . n)
  (let* ((f (pcf:expression-> (expression (apply F n)) (lambda (p v) p)))
	 (g (pcf:expression-> (expression (apply G n)) (lambda (p v) p)))
	 (ans 'start))
    ;(display "\n\nEuclid:\n")
    (set! ans (poly/gcd-euclid f g))
    (if (or (poly/not-divisible? f ans)
	    (poly/not-divisible? g ans))
	(error "Bad gcd euclid" f g ans))
    ;(display "\n\nSparse:\n")
    (set! ans (poly/gcd-sparse f g))
    (if (or (poly/not-divisible? f ans)
	    (poly/not-divisible? g ans))
	(error "Bad gcd sparse" f g ans)))
  'done)
(define (F nu)
  (* (+ 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 1)
     (+ 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 2)))
(define (G nu)
  (* (+ (square 'x) (sigma (lambda (i) (symbol 'y i)) 1 nu) 1)
     (+ (* -3 'y1 (square 'x)) (square 'y1) 1)))


(provide the-tests)
(define the-tests
  (test-suite
   "simplify/rz-test-cases"
   (test-case
    "Case 1: GCD=1"
    (set! F (λ (nu)
              (* (+ 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 1)
                 (+ 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 2))))
    (set! G (λ (nu)
              (* (+ (square 'x) (sigma (lambda (i) (symbol 'y i)) 1 nu) 1)
                 (+ (* -3 'y1 (square 'x)) (square 'y1) 1))))
    (rkt:time (void (try 4) (try 5) (try 6) (check-true #t))))
   
   (test-case
    "Case 2: Linearly dense quartic inputs with quadratic gcd"
    (define (H nu)
      (square (+ 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 1)))
    (set! F(λ (nu)
             (* (H nu)
                (square (- 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 2)))))
    (set! G (λ (nu)
              (* (H nu)
                 (square (+ 'x (sigma (lambda (i) (symbol 'y i)) 1 nu) 2)))))
    (rkt:time (void (try 5) (try 6) (try 7) (check-true #t))))
   
   (test-case
    "Case 3: Sparse GCD and inputs where degrees are proportional to the number of variables"
    (define (H nu)
      (+ (expt 'x (+ nu 1))
         (sigma (lambda (i)
                  (expt (symbol 'y i) (+ nu 1)))
                1 nu)
         1))
    (set! F (λ (nu)
              (* (H nu)
                 (- (expt 'x (+ nu 1))
                    (sigma (lambda (i)
                             (expt (symbol 'y i) (+ nu 1)))
                           1 nu)
                    2))))
    (set! G (λ (nu)
              (* (H nu)
                 (+ (expt 'x (+ nu 1))
                    (sigma (lambda (i)
                             (expt (symbol 'y i) (+ nu 1)))
                           1 nu)
                    2))))
    (rkt:time (void (try 8) (try 9) (try 10) (check-true #t))))
   
   (test-case
    "Case 3': Alternatively"
    (define (H nu)
      (+ (expt 'x (+ nu 1))
         (sigma (lambda (i)
                  (expt (symbol 'y i) (+ nu 1)))
                1 nu)
         1))
    (set! F (λ (nu)
              (* (H nu)
                 (- (expt 'x (+ nu 1))
                    (sigma (lambda (i)
                             (expt (symbol 'y i) (+ nu 1)))
                           1 nu)
                    2))))
    (set! G (λ (nu)
              (* (H nu)
                 (+ (expt 'x nu)
                    (sigma (lambda (i)
                             (expt (symbol 'y i) nu))
                           1 nu)
                    2))))
    (rkt:time (void (try 3) (try 4) (try 5) (try 6) (check-true #t))))

   (test-case
    "Case 4: Quadratic non-monic GCD.  F and G have other quadratic factors."
    (define (H nu)
      (+ (* (expt 'y1 2) (expt 'x 2)
            (sigma (lambda (i) (expt (symbol 'y i) 2))
                   1 nu)
            1)))
    (set! F (λ (nu)
              (* (H nu)
                 (+ (expt 'x 2)
                    (* -1 (expt 'y1 2))
                    (sigma (lambda (i) (expt (symbol 'y i) 2))
                           1 nu)
                    -1))))
    (set! G (λ (nu)
              (* (H nu)
                 (square (+ (* 'y1 'x)
                            (sigma (lambda (i) (symbol 'y i))
                                   1 nu)
                            2)))))
    (rkt:time (void (try 5) (try 6) (try 7) (check-true #t))))
   (test-case
    "Case 5: Completely dense non-monic quadratic inputs with dense non-monic linear GCD."
    (define (Pi f lo hi)
      (if (> lo hi)
          1
          (* (f lo) (Pi f (+ lo 1) hi))))
    (define (H nu)
      (- (* (+ 'x 1)
            (Pi (lambda (i) (+ (symbol 'y i) 1))
                1 nu))
         3))
    (set! F (λ (nu)
              (* (H nu)
                 (+ (* (- 'x 2)
                       (Pi (lambda (i) (- (symbol 'y i) 2))
                           1 nu))
                    3))))
    (set! G (λ (nu)
              (* (H nu)
                 (- (* (+ 'x 2)
                       (Pi (lambda (i) (+ (symbol 'y i) 2))
                           1 nu))
                    3))))
    (rkt:time (void (try 4) (try 5) (try 6) (check-true #t))))
   (test-case
    "Case 5': Sparse non-monic quadratic inputs with linear GCDs."
    (define (Pi f lo hi)
      (if (> lo hi)
          1
          (* (f lo) (Pi f (+ lo 1) hi))))
    (define (H nu)
      (- (* 'x
            (Pi (lambda (i) (symbol 'y i))
                1 nu))
         1))
    (set! F (λ (nu)
              (* (H nu)
                 (+ (* 'x
                       (Pi (lambda (i)
                             (symbol 'y i))
                           1 nu))
                    3))))
    (set! G (λ (nu)
              (* (H nu)
                 (- (* 'x
                       (Pi (lambda (i)
                             (symbol 'y i))
                           1 nu))
                    3))))
    (rkt:time (void (try 5) #;(try 100) (try 10) (check-true #t))))
   (test-case
    "Case 6: Trivariate inputs with increasing degrees."
    (define (H j)
      (* (expt 'x j) 'y (- 'z 1)))
    (set! F (λ (j)
              (* (H j)
                 (+ (expt 'x j)
                    (* (expt 'y (+ j 1))
                       (expt 'z j))
                    1))))

    (set! G (λ (j)
              (* (H j)
                 (+ (expt 'x (+ j 1))
                    (* (expt 'y j)
                       (expt 'z (+ j 1)))
                    -7))))
    (rkt:time (void (try 10) (try 30) (check-true #t))))
   (test-case
    "Case 7: Trivariate polynomials whose GCD has common factors with it cofactors"
    (define P (+ 'x (* -1 'y 'z) 1))
    (define Q (+ 'x (* -1 'y) (* 3 'z)))
    (define (H j) (* (expt P j) (expt Q j)))
    (set! F (λ (j k) (* (expt P j) (expt Q k))))
    (set! G (λ (j k) (* (expt P k) (expt Q j))))
    (rkt:time (void (try 1 4) (try 2 4) (try 3 4) (try 4 5) (check-true #t))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))