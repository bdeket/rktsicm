#lang racket/base

(require rackunit
         "../../kernel/modarith.rkt"
         "../../rkt/int.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/modarith"
   (test-case
    "mod:make"
    (check-equal? (modint? (mod:make 3 5)) #t)
    (check-equal? (mod:residue (mod:make 3 5)) 3)
    (check-equal? (mod:modulus (mod:make 3 5)) 5)
    (check-equal? (mod:reduce 3 5) 3))
   (test-case
    "modint:invert"
    (check-equal? (modint:invert 3 5) 2)
    (check-equal? (modint:invert 1 5) 1)
    (skip #;"TODO: next needs better error message")
    (check-exn #px"\\$quotient-remainder: division by zero" (λ () (modint:invert 3 15))))
   (test-case
    "unary-combine / mod:invert"
    (check-exn #px"Not a modular integer" (λ () (mod:invert 5)))
    (check-equal? (mod:invert (mod:make 3 5)) (mod:make 2 5))
    (for* ([i (in-range 10)]
           [n (in-value (+ 5 (random 15)))]
           [p (in-value (+ 8 (random 10)))]
           [M (in-value (mod:make n p))]
           #:unless (= 0 (mod:residue M)))
      (with-handlers ([exn:fail? (λ (e)
                                   (check-not-false (regexp-match #px"\\$quotient-remainder: division by zero"
                                                                  (exn-message e))))])
        (define R (mod:* M (mod:invert M)))
        (check-equal? (mod:residue R) 1)
        (check-equal? (mod:modulus R) p))))
   (test-case
    "binary-combine / +*-/"
    (check-equal? (modint:+ 3 4 5) 2)
    (check-equal? (mod:+ (mod:make 3 5) (mod:make 4 5)) (mod:make 2 5))
    (check-equal? (modint:- 3 4 5) 4)
    (check-equal? (mod:- (mod:make 3 5) (mod:make 4 5)) (mod:make 4 5))
    (check-equal? (modint:* 3 4 5) 2)
    (check-equal? (mod:* (mod:make 3 5) (mod:make 4 5)) (mod:make 2 5))
    (check-equal? (modint:/ 3 4 5) 2)
    (check-equal? (mod:/ (mod:make 3 5) (mod:make 4 5)) (mod:make 2 5))
    (check-equal? (modint:/ 4 3 5) 3)
    (check-equal? (mod:/ (mod:make 4 5) (mod:make 3 5)) (mod:make 3 5))
    (check-equal? (modint:* 3 3 5) 4)
    (check-equal? (mod:* (mod:make 3 5) (mod:make 3 5)) (mod:make 4 5))
    (check-exn #px": Not same modulus"
               (λ () (mod:* (mod:make 3 5) (mod:make 3 6))))
    (check-exn #px": Not modular integers"
               (λ () (mod:* 3 (mod:make 3 6)))))
   (test-case
    "expt"
    (check-equal? (modint:expt 3 0 15)  1)
    (check-equal? (modint:expt 3 1 15)  3)
    (check-equal? (modint:expt 3 2 15)  9)
    (check-equal? (modint:expt 3 3 15) 12)
    (check-equal? (modint:expt 3 4 15)  6)
    (check-equal? (mod:expt (mod:make 3 15) (mod:make 4 15)) (mod:make 6 15)))
   (test-case
    "="
    (check-exn #px": Not modular integers -- =" (λ () (mod:= 3 (mod:make 3 15))))
    (check-exn #px": Not same modulus -- =" (λ () (mod:= (mod:make 3 16) (mod:make 3 15))))
    (check-equal? (mod:= (mod:make 3 15) (mod:make 18 15)) #t)
    (check-equal? (mod:= (mod:make 3 15) (mod:make 4 15)) #f))
   (test-case
    "linear-solve"
    (modarith:assign-operations)
    (local-require "../../kernel/generic.rkt")
    (check-equal? (g:solve-linear-left (mod:make 3 5) (mod:make 4 5)) (mod:make 3 5))
    (check-equal? (g:solve-linear (mod:make 3 5) (mod:make 4 5)) (mod:make 3 5)))
   (test-case "ORIG:testinv"
              (define (testinv n p)
                (= 1 (modint:* n (modint:invert n p) p)))

              (check-equal? (testinv 3 5)
                            #t)
              )

   (test-case "ORIG:chinese-remainder"
              (define a1 (mod:make 2 5))
              (define a2 (mod:make 3 13))

              (check-equal?
               (mod:chinese-remainder a1 a2)
               42)
              )

   (test-case "ORIG:othr"
              (define (test p)
                (let jlp ((j (- p)))
                  (cond ((int:= j p) 'ok)
                        (else
                         (let ilp ((i (- p)))
                           ;;(write-line `(trying ,i ,j)) 
                           (cond ((int:= i p) (jlp (int:+ j 1)))
                                 ((int:= (modulo i p) 0) (ilp (int:+ i 1)))
                                 (else
                                  (let ((jp (mod:make j p))
                                        (ip (mod:make i p)))
                                    (let ((b (mod:/ jp ip)))
                                      (if (mod:= (mod:* b ip) jp)
                                          (ilp (int:+ i 1))
                                          (begin (println `(problem dividing ,j ,i))
                                                 (println `((/ ,jp ,ip) =  ,(mod:/ jp ip)))
                                                 (println `((* ,b ,ip) = ,(mod:* b ip))))))))))))))

              (check-equal? (test 47)
                            'ok))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))