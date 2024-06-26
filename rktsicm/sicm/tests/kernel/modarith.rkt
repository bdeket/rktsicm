#lang racket/base

(require rackunit
         "../../main.rkt"
         "../../rkt/int.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/modarith"
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