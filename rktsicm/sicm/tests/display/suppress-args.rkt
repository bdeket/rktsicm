#lang racket/base

(require rackunit
         "../../display/suppress-args.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "display/suppress-args"
   (test-case
    "add-arguments"
    (check-equal? (suppress-arguments '((up t
                                            (up r s)
                                            (down (* -1 (((partial 2 0) F1) t (up x y) (up r s)))
                                                  (* -1 (((partial 2 1) F1) t (up x y) (up r s)))))))
                  1)
    (check-equal? (suppress-arguments '(t (up x y) (up r s))) 2)
    (check-equal? (show-suppressed-arguments)
                  '((args.2 = t (up x y) (up r s))
                    (args.1 = (up t
                                  (up r s)
                                  (down (* -1 (((partial 2 0) F1) t (up x y) (up r s)))
                                        (* -1 (((partial 2 1) F1) t (up x y) (up r s)))))))))
   (test-case
    "replace-arguments"
    (void (clear-arguments)
          (suppress-arguments '(x)))
    (check-equal? (arg-suppressor '(+ (f x) 4)) '(+ (f args.1) 4))
    (check-equal? (arg-suppressor+ '(+ (f x) 4)) '(+ f 4)))
   (test-case
    "rename-arguments"
    (rename-part '(partial 0) 'D)
    (check-equal? (rename-expression '((partial 0) f)) '(D f)))))


(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))