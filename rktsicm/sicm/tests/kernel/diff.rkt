#lang racket/base

(require rackunit
         racket/stream
         racket/port
         racket/logging
         "../../kernel-intr.rkt"
         "../../kernel/diff.rkt"
         "../../rkt/applyhook.rkt"
         "../../rkt/racket-help.rkt"
         "../helper.rkt"
         )
(kernel:assign-operations #t)

(define-syntax-rule (MDT (tt tc) ...) (make-differential-quantity (list (make-differential-term `tt `tc) ...)))
(define-syntax-rule (eMDT (tt tc) ...) (expression (MDT (tt tc) ...)))

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/diff"
   ;; ==== cstm ====
   ;; a tag is created for each differential, so dx would be 0, dy 1, dz 2 etc
   ;; a taglist '(0 0) means dx^2, and taglist should be 'sorted <'
   ;; the terms should never have a coefficient that is 0
   (test-case
    "creation / access"
    (define d (make-differential-quantity (list (make-differential-term '(0 0) 1)
                                                (make-differential-term '(1 0) 2)
                                                (make-differential-term '(0 1) 3))))
    (check-true (differential? d))
    (define t0 (car (differential-term-list d)))
    (check-equal? (differential-tags t0) '(0 0))
    (check-equal? (differential-coefficient t0) 1)
    (check-equal? (differential-of d) 1)
    (check-equal? (terms->differential (differential->terms d)) d)
    (check-equal? (terms->differential '()) 0)
    (check-equal? (differential->terms 0) '())
    (check-equal? (differential->terms (MDT ((0) 0))) '())
    (check-equal? (differential->terms 2) '((() 2)))
    (check-equal? (terms->differential '((() 2))) 2))
   ;; ==== main ====
   (test-case
    "diff-colapse"
    (check-equal? (terms->differential-collapse (list (make-differential-term '(0 0) 1)
                                                      (make-differential-term '(1 0) 2)
                                                      (make-differential-term '(0 1) 3)
                                                      (make-differential-term '(1 0) 5)))
                  (MDT ((0 0) 1) ((1 0) 7) ((0 1) 3))))
   (test-case
    "arity"
    (check-equal? (diff:arity (MDT ((0 0) 1) ((1 0) 7) ((0 1) 3)))
                  (arity-at-least 0))
    (check-equal? (diff:arity (MDT ((0 0) ,(λ (x y) 0)) ((1 0) ,(λ (x y) x)) ((0 1) ,(λ (x y) y))))
                  2))
   (test-case
    "apply"
    (check-equal? (diff:apply (MDT ((0 0) ,(λ (x y) 0)) ((1 0) ,(λ (x y) x)) ((0 1) ,(λ (x y) y)))
                              '(a b))
                  (MDT ((0 0) 0) ((1 0) a) ((0 1) b))))
   (test-case
    "dtl+"
    (define t0 (list (make-differential-term '(0) 1)
                     (make-differential-term '(1) 2)
                     (make-differential-term '(2) -11)
                     (make-differential-term '(0 1) 3)))
    (define t1 (list (make-differential-term '(0) 5)
                     (make-differential-term '(2) 11)
                     (make-differential-term '(0 1) 7)))
    (define tr (list (make-differential-term '(0) 6)
                     (make-differential-term '(1) 2)
                     (make-differential-term '(0 1) 10)))
    (check-equal? (dtl:+ t0 t1) tr)
    (check-equal? (d:+ (terms->differential t0) (terms->differential t1))
                  (terms->differential tr)))
   (test-case
    "dtl*"
    (define t0 (list (make-differential-term '(0) 1)
                     (make-differential-term '(1) 2)
                     (make-differential-term '(0 1) 3)))
    (define t1 (list (make-differential-term '(0) 5)
                     (make-differential-term '(2) 11)
                     (make-differential-term '(0 1) 7)))
    (define tr (list (make-differential-term '(0 1) 10)
                     (make-differential-term '(0 2) 11)
                     (make-differential-term '(1 2) 22)
                     (make-differential-term '(0 1 2) 33)))
    (check-equal? (dtl:* t0 t1) tr)
    (check-equal? (d:* (terms->differential t0) (terms->differential t1))
                  (terms->differential tr)))
   (test-case
    "diff-tags"
    (check-false (equal? (make-differential-tag) (make-differential-tag)))
    (check-true (same-differential-tags? (make-differential-term '(0 1 2) 3)
                                         (make-differential-term (list 0 1 2) 4)))
    (check-false (same-differential-tags? (make-differential-term '(0 1) 3)
                                          (make-differential-term (list 0 1 2) 3)))
    (check-true (<differential-tags? (make-differential-term '(0 1) 5)
                                     (make-differential-term (list 0 1 2) 4)))
    (check-true (<differential-tags? (make-differential-term '(0 1) 5)
                                     (make-differential-term (list 1 2) 4)))
    (check-false (<differential-tags? (make-differential-term (list 0 1 2) 4)
                                     (make-differential-term '(0 1) 5)))
    (check-false (<differential-tags? (make-differential-term (list 1 2) 4)
                                      (make-differential-term '(0 1) 5))))
   (test-case
    "diff-tag set operations"
    (check-equal? (union-differential-tags '(0 1 2) '(1 3 4))
                  '(0 1 2 3 4))
    (check-equal? (intersect-differential-tags '(0 1 2) '(1 3 4))
                  '(1)))
   (test-case
    "unary / binary op"
    (define D1 (terms->differential (list (make-differential-term '() 2/3)
                                          (make-differential-term '(0) 5/7))))
    (define D2 (MDT (() 0)))
    (define D3 (MDT (() 3) ((0) 0) ((1) 0)))
    (check-equal? (finite-part 4) 4)
    (check-equal? (finite-part D1) 2/3)
    (check-equal? (finite-part D2) 0)
    (check-equal? (finite-part D3) 3)
    (check-equal? (infinitesimal-part 4) 0)
    (check-equal? (infinitesimal-part D1) (MDT ((0) 5/7)))
    (check-equal? (infinitesimal-part D2) 0)
    (check-equal? (infinitesimal-part D3) 0)
    (check-equal? ((diff:unary-op (λ (x) (g:* x x x)) (λ (x) (g:* 3 x x))) D1)
                  (MDT (() 8/27) ((0) 20/21)))
    (check-equal? ((diff:unary-op (λ (x) (g:* x x x)) (λ (x) (g:* 3 x x))) 5) 125)

    (define D4 (MDT (() 2/3) ((1) 11/5)))

    (check-equal? (max-order-tag D1 D4) 1)
    (check-equal? (max-order-tag 5 7) '())
    (check-equal? (with-tag D1 0) (MDT ((0) 5/7)))
    (check-equal? (without-tag D1 0) 2/3)
    (check-equal? ((diff:binary-op (λ (x y) (g:* x x y)) (λ (x y) (g:* 2 x y)) (λ (x y) (g:* x x))) D1 D4)
                  (MDT (() 8/27) ((0) 40/63) ((1) 44/45) ((0 1) 44/21))))
   (test-case
    "diff:_"
    (define X (MDT (() x) ((0) dx)))
    (define Y (MDT (() y) ((1) dy)))
    (check-equal? (expression (diff:+ X Y))
                  (eMDT (() (+ x y))
                        ((0) dx)
                        ((1) dy)))
    (check-equal? (expression (diff:- X Y))
                  (eMDT (() (- x y))
                        ((0) dx)
                        ((1) (* -1 dy))))
    (check-equal? (expression (diff:* X Y))
                  (eMDT (() (* x y))
                        ((0) (* dx y))
                        ((1) (* x dy))
                        ((0 1) (* dx dy))))
    (check-equal? (expression (diff:/ X Y))
                  (eMDT (() (/ x y))
                        ((0) (/ dx y))
                        ((1) (/ (* (* -1 x) dy) (* y y)))
                        ((0 1) (/ (* (* -1 dx) dy) (* y y)))))
    (check-equal? (expression (diff:identity X)) (expression X))
    (check-equal? (expression (diff:negate X)) (expression (g:- X)))
    (check-equal? (expression (diff:invert X))
                  (eMDT (() (/ 1 x))
                        ((0) (/ (* -1 dx) (* x x)))))
    (check-equal? (expression (diff:sqrt X))
                  (eMDT (() (sqrt x))
                        ((0) (/ dx (* 2 (sqrt x))))))
    (check-equal? (expression (diff:power X 3))
                  (eMDT (() (expt x 3))
                        ((0) (* 3 dx (expt x 2)))))
    (check-exn #px"Should not get here: DIFF:POWER" (λ () (diff:power X Y)))
    (check-equal? (expression (diff:square X))
                  (eMDT (() (* x x))
                        ((0) (* 2 x dx))))
    (check-equal? (expression (diff:expt X Y))
                  (eMDT (() (expt x y))
                        ((0) (* dx y (expt x (+ -1 y))))
                        ((1) (* (log x) (expt x y) dy))
                        ((0 1) (* (+ (/ (* dx (expt x y)) x) (* (log x) dx y (expt x (+ -1 y)))) dy))))
    (check-equal? (expression (diff:expt 0 (MDT (() 2) ((1) dy)))) 0)
    (check-equal? (expression (diff:expt Y X))
                  (eMDT (() (expt y x))
                        ((0) (* (log y) (expt y x) dx))
                        ((1) (* dy x (expt y (+ -1 x))))
                        ((0 1) (* dy (+ (* dx (expt y (+ -1 x))) (* x (log y) (expt y (+ -1 x)) dx))))))
    
    ;; more expt !!
    (check-equal? (expression (diff:exp X))
                  (eMDT (() (exp x))
                        ((0) (* (exp x) dx))))
    (check-equal? (expression (diff:log X))
                  (eMDT (() (log x))
                        ((0) (/ dx x))))
    (check-equal? (expression (diff:sin X))
                  (eMDT (() (sin x))
                        ((0) (* (cos x) dx))))
    (check-equal? (expression (diff:cos X))
                  (eMDT (() (cos x))
                        ((0) (* -1 (sin x) dx))))
    (check-equal? (expression (diff:asin X))
                  (eMDT (() (asin x))
                        ((0) (/ dx (sqrt (- 1 (* x x)))))))
    (check-equal? (expression (diff:acos X))
                  (eMDT (() (acos x))
                        ((0) (/ (* -1 dx) (sqrt (- 1 (* x x)))))))
    (check-equal? (expression (diff:atan1 X))
                  (eMDT (() (atan x))
                        ((0) (/ dx (+ 1 (* x x))))))
    (check-equal? (expression (diff:atan2 X Y))
                  (eMDT (() (atan x y))
                        ((0) (/ (* dx y) (+ (* y y) (* x x))))
                        ((1) (/ (* (* -1 x) dy) (+ (* y y) (* x x))))
                        ((0 1) (* (+ (/ (* -1 dx) (+ (* y y) (* x x)))
                                     (/ (* 2 (* -1 (* -1 x)) x dx)
                                        (* (+ (* y y) (* x x)) (+ (* y y) (* x x)))))
                                  dy))))
    (check-equal? (expression (diff:sinh X))
                  (eMDT (() (sinh x))
                       ((0) (* (cosh x) dx))))
    (check-equal? (expression (diff:cosh X))
                  (eMDT (() (cosh x))
                       ((0) (* (sinh x) dx))))
    (check-equal? (diff:abs (MDT (() 1) ((0) 1)))
                  (MDT (() 1) ((0) 1)))
    (check-equal? (diff:abs (MDT (() -1) ((0) 1)))
                  (MDT (() 1) ((0) -1)))
    (check-exn #px"Derivative of ABS undefined at zero"
               (λ ()(diff:abs (MDT (() 0) ((0) 1)))))
    (check-exn #px"Derivative of ABS at"
               (λ ()(diff:abs X))))
   (test-case
    "diff:type etc"
    (define X (MDT (() x) ((0) dx)))
    (define Y (MDT (() y) ((1) dy)))
    (check-equal? (diff:type X) differential-type-tag)
    (check-equal? (diff:type-predicate X) differential?)
    (check-true ((diff:type-predicate X) Y))
    (check-equal? (diff:zero-like X) 0)
    (check-equal? (diff:one-like X) 1)
    (check-true (diff:zero? (make-differential-quantity '())))
    (check-true (diff:zero? (MDT (() 0) ((0) 0))))
    (check-false (diff:zero? (MDT (() 0) ((0) 1))))
    (check-true (diff:one? (MDT (() 1))))
    (check-true (diff:one? (MDT (() 1) ((0) 0))))
    (check-false (diff:one? (MDT ((0) 1) ((1) 0))))
    (check-false (diff:one? (MDT (() 1) ((0) 1))))
    (check-true ((diff:binary-comparator <) (MDT (() 1) ((0) 2))
                                            (MDT (() 2) ((1) 1))))
    (check-true ((diff:binary-comparator <) 1 2))
    (check-false ((diff:binary-comparator <) (MDT (() 2) ((0) 1))
                                             (MDT (() 2) ((1) 2)))))
   (test-case
    "solve"
    (define X (MDT (() x) ((0) dx)))
    (define Y (MDT (() y) ((1) dy)))
    (check-equal? (expression (g:solve-linear-left Y X))
                  (eMDT (() (/ x y))
                        ((0) (/ dx y))
                        ((1) (/ (* (* -1 x) dy) (* y y)))
                        ((0 1) (/ (* (* -1 dx) dy) (* y y)))))
    (check-equal? (expression (g:solve-linear-left 3 X))
                  (eMDT (() (/ x 3))
                        ((0) (* 1/3 dx))))
    (check-equal? (expression (g:solve-linear Y X))
                  (eMDT (() (/ x y))
                        ((0) (/ dx y))
                        ((1) (/ (* (* -1 x) dy) (* y y)))
                        ((0 1) (/ (* (* -1 dx) dy) (* y y)))))
    (check-equal? (expression (g:solve-linear 3 X))
                  (eMDT (() (/ x 3))
                        ((0) (* 1/3 dx)))))
   (test-case
    "derivative"
    (define F (λ (x) (g:* (g:+ x (g:sin x)) x)))
    (check-equal? (expression ((diff:derivative F) 't))
                  '(+ (* (+ 1 (cos t)) t) t (sin t)))
    (check-= ((diff:derivative F) 3)
             3.171142518258531 1e-15)
    (check-equal? (expression (simple-derivative-internal F 't))
                  '(+ (* (+ 1 (cos t)) t) t (sin t)))
    (check-= (simple-derivative-internal F 3) 3.171142518258531 1e-15)
    (check-equal? (make-x+dx 'x 0)
                  (MDT (() x) ((0) 1)))
    (let ([c (+ 1 (make-differential-tag))])
      (check-equal? (differential-object 'x)
                    (list (MDT (() x) ((,c) 1))
                          c)))
    )
   (test-case
    "dx-helpers 1"
    (check-equal? (*active-tags*) '())
    (check-not-false (with-active-tag 3 tag-active? '(3)))
    (check-false (tag-active? 3))
    (check-false (with-active-tag 3 tag-active? '(2)))
    (check-equal? (remove-differential-tag 1 '(0 1 2 1)) '(0 2))
    (check-equal? (remove-differential-tag 1 '(1)) '())
    (check-equal? (insert-differential-tag 0 '()) '(0))
    (check-equal? (insert-differential-tag 1 '(0 2)) '(0 1 2))
    (check-equal? (insert-differential-tag 2 '(0 1)) '(0 1 2))
    (check-exn #px"INSERT-DIFFERENTIAL-TAGS:" (λ () (insert-differential-tag 0 '(0)))))
   (test-case
    "extract-dx-diff/part"
    (check-equal? (extract-dx-differential 0 'any) 0)
    (check-equal? (call-with-output-string
                   (λ (out)
                     (with-logging-to-port out
                                           (λ () (with-active-tag 0 extract-dx-differential '(0 any)))
                                           #:logger rktsicm-logger
                                           'debug)))
                  "rktsicm: Entering Radul Territory\nrktsicm:  (0)\n")
    (check-equal? (extract-dx-differential 0 (MDT (() 1))) 0)
    (check-equal? (extract-dx-differential 0 (MDT (() 1) ((1) 1))) 0)
    (check-equal? (extract-dx-differential 0 (MDT (() 1) ((0) 1))) 1)
    (check-equal? (extract-dx-differential 0 (MDT (() 1) ((0) 1) ((0 1) 2)))
                  (MDT (() 1) ((1) 2)))

    (check-equal? (extract-dx-part 0 (up (MDT ((0) 2)))) (up 2))
    (check-equal? (extract-dx-part 0 (down (MDT ((0) 2)))) (down 2))
    (check-equal? (extract-dx-part 0 (matrix-by-rows (list (MDT ((0) 2))))) (matrix-by-rows '(2)))
    (check-equal? (extract-dx-part 0 (quaternion (MDT ((0) 2))
                                                 (MDT ((0 1) 2))
                                                 (MDT ((1) 2))
                                                 (MDT (() 2))))
                  (quaternion 2 (MDT ((1) 2)) 0 0))
    ;; function => extract-dx-function
    ;; operator => extract-dx-operator => extract-dex-function
    ;; series
    (check-equal? (g:ref (extract-dx-part 0 (series (MDT ((0) 2)))) 0) 2)
    ;; else
    (check-equal? (extract-dx-part 0 (MDT (() 1) ((0) 1) ((0 1) 2)))
                  (MDT (() 1) ((1) 2)))

    (define A (MDT ((1) 1) ((2) 2) ((1 2) 3) ((1 2 3) 4)))
    (define B (MDT ((3) 1) ((2) 2) ((2 3) 3) ((2) 0)))
    (check-equal? ((replace-differential-tag 1 3) A) B)
    (check-equal? ((replace-differential-tag 1 3) (up A)) (up B))
    (check-equal? ((replace-differential-tag 1 3) (matrix-by-rows (list A))) (matrix-by-rows (list B)))
    (check-equal? ((replace-differential-tag 1 3) (quaternion (MDT ((1) 1))
                                                              (MDT ((2) 2))
                                                              (MDT ((1 2) 3))
                                                              (MDT ((1 2 3) 4))))
                  (quaternion (MDT ((3) 1)) (MDT ((2) 2)) (MDT ((2 3) 3)) (MDT ((2) 0))))
    (check-equal? (g:ref ((replace-differential-tag 1 3) (series (MDT ((1) 1))
                                                                 (MDT ((2) 2))
                                                                 (MDT ((1 2) 3))
                                                                 (MDT ((1 2 3) 4)))) 0)
                  (MDT ((3) 1)))
    ;; series of fun => replace-dx-fun
    ;; series of op => replace-dx-op
    (check-equal? ((replace-differential-tag 1 3) 'any) 'any)
    
    
    
    
    extract-dx-operator
    
    replace-dx-function
    replace-dx-operator
    )
   (test-case
    "extract-dx-function/op"
    (check-equal? ((extract-dx-function 0 (λ (x) x)) (MDT ((1) 1) ((0) 2))) 2)
    (check-equal? (accumulate acc ((extract-dx-function 0 (λ (x) (acc (*active-tags*)) x))
                                   (MDT ((1) 1) ((0) 2))))
                  ;; acc called once with active tags '(0)
                  '((0)))
    (let ([c (+ 1 (make-differential-tag))])
      (check-equal? (accumulate acc (with-active-tag 0
                                                     (λ () ((extract-dx-function 0 (λ (x) (acc x) (acc (*active-tags*)) x))
                                                            (MDT ((1) 1) ((0) 2))))
                                                     '()))
                    ;; within PROC all tags are replaced by new tag, and tag is added to the active tag list
                    (list (MDT ((1) 1) ((,c) 2))
                          '(0 0))))
    (check-equal? (with-active-tag 0 (λ () ((extract-dx-function 0 (λ (x) (d:+ x (MDT ((0) 3)))))
                                            (MDT ((1) 1) ((0) 2))))
                                   '())
                  3)
    (check-equal? (with-active-tag 0 (λ () ((extract-dx-part 0 (λ (x) (d:+ x (MDT ((0) 3)))))
                                            (MDT ((1) 1) ((0) 2))))
                                   '())
                  3)

    (check-equal? ((extract-dx-operator 0 o:identity) (MDT ((1) 1) ((0) 2))) 2)
    (check-equal? ((extract-dx-part 0 o:identity) (MDT ((1) 1) ((0) 2))) 2)
    (check-true (operator? (extract-dx-operator 0 o:identity)))
    (check-equal? (apply-hook-extra (extract-dx-operator 0 o:identity))
                  (apply-hook-extra o:identity)))
   (test-case
    "replace-dx-fun/op"
    ;; !!! just to confuse, replace-dx-funtion & replace-dx-operator reverse the order of
    ;; oldtag and newtag, compared with replace-differential-tag
    ;; (replace-dx-function newtag oldtag) vs (replace-differential-tag oldtag newtag)

    (define (F o) (λ (x) (d:+ x (MDT ((,o) 5)))))
    
    (let ([c (+ 1 (make-differential-tag))]
          [n 3] [o 0])
      ;; not realistic since c should be a new tag:
      (check-equal? (((replace-dx-function n o) (λ (x) x)) (MDT ((,o) 2) ((1) 1) ((,n) 3) ((,c) 4)))
                    (MDT ((,o) 2) ((1) 1) ((,n) 3) ((,o) 4))))
    (let ([c (+ 1 (make-differential-tag))]
          [n 3] [o 0])
      (check-equal? (((replace-dx-function n o) (F o)) (MDT ((,o) 2) ((1) 1) ((,n) 3)))
                    (MDT ((,n) 5) ((,o) 2) ((1) 1) ((,n) 3))))
    (let ([c (+ 1 (make-differential-tag))]
          [n 3] [o 0])
      (check-equal? (accumulate acc (((replace-dx-function n o) (λ (x) (acc x) x))
                                     (MDT ((,o) 2) ((1) 1) ((,n) 3))))
                    ;; acc called once with active tags '(0)
                    (list (MDT ((,c) 2) ((1) 1) ((,n) 3)))))

    (check-equal? (((replace-dx-operator 3 0) o:identity) (MDT ((0) 2) ((1) 1) ((3) 3)))
                  (MDT ((0) 2) ((1) 1) ((3) 3)))
    (check-equal? ((((replace-dx-operator 3 0) o:identity) (F 0))
                   (MDT ((0) 2) ((1) 1) ((3) 3)))
                  (MDT ((0) 5) ((0) 2) ((1) 1) ((3) 3)))
    (check-equal? ((((replace-dx-operator 3 0)
                     (make-operator (λ (f) (λ (x) (f (d:+ x (MDT ((0) 7))))))))
                    (F 0))
                   (MDT ((0) 2) ((1) 1)))
                  ;; Δ of operater is changed, original, and added by F are left alone
                  (MDT ((0) 5) ((3) 7) ((0) 2) ((1) 1)))
    (check-true (operator? ((replace-dx-operator 3 0) o:identity)))
    (check-equal? (apply-hook-extra ((replace-dx-operator 3 0) o:identity))
                  (apply-hook-extra o:identity))

    (check-equal? (g:ref (g:apply ((replace-differential-tag 0 3) (series (F 0)))
                                  (list (MDT ((0) 2) ((1) 1))))
                         0)
                  (MDT ((3) 5) ((0) 2) ((1) 1)))
    (check-equal? (g:ref (g:apply (g:apply ((replace-differential-tag 0 3)
                                            (series (make-operator (λ (f) (λ (x) (f (d:+ x (MDT ((0) 7)))))))))
                                           (list (F 0)))
                                  (list (MDT ((0) 2) ((1) 1))))
                         0)
                  (MDT ((0) 5) ((3) 7) ((0) 2) ((1) 1))))
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))