#lang racket/base

(require (only-in racket/syntax format-id)
         (only-in racket/list last take)
         "arity.rkt")

(provide make-plain-procedure make-plain-procedure-stx)

#; ;waaaay to slow
(define (rest-app f . rst)
  (eval-syntax
   (with-syntax* ([(x ... Y) rst]
                  [(y ...) (syntax->list #'Y)])
     #`(#,f x ... y ...))))

;;procedure-reduce-arity(-mask) is 10x slower for exact arities and 2x slower for arity-at-least?s
;;#%plain-app + apply is 3x slower than just apply
;;putting f inside an extra function call makes it 5x slower
;;outside the extra function call f and apply f are same speed
;;inside function call (apply f ...) is a lot slower (depending on arguments provided)

(define (make-plain-procedure f A)
  (define arity (if (procedure-arity? A)
                    (normalize-arity A)
                    (raise-argument-error 'make-plain-procedure "procedure-arity?" arity)))
  (define stx
    (cond
      [(exactly-n? arity)
       (with-syntax ([(x ...) (build-list arity
                                          (λ (i) (format-id #f "x~a" i)))])
         #`(λ (x ...)
             (#,f x ...)))]
      [(arity-at-least? arity)
       (with-syntax ([(x ...) (build-list (arity-at-least-value arity)
                                          (λ (i) (format-id #f "x~a" i)))])
         #`(λ (x ... . y)
             (apply #,f x ... y)))]
      [else
       (define END (last arity))
       (define LEN (length arity))
       (cond
         [(number? END)
          (with-syntax ([((Xs ...)...)
                         (map (λ (x) (build-list x (λ (i) (format-id #f "x~a" i))))
                              arity)])
            #`(case-lambda [(Xs ...) (#%plain-app #,f Xs ...)]...))]
         [else
          (with-syntax ([((Xs ...)...)
                         (map (λ (x) (build-list x (λ (i) (format-id #f "x~a" i))))
                              (take arity (- LEN 1)))]
                        [(Ys ...) (build-list (arity-at-least-value END)
                                              (λ (i) (format-id #f "x~a" i)))])
            #`(case-lambda [(Xs ...) (#%plain-app #,f Xs ...)]...
                           [(Ys ... . y) (apply #,f Ys ... y)]))])]))
  ;(println stx)
  (eval-syntax stx))

(define (make-plain-procedure-stx f A)
  (define arity (if (procedure-arity? A)
                    (normalize-arity A)
                    (raise-argument-error 'make-generic-operator "procedure-arity?" arity)))
  (define stx
    (cond
      [(exactly-n? arity)
       (with-syntax ([(x ...) (build-list arity
                                          (λ (i) (format-id #f "x~a" i)))])
         #`(λ (x ...)
             #,(f #'(x ...) #f)))]
      [(arity-at-least? arity)
       (with-syntax ([(x ...) (build-list (arity-at-least-value arity)
                                          (λ (i) (format-id #f "x~a" i)))]
                     [y (format-id #f "y")])
         #`(λ (x ... . y)
             #,(f #'(x ...) #'y)))]
      [else
       (define END (last arity))
       (define LEN (length arity))
       (cond
         [(number? END)
          (with-syntax ([((Xs ...)...)
                         (map (λ (x) (build-list x (λ (i) (format-id #f "x~a" i))))
                              arity)])
            #`(case-lambda #,@(map (λ (xs) #`[#,xs #,(f xs #f)]) (syntax->list #'((Xs ...) ...)))))]
         [else
          (with-syntax ([((Xs ...)...)
                         (map (λ (x) (build-list x (λ (i) (format-id #f "x~a" i))))
                              (take arity (- LEN 1)))]
                        [(Ys ...) (build-list (arity-at-least-value END)
                                              (λ (i) (format-id #f "x~a" i)))]
                        [y (format-id #f "y")])
            #`(case-lambda #,@(map (λ (xs) #`[#,xs #,(f xs #f)]) (syntax->list #'((Xs ...) ...)))
                           [(Ys ... . y) #,(f #'(Ys ...) #'y)]))])]))
  ;(println stx)
  (eval-syntax stx))


#;
(module+ main
  (define-syntax-rule (test [m n] fct)
    (begin
      (displayln 'fct) 
      (for ([i (in-range m)])
        (collect-garbage)(collect-garbage)(collect-garbage)
        (time
         (for ([i (in-range n)])
           fct)))))
  (define M 3)
  (define N 10000000)

  (define (f1 x y) (+ x y))
  (define (f2 x y) (* x y))
  (define (g1 x y . z) (apply + x y z))
  (define (g2 x y . z) (apply * x y z))
  (define k1 (case-lambda [(x) (- x)]
                          [(x y . z) (- x (apply + y z))]))
  (define k2 (case-lambda [(x) (/ x)]
                          [(x y . z) (/ x (apply * y z))]))

  (define (o1 f1 f2) (make-plain-procedure (λ x (+ (apply f1 x) (apply f2 x)))
                                           (procedure-arity f1)))
  (define (o2 f1 f2) (make-plain-procedure-stx (λ (x y)
                                                 (if y
                                                     #`(#,+ (apply #,f1 #,@x #,y)
                                                            (apply #,f2 #,@x #,y))
                                                     #`(#,+ (#,f1 #,@x) (#,f2 #,@x))))
                                               (procedure-arity f1)))
  (define (o3 f1 f2) (procedure-reduce-arity (λ x (+ (apply f1 x) (apply f2 x)))
                                             (procedure-arity f1)))
  
  
  (define F1 (o1 f1 f2))
  (define F2 (o2 f1 f2))
  (define F3 (o3 f1 f2))
  (define G1 (o1 g1 g2))
  (define G2 (o2 g1 g2))
  (define G3 (o3 g1 g2))
  (define K1 (o1 k1 k2))
  (define K2 (o2 k1 k2))
  (define K3 (o3 k1 k2))

  (F1 1 2)
  (F2 1 2)
  (G1 1 2)
  (G2 1 2)
  (G1 1 2 3 4)
  (G2 1 2 3 4)
  (K1 2)
  (K2 2)
  (K1 2 2 1 2)
  (K2 2 2 1 2)

  (test [M N] (F1 1 2))
  (test [M N] (F2 1 2))
  (test [M N] (F3 1 2))

  (test [M N] (G1 1 2))
  (test [M N] (G2 1 2))
  (test [M N] (G3 1 2))
  (test [M N] (G1 1 2 0 0))
  (test [M N] (G2 1 2 0 0))
  (test [M N] (G3 1 2 0 0))

  (test [M N] (K1 2))
  (test [M N] (K2 2))
  (test [M N] (K3 2))
  (test [M N] (K1 2 1 1 1))
  (test [M N] (K2 2 1 1 1))
  (test [M N] (K3 2 1 1 1))
)