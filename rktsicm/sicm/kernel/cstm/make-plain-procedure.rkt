#lang racket/base

(require (for-syntax racket/base
                     (only-in racket/syntax format-id))
         (only-in racket/syntax format-id)
         (only-in racket/list last take)
         "arity.rkt")

(provide make-plain-procedure make-plain-procedure-stx make-plain-procedure-slct make-plain-procedure-slct+
         (for-syntax λ quasisyntax unsyntax-splicing unsyntax))

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

;; slow construction, slow execution
(define (make-plain-procedure name f A)
  (define arity (if (procedure-arity? A)
                    (normalize-arity A)
                    (raise-argument-error name "procedure-arity?" A)))
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

;; slow construction, fast execution
(define (make-plain-procedure-stx name o r A)
  (define arity (if (procedure-arity? A)
                    (normalize-arity A)
                    (raise-argument-error name "procedure-arity?" A)))
  (define stx
    (cond
      [(exactly-n? arity)
       (with-syntax ([(x ...) (build-list arity
                                          (λ (i) (format-id #f "x~a" i)))])
         #`(λ (x ...)
             #,(o #'(x ...))))]
      [(arity-at-least? arity)
       (with-syntax ([(x ...) (build-list (arity-at-least-value arity)
                                          (λ (i) (format-id #f "x~a" i)))]
                     [y (format-id #f "y")])
         #`(λ (x ... . y)
             #,(r #'(x ...) #'y)))]
      [else
       (define END (last arity))
       (define LEN (length arity))
       (cond
         [(exactly-n? END)
          (with-syntax ([((Xs ...)...)
                         (map (λ (x) (build-list x (λ (i) (format-id #f "x~a" i))))
                              arity)])
            #`(case-lambda #,@(map (λ (xs) #`[#,xs #,(o xs)]) (syntax->list #'((Xs ...) ...)))))]
         [else
          (with-syntax ([((Xs ...)...)
                         (map (λ (x) (build-list x (λ (i) (format-id #f "x~a" i))))
                              (take arity (- LEN 1)))]
                        [(Ys ...) (build-list (arity-at-least-value END)
                                              (λ (i) (format-id #f "x~a" i)))]
                        [y (format-id #f "y")])
            #`(case-lambda #,@(map (λ (xs) #`[#,xs #,(o xs)]) (syntax->list #'((Xs ...) ...)))
                           [(Ys ... . y) #,(r #'(Ys ...) #'y)]))])]))
  ;(println stx)
  (eval-syntax stx))

;; fast construction, slow execution
(define (plain-procedure-slct name f A)
  (define arity (if (procedure-arity? A)
                    (normalize-arity A)
                    (raise-argument-error name "procedure-arity?" arity)))
  (cond
    [(equal? arity *exactly-zero*)   (λ () (f))]
    [(equal? arity *exactly-one*)    (λ (x) (f x))]
    [(equal? arity *exactly-two*)    (λ (x1 x2) (f x1 x2))]
    [(equal? arity *exactly-three*)  (λ (x1 x2 x3) (f x1 x2 x3))]
    [(equal? arity *at-least-three*) (λ (x1 x2 x3 . r) (apply f x1 x2 x3 r))]
    [(equal? arity *at-least-two*)   (λ (x1 x2 . r) (apply f x1 x2 r))]
    [(equal? arity *at-least-one*)   (λ (x1 . r) (apply f x1 r))]
    [(equal? arity *at-least-zero*)  (λ r (apply f r))]
    [(equal? arity *one-or-two*)     (case-lambda [(x) (f x)]
                                                  [(x y) (f x y)])]
    [else                            (make-plain-procedure f arity)]))

(define (make-plain-procedure-slct+ name wrp)
  (define (stx f e arity)
    #`(cond
        [(equal? #,arity *exactly-zero*)
         #,(with-syntax ([(x ...) (build-list 0 (λ (i) (format-id #f "x~a" i)))])
           #`(λ (x ...) #,(f #'(x ...) #f)))]
        [(equal? #,arity *exactly-one*)
         #,(with-syntax ([(x ...) (build-list 1 (λ (i) (format-id #f "x~a" i)))])
           #`(λ (x ...) #,(f #'(x ...) #f)))]
        [(equal? #,arity *exactly-two*)
         #,(with-syntax ([(x ...) (build-list 2 (λ (i) (format-id #f "x~a" i)))])
           #`(λ (x ...) #,(f #'(x ...) #f)))]
        [(equal? #,arity *exactly-three*)
         #,(with-syntax ([(x ...) (build-list 3 (λ (i) (format-id #f "x~a" i)))])
           #`(λ (x ...) #,(f #'(x ...) #f)))]
        [(equal? #,arity *at-least-three*)
         #,(with-syntax ([(x ...) (build-list 3 (λ (i) (format-id #f "x~a" i)))]
                       [y       (format-id #f "y")])
           #`(λ (x ... . y) #,(f #'(x ...) #'y)))]
        [(equal? #,arity *at-least-two*)
         #,(with-syntax ([(x ...) (build-list 2 (λ (i) (format-id #f "x~a" i)))]
                       [y       (format-id #f "y")])
           #`(λ (x ... . y) #,(f #'(x ...) #'y)))]
        [(equal? #,arity *at-least-one*)
         #,(with-syntax ([(x ...) (build-list 1 (λ (i) (format-id #f "x~a" i)))]
                       [y       (format-id #f "y")])
           #`(λ (x ... . y) #,(f #'(x ...) #'y)))]
        [(equal? #,arity *at-least-zero*)
         #,(with-syntax ([(x ...) (build-list 0 (λ (i) (format-id #f "x~a" i)))]
                       [y       (format-id #f "y")])
           #`(λ (x ... . y) #,(f #'(x ...) #'y)))]
        [(equal? #,arity *one-or-two*)
         (case-lambda #,(with-syntax ([(x ...) (build-list 1 (λ (i) (format-id #f "x~a" i)))])
                          #`[(x ...) #,(f #'(x ...) #f)])
                      #,(with-syntax ([(x ...) (build-list 2 (λ (i) (format-id #f "x~a" i)))])
                          #`[(x ...) #,(f #'(x ...) #f)]))]
        [else
         ;(println (list 'falback 'make-plain-procedure-stx #,arity '#,name))
         #,e]))
  ;(displayln (wrp stx))
  (eval-syntax (wrp stx)))

(define-syntax (make-plain-procedure-slct stx)
  (syntax-case stx ()
    [(_ n a o r oo rr)
     (let ([O (eval-syntax #'o)]
           [R (eval-syntax #'r)])
       #`(cond
           [(equal? a *exactly-zero*)
            #,(with-syntax ([(x ...) (build-list 0 (λ (i) (format-id #f "x~a" i)))])
                #`(λ (x ...) #,(O #'(x ...))))]
           [(equal? a *exactly-one*)
            #,(with-syntax ([(x ...) (build-list 1 (λ (i) (format-id #f "x~a" i)))])
                #`(λ (x ...) #,(O #'(x ...))))]
           [(equal? a *exactly-two*)
            #,(with-syntax ([(x ...) (build-list 2 (λ (i) (format-id #f "x~a" i)))])
                #`(λ (x ...) #,(O #'(x ...))))]
           [(equal? a *exactly-three*)
            #,(with-syntax ([(x ...) (build-list 3 (λ (i) (format-id #f "x~a" i)))])
                #`(λ (x ...) #,(O #'(x ...))))]
           [(equal? a *at-least-three*)
            #,(with-syntax ([(x ...) (build-list 3 (λ (i) (format-id #f "x~a" i)))]
                            [y       (format-id #f "y")])
                #`(λ (x ... . y) #,(R #'(x ...) #'y)))]
           [(equal? a *at-least-two*)
            #,(with-syntax ([(x ...) (build-list 2 (λ (i) (format-id #f "x~a" i)))]
                            [y       (format-id #f "y")])
                #`(λ (x ... . y) #,(R #'(x ...) #'y)))]
           [(equal? a *at-least-one*)
            #,(with-syntax ([(x ...) (build-list 1 (λ (i) (format-id #f "x~a" i)))]
                            [y       (format-id #f "y")])
                #`(λ (x ... . y) #,(R #'(x ...) #'y)))]
           [(equal? a *at-least-zero*)
            #,(with-syntax ([(x ...) (build-list 0 (λ (i) (format-id #f "x~a" i)))]
                            [y       (format-id #f "y")])
                #`(λ (x ... . y) #,(R #'(x ...) #'y)))]
           [(equal? a *one-or-two*)
            (case-lambda #,(with-syntax ([(x ...) (build-list 1 (λ (i) (format-id #f "x~a" i)))])
                             #`[(x ...) #,(O #'(x ...))])
                         #,(with-syntax ([(x ...) (build-list 2 (λ (i) (format-id #f "x~a" i)))])
                             #`[(x ...) #,(O #'(x ...))]))]
           [else
            ;(println (list 'falback 'make-plain-procedure-stx #,arity '#,name))
            (make-plain-procedure-stx n oo rr a)]))]))


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
  (define (o4 f1 f2) (make-plain-procedure-slct (λ x (+ (apply f1 x) (apply f2 x)))
                                                (procedure-arity f1)))
  
  
  (define F1 (o1 f1 f2))
  (define F2 (o2 f1 f2))
  (define F3 (o3 f1 f2))
  (define F4 (o4 f1 f2))
  (define G1 (o1 g1 g2))
  (define G2 (o2 g1 g2))
  (define G3 (o3 g1 g2))
  (define G4 (o4 g1 g2))
  (define K1 (o1 k1 k2))
  (define K2 (o2 k1 k2))
  (define K3 (o3 k1 k2))
  (define K4 (o4 k1 k2))

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
  (test [M N] (F4 1 2))

  (test [M N] (G1 1 2))
  (test [M N] (G2 1 2))
  (test [M N] (G3 1 2))
  (test [M N] (G4 1 2))
  (test [M N] (G1 1 2 0 0))
  (test [M N] (G2 1 2 0 0))
  (test [M N] (G3 1 2 0 0))
  (test [M N] (G4 1 2 0 0))

  (test [M N] (K1 2))
  (test [M N] (K2 2))
  (test [M N] (K3 2))
  (test [M N] (K4 2))
  (test [M N] (K1 2 1 1 1))
  (test [M N] (K2 2 1 1 1))
  (test [M N] (K3 2 1 1 1))
  (test [M N] (K4 2 1 1 1))
)