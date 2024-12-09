#lang racket/base

(require (for-syntax racket/base
                     (only-in racket/syntax format-id))
         (only-in racket/syntax format-id)
         (only-in racket/list last take)
         "arity.rkt")

(provide make-plain-procedure make-plain-procedure-stx make-plain-procedure-slct
         (for-syntax λ quasisyntax unsyntax-splicing unsyntax))
(module+ ALL (provide (all-from-out (submod ".."))))

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
                    (raise-argument-error name "procedure-arity?" A)))
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
    [else                            (make-plain-procedure name f arity)]))

(define (make-plain-procedure-slct+ name wrp)
  (define (stx f e arity)
    #`(cond
        [(equal? '#,arity *exactly-zero*)
         #,(with-syntax ([(x ...) '()])
           #`(λ (x ...) #,(f #'(x ...) #f)))]
        [(equal? '#,arity *exactly-one*)
         #,(with-syntax ([(x ...) (build-list 1 (λ (i) (format-id #f "x~a" i)))])
           #`(λ (x ...) #,(f #'(x ...) #f)))]
        [(equal? '#,arity *exactly-two*)
         #,(with-syntax ([(x ...) (build-list 2 (λ (i) (format-id #f "x~a" i)))])
           #`(λ (x ...) #,(f #'(x ...) #f)))]
        [(equal? '#,arity *exactly-three*)
         #,(with-syntax ([(x ...) (build-list 3 (λ (i) (format-id #f "x~a" i)))])
           #`(λ (x ...) #,(f #'(x ...) #f)))]
        [(equal? '#,arity *at-least-three*)
         #,(with-syntax ([(x ...) (build-list 3 (λ (i) (format-id #f "x~a" i)))]
                       [y       (format-id #f "y")])
           #`(λ (x ... . y) #,(f #'(x ...) #'y)))]
        [(equal? '#,arity *at-least-two*)
         #,(with-syntax ([(x ...) (build-list 2 (λ (i) (format-id #f "x~a" i)))]
                       [y       (format-id #f "y")])
           #`(λ (x ... . y) #,(f #'(x ...) #'y)))]
        [(equal? '#,arity *at-least-one*)
         #,(with-syntax ([(x ...) (build-list 1 (λ (i) (format-id #f "x~a" i)))]
                       [y       (format-id #f "y")])
           #`(λ (x ... . y) #,(f #'(x ...) #'y)))]
        [(equal? '#,arity *at-least-zero*)
         #,(with-syntax ([(x ...) '()]
                       [y       (format-id #f "y")])
           #`(λ (x ... . y) #,(f #'(x ...) #'y)))]
        [(equal? '#,arity *one-or-two*)
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
            #,(with-syntax ([(x ...) '()])
                (quasisyntax/loc #'o (λ (x ...) #,(O #'(x ...)))))]
           [(equal? a *exactly-one*)
            #,(with-syntax ([(x ...) (build-list 1 (λ (i) (format-id #f "x~a" i)))])
                (quasisyntax/loc #'o (λ (x ...) #,(O #'(x ...)))))]
           [(equal? a *exactly-two*)
            #,(with-syntax ([(x ...) (build-list 2 (λ (i) (format-id #f "x~a" i)))])
                (quasisyntax/loc #'o (λ (x ...) #,(O #'(x ...)))))]
           [(equal? a *exactly-three*)
            #,(with-syntax ([(x ...) (build-list 3 (λ (i) (format-id #f "x~a" i)))])
                (quasisyntax/loc #'o (λ (x ...) #,(O #'(x ...)))))]
           [(equal? a *at-least-three*)
            #,(with-syntax ([(x ...) (build-list 3 (λ (i) (format-id #f "x~a" i)))]
                            [y       (format-id #f "y")])
                (quasisyntax/loc #'r (λ (x ... . y) #,(R #'(x ...) #'y))))]
           [(equal? a *at-least-two*)
            #,(with-syntax ([(x ...) (build-list 2 (λ (i) (format-id #f "x~a" i)))]
                            [y       (format-id #f "y")])
                (quasisyntax/loc #'r (λ (x ... . y) #,(R #'(x ...) #'y))))]
           [(equal? a *at-least-one*)
            #,(with-syntax ([(x ...) (build-list 1 (λ (i) (format-id #f "x~a" i)))]
                            [y       (format-id #f "y")])
                (quasisyntax/loc #'r (λ (x ... . y) #,(R #'(x ...) #'y))))]
           [(equal? a *at-least-zero*)
            #,(with-syntax ([(x ...) '()]
                            [y       (format-id #f "y")])
                (quasisyntax/loc #'r (λ (x ... . y) #,(R #'(x ...) #'y))))]
           [(equal? a *one-or-two*)
            (case-lambda #,(with-syntax ([(x ...) (build-list 1 (λ (i) (format-id #f "x~a" i)))])
                             (quasisyntax/loc #'o [(x ...) #,(O #'(x ...))]))
                         #,(with-syntax ([(x ...) (build-list 2 (λ (i) (format-id #f "x~a" i)))])
                             (quasisyntax/loc #'o [(x ...) #,(O #'(x ...))])))]
           [else
            ;(println (list 'falback 'make-plain-procedure-stx #,arity '#,name))
            #,(quasisyntax/loc stx (make-plain-procedure-stx n oo rr a))]))]))


#;
(module+ main
  (provide HH printHH)
  (define HH (make-hash))
  (define (printHH)
    (local-require racket/list racket/format)
    (let ([cols (cons " " (sort (remove-duplicates (append-map hash-keys (hash-values HH))) string<=?))])
      (define sizes (cons
                     (apply max (string-length (car cols))
                            (for/list ([(k v) (in-hash HH)])
                              (string-length (format "~a" k))))
                     (for/list ([c (in-list (cdr cols))])
                       (apply max (string-length c)
                              (for/list ([(k v) (in-hash HH)])
                                (string-length (format "~a" (hash-ref v c "-  "))))))))
      (define (printline r)
        (define r* (cons (~a (car r) #:min-width (car sizes))
                         (let ([s (apply max (cdr sizes))])
                           (for/list ([r (in-list (cdr r))])
                             (~a r #:min-width s #:align 'right)))))
        (displayln (apply string-append (list* (car r*) " | " (add-between (cdr r*) " ")))))

      (printline cols)
      (displayln (build-string (apply + 2 (length sizes) sizes) (λ (_) #\=)))
      (for ([k (in-list (sort (hash-keys HH) symbol<?))])
        (define v (hash-ref HH k))
        (printline (cons k (for/list ([c (in-list (cdr cols))]) (hash-ref v c "-  ")))))))
  (define (add row col t)
    (define H (hash-ref! HH row (λ () (make-hash))))
    (hash-set! H col t))

  (define-syntax (test stx)
    (syntax-case stx ()
      [(test [m n r] (combi ...) f1 f2 AA (a ...) ...)
       #`(begin
           (let ()
             (define O 'combi)
             (define AAA AA)
             (displayln `(combi f1 f2 ,AAA))
             (define T1 (/ (for/sum ([i (in-range m)])
                             (collect-garbage)(collect-garbage)(collect-garbage)
                             (define V #f)
                             (define start (current-milliseconds))
                             (for ([i (in-range n)]) (set! V (combi f1 f2 AAA)))
                             (- (current-milliseconds) start))
                           1. m))
             (add O (format "~ac" 'f1) T1)
             (displayln `(creation [,m × ,n] in ,T1))
        
             (define fct (combi f1 f2 AAA))
             (let ()
               (define T2 (/ (for/sum ([i (in-range m)])
                               (collect-garbage)(collect-garbage)(collect-garbage)
                               (define V #f)
                               (define start (current-milliseconds))
                               (for ([i (in-range r)]) (set! V (fct a ...)))
                               (- (current-milliseconds) start))
                             1. m))
               (add O (format "~ae~a" 'f1 (list a ...)) T2)
               (displayln `(execution [,m × ,r] of (fct a ...) -> ,(fct a ...) in ,T2))
               ) ...) ...
                      (displayln ""))]))
  (define M 5)
  (define N 10000)
  (define R 10000000)

  (define (f1 x y) (+ x y))
  (define (f2 x y) (* x y))
  (define (g1 x y . z) (apply + x y z))
  (define (g2 x y . z) (apply * x y z))
  (define (h1 u v w x) (+ u v w x))
  (define (h2 u v w x) (* u v w x))
  (define k1 (case-lambda [(x) (- x)]
                          [(x y . z) (- x (apply + y z))]))
  (define k2 (case-lambda [(x) (/ x)]
                          [(x y . z) (/ x (apply * y z))]))
  (define l1 (case-lambda [(x) (- x)]
                          [(u v w x) (- u (+ v w x))]))
  (define l2 (case-lambda [(x) (/ x)]
                          [(u v w x) (/ u (* v w x))]))
  (define m1 (case-lambda [(x) (- x)]
                          [(x y) (- y x)]
                          [(x y z) (- x (+ y z))]))
  (define m2 (case-lambda [(x y z) (* x y z)]
                          [(u v w x) (/ u (* v w x))]))

  (define (o0 f1 f2 [A #f]) (λ x (+ (apply f1 x) (apply f2 x))))
  (define (o1 f1 f2 [A (procedure-arity f1)])
    (procedure-reduce-arity (λ x (+ (apply f1 x) (apply f2 x))) A))
  (define (o2 f1 f2 [A (procedure-arity f1)])
    (define B (procedure-arity-mask (procedure-reduce-arity (λ x 1) A)))
    (procedure-reduce-arity-mask (λ x (+ (apply f1 x) (apply f2 x))) B))

  (define (o3 f1 f2 [A (procedure-arity f1)])
    (make-plain-procedure "name" (λ x (+ (apply f1 x) (apply f2 x))) A))
  (define (o4 f1 f2 [A (procedure-arity f1)])
    (make-plain-procedure-stx "name"
                              (λ (x)   #`(#,+ (#,f1 #,@x) (#,f2 #,@x)))
                              (λ (x y) #`(#,+ (apply #,f1 #,@x #,y) (apply #,f2 #,@x #,y)))
                              A))
  (define (o5 f1 f2 [A (procedure-arity f1)]) (plain-procedure-slct "name"
                                                                    (λ x (+ (apply f1 x) (apply f2 x)))
                                                                    (procedure-arity f1)))
  ;; extremeley slow (×10-20) creation (fast execution, ~ as o2/o6)
  (define (o6 f1 f2 [A (procedure-arity f1)])
    (make-plain-procedure-slct+ "name"
                                (λ (F)
                                  (F (λ (x y)
                                       (if y
                                           #`(#,+ (apply #,f1 #,@x #,y) (apply #,f2 #,@x #,y))
                                           #`(#,+ (#,f1 #,@x) (#,f2 #,@x))))
                                     #`(make-plain-procedure "name"
                                                             (λ x (+ (apply #,f1 x) (apply #,f2 x)))
                                                             '#,A)
                                     A))))
  (define (o7 f1 f2 [A (procedure-arity f1)])
    (make-plain-procedure-slct "name"
                               A
                               (λ (x)   #`(+ (f1 #,@x) (f2 #,@x)))
                               (λ (x y) #`(+ (apply f1 #,@x #,y) (apply f2 #,@x #,y)))
                               (λ (x)   #`(#,+ (#,f1 #,@x) (#,f2 #,@x)))
                               (λ (x y) #`(#,+ (apply #,f1 #,@x #,y) (apply #,f2 #,@x #,y)))))

  (for ([f1 (in-list (list f1 g1 h1 k1 l1 m1))]
        [f2 (in-list (list f2 g2 h2 k2 l2 m1))]
        [A  (in-list (list #f #f #f #f #f 3))]
        [args (in-list (list '((1 2))
                             '((1 2) (1 2 0 0))
                             '((1 2 3 4))
                             '((2) (2 1 1 1))
                             '((3) (4 2 1 -1))
                             '((3 2 1))))])
    (define Os (list o0 o3 o4 o5 o6 o7))
    (define Fs (map (λ (o) (if A (o f1 f2 A) (o f1 f2))) Os))
    (for ([arg (in-list args)])
      (define A0 (apply (car Fs) arg))
      (for ([F (in-list (cdr Fs))]
            [O (in-list (cdr Os))])
        (define v (apply F arg))
        (unless (= A0 )
          (error (format "(~a ~a ~a) = ~a, expected ~a" O f1 f2 A0 v))))))
  
  
  (test [M N R] (o0 o1 o2 o3 o4 o5   o6 o7) f1 f2 (procedure-arity f1) (1 2))
  (test [M N R] (o0 o1 o2 o3 o4 o5 #;o6 o7) g1 g2 (procedure-arity g1) (1 2) (1 2 0 0))
  (test [M N R] (o0 o1 o2 o3 o4 o5 #;o6 o7) h1 h2 (procedure-arity h1) (1 2 3 4))
  (test [M N R] (o0 o1 o2 o3 o4 o5 #;o6 o7) k1 k2 (procedure-arity k1) (2) (2 1 1 1))
  (test [M N R] (o0 o1 o2 o3 o4 o5   o6 o7) l1 l2 (procedure-arity l1) (2) (2 1 1 1))
  (test [M N R] (o0 o1 o2 o3 o4 o5 #;o6 o7) m1 m2  3                   (1 2 3))
  
  (printHH)
#|
;; BC 8.15.0.4

arity:          2                        2+                                     4                         1+                                (2 . 4)                                      3

   |          f1c     f1e(1 2)          g1c g1e(1 2 0 0)     g1e(1 2)          h1c h1e(1 2 3 4)          k1c k1e(2 1 1 1)       k1e(2)          l1c l1e(2 1 1 1)       l1e(2)
   |          ***                       ***                                    ***                       ***                                    ***                          
==============================================================================================================================================================================
o1 |          2.4        360.0          3.4       1243.4        758.6          2.6        479.0          3.4       1564.2       1221.4          3.0        594.4       1192.6
o3 |       1149.6        326.0       1354.0       1308.8        708.0       1231.4        450.2       1292.8       1635.6       1231.6        981.8        533.6       1455.4
o4 |       1668.4        138.2       2265.8       1084.4        574.2       1817.4        171.0       2135.0       1430.0       1060.4       2157.2        265.0        973.6
o5 |          0.6        323.4          2.8       1365.0        754.4       1231.8        451.4          4.0       1660.8       1287.0        979.2        538.0       1474.2
o6 |      20223.6        139.6                                                                                                              22821.4        546.0       1472.6
o7 |          1.0        164.0          6.6       1120.8        592.8       1874.8        177.2         13.6       1404.4       1058.8       2140.2        269.4        971.0


   
   |          f1c     f1e(1 2)          g1c g1e(1 2 0 0)     g1e(1 2)          h1c h1e(1 2 3 4)          k1c k1e(2 1 1 1)       k1e(2)          l1c l1e(2 1 1 1)       l1e(2)
   |          ***                       ***                                    ***                       ***                                    ***                          
==============================================================================================================================================================================
o0 |            0        234.4            0       1044.2        571.6          0.2        339.4            0       1327.0       1072.4            0        432.2       1087.8
o1 |          2.2        351.8          3.0       1262.6        756.0          2.2        480.2          3.6       1552.2       1259.0          3.0        594.2       1221.4
o3 |       1136.4        319.4       1435.2       1381.8        760.6       1235.6        451.0       1260.2       1649.4       1241.4        993.6        558.2       1506.0
o4 |       1686.2        133.4       2226.4       1078.0        563.8       1824.8        169.8       2090.0       1395.2       1085.6       2201.4        267.6       1032.2
o5 |          0.2        309.8          3.2       1318.6        746.0       1241.2        454.0          4.0       1649.4       1239.8       1003.6        552.2       1488.2
o6 |      20154.0        138.8          -            -            -            -            -            -            -            -        23140.8        548.4       1499.2
o7 |          0.8        163.2          7.0       1129.6        569.6       1831.2        171.0         13.2       1411.6       1091.4       2209.2        272.0       1016.4

   
   |          f1c     f1e(1 2)          g1c g1e(1 2 0 0)     g1e(1 2)          h1c h1e(1 2 3 4)          k1c k1e(2 1 1 1)       k1e(2)          l1c l1e(2 1 1 1)       l1e(2)          m1c   m1e(1 2 3)
   |          ***                       ***                                    ***                       ***                                    ***                                    ***
========================================================================================================================================================================================================
o0 |            0        368.4          0.4       1073.0        594.2            0        342.4            0       1438.4       1054.0            0        426.4       1042.2          0.2        306.0
o1 |          2.0        417.4          2.0       1300.6        790.0          2.0        481.6          2.2       1555.2       1208.2          2.8        602.2       1227.2          1.8        447.6
o3 |       1118.0        324.0       1373.8       1327.8        748.2       1235.4        475.8       1298.2       1631.8       1218.8        987.8        567.4       1479.0       1222.8        408.0
o4 |       1702.4        142.6       2295.8       1068.4        574.4       1938.8        172.6       2087.6       1389.8       1061.6       2229.2        286.2       1009.2       1793.6        182.8
o5 |          0.2        321.0          3.0       1332.8        756.4       1250.2        461.6          4.0       1638.0       1229.8       1045.0        553.6       1501.2       1204.2        439.0
o6 |      21172.2        140.4          -            -            -            -            -            -            -            -        23514.4        557.8       1484.6      20898.2        182.6
o7 |          0.2        168.2          1.0       1107.4        575.6       1862.2        174.6          2.0       1401.4       1071.0       2230.8        279.2       1042.2            0        195.8


   |          f1c     f1e(1 2)          g1c g1e(1 2 0 0)     g1e(1 2)          h1c h1e(1 2 3 4)          k1c k1e(2 1 1 1)       k1e(2)          l1c l1e(2 1 1 1)       l1e(2)          m1c   m1e(1 2 3)
   |          ***                       ***                                    ***                       ***                                    ***                                    ***
========================================================================================================================================================================================================
o0 |            0        227.8            0       1033.4        556.6            0        342.0          0.2       1355.8       1034.2          0.2        451.0       1090.8            0        316.0
o1 |          2.0        357.4          2.2       1241.4        745.8          2.0        480.4          2.2       1509.0       1225.4          2.2        639.6       1303.4          2.0        479.8
o2 |          4.0        353.0          4.2       1265.6        737.4          4.2        466.8          4.0       1523.8       1212.4          4.4        649.0       1285.2          4.6        469.0
o3 |       1125.8        317.2       1371.8       1294.4        699.4       1247.8        447.8       1294.2       1602.6       1214.4       1064.6        580.6       1552.6       1303.4        441.8
o4 |       1695.6        137.6       2187.4       1036.6        558.8       1835.8        169.2       2132.2       1666.4       1187.8       2362.8        283.2       1043.8       1882.6        186.8
o5 |          0.6        319.6          3.0       1306.6        711.2       1250.0        452.6          4.2       1891.4       1415.0       1072.2        602.8       1565.6       1297.8        473.2
o6 |      20500.2        138.4          -            -            -            -            -            -            -            -        24084.0        590.0       1563.6          -            -
o7 |          0.2        161.4          1.6       1045.0        560.8       1850.6        170.6          2.0       1464.6       1109.4       2365.6        283.8       1051.6            0        201.0

;; CS 8.13.0.7

   |          f1c     f1e(1 2)          g1c g1e(1 2 0 0)     g1e(1 2)          h1c h1e(1 2 3 4)          k1c k1e(2 1 1 1)       k1e(2)          l1c l1e(2 1 1 1)       l1e(2)          m1c   m1e(1 2 3)
   |          ***                       ***                                    ***                       ***                                    ***                                    ***
========================================================================================================================================================================================================
o0 |            0        196.0            0        771.2        538.6            0        396.2            0        687.4        777.6            0        351.4        761.8            0        254.6
o1 |          0.2        200.8          0.2        822.6        553.6            0        398.4          0.4        691.0        760.2          0.6        345.4        755.2            0        257.0
o2 |          0.2        197.2          0.6        820.2        566.2          0.2        401.6          0.4        688.4        766.4          1.2        337.8        758.6          0.8        266.6
o3 |       2138.2        196.4       2585.0       1054.2        745.6       2320.0        400.0       2348.0        906.2        912.0       2210.8        367.6        773.6       2412.8        259.0
o4 |       3214.4         53.2       3828.4       1001.6        702.6       3466.8        184.2       3581.2        809.0        907.6       4178.2        125.4        628.0       3530.0         82.8
o5 |          1.0        206.4          3.0       1021.8        735.4       2316.8        419.4          3.6        890.0        901.0       2234.6        361.6        769.0       2650.6        268.0
o6 |      23251.2         52.2          -            -            -            -            -            -            -            -        26966.0        361.0        792.2          -            -
o7 |          1.0         57.6          2.0        950.8        690.4       3532.6        199.4          3.0        810.8        875.0       4368.2        128.2        667.6          1.0         86.6


|#
)