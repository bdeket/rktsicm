#lang racket/base

(require rackunit
         "../../main.rkt"
         "../helper.rkt"
         )

(provide the-tests)

(define the-tests
  (test-suite
   "kernel/structs"
   (test-case "ORIG:map-chain"
              (check-equal?
               (simplify
                (s:map-chain cons (up 'a 'b 'c)))
               '(up (a 0) (b 1) (c 2)))

              (check-equal?
               (simplify
                (s:map-chain cons (up 'a (down 'b 'c) 'd)))
               '(up (a 0) (down (b 1 0) (c 1 1)) (d 2))))
   
   (test-case "ORIG:matrix*matrix"
              (check-equal?
               (simplify
                (matrix*matrix
                 (matrix-by-rows '(a b) '(c d))
                 (matrix-by-rows '(e f) '(g h))))
               '(matrix-by-rows (list (+ (* a e) (* b g)) (+ (* a f) (* b h)))
                                (list (+ (* c e) (* d g)) (+ (* c f) (* d h))))))

   (test-case "ORIG:s:multiply"
              (check-equal?
               (simplify
                (s:multiply
                 (down (up 'a 'c) (up 'b 'd))
                 (down (up 'e 'g) (up 'f 'h))))
               '(down (up (+ (* a e) (* b g)) (+ (* c e) (* d g)))
                      (up (+ (* a f) (* b h)) (+ (* c f) (* d h)))))
              
              (check-equal?
               (simplify
                (s:multiply
                 (down (up 'a 'c) (up 'b 'd))
                 (up 'dx 'dy)))
               '(up (+ (* a dx) (* b dy)) (+ (* c dx) (* d dy)))))
   
   (test-case "ORIG:s:outer-product"
              (check-equal?
               (simplify
                (s:outer-product (up 'a0 'a1)
                                 (down 'b0 'b1 'b2)))
               '(down (up (* a0 b0) (* a1 b0))
                      (up (* a0 b1) (* a1 b1))
                      (up (* a0 b2) (* a1 b2))))

              (check-equal?
               (simplify
                (m:outer-product (column-matrix 'a0 'a1)
                                 (row-matrix 'b0 'b1 'b2)))
               '(matrix-by-rows (list (* a0 b0) (* a0 b1) (* a0 b2))
                                (list (* a1 b0) (* a1 b1) (* a1 b2))))
              
              (check-unique-match?
               (simplify
                (let* ((s (up 'dt (up 'dx 'dy) (down 'dpx 'dpy)))
                       (s* (compatible-shape s)))
                  (* s* (s:outer-product s s*) s)))
               (x0 x1 x2 x3 x4)
               `(+ (* (expt dpx 2) (expt ,x0 2))
                   (* 2 dpx dpy ,x0 ,x1)
                   (* 2 dpx dt ,x2 ,x0)
                   (* 2 dpx dx ,x3 ,x0)
                   (* 2 dpx dy ,x4 ,x0)
                   (* (expt dpy 2) (expt ,x1 2))
                   (* 2 dpy dt ,x2 ,x1)
                   (* 2 dpy dx ,x3 ,x1)
                   (* 2 dpy dy ,x4 ,x1)
                   (* (expt dt 2) (expt ,x2 2))
                   (* 2 dt dx ,x2 ,x3)
                   (* 2 dt dy ,x2 ,x4)
                   (* (expt dx 2) (expt ,x3 2))
                   (* 2 dx dy ,x3 ,x4)
                   (* (expt dy 2) (expt ,x4 2)))))

   (test-case "ORIG:s:inverse1"
              (check-equal?
               (simplify
                (let* ([a (down (down 'a 'b) (down 'c 'd))]
                       [b (up 'e 'f)]
                       [c (* a b)])
                  (- b (* (s:inverse1 a b) c))))
               '(up 0 0)))

   (test-case "ORIG:A_mn->Mnm"
              (check-equal?
               (A_mn->Mnm (down (down 'a 'b) (down 'c 'd) (down 'e 'f)))
               '(*matrix* (3 . 2) #(#(a b) #(c d) #(e f)))))

   (test-case "ORIG:Mnm->A_mn"
              (check-equal?
               ((compose Mnm->A_mn A_mn->Mnm) (down (down 'a 'b) (down 'c 'd) (down 'e 'f)))
               (down (down 'a 'b) (down 'c 'd) (down 'e 'f))))

   (test-case "ORIG:A^mn->Mmn"
              (check-equal?
               (A^mn->Mmn (up (up 'a 'b) (up 'c 'd) (up 'e 'f)))
               '(*matrix* (2 . 3) #(#(a c e) #(b d f)))))

   (test-case "ORIG:Mmn->A^mn"
              (check-equal?
               ((compose Mmn->A^mn A^mn->Mmn) (up (up 'a 'b) (up 'c 'd) (up 'e 'f)))
               (up (up 'a 'b) (up 'c 'd) (up 'e 'f))))

   (test-case "ORIG:A^m_n->Mmn"
              (check-equal?
               (A^m_n->Mmn (down (up 'a 'b) (up 'c 'd) (up 'e 'f)))
               '(*matrix* (2 . 3) #(#(a c e) #(b d f)))))

   (test-case "ORIG:Mmn->A^m_n"
              (check-equal?
               ((compose Mmn->A^m_n A^m_n->Mmn) (down (up 'a 'b) (up 'c 'd) (up 'e 'f)))
               (down (up 'a 'b) (up 'c 'd) (up 'e 'f))))

   (test-case "ORIG:A_m^n->Mnm"
              (check-equal?
               (A_m^n->Mnm (up (down 'a 'b) (down 'c 'd) (down 'e 'f)))
               '(*matrix* (3 . 2) #(#(a b) #(c d) #(e f)))))

   (test-case "ORIG:Mnm->A_m^n"
              (check-equal?
               ((compose Mnm->A_m^n A_m^n->Mnm) (up (down 'a 'b) (down 'c 'd) (down 'e 'f)))
               (up (down 'a 'b) (down 'c 'd) (down 'e 'f))))

   (test-case "ORIG:s:invert"
              (check-equal?
               (simplify
                (let* ([a (down (down 'a 'b) (down 'c 'd))]
                       [b (up 'e 'f)]
                       [c (* a b)])
                  (- b (* (s:invert a) c))))
               '(up 0 0))

              (check-equal?
               (simplify
                (let* ([a (up (up 'a 'b) (up 'c 'd))]
                       [b (down 'e 'f)]
                       [c (* a b)])
                  (- b (* (s:invert a) c))))
               '(down 0 0))

              (check-equal?
               (simplify
                (let* ([a (up (down 'a 'b) (down 'c 'd))]
                       [b (down 'e 'f)]
                       [c (* a b)])
                  (- b (* (s:invert a) c))))
               '(down 0 0))

              (check-equal?
               (simplify
                (let* ([a (down (up 'a 'b) (up 'c 'd))]
                       [b (up 'e 'f)]
                       [c (* a b)])
                  (- b (* (s:invert a) c))))
               '(up 0 0)))

   (test-case "ORIG:s:divide-by-structure"
              (check-equal?
               (simplify
                (let* ([a (up (down 'a 'b) (down 'c 'd))]
                       [b (down 'e 'f)]
                       [c (* a b)])
                  (- b (s:divide-by-structure c a))))
               '(down 0 0))

              (check-equal?
               (simplify
                (let* ([a (down (up 'a 'b) (up 'c 'd))]
                       [b (up 'e 'f)]
                       [c (* a b)])
                  (- b (s:divide-by-structure c a))))
               '(up 0 0))

              (check-equal?
               (simplify
                (let* ([a (down (down 'a 'b) (down 'c 'd))]
                       [b (down 'e 'f)]
                       [c (* a b)])
                  (* a (s:divide-by-structure b a))))
               '(down e f))

              (check-equal?
               (simplify
                (let* ([a (up (up 'a 'b) (up 'c 'd))]
                       [b (up 'e 'f)]
                       [c (* a b)])
                  (* a (s:divide-by-structure b a))))
               '(up e f)))

   (test-case "ORIG:flip-indices"
              (check-equal?
               (flip-indices (up 'a (up 'b 'c) (down 'd (up 'e 'f) 'g)))
               (down 'a (down 'b 'c) (up 'd (down 'e 'f) 'g))))

   (test-case "ORIG:flip-outer-indices"
              (check-equal?
               (flip-outer-index (up 'a (up 'b 'c) (down 'd (up 'e 'f) 'g)))
               (down 'a (up 'b 'c) (down 'd (up 'e 'f) 'g))))

   (test-case "ORIG:typical-object"
              (check-unique-match?
               (simplify (typical-object (up 't (up 'u 'v) (down 'r 's) (up 'v1 'v2))))
               (t u v r s v1 v2)
               `(up ,t (up ,u ,v) (down ,r ,s) (up ,v1 ,v2))))

   (test-case "ORIG:structure->access-chains"
              (check-equal?
               (structure->access-chains (up 't (up 'u 'v) (down 'r 's) (up 'v1 'v2)))
               (up '(0) (up '(1 0) '(1 1)) (down '(2 0) '(2 1)) (up '(3 0) '(3 1)))))

   (test-case "ORIG:structure->prototype"
              (check-equal?
               (structure->prototype 'foo (up 't (up 'u 'v) (down 'r 's) (up 'v1 'v2)))
               (up 'foo:0 (up 'foo:1:0 'foo:1:1) (down 'foo:2:0 'foo:2:1) (up 'foo:3:0 'foo:3:1))))

   (test-case "ORIG:compatible-zero"
              (check-equal?
               (compatible-zero (up 't (up 'u 'v) (down 'r 's) (up 'v1 'v2)))
               (down 0 (down 0 0) (up 0 0) (down 0 0))))

   (test-case "ORIG:compatible-shape"
              (check-unique-match?
               (simplify (compatible-shape (up 't (up 'u 'v) (down 'r 's) (up 'v1 'v2))))
               (t u v r s v1 v2)
               `(down ,t (down ,u ,v) (up ,r ,s) (down ,v1 ,v2))))

   (test-case "ORIG:compatible-zero"
              (check-equal?
               (s:transpose-outer (down (down (up 'x 'y)
                                              (up 'z 'w))
                                        (down (up 'a 'b)
                                              (up 'c 'd))))
               (down (down (up 'x 'y)
                           (up 'a 'b))
                     (down (up 'z 'w)
                           (up 'c 'd)))))

   (test-case "ORIG:ultra-flatten"
              (check-equal?
               (ultra-flatten (up 1 2 'a (down 3 4) (up (down 'c 'd) 'e)))
               '(1 2 a 3 4 c d e)))

   (test-case "ORIG:s:dimension"
              (check-equal?
               (s:dimension (up 1 2 'a (down 3 4) (up (down 'c 'd) 'e)))
               8))

   (test-case "ORIG:ultra-unflatten"
              (check-equal?
               (ultra-unflatten
                (up 'x 'x 'x (down 'x 'x) (up (down 'x 'x) 'x))
                (list 1 2 'a 3 4 'c 'd 'e))
               #(1 2 a (*down* #(3 4)) #((*down* #(c d)) e)))

              (check-equal?
               (simplify
                (- (ultra-unflatten
                    (up 'x 'x 'x (down 'x 'x) (up (down 'x 'x) 'x))
                    (list 1 2 'a 3 4 'c 'd 'e))
                   (up 1 2 'a (down 3 4) (up (down 'c 'd) 'e))))
               '(up 0 0 0 (down 0 0) (up (down 0 0) 0))))

   (test-case "ORIG:s->m & others"
              (define vs
                (velocity-tuple
                 (velocity-tuple 'vx1 'vy1)
                 (velocity-tuple 'vx2 'vy2)))
              (define (L1 vs)
                (let ((v1 (ref vs 0))
                      (v2 (ref vs 1)))
                  (+ (* 1/2 'm1 (square v1))
                     (* 1/2 'm2 (square v2)))))

              (check-equal?
               (simplify (((expt D 2) L1) vs))
               '(down (down (down (down m1 0) (down 0 0)) (down (down 0 m1) (down 0 0)))
                      (down (down (down 0 0) (down m2 0)) (down (down 0 0) (down 0 m2)))))

              (check-equal?
               (simplify (s->m vs (((expt D 2) L1) vs) vs))
               '(matrix-by-rows (list m1 0 0 0)
                                (list 0 m1 0 0)
                                (list 0 0 m2 0)
                                (list 0 0 0 m2)))

              (check-equal?
               (simplify (m->s vs (s->m vs (((expt D 2) L1) vs) vs) vs))
               '(down (down (down (down m1 0) (down 0 0)) (down (down 0 m1) (down 0 0)))
                      (down (down (down 0 0) (down m2 0)) (down (down 0 0) (down 0 m2))))))

   (test-case "ORIG:as-matrix"
              (define ((D-as-matrix F) s)
                (s->m (compatible-shape (F s)) ((D F) s) s))
              (define C-general
                (literal-function 'C
                                  (-> (UP Real
                                          (UP Real Real)
                                          (DOWN Real Real))
                                      (UP Real
                                          (UP Real Real)
                                          (DOWN Real Real)))))
              (define s (up 't (up 'x 'y) (down 'px 'py)))
              (check-equal?
               (simplify ((as-matrix (D C-general)) s))
               '(matrix-by-rows
                 (list (((partial 0) C^0) (up t (up x y) (down px py)))
                       (((partial 1 0) C^0) (up t (up x y) (down px py)))
                       (((partial 1 1) C^0) (up t (up x y) (down px py)))
                       (((partial 2 0) C^0) (up t (up x y) (down px py)))
                       (((partial 2 1) C^0) (up t (up x y) (down px py))))
                 (list (((partial 0) C^1^0) (up t (up x y) (down px py)))
                       (((partial 1 0) C^1^0) (up t (up x y) (down px py)))
                       (((partial 1 1) C^1^0) (up t (up x y) (down px py)))
                       (((partial 2 0) C^1^0) (up t (up x y) (down px py)))
                       (((partial 2 1) C^1^0) (up t (up x y) (down px py))))
                 (list (((partial 0) C^1^1) (up t (up x y) (down px py)))
                       (((partial 1 0) C^1^1) (up t (up x y) (down px py)))
                       (((partial 1 1) C^1^1) (up t (up x y) (down px py)))
                       (((partial 2 0) C^1^1) (up t (up x y) (down px py)))
                       (((partial 2 1) C^1^1) (up t (up x y) (down px py))))
                 (list (((partial 0) C^2_0) (up t (up x y) (down px py)))
                       (((partial 1 0) C^2_0) (up t (up x y) (down px py)))
                       (((partial 1 1) C^2_0) (up t (up x y) (down px py)))
                       (((partial 2 0) C^2_0) (up t (up x y) (down px py)))
                       (((partial 2 1) C^2_0) (up t (up x y) (down px py))))
                 (list (((partial 0) C^2_1) (up t (up x y) (down px py)))
                       (((partial 1 0) C^2_1) (up t (up x y) (down px py)))
                       (((partial 1 1) C^2_1) (up t (up x y) (down px py)))
                       (((partial 2 0) C^2_1) (up t (up x y) (down px py)))
                       (((partial 2 1) C^2_1) (up t (up x y) (down px py))))))

              (check-equal?
               (simplify ((- (D-as-matrix C-general)
                             (as-matrix (D C-general)))
                          s))
               '(matrix-by-rows (list 0 0 0 0 0)
                                (list 0 0 0 0 0)
                                (list 0 0 0 0 0)
                                (list 0 0 0 0 0)
                                (list 0 0 0 0 0))))


   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))