#lang s-exp "../../kernel.rkt"

(require rackunit)

;some examples litered troughout the files of calculus/*
;kernel
(require "../../solve.rkt"
         "../../general/list-utils.rkt")

(define-syntax-rule (hypothetical x) (make-hypothetical 'x #f))
;***************************************************************************************************
;* from solve.rkt                                                                                  *
;***************************************************************************************************
;;; For example...

(check-equal?
 (standardize-equation '(- (* 3 ((D f) t))
                           (+ (* (sqrt x) z (f t))
                              (g t)
                              (((expt D 2) g) t)
                              (square y)))
                       '() '() 't)
 #;'((+ (* -1 z (f t) (sqrt x))
      (* -1 (expt y 2))
      (* 3 ((D f) t))
      (* -1 (g t))
      (* -1 (((expt D 2) g) t)))
   ((((expt D 2) g) t) (g t) ((D f) t) y x (f t) z)
   (((expt D 2) g) g (D f) f))
 ;this looks ok, ... maybe? at least (f t) is put consistently last
 '((+
    (* -1 z (sqrt x) (f t))
    (* -1 (expt y 2))
    (* 3 ((D f) t))
    (* -1 (g t))
    (* -1 (((expt D 2) g) t)))
   ((((expt D 2) g) t) (g t) ((D f) t) y (f t) x z)
   (((expt D 2) g) g (D f) f)))

;;; Signs of life.  
(check-equal?
 (solve-incremental
  (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
        (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
  '(x y))
 '(() () (((= y 1) (B A)) ((= x 2) (B A))) ()))

(check-equal?
 (solve-incremental
  (list (make-equation '(+  x   y   z  1)  (list 'A))
        (make-equation '(+  x   y      2)  (list 'B))
        (make-equation '(+  x          1)  (list 'C)))
  '(x y z))
 '(() () (((= z 1) (A B C)) ((= y -1) (B C)) ((= x -1) (C))) ()))

(check-equal?
 (solve-incremental
  (list (make-equation '(+  x          1)  (list 'C))
        (make-equation '(+  x   y      2)  (list 'B))
        (make-equation '(+  x   y   z  1)  (list 'A)))
  '(x y z))
 '(() () (((= z 1) (A B C)) ((= y -1) (B C)) ((= x -1) (C))) ()))

;;; The following signals a contradiction, as it should:

(check-equal?
 (solve-incremental
      (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	    (make-equation '(+ (* 3 x)     y  -5)  (list 'B)))
      '(x y))
 '(contradictions (-2 (A B) ()) (2 (B A) ())))

;;; Some slightly nonlinear systems can be solved:
(check-equal?
 (solve-incremental
  (list (make-equation '(-  3 (+ x y))  (list 'A))
        (make-equation '(-  5 (- x y))  (list 'B))
        (make-equation '(-  3 (+ (* (sqrt x) z) (square y)))  (list 'C)))
  '(x y z))
 '(() () (((= z 1) (C B A)) ((= y -1) (B A)) ((= x 4) (B A))) ()))

;;; Underdetermined systems can be reduced:
(check-equal?
 (solve-incremental
  (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
        (make-equation '(- 3 (+ a b))  (list 'B)))
  '(a b c))
 '(() (c) (((= b (+ 3 (* -2/3 c))) (A B)) ((= a (* 2/3 c)) (A B))) ()))

(check-equal?
 (solve-incremental
  (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
        (make-equation '(- 3 (- a c))  (list 'B)))
  '(a b c))
 '(() (c) (((= b (+ -3 (* -4/3 c))) (A B)) ((= a (+ 3 c)) (B))) ()))

;;; Even very hard ones are clarified.
(check-equal?
 (solve-incremental
  (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
        (make-equation '(- 3 (- a b))  (list 'B)))
  '(a b c))
 '(()
   (b)
   (((= c (/ (+ 9 (* 2 (expt b 2)) (* 9 b)) (+ 2 (* 2 b)))) (A B))
    ((= a (+ 3 b)) (B)))
   ()))

;;; The following are permutations of the solution sequence
(check-equal?
 (solve-incremental
  (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
        (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
        (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
  '(x y z))
 '(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ()))

(check-equal?
 (solve-incremental
  (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
        (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
        (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
  '(z x y))
 '(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ()))

(check-equal?
 (solve-incremental
  (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
        (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
        (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
  '(y z x))
 ;'(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
 ;previous test seems to sugest that if two equations are equal-difficulty, the order of the
 ;the variables should be followed, this test seems to suggest something else intirely
 '(() () (((= z -1/2) (C B A)) ((= x 2) (B A)) ((= y 1) (B A))) ()))

(check-equal?
 (solve-incremental
      (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	    (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	    (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
      '(y x z))
 ;'(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
 ;previous test seems to sugest that if two equations are equal-difficulty, the order of the
 ;the variables should be followed, this test seems to suggest something else intirely
 '(() () (((= z -1/2) (C B A)) ((= x 2) (B A)) ((= y 1) (B A))) ()))

(check-equal?
 (solve-incremental
      (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	    (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	    (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
      '(z y x))
 ;'(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
 ;previous test seems to sugest that if two equations are equal-difficulty, the order of the
 ;the variables should be followed, this test seems to suggest something else intirely
 '(() () (((= z -1/2) (C B A)) ((= x 2) (B A)) ((= y 1) (B A))) ()))

(check-equal?
 (solve-incremental
  (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
        (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
        (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
  '(x z y))
 '(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ()))

;;; This wins somehow...
;TODO investigate problem of missing 0 solution (see solve/solve)
#|
(check-equal?
 (solve-incremental
      (list (make-equation '(- 200/3 (/ 1 (+ (/ 1 R1) (/ 1 R2))))  (list 'A))
	    (make-equation '(-  1/3 (/ R2 (+ R1 R2)))  (list 'B)))
      '(R1 R2))
 '(() () (((= R2 100) (B A)) ((= R1 200) (B A))) ()))

(check-equal?
 (solve-incremental
      (list (make-equation '(- 200/3 (/ 1 (+ (/ 1 R1) (/ 1 R2))))  (list 'A))
	    (make-equation '(-  1/3 (/ R2 (+ R1 R2)))  (list 'B)))
      '(R2 R1))
 '(() () (((= R2 100) (B A)) ((= R1 200) (B A))) ()))

(check-equal?
 (solve-incremental
  (list (make-equation '(- (* 1/3 (+ R1 R2)) R2)  (list 'B))
        (make-equation '(- (* 200/3 (+ R1 R2)) (* R1 R2))  (list 'A)))
  '(R1 R2))
 '(() () (((= R2 100) (B A)) ((= R1 200) (B A))) ()))

(check-equal?
 (solve-incremental
  (list (make-equation '(- (* 1/3 (+ R1 R2)) R2)  (list 'B))
        (make-equation '(- (* 200/3 (+ R1 R2)) (* R1 R2))  (list 'A)))
  '(R2 R1))
 '(() () (((= R2 100) (B A)) ((= R1 200) (B A))) ()))
|#

;;; Now can solve quadratics and does backtracking to find a root
(check-equal?
 (solve-incremental
  (list (make-equation '(- (expt x 2) 1)  (list 'A))
        (make-equation '(- x 1)  (list 'B)))
  '(x))
 '(() () (((= x 1) (B))) ()))

(check-equal?
 (solve-incremental
  (list (make-equation '(- (expt x 2) 1)  (list 'A))
        (make-equation '(- x -1)  (list 'B)))
  '(x))
 '(() () (((= x -1) (B))) ()))

;;; It doesn't to look at A to get answer, but A constrains the answer.
;TODO
#;(check-equal?
 (solve-incremental
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(- (expt y 2) 9) (list 'B))
        (make-equation '(- (- y x) 1) (list 'C)))
  '(x y))
 '(() () (((= y 3) (B C A)) ((= x 2) (A))) ()))

(check-equal?
 (solve-incremental
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(- (expt y 2) 9) (list 'B))
        (make-equation '(- (- y x) 2) (list 'C)))
  '(x y))
 `(contradictions
   (7 (B C A ,(hypothetical (- quadratic 1 -9 20))) ())
   (16 (B C A ,(hypothetical (+ quadratic 1 -9 20))) ())
   (2 (C A B ,(hypothetical (- quadratic 1 0 -9))) ())
   (56 (C A B ,(hypothetical (+ quadratic 1 0 -9))) ())
   (-1
    (,(hypothetical (- quadratic 1 0 -9)) B
                                          C
                                          A
                                          ,(hypothetical (- quadratic 1 -5 6)))
    ())
   (-2
    (,(hypothetical (- quadratic 1 0 -9)) B
                                          C
                                          A
                                          ,(hypothetical (+ quadratic 1 -5 6)))
    ())
   (2 (A C B ,(hypothetical (- quadratic 1 0 -9))) ())
   (2 (A C B ,(hypothetical (- quadratic 1 0 -9))) ())
   (-7
    (,(hypothetical (+ quadratic 1 0 -9)) B
                                          C
                                          A
                                          ,(hypothetical (- quadratic 1 -5 6)))
    ())
   (-8
    (,(hypothetical (+ quadratic 1 0 -9)) B
                                          C
                                          A
                                          ,(hypothetical (+ quadratic 1 -5 6)))
    ())
   (56 (A C B ,(hypothetical (+ quadratic 1 0 -9))) ())
   (56 (A C B ,(hypothetical (+ quadratic 1 0 -9))) ())
   (-1
    (,(hypothetical (- quadratic 1 -5 6)) A
                                          C
                                          B
                                          ,(hypothetical (- quadratic 1 0 -9)))
    ())
   (-7
    (,(hypothetical (- quadratic 1 -5 6)) A
                                          C
                                          B
                                          ,(hypothetical (+ quadratic 1 0 -9)))
    ())
   (7 (B C A ,(hypothetical (- quadratic 1 -5 6))) ())
   (-1
    (,(hypothetical (- quadratic 1 -5 6)) A
                                          C
                                          B
                                          ,(hypothetical (- quadratic 1 0 -9)))
    ())
   (-7
    (,(hypothetical (- quadratic 1 -5 6)) A
                                          C
                                          B
                                          ,(hypothetical (+ quadratic 1 0 -9)))
    ())
   (7 (B C A ,(hypothetical (- quadratic 1 -5 6))) ())
   (-2
    (,(hypothetical (+ quadratic 1 -5 6)) A
                                          C
                                          B
                                          ,(hypothetical (- quadratic 1 0 -9)))
    ())
   (-8
    (,(hypothetical (+ quadratic 1 -5 6)) A
                                          C
                                          B
                                          ,(hypothetical (+ quadratic 1 0 -9)))
    ())
   (16 (B C A ,(hypothetical (+ quadratic 1 -5 6))) ())
   (-2
    (,(hypothetical (+ quadratic 1 -5 6)) A
                                          C
                                          B
                                          ,(hypothetical (- quadratic 1 0 -9)))
    ())
   (-8
    (,(hypothetical (+ quadratic 1 -5 6)) A
                                          C
                                          B
                                          ,(hypothetical (+ quadratic 1 0 -9)))
    ())
   (16 (B C A ,(hypothetical (+ quadratic 1 -5 6))) ())))

;;; so it knows the outstanding contradictions...

;;; Here we are left over with a residual equation in z
(check-equal?
 (solve-incremental
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(- (expt y 2) z) (list 'B))
        (make-equation '(- (- y x) 2) (list 'C)))
  '(x y))
 `((((+ 25 (* -1 z)) (B C A ,(hypothetical (+ quadratic 1 -5 6))) (z)))
   ()
   (((= y 5) (C A ,(hypothetical (+ quadratic 1 -5 6))))
    ((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6)))))
   ()))

;;; But we can ask to solve for z
(check-equal?
 (solve-incremental
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(- (expt y 2) z) (list 'B))
        (make-equation '(- (- y x) 2) (list 'C)))
  '(x y z))
 `(()
  ()
  (((= z 25) (B C A ,(hypothetical (+ quadratic 1 -5 6))))
   ((= y 5) (C A ,(hypothetical (+ quadratic 1 -5 6))))
   ((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6)))))
  ()))

;;; Multiple results can be obtained.
#|
Some things are not right, see solve/solve
(check-equal?
 (solve-incremental
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A)))
  '(x))
 '())

(check-equal?
 (solve-incremental
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(+ (expt x 2) (* -7 x) 10)  (list 'B)))
  '(x))
 '())

(check-equal?
 (solve-incremental
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(+ (expt x 2) (* a x) 10)  (list 'B)))
  '(a x))
 '())
|#

;;; perhaps a bit of filtering would help?

;;; It now knows about some special functions with inverses
(check-equal?
 (solve-incremental
      (list (make-equation '(- 2 (sqrt (+ x 1)))  (list 'A)))
      '(x))
 '(() () (((= x 3) (A))) ()))

(check-equal?
 (solve-incremental
  (list (make-equation '(- 2 (acos (sqrt (+ x 1))))  (list 'A)))
  '(x))
 '(() () (((= x (* -1 (expt (sin 2) 2))) (A))) ())
 #; ;equivalent
 '(() () (((= x (+ -1 (expt (cos 2) 2))) (A))) ()))

;;; A real use. Note how dependencies keep track of contributions to solution
;TODO
#;(let ()
  (define equations
    (list
     (make-equation
      '(+ (* -1/6 eta sr0) (* 1/12 eta sr1) (* -1/2 nu siga0) (* -1/4 nu siga1)
          (* -1/4 nu siga2) (* -1/2 nu siga3) (* -1 nu siga4) (* 1/2 sigd0)
          (* 1/4 sigd1) (* 1/4 sigd2) (* 1/2 sigd3) sigd4)
      (list 'A))
     (make-equation
      '(+ (* -1/4 eta sd1) (* 1/8 eta sr1) (* -1/8 nu siga1) (* 1/8 nu siga2)
          (* 1/8 sigd1) (* -1/8 sigd2))
      (list 'B))
     (make-equation
      '(+ (* -1/4 eta sd1) (* 1/8 eta sr1) (* -1/8 nu siga1) (* 1/8 nu siga2)
          (* 1/8 sigd1) (* -1/8 sigd2))
      (list 'C))
     (make-equation
      '(+ (* -1/4 eta sr1) (* -1/4 nu siga1) (* -1/4 nu siga2) (* -1/2 nu siga3)
          (* 1/4 sigd1) (* 1/4 sigd2) (* 1/2 sigd3))
      (list 'D))
     (make-equation
      '(+ (* -1 eta sd0) (* -1/2 eta sd1) (* -1/2 eta sr0) (* 1/4 eta sr1)
          (* -1/2 nu siga0) (* -1/4 nu siga1) (* 1/4 nu siga2) (* 1/2 sigd0)
          (* 1/4 sigd1) (* -1/4 sigd2))
      (list 'E))
     (make-equation
      '(+ (* -1/8 eta sa0) (* 1/16 eta sd1) (* -1/16 eta sr1) (* 1/16 nu sigd1)
          (* -1/16 nu sigd2) (* -1/16 siga1) (* 1/16 siga2))
      (list 'F))
     (make-equation
      '(+ (* 1/8 eta sa0) (* -1/16 eta sd1) (* 1/16 eta sr1) (* -1/16 nu sigd1)
          (* 1/16 nu sigd2) (* 1/16 siga1) (* -1/16 siga2))
      (list 'G))
     (make-equation
      '(+ (* -3/8 eta sa0) (* -1/2 eta sa1) (* -1/16 eta sd1) (* -3/16 eta sr1)
          (* -1/16 nu sigd1) (* -3/16 nu sigd2) (* -1/4 nu sigd3) (* 1/16 siga1)
          (* 3/16 siga2) (* 1/4 siga3))
      (list 'H))
     (make-equation
      '(+ (* 3/8 eta sa0) (* 1/2 eta sa1) (* 1/16 eta sd1) (* 3/16 eta sr1)
          (* 1/16 nu sigd1) (* 3/16 nu sigd2) (* 1/4 nu sigd3) (* -1/16 siga1)
          (* -3/16 siga2) (* -1/4 siga3))
      (list 'I))
     (make-equation
      '(+ (* -1/4 eta sd0) (* -1/8 eta sd1) (* -1/4 eta sr0) (* 1/8 eta sr1)
          (* -1/4 nu sigd0) (* -1/8 nu sigd1) (* 1/8 nu sigd2) (* 1/4 siga0)
          (* 1/8 siga1) (* -1/8 siga2))
      (list 'J))
     (make-equation
      '(+ (* -1/4 eta sd0) (* -1/8 eta sd1) (* 1/12 eta sr0) (* -1/24 eta sr1)
          (* -1/4 nu sigd0) (* -1/8 nu sigd1) (* -3/8 nu sigd2) (* -1/2 nu sigd3)
          (* -1 nu sigd4) (* 1/4 siga0) (* 1/8 siga1) (* 3/8 siga2) (* 1/2 siga3)
          siga4)
      (list 'K))
     ))
  
  (define unknowns
    '(siga0 siga1 siga2 siga3 siga4 sigd0 sigd1 sigd2 sigd3 sigd4 sa0 sa1 sd0 sd1))

  (define solution (solve-incremental equations unknowns))

  (check-equal?
   solution
   '(()                                 ; no residuals left
     (sa0 sa1 sd0 sd1 siga3 sigd3)      ; excess variables
     (((= sigd4                         ; substitutions
          (/ (+ (* 3 eta nu sa0)
                (* 3 eta nu sa1)
                (* eta nu sr0)
                (* eta nu sr1)
                (* 3 eta sd0)
                (* eta sr0)
                (* eta sr1))
             (+ -3 (* 3 (expt nu 2)))))
       (K A J E H D F B))
      ((= sigd2
          (/ (+ (* -2 eta nu sa0)
                (* -2 eta nu sa1)
                (* -1 eta nu sr1)
                (* -1 (expt nu 2) sigd3)
                (* eta sd1)
                (* -1 eta sr1)
                sigd3)
             (+ -1 (expt nu 2))))
       (H D F B))
      ((= sigd1
          (/ (+ (* -2 eta nu sa1)
                (* -1 eta nu sd1)
                (* -1 (expt nu 2) sigd3)
                (* -1 eta sd1)
                sigd3)
             (+ -1 (expt nu 2))))
       (H D F B))
      ((= sigd0
          (/ (+ (* -1 eta nu sa0)
                (* -1 eta nu sd0)
                (* -1 eta nu sr0)
                (* -2 eta sd0)
                (* -1 eta sr0))
             (+ -1 (expt nu 2))))
       (J E F B))
      ((= siga4
          (/ (+ (* 3 eta nu sd0)
                (* eta nu sr0)
                (* eta nu sr1)
                (* 3 eta sa0)
                (* 3 eta sa1)
                (* eta sr0)
                (* eta sr1))
             (+ -3 (* 3 (expt nu 2)))))
       (K A J E H D F B))

      ((= siga2
          (/ (+ (* eta nu sd1)
                (* -1 eta nu sr1)
                (* -1 (expt nu 2) siga3)
                (* -2 eta sa0)
                (* -2 eta sa1)
                (* -1 eta sr1)
                siga3)
             (+ -1 (expt nu 2))))
       (H D F B))
      ((= siga1
          (/ (+ (* -1 eta nu sd1)
                (* -1 (expt nu 2) siga3)
                (* -2 eta sa1)
                (* -1 eta sd1)
                siga3)
             (+ -1 (expt nu 2))))
       (H D F B))
      ((= siga0
          (/ (+ (* -2 eta nu sd0)
                (* -1 eta nu sr0)
                (* -1 eta sa0)
                (* -1 eta sd0)
                (* -1 eta sr0))
             (+ -1 (expt nu 2))))
       (J E F B)))
     ())  ; no equations considered tough (no way to isolate)
   )
  ;;; Check
  (check-equal?
   (map (lambda (equation)
          (apply-substitutions-to-equation equation
                                           (substitutions solution)))
        equations)
   '((0 (B F H D K E J A) ())
     (0 (F H D B) ())
     (0 (B F H D C) ())
     (0 (B F H D) ())
     (0 (B F H D J E) ())
     (0 (B H D F) ())
     (0 (B F H D G) ())
     (0 (B F D H) ())
     (0 (B F H D I) ())
     (0 (B F H D E J) ())
     (0 (B F H D A E J K) ())))
  
  )

;;; SIMPLE SOLVE
(check-equal?
 (simple-solve
  (up '(+ (* 3 x)     y  -7)
      '(+ (* 3 x) (- y)  -5))
  '(x y))
 '(*solution* ()
              ()
              (((= y 1) (eq:1 eq:0))
               ((= x 2) (eq:1 eq:0)))
              ()) )

(check-equal?
 (simple-solve
  (up '(+ (* 3 (f x))     (g y)  -7)
      '(+ (* 3 (f x)) (- (g y))  -5))
  '((f x) (g y))
  '()
  #t)
 #;(((+ -5 (* 3 G439) (* -1 G440)) (eq:0) (G440 G439)) ((+ -7 (* 3 G439) G440) (eq:1) (G440 G439)))
 '(*solution* ()
              ()
              (((= (g y) 1) (eq:1 eq:0))
               ((= (f x) 2) (eq:1 eq:0)))
              ()) )

(check-equal?
 (simple-solve
  (up '(+ (* 3 (f x))     (g y)  (H q))
      '(+ (* 3 (f x)) (- (g y))  -5))
  '((f x) (g y))
  '((H q))
  #t)
 ;prints:
 ;(((+ -5 (* 3 x57) (* -1 x58)) (eq:0) (x58 x57)) ((+ k59 (* 3 x57) x58) (eq:1) (x58 x57 k59)))
 '(*solution* ()
              ()
              (((= (g y) (+ -5/2 (* -1/2 (H q)))) (eq:1 eq:0))
               ((= (f x) (+ 5/6 (* -1/6 (H q)))) (eq:1 eq:0)))
              ()))

;***************************************************************************************************
;* from solve-utils.rkt                                                                            *
;***************************************************************************************************
;;; Examples of use

(check-equal?
 (solve-equations
  (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
        (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
  '(x y))
 '(full-solutions (() () (((= x 2) (A B)) ((= y 1) (A B))) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(+  x   y   z  1)  (list 'A))
        (make-equation '(+  x   y      2)  (list 'B))
        (make-equation '(+  x          1)  (list 'C)))
  '(x y z))
 '(full-solutions (() () (((= x -1) (C)) ((= y -1) (B C)) ((= z 1) (A B))) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
        (make-equation '(+ (* 3 x)     y  -5)  (list 'B)))
  '(x y))
 '(contradictions (-2 (A B) ()) (2 (B A) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(-  3 (+ x y))                       (list 'A))
        (make-equation '(-  5 (- x y))                       (list 'B))
        (make-equation '(-  3 (+ (* (sqrt x) z) (square y))) (list 'C)))
  '(x y z))
 '(full-solutions (() () (((= x 4) (A B)) ((= y -1) (A B)) ((= z 1) (A B C))) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(+ (* (+ a b) (- a c)) c) (list 'A))
        (make-equation '(- 3 (+ a b))             (list 'B)))
  '(a b c))
 '(underdetermined (() (c) (((= a (* 2/3 c)) (A B)) ((= b (+ 3 (* -2/3 c))) (A B))) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
        (make-equation '(- 3 (- a c))  (list 'B)))
  '(a b c))
 '(underdetermined (() (c) (((= a (+ 3 c)) (B)) ((= b (+ -3 (* -4/3 c))) (A B))) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
        (make-equation '(- 3 (- a b))  (list 'B)))
  '(a b c))
 '(underdetermined
   (()
    (a)
    (((= b (+ -3 a)) (A B))
     ((= c (/ (+ (* 2 (expt a 2)) (* -3 a)) (+ -4 (* 2 a)))) (A B)))
    ())
   (()
    (b)
    (((= a (+ 3 b)) (B))
     ((= c (/ (+ 9 (* 2 (expt b 2)) (* 9 b)) (+ 2 (* 2 b)))) (A B)))
    ())))

(check-equal?
 (solve-equations
  (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
        (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
        (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
  '(x y z))
 '(full-solutions (() () (((= x 2) (A B)) ((= y 1) (A B)) ((= z -1/2) (A B C))) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(- 200/3 (/ 1 (+ (/ 1 R1) (/ 1 R2))))  (list 'A))
        (make-equation '(-  1/3 (/ R2 (+ R1 R2)))  (list 'B)))
  '(R1 R2))
 `(full-solutions
   (()
    ()
    (((= R1 0) (A B ,(hypothetical (- quadratic -6 600 0))))
     ((= R2 0) (A B ,(hypothetical (- quadratic -6 600 0)))))
    ())
   (()
    ()
    (((= R1 200) (A B ,(hypothetical (+ quadratic -6 600 0))))
     ((= R2 100) (A B ,(hypothetical (+ quadratic -6 600 0)))))
    ())))

(check (λ (x y) (lset= equal? x y))
 (solve-equations
  (list (make-equation '(- (* 1/3 (+ R1 R2)) R2)  (list 'B))
        (make-equation '(- (* 200/3 (+ R1 R2)) (* R1 R2))  (list 'A)))
  '(R1 R2))
 `(full-solutions
   (()
    ()
    (((= R1 0) (A B ,(hypothetical (- quadratic -6 600 0))))
     ((= R2 0) (A B ,(hypothetical (- quadratic -6 600 0)))))
    ())
   (()
    ()
    (((= R1 0) (A B ,(hypothetical (- quadratic -2 200 0))))
     ((= R2 0) (A B ,(hypothetical (- quadratic -6 600 0)))))
    ())
   (()
    ()
    (((= R1 0) (A B ,(hypothetical (- quadratic -6 600 0))))
     ((= R2 0) (A B ,(hypothetical (- quadratic -2 200 0)))))
    ())
   (()
    ()
    (((= R1 0) (A B ,(hypothetical (- quadratic -2 200 0))))
     ((= R2 0) (A B ,(hypothetical (- quadratic -2 200 0)))))
    ())
   (()
    ()
    (((= R1 200) (A B ,(hypothetical (+ quadratic -2 200 0))))
     ((= R2 100) (A B ,(hypothetical (+ quadratic -2 200 0)))))
    ())
   (()
    ()
    (((= R1 200) (A B ,(hypothetical (+ quadratic -6 600 0))))
     ((= R2 100) (A B ,(hypothetical (+ quadratic -2 200 0)))))
    ())
   (()
    ()
    (((= R1 200) (A B ,(hypothetical (+ quadratic -2 200 0))))
     ((= R2 100) (A B ,(hypothetical (+ quadratic -6 600 0)))))
    ())
   (()
    ()
    (((= R1 200) (A B ,(hypothetical (+ quadratic -6 600 0))))
     ((= R2 100) (A B ,(hypothetical (+ quadratic -6 600 0)))))
    ())))

(check-equal?
 (solve-equations
  (list (make-equation '(- (expt x 2) 1)  (list 'A))
        (make-equation '(- x 1)  (list 'B)))
  '(x))
 '(full-solutions (() () (((= x 1) (B))) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(- (expt x 2) 1)  (list 'A))
        (make-equation '(- x -1)  (list 'B)))
  '(x))
 '(full-solutions (() () (((= x -1) (B))) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(- (expt y 2) 9) (list 'B))
        (make-equation '(- (- y x) 1) (list 'C)))
  '(x y))
 `(full-solutions
   (()
    ()
    (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
     ((= y 3) (B ,(hypothetical (- quadratic 1 0 -9)))))
    ())))

(check-equal?
 (solve-equations
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(- (expt y 2) 9) (list 'B))
        (make-equation '(- (- y x) 2) (list 'C)))
  '(x y))
 `(contradictions
   (7 (B C A ,(hypothetical (- quadratic 1 -9 20))) ())
   (16 (B C A ,(hypothetical (+ quadratic 1 -9 20))) ())
   (2 (C A B ,(hypothetical (- quadratic 1 0 -9))) ())
   (56 (C A B ,(hypothetical (+ quadratic 1 0 -9))) ())
   (-1
    (,(hypothetical (- quadratic 1 0 -9)) B
                                         C
                                         A
                                         ,(hypothetical (- quadratic 1 -5 6)))
    ())
   (-2
    (,(hypothetical (- quadratic 1 0 -9)) B
                                         C
                                         A
                                         ,(hypothetical (+ quadratic 1 -5 6)))
    ())
   (2 (A C B ,(hypothetical (- quadratic 1 0 -9))) ())
   (2 (A C B ,(hypothetical (- quadratic 1 0 -9))) ())
   (-7
    (,(hypothetical (+ quadratic 1 0 -9)) B
                                         C
                                         A
                                         ,(hypothetical (- quadratic 1 -5 6)))
    ())
   (-8
    (,(hypothetical (+ quadratic 1 0 -9)) B
                                         C
                                         A
                                         ,(hypothetical (+ quadratic 1 -5 6)))
    ())
   (56 (A C B ,(hypothetical (+ quadratic 1 0 -9))) ())
   (56 (A C B ,(hypothetical (+ quadratic 1 0 -9))) ())
   (-1
    (,(hypothetical (- quadratic 1 -5 6)) A
                                         C
                                         B
                                         ,(hypothetical (- quadratic 1 0 -9)))
    ())
   (-7
    (,(hypothetical (- quadratic 1 -5 6)) A
                                         C
                                         B
                                         ,(hypothetical (+ quadratic 1 0 -9)))
    ())
   (7 (B C A ,(hypothetical (- quadratic 1 -5 6))) ())
   (-1
    (,(hypothetical (- quadratic 1 -5 6)) A
                                         C
                                         B
                                         ,(hypothetical (- quadratic 1 0 -9)))
    ())
   (-7
    (,(hypothetical (- quadratic 1 -5 6)) A
                                         C
                                         B
                                         ,(hypothetical (+ quadratic 1 0 -9)))
    ())
   (7 (B C A ,(hypothetical (- quadratic 1 -5 6))) ())
   (-2
    (,(hypothetical (+ quadratic 1 -5 6)) A
                                         C
                                         B
                                         ,(hypothetical (- quadratic 1 0 -9)))
    ())
   (-8
    (,(hypothetical (+ quadratic 1 -5 6)) A
                                         C
                                         B
                                         ,(hypothetical (+ quadratic 1 0 -9)))
    ())
   (16 (B C A ,(hypothetical (+ quadratic 1 -5 6))) ())
   (-2
    (,(hypothetical (+ quadratic 1 -5 6)) A
                                         C
                                         B
                                         ,(hypothetical (- quadratic 1 0 -9)))
    ())
   (-8
    (,(hypothetical (+ quadratic 1 -5 6)) A
                                         C
                                         B
                                         ,(hypothetical (+ quadratic 1 0 -9)))
    ())
   (16 (B C A ,(hypothetical (+ quadratic 1 -5 6))) ())))


(check (λ (x y) (lset= equal? x y))
 (solve-equations
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(- (expt y 2) z) (list 'B))
        (make-equation '(- (- y x) 2) (list 'C)))
  '(x y))
 `(parameters-constrained
   ((((+ 16 (* -1 z)) (B C A ,(hypothetical (- quadratic 1 -5 6))) (z)))
    ()
    (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
     ((= y 4) (A C ,(hypothetical (- quadratic 1 -5 6)))))
    ())
   ((((+ 16 (* -1 z)) (B C A ,(hypothetical (- quadratic 1 -9 20))) (z)))
    ()
    (((= x 2) (A C ,(hypothetical (- quadratic 1 -9 20))))
     ((= y 4) (A C ,(hypothetical (- quadratic 1 -9 20)))))
    ())
   ((((+ 20 z (* 9 (sqrt z))) (A C B ,(hypothetical (- quadratic 1 0 (* -1 z))))
                              (z)))
    ()
    (((= x (+ -2 (* -1 (sqrt z))))
      (B C ,(hypothetical (- quadratic 1 0 (* -1 z)))))
     ((= y (* -1 (sqrt z))) (B ,(hypothetical (- quadratic 1 0 (* -1 z))))))
    ())
   ((((+ 20 z (* -9 (sqrt z))) (A C B ,(hypothetical (+ quadratic 1 0 (* -1 z))))
                               (z)))
    ()
    (((= x (+ -2 (sqrt z))) (B C ,(hypothetical (+ quadratic 1 0 (* -1 z)))))
     ((= y (sqrt z)) (B ,(hypothetical (+ quadratic 1 0 (* -1 z))))))
    ())
   ((((+ 25 (* -1 z)) (B C A ,(hypothetical (+ quadratic 1 -9 20))) (z)))
    ()
    (((= x 3) (A C ,(hypothetical (+ quadratic 1 -9 20))))
     ((= y 5) (A C ,(hypothetical (+ quadratic 1 -9 20)))))
    ())
   ((((+ 25 (* -1 z)) (B C A ,(hypothetical (+ quadratic 1 -5 6))) (z)))
    ()
    (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))
     ((= y 5) (A C ,(hypothetical (+ quadratic 1 -5 6)))))
    ())))

(check (λ (x y) (lset= equal? x y))
 (solve-equations
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(- (expt y 2) z) (list 'B))
        (make-equation '(- (- y x) 2) (list 'C)))
  '(x y z))
 `(full-solutions
   (()
    ()
    (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
     ((= y 4) (A C ,(hypothetical (- quadratic 1 -9 20))))
     ((= z 16) (A B C ,(hypothetical (- quadratic 1 -9 20)))))
    ())
   (()
    ()
    (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
     ((= y 4) (A C ,(hypothetical (- quadratic 1 -5 6))))
     ((= z 16) (A B C ,(hypothetical (- quadratic 1 -9 20)))))
    ())
   (()
    ()
    (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
     ((= y 4) (A C ,(hypothetical (- quadratic 1 -9 20))))
     ((= z 16) (A B C ,(hypothetical (- quadratic 1 -5 6)))))
    ())
   (()
    ()
    (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))
     ((= y 4) (A C ,(hypothetical (- quadratic 1 -5 6))))
     ((= z 16) (A B C ,(hypothetical (- quadratic 1 -5 6)))))
    ())
   (()
    ()
    (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))
     ((= y 5) (A C ,(hypothetical (+ quadratic 1 -5 6))))
     ((= z 25) (A B C ,(hypothetical (+ quadratic 1 -5 6)))))
    ())
   (()
    ()
    (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))
     ((= y 5) (A C ,(hypothetical (+ quadratic 1 -9 20))))
     ((= z 25) (A B C ,(hypothetical (+ quadratic 1 -5 6)))))
    ())
   (()
    ()
    (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))
     ((= y 5) (A C ,(hypothetical (+ quadratic 1 -5 6))))
     ((= z 25) (A B C ,(hypothetical (+ quadratic 1 -9 20)))))
    ())
   (()
    ()
    (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))
     ((= y 5) (A C ,(hypothetical (+ quadratic 1 -9 20))))
     ((= z 25) (A B C ,(hypothetical (+ quadratic 1 -9 20)))))
    ())))

(check-equal?
 (solve-equations
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A)))
  '(x))
 `(full-solutions
   (() () (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))) ())
   (() () (((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6))))) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(+ (expt x 2) (* -7 x) 10)  (list 'B)))
  '(x))
 `(full-solutions
   (() () (((= x 2) (A ,(hypothetical (- quadratic 1 -5 6))))) ())
   (() () (((= x 2) (B ,(hypothetical (- quadratic 1 -7 10))))) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
        (make-equation '(+ (expt x 2) (* a x) 10)  (list 'B)))
  '(a x))
 `(full-solutions
   (()
    ()
    (((= a -7) (A B ,(hypothetical (- quadratic 1 -5 6))))
     ((= x 2) (A ,(hypothetical (- quadratic 1 -5 6)))))
    ())
   (()
    ()
    (((= a -19/3) (A B ,(hypothetical (+ quadratic 1 -5 6))))
     ((= x 3) (A ,(hypothetical (+ quadratic 1 -5 6)))))
    ())))

(check-equal?
 (solve-equations
  (list (make-equation '(- 2 (sqrt (+ x 1)))  (list 'A)))
  '(x))
 `(full-solutions (() () (((= x 3) (A))) ())))

(check-equal?
 (solve-equations
  (list (make-equation '(- 2 (acos (sqrt (+ x 1))))  (list 'A)))
  '(x))
 `(full-solutions (() () (((= x (* -1 (expt (sin 2) 2))) (A))) ())))

(check-within
 (solve-equations
  (list (make-equation '(+ 1 x (square x))  (list 'A)))
  '(x))
 `(full-solutions
   (()
    ()
    (((= x -.5000000000000001+.8660254037844387i)
      (A ,(hypothetical (- quadratic 1 1 1)))))
    ())
   (()
    ()
    (((= x -1/2-.8660254037844386i)
      (A ,(hypothetical (+ quadratic 1 1 1)))))
    ()))
 5e-16)


(check-within
 (solve-equations
  (list (make-equation '(+ 1 x (square x))  (list 'A))
        (make-equation '(+ (square y) 3) (list 'B))
        (make-equation '(- (* 2 x) (-  y 1)) (list 'C)))
  '(x y))
 `(full-solutions
   (()
    ()
    (((= x -1/2+.8660254037844388i) (A ,(hypothetical (- quadratic 1 1 1))))
     ((= y +1.7320508075688776i) (B ,(hypothetical (- quadratic 1 0 3)))))
    ())
   (()
    ()
    (((= x -1/2-.8660254037844386i) (A ,(hypothetical (+ quadratic 1 1 1))))
     ((= y -1.7320508075688772i) (B ,(hypothetical (+ quadratic 1 0 3)))))
    ()))
 5e-16)


