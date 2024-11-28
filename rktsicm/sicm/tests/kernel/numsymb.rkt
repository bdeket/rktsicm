#lang racket/base

(require rackunit
         "../../kernel-intr.rkt"
         "../../general/notes.rkt"
         "../helper.rkt"
         )

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/numsymb"
   #:before (λ () (enable-constructor-simplifications #t))

   (test-case
    "skipper"
    (skip "The simplifying (symb1:???) functions make some weird choices"
          "that can be improved in my opininion. Look for ;; !!! in this test file"))
   ;; cstm
   (test-case
    "enable-constructor-simplifications"
    (parameterize ([enable-constructor-simplifications? #t])
      (enable-constructor-simplifications #f)
      (check-false (enable-constructor-simplifications?)))
    (parameterize ([enable-constructor-simplifications? #f])
      (enable-constructor-simplifications #t)
      (check-true (enable-constructor-simplifications?)))
    (check-exn #px"argument must be a boolean" (λ () (enable-constructor-simplifications 'test)))
    )
   (test-case
    "numerical-expression"
    (check-false heuristic-number-canonicalizer)
    (check-false numerical-expression-canonicalizer)
    (check-equal? (numerical-expression #i11/10) #i11/10)
    (check-equal? (numerical-expression 5) 5)
    (check-equal? (numerical-expression 'a) 'a)
    (check-equal? (numerical-expression (literal-number 'c)) 'c)
    (check-equal? (numerical-expression ((literal-function 'f) 't)) '(f t))
    (check-equal? (numerical-expression (down 4 8)) (down 4 8))
    (check-equal? (numerical-expression '(4 8)) '(4 8))
    (check-equal? (numerical-expression #(4 8)) #(4 8))
    )
   (test-case
    "make-numsymb-expression"
    (check-false (incremental-simplifier))
    (check-not-false (hash-ref symbolic-operator-table '+ #f))
    (check-equal? (expression (make-numsymb-expression '+ '(a b))) '(+ a b))
    (check-equal? (expression (make-numsymb-expression '+ '(1 2))) 3)
    (check-false (hash-ref symbolic-operator-table 'Q #f))
    (check-equal? (expression (make-numsymb-expression 'Q '(a b))) '(Q a b))
    (parameterize ([incremental-simplifier (λ (x) 8)])
      (check-equal? (expression (make-numsymb-expression '+ '(1 2))) 8)))
   ;; main - complex numbers
   (test-case
    "make-rectangular"
    (check-true (make-rectangular? '(make-rectangular a b)))
    (check-equal? (symb:make-rectangular 'a 0) 'a)
    (check-equal? (symb:make-rectangular 2 3) 2+3i)
    (check-equal? (symb:make-rectangular 'a 'b) '(+ a (* +i b))))
   (test-case
    "make-polar"
    (check-true (make-polar? '(make-polar 'a 'b)))
    (check-equal? (symb:make-polar 'a 0) 'a)
    (check-equal? (symb:make-polar 0 'a) 0)
    (check-equal? (symb:make-polar 2 3) (make-polar 2 3))
    (check-equal? (symb:make-polar 'a 'b) '(* a (+ (cos b) (* +i (sin b))))))
   (test-case
    "real-part / imag-part / conjugate"
    (check-true (real-part? '(real-part 'a)))
    (check-equal? (symb:real-part 2+3i) 2)
    (check-equal? (symb:real-part 'a) '(* 1/2 (+ a (conjugate a))))
    (check-true (imag-part? '(imag-part 'a)))
    (check-equal? (symb:imag-part 2+3i) 3)
    (check-equal? (symb:imag-part 'a) '(* -1/2i (- a (conjugate a))))
    (check-true (conjugate? '(conjugate 'a)))
    (check-equal? (symb:conjugate 2+3i) 2-3i)
    (check-equal? (symb:conjugate 2) 2)
    (declare-known-reals 'this-is-always-real)
    (check-equal? (symb:conjugate 'this-is-always-real) 'this-is-always-real)
    (check-equal? (symb:conjugate 'a) '(conjugate a))
    (check-equal? (symb:conjugate '(log a)) '(log (conjugate a))))
   (test-case
    "magnitude / angle"
    (check-true (magnitude? '(magnitude a)))
    (check-equal? (symb:magnitude 3+4i) 5)
    (check-equal? (symb:magnitude 1+1i) '(sqrt 2))
    (check-equal? (symb:magnitude 1.+1.i) (sqrt 2))
    (check-equal? (symb:magnitude 'a) '(sqrt (* (conjugate a) a)))
    (check-true (angle? '(angle a)))
    (check-equal? (symb:angle 3+0i) 0)
    (check-equal? (symb:angle 1+1i) '(atan 1 1))
    (check-equal? (symb:angle 1.+1.i) (atan 1 1))
    (check-equal? (symb:angle 'a) `(atan ,(symb:imag-part 'a) ,(symb:real-part 'a))))

   ;; main - logical
   (test-case
    "logical operators"
    ;; and
    (check-equal? (symb:and 'a 'b 'c) '(and a b c))
    (check-equal? (symb:and 'a 'b #f) #f)
    ;; or
    (check-equal? (symb:or 'a 'b 'c) '(or a b c))
    (check-equal? (symb:or 'a 'b #t) #t)
    ;; not
    (check-equal? (symb:not 'a) '(not a))
    (check-equal? (symb:not #t) #f)
    (check-equal? (symb:not #f) #t)
    ;; if
    (check-equal? (symb:if 'a 'b 'c) '(if a b c))
    ;; =
    (check-true (equality? '(= a b c)))
    (check-equal? (symb:=) #t)
    (check-equal? (symb:= 'a) #t)
    (check-equal? (symb:= 'a) #t)
    (check-equal? (symb:= 1 1) #t)
    (check-equal? (symb:= 1 1.) #f)
    (check-equal? (symb:= 1. 1.) #f)
    (check-equal? (symb:= 1 'a) #f)
    (check-equal? (symb:= 'a 1) #f)
    (check-equal? (symb:= 'a 'a) #t)
    (check-equal? (symb:= 'a 'b) '(= a b))
    (check-equal? (symb:= #(a) #(b)) '(= #(a) #(b)))
    (check-equal? (symb:= #(a a) #(a a)) #t)
    (check-equal? (symb:= 'a 'b 'c) '(and (= a b) (= b c)))
    ;; 0?
    (check-equal? (symb:zero? 0) #t)
    (check-equal? (symb:zero? 2) #f)
    (check-equal? (symb:zero? 'a) `(= ,:zero a))
    (check-equal? (symb:one? 1) #t)
    (check-equal? (symb:one? 2) #f)
    (check-equal? (symb:one? 'a) `(= ,:one a))
    )

   ;; main - + / -
   (test-case
    "+ / -"
    (check-true (sum? '(+ 2 3 4)))
    (check-false (sum? '(- 2 3 4)))
    (check-equal? (symb:addends '(+ 2 3 4)) '(2 3 4))
    (check-equal? (symb:+ 1 2) 3)
    (check-equal? (symb:+ 1 2.5) (+ 1 2.5))
    (check-equal? (symb:+ 0 'a) 'a)
    (check-equal? (symb:+ 'b '(+ 2 a)) '(+ b 2 a))
    (check-equal? (symb:+ 1 '(+ 2 a)) '(+ 1 2 a))
    (check-equal? (symb:+ 1 'a) '(+ 1 a))
    (check-equal? (symb:+ 'b 0) 'b)
    (check-equal? (symb:+ '(+ 2 b) 'a) '(+ 2 b a))
    (check-equal? (symb:+ '(+ 2 b) 3) '(+ 3 2 b))
    (check-equal? (symb:+ 'b 3) '(+ 3 b))
    (check-equal? (symb:+ '(+ 2 b) '(+ 3 a)) '(+ 2 b 3 a))
    (check-equal? (symb:+ 'b 'a) '(+ b a))

    (check-equal? (symb:add:n '()) 0)
    (check-equal? (symb:add:n '(a)) 'a)
    (check-equal? (symb:add:n '(a b c)) '(+ a b c))
    (check-equal? (symb:sum 'a 'b 'c) '(+ a b c))

    (check-true (difference? '(- 2 3 4)))
    (check-false (difference? '(+ 2 -3)))
    (check-equal? (symb:minuend '(-)) 0)
    (check-equal? (symb:minuend '(- a)) 0)
    (check-equal? (symb:minuend '(- a b c)) 'a)
    (check-equal? (symb:subtrahend '(-)) 0)
    (check-equal? (symb:subtrahend '(- a)) 'a)
    (check-equal? (symb:subtrahend '(- a b)) 'b)
    (check-equal? (symb:subtrahend '(- a b c)) '(+ b c))
    (check-equal? (symb:- 1 2) -1)
    (check-equal? (symb:- 1 2.5) (- 1 2.5))
    (check-equal? (symb:- 0 'a) '(- a))
    (check-equal? (symb:- 'b '(+ 2 a)) '(- b (+ 2 a)))
    (check-equal? (symb:- 1 '(+ 2 a)) '(- 1 (+ 2 a)))
    (check-equal? (symb:- 1 'a) '(- 1 a))
    (check-equal? (symb:- 'b 0) 'b)
    (check-equal? (symb:- '(+ 2 b) 'a) '(- (+ 2 b) a))
    (check-equal? (symb:- '(+ 2 b) 3) '(- (+ 2 b) 3))
    (check-equal? (symb:- 'b 3) '(- b 3))
    (check-equal? (symb:- '(+ 2 b) '(+ 3 a)) '(- (+ 2 b) (+ 3 a)))
    (if allow-nary-difference-quotient
        (check-equal? (symb:- '(- 2 b) '(+ 3 a)) '(- 2 (+ b 3 a)))
        (check-equal? (symb:- '(- 2 b) '(+ 3 a)) '(- (- 2 b) (+ 3 a))))
    (check-equal? (symb:- 'b 'a) '(- b a))

    (check-true (negate? '(negate a)))
    (check-equal? (symb:negate 'a) '(- a))
    
    (check-equal? (symb:dif:n '()) 0)
    (check-equal? (symb:dif:n '(a)) '(- a))
    (check-equal? (symb:dif:n '(a b c)) '(- a (+ b c)))
    (check-equal? (symb:difference 'a 'b 'c) '(- a (+ b c)))

    (check-equal? (symb1:+ '(+ 2 3 a) '(+ 2 b c)) '(+ 7 a b c))
    (check-equal? (symb1:+ '(+ 2 3 a) '(- 2 b c)) '(- (+ 7 a) (+ b c)))
    (check-equal? (symb1:+ '(+ 2 3 a) '(- 2)) '(+ 3 a))
    (check-equal? (symb1:+ '(+ 2 3 a) 2) '(+ 7 a))
    (check-equal? (symb1:+ '(- 2 3 a) '(+ 2 b c)) '(- (+ 1 b c) a))
    (check-equal? (symb1:+ '(- 2 3 a) '(- 2 b c)) '(- 1 (+ a b c)))
    (check-equal? (symb1:+ '(- 2 3 a) '(- 2)) '(- -3 a))
    (check-equal? (symb1:+ '(- 2 3 a) 2) '(- 1 a))
    (check-equal? (symb1:+ '(- a) '(+ 2 b c)) '(- (+ 2 b c) a))
    (check-equal? (symb1:+ '(- a) '(- 2 b c)) '(- 2 (+ a b c)))
    (check-equal? (symb1:+ '(- a) '(- b)) '(- (+ a b)))
    (check-equal? (symb1:+ '(- a) 2) '(- 2 a))
    (check-equal? (symb1:+ 'a '(+ 2 b c)) '(+ 2 a b c))
    (check-equal? (symb1:+ 'a '(- 2 b c)) '(- (+ 2 a) (+ b c)))
    (check-equal? (symb1:+ 'a '(- c)) '(- a c))
    (check-equal? (symb1:+ 'a 'b) '(+ a b))
    (check-equal? (symb1:+ '(+ 2 a c) '(+ 2 a c)) '(+ 4 a c a c))
    (check-equal? (symb1:+ 'a '(- a)) 0)
    (check-equal? (symb1:+ '(+ 2 3) '(- -6 -1)) 0)
    (check-equal? (symb1:+ '(+ 2 3 a) '(- -6 -1)) 'a)
    (check-equal? (symb1:+ '(+ 2 3) '(+ -6 1)) 0)
    (check-equal? (symb1:+ '(+ 2 3 a) '(+ -6 1)) 'a)

    (check-equal? (symb1:- '(+ 2 3 a) '(+ 2 b c)) '(- (+ 3 a) (+ b c)))
    (check-equal? (symb1:- '(+ 2 3 a) '(- 2 b c)) '(+ 3 a b c))
    (check-equal? (symb1:- '(+ 2 3 a) '(- 2)) '(+ 7 a))
    (check-equal? (symb1:- '(+ 2 3 a) 2) '(+ 3 a))
    (check-equal? (symb1:- '(- 2 3 a) '(+ 2 b c)) '(- -3 (+ a b c)))
    (check-equal? (symb1:- '(- 2 3 a) '(- 2 b c)) '(- (+ -3 b c) a))
    (check-equal? (symb1:- '(- 2 3 a) '(- 2)) '(- 1 a))
    (check-equal? (symb1:- '(- 2 3 a) 2) '(- -3 a))
    (check-equal? (symb1:- '(- a) '(+ 2 b c)) '(- -2 (+ a b c)))
    (check-equal? (symb1:- '(- a) '(- 2 b c)) '(- (+ -2 b c) a))
    (check-equal? (symb1:- '(- a) '(- b)) '(- b a))
    (check-equal? (symb1:- '(- a) 2) '(- -2 a))
    (check-equal? (symb1:- 'a '(+ 2 b c)) '(- (+ -2 a) (+ b c)))
    (check-equal? (symb1:- 'a '(- 2 b c)) '(+ -2 a b c))
    (check-equal? (symb1:- 'a '(- c)) '(+ a c))
    (check-equal? (symb1:- 'a 'b) '(- a b))
    (check-equal? (symb1:- '(+ 2 a) '(+ 2 b)) '(- a b))
    (check-equal? (symb1:- '(+ 2 a) '(+ 2 a)) 0)
    (check-equal? (symb1:- '(+ 2 a c) '(+ 2 b)) '(- (+ a c) b))
    (check-equal? (symb1:- '(+ 2 a c) '(+ 2 a c)) 0)
    (check-equal? (symb1:- '(+ 2 a c) '(+ 2 b d)) '(- (+ a c) (+ b d)))
    ;; !!!
    (check-equal? (symb1:- '(+ 2 a c) '(+ 2 c a)) '(- (+ a c) (+ c a)))

    (parameterize ([enable-constructor-simplifications? #f])
      (check-equal? (symb:add '(+ 2 3 a) '(+ 2 b 8)) '(+ 2 3 a 2 b 8))
      (check-equal? (symb:dif '(+ 2 3 a) '(+ 2 b 8)) '(- (+ 2 3 a) (+ 2 b 8))))
    (parameterize ([enable-constructor-simplifications? #t])
      (check-equal? (symb:add '(+ 2 3 a) '(+ 2 b 8)) '(+ 15 a b))
      (check-equal? (symb:dif '(+ 2 3 a) '(+ 2 b 8)) '(- (+ -5 a) b))))

   (test-case
    "* / /"
    (check-true (product? '(* 2 3 4)))
    (check-false (product? '(/ 2 3 4)))
    (check-equal? (symb:multiplicands '(* 2 3 4)) '(2 3 4))
    (check-equal? (symb:* 1 2) 2)
    (check-equal? (symb:* 1 2.5) (* 1 2.5))
    (check-equal? (symb:* 0 'a) 0)
    (check-equal? (symb:* 1 'a) 'a)
    (check-equal? (symb:* 'b '(* 2 a)) '(* b 2 a))
    (check-equal? (symb:* 2 '(* 2 a)) '(* 2 2 a))
    (check-equal? (symb:* 2 'a) '(* 2 a))
    (check-equal? (symb:* 'b 0) 0)
    (check-equal? (symb:* 'b 1) 'b)
    (check-equal? (symb:* '(* 2 b) 'a) '(* 2 b a))
    (check-equal? (symb:* '(* 2 b) 3) '(* 3 2 b))
    (check-equal? (symb:* 'b 3) '(* 3 b))
    (check-equal? (symb:* '(* 2 b) '(* 3 a)) '(* 2 b 3 a))
    (check-equal? (symb:* 'b 'a) '(* b a))

    (check-equal? (symb:mul:n '()) 1)
    (check-equal? (symb:mul:n '(a)) 'a)
    (check-equal? (symb:mul:n '(a b c)) '(* a b c))
    (check-equal? (symb:product 'a 'b 'c) '(* a b c))

    (check-true (quotient? '(/ 2 3 4)))
    (check-false (quotient? '(* 2 -3)))
    (check-equal? (symb:dividend '(/ )) 1)
    (check-equal? (symb:dividend '(/ a)) 1)
    (check-equal? (symb:dividend '(/ a b c)) 'a)
    (check-equal? (symb:divisor '(/)) 1)
    (check-equal? (symb:divisor '(/ a)) 'a)
    (check-equal? (symb:divisor '(/ a b)) 'b)
    (check-equal? (symb:divisor '(/ a b c)) '(* b c))
    (check-equal? (symb:/ 1 2) 1/2)
    (check-equal? (symb:/ 1 2.5) (/ 1 2.5))
    (check-equal? (symb:/ 0 'a) 0)
    (check-equal? (symb:/ 1 'a) '(/ 1 a))
    (check-equal? (symb:/ 'b '(* 2 a)) '(/ b (* 2 a)))
    (check-equal? (symb:/ 1 '(* 2 a)) '(/ 1 (* 2 a)))
    (check-equal? (symb:/ 1 'a) '(/ 1 a))
    (check-exn #px"Divide by zero -- MAKE-QUO" (λ () (symb:/ 'b 0)))
    (check-equal? (symb:/ '(* 2 b) 'a) '(/ (* 2 b) a))
    (check-equal? (symb:/ '(* 2 b) 1) '(* 2 b))
    (check-equal? (symb:/ '(* 2 b) 3) '(/ (* 2 b) 3))
    (check-equal? (symb:/ 'b 3) '(/ b 3))
    (check-equal? (symb:/ '(* 2 b) '(* 3 a)) '(/ (* 2 b) (* 3 a)))
    (if allow-nary-difference-quotient
        (check-equal? (symb:/ '(/ 2 b) '(* 3 a)) '(/ 2 (* b 3 a)))
        (check-equal? (symb:/ '(/ 2 b) '(* 3 a)) '(/ (/ 2 b) (* 3 a))))
    (check-equal? (symb:/ 'b 'a) '(/ b a))

    (check-true (invert? '(invert a)))
    (check-equal? (symb:invert 'a) '(/ 1 a))
    
    (check-equal? (symb:quo:n '()) 1)
    (check-equal? (symb:quo:n '(a)) '(/ 1 a))
    (check-equal? (symb:quo:n '(a b c)) '(/ a (* b c)))
    (check-equal? (symb:quotient 'a 'b 'c) '(/ a (* b c)))

    (check-equal? (symb1:* '(* 2 3 a) '(* 2 b c)) '(* 12 a b c))
    (check-equal? (symb1:* '(* 2 3 a) '(/ 2 b c)) '(/ (* 12 a) (* b c)))
    ;; !!!
    (check-equal? (symb1:* '(* 2 3 a) '(/ 2)) '(/ (* 6 a) 2))
    (check-equal? (symb1:* '(* 2 3 a) 2) '(* 12 a))
    (check-equal? (symb1:* '(/ 2 3 a) '(* 2 b c)) '(/ (* 4 b c) (* 3 a)))
    (check-equal? (symb1:* '(/ 2 3 a) '(/ 2 b c)) '(/ 4 (* 3 a b c)))
    ;; !!!
    (check-equal? (symb1:* '(/ 2 3 a) '(/ 2)) '(/ 2 (* 6 a)))
    (check-equal? (symb1:* '(/ 2 3 a) 2) '(/ 4 (* 3 a)))
    (check-equal? (symb1:* '(/ a) '(* 2 b c)) '(/ (* 2 b c) a))
    (check-equal? (symb1:* '(/ a) '(/ 2 b c)) '(/ 2 (* a b c)))
    (check-equal? (symb1:* '(/ a) '(/ b)) '(/ 1 (* a b)))
    (check-equal? (symb1:* '(/ a) 2) '(/ 2 a))
    (check-equal? (symb1:* 'a '(* 2 b c)) '(* 2 a b c))
    (check-equal? (symb1:* 'a '(/ 2 b c)) '(/ (* 2 a) (* b c)))
    (check-equal? (symb1:* 'a '(/ c)) '(/ a c))
    (check-equal? (symb1:* 'a 'b) '(* a b))
    (check-equal? (symb1:* '(* 2 a c) '(* 2 a c)) '(* 4 a c a c))
    (check-equal? (symb1:* 'a '(/ a)) 1)
    (check-equal? (symb1:* '(* 3 4) '(/ 1 2 6)) 1)
    ;; !!!
    (check-equal? (symb1:* '(* 3 4 a) '(/ 1 2 6)) '(/ (* 12 a) 12))
    (check-equal? (symb1:* '(* 3 4) '(* 1/2 1/6)) 1)
    (check-equal? (symb1:* '(* 3 4 a) '(* 1/2 1/6)) 'a)
    (check-exn #px"zero divide in mulup-args" (λ () (symb1:* '(* 3 4 a) '(/ 1/2 1/6 c 0))))
    (check-equal? (symb1:* '(* 3 4 a) '(* 1/2 1/6 c 0)) 0)

    ;; !!!
    (check-equal? (symb1:/ '(* 2 3 a) '(* 2 b c)) '(/ (* 6 a) (* 2 b c)))
    ;; !!!
    (check-equal? (symb1:/ '(* 2 3 a) '(/ 2 b c)) '(/ (* 6 a b c) 2))
    (check-equal? (symb1:/ '(* 2 3 a) '(/ 2)) '(* 12 a))
    (check-equal? (symb1:/ '(* 2 3 a) 2) '(/ (* 6 a) 2))
    (check-equal? (symb1:/ '(/ 2 3 a) '(* 2 b c)) '(/ 2 (* 6 a b c)))
    (check-equal? (symb1:/ '(/ 2 3 a) '(/ 2 b c)) '(/ (* 2 b c) (* 6 a)))
    (check-equal? (symb1:/ '(/ 2 3 a) '(/ 2)) '(/ 4 (* 3 a)))
    (check-equal? (symb1:/ '(/ 2 3 a) 2) '(/ 2 (* 6 a)))
    (check-equal? (symb1:/ '(/ a) '(* 2 b c)) '(/ 1 (* 2 a b c)))
    (check-equal? (symb1:/ '(/ a) '(/ 2 b c)) '(/ (* b c) (* 2 a)))
    (check-equal? (symb1:/ '(/ a) '(/ b)) '(/ b a))
    (check-equal? (symb1:/ '(/ a) 2) '(/ 1 (* 2 a)))
    (check-equal? (symb1:/ 'a '(* 2 b c)) '(/ a (* 2 b c)))
    (check-equal? (symb1:/ 'a '(/ 2 b c)) '(/ (* a b c) 2))
    (check-equal? (symb1:/ 'a '(/ c)) '(* a c))
    (check-equal? (symb1:/ 'a 'b) '(/ a b))
    ;; !!!
    (check-equal? (symb1:/ '(* 2 a) '(* 2 b)) '(/ (* 2 a) (* 2 b)))
    (check-equal? (symb1:/ '(* 2 a) '(* 2 a)) 1)
    ;; !!!
    (check-equal? (symb1:/ '(* 2 a c) '(* 2 b)) '(/ (* 2 a c) (* 2 b)))
    (check-equal? (symb1:/ '(* 2 a c) '(* 2 a c)) 1)
    ;; !!!
    (check-equal? (symb1:/ '(* 2 a c) '(* 2 b d)) '(/ (* 2 a c) (* 2 b d)))
    (check-equal? (symb1:/ '(* 2 a c) '(* 2 c a)) '(/ (* 2 a c) (* 2 c a)))

    (parameterize ([enable-constructor-simplifications? #f])
      (check-equal? (symb:mul '(* 2 3 a) '(* 2 b 8)) '(* 2 3 a 2 b 8))
      (check-equal? (symb:quo '(* 2 3 a) '(* 2 b 8)) '(/ (* 2 3 a) (* 2 b 8))))
    (parameterize ([enable-constructor-simplifications? #t])
      (check-equal? (symb:mul '(* 2 3 a) '(* 2 b 8)) '(* 96 a b))
      (check-equal? (symb:quo '(* 2 3 a) '(* 2 b 8)) '(/ (* 6 a) (* 16 b)))))

   (test-case
    "abs"
    (check-true (abs? '(abs a)))
    (check-equal? (symb:abs -2) 2)
    (check-equal? (symb:abs 'a) '(abs a)))
   (test-case
    "expt / square / cube / sqrt"
    (check-true (expt? '(expt a 3)))
    (check-true (expt? '(expt a c)))
    (check-true (square? '(square a)))
    ;; !!!
    ;(check-true (square? '(expt a 2)))
    (check-true (cube? '(cube a)))
    ;; !!!
    ;(check-true (cube? '(expt a 3)))
    (check-true (sqrt? '(sqrt a)))
        
    (check-equal? (symb:expt 2 3) (expt 2 3))
    (check-equal? (symb:expt 0 'a) '(expt 0 a))
    (check-equal? (symb:expt 1 'a) 1)
    (check-equal? (symb:expt 2 'a) '(expt 2 a))
    (check-equal? (symb:expt 'a 0) 1)
    (check-equal? (symb:expt 'a 1) 'a)
    (check-equal? (symb:expt 'a 2) '(expt a 2))
    (check-equal? (symb:expt '(sqrt a) 6) '(expt a 3))
    (check-equal? (symb:expt '(expt a 4) 5) '(expt a 20))
    (check-equal? (symb:expt '(expt a 2.5) 6) '(expt a 15.))
    (check-equal? (symb:expt 'a -2) '(/ 1 (expt a 2)))
    (check-equal? (symb:expt 'a '(- c)) '(expt a (- c)))

    (check-equal? (symb:square -2) 4)
    (check-equal? (symb:square 'a) '(expt a 2))
    (check-equal? (symb:cube -2) -8)
    (check-equal? (symb:cube 'a) '(expt a 3))

    (check-equal? (symb:sqrt 'a) '(sqrt a))
    (check-equal? (symb:sqrt 4) 2)
    (check-equal? (symb:sqrt 4.) 2.)
    (check-equal? (symb:sqrt 0) 0)
    (check-equal? (symb:sqrt 1) 1)
    (check-equal? (symb:sqrt 3) `(sqrt 3)))
   (test-case
    "exp / log"
    (check-true (exp? '(exp a)))
    (check-true (log? '(log a)))

    (check-equal? (symb:exp 2.) (exp 2.))
    (check-equal? (symb:exp 0) 1)
    (check-equal? (symb:exp 1) `(exp 1))
    (check-equal? (symb:exp 'a) `(exp a))

    (check-equal? (symb:log 2.) (log 2.))
    (check-equal? (symb:log 1) 0)
    (check-equal? (symb:log 2) `(log 2))
    (check-equal? (symb:log 'a) `(log a)))

   (test-case
    "almost-integer"
    (check-true (almost-integer? (random 10)))
    (define δ (* .999 relative-integer-tolerance))
    (define Δ (* .999 absolute-integer-tolerance))
    (if heuristic-sin-cos-simplify
        (begin
          (check-true (almost-integer? 4.))
          (check-true (almost-integer? Δ))
          (check-true (almost-integer? (+ 4. δ))))
        (begin
          (check-false (almost-integer? 4.))
          (check-false (almost-integer? Δ))
          (check-false (almost-integer? (+ 4. δ)))))
    (check-not-false (   n:zero-mod-pi? (- (* 2 :pi) δ)))
    (check-false     (   n:zero-mod-pi? (- (* 1/2 :pi) δ)))
    (check-not-false (symb:zero-mod-pi? ':-pi))
    
    (check-not-false (   n:pi/2-mod-2pi? (- (* 17 :pi/2) δ)))
    (check-false     (   n:pi/2-mod-2pi? (- (* 15 :pi/2) δ)))
    (check-not-false (symb:pi/2-mod-2pi? ':+pi/2))
    
    (check-not-false (   n:-pi/2-mod-2pi? (- (* 13 :-pi/2) δ)))
    (check-false     (   n:-pi/2-mod-2pi? (- (* 11 :-pi/2) δ)))
    (check-not-false (symb:-pi/2-mod-2pi? ':-pi/2))
    
    (check-not-false (   n:pi/2-mod-pi? (- (* 13 :pi/2) δ)))
    (check-false     (   n:pi/2-mod-pi? (- (* 12 :pi/2) δ)))
    (check-not-false (symb:pi/2-mod-pi? ':-pi/2))
    
    (check-not-false (   n:zero-mod-2pi? (- (* 6 :pi) δ)))
    (check-false     (   n:zero-mod-2pi? (- (* 5 :pi) δ)))
    (check-not-false (symb:zero-mod-2pi? ':2pi))

    (check-not-false (   n:pi-mod-2pi? (- (* 5 :pi) δ)))
    (check-false     (   n:pi-mod-2pi? (- (* 6 :pi) δ)))
    (check-not-false (symb:pi-mod-2pi? ':-pi))
    
    (check-not-false (   n:pi/4-mod-pi? (- (* 13 :pi/4) δ)))
    (check-false     (   n:pi/4-mod-pi? (- (* 12 :pi/4) δ)))
    (check-not-false (symb:pi/4-mod-pi? ':pi/4))
    
    (check-not-false (   n:-pi/4-mod-pi? (- (* 13 :-pi/4) δ)))
    (check-false     (   n:-pi/4-mod-pi? (- (* 11 :-pi/4) δ)))
    (check-not-false (symb:-pi/4-mod-pi? ':-pi/4)))

   (test-case
    "sin-like"
    (check-true (sin? '(sin a)))
    (check-equal? (symb:sin 0) 0)
    (check-equal? (symb:sin 1) `(sin 1))
    (check-equal? (symb:sin 1e-30) 0.)
    (check-equal? (symb:sin :pi/2) 1.)
    (check-equal? (symb:sin :-pi/2) -1.)
    (check-equal? (symb:sin 3.) (sin 3.))
    (check-equal? (symb:sin ':pi) 0)
    (check-equal? (symb:sin ':pi/2) 1)
    (check-equal? (symb:sin ':-pi/2) -1)
    (check-equal? (symb:sin 'a) '(sin a))
    (check-equal? (symb:sin '(* 2 :pi)) '(sin (* 2 :pi)))
    
    (check-true (csc? '(csc a)))
    (check-equal? (symb:csc 3.) (csc 3.))
    (check-exn #px"Zero argument -- CSC" (λ () (symb:csc 0)))
    (check-equal? (symb:csc 3) '(/ 1 (sin 3)))
    (check-equal? (symb:csc 'a) '(/ 1 (sin a)))
    ;; !!!
    ;(check-equal? (symb:csc ':pi) '(/ 1 0))
    
    (check-true (asin? '(asin a)))
    (check-equal? (symb:asin 3.) (asin 3.))
    (check-equal? (symb:asin 0) 0)
    (check-equal? (symb:asin 3) '(asin 3))
    ;; !!!
    ;(check-equal? (symb:asin 1) ':pi/2)
    ;(check-equal? (symb:asin -1) ':-pi/2)
    (check-equal? (symb:asin 'a) '(asin a))
    
    (check-true (sinh? '(sinh a)))
    (check-equal? (symb:sinh 3.) (sinh 3.))
    (check-equal? (symb:sinh 0) (sinh 0))
    (check-equal? (symb:sinh 3) '(sinh 3))
    (check-equal? (symb:sinh 'a) '(sinh a)))
   
   (test-case
    "cos-like"
    (check-true (cos? '(cos a)))
    (check-equal? (symb:cos 0) 1)
    (check-equal? (symb:cos 1) `(cos 1))
    (check-equal? (symb:cos 1e-30) 1.)
    (check-equal? (symb:cos :pi/2) 0.)
    (check-equal? (symb:cos :pi) -1.)
    (check-equal? (symb:cos 3.) (cos 3.))
    (check-equal? (symb:cos ':pi) -1)
    (check-equal? (symb:cos ':pi/2) 0)
    (check-equal? (symb:cos ':2pi) 1)
    (check-equal? (symb:cos 'a) '(cos a))
    (check-equal? (symb:cos '(* 2 :pi)) '(cos (* 2 :pi)))
    
    (check-true (sec? '(sec a)))
    (check-equal? (symb:sec 3.) (sec 3.))
    ;; !!
    ;(check-exn #px"Zero argument -- SEC" (λ () (symb:csc ':pi/2)))
    (check-equal? (symb:sec 3) '(/ 1 (cos 3)))
    (check-equal? (symb:sec 'a) '(/ 1 (cos a)))
    ;; !!!
    ;(check-equal? (symb:sec ':pi/2) '(/ 1 0))
    
    (check-true (acos? '(acos a)))
    (check-equal? (symb:acos 3.) (acos 3.))
    (check-equal? (symb:acos 1) 0)
    (check-equal? (symb:acos 3) '(acos 3))
    ;; !!!
    ;(check-equal? (symb:acos 0) ':pi/2)
    ;(check-equal? (symb:acos -1) ':pi)
    (check-equal? (symb:acos 'a) '(acos a))
    
    (check-true (cosh? '(cosh a)))
    (check-equal? (symb:cosh 3.) (cosh 3.))
    (check-equal? (symb:cosh 0) (cosh 0))
    (check-equal? (symb:cosh 3) '(cosh 3))
    (check-equal? (symb:cosh 'a) '(cosh a)))

   (test-case
    "tan-like"
    (check-true (tan? '(tan a)))
    (check-equal? (symb:tan 0) 0)
    (check-equal? (symb:tan 1) `(tan 1))
    (check-equal? (symb:tan 1e-30) 0.)
    (check-equal? (symb:tan :pi/4) 1.)
    (check-equal? (symb:tan (+ :-pi/4 :pi)) -1.)
    (check-exn #px"Undefined -- TAN" (λ () (symb:tan :pi/2)))
    (check-equal? (symb:tan 3.) (tan 3.))
    (check-equal? (symb:tan ':pi) 0)
    (check-exn #px"Undefined -- TAN" (λ () (symb:tan ':-pi/2)))
    (check-equal? (symb:tan ':pi/4) 1)
    (check-equal? (symb:tan ':-pi/4) -1)
    (check-equal? (symb:tan 'a) '(tan a))
    (check-equal? (symb:tan '(+ :pi/4 :pi)) '(tan (+ :pi/4 :pi)))
    
    (check-true (atan? '(atan a)))
    (check-equal? (symb:atan 3.) (atan 3.))
    (check-equal? (symb:atan 0) 0)
    ;; !!!
    ;(check-equal? (symb:atan 1) ':pi/4)
    ;(check-equal? (symb:atan -1) ':-pi/4)
    (check-equal? (symb:atan 3) '(atan 3))
    (check-equal? (symb:atan 'a) '(atan a))
    
    (check-equal? (symb:atan 0 1) 0)
    (check-equal? (symb:atan 0 -2) ':pi)
    (begin
      (clear-notes!)
      (check-equal? (symb:atan 0 'x) 0)
      (check-equal? *notes* '((assuming (positive? x)))))

    (check-equal? (symb:atan -3 0) ':-pi/2)
    (check-equal? (symb:atan  2 0) ':pi/2)
    (begin
      (clear-notes!)
      (check-equal? (symb:atan 'y 0) ':pi/2)
      (check-equal? *notes* '((assuming (positive? y)))))
    (check-equal? (symb:atan 1 1.) (atan 1 1.))
    (check-equal? (symb:atan 1. 1) (atan 1. 1))
    ;; !!!
    (check-equal? (symb:atan 1 1) '(atan 1 1)) ; :pi/4
    (check-equal? (symb:atan 1 'a) '(atan 1 a))
    (check-equal? (symb:atan 'a 1) '(atan a 1))
    (check-equal? (symb:atan 'a 'b) '(atan a b))
    ;; !!!
    (check-equal? (symb:atan 'a 'a) '(atan a a)) ; :pi/4
    )

   (test-case
    "min / max"
    (check-true (min? '(min 1 2 a)))
    (check-true (max? '(max 1 2 a)))
    (check-equal? (symb:min 1 2 3) 1)
    (check-equal? (symb:min 1 2 3 'c) '(min 1 2 3 c))
    (check-equal? (symb:max 1 2 3) 3)
    (check-equal? (symb:max 1 2 3 'c) '(max 1 2 3 c)))

   ;; units
   (test-case
    "symb:&"
    (check-equal? (symb:& 'a 'm) '(& a m))
    (check-equal? (symb:& 'a 'm 'SI) '(& a m SI)))

   (test-case
    "derivative"
    (check-true (derivative? '(derivative f)))
    (check-true (ederivative? '((expt derivative 3) f)))
    (check-equal? (symb:derivative 'f) '(derivative f))
    (check-equal? (symb:derivative '(derivative f)) '((expt derivative 2) f))
    (check-equal? (symb:derivative '((expt derivative 2) f)) '((expt derivative 3) f)))

   (test-case
    "symb:elementary-access?"
    ;; only one element
    ;; if last element is not tuple (up/down) => #t
    (check-true (symb:elementary-access? '() '(a)))
    (check-true (symb:elementary-access? '() '((+ a b c))))
    (check-false (symb:elementary-access? '() '((up a))))
    (check-false (symb:elementary-access? '() '((down (+ a b c)))))
    ;; trying to select into something that is not a tuple => #f
    (check-false (symb:elementary-access? '(0) '(a)))
    (check-false (symb:elementary-access? '(0) '((+ a b c))))
    
    (check-true (symb:elementary-access? '(0) '((up a))))
    (check-true (symb:elementary-access? '(0) '((down (+ a b c)))))
    (check-false (symb:elementary-access? '(0) '((up (down a b) c))))
    (check-false (symb:elementary-access? '(0) '((down (up (+ a b c) d) e))))

    ;; same if args is a list of elements,
    ;; except that the first select is direct and chain needs 1 elem
    (check-true (symb:elementary-access? '(0) '(a 1)))
    (check-true (symb:elementary-access? '(0) '((+ a b c) 1)))
    (check-false (symb:elementary-access? '(0) '((up a) 1)))
    (check-false (symb:elementary-access? '(0) '((down (+ a b c)) 1)))
    (check-true (symb:elementary-access? '(0 0) '((up a) 1)))
    (check-true (symb:elementary-access? '(0 0) '((down (+ a b c)) 1)))
    (check-true (symb:elementary-access? '(1 0) '(1 (up a))))
    (check-true (symb:elementary-access? '(1 0) '(1 (down (+ a b c))))))

   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))