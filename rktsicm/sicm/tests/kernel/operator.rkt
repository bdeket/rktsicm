#lang racket/base

(require rackunit
         "../../kernel.rkt" ;; I want to be able to do ((vector (λ (x) x)) 1)
         "../../rkt/applyhook.rkt"
         "../helper.rkt"
         )

(define (operator-check O name subtype arity opts)
  (check-equal? (operator-name O) name)
  (check-equal? (operator-subtype O) subtype)
  (check-equal? (operator-arity O) arity)
  (check-equal? (operator-optionals O) opts))

(provide the-tests)
(define the-tests
  (test-suite
   "kernel/pseries"
   ;; ==== cstm/s-operator ====
   (test-case
    "make / access"
    (skip "better control of arity (of incoming operators, generated operator, incoming functions and result ... values?")
    (define (F x) x)
    (define O1 (make-operator F 'name 'subtype '(1 3 5) 'rest))
    (define O2 (make-operator F))
    (check-true (operator? O1))
    (check-equal? (operator-procedure O1) F)
    (check-equal? (operator-subtype O1) 'subtype)
    (check-equal? (operator-name O1) 'name)
    (check-equal? (operator-arity O1) '(1 3 5))
    (check-equal? (operator-optionals O1) '(rest))
    (check-equal? (apply-hook-extra (make-op F 'name 'subtype '(1 3 5) '(rest)))
                  (apply-hook-extra O1))
    (check-true (operator? O2))
    (check-equal? (operator-procedure O2) F)
    (check-equal? (operator-subtype O2) #f)
    (check-equal? (operator-name O2) #f)
    (check-equal? (operator-arity O2) 1)
    (check-equal? (operator-optionals O2) '()))
   ;; ==== main ====
   (test-case
    "type etc."
    (check-equal? (o:type (make-operator values)) operator-type-tag)
    (check-equal? (o:type-predicate (make-operator values)) operator?)
    (check-true ((o:type-predicate (make-operator values)) (make-operator +)))
    (check-equal? (o:arity (make-operator +)) (procedure-arity +))
    (check-equal? (o:arity (make-operator + 'name #f '(1 2 3))) '(1 2 3))
    (check-true (simple-operator? (make-operator -)))
    (check-false (simple-operator? (make-operator - 'min 'min)))
    (define O (make-operator -))
    (check-equal? (operator-optionals O) '())
    (set-operator-optionals! O '(lief))
    (check-equal? (operator-optionals O) '(lief)))
   (test-case
    "merge-subtypes"
    (check-equal? (operator-merge-subtypes (make-operator - 'a 'a) (make-operator + 'b 'a)) 'a)
    (check-equal? (operator-merge-subtypes (make-operator - 'a 'a) (make-operator + 'b #f)) 'a)
    (check-equal? (operator-merge-subtypes (make-operator - 'a #f) (make-operator + 'b 'a)) 'a)
    (check-exn #px"Incompatible subtypes -- OPERATOR"
               (λ () (operator-merge-subtypes (make-operator - 'a 'a) (make-operator + 'b 'b)))))
   (test-case
    "merge-ari / opt"
    (check-equal? (operator-merge-arities (make-operator (case-lambda [(x) x][(x y z) x][(a b c d e . f) a]))
                                          (make-operator values 'a #f (arity-at-least 3)))
                  (list 3 (arity-at-least 5)))
    (check-equal? (operator-merge-optionals (make-operator + 1 2 3 4 5 6 7 8)
                                            (make-operator - 'a 'b 3 'd 'e 'f 'g 'h))
                  '(4 5 6 7 8 d e f g h)))
   (test-case
    "zero/one"
    (define O0 (o:zero-like (make-operator (λ (x) x) 'a 'b 1 'rest)))
    (operator-check O0 'zero 'b 1 '(rest))
    (check-equal? ((O0 +) 1) 0)
    (check-equal? ((O0 +) 8) 0)
    (check-exn #px"o:zero-like:\n\tassertion failed: \\(pair:eq\\?"
               (λ () (o:zero-like (make-operator +))))
    (define O1 (o:one-like (make-operator (λ (x) x) 'a 'b 1 'rest)))
    (operator-check O1 'identity 'b 1 '(rest))
    (check-equal? ((O1 +) 1) 1)
    (check-equal? ((O1 +) 9) 9)
    (check-exn #px"o:one-like:\n\tassertion failed: \\(pair:eq\\?"
               (λ () (o:one-like (make-operator +))))
    (check-equal? (o:identity +) +))
   (test-case
    "unary (-, expt, /n, exp, cos, sin)"
    (define O (make-operator vector 'a 'b '(1 2 3) 'rest))
    (operator-check (o:negate O) '(- a) 'b '(1 2 3) '(rest))
    (check-equal? (((o:negate O) + -) 3 6) #(-9 3))

    (operator-check (o:o/n O 3) '(/ a 3) 'b '(1 2 3) '(rest))
    (check-equal? (((o:o/n O 3) + -) 3 6) #(3 -1))

    (define O1 (make-operator vector 'a 'b 1 'rest))

    (operator-check (o:expt O1 3) '(expt a 3) 'b 1 '(rest))
    (check-exn #px"o:expt:\n\tassertion failed:" (λ () (o:expt O 3)))
    (check-equal? (((o:expt O1 3) + -) 3 6) #(#(#(9 -3))))

    (define O2 (make-operator (λ (f) (λ _ (* 2 (apply f _)))) 'a 'b 1 'rest))
    (operator-check (o:exp O2) '(exp a) 'b 1 '(rest))
    (check-exn #px"o:exp:\n\tassertion failed:" (λ () (o:exp O )))
    (check-equal? (series:sum (((o:exp O2) -) 3 6) 3) -19)
    (check-equal? (expression (series:sum (((o:exp o:identity) (literal-function 'f)) 't) 5))
                  '(+ (f t) (f t) (* 1/2 (f t)) (* 1/6 (f t)) (* 1/24 (f t)) (* 1/120 (f t))))
    
    (operator-check (o:cos O2) '(cos a) 'b 1 '(rest))
    (check-exn #px"o:cos:\n\tassertion failed:" (λ () (o:cos O )))
    (check-equal? (expression (series:sum (((o:cos o:identity) (literal-function 'f)) 't) 5))
                  '(+ (f t) (* -1/2 (f t)) (* 1/24 (f t))))

    (operator-check (o:sin O2) '(sin a) 'b 1 '(rest))
    (check-exn #px"o:sin:\n\tassertion failed:" (λ () (o:sin O )))
    (check-equal? (expression (series:sum (((o:sin o:identity) (literal-function 'f)) 't) 5))
                  '(+ (f t) (* -1/6 (f t)) (* 1/120 (f t))))

    (operator-check (expn O2) '(exp a) 'b 1 '(rest))
    (check-exn #px"o:expn:\n\tassertion failed:" (λ () (expn O )))
    (check-equal? (expression (series:sum (((expn o:identity) (literal-function 'f)) 't) 5))
                  '(+ (f t) (f t) (* 1/2 (f t)) (* 1/6 (f t)) (* 1/24 (f t)) (* 1/120 (f t))))
    (check-equal? (expression (series:sum (((expn o:identity 3) (literal-function 'f)) 't) 12))
                  '(+ (f t) (f t) (* 1/2 (f t)) (* 1/6 (f t)) (* 1/24 (f t)))))
   (test-case
    "2op (+, -, *"
    (define (sub a b) (o:* O1 O2))
    (define O1 (make-operator vector 'a 'b (arity-at-least 1) 'rest))
    (define O2 (make-operator (λ fs (λ as (list->vector (map (λ (f) (g:apply f (map (λ (a) (* 2 a)) as))) fs))))
                              'c #f (arity-at-least 1) 'rust))
    (define O3 (make-operator vector 'd  sub (arity-at-least 1) 'rast))

    (operator-check (o:+ O1 O2) '(+ a c) 'b (arity-at-least 1) '(rest rust))
    (check-equal? (((o:+ O1 O2) + -) 3 6) #(27 -9))

    (operator-check (o:- O1 O2) '(- a c) 'b (arity-at-least 1) '(rest rust))
    (check-equal? (((o:- O1 O2) + -) 3 6) #(-9 3))

    (operator-check (o:* O1 O2) '(* a c) 'b (arity-at-least 1) '(rest rust))
    (check-equal? (((o:* O1 O2) + -) 3 6) #(#(18 -6)))

    (operator-check (o:* O3 O2) '(* a c) 'b (arity-at-least 1) '(rest rust))
    (check-equal? (((o:* O3 O2) + -) 3 6) #(#(18 -6))))
   (test-case
    "o&f (+, -, *"
    (define O1 (make-operator (λ (f) (λ as (g:apply f (map (λ (a) (* 2 a)) as))))
                              'a 'b (arity-at-least 1) 'rest))
    (define F1 (λ as (apply + as)))
    (define F2 (literal-function 'F))

    (operator-check (o:o+f O1 F1) `(+ a F1) 'b (arity-at-least 1) '(rest))
    (check-equal? (((o:o+f O1 F1) +) 3 6) 27)

    (operator-check (o:f+o F1 O1) `(+ F1 a) 'b (arity-at-least 1) '(rest))
    (check-equal? (((o:f+o F1 O1) +) 3 6) 27)

    (operator-check (o:o-f O1 F1) `(- a F1) 'b (arity-at-least 1) '(rest))
    (check-equal? (((o:o-f O1 F1) +) 3 6) 9)

    (operator-check (o:f-o F1 O1) `(- F1 a) 'b (arity-at-least 1) '(rest))
    (check-equal? (((o:f-o F1 O1) +) 3 6) -9)

    (operator-check (o:o*f O1 F1) `(* a F1) 'b (arity-at-least 1) '(rest))
    (check-equal? (((o:o*f O1 F1) +) 3 6) 324))
   (test-case
    "solve"
    (define O1 (g:solve-linear-left 3 o:identity))
    (operator-check O1 '(/ identity 3) #f 1 '())
    (define O2 (g:solve-linear      3 o:identity))
    (operator-check O2 '(/ identity 3) #f 1 '()))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))