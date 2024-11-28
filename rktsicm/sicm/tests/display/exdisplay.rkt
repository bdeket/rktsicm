#lang racket/base

(require rackunit
         (submod "../../display/exdisplay.rkt" ALL)
         "../helper.rkt")

(define expr0 '())
(define expr1 '(+ 4 (* 3 y)))
(define expr2 '(+ z (/ y (expt x 3))))
(define expr3
  '(/ (+ alpha (/ ((derivative f) b) (+ alpha beta)))
      (+ (/ (+ x y) 2) (expt (/ (+ a c (/ 2 x)) (* d e)) (+ f (/ g h))))))
(define expr4
  '(matrix-by-rows (list (up ((partial 2) x_1_3)) (down ((expt derivative 3) f^2)) (vector (((expt D 2) (expt z 3)) q)))
                   (list 1 (/ (expt (+ x y) z) 3) (/ (- -4) 2/3))
                   (list qdotdotprime (+ (+) (+ (* -1 2 3) 4)) (+ 1))))
(define expr5 '(= (up 2 4) (expt 4 (+ 9 2/3)) (+ (up 6 8 9) ((partial 1 2 3) x) (- (sqrt g^8^2_f) (down 3 2) (expt 1 8)))))
(define expr6 '(superscript x 1 a^2 3))
(define expr7 '(sum (dotted (up 4 2)) (dotdotted x^2)
                    (primed (expt 3 z)) (primeprimed (sqrt a))
                    ((* (partial x) (partial y) f) x) () (())))
(define expr8 '(+ (* -2 a 4) (/ -4 b) (/ (* -1 c) (* -3 d)) (/ (* -1) (* -3 e -8)) (/ (* 6 f) g)))
(define expr9 '(* fx ft))

(provide the-tests)
(define the-tests
  (test-suite
   "display/exdisplay"
   (test-case
    "internal-show-expression"
    (skip "not implemented! links to void")
    )
   (test-case
    "2d-show-expression"
    (check-equal? (out->string (2d-show-expression expr0))
                  "\n\n\n\n")
    (check-equal? (out->string (2d-show-expression expr1))
                  "\n\n4 + 3 y\n\n")
    (check-equal? (out->string (2d-show-expression expr2))
                  "\n\n    y \nz + --\n     3\n    x \n\n")
    (check-equal? (out->string (2d-show-expression expr3))
                  (string-append "\n\n"
                                 "               Df(b)        \n"
                                 "    alpha + ------------    \n"
                                 "            alpha + beta    \n"
                                 "----------------------------\n"
                                 "                           g\n"
                                 "                       f + -\n"
                                 "         /         2 \\     h\n"
                                 "        |  a + c + -  |     \n"
                                 "x + y   |          x  |     \n"
                                 "----- + |  ---------  |     \n"
                                 "  2      \\    d e    /      \n\n"))
    (check-equal? (out->string (2d-show-expression expr4))
                  (string-append "\n\n"
                                 "[    [D x  ]           [ 3 2]      [ 2  3    ]]\n"
                                 "[    [ 2 1 ]           [D f ]      [D (z )(q)]]\n"
                                 "[    [    3]                                  ]\n"
                                 "[                                             ]\n"
                                 "[                            z       - (- 4)  ]\n"
                                 "[                     (x + y)        -------  ]\n"
                                 "[       1             --------          2     ]\n"
                                 "[                        3              -     ]\n"
                                 "[                                       3     ]\n"
                                 "[                                             ]\n"
                                 "[(prime (ddot q))  0 +  - 2 3 + 4       1     ]\n\n"))
    (check-equal? (out->string (2d-show-expression expr5))
                  (string-append "\n\n"
                                 "           2                            /   2  \\            \n"
                                 "       9 + -   [6]                     |     f  |           \n"
                                 "[2]        3   [ ]                     |   8    |   [3]    8\n"
                                 "[ ] = 4      = [8] + D         x + sqrt \\ g    /  - [ ] - 1 \n"
                                 "[4]            [ ]    [1  2  3]                     [2]     \n"
                                 "               [9]                                          \n\n"))
    (check-equal? (out->string (2d-show-expression expr6))
                  (string-append "\n\n"
                                 " [1   2  3]\n"
                                 " [   a    ]\n"
                                 "x          \n\n"))
    (check-equal? (out->string (2d-show-expression expr7))
                  (string-append "\n\n"
                                 "     [4]           2             z                                                                  \n"
                                 "(dot [ ]) + (ddot x ) + (prime (3 )) + (primeprime (sqrt(a))) + (partial(x) partial(y) f)(x) +  + ()\n"
                                 "     [2]                                                                                            \n\n"))
    (check-equal? (out->string (2d-show-expression expr8))
                  (string-append "\n\n"
                                 "           4      c            1         6 f\n"
                                 " - 2 a 4 - - - ------- - ------------- + ---\n"
                                 "           b   (- 3) d   (- 3) e (- 8)    g \n\n"))
    (check-equal? (out->string (2d-show-expression expr9))
                  (string-append "\n\nfx ft\n\n")))
   (test-case
    "expression->tex-string"
    (check-equal? (expression->tex-string expr0)
                  "\\boxit{ $$$$}")
    (check-equal? (expression->tex-string expr1)
                  "\\boxit{ $$4 + 3 y$$}")
    (check-equal? (expression->tex-string expr2)
                  "\\boxit{ $$z + {{y}\\over {{x}^{3}}}$$}")
    (check-equal? (expression->tex-string expr3)
                  (string-append "\\boxit{"
                                 " $${{\\alpha + {{Df\\left( b \\right)}\\over {\\alpha + \\beta}}}"
                                 "\\over {{{x + y}\\over {2}} + {\\left( {{a + c + {{2}\\over {x}}}"
                                 "\\over {d e}} \\right)}^{f + {{g}\\over {h}}}}}$$}"))
    (check-equal? (expression->tex-string expr4)
                  (string-append "\\boxit{"
                                 " $$\\left\\lgroup \\matrix{ \\displaystyle{ \\left( \\matrix{"
                                 " \\displaystyle{ {\\partial}_{2}{x}_{{1}_{3}}}} \\right)} &"
                                 " \\displaystyle{ \\left[ \\matrix{ \\displaystyle{"
                                 " {D}^{3}{f}^{2}}} \\right]} & \\displaystyle{ \\left\\lgroup"
                                 " \\matrix{ \\displaystyle{ {D}^{2}\\left( {z}^{3} \\right)\\left("
                                 " q \\right)}} \\right\\rgroup} \\cr \\cr \\displaystyle{ 1} &"
                                 " \\displaystyle{ {{{\\left( x + y \\right)}^{z}}\\over {3}}} &"
                                 " \\displaystyle{ {{- \\left( - 4 \\right)}\\over {{{2}\\over"
                                 " {3}}}}} \\cr \\cr \\displaystyle{ {\\ddot{q}}^\\prime} &"
                                 " \\displaystyle{ 0 +  - 2 3 + 4} & \\displaystyle{ 1}}"
                                 " \\right\\rgroup$$}"))
    (check-equal? (expression->tex-string expr5)
                  (string-append "\\boxit{ "
                                 "$$\\left( \\matrix{ \\displaystyle{ 2} \\cr \\cr \\displaystyle{ "
                                 "4}} \\right) = {4}^{9 + {{2}\\over {3}}} = \\left( \\matrix{ "
                                 "\\displaystyle{ 6} \\cr \\cr \\displaystyle{ 8} \\cr \\cr "
                                 "\\displaystyle{ 9}} \\right) + {\\partial}_{1, 2, 3}x + "
                                 "\\sqrt{{g}^{{8}^{{2}_{f}}}} - \\left[ \\matrix{ \\displaystyle{ "
                                 "3} \\cr \\cr \\displaystyle{ 2}} \\right] - {1}^{8}$$}"))
    (check-equal? (expression->tex-string expr6)
                  "\\boxit{ $${x}^{1, {a}^{2}, 3}$$}")
    (check-equal? (expression->tex-string expr7)
                  (string-append "\\boxit{ $$\\dot{\\left( \\matrix{ \\displaystyle{ 4} \\cr \\cr"
                                 " \\displaystyle{ 2}} \\right)} + \\ddot{{x}^{2}} + {\\left( {3}^{z}"
                                 " \\right)}^\\prime + {\\sqrt{a}}^{\\prime\\prime} + \\left("
                                 " {\\partial}_{x} {\\partial}_{y} f \\right)\\left( x \\right) +  +"
                                 " \\left(  \\right)$$}"))
    (check-equal? (expression->tex-string expr8)
                  (string-append "\\boxit{ $$ - 2 a 4 - {{4}\\over {b}} - {{c}\\over {\\left( - 3"
                                 " \\right) d}} - {{1}\\over {\\left( - 3 \\right) e \\left( - 8"
                                 " \\right)}} + {{6 f}\\over {g}}$$}"))
    (check-equal? (expression->tex-string expr9)
                  (string-append "\\boxit{ $$fx \\cdot ft$$}")))
   ;**********************************************************
   (test-case
    "interpolate empty list"
    (check-equal? (interpolate 'el '()) '())
    (check-equal? (interpolate 'el '(1)) '(1))
    (check-equal? (interpolate 'el '(1 2)) '(1 el 2)))
   (test-case
    "2d:partesize empty box"
    (check-equal? (2d:parenthesize '() (make-box 0 0 0 '())) "()"))
   (test-case
    "interpolate-signs empty-args"
    (check-equal? (interpolate-signs '(+) '()) '()))
   (test-case
    "interpolate-for-tex-product empty-args"
    (check-equal? (interpolate-for-tex-product '()) '()))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))