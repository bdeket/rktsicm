#lang s-exp "../../generic.rkt"

(require rackunit
         "../../calculus/manifold.rkt"
         "../../calculus/manifold/helper.rkt"
         (only-in "../../calculus/basis.rkt" coordinate-basis?)
         (only-in racket/list last)
         "../helper+scm.rkt"
         )

(manifold:assign-operations)

(define (check-method obj msg art)
  (check-true (procedure? (obj msg)))
  (check-equal? (arity (obj msg)) art))
(define (check-minManifold M n d t [ptchs '()])
  (check-equal? (M 'name) n)
  (check-equal? (M 'manifold) M)
  (check-subManifold M d t ptchs))
(define (check-subManifold M d t ptchs)
  (check-equal? (M 'type) t)
  (check-equal? (M 'dimension) (car d))
  (check-equal? (M 'embedding-dimension) (last d))
  (check-equal? (M 'distinguished-points) '()) ;; no points installed
  (check-method M 'add-distinguished-point! *exactly-two*)
      
  (check-equal? (M 'patch-names) ptchs) ;; no patches installed
  (check-method M 'get-patch *exactly-one*)
  (check-exn #px"Unknown patch" (λ () ((M 'get-patch) (gensym))))
      
  (check-exn #px"Unknown message: manifold generator" (λ () (M (gensym)))))
(define (check-patch P M n d t)
        (check-equal? (M 'patch-names) (list n))
        (check-exn #px"Unknown patch " (λ () (patch (gensym) M)))
        (skip #;TODO? (check-equal? (patch n M) P))
        (check-equal? (P 'name) n)
        (check-equal? (P 'patch) P)

        (check-equal? (P 'coordinate-system-names) '()) ;; no coordinate systems installed
        (check-method P 'get-coordinate-system *exactly-one*)
        (check-exn #px"Unknown coordinate-system" (λ () ((P 'get-coordinate-system) (gensym))))

        ;; it sends back unknown messages to the manifold so it 'acts' as a manifold
        ;; except for the messages shadowed above
        (check-subManifold P d t '(origin))
        (check-equal? (P 'manifold) M))

(provide the-tests)
(define the-tests
  (test-suite
   "calculus/manifold"
   ;; HELPER
   (test-case
    "c:generate"
    (define (F x) (* 2 (+ 2 x)))
    (check-equal? (c:generate 1 'up F) 4)
    (check-equal? (c:generate 2 'up F) (up 4 6))
    (check-equal? (c:generate 1 'up (down (λ (x) x) F)) (down 0 4))
    (check-equal? (c:generate 2 'up (down (λ (x) x) F)) (up (down 0 4) (down 1 6)))
    (check-equal? (c:generate 1 'down F) 4)
    (check-equal? (c:generate 3 'down F) (down 4 6 8))
    ;; todo : is this reasonable?
    (check-equal? (c:generate 0 'up F) (up))
    (check-equal? (c:generate 0 'down F) (down)))
   (test-case
    "c:lookup"
    (check-equal? (c:lookup 1 'any) #f)
    (check-equal? (c:lookup 1 '(2 2)) #f)
    (check-equal? (c:lookup 1 '(1 2)) '(1 2))
    (check-equal? (c:lookup 1 (up '(1 2) '(3 4) '(4 5))) '(1 2))
    (check-equal? (c:lookup 3 (down '(1 2) '(3 4) '(4 5))) '(3 4))
    (check-equal? (c:lookup 2 (down '(1 2) '(3 4) '(4 5))) #f))
   ;; MAIN
   ;;; SPACE
   (test-case
    "specify-manifold"
    (define RTEST (specify-manifold 'rtest^n))
    (define CTEST (specify-manifold 'ctest Complex))
    (for ([MM (in-list (list RTEST CTEST))]
          [d  (in-list '(    (2)   (2 3)))]
          [n  (in-list '(rtest^2-1 ctest-1))]
          [t  (in-list (list Real  Complex))])
      ;; manifold generator functions
      (check-method MM 'new-patch *exactly-three*)
      (check-method MM 'patch-setup *exactly-one*)
      (check-exn #px"Unknown patch" (λ () ((MM 'patch-setup) (gensym)))) ;; no patch installed
      (check-method MM 'generator *one-or-two*)
      (check-exn #px"Unknown message: manifold setup" (λ () (MM (gensym))))
      ;; generated manifold functions
      (define M (apply (MM 'generator) d))
      (check-minManifold M n d t))
    (check-exn #px"assertion failed: \\(and \\(exact-integer\\? embedding-dimension\\) \\(fix:>= embedding-dimension dimension\\)\\)"
               (λ () ((RTEST 'generator) 2 1)))
    (check-exn #px"assertion failed: \\(and \\(exact-integer\\? embedding-dimension\\) \\(fix:>= embedding-dimension dimension\\)\\)"
               (λ () ((RTEST 'generator) 2 3.2))))
   ;;; - PATCH
   (test-case
    "manifold-patch"
    (define RTEST (specify-manifold 'rtest))
    (define CTEST (specify-manifold 'ctest Complex))
    (attach-patch 'origin RTEST)
    (attach-patch 'origin CTEST)
    (for ([MM (in-list (list RTEST CTEST))]
          [d  (in-list '(    (2)   (2 3)))]
          [t  (in-list (list Real  Complex))])
      ;; patch setup
      (define S ((MM 'patch-setup) 'origin))
      (check-method S 'new-coordinate-system *exactly-two*)
      (check-method S 'generator *exactly-one*)
      (check-exn #px"Unknown message patch-setup" (λ () (S (gensym))))

      ;; patch generator
      (define M (apply (MM 'generator) d))
      ;; get patch via manifold
      (check-patch ((M 'get-patch) 'origin) M 'origin d t)
      ;; get patch via generator
      (check-patch ((S 'generator) M) M 'origin d t)
      ))
   ;;; COORDINATE SYSTEM
   (test-case
    "coordinates"
    (define (fake-coord-sys mani)
      (define g (gensym))
      (λ (msg) (λ rst (cons g msg))))
    (define RTEST (specify-manifold 'rtest))
    (define CTEST (specify-manifold 'ctest Complex))
    (attach-patch 'origin RTEST)
    (attach-patch 'origin CTEST)
    (attach-coordinate-system 'CO_1 'origin RTEST fake-coord-sys)
    (attach-coordinate-system 'CO_1 'origin CTEST fake-coord-sys (up 'x0 'x1 'x2))
    (for ([MM (in-list (list RTEST CTEST))]
          [d  (in-list '(    (2)   (3 5)))]
          [t  (in-list (list Real  Complex))])
      (define M (apply (MM 'generator) d))
      (define P (patch 'origin M))
      (define C (coordinate-system-at 'CO_1 'origin M))
      (check-equal? (C 'name) 'CO_1)
      (check-equal? ((C 'patch) 'name) 'origin)
      (skip #;TODO? (check-equal? (C 'patch) (P 'patch)))
      (define fake (car ((C '->point))))
      (check-equal? ((C '->point)) (cons fake 'coords->point))
      (check-equal? ((C '->coords)) (cons fake 'point->coords))
      (check-equal? ((C 'check-point)) (cons fake 'check-point))
      (check-equal? ((C 'check-coords)) (cons fake 'check-coordinates))
      
      (check-equal? (g:size (C 'typical-coords)) (car d))
      (check-equal? (C 'coordinate-prototype) (s:generate (car d) 'up (λ (i) (string->symbol (format "x~a" i)))))
      (check-method C 'set-coordinate-prototype! *exactly-one*)
      (check-equal? (C 'access-chains) (s:generate (car d) 'up list))
      (check-equal? (C 'dual-chains) (s:generate (car d) 'down list))
      (let ([cc (C 'coordinate-function-specs)])
        (check-true (up? cc))
        (check-equal? (car (g:ref cc (- (car d) 1))) (string->symbol (format "x~a" (- (car d) 1))))
        (check-true (procedure? (cadr (g:ref cc (- (car d) 1))))))
      (let ([cc (C 'coordinate-basis-vector-field-specs)])
        (check-true (down? cc))
        (check-equal? (car (g:ref cc (- (car d) 1))) (string->symbol (format "d/dx~a" (- (car d) 1))))
        (check-true (procedure? (cadr (g:ref cc (- (car d) 1))))))
      (let ([cc (C 'coordinate-basis-1form-field-specs)])
        (check-true (up? cc))
        (check-equal? (car (g:ref cc (- (car d) 1))) (string->symbol (format "dx~a" (- (car d) 1))))
        (check-true (procedure? (cadr (g:ref cc (- (car d) 1))))))
      ; not really checking: see implementation test in manifold-extra
      (check-true (coordinate-basis? (C 'coordinate-basis)))
      (check-true (up? (C 'coordinate-functions)))
      (check-true (down? ((C 'coordinate-basis-vector-fields) 0)))
      (check-true (up? (C 'coordinate-basis-1form-fields)))
      ;;else
      (check-subManifold C d t '(origin))
      (check-equal? (coordinate-system-dimension C) (car d))
      ((C 'set-coordinate-prototype!) (s:generate (car d) 'up (λ (i) (string->symbol (format "z~a" i)))))
      (check-equal? (car (g:ref (C 'coordinate-basis-vector-field-specs) 0)) 'd/dz0)
      (check-true (procedure? (C 'z0)))
      (check-equal? (expression (C 'd/dz0)) 'd/dz0)
      (check-equal? (expression (C 'dz1)) 'dz1)
      (check-exn #px"bad message" (λ () (C "anything")))))
   (test-case
    "actual coordinates"
    (check-true ((R2-rect 'check-coords) (up 1 2)))
    (check-false ((R2-rect 'check-coords) (up 1 2 3)))
    (check-false ((R2-rect 'check-coords) 1))
    (check-true ((R2-rect 'check-point)  ((R2-rect '->point) (up 1 2))))
    (check-false ((R2-rect 'check-point) ((R1-rect '->point) 1))) ;; not on same manifold
    ;; passing from one coordinate system to another
    (check-equal? (expression ((R2-polar '->coords) ((R2-rect '->point) (up 'x 'y))))
                  '(up (sqrt (+ (expt x 2) (expt y 2))) (atan y x)))
    (check-equal? (expression ((R3-spherical '->coords) ((R3-cyl'->point) (up 'r 'α 'z))))
                  '(up (sqrt (+ (expt r 2) (expt z 2)))
                       (acos (/ z (sqrt (+ (expt r 2) (expt z 2)))))
                       α)))
   
   ;;; MANIFOLD
   ;; MANIFOLD-POINT
   ;; access-chains?
   ;; coordinate-???-specs
   (test-case
    "install-coordinates"
    ;; this only works ±ok at the toplevel...
    (local-require "../../rkt/environment.rkt")
    (eval '(define x 4) generic-environment)
    (check-equal? (out->string (install-coordinates R2-rect (up 'x 'y))) "")
    (check-not-exn (λ () (eval 'x))) ;; x is defined but in an otherwise empty namespace
    (check-equal? (out->string (install-coordinates R2-rect (up 'x 'y) generic-environment)) "(clobbering x)\n")
    (check-true (eval '(procedure? x) generic-environment))
    (check-equal? (eval '(expression x) generic-environment) 'x)
    (uninstall-coordinates)
    (check-equal? (eval 'x generic-environment) 4))

   (test-case
    "public interface"
    ;; other ???
    ; get-coordinate-rep
    ; my-manifold-point?
    ; make-manifold
    ; coordinate-system-dimension
    ; environment-define 'coordinate-system-dimension
    ; frame?
    (check-false (frame? R2))
    (check-false (frame? R2-rect))
    (skip #;"this can not be correct")
    (check-not-false (frame? (λ (x) #f)))
    (define (fake-frame m)
      (case m
        [(coords->event) 1]
        [(event->coords) 2]
        [(name) 'fake]
        [(ancestor-frame) #f]
        [(params) '()]
        [(manifold) #f]
        [else (error)]))
    (check-not-false (frame? fake-frame))
    ; chart
    (define P ((R2-rect '->point) #(x y)))
    (check-equal? ((chart R2-rect) P) #(x y))
    (check-equal? (chart fake-frame) 2)
    ; point
    (check-equal? ((chart R2-rect) ((point R2-rect) #(x y))) #(x y))
    (check-equal? (point fake-frame) 1)
    ; typical-point
    (check-equal? (dimension ((R2-rect '->coords) (typical-point R2-rect))) 2)
    ; typical-coords
    (check-equal? (dimension (typical-coords R2-rect)) 2)
    ; corresponding-velocities
    (check-equal? (format "~s" (corresponding-velocities #(x y))) "#(v:x v:y)")
    (check-false (equal? (corresponding-velocities #(x y)) #(v:x v:y)))
    )
   (test-case
    "constant-manifold"
    (define g (gensym))
    (check-equal? ((constant-manifold-function g) ((R2-rect '->point) (up 1 2))) g)
    (check-exn #px"" (λ () ((constant-manifold-function g) (up 1 2))))
    (check-equal? (zero-manifold-function ((R2-rect '->point) (up 1 2))) 0)
    (check-equal? (one-manifold-function ((R2-rect '->point) (up 1 2))) 1)
    (check-equal? (zero-coordinate-function (up 1 2 3)) 0))

   ;; implementations
   ; R^n  S^n  S^2  SO3
   (test-case
    "R^n R1 rect"
    (check-minManifold R1 'R^1-1 '(1) 'Real '(origin))
    (define p1 (((coordinate-system-at 'rectangular 'origin R1) '->point) 1))
    (define p2 ((R1-rect '->point) 2))
    (check-true (manifold-point? p1))
    (check-true (my-manifold-point? p1 R1))
    (check-true (my-manifold-point? p2 R1))
    (check-false (my-manifold-point? p1 R2))
    (check-equal? (point->manifold p1) R1))

   (test-case
    "coordinate systems of R4"
    (let ([P ((R4 'get-patch) 'origin)])
      (for* ([n (in-list (P 'coordinate-system-names))]
             [C (in-value ((P 'get-coordinate-system) n))])
        (define PP (up 1 2 3 4))
        (check-true ((C 'check-coords) PP))
        (check-false ((C 'check-coords) (up 1 2 3)))
        (define p ((C '->point) PP))
        (check-equal? (get-coordinate-rep p) PP)
        (check-true ((C 'check-point) p))
        (check-equal? ((C '->coords) p) PP)
        (check-equal? (C 'manifold) R4)
        (check-exn (pregexp (format "Bad coordinates: ~a" n)) (λ () ((C '->point) (up 1 2 3))))
        (check-exn (pregexp (format "Bad point: ~a" n)) (λ () ((C '->coords) #f)))
        (check-exn (pregexp (format "Bad point: ~a" n)) (λ () ((C '->coords) (make-manifold-point #f R4 #f #f))))))
    
    (check-equal? (expression ((R4-rect '->coords) (make-manifold-point (up 't 'x 'y 'z) R4 #f #f)))
                  '(up t x y z))
    (check-equal? (expression ((R4-cyl '->coords) (make-manifold-point (up 't 'x 'y 'z) R4 #f #f)))
                  '(up (sqrt (+ (expt t 2) (expt x 2))) (atan x t) y z))
    (check-equal? (expression (((coordinate-system-at 'spherical/cylindrical 'origin R4) '->coords)
                               (make-manifold-point (up 't 'x 'y 'z) R4 #f #f)))
                  '(up (sqrt (+ (expt t 2) (expt x 2) (expt y 2)))
                       (acos (/ y (sqrt (+ (expt t 2) (expt x 2) (expt y 2)))))
                       (atan x t)
                       z))
    (check-equal? (expression ((spacetime-sphere '->coords) (make-manifold-point (up 't 'x 'y 'z) spacetime #f #f)))
                  '(up t
                       (sqrt (+ (expt x 2) (expt y 2) (expt z 2)))
                       (acos (/ z (sqrt (+ (expt x 2) (expt y 2) (expt z 2)))))
                       (atan y x))))
   (test-case
    "coordiante systems of S3"
    (let ([P ((S3 'get-patch) 'north-pole)])
      (for* ([n (in-list (P 'coordinate-system-names))]
             [C (in-value ((P 'get-coordinate-system) n))])
        (define PP (up 1 2 3))
        (check-true ((C 'check-coords) PP))
        (check-false ((C 'check-coords) (up .1 .2)))
        (define p ((C '->point) PP))
        (check-true ((C 'check-point) p))
        (if (equal? n 'gnomic)
            (skip (check-equal? ((C '->coords) p) PP))
            (check-within ((C '->coords) p) PP 1e-15))
        (check-equal? (C 'manifold) S3)
        (check-exn (pregexp "Bad coordinates:") (λ () ((C '->point) (up .1 .2))))
        (check-exn (pregexp "Bad point:") (λ () ((C '->coords) #f)))))
    ;; gnomic works with non-numeric coordinates:
    (check-equal? (expression ((S3-gnomic '->coords) ((S3-gnomic '->point) (up 'x 0 1))))
                  '(up (/ (* x (sqrt (+ 2 (* x x)))) (sqrt (+ 2 (* x x)))) 0 1)))
   (test-case
    "coordinate systems of SO3"
    (for* ([C (in-list (list Euler-angles alternate-angles))])
      (define PP (up 1 2 3))
      (check-true ((C 'check-coords) PP))
      (check-false ((C 'check-coords) (up .1 .2)))
      (define p ((C '->point) PP))
      (check-true ((C 'check-point) p))
      (check-within ((C '->coords) p) PP 1e-15)
      (check-equal? (C 'manifold) SO3)
      (check-exn (pregexp "Bad coordinates:") (λ () ((C '->point) (up .1 .2))))
      (check-exn (pregexp "Bad manifold point:") (λ () ((C '->coords) #f))))
    (check-equal? (expression ((Euler-angles '->coords) (make-manifold-point #(#(a b c)#(0 1 0)#(e f g)) SO3 #f #f)))
                  '(up (acos g) (atan e (* -1 f)) :pi/2))
    (check-equal? (expression ((alternate-angles '->coords) (make-manifold-point #(#(a b c)#(0 1 0)#(e f g)) SO3 #f #f)))
                  '(up 0 0 (atan (* -1 c) g))))
   (test-case
    "point transfer"
    (check-equal? (expression
                   ((S2-spherical '->coords) ((transfer-point R3-rect S2-spherical)
                                              ((R3-rect '->point) (up 'x 'y 'z)))))
                  '(up (acos z) (atan y x)))
    (check-equal? (expression
                   ((R3-rect '->coords) ((transfer-point S2-spherical R3-rect)
                                         ((S2-spherical '->point) (up 'x 'y)))))
                  '(up (* (cos y) (sin x)) (* (sin x) (sin y)) (cos x))))

   (test-case
    "dimension"
    (check-equal? (dimension R2-rect) 2)
    (check-equal? (dimension S3-tilted) 3))
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))