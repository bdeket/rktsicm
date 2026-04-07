#lang racket/base

(require rackunit
         "../../units/system.rkt"
         "../../units/units.rkt"
         "../../units/with-units.rkt"
         "../../general/eq-properties.rkt"
         "../../kernel.rkt"
         "../../rkt/environment.rkt"
         "../../rkt/racket-help.rkt"
         "../helper.rkt")

(provide the-tests)
(define the-tests
  (test-suite
   "units/system"
   (test-case
    "unit-system"
    ;(struct unit-system (name base [derived #:mutable] [alternate #:mutable]))
    (define G (gensym))
    (check-equal? (base-units (make-unit-system 'name G 'derived 'alternate)) G)
    (check-equal? (derived-units (make-unit-system 'name 'base G 'alternate)) G)
    (check-equal? (alternate-units (make-unit-system 'name 'base 'derived G)) G)
    (let ([U (make-unit-system 'name 'base 'derived 'alternate)] [G (gensym)])
      (set-unit-system-derived! U G)
      (check-equal? (derived-units U) G))
    (let ([U (make-unit-system 'name 'base 'derived 'alternate)] [G (gensym)])
      (set-unit-system-alternate! U G)
      (check-equal? (alternate-units U) G)))
   (test-case
    "define-unit-system/"
    (define-unit-system 'HB
      (list '&horse "H" "speed")
      (list '&bull "B" "mass"))
    (check-equal? HB (make-unit-system 'HB `((&horse "H" "speed" ,&horse)
                                             (&bull  "B" "mass"  ,&bull))
                                       '() '()))
    (check-equal? &horse (make-unit 'HB #(1 0) 1))
    (check-equal? &bull (make-unit 'HB #(0 1) 1))
    (check-equal? (eval 'HB scmutils-base-environment)
                  (make-unit-system 'HB `((&horse "H" "speed" ,&horse)
                                          (&bull  "B" "mass"  ,&bull))
                                    '() '()))
    (check-true (unit-system? (eval 'HB scmutils-base-environment))))
   (test-case
    "define-unit-system/2"
    (define-unit-system two (&one "1" "number"))
    (check-equal? two (make-unit-system 'two `((&one "1" "number" ,&one))
                                       '() '()))
    (check-equal? &one (make-unit 'two #(1) 1)))
   (test-case
    "define-derived-unit(!)"
    (define-unit-system HB (&horse "H" "speed") (&bull "B" "mass"))
    (define-derived-unit HB '&monkey "M" "crazy" (*units &horse &bull))
    (check-equal? &monkey (make-unit 'HB #(1 1) 1))
    (define-derived-unit HB &kingkong "kk" "crazybig" (*units &horse &bull) 10)
    (check-equal? &kingkong (make-unit 'HB #(1 1) 10))
    (define-derived-unit HB &chicken "c" "flat" (/units &horse &bull))
    (check-equal? &chicken (make-unit 'HB #(1 -1) 1))
    (check-equal? (derived-units HB)
                  `((&monkey "M" "crazy" ,&monkey)
                    (&kingkong "kk" "crazybig" ,&kingkong)
                    (&chicken "c" "flat" ,&chicken)))
    (check-equal? (alternate-units HB) '()))
   (test-case
    "define-additional-unit(!)"
    (define-unit-system HB (&horse "H" "speed") (&bull "B" "mass"))
    (define-additional-unit HB '&monkey "M" "crazy" (*units &horse &bull))
    (check-equal? &monkey (make-unit 'HB #(1 1) 1))
    (define-additional-unit HB &kingkong "kk" "crazybig" &monkey 10)
    (check-equal? &kingkong (make-unit 'HB #(1 1) 10))
    (define-additional-unit HB '&feret "f" "long" (/units &horse &bull) 3)
    (check-equal? &feret (make-unit 'HB #(1 -1) 3))
    (define-additional-unit HB &chicken "c" "flat" (/units &horse &bull))
    (check-equal? &chicken (make-unit 'HB #(1 -1) 1))
    (check-equal? (derived-units HB) '())
    (check-equal? (alternate-units HB)
                  `((&monkey "M" "crazy" ,&monkey)
                    (&kingkong "kk" "crazybig" ,&kingkong)
                    (&feret "f" "long" ,&feret)
                    (&chicken "c" "flat" ,&chicken))))
   (test-case
    "define-multiplier"
    (check-equal? *multiplier-names* '())
    (define-multiplier &beastly "beastly" 666)
    (define-multiplier '&aL "a-life" 42)
    (check-equal? *multiplier-names* `((&aL "a-life" 42)
                                       (&beastly "beastly" 666)))
    (check-equal? &beastly (expt 10 666))
    (check-equal? &aL (expt 10 42)))
   (test-case
    "define-constant"
    (parameterize ([*numerical-constants* '()])
      (check-equal? (*numerical-constants*) '())
      (define-unit-system HB (&horse "H" "speed") (&bull "B" "mass"))
      (define-constant ':meaning-of-life "mol" "life" 42 &unitless 30)
      (define-constant ':pony "\\circ" "us" 0.5 &horse)
      (define-constant ':elefant "\\circ" "us" (& 3 &horse) &horse)
      (check-equal? :pony (& 0.5 &horse))
      (check-equal? :meaning-of-life 42)
      (check-equal? :elefant (& 3 &horse))
      ;; TODO ;; should this really be bound to the symbol instead of the id ?
      (check-equal? (get-property (eq-get ':pony 'constant) 'name) ':pony)
      (check-equal? (get-property (eq-get ':pony 'constant) 'numerical-value) 0.5)
      (check-equal? (get-property (eq-get ':pony 'constant) 'tex-string) "\\circ")
      (check-equal? (get-property (eq-get ':pony 'constant) 'description) "us")
      (check-equal? (get-property (eq-get ':pony 'constant) 'units) &horse)
      (check-equal? (get-property (eq-get ':meaning-of-life 'constant) 'uncertainty) 30)
      (check-not-false (member ':pony (map (λ (x) (get-property x 'name)) (*numerical-constants*))))
      (check-not-false (member 42 (map (λ (x) (get-property x 'numerical-value)) (*numerical-constants*))))
      (check-equal? (eval ':elefant scmutils-base-environment) :elefant)

      (check-equal? (get-property (get-constant-data ':elefant) 'name) ':elefant)
      (check-equal? (get-property (get-constant-data ':elefant) 'units) &horse)))
   (test-case
    "numerical-constants"
    ;; TODO ;; get rid of the environments - please don't use them
    (parameterize ([*numerical-constants* '()])
      (check-equal? (*numerical-constants*) '())
      ;; still defined :/
      (check-equal? (eval ':elefant scmutils-base-environment) (& 3 (make-unit 'HB #(1 0) 1)))
      (define rcv (make-log-receiver rktsicm-logger 'debug))
      (define-constant ':meaning-of-life "mol" "life" 42 &unitless 30)
      (check-equal? (format "~s" (sync rcv)) "#(warning \"rktsicm: (clobbering :meaning-of-life)\" #<continuation-mark-set> rktsicm)")

      (define X (literal-number 'xxx))
      (add-property! X 'name 'y)
      (add-property! X 'numerical-value 69)
      (add-property! X 'units (make-unit 'any #() 5))

      (check-exn #px"y: undefined" (λ () (eval 'y scmutils-base-environment)))
      (parameterize ([*numerical-constants* (list X)])
        (numerical-constants)
        (check-equal? (eval 'y scmutils-base-environment) (with-units 69 (make-unit 'any #() 5)))
        ;; overwrite
        (numerical-constants #f)
        (check-equal? (eval 'y scmutils-base-environment) 345))

      ;; still defined :/
      (check-equal? (eval 'y scmutils-base-environment) 345)
      (define Z (literal-number 'zzz))
      (add-property! Z 'name 'zzz)
      (add-property! Z 'numerical-value 1)
      (add-property! Z 'units (make-unit 'HB #(1 0) 1))

      (numerical-constants #t (list Z))
      (check-equal? (eval 'zzz scmutils-base-environment) (with-units 1 (make-unit 'HB #(1 0) 1)))
    ))
   (test-case
    "symbolic-constants"
    ;; TODO ;; get rid of the environments - please don't use them
    (parameterize ([*numerical-constants* '()])
      (check-equal? (*numerical-constants*) '())
      (define X (literal-number 'xxx))
      (add-property! X 'name 'y)
      (add-property! X 'numerical-value 69)
      (add-property! X 'units (make-unit 'any #() 5))

      (parameterize ([*numerical-constants* (list X)])
        (symbolic-constants)
        (check-equal? (eval 'y scmutils-base-environment) (with-units 'y (make-unit 'any #() 5)))
        ;; overwrite
        (symbolic-constants #f)
        (check-equal? (eval 'y scmutils-base-environment) (g:* 5 'y)))

      ;; still defined :/
      (check-equal? (eval 'y scmutils-base-environment) (g:* 5 'y))
      (define Z (literal-number 'zzz))
      (add-property! Z 'name 'zzz)
      (add-property! Z 'numerical-value 1)
      (add-property! Z 'units (make-unit 'HB #(1 0) 1))

      (symbolic-constants #t (list Z))
      (check-equal? (eval 'zzz scmutils-base-environment) (with-units 'zzz (make-unit 'HB #(1 0) 1)))
    ))
   (test-case
    "&"
    (define &horse (make-unit 'HB #(1 0) 1))
    (define &bull (make-unit 'HB #(0 1) 1))
    (define &pony (make-unit 'HB #(1 0) 1/2))
    (define-multiplier &beastly "beastly" (log 666 10))
    (check-equal? (& 5 &unitless) 5)
    (check-equal? (& 5 &horse) (with-units 5 &horse))
    (check-within (& 5 &beastly &horse) (with-units (* 5 666) &horse) 1e-10)
    (check-within (& 5 &beastly &pony) (with-units (* 5/2 666) &horse) 1e-10)
    (check-equal? (& (& 5 &horse) &pony) (& 5 &horse))
    (check-equal? (& (& 5 &pony) &horse) (& 5/2 &horse))
    (check-exn #px"Units do not match: &" (λ () (& (& 5 &bull) &horse))))
   (test-case
    "express-in-given-units"
    (define &horse (make-unit 'HB #(1 0) 1))
    (define &bull (make-unit 'HB #(0 1) 1))
    (define &pony (make-unit 'HB #(1 0) 1/2))
    (check-equal? (express-in-given-units (& 'x &pony) &horse '&horse) '(& (* 1/2 x) &horse))
    (check-equal? (express-in-given-units &pony &horse '&horse) '(& 1/2 &horse))
    (check-equal? (express-in-given-units 5 &horse '&horse) 5)
    (check-exn #px"Cannot express in given units" (λ () (express-in-given-units (& 'x &pony) &bull '&horse)))
    (check-exn #px"Cannot express in given units" (λ () (express-in-given-units &pony &bull '&horse))))
   (test-case
    "express-as"
    (define &horse (make-unit 'HB #(1 0) 1))
    (define &bull (make-unit 'HB #(0 1) 1))
    (define &pony (make-unit 'HB #(1 0) 1/2))
    (check-equal? (express-as (& 'x &pony) '&horse) '(& (* 1/2 x) &horse))
    (skip
     ;; not working because not in generic-environment
     ;;TODO - see generic-environment -> scmutils-base-environment in system.rkt
     (check-equal? (express-as &pony '(g:* 1/2 &horse)) '(& 1/2 &horse)))
    (check-equal? (express-as 5 6) 5)
    (check-exn #px"Cannot express in given units" (λ () (express-as (& 'x &pony) '&bull)))
    (check-exn #px"Cannot express in given units" (λ () (express-as &pony '&bull))))
   (test-case
    "unit-convert"
    (define &horse (make-unit 'HB #(1 0) 1))
    (define &bull (make-unit 'HB #(0 1) 1))
    (define &pony (make-unit 'HB #(1 0) 1/2))
    (check-equal? (unit-convert (& 'x &pony) &horse) '(& (* 1/2 x) &horse))
    (skip
     ;; not working because not in generic-environment
     ;;TODO - see generic-environment -> scmutils-base-environment in system.rkt
     (check-equal? (unit-convert &pony (g:* 1/2 &horse)) '(& 1/2 &horse)))
    (check-equal? (unit-convert 5 6) 5)
    (check-exn #px"Cannot express in given units" (λ () (unit-convert (& 'x &pony) &bull)))
    (check-exn #px"Cannot express in given units" (λ () (unit-convert &pony &bull))))
   (test-case
    "find-unit-description"
    (check-equal? (find-unit-description #(1 1) '()) #f)
    (check-equal? (find-unit-description #(1 1) `((0 1 2 ,(make-unit 'HB #(1 1) 2))))
                  (list 0 1 2 (make-unit 'HB #(1 1) 2)))
    (check-equal? (find-unit-description #(1 1) `((0 1 2 ,(make-unit 'HB #(1 1) 2))
                                                  (0 1 2 ,(make-unit 'HB #(1 1) 3))))
                  (list 0 1 2 (make-unit 'HB #(1 1) 2)))
    (check-equal? (find-unit-description #(1 1) `((0 1 2 ,(make-unit 'HB #(1 0) 2))
                                                  (0 1 2 ,(make-unit 'HB #(1 1) 3))))
                  (list 0 1 2 (make-unit 'HB #(1 1) 3))))
   (test-case
    "find-unit-name"
    (check-equal? (find-unit-name #(1 1) '()) #f)
    (check-equal? (find-unit-name #(1 1) `((0 1 2 ,(make-unit 'HB #(1 1) 3)))) 0)
    (check-equal? (find-unit-name #(1 1) `((0 1 2 ,(make-unit 'HB #(1 1) 3))
                                           (1 2 3 ,(make-unit 'HB #(1 1) 4)))) 0)
    (check-equal? (find-unit-name #(1 1) `((0 1 2 ,(make-unit 'HB #(1 0) 3))
                                           (1 2 3 ,(make-unit 'HB #(1 1) 4)))) 1))
   (test-case
    "make-unit-description"
    (define-unit-system HB (&horse "H" "speed") (&bull "B" "mass"))
    (define-additional-unit HB '&monkey "M" "crazy" (*units &horse &bull))
    (define-derived-unit HB &bird "Q" "fly" (*units (*units &horse &horse) &bull))
    (check-equal? (make-unit-description 5 #(1 1) HB) '(& 5 (* &horse &bull)))
    (check-equal? (make-unit-description 5 #(2 1) HB) '(& 5 &bird)))
   (test-case
    "with-units->expression"
    (define-unit-system HB (&horse "H" "speed") (&bull "B" "mass"))
    (define &pony (make-unit 'HB #(1 0) 1/2))
    (check-equal? (expression (with-units->expression HB (& 'x &pony))) '(& (* 1/2 x) &horse))
    (check-equal? (with-units->expression HB &pony) '(& 1/2 &horse))
    (check-equal? (with-units->expression HB 5) 5))
   (test-case
    "unit-expression"
    (check-equal? (unit-expression '() '()) '(*))
    (check-equal? (unit-expression '(1 2 3) '(a b c)) '(* a (expt b 2) (expt c 3)))
    (check-equal? (unit-expression '(1 0 3) '(a b c)) '(* a (expt c 3))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests the-tests))