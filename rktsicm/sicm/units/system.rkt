#lang racket/base

(provide (except-out (all-defined-out) unit-system))

(require (for-syntax racket/base)
         (only-in "../rkt/glue.rkt" if find write-line warn)
         (only-in "../rkt/define.rkt" define default-object?)
         (only-in "../rkt/environment.rkt" environment-bound? environment-define environment-assign! scmutils-base-environment generic-environment)
         "../general/assert.rkt"
         "../general/eq-properties.rkt"
         "../general/equals.rkt"
         "../kernel/express.rkt"
         "../kernel/generic.rkt"
         "../kernel/numbers.rkt"
         "../kernel/types.rkt"
         (rename-in "units.rkt" [unit-system u:unit-system])
         "with-units.rkt")

;;bdk;; start original file

;;;; Unit systems

;;bdk;; new implementation
(define-syntax (define-unit-system stx)
  (syntax-case stx (list quote)
    [(_ (quote id) (list (quote u-id) u-name u-type) ...)
     (syntax/loc
         stx
       (define-unit-system id (u-id u-name u-type) ...))]
    [(_ id (u-id u-name u-type) ...)
     (let ([len (length (syntax->list #'(u-name ...)))])
       (with-syntax
           ([(expo ...) (for/list ([i (in-range len)])
                          (build-vector len
                                        (lambda (j) (if (= i j) 1 0))))])
         (syntax/loc
           stx
         (begin
           (define-values (id u-id ...)
             (let* ([units (list (make-unit 'id expo 1) ...)]
                    [base-spec (for/list ([info (in-list (list (list 'u-id u-name u-type) ...))]
                                          [unit (in-list units)])
                                 (append info (list unit)))])
               (apply
                values
                (make-unit-system 'id base-spec '() '())
                units)))))))]))
#;
(define (define-unit-system system-name . base-units)
  (if (environment-bound? scmutils-base-environment system-name)
    (write-line `(clobbering ,system-name)))
  (let ((n (length base-units)))    
    (let ((base-specs
           (map (lambda (base-spec i)
                  (let* ((unit-name (car base-spec))
                         (exponents
                          (make-initialized-vector n
                                        (lambda (j) (if (fix:= i j) 1 0))))
                         (unit (make-unit system-name exponents 1)))
                    (if (environment-bound? scmutils-base-environment
                                              unit-name)
                      (write-line `(clobbering ,unit-name)))
                    (environment-define scmutils-base-environment
                                        unit-name
                                        unit)
                    (append base-spec (list unit))))
                base-units
                (iota n))))
      (environment-define scmutils-base-environment
			  system-name
			  (list '*unit-system*
				system-name
				base-specs          ;base units
				'()	            ;derived units
				'()	            ;additional units
				))))
  system-name)

(struct unit-system (name base [derived #:mutable] [alternate #:mutable])
  #:transparent
  #:constructor-name make-unit-system)

(define base-units unit-system-base)
(define derived-units unit-system-derived)
(define alternate-units unit-system-alternate)
#;#;#;#;#;
(define (unit-system? system)
  (and (pair? system)
       (eq? (car system) '*unit-system*)))

(define (unit-system-name system)
  (cadr system))

(define (base-units system)
  (caddr system))

(define (derived-units system)
  (cadddr system))

(define (alternate-units system)
  (car (cddddr system)))

;;; Data may be entered and results may be presented in derived units.

(define-syntax (define-derived-unit stx)
  (syntax-case stx (quote)
    [(_ system (quote unit-name) tex description content)
     (syntax/loc stx (define-derived-unit system unit-name tex description content 1))]
    [(_ system unit-name tex description content)
     (syntax/loc stx (define-derived-unit system unit-name tex description content 1))]
    [(_ system unit-name tex description content scale-factor)
     (syntax/loc
         #'unit-name
       (define unit-name
         (begin
           (assert (unit-system? system))
           (let* ([unit (make-unit (unit-system-name system)
                                   (unit-exponents content)
                                   (* (expression scale-factor) (unit-scale content)))]
                  [unit-spec (list 'unit-name tex description unit)])
             (define-derived-unit! system unit-spec)
             unit))))]))
#;
(define (define-derived-unit system unit-name tex description content
          #:optional scale-factor)
  (assert (unit-system? system))
  (if (environment-bound? scmutils-base-environment unit-name)
      (write-line `(clobbering ,unit-name)))
  (if (default-object? scale-factor)
      (set! scale-factor 1))
  (set! content
        (make-unit (unit-system-name system)
                   (unit-exponents content)
                   (* (expression scale-factor) (unit-scale content))))
  (let ((unit-spec (list unit-name tex description content)))
    (define-derived-unit! system unit-spec)
    (environment-define scmutils-base-environment unit-name content)
    unit-name))

(define (define-derived-unit! system unit-spec)
  (set-unit-system-derived! system
                            (append (derived-units system)
                                    (list unit-spec))))


;;; Data may be entered in additional units but results will not be
;;; presented in additional units.

(define-syntax (define-additional-unit stx)
  (syntax-case stx (quote)
    [(_ system (quote unit-name) tex description content)
     (syntax/loc stx (define-derived-unit system unit-name tex description content 1))]
    [(_ system (quote unit-name) tex description content scale)
     (syntax/loc stx (define-derived-unit system unit-name tex description content scale))]
    [(_ system unit-name tex description content)
     (syntax/loc stx (define-derived-unit system unit-name tex description content 1))]
    [(_ system unit-name tex description content scale-factor)
     (syntax/loc
         #'unit-name
       (define unit-name
         (begin
           (assert (unit-system? system))
           (let* ([unit (make-unit (unit-system-name system)
                                   (unit-exponents content)
                                   (* (expression scale-factor) (unit-scale content)))]
                  [unit-spec (list 'unit-name tex description unit)])
             (define-additional-unit! system unit-spec)
             unit))))]))
#;
(define (define-additional-unit system unit-name tex description content
          #:optional scale-factor)
  (assert (unit-system? system))
  (if (environment-bound? scmutils-base-environment unit-name)
    (write-line `(clobbering ,unit-name)))
  (if (default-object? scale-factor)
      (set! scale-factor 1))
  (set! content
        (make-unit (unit-system-name system)
                   (unit-exponents content)
                   (* (expression scale-factor) (unit-scale content))))
  (let ((unit-spec (list unit-name tex description content)))
    (define-additional-unit! system unit-spec)
    (environment-define scmutils-base-environment unit-name content)
    unit-name))

(define (define-additional-unit! system unit-spec)
  (set-unit-system-alternate! system
                               (append (alternate-units system)
                                       (list unit-spec))))


(define *multiplier-names* '())
(define (add-multiplier mult)
  (set! *multiplier-names*
        (cons mult *multiplier-names*)))

(define-syntax (define-multiplier stx)
  (syntax-case stx (quote)
    [(_ (quote name) tex-string log-value)
     (syntax/loc stx (define-multiplier name tex-string log-value))]
    [(_ name tex-string log-value)
     (syntax/loc
         stx
       (define name
         (begin
           (add-multiplier (list 'name tex-string log-value))
           (expt 10 log-value))))]))
#;
(define (define-multiplier name tex-string log-value)
  (if (environment-bound? scmutils-base-environment name)
      (write-line `(clobbering ,name)))
  (set! *multiplier-names*
        (cons (list name tex-string log-value)
              *multiplier-names*))
  (environment-define scmutils-base-environment
                      name
                      (expt 10 log-value)))

(define *numerical-constants* (make-parameter '()))

(define-syntax define-constant
  (syntax-rules (quote)
    [(_ (quote dname) dtex-string ddescription the-value dunits duncertainty)
     (define dname
       (let ([sname 'dname][value the-value])
         (if (environment-bound? scmutils-base-environment sname)
             (warn `(clobbering ,sname)))
         (let ((constant (literal-number sname)))
           (cond ((with-units? value)
                  (assert (same-units? (u:units value) dunits))))
           (set! value (g:simplify (u:value value)))
           (eq-put! sname 'constant constant)
           (add-property! constant 'name sname)
           (add-property! constant 'numerical-value value)
           (add-property! constant 'units dunits)
           (add-property! constant 'tex-string dtex-string)
           (add-property! constant 'description ddescription)
           (if (real? value) (declare-known-reals sname))
           (if duncertainty
               (add-property! constant 'uncertainty duncertainty))
           (*numerical-constants* (cons constant (*numerical-constants*)))
           (define the-unit (with-units value dunits))
           (environment-define scmutils-base-environment
                               sname
                               the-unit)
           the-unit)))]
    [(_ (quote name) tex-string description value units)
     (define-constant (quote name) tex-string description value units #f)]))
#;
(define (define-constant name tex-string description value units
          #:optional uncertainty)
  (if (environment-bound? scmutils-base-environment name)
      (write-line `(clobbering ,name)))
  (let ((constant (literal-number name)))
    (cond ((with-units? value)
           (assert (same-units? (u:units value) units))))
    (set! value (g:simplify (u:value value)))
    (eq-put! name 'constant constant)
    (add-property! constant 'name name)
    (add-property! constant 'numerical-value value)
    (add-property! constant 'units units)
    (add-property! constant 'tex-string tex-string)
    (add-property! constant 'description description)
    (if (real? value) (declare-known-reals name))
    (if (not (default-object? uncertainty))
      (add-property! constant 'uncertainty uncertainty))
    (*numerical-constants* (cons constant (*numerical-constants*)))
    (environment-define scmutils-base-environment
                        name
                        (with-units value units))
    name))

(define (numerical-constants #:optional units? constants)
  (if (default-object? units?) (set! units? #t))
  (if (default-object? constants) (set! constants (*numerical-constants*)))
  (for-each (lambda (c)
              (environment-assign!
               scmutils-base-environment
               (get-property c 'name)
               (if units?
                   (with-units (get-property c 'numerical-value)
                     (get-property c 'units))
                   (g:* (get-property c 'numerical-value)
                        (unit-scale (get-property c 'units))))))
            constants))

(define (symbolic-constants #:optional units? constants)
  (if (default-object? units?) (set! units? #t))
  (if (default-object? constants) (set! constants (*numerical-constants*)))
  (for-each (lambda (c)
              (environment-assign!
               scmutils-base-environment
               (get-property c 'name)
               (if units?
                   (with-units (get-property c 'name)
                     (get-property c 'units))
                   (g:* (get-property c 'name)
                        (unit-scale (get-property c 'units))))))
            constants))

(define (get-constant-data name)
  (find (lambda (c) (eq? (get-property c 'name) name))
         (*numerical-constants*)))

;;; & is used to attach units to a number, or to check that a number
;;; has the given units.

(define (& value u1 #:optional u2)
  (let ((units (if (default-object? u2) u1 u2))
        (scale (if (default-object? u2) 1 u1)))
    (assert (and (not (units? value)) (number? scale) (units? units)))
    (if (with-units? value)
        (if (simple:equal? (unit-exponents units)
                           (unit-exponents (u:units value)))
            value
            (error "Units do not match: &" value units))
        (with-units (g:* scale (unit-scale units) value)
          (make-unit (u:unit-system units)
                     (unit-exponents units)
                     1)))))

(define *unit-constructor* '&)

(define unit-environment generic-environment)

(define (express-as num target-unit-expression)
  (let ((target-unit-expression-value
         (eval target-unit-expression unit-environment)))
    (cond ((with-units? target-unit-expression-value)
           (let ((target-val (u:value target-unit-expression-value))
                 (target-units (u:units target-unit-expression-value)))
             (express-in-given-units (g:/ num target-val)
                                     target-units
                                     target-unit-expression)))
          ((units? target-unit-expression-value)
           (express-in-given-units num
                                   target-unit-expression-value
                                   target-unit-expression))
          (else num))))

(define (express-in-given-units num target-unit target-unit-expression)
  (cond ((with-units? num)
         (let ((value (g:* (unit-scale (u:units num)) (u:value num)))
               (vect (unit-exponents (u:units num))))
           (if (not (simple:equal? vect (unit-exponents target-unit)))
               (error "Cannot express in given units"
                      num target-unit target-unit-expression))
           (list *unit-constructor*
                 (g:/ (expression value) (unit-scale target-unit))
                 target-unit-expression)))
        ((units? num)
         (list *unit-constructor*
               (g:/ (unit-scale num) (unit-scale target-unit))
               target-unit-expression))
        (else num)))

(define (with-units->expression system num)
  (assert (unit-system? system))
  (cond ((with-units? num)
         (let ((value (g:* (unit-scale (u:units num)) (u:value num)))
               (vect (unit-exponents (u:units num))))
           (make-unit-description value vect system)))
        ((units? num)
         (make-unit-description (unit-scale num)
                                (unit-exponents num)
                                system))
        (else num)))

(define (make-unit-description value exponent-vector system)
  (let ((available
         (or (find-unit-description exponent-vector
                                    (base-units system))
             (find-unit-description exponent-vector
                                    (derived-units system)))))
    (if available
        (let ((unit-name (car available))
              (scale (unit-scale (list-ref available 3))))
          (list *unit-constructor*
                (g:simplify (g:/ value scale))
                unit-name))
        (list *unit-constructor*
              (g:simplify value)
              (unit-expresson (vector->list exponent-vector)
                              (map car (base-units system)))))))


(define (find-unit-description vect ulist)
  (find (lambda (entry)
           (simple:equal? (unit-exponents (list-ref entry 3))
                          vect))
         ulist))

(define (find-unit-name vect ulist)
  (let ((v (find-unit-description vect ulist)))
    (if v (car v) #f)))

(define (unit-expresson exponents base-unit-names)
  (cons '*
        (apply append
               (map (lambda (exponent base-name)
                      (cond ((g:zero? exponent) '())
                            ((g:one? exponent) (list base-name))
                            (else
                             (list (list 'expt base-name exponent)))))
                    exponents
                    base-unit-names))))

;;bdk;; was in convert
(define-syntax unit-convert
  (syntax-rules ()
    ((unit-convert expr target-unit)
     (express-as expr 'target-unit))))

#|
(with-units->expression SI &foot)
;Value: (& .3048 &meter)

(with-units->expression SI (& 2 &foot))
;Value: (& .6096 &meter)

(with-units->expression SI (/ (* :k (& 300 &kelvin)) :e))
;Value: (& .02585215707677003 &volt)

(with-units->expression SI :c)
;Value: (& 299792458. (* &meter (expt &second -1)))

(with-units->expression SI :h)
;Value: (& 6.6260755e-34 (* (expt &meter 2) &kilogram (expt &second -1)))
|#


#|
;;; Work in progress

(define (foosh x)
  (let* ((logscale (round->exact (log10 x)))
	 (scale (expt 10 logscale))
	 )
    (list (/ x scale) scale)
  ))

(foosh 3/1000)
#|
(3 1/1000)
|#
|#
