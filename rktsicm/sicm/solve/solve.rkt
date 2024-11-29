#lang racket/base

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" if any every delete find hash-table-ref/default hash-table-set!
                  iota symbol generate-uninterned-symbol)
         (only-in "../rkt/define.rkt" define default-object?)
         "../kernel-intr.rkt"
         "../simplify.rkt"
         "../general/list-utils.rkt"
         "../general/sets.rkt"
         "../general/assert.rkt"
         "../general/eq-properties.rkt"
         "../general/equals.rkt"
         )

;;bdk;; start original file

;;;; Simple catch-as-catch-can solver #42.  
;;;  By GJS, November 2003.  For use with SCMUTILS.
;;;  Updated by GJS, 7 April 2008.
;;;  Updated by GJS, 8 April 2009, with suggestions by AXCH.
;;;  Improved by GJS, 16 December 2009.
;;;  Improved by GJS, 10 April 2014: if progress, hopeless->residual.
;;;  New version by GJS 26 Nov 2016: improved, added quadratic
;;;  New version by GJS December 2017: root premises


;;; Must be loaded before handler.scm if used in constraint propagator.
;;; Assumes standardized equations, as produced below.

#|
;;; The basic top-level procedures are:

(define (solve-incremental equations variables) ...)

;;; Variables list gives variables to be eliminated.

;;; To access the parts of a solution returned by solve-incremental

(define (residual-equations solution) ...)
(define (residual-variables solution) ...)
(define (substitutions solution) ...)
(define (tough-equations solution) ...)

;;; To access substitutions produced by solve-incremental

(define (substitution-variable subst) ...)
(define (substitution-expression subst) ...)
(define (substitution-justifications subst) ...)

;;; To access residual equations of a solution

(define (equation-expression eqn) ...)
(define (equation-justifications eqn) ...)

(define (simple-solve struct unknowns #:optional knowns show-eqns?)
  ...)

;;; Examples are given in comments.

;;; Idea of program
We are looking to accumulate a substitution for
each variable, and use it to eliminate that
variable from the resulting equations and
previously acquired substitutions.
The general strategy is:
1. Look for an equation for which a variable
   can be isolated.
2. Choose a variable to eliminate.
3. Isolate and make a substitution for that
   variable.
4. Use the substitution to eliminate the variable
   from the remaining equations.
5. Use the substitution to eliminate the variable
   from the accumulated substitutions.
6. If more variables and more equations,
     go to step #1.
|#

(define (make-solution resid-eqs resid-vars substs tough)
  (list resid-eqs resid-vars substs tough))

(define (residual-equations solution) (car solution))
(define (residual-variables solution) (cadr solution))
(define (substitutions solution) (caddr solution))
(define (tough-equations solution) (cadddr solution))


(define *outstanding-contradictions* (make-parameter (gensym 'undefined)))

(define (solve-incremental equations variables #:optional succeed fail)
  (define *solver-state* #f)
  (define (default-succeed result fail)
    result)
  (define (default-fail)
    (if (null? (*outstanding-contradictions*))
        `(failed ,@*solver-state*)
        `(contradictions ,@(*outstanding-contradictions*))))
  (if (default-object? fail) (set! fail default-fail))
  (if (default-object? succeed) (set! succeed default-succeed))
  (*outstanding-contradictions* '())

  (let lp ((residual-eqs equations)
	   (residual-vars     variables)
	   (substitutions     '())
	   (tough-eqns '())
	   (progress #f)
	   (fail fail))
  
    (define (current-solution)
      (let ((solver-state
             (make-solution
              residual-eqs residual-vars substitutions tough-eqns)))
        (set! *solver-state* solver-state)
        solver-state))

    (define (win)
      (succeed (current-solution) fail))

;;; Continued on next page

;;; Continued

    (cond ((null? residual-vars)
           (cond ((correct-substitutions? equations substitutions)
                  (win))
                 ((and (not (any contradictory-equation? residual-eqs))
                       (not (any contradictory-equation? tough-eqns)))
                  (win))
                 (else (fail))))
          ((null? residual-eqs)
           (cond ((null? tough-eqns)
                  (if (correct-substitutions? equations substitutions)
                      (win)
                      (fail)))
                 (progress
                  (lp (append residual-eqs tough-eqns)
                      residual-vars
                      substitutions
                      '()
                      #f fail))
                 (else (fail))))
          (else
           (let ((eqs (sort residual-eqs less-difficult?)))
             (let ((eqn (car eqs)))
                   (let ((vars
                          (lset-intersection simple:equal?
                                             residual-vars (equation-variables eqn))))
                     (if (null? vars)       ; contradiction!
                         (contradiction-failure (list eqn) fail)
                         (let ((var (car (sort vars (lower-order? eqn)))))
                           (isolate-var var eqn
                                        (lambda (new-substitution fail)	
                                          (use-new-substitution
                                           new-substitution
                                           (delete eqn residual-eqs simple:equal?)
                                           substitutions
                                           tough-eqns
                                           (lambda (new-residuals
                                                    new-substitutions
                                                    new-tough
                                                    fail)
                                             (lp new-residuals
                                                 (delete var residual-vars simple:equal?)
                                                 new-substitutions
                                                 new-tough
                                                 #t fail))
                                           fail))
                                        (lambda ()     ; eqn too hard now
                                          (lp (delete eqn residual-eqs simple:equal?)
                                              residual-vars
                                              substitutions
                                              (cons eqn tough-eqns)
                                              progress fail))))))))))))

(define (correct-substitutions? equations substitutions)
  (every (lambda (equation)
           (let ((expr
                  (equation-expression
                   (apply-substitutions-to-equation equation
                                                    substitutions))))
             (and (number? expr) (~0? expr))))
         equations))

(define (contradiction-failure contradictions fail)
  (parameterize ([*outstanding-contradictions*
               (append contradictions (*outstanding-contradictions*))])
    (fail)))

(define (use-new-substitution newsubst oldresids oldsubsts oldtough
                              succeed fail)
  (let ((new-substitutions
	 (cons newsubst (next-substitutions newsubst oldsubsts)))
	(new-equations
	 (flush-tautologies (next-equations newsubst oldresids)))
	(new-tough
	 (flush-tautologies (next-equations newsubst oldtough))))
    (let ((contradictions
	   (append (filter contradictory-equation? new-equations)
		   (filter contradictory-equation? new-tough))))
      (cond ((null? contradictions)
	     (succeed new-equations new-substitutions new-tough fail))
	    (else
	     (contradiction-failure contradictions fail))))))

(define (contradictory-equation? eqn)
  (let ((expr (equation-expression eqn)))
    (and (number? expr) (not (~0? expr)))))

(define (flush-tautologies equations)
  (filter (lambda (eqn) 
	    (let ((expr (equation-expression eqn)))
	      (not (and (number? expr) (~0? expr)))))
	  equations))

(define (next-equations new-substitution old-equations)
  (let ((new-justs (substitution-justifications new-substitution)))
    (map (lambda (old-equation)
           (backsubstitute-equation new-substitution old-equation))
         (filter (lambda (old-equation)
                   (allowed-substitution? new-justs
                    (equation-justifications old-equation)))
                 old-equations))))

(define (next-substitutions new-substitution old-substitutions)
  (let ((new-justs (substitution-justifications new-substitution)))
    (map (lambda (old-substitution)
           (backsubstitute-substitution new-substitution 
                                        old-substitution))
         (filter (lambda (old-substitution)
                   (allowed-substitution? new-justs
                    (substitution-justifications old-substitution)))
                 old-substitutions))))

(define (allowed-substitution? substitution-justs template-justs)
  (not (any (lambda (premise)
                (and (root-premise? premise)
                     (member (root-premise-opposite premise)
                             template-justs
                             simple:equal?)))
              substitution-justs)))    

(define (isolate-var var eqn succeed fail)
  ;; succeed = (lambda (new-substitution fail) ...)
  ;; fail    = (lambda () ...)
  (isolatable? var (equation-expression eqn)
               (lambda (value fail . root-premises)
                 ;; If there are multiple roots, we get root premises supporting
                 ;; the choice of root.  Seq quadratic-formula, below.
                 (let ((justs (equation-justifications eqn)))
                   ;;(pp `(isolate ,eqn ,value ,root-premises ,justs))
                   (if (any (lambda (root-premise)
                                (member (root-premise-opposite root-premise)
                                        justs
                                        simple:equal?))
                              root-premises)
                       (error "This should not happen--ISOLATE-VAR") ;(fail)
                       (succeed
                        (make-substitution var value
                                           (just-union root-premises justs))
                        fail))))
               fail))
	
(define (isolatable? var expr succeed fail)
  (let lp ((expr expr) (fail fail))
    ;; expr is residual asserted to be zero 
    (cond ((simple:equal? var expr)
           (succeed 0 fail))
          ((positive-power? expr)
	   (lp (car (operands expr)) fail))
          ((product? expr)
           (let flp ((factors (operands expr)))
             (if (pair? factors)
                 (let ((ff (car factors)))
                   (lp ff
                       (lambda () (flp (cdr factors)))))
                 (fail))))
          ((quotient? expr)             ;numerator
           (lp (car (operands expr)) fail))
          ((sum? expr)
           (fpf-analyze expr
             (lambda (analyzed kernel-map)
               (if (occurs? var kernel-map)
                   (if (occurs? var analyzed)
                       (fail)
                       (kernel-subproblem var analyzed kernel-map
                                          succeed fail))
                   (algebra-problem var analyzed succeed fail)))))
          (else (fail)))))

(define (fpf-analyze expr cont)
  ;; cont = (lambda (analyzed kernel-map) ...)
  ((initializer fpf:analyzer))
  (let ((analysis
         ((expression-analyzer fpf:analyzer)
          expr)))
    (cont analysis
          ((auxiliary-variable-fetcher fpf:analyzer)))))

(define (algebra-problem var analyzed succeed fail)
  (collect-terms var analyzed
                 (lambda (c b a high)
                   (cond ((not (g:zero? high)) (fail))
                         ((not (g:zero? a)) ;a*x^2+b*x+c=0
                          (quadratic-formula a b c
                                             succeed fail))
                         ((not (g:zero? b)) ;b*x+c=0
                          (linear-formula b c
                                          succeed fail))
                         ((not (g:zero? c))
                          ;;(error "Contradiction")
                          (fail))))))

(define (kernel-subproblem var analyzed kernel-map succeed fail)
  (let ((kernels (map car kernel-map))
        (top-level-kspecs 
         (filter (lambda (kentry) (occurs? (car kentry) analyzed)) kernel-map)))
    (cond ((null? top-level-kspecs) (error "Non-kernel residue" analyzed) #f)
          ((null? (cdr top-level-kspecs))
           ;; I can handle only one kernel in a residue
           (let* ((kspec (car top-level-kspecs))
		  (kvar (car kspec)) (kexp (cadr kspec))
		  (koperator (operator kexp)) (krands (operands kexp)))
	     (let ((karg (car krands)) 
		   (opspec (kernel-operator-spec koperator))) ;invertible?
	       ;; solve for kernel var and invert kernel operator
	       ;; to get kargval, the value for karg
	       (if opspec      
		   (if (not (null? (cdr krands))) ;only 1arg kernel operators
		       (fail)
		       (isolatable? kvar analyzed
		         (lambda (kvar-value fail)
                           (let* ((kargval ((kernel-invert opspec) kvar-value))
                                  (to-solve (s:simplify (symb:- karg kargval))))
                             (if (any (lambda (kernel) (occurs? kernel karg)) kernels)
                                 (kernel-subproblem var to-solve kernel-map succeed fail)
                                 (isolatable? var to-solve succeed fail))))
                         fail))
		   (isolatable? var (s:simplify (substitute kexp kvar analyzed))
                                succeed fail)))))
	  (else (fail)))))

(define (kernel-operator-spec op)
  (assoc op *kernel-operator-table* simple:equal?))

(define *kernel-operator-table*
  `( (sqrt ,symb:square)
     (exp ,symb:log) (log ,symb:exp)
     (sin ,symb:asin) (asin ,symb:sin)
     (cos ,symb:acos) (acos ,symb:cos)
     (tan ,symb:atan) (atan ,symb:tan) ))

(define (kernel-invert opspec)
  (cadr opspec))

(define (collect-terms var analyzed cont)
  ;; Assumes fpf. finds coefficients of powers of var in order:
  ;; cont = (lambda (constant linear quadratic higher) ...)
  (assert (sum? analyzed))
  (let ((terms (operands analyzed)))
    (assert (not (null? terms)))
    (let lp ((terms terms) (const 0) (lin 0) (quad 0) (high 0))
      (define (assimilate-expt t coeff)
        (let ((exponent (cadr (operands t))))
          (if (number? exponent)
              (cond ((= exponent 2)
                     (lp (cdr terms)
                         const
                         lin
                         (symb:sum coeff quad)
                         high))
		    (else
		     (lp (cdr terms)
			 const
			 lin
			 quad
			 (symb:sum coeff high))))
              (lp (cdr terms)
                  const
                  lin
                  quad
                  (symb:sum coeff high)))))
      (if (null? terms)
          (cont (s:simplify const)
                (s:simplify lin)
                (s:simplify quad)
                (s:simplify high))
          (let ((t (car terms)))
            (if (occurs? var t)
                (cond ((simple:equal? var t)
                       (lp (cdr terms) const (symb:sum 1 lin) quad high))
                      ((expt? t)
                       (assimilate-expt t 1))
                      ((product? t)
                       (let ((f
                              (find (lambda (factor) (occurs? var factor))
                                    (operands t))))
                         (let ((others (delete f t simple:equal?)))
                           (cond ((simple:equal? var f)
                                  (lp (cdr terms)
                                      const
                                      (symb:sum others lin)
                                      quad
                                      high))
                                 ((expt? f)
                                  (assimilate-expt f others))
                                 (else (error "not in fpf"))))))
                      (else (error "not in fpf")))
                (lp (cdr terms)
                    (symb:sum t const) lin quad high)))))))

(define (positive-power? expr)
  (and (expt? expr)
       (number? (cadr (operands expr)))
       (> (cadr (operands expr)) 0)))

(define (linear-formula m b succeed fail)
  (succeed (symb:quo (symb:negate b) m)
           fail))

;;; There may be two roots to a quadratic!  So we need to have
;;; hypothetical premises for each choice.

#|
(define (make-root-premise-pair name)
  (let ((hyps
         (list (list 'hypothetical (cons '+ name))
               (list 'hypothetical (cons '- name)))))
    (eq-put! (car hyps) 'opposite (cadr hyps))
    (eq-put! (cadr hyps) 'opposite (car hyps))
    (eq-put! (car hyps) 'root-premise #t)
    (eq-put! (cadr hyps) 'root-premise #t)
    hyps))
|#

(define (make-root-premise-pair equation-name)
  (let ((+premise-name (cons '+ equation-name))
        (-premise-name (cons '- equation-name)))
    (let ((+premise (make-root-premise +premise-name))
          (-premise (make-root-premise -premise-name)))
      (eq-put! +premise 'opposite -premise)
      (eq-put! -premise 'opposite +premise)
      (eq-put! +premise 'root-premise #t)
      (eq-put! -premise 'root-premise #t)
      (list +premise -premise))))
         
(struct hypothetical (name extra) #:constructor-name make-hypothetical #:transparent)

#|
;;; Adding to simplify/default.scm. 
;;; This does not work!  Not clear why...

(define (simplify-hypothetical expr)
  `(hypothetical ,(hypothetical-name expr)))

(assign-operation 'simplify simplify-hypothetical hypothetical?)

(define hypothetical make-hypothetical)
|#

(define hypothetical-memory (make-hash))

(define (make-root-premise premise-name)
  ;; Assumes name is a list of elts by eqv?
  (hash-table-intern! hypothetical-memory
                      premise-name
                      (lambda ()
                        (make-hypothetical premise-name #f))))

(define (hash-table-intern! table key get-value)
  (or (hash-table-ref/default table key #f)
      (let ((value (get-value)))
        (hash-table-set! table key value)
        value)))

(define (root-premise? thing)
  (eq-get thing 'root-premise))

(define (root-premise-opposite root-premise)
  (eq-get root-premise 'opposite))

(define (quadratic-formula a b c succeed fail)
  (define (two-roots r1 r2)
    (let ((hyps
           (make-root-premise-pair
            (list 'quadratic a b c))))
      ;;The extra arg to succeed is a root premise
      ;; see isolate-var, above.
      (succeed r1     
               (lambda ()
                 (succeed r2 fail (cadr hyps)))
               (car hyps))))
  (define (quadratic-symbolic a b c)
    (let ((sd
           (symb:sqrt
            (symb:difference (symb:square b)
                             (symb:product 4 a c))))
          (-b (symb:difference b))
          (2a (symb:product 2 a)))
      (two-roots
       (symb:quotient (symb:sum -b sd) 2a)
       (symb:quotient (symb:difference -b sd) 2a))))
  ;;(cpp `(a= ,a  b= ,b  c= ,c))
  (if (and (number? a) (number? b) (number? c))
      (quadratic a b c  ;from src/kernel/numeric.scm
                 two-roots              ;real
                 two-roots              ;complex
                 (lambda (r)            ;double
                   (succeed r fail))
                 fail                   ;linear
                 fail)                  ;no solution
      (quadratic-symbolic a b c)))

(define (backsubstitute-substitution new-substitution substitution)
  (if (occurs? (substitution-variable new-substitution)
	       (substitution-expression substitution))
      (make-substitution
       (substitution-variable substitution)
       (substitute (substitution-expression new-substitution)
		   (substitution-variable new-substitution)
		   (substitution-expression substitution))
       (just-union (substitution-justifications new-substitution)
		   (substitution-justifications substitution)))
      substitution))

(define (backsubstitute-equation substitution equation)
  (if (occurs? (substitution-variable substitution)
	       (equation-expression equation))
      (make-equation
       (substitute (substitution-expression substitution)
		   (substitution-variable substitution)
		   (equation-expression equation))
       (just-union (substitution-justifications substitution)
		   (equation-justifications equation)))
      equation))

(define (substs->equations substs)
  (map subst->equation substs))

(define (subst->equation subst)
  (make-equation
   (symb:- (substitution-variable subst)
	   (substitution-expression subst))
   (substitution-justifications subst)))

(define (apply-substitutions expression substitutions)
  (let loop ((expression expression)
	     (substs substitutions)
	     (justs '()))
    (cond  ((null? substs) (cons expression justs))
	   ((occurs? (substitution-variable (car substs)) expression)
	    (loop (s:simplify
		   (substitute (substitution-expression (car substs))
			       (substitution-variable (car substs))
			       expression))
		  (cdr substs)
		  (just-union (substitution-justifications (car substs))
			      justs)))
	   (else (loop expression (cdr substs) justs)))))

(define (apply-substitutions-to-equation equation substitutions)
  (let ((result
	 (apply-substitutions (equation-expression equation)
			      substitutions)))
    (let ((expression (car result)) (justs (cdr result)))
      (make-equation expression         ;not just-union; already done.
	(lset-union simple:equal? (equation-justifications equation) justs)))))

(define (make-substitution var value justs)
  (if (any (lambda (just)
                 (and (root-premise? just)
                      (member (root-premise-opposite just)
                              justs
                              simple:equal?)))
               justs)
      (begin (error "Aargh-subst") #f))
  (list (list '= var (s:simplify value)) justs))

(define (substitution-variable subst) (cadar subst))
(define (substitution-expression subst) (caddar subst))
(define (substitution-justifications subst) (cadr subst))


(define (make-equation expr justs)
  (let* ((specs (standardize-equation expr '() '() #f))
	 (pexpr (car specs))
	 (vspecs (cadr specs)))
    (if (any (lambda (just)
                   (and (root-premise? just)
                        (member (root-premise-opposite just)
                                justs
                                simple:equal?)))
                 justs)
      (begin (error "Aargh-eqn") #f))
    (list pexpr justs vspecs)))

(define (equation-expression eqn) (car eqn))
(define (equation-justifications eqn) (cadr eqn))
(define (equation-variables eqn) (caddr eqn))

(define s:simplify g:simplify)

(define (occurs? var expr)
  (or (simple:equal? var expr)
      (and (pair? expr)
	   (or (occurs? var (car expr))
	       (occurs? var (cdr expr))))))

(define ((variable-present? var) eqn)
  (member var (equation-variables eqn) simple:equal?))

(define (fewer-variables? eqn1 eqn2)
  (< (length (equation-variables eqn1))
     (length (equation-variables eqn2))))

(define (equation-difficulty equation)
  (apply +
         (map (max-exponent (equation-expression equation))
              (equation-variables equation))))

(define ((max-exponent expression) var)
  (let lp ((expr expression))
    (cond ((null? expr) 0)
	  ((simple:equal? expr var) 1)
	  ((expt? expr)
	   (if (simple:equal? (car (operands expr)) var)
	       (cadr (operands expr))
	       0))
	  ((list? expr) (apply max (map lp expr)))
	  (else 0))))

(define (less-difficult? eqn1 eqn2)
  (< (equation-difficulty eqn1) (equation-difficulty eqn2)))

(define (lower-order? eqn)
  (let ((me (max-exponent (equation-expression eqn))))
    (lambda (v1 v2)
      (< (me v1) (me v2)))))


(define (just-union j1s j2s)
  (lset-union simple:equal? j1s j2s))

#|
(define (just-union j1s j2s)
  (if (null? (*outstanding-contradictions*))
      (lset-union simple:equal? j1s j2s)
      (apply lset-union simple:equal? j1s j2s
             (map contradiction-justifications
                  (*outstanding-contradictions*)))))
|#

;;; A contradiction is an equation where the expression 
;;; cannot be zero.

(define (make-contradiction expr justs vars)
  (list expr justs vars))

(define (contradiction-expression c)
  (car c))

(define (contradiction-justifications c)
  (cadr c))

(define (contradiction-variables c)
  (caddr c))

;;; This stuff is in support of standardize-equation, below.

(define *zero-threshold* 1e-15)		;for small numbers

(define (differential-operator? expression)
  (or (D? expression) (Dn? expression)))

(define (D? x)
  (and (pair? x)
       (eq? (car x) 'D)))

(define (D2? x)
  (and (pair? x) 
       (simple:equal? (car x) '(expt D 2))))

(define (Dn? x)
  (and (pair? x) 
       (expt? (car x))
       (eq? (car (operands (car x))) 'D)))


;;; The procedure standardize-equation is a wierd device that performs
;;; several functions.  It walks the residual, finding the variables
;;; that one might want to solve for and adds them to the given
;;; variables.  This is the new-map.  Given an independent variable,
;;; say t, it finds expression that are functions of t, such as (f t),
;;; ((D f) t), (((expt D 2) f) t) and adds them to the given functions
;;; and the map.  This is useful for hacking differential equations,
;;; by extracting the time functions from the algebraic skeleton.

;;; This code also finds very small numbers and makes them zero, to
;;; improve simplification -- this is questionable.

#|
;;; For example...
(cpp (standardize-equation '(- (* 3 ((D f) t))
			      (+ (* (sqrt x) z (f t))
				 (g t)
				 (((expt D 2) g) t)
				 (square y)))
			  '() '() 't))
((+ (* -1 z (f t) (sqrt x))
    (* -1 (expt y 2))
    (* 3 ((D f) t))
    (* -1 (g t))
    (* -1 (((expt D 2) g) t)))
 ((((expt D 2) g) t) (g t) ((D f) t) y x (f t) z)
 (((expt D 2) g) g (D f) f))
|#

(define (standardize-equation residual variables functions variable #:optional continue)
  ;; returns list = (new-residual new-map functions)
  (let ((redo #f))	; True if an inexact number becomes exact
    (define (walk-expression expression map functions continue)
    (cond ((pair? expression)
           (let ((rator (operator expression))
                 (rands (operands expression)))
             (cond ((and (= (length rands) 1) (eq? (car rands) variable))
                    (continue expression
                              (list-adjoin expression map)
                              (list-adjoin rator functions)))
                   ((differential-operator? expression)
                    (continue expression
                              map
                              (list-adjoin expression functions)))
                   (else
                    (walk-expression rator map functions
                                     (lambda (rator-result rator-map rator-functions)
                                       (walk-list rands rator-map rator-functions
                                                  (lambda (rands-result rands-map rands-functions)
                                                    (continue (cons rator-result rands-result)
                                                              rands-map
                                                              rands-functions)))))))))
          ((number? expression)
           (continue (if (and (inexact? expression)
                              (< (magnitude expression) *zero-threshold*))
                         (begin (set! redo #t) 0)
                         expression)
                     map
                     functions))
          ((memq expression '(+ - / * D expt sqrt exp log sin cos))
           (continue expression map functions))
          (else
           (continue expression (list-adjoin expression map) functions))))
  (define (walk-list elist map functions continue)
    (if (pair? elist)
        (walk-expression (car elist) map functions
                         (lambda (car-result car-map car-functions)
                           (walk-list (cdr elist) car-map car-functions 
                                      (lambda (cdr-result cdr-map cdr-functions)
                                        (continue (cons car-result cdr-result)
                                                  cdr-map
                                                  cdr-functions)))))
        (continue elist map functions)))
  (let lp ((residual (s:simplify residual)))
    (walk-expression (if (quotient? residual)
                         (symb:dividend residual)
                         residual)
                     variables
                     functions
                     (lambda (expression map funs)
                       (if redo
                           (begin (set! redo #f)
                                  (lp (s:simplify expression)))
                           ((if (default-object? continue)
                                  list
                                  continue)
                              expression map funs)))))))

#|
;;; Signs of life.  

(cpp (solve-incremental
      (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	    (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
      '(x y)))
#|
(() () (((= y 1) (B A)) ((= x 2) (B A))) ())
|#

(cpp (solve-incremental
      (list (make-equation '(+  x   y   z  1)  (list 'A))
	    (make-equation '(+  x   y      2)  (list 'B))
	    (make-equation '(+  x          1)  (list 'C)))
      '(x y z)))
#|
(() () (((= z 1) (A B C)) ((= y -1) (B C)) ((= x -1) (C))) ())
|#

(cpp (solve-incremental
      (list (make-equation '(+  x          1)  (list 'C))
	    (make-equation '(+  x   y      2)  (list 'B))
	    (make-equation '(+  x   y   z  1)  (list 'A)))
      '(x y z)))
#|
(() () (((= z 1) (A B C)) ((= y -1) (B C)) ((= x -1) (C))) ())
|#

;;; The following signals a contradiction, as it should:

(cpp (solve-incremental
      (list (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	    (make-equation '(+ (* 3 x)     y  -5)  (list 'B)))
      '(x y)))
#|
(contradictions (-2 (A B) ()) (2 (B A) ()))
|#

;;; Some slightly nonlinear systems can be solved:

(cpp (solve-incremental
      (list (make-equation '(-  3 (+ x y))  (list 'A))
	    (make-equation '(-  5 (- x y))  (list 'B))
	    (make-equation '(-  3 (+ (* (sqrt x) z) (square y)))  (list 'C)))
      '(x y z)))
#|
(() () (((= z 1) (C B A)) ((= y -1) (B A)) ((= x 4) (B A))) ())
|#

;;; Underdetermined systems can be reduced:

(cpp (solve-incremental
      (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
	    (make-equation '(- 3 (+ a b))  (list 'B)))
      '(a b c)))
#|
(() (c) (((= b (+ 3 (* -2/3 c))) (A B)) ((= a (* 2/3 c)) (A B))) ())
|#

(cpp (solve-incremental
      (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
	    (make-equation '(- 3 (- a c))  (list 'B)))
      '(a b c)))
#|
(() (c) (((= b (+ -3 (* -4/3 c))) (A B)) ((= a (+ 3 c)) (B))) ())
|#

;;; Even very hard ones are clarified.

(cpp (solve-incremental
      (list (make-equation '(+ (* (+ a b) (- a c)) c)  (list 'A))
	    (make-equation '(- 3 (- a b))  (list 'B)))
      '(a b c)))
#|
(()
 (b)
 (((= c (/ (+ 9 (* 2 (expt b 2)) (* 9 b)) (+ 2 (* 2 b)))) (A B))
  ((= a (+ 3 b)) (B)))
 ())
|#

;;; The following are permutations of the solution sequence

(cpp (solve-incremental
      (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	    (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	    (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
      '(x y z)))
#|
(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
|#

(cpp (solve-incremental
      (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	    (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	    (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
      '(z x y)))
#|
(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
|#

(cpp (solve-incremental
      (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	    (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	    (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
      '(y z x)))
#|
(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
|#

(cpp (solve-incremental
      (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	    (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	    (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
      '(y x z)))
#|
(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
|#

(cpp (solve-incremental
      (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	    (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	    (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
      '(z y x)))
#|
(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
|#

(cpp (solve-incremental
      (list (make-equation '(+ (* (- x (* 2 y)) (expt z 2)) (* 2 z) 1) (list 'C))
	    (make-equation '(+ (* 3 x)     y  -7)  (list 'A))
	    (make-equation '(+ (* 3 x) (- y)  -5)  (list 'B)))
      '(x z y)))
#|
(() () (((= z -1/2) (C B A)) ((= y 1) (B A)) ((= x 2) (B A))) ())
|#

;;; This wins somehow...

(cpp (solve-incremental
      (list (make-equation '(- 200/3 (/ 1 (+ (/ 1 R1) (/ 1 R2))))  (list 'A))
	    (make-equation '(-  1/3 (/ R2 (+ R1 R2)))  (list 'B)))
      '(R1 R2)))
#|
(() () (((= R2 100) (B A)) ((= R1 200) (B A))) ())
|#

(cpp (solve-incremental
      (list (make-equation '(- (* 1/3 (+ R1 R2)) R2)  (list 'B))
	    (make-equation '(- (* 200/3 (+ R1 R2)) (* R1 R2))  (list 'A)))
      '(R1 R2)))
#|
(() () (((= R2 100) (B A)) ((= R1 200) (B A))) ())
|#
;;; How did we avoid the extra root, R2=0 & R1=0, that satisfies the
;;; given equations but not the original problem?

;;; Answer: it is there, but we also have a redundant solutions problem!
(cpp (solve-incremental
      (list (make-equation '(- (* 1/3 (+ R1 R2)) R2)  (list 'B))
	    (make-equation '(- (* 200/3 (+ R1 R2)) (* R1 R2))  (list 'A)))
      '(R1 R2)
      (lambda (sol fail) (cpp sol) (fail))
      (lambda () 'done)))

#|
(()
 ()
 (((= R2 100) (B A (hypothetical (+ quadratic -2 200 0))))
  ((= R1 200) (B A (hypothetical (+ quadratic -2 200 0)))))
 ())
|#
#|
(()
 ()
 (((= R2 0) (B A (hypothetical (- quadratic -2 200 0))))
  ((= R1 0) (B A (hypothetical (- quadratic -2 200 0)))))
 ())
|#
#|
(()
 ()
 (((= R2 100) (B A (hypothetical (+ quadratic -2 200 0))))
  ((= R1 200) (B A (hypothetical (+ quadratic -2 200 0)))))
 ())
|#
#|
(()
 ()
 (((= R2 0) (B A (hypothetical (- quadratic -2 200 0))))
  ((= R1 0) (B A (hypothetical (- quadratic -2 200 0)))))
 ())
|#
#|
(()
 ()
 (((= R2 100) (A B (hypothetical (+ quadratic -6 600 0))))
  ((= R1 200) (A B (hypothetical (+ quadratic -6 600 0)))))
 ())
|#
#|
(()
 ()
 (((= R2 0) (A B (hypothetical (- quadratic -6 600 0))))
  ((= R1 0) (A B (hypothetical (- quadratic -6 600 0)))))
 ())
|#
#|
done
|#


;;; Now can solve quadratics and does backtracking to find a root
(cpp (solve-incremental
      (list (make-equation '(- (expt x 2) 1)  (list 'A))
	    (make-equation '(- x 1)  (list 'B)))
      '(x)))
#|
(() () (((= x 1) (B))) ())
|#

(cpp (solve-incremental
      (list (make-equation '(- (expt x 2) 1)  (list 'A))
	    (make-equation '(- x -1)  (list 'B)))
      '(x)))
#|
(() () (((= x -1) (B))) ())
|#
;;; It doesn't to look at A to get answer, but A constrains the answer.

(cpp (solve-incremental
      (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
	    (make-equation '(- (expt y 2) 9) (list 'B))
	    (make-equation '(- (- y x) 1) (list 'C)))
      '(x y)))
#|
(() () (((= y 3) (B C A)) ((= x 2) (A))) ())
|#

(cpp (solve-incremental
      (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
	    (make-equation '(- (expt y 2) 9) (list 'B))
	    (make-equation '(- (- y x) 2) (list 'C)))
      '(x y)))
#|
(contradictions
 (7 (B C A (hypothetical (- quadratic 1 -9 20))) ())
 (16 (B C A (hypothetical (+ quadratic 1 -9 20))) ())
 (2 (C A B (hypothetical (- quadratic 1 0 -9))) ())
 (56 (C A B (hypothetical (+ quadratic 1 0 -9))) ())
 (-1
  ((hypothetical (- quadratic 1 0 -9)) B
                                       C
                                       A
                                       (hypothetical (- quadratic 1 -5 6)))
  ())
 (-2
  ((hypothetical (- quadratic 1 0 -9)) B
                                       C
                                       A
                                       (hypothetical (+ quadratic 1 -5 6)))
  ())
 (2 (A C B (hypothetical (- quadratic 1 0 -9))) ())
 (2 (A C B (hypothetical (- quadratic 1 0 -9))) ())
 (-7
  ((hypothetical (+ quadratic 1 0 -9)) B
                                       C
                                       A
                                       (hypothetical (- quadratic 1 -5 6)))
  ())
 (-8
  ((hypothetical (+ quadratic 1 0 -9)) B
                                       C
                                       A
                                       (hypothetical (+ quadratic 1 -5 6)))
  ())
 (56 (A C B (hypothetical (+ quadratic 1 0 -9))) ())
 (56 (A C B (hypothetical (+ quadratic 1 0 -9))) ())
 (-1
  ((hypothetical (- quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (- quadratic 1 0 -9)))
  ())
 (-7
  ((hypothetical (- quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (+ quadratic 1 0 -9)))
  ())
 (7 (B C A (hypothetical (- quadratic 1 -5 6))) ())
 (-1
  ((hypothetical (- quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (- quadratic 1 0 -9)))
  ())
 (-7
  ((hypothetical (- quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (+ quadratic 1 0 -9)))
  ())
 (7 (B C A (hypothetical (- quadratic 1 -5 6))) ())
 (-2
  ((hypothetical (+ quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (- quadratic 1 0 -9)))
  ())
 (-8
  ((hypothetical (+ quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (+ quadratic 1 0 -9)))
  ())
 (16 (B C A (hypothetical (+ quadratic 1 -5 6))) ())
 (-2
  ((hypothetical (+ quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (- quadratic 1 0 -9)))
  ())
 (-8
  ((hypothetical (+ quadratic 1 -5 6)) A
                                       C
                                       B
                                       (hypothetical (+ quadratic 1 0 -9)))
  ())
 (16 (B C A (hypothetical (+ quadratic 1 -5 6))) ()))
|#
;;; so it knows the outstanding contradictions...

;;; Here we are left over with a residual equation in z
(cpp (solve-incremental
      (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
	    (make-equation '(- (expt y 2) z) (list 'B))
	    (make-equation '(- (- y x) 2) (list 'C)))
      '(x y)))
#|
((((+ 25 (* -1 z)) (B C A (hypothetical (+ quadratic 1 -5 6))) (z)))
 ()
 (((= y 5) (C A (hypothetical (+ quadratic 1 -5 6))))
  ((= x 3) (A (hypothetical (+ quadratic 1 -5 6)))))
 ())
|#

;;; But we can ask to solve for z
(cpp (solve-incremental
      (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
	    (make-equation '(- (expt y 2) z) (list 'B))
	    (make-equation '(- (- y x) 2) (list 'C)))
      '(x y z)))
#| 
(()
 ()
 (((= z 25) (B C A (hypothetical (+ quadratic 1 -5 6))))
  ((= y 5) (C A (hypothetical (+ quadratic 1 -5 6))))
  ((= x 3) (A (hypothetical (+ quadratic 1 -5 6)))))
 ())
|#

;;; Multiple results can be obtained.

(solve-incremental
 (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A)))
 '(x)
 (lambda (result fail)
   (cpp result)
   (fail))
 (lambda () 'done))
#|
(() () (((= x 3) (A (hypothetical (+ quadratic 1 -5 6))))) ())
|#
#|
(() () (((= x 2) (A (hypothetical (- quadratic 1 -5 6))))) ())
|#
#| done |#


(solve-incremental
 (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
       (make-equation '(+ (expt x 2) (* -7 x) 10)  (list 'B)))
 '(x)
 (lambda (result fail)
   (cpp result)
   (fail))
 (lambda () 'done))
#|
(() () (((= x 2) (A (hypothetical (- quadratic 1 -5 6))))) ())
|#
#|
(() () (((= x 2) (B (hypothetical (- quadratic 1 -7 10))))) ())
|#
#| done |#

(solve-incremental
 (list (make-equation '(+ (expt x 2) (* -5 x) 6)  (list 'A))
       (make-equation '(+ (expt x 2) (* a x) 10)  (list 'B)))
 '(a x)
 (lambda (result fail)
   (cpp result)
   (fail))
 (lambda () 'done))
#|
(()
 ()
 (((= a -19/3) (B A (hypothetical (+ quadratic 1 -5 6))))
  ((= x 3) (A (hypothetical (+ quadratic 1 -5 6)))))
 ())
|#
#|
(()
 ()
 (((= a -19/3) (B A (hypothetical (+ quadratic 1 -5 6))))
  ((= x 3) (A (hypothetical (+ quadratic 1 -5 6)))))
 ())
|#
#|
(()
 ()
 (((= a -7) (B A (hypothetical (- quadratic 1 -5 6))))
  ((= x 2) (A (hypothetical (- quadratic 1 -5 6)))))
 ())
|#
#|
(()
 ()
 (((= a -7) (B A (hypothetical (- quadratic 1 -5 6))))
  ((= x 2) (A (hypothetical (- quadratic 1 -5 6)))))
 ())
|#
#|
(()
 ()
 (((= x 3) (A (hypothetical (+ quadratic 1 -5 6))))
  ((= a -19/3) (B A (hypothetical (+ quadratic 1 -5 6)))))
 ())
|#
#|
(()
 ()
 (((= x 2) (A (hypothetical (- quadratic 1 -5 6))))
  ((= a -7) (B A (hypothetical (- quadratic 1 -5 6)))))
 ())
|#
#| done |#

;;; perhaps a bit of filtering would help?


;;; It now knows about some special functions with inverses

(cpp (solve-incremental
      (list (make-equation '(- 2 (sqrt (+ x 1)))  (list 'A)))
      '(x)))
#| (() () (((= x 3) (A))) ()) |#

(cpp (solve-incremental
      (list (make-equation '(- 2 (acos (sqrt (+ x 1))))  (list 'A)))
      '(x)))
#| (() () (((= x (* -1 (expt (sin 2) 2))) (A))) ()) |#
|#

#|
;;; A real use. Note how dependencies keep track of contributions to solution
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
    (list 'K)) ))

(define unknowns
  '(siga0 siga1 siga2 siga3 siga4 sigd0 sigd1 sigd2 sigd3 sigd4 sa0 sa1 sd0 sd1))

(define solution (solve-incremental equations unknowns))

(cpp solution)
#|
(()					; no residuals left
 (sa0 sa1 sd0 sd1 siga3 sigd3)		; excess variables
 (((= sigd4				; substitutions
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
|#

;;; Check
(cpp (map (lambda (equation)
	    (apply-substitutions-to-equation equation
					     (substitutions solution)))
	  equations))
#|
((0 (B F H D K E J A) ())
 (0 (F H D B) ())
 (0 (B F H D C) ())
 (0 (B F H D) ())
 (0 (B F H D J E) ())
 (0 (B H D F) ())
 (0 (B F H D G) ())
 (0 (B F D H) ())
 (0 (B F H D I) ())
 (0 (B F H D E J) ())
 (0 (B F H D A E J K) ()))
|#
|#

;;; To make this less painful, 
;;; given a structure of residuals that should be zero.


(define (simple-solve struct unknowns #:optional knowns show-eqns?)
  (define (make-equations resids)
    (map (lambda (e n)
	   (make-equation e (list (symbol 'eq: n))))
	 resids
	 (iota (length resids))))
  (define (make-substitutions news olds expression)
    (assert (= (length news) (length olds)))
    (let lp ((n news) (o olds) (expression expression))
      (cond ((null? n) expression)
	    ((simple:equal? (car n) (car o))
	     (lp (cdr n) (cdr o) expression))
	    (else
	     (lp (cdr n)
		 (cdr o)
		 (substitute (car n) (car o) expression))))))
  (let* ((knowns (if (default-object? knowns) '() knowns))
	 (internal-unknowns
	  (map (lambda (unk)
		 (if (symbol? unk)
		     unk
		     (generate-uninterned-symbol 'x)))
	       unknowns))
	 (internal-knowns
	  (map (lambda (kn)
		 (if (symbol? kn)
		     kn
		     (generate-uninterned-symbol 'k)))
	       knowns))
	 (eqns
	  (flush-tautologies
	   (make-equations
	     (map (lambda (resid)
		    (make-substitutions internal-knowns knowns
		      (make-substitutions internal-unknowns unknowns
					  (s:simplify resid))))
		  (s:fringe struct)))))
	 (solns
	  (make-substitutions knowns internal-knowns
	    (make-substitutions unknowns internal-unknowns 
	      (solve-incremental eqns internal-unknowns)))))
    (if (not (or (default-object? show-eqns?)
                   (not show-eqns?)))
      (println eqns))
    #| ;;; Check solver for wrong solutions.
    (cpp (s:simplify
	 (make-substitutions (map substitution-expression (substitutions solns))
			     (map substitution-variable (substitutions solns))
			     (map simplify (s:fringe struct)))))
    |#
    (cons '*solution* solns)))

#|
(simple-solve
 (up '(+ (* 3 x)     y  -7)
     '(+ (* 3 x) (- y)  -5))
 '(x y))
#|
(*solution* ()
	    ()
	    (((= y 1) (eq:1 eq:0))
	     ((= x 2) (eq:1 eq:0)))
	    ()) 
|#

(simple-solve
 (up '(+ (* 3 (f x))     (g y)  -7)
     '(+ (* 3 (f x)) (- (g y))  -5))
 '((f x) (g y))
 '()
 #t)
(((+ -5 (* 3 G439) (* -1 G440)) (eq:0) (G440 G439))
 ((+ -7 (* 3 G439) G440) (eq:1) (G440 G439)))
#|
(*solution* ()
	    ()
	    (((= (g y) 1) (eq:1 eq:0))
	     ((= (f x) 2) (eq:1 eq:0)))
	    ()) 
|#

(simple-solve
 (up '(+ (* 3 (f x))     (g y)  (H q))
     '(+ (* 3 (f x)) (- (g y))  -5))
 '((f x) (g y))
 '((H q))
 #t)
(((+ -5 (* 3 x57) (* -1 x58)) (eq:0) (x58 x57))
 ((+ k59 (* 3 x57) x58) (eq:1) (x58 x57 k59)))
#|
(*solution* ()
	    ()
	    (((= (g y) (+ -5/2 (* -1/2 (H q)))) (eq:1 eq:0))
	     ((= (f x) (+ 5/6 (* -1/6 (H q)))) (eq:1 eq:0)))
	    ())
|#
|#
