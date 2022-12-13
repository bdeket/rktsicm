#lang s-exp "../generic.rkt"

(provide (all-defined-out))

(require (for-syntax "../generic.rkt"
                     (only-in racket/syntax format-id))
         )


;;bdk;; start original file

;; (define-values (v1 v2 v3 ...) values-expr)

#; ;;bdk;; we get this from racket
(define-syntax define-values
  (er-macro-transformer
   (lambda (exp r c)
     (let* ((names (cadr exp))
            (values-expr (caddr exp))
            (temps (map generate-uninterned-symbol names)))
       `(,(r 'begin)
         ,@(map (lambda (name) 
                  `(,(r 'define) ,name))
                names)
         (,(r 'call-with-values)
          (,(r 'lambda) () ,values-expr)
          (,(r 'lambda) ,temps
                        ,@(map (lambda (name temp) 
                                 `(,(r 'set!) ,name ,temp))
                               names temps))))))))
	     

;; Use:
;; (define-coordinates (up x y z (down p q) ...) coord-sys)
;; Should expand into
;;(begin 
;;  (define x <x-coordinate-fn>)
;;  ...
;;  (define d/dx <x-basis-vector>)
;;  ...
;;  (define dx <x-basis-one-form>))

(define-syntax (define-coordinates stx)
  (syntax-case stx ()
    [(_ names coord-sys)
     (let ()
       ;get-symbol-names as syntax
       (define (make-getter strc sym)
         (define (inner stx)
           (cond
             [(let ([lst (syntax->list stx)])
                (and lst (memq (syntax-e (car lst)) '(up down)) lst))
              => (λ (lst)(strc (car lst) (map inner (cdr lst))))]
             [(symbol? (syntax-e stx)) (sym stx)]
             [else (error "bad coordinate prototype" (syntax->datum stx))]))
         inner)
       (define ->proto (make-getter (λ (c r) (cons c r)) (λ (s) (list 'quote (syntax-e s)))))
       (define ->symbs (make-getter (λ (c r) (apply append r)) list))

       (let* ([coord-symbs (->symbs #'names)]
              [coord-vector-syms 
               (map (lambda (sym) (format-id sym "d/d~a" sym #:source sym #:props sym)) coord-symbs)]
              [coord-one-form-syms 
               (map (lambda (sym) (format-id sym "d~a" sym #:source sym #:props sym)) coord-symbs)])
       (quasisyntax/loc
           stx
         (define-values #,(append coord-symbs
                                  coord-vector-syms
                                  coord-one-form-syms)
           (let ()
             ((coord-sys 'set-coordinate-prototype!) #,(->proto #'names))
             
             (apply
              values
              (append
               (map cadr (ultra-flatten (coord-sys 'coordinate-function-specs)))
               (map cadr (ultra-flatten (coord-sys 'coordinate-basis-vector-field-specs)))
               (map cadr (ultra-flatten (coord-sys 'coordinate-basis-1form-field-specs))))))))))]))
#;
(define-syntax define-coordinates
  (er-macro-transformer
   (lambda (e r c)
     (define (quote-symbol-names symbs)
       (cond
	((and (pair? symbs)
	      (memq (car symbs) '(up down)))
	 `(,(car symbs) ,@(map quote-symbol-names (cdr symbs))))
	((symbol? symbs)
	 `(quote ,symbs))
	(else 
	 (error "bad coordinate prototype" symbs))))
     (define (get-symbol-names symbs)
       (cond
	((and (pair? symbs)
	      (memq (car symbs) '(up down)))
	 (apply append
		(map get-symbol-names
		     (cdr symbs))))
	((symbol? symbs) (list symbs))
	(else (error "bad coordinate prototype" symbs))))
     (let ((coord-proto-symbs (cadr e))
	   (coord-proto (quote-symbol-names (cadr e)))
	   (coord-sys-expr (caddr e))
	   (coord-sys (generate-uninterned-symbol 'coord-sys))
	   (chart-functions (generate-uninterned-symbol 'chart-fn))
	   (proto (generate-uninterned-symbol 'coord-proto)))
       (let* ((coord-symbs (get-symbol-names coord-proto-symbs))
	      (coord-vector-syms 
	       (map (lambda (sym) (symbol 'd/d sym)) coord-symbs))
	      (coord-one-form-syms 
	       (map (lambda (sym) (symbol 'd sym)) coord-symbs)))
	 `(,(r 'begin)
	    (,(r 'define-values)
	      ,(append coord-symbs coord-vector-syms coord-one-form-syms)
	      (,(r 'let) ((,coord-sys ,coord-sys-expr)
			  (,proto ,coord-proto))
		((,coord-sys 'set-coordinate-prototype!) ,proto)
		(,(r 'let) ((,chart-functions 
		       (,(r 'append)
			(,(r 'map) ,(r 'cadr) 
			     (,(r 'ultra-flatten)
			      (,coord-sys 'coordinate-function-specs)))
			(,(r 'map) ,(r 'cadr) 
			     (,(r 'ultra-flatten)
			      (,coord-sys 'coordinate-basis-vector-field-specs)))
			(,(r 'map) ,(r 'cadr) 
			     (,(r 'ultra-flatten)
			      (,coord-sys 'coordinate-basis-1form-field-specs))))))
		  (,(r 'apply) ,(r 'values) ,chart-functions))))))))))
       
#|
(pec 
 (let ()
   (define-coordinates (up x y) R2-rect)
   (x ((R2-rect '->point) (up 'a 'b)))))
#| Result:
a
|#

(pec 
 (let ()
   (define-coordinates (up x y) R2-rect)
   ((d/dx x) ((R2-rect '->point) (up 'a 'b)))))
#| Result:
1
|#

(pec 
 (let ()
   (define-coordinates (up p q) R2-rect)
   (R2-rect 'coordinate-function-specs)))
#| Result:
(up (p (??? x)) (q (??? x)))
|#
|#

(define-syntax-rule (using-coordinates names coord-sys body ...)
  (let ()
    (define-coordinates names coord-sys)
    body ...))
#;
(define-syntax using-coordinates
  (er-macro-transformer
   (lambda (x r c)
     (let ((coord-proto (cadr x))
	   (coord-sys-expr (caddr x))
	   (body (cdddr x)))
       `(,(r 'let) ()
	  (,(r 'define-coordinates) ,coord-proto ,coord-sys-expr)
	  ,@body)))))

#|
(using-coordinates (up x y) R2-rect
  (pec (x ((R2-rect '->point) (up 'a 'b)))))
#| Result:
a
|#

(using-coordinates (up x y) R2-rect
  (pec ((dx d/dx) ((R2-polar '->point) (up 'a 'b)))))
#| Result:
1
|#
|#

#|
;; Global definitions and shadowing:
(define-coordinates (up x y) R2-rect)

(pe (x ((R2-polar '->point) (up 'r 'theta))))
(* r (cos theta))

(using-coordinates (up x y) R2-polar ;; funky
  (pe (x ((R2-rect '->point) (up 'a 'b)))))
(sqrt (+ (expt a 2) (expt b 2)))

(pe (x ((R2-rect '->point) (up 'a 'b))))
a
|#


