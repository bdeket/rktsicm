#lang s-exp "../generic.rkt"

(provide (all-defined-out))

(require (only-in "../rkt/glue.rkt" if)
         "../general/assert.rkt"
         "../general/eq-properties.rkt"
         )

;;;; System code for making frames

;;; Every frame has a name, and a frame that it is built on (which may be #f).
;;; Every frame owns coordinates that it may coerce to an absolute event or that
;;; it may export as its representation of an absolute event.

(define ((frame-maker c->e e->c) name ancestor-frame . params)

   (define (coordinates->event coords)
     (assert (eq? (frame-owner coords) this-frame))
     (let ((event
	    ((apply c->e ancestor-frame this-frame params) coords)))
       (assert (event? event))
       event))

   (define (event->coordinates event)
     (assert (event? event))
     (let ((coords 
	    ((apply e->c ancestor-frame this-frame params) event)))
       (assert (eq? (frame-owner coords) this-frame))
       coords))

   (define (this-frame m)
     (case m
       ((coords->event) coordinates->event)
       ((event->coords) event->coordinates)
       ((name) name)
       ((ancestor-frame) ancestor-frame)
       ((params) params)
       ((manifold) #f)			;Kludge.  See frame? in manifold.scm
       (else (error "Unknown message: " name m))))
   this-frame)

(define (event->coords frame) (frame 'event->coords))
(define (coords->event frame) (frame 'coords->event))
(define (ancestor-frame frame) (frame 'ancestor-frame))

(define (make-event e)
  (eq-put! e 'event #t)
  e)

(define (event? e)
  (eq-get e 'event))

(define (frame-owner coords)
  (eq-get coords 'owner))

(define (claim! coords owner)
  (let ((other (frame-owner coords)))
    (if other
	(if (not (eq? other owner))
	    (error "Someone else owns these coords" coords owner))
	(eq-put! coords 'owner owner))
    coords))

(define (frame-params frame) (frame 'params))
(define (frame-name frame) (frame 'name))

