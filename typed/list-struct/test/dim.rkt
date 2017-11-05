#lang type-expander/base #:with-refinements

(require syntax/parse/define
         "../../list-struct.rkt"
         (for-syntax racket/base))
(module+ test
  (require typed/rackunit))

;; ------------------------------------------------------------------------

(provide Dim
         dim
         dim-M
         dim-L
         dim-T
         d1
         d+
         d*
         d/
         dsqr)

(list-struct dim
  ([M : Integer]
   [L : Integer]
   [T : Integer])
  #:type-name Dim)

(: d1 (dim 0 0 0))
(define d1 (dim 0 0 0))

(: d+ (-> ([a : () Dim]
           [b : (a) (dim (dim-M a)
                         (dim-L a)
                         (dim-T a))])
          (dim (dim-M a)
               (dim-L a)
               (dim-T a))))
(define (d+ a b) a)

(: d* (-> ([a : () Dim]
           [b : () Dim])
          (dim (+ (dim-M a) (dim-M b))
               (+ (dim-L a) (dim-L b))
               (+ (dim-T a) (dim-T b)))))
(define (d* a b)
  (define r
    (dim (+ (dim-M a) (dim-M b))
         (+ (dim-L a) (dim-L b))
         (+ (dim-T a) (dim-T b))))
  r)

(: d/ (-> ([a : () Dim]
           [b : () Dim])
          (dim (- (dim-M a) (dim-M b))
               (- (dim-L a) (dim-L b))
               (- (dim-T a) (dim-T b)))))
(define (d/ a b)
  (define r
    (dim (- (dim-M a) (dim-M b))
         (- (dim-L a) (dim-L b))
         (- (dim-T a) (dim-T b))))
  r)

(: dsqr (-> ([a : () Dim])
            (dim (* 2 (dim-M a))
                 (* 2 (dim-L a))
                 (* 2 (dim-T a)))))
(define (dsqr a)
  (d* a a))

;; ------------------------------------------------------------------------

(provide mass-dim
         length-dim
         time-dim
         area-dim
         volume-dim
         mass-density-dim
         velocity-dim
         acceleration-dim
         momentum-dim
         force-dim
         work-dim
         energy-dim
         power-dim
         pressure-dim)

;; Base Dimensions
(define mass-dim : (dim 1 0 0) (dim 1 0 0))
(define length-dim : (dim 0 1 0) (dim 0 1 0))
(define time-dim : (dim 0 0 1) (dim 0 0 1))

;; Derived Dimensions

(define area-dim : (dim 0 2 0) (d* length-dim length-dim))
(define volume-dim : (dim 0 3 0) (d* area-dim length-dim))

(define mass-density-dim : (dim 1 -3 0) (d/ mass-dim volume-dim))

(define velocity-dim : (dim 0 1 -1) (d/ length-dim time-dim))
(define acceleration-dim : (dim 0 1 -2) (d/ velocity-dim time-dim))

(define momentum-dim : (dim 1 1 -1) (d* mass-dim velocity-dim))
(define force-dim : (dim 1 1 -2) (d* mass-dim acceleration-dim))

(define work-dim : (dim 1 2 -2) (d* force-dim length-dim))
(define energy-dim : (dim 1 2 -2) work-dim)

(define power-dim : (dim 1 2 -3) (d/ energy-dim time-dim))

(define pressure-dim : (dim 1 -1 -2) (d/ force-dim area-dim))

;; ------------------------------------------------------------------------

(provide kinetic-energy/dim)

(: kinetic-energy/dim (-> ([m : () (dim 1 0 0)]
                           [v : () (dim 0 1 -1)])
                          (dim 1 2 -2)))
(define (kinetic-energy/dim m v)
  (define r
    (d* m (dsqr v)))
  r)

;; ------------------------------------------------------------------------

(module+ test
  (check-equal? (d+ length-dim length-dim) length-dim)
  (check-equal? (d* mass-dim (dsqr velocity-dim)) energy-dim)
  (check-equal? (d* pressure-dim area-dim) force-dim)
  (check-equal? (d/ force-dim mass-dim) acceleration-dim)
  (check-equal? (d* acceleration-dim time-dim) velocity-dim)
  (check-equal? (d* velocity-dim time-dim) length-dim)
  (check-equal? (d* mass-density-dim volume-dim) mass-dim)
  (check-equal? (kinetic-energy/dim mass-dim velocity-dim) energy-dim)
  (check-equal? (kinetic-energy/dim (d* mass-density-dim volume-dim)
                                    (d* acceleration-dim time-dim))
                energy-dim)
  (check-equal? (d+ (kinetic-energy/dim mass-dim (d/ length-dim time-dim))
                    (kinetic-energy/dim (d* mass-density-dim volume-dim)
                                        (d* acceleration-dim time-dim)))
                energy-dim)
  (check-true (sp= Dim length-dim length-dim))
  (check-false (sp= Dim length-dim mass-dim))
  )

