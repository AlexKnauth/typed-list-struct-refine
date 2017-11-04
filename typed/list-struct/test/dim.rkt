#lang type-expander/base #:with-refinements

(require syntax/parse/define
         "../../list-struct.rkt"
         (for-syntax racket/base))
(module+ test
  (require typed/rackunit))

;; ------------------------------------------------------------------------

(list-struct dim
  ([M : Integer]
   [L : Integer]
   [T : Integer])
  #:type-name Dim)

(: d1 (Refine [d1 : Dim]
              (and (= (dim-M d1) 0)
                   (= (dim-L d1) 0)
                   (= (dim-T d1) 0))))
(define d1 (dim 0 0 0))

(: d+ (-> ([a : () Dim]
           [b : (a) (Refine
                     [b : Dim]
                     (and (= (dim-M a) (dim-M b))
                          (= (dim-L a) (dim-L b))
                          (= (dim-T a) (dim-T b))))])
          (Refine
           [r : Dim]
           (and (= (dim-M a) (dim-M r))
                (= (dim-L a) (dim-L r))
                (= (dim-T a) (dim-T r))))))
(define (d+ a b) a)

(: d* (-> ([a : () Dim]
           [b : () Dim])
          (Refine
           [r : Dim]
           (and (= (dim-M r) (+ (dim-M a) (dim-M b)))
                (= (dim-L r) (+ (dim-L a) (dim-L b)))
                (= (dim-T r) (+ (dim-T a) (dim-T b)))))))
(define (d* a b)
  (define r
    (dim (+ (dim-M a) (dim-M b))
         (+ (dim-L a) (dim-L b))
         (+ (dim-T a) (dim-T b))))
  r)

(: d/ (-> ([a : () Dim]
           [b : () Dim])
          (Refine
           [r : Dim]
           (and (= (dim-M r) (- (dim-M a) (dim-M b)))
                (= (dim-L r) (- (dim-L a) (dim-L b)))
                (= (dim-T r) (- (dim-T a) (dim-T b)))))))
(define (d/ a b)
  (define r
    (dim (- (dim-M a) (dim-M b))
         (- (dim-L a) (dim-L b))
         (- (dim-T a) (dim-T b))))
  r)

(: dsqr (-> ([a : () Dim])
            (Refine
             [r : Dim]
             (and (= (dim-M r) (* 2 (dim-M a)))
                  (= (dim-L r) (* 2 (dim-L a)))
                  (= (dim-T r) (* 2 (dim-T a)))))))
(define (dsqr a)
  (d* a a))

;; ------------------------------------------------------------------------

(define-syntax-parser define-dim
  #:literals [: dim]
  [(_ name : (dim-id:dim M L T) expr)
   (syntax-property
    #'(begin
        (: name (Refine [name : Dim]
                        (and (= (dim-M name) M)
                             (= (dim-L name) L)
                             (= (dim-T name) T))))
        (define name expr))
    'disappeared-use
    (list (syntax-local-introduce #'dim-id)))])

;; Base Dimensions
(define-dim mass-dim : (dim 1 0 0) (dim 1 0 0))
(define-dim length-dim : (dim 0 1 0) (dim 0 1 0))
(define-dim time-dim : (dim 0 0 1) (dim 0 0 1))

;; Derived Dimensions

(define-dim area-dim : (dim 0 2 0) (d* length-dim length-dim))
(define-dim volume-dim : (dim 0 3 0) (d* area-dim length-dim))

(define-dim mass-density-dim : (dim 1 -3 0) (d/ mass-dim volume-dim))

(define-dim velocity-dim : (dim 0 1 -1) (d/ length-dim time-dim))
(define-dim acceleration-dim : (dim 0 1 -2) (d/ velocity-dim time-dim))

(define-dim momentum-dim : (dim 1 1 -1) (d* mass-dim velocity-dim))
(define-dim force-dim : (dim 1 1 -2) (d* mass-dim acceleration-dim))

(define-dim work-dim : (dim 1 2 -2) (d* force-dim length-dim))
(define-dim energy-dim : (dim 1 2 -2) work-dim)

(define-dim power-dim : (dim 1 2 -3) (d/ energy-dim time-dim))

(define-dim pressure-dim : (dim 1 -1 -2) (d/ force-dim area-dim))

;; ------------------------------------------------------------------------

(: kinetic-energy/dim (-> ([m : () (Refine [m : Dim]
                                           (and (= (dim-M m) 1)
                                                (= (dim-L m) 0)
                                                (= (dim-T m) 0)))]
                           [v : () (Refine [v : Dim]
                                           (and (= (dim-M v) 0)
                                                (= (dim-L v) 1)
                                                (= (dim-T v) -1)))])
                          (Refine [r : Dim]
                                  (and (= (dim-M r) 1)
                                       (= (dim-L r) 2)
                                       (= (dim-T r) -2)))))
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
  )

