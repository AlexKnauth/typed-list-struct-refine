#lang type-expander/base #:with-refinements

(require "../../list-struct.rkt"
         "dim.rkt")

(list-struct unit
  ([scalar : Integer] [dim : Dim])
  #:type-name Unit)

(: u1 (Refine [r : Unit]
              (sv= Dim
                   (unit-dim r)
                   D1)))
(define u1
  (unit 1 d1))


(: u* (-> ([a : Unit] [b : Unit])
          (Refine [r : Unit]
                  (sv= Dim
                       (unit-dim r)
                       (D* (unit-dim a) (unit-dim b)))
                  #;-->
                  #;(Let ([x (unit-dim r)])
                      (and
                       (: x Dim)
                       (= (dim-M x) (+ (dim-M a) (dim-M b)))
                       ...
                       ))
                  )))
(define (u* a b)
  (unit (* (unit-scalar a)
           (unit-scalar b))
        (d* (unit-dim a)
            (unit-dim b))))

(ann (u* (unit 1 (dim 1 2 3))
         (unit 1 (dim 4 5 6)))
     (Refine [r : Unit]
             (sv= Dim
                  (unit-dim r)
                  (dim 5 7 9))))

