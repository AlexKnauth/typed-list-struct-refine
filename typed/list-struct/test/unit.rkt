#lang type-expander/base #:with-refinements

(require "../../list-struct.rkt"
         "dim.rkt")

(list-struct unit
  ([scalar : Integer] [dim : Dim])
  #:type-name Unit)

