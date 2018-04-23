#lang racket/base

(provide (struct-out procedure+type-expander)
         var-like-transformer+type-expander)

(require racket/base
         syntax/transformer
         type-expander/expander
         (for-template type-expander/base))

(struct procedure+type-expander [procedure type-expander]
  #:property prop:procedure (struct-field-index procedure)
  #:property prop:type-expander (struct-field-index type-expander))

;; var-like-transformer+type-expander :
;; Id TypeExpanderFn -> Procedure+TypeExpander
(define (var-like-transformer+type-expander internal-id tef)
  (procedure+type-expander
   (set!-transformer-procedure
    (make-variable-like-transformer internal-id))
   tef))

