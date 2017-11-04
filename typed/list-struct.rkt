#lang type-expander/base #:with-refinements

(provide list-struct)

(require syntax/parse/define
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/id-table
                     syntax/transformer
                     type-expander/expander))

;; ------------------------------------------------------------------------

(begin-for-syntax
  (struct procedure+type-expander [procedure type-expander]
    #:property prop:procedure (struct-field-index procedure)
    #:property prop:type-expander (struct-field-index type-expander))

  ;; A PathElemFn is a [Stx Stx -> Stx]
  ;; The first argument represents the source location for the path.
  ;; The second argument replesents the symbolic path it starts from.

  ;; pe:car, pe:cdr : PathElemFn
  (define (pe:car src sp) (quasisyntax/loc src (car #,sp)))
  (define (pe:cdr src sp) (quasisyntax/loc src (cdr #,sp)))

  ;; pe:list-ref : Natural -> PathElemFn
  (define ((pe:list-ref i) src sp)
    (cond
      [(zero? i) (pe:car src sp)]
      [(positive? i) ((pe:list-ref (sub1 i)) src (pe:cdr src sp))]
      [else (error 'pe:list-ref "bad index")]))

  ;; A TypeExpanderFn is a [Stx -> Stx]

  ;; pef->tef : PathElemFn -> TypeExpanderFn
  (define (pef->tef pe)
    (λ (stx)
      (syntax-parse stx
        [(_ sp) (pe stx #'sp)])))

  ;; accessor+path-elem : Id PathElemFn -> Procedure+TypeExpander
  (define (accessor+path-elem internal-accessor-id path-elem-fn)
    (procedure+type-expander
     (set!-transformer-procedure
      (make-variable-like-transformer internal-accessor-id))
     (pef->tef path-elem-fn)))
  )

;; ------------------------------------------------------------------------

(define-syntax-parser list-struct
  #:literals [:]
  [(_ name ([field:id : type:expr] ...)
      #:type-name Name)
   #:with Name-Desc (generate-temporary #'name)
   #:with [name-field ...]
   (for/list ([field (in-list (syntax->list #'[field ...]))])
     (format-id #'name "~a-~a" #'name field))
   #:with [name-field* ...] (generate-temporaries #'[name-field ...])
   #:with accessor-arg #'v
   #:with result-id #'r
   #:do [(define lst-indexes
           (map add1 (range (length (syntax->list #'[field ...])))))
         (define lst-path-elems
           (map pe:list-ref lst-indexes))
         (define lst-path-accessors
           (for/list ([pe (in-list lst-path-elems)])
             (pe #'here #'accessor-arg)))
         (define lst-path-result-parts
           (for/list ([pe (in-list lst-path-elems)])
             (pe #'here #'result-id)))]
   #:with [lst-i ...] lst-indexes
   #:with [lst-path-accessor ...] lst-path-accessors
   #:with [lst-path-result-part ...] lst-path-result-parts
   #`(begin
       (struct Name-Desc [] #:transparent)
       (define-type Name
         (List Name-Desc type ...))
       (: name (-> ([field : () type]
                    ...)
                   (Refine
                    [result-id : Name]
                    (and (= lst-path-result-part field)
                         ...))))
       (define (name field ...)
         (define result-id (list (Name-Desc) field ...))
         (assert (= lst-path-result-part field)) ...
         result-id)
       (: name-field* (-> ([accessor-arg : () Name])
                          (Refine
                           [r : type]
                           (= r lst-path-accessor))))
       ...
       (define (name-field* accessor-arg)
         lst-path-accessor)
       ...
       (define-syntax name-field
         (accessor+path-elem #'name-field* (pe:list-ref 'lst-i)))
       ...)])

;; ------------------------------------------------------------------------

