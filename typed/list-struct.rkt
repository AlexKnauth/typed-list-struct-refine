#lang type-expander/base #:with-refinements

(provide list-struct
         sp=
         sv=)

(require syntax/parse/define
         (only-in typed/racket/base [: tr:])
         (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/id-table
                     syntax/stx
                     syntax/transformer
                     type-expander/expander
                     "list-struct/util/with-type-expander.rkt"))
(module+ test
  (require typed/rackunit))

;; ------------------------------------------------------------------------

(begin-for-syntax
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
    (var-like-transformer+type-expander
     internal-accessor-id
     (pef->tef path-elem-fn)))
  )

;; ------------------------------------------------------------------------

(begin-for-syntax
  (define (stx-e stx)
    (if (syntax? stx) (syntax-e stx) stx))
  ;; A SPEquality is one of:
  ;;  - Id
  ;;  - (sp-parts [Listof [Pair PathElemIds SPEquality]])
  (struct sp-parts [accessors/speqs] #:prefab)

  ;; A PathElemIds is a [StxListof Id]
  ;; app-path-elem-ids : PathElemIds Stx -> Stx
  (define (app-path-elem-ids ids base)
    (foldr (λ (id base) #`(#,id #,base))
           base
           (stx->list ids)))

  ;; sptype-table : [FreeIdTableof SPEquality]
  (define sptype-table
    (make-free-id-table
     (list (cons #'Integer #'=))))

  (define-syntax-class sptype
    #:attributes [speq]
    #:literals [List]
    [pattern x:id
      #:attr speq (free-id-table-ref sptype-table #'x #f)
      #:when (attribute speq)]
    [pattern (List t:sptype ...)
      #:attr speq
      (sp-parts
       (for/list ([t-speq (in-list (attribute t.speq))]
                  [i (in-naturals)])
         (cons (cons #'car (make-list i #'cdr))
               t-speq)))])

  (define (sp=/stx speq a b)
    (cond
      [(identifier? speq) #`(#,speq #,a #,b)]
      [else
       #`(and
          #,@(for/list ([acc/speq
                         (in-list (stx->list
                                   (sp-parts-accessors/speqs
                                    (stx-e speq))))])
               (match-define (cons acc speq) (stx-e acc/speq))
               (sp=/stx speq
                        (app-path-elem-ids acc a)
                        (app-path-elem-ids acc b))))])))

(define-syntax sp=
  (procedure+type-expander
   (syntax-parser
     [(_ type:sptype a b) (sp=/stx (attribute type.speq) #'a #'b)])
   (syntax-parser
     [(_ type:sptype a b) (sp=/stx (attribute type.speq) #'a #'b)])))

;; ------------------------------------------------------------------------

(begin-for-syntax
  ;; an equation saying `a` and `b` are equal
  (define (sv=/stx speq a b)
    (syntax-parse (list (expand-type a) (expand-type b))
      #:literals [Refine]
      #;[[(Refine [x :colon τ_x] prop_x) (Refine [y :colon τ_y] prop_y)]
       #'(......
          ???
          ......)]
      [[a:expr (Refine ~! [x:id :colon τ:expr] prop:expr)]
       #'(Let ([x a])
           (and (tr: x τ)
                prop))]
      [else
       (sp=/stx speq a b)])))

(define-syntax sv=
  (procedure+type-expander
   (syntax-parser
     [(_ type:sptype a b) (sv=/stx (attribute type.speq) #'a #'b)])
   (syntax-parser
     [(_ type:sptype a b) (sv=/stx (attribute type.speq) #'a #'b)])))

;; ------------------------------------------------------------------------

;; (list-struct name (field-spec ...) #:type-name Name)
;;
;;   field-spec  = [field : sptype]
;;                 ;; TODO: add [field : type #:speq-ignored] ?
;;
;;       sptype  = Integer
;;               | (List sptype ...)
;;               | x                 ; x is a type for another list-struct

;; Defines:
;;  - Name
;;  - name
;;  - name-field ...
;; Name is a type for instances.
;; name is a constructor and a type-expander. The constructor constructs
;;   instances from the field values, just like a struct constructor.
;;   The type-expander takes symbolic-objects (see grammar for Refine) and
;;   expands to a type that refines the fields to be equal to the
;;   symbolic-objects.
;; name-field is an accessor and a type-expander. The accessor gets that
;;   field from the instance, just like a struct accessor. The
;;   type-expander takes a symbolic-path (see grammar for Refine) for an
;;   instance, and expands to a symbolic-path for the field within the
;;   instance.

(begin-for-syntax
  ;; accessor-names : Id [Listof Id] -> [Listof Id]
  (define (accessor-names name fields)
    (for/list ([field (in-list fields)])
      (format-id name "~a-~a" name field)))

  (define-syntax-class field-spec
    #:attributes [field type [speq-field 1] [speq-type.speq 1]]
    #:literals [:]
    [pattern [field : type:sptype]
      #:with [speq-field ...] #'[field]
      #:with [speq-type.speq ...] #'[type.speq]]))

(define-syntax-parser list-struct
  #:literals [:]
  [(_ name (f:field-spec ...)
      #:type-name Name)
   #:with Name-Desc (generate-temporary #'name)
   #:with [speq-field ...] #'[f.speq-field ... ...]
   #:with [speq-type.speq ...] #'[f.speq-type.speq ... ...]
   ;; ------------------------------------------------------
   #:with [name-field ...]
   (accessor-names #'name (attribute f.field))
   #:with [speq-name-field ...]
   (accessor-names #'name (attribute speq-field))
   #:with [name* field* ...]
   (generate-temporaries #'[name f.field ...])
   #:with [name-field* ...]
   (generate-temporaries #'[name-field ...])
   #:with accessor-arg #'v
   #:with result-id #'r
   #:do [(define lst-indexes
           (map add1 (range (length (attribute f.field)))))
         (define lst-path-elems
           (map pe:list-ref lst-indexes))
         (define name-speq
           (sp-parts
            #'([[speq-name-field] . speq-type.speq] ...)))
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
       ;; types
       (struct Name-Desc [] #:transparent)
       (define-type Name
         (List Name-Desc f.type ...))
       (begin-for-syntax
         (free-id-table-set! sptype-table #'Name #'#,name-speq))
       ;; type expanders
       (define-syntax name
         (var-like-transformer+type-expander
          #'name*
          (syntax-parser
            [(_ (~var field* expr) ...)
             (syntax/loc this-syntax
               (Refine [result-id : Name]
                       (and (sp= f.type (name-field result-id) field*) ...)))])))
       (define-syntax name-field
         (accessor+path-elem #'name-field* (pe:list-ref 'lst-i)))
       ...
       ;; functions
       (: name* (-> ([field* : () f.type]
                     ...)
                    (name field* ...)))
       (define (name* field* ...)
         (define result-id (list (Name-Desc) field* ...))
         (assert (sp= f.type lst-path-result-part field*)) ...
         result-id)
       (: name-field* (-> ([accessor-arg : () Name])
                          (Refine
                           [r : f.type]
                           (sp= f.type r (name-field accessor-arg)))))
       ...
       (define (name-field* accessor-arg)
         lst-path-accessor)
       ...)])

;; ------------------------------------------------------------------------

(module+ test
  (check-true (sp= (List Integer Integer Integer)
                   (list 1 2 3)
                   (list 1 2 3)))
  (check-false (sp= (List Integer Integer Integer)
                    (list 1 2 3)
                    (list 1 3 3))))

