#lang typed/racket/base #:with-refinements

(provide list-struct
         ::)

(require syntax/parse/define
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/id-table))

;; ------------------------------------------------------------------------

(begin-for-syntax
  ;; A PathElemFn is a [Stx Stx -> Stx]
  ;; The first argument represents the source location for the path.
  ;; The second argument replesents the symbolic path it starts from.

  ;; pe:car, pe:cdr, pe:vector-length : PathElemFn
  (define (pe:car src sp) (quasisyntax/loc src (car #,sp)))
  (define (pe:cdr src sp) (quasisyntax/loc src (cdr #,sp)))
  (define (pe:vector-length src sp) (quasisyntax/loc src (vector-length #,sp)))

  ;; pe:list-ref : Natural -> PathElemFn
  (define ((pe:list-ref i) src sp)
    (cond
      [(zero? i) (pe:car src sp)]
      [(positive? i) ((pe:list-ref (sub1 i)) src (pe:cdr src sp))]
      [else (error 'pe:list-ref "bad index")]))

  ;; path-elems : [FreeIdTable PathElemFn]
  (define path-elems
    (make-free-id-table
     (list (cons #'car pe:car)
           (cons #'cdr pe:cdr)
           (cons #'vector-length pe:vector-length)))))

;; ------------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class type*
    #:attributes [norm]
    #:literals [-> Refine :]
    [pattern (Refine ~! [x:id : type:type*] prop:proposition*)
      #:with norm (syntax/loc this-syntax
                    (Refine [x : type.norm] prop.norm))]
    [pattern (-> ([~! arg:id : (arg-dep:id ...) arg-type:type*]
                  ...)
                 result-type:type*)
      #:with norm (syntax/loc this-syntax
                    (-> ([arg : (arg-dep ...) arg-type.norm]
                         ...)
                        result-type.norm))]
    [pattern x:id #:with norm #'x]
    [pattern ((~and c:id (~not (~or Refine ->))) t:type* ...)
      #:with norm (syntax/loc this-syntax (c t.norm ...))])
  (define-syntax-class proposition*
    #:attributes [norm]
    #:literals [Top Bot : ! and or when unless if]
    [pattern Top #:with norm this-syntax]
    [pattern Bot #:with norm this-syntax]
    [pattern (: o:sym-obj* type:type*)
      #:with norm #'(: o.norm type.norm)]
    [pattern (! o:sym-obj* type:type*)
      #:with norm #'(! o.norm type.norm)]
    [pattern (and p:proposition* ...)
      #:with norm #'(and p.norm ...)]
    [pattern (or p:proposition* ...)
      #:with norm #'(or p.norm ...)]
    [pattern (lc:linear-comp a:sym-obj* b:sym-obj*)
      #:with norm #'(lc a.norm b.norm)])
  (define-syntax-class linear-comp
    #:literals [< <= = >= >]
    [pattern (~or < <= = >= >)])
  (define-syntax-class sym-obj*
    #:attributes [norm]
    #:literals [+ -]
    [pattern i:exact-integer #:with norm #'i]
    [pattern lt:linear-term* #:with norm #'lt.norm]
    [pattern (+ lt:linear-term* ...)
      #:with norm #'(+ lt.norm ...)]
    [pattern (- lt:linear-term* ...)
      #:with norm #'(- lt.norm ...)])
  (define-syntax-class linear-term*
    #:attributes [norm]
    #:literals [*]
    [pattern sp:symbolic-path* #:with norm #'sp.norm]
    [pattern i:exact-integer #:with norm #'i]
    [pattern (* i:exact-integer sp:symbolic-path*)
      #:with norm #'(* i sp.norm)])
  (define-syntax-class symbolic-path*
    #:attributes [norm]
    [pattern x:id #:with norm #'x]
    [pattern (pe:path-elem* sp:symbolic-path*)
      #:with norm ((attribute pe.f) this-syntax #'sp.norm)])
  (define-syntax-class path-elem*
    #:attributes [f]
    [pattern x:id
      #:attr f (free-id-table-ref path-elems #'x #f)
      #:fail-unless (attribute f) "invalid path element"]))

;; ------------------------------------------------------------------------

(define-syntax-parser ::
  #:literals [: ->]
  [(_ name:id : type:type*)
   #'(: name : type.norm)])

(define-syntax-parser list-struct
  #:literals [:]
  [(_ name ([field:id : type:expr] ...)
      #:type-name Name)
   #:with Name-Desc (generate-temporary #'name)
   #:with [name-field ...]
   (for/list ([field (in-list (syntax->list #'[field ...]))])
     (format-id #'name "~a-~a" #'name field))
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
       (:: name : (-> ([field : () type]
                       ...)
                      (Refine
                       [result-id : Name]
                       (and (= lst-path-result-part field)
                            ...))))
       (define (name field ...)
         (define result-id (list (Name-Desc) field ...))
         (assert (= lst-path-result-part field)) ...
         result-id)
       (:: name-field : (-> ([accessor-arg : () Name])
                            (Refine
                            [r : type]
                            (= r lst-path-accessor))))
       ...
       (define (name-field accessor-arg)
         lst-path-accessor)
       ...
       (begin-for-syntax
         (free-id-table-set! path-elems #'name-field
                             (pe:list-ref 'lst-i))
         ...))])

;; ------------------------------------------------------------------------

