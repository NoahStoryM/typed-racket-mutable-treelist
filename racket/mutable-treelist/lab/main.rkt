#lang typed/racket/base

(require (for-syntax racket/base syntax/parse)
         typed/racket/unsafe)

;; TODO (Mutable-TreeListof t) → (Mutable-TreeListof t t)
(struct (w r) Mutable-TreeListof ([_ : (Parameter w r)]))
#;(struct (a ...) _ ([_ : (Parameter a ...)]) #:type-name Mutable-TreeListof) ; not work well
(define-type (MTreeListof w r) (Mutable-TreeListof w r)) ; avoid printing #(struct:Mutable-TreeListof ...)
(provide (rename-out [MTreeListof Mutable-TreeListof]))

(define-type Mutable-TreeListTop (MTreeListof Nothing Any))
(define-type Mutable-TreeListBot (MTreeListof Any Nothing))
(provide Mutable-TreeListTop Mutable-TreeListBot)

#;(define-type (TreeListof t) (∪ (Immutable-TreeListof t) (MListof-TreeListof t t)))
#;(provide TreeListof)

(unsafe-require/typed/provide racket/mutable-treelist
  [mutable-treelist? (pred Mutable-TreeListTop)]
  [mutable-treelist  (∀ (t) (→ t * (MTreeListof t t)))]
  [make-mutable-treelist (∀ (t) (case→ (→ Integer (MTreeListof (Option t) (Option t)))
                                       (→ Integer t (MTreeListof t t))))]

  #;[treelist-copy (∀ (t) (→ (Immutable-TreeListof t) (MTreeListof t t)))]
  #;[mutable-treelist-snapshot (∀ (w r) (→ (MTreeListof w r) (Immutable-TreeListof r)))]

  [mutable-treelist-empty? (→ Mutable-TreeListTop Boolean)]
  [mutable-treelist-length (→ Mutable-TreeListTop Index)]

  [mutable-treelist-first (∀ (r) (→ (MTreeListof Nothing r) r))]
  [mutable-treelist-last  (∀ (r) (→ (MTreeListof Nothing r) r))]
  [mutable-treelist-ref   (∀ (r) (→ (MTreeListof Nothing r) Integer r))]

  [mutable-treelist-cons!   (∀ (w) (→ (MTreeListof w Any) w Void))]
  [mutable-treelist-add!    (∀ (w) (→ (MTreeListof w Any) w Void))]
  [mutable-treelist-set!    (∀ (w) (→ (MTreeListof w Any) Integer w Void))]
  [mutable-treelist-insert! (∀ (w) (→ (MTreeListof w Any) Integer w Void))]

  [mutable-treelist-append! (∀ (t) (→ (MTreeListof t Any) (∪ (MTreeListof Nothing t) #;(Immutable-TreeListof t)) Void))]

  [mutable-treelist-reverse!    (→ Mutable-TreeListTop Void)]
  [mutable-treelist-delete!     (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-take!       (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-drop!       (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-take-right! (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-drop-right! (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-sublist!    (→ Mutable-TreeListTop Integer Integer Void)]

  [mutable-treelist->vector (∀ (r) (→ (MTreeListof Nothing r) (Mutable-Vectorof r)))]
  [mutable-treelist->list   (∀ (r) (→ (MTreeListof Nothing r) (Listof           r)))]
  [vector->mutable-treelist (∀ (t) (→ (Vectorof t) (MTreeListof t t)))]
  [list->mutable-treelist   (∀ (t) (→ (Listof   t) (MTreeListof t t)))]

  [mutable-treelist-map! (∀ (w r) (→ (MTreeListof w r) (→ r w) Void))]

  [mutable-treelist-for-each (∀ (r) (→ (MTreeListof Nothing r) (→ r Any) Void))]

  [mutable-treelist-member? (∀ (r t) (case→ (→ Mutable-TreeListTop Any Boolean)
                                            (→ (MTreeListof Nothing r) t (→ t r Any) Boolean)))]

  [mutable-treelist-find (∀ (r) (→ (MTreeListof Nothing r) (→ r Any) r))]

  [mutable-treelist-sort! (∀ (a b) (case→ (→ (MTreeListof a a) (→ a a Boolean)
                                             [#:key (Option (→ a a))]
                                             [#:cache-keys? Boolean]
                                             Void)
                                          (→ (MTreeListof a a) (→ b b Boolean)
                                             #:key (→ a b)
                                             [#:cache-keys? Boolean]
                                             Void)))]

  [in-mutable-treelist (∀ (r) (→ (MTreeListof Nothing r) (Sequenceof r)))])

(define-syntaxes (for/mutable-treelist for*/mutable-treelist)
  (let ()
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define (make-for/mutable-treelist derived-stx)
      (define (parser stx)
        (syntax-parse stx
          #:datum-literals (:)
          [(_ : t1 (clause ...) : t2 break:break-clause ... body* ... body)
           (quasisyntax/loc stx
             (let ([mtl : t1 (ann (mutable-treelist) t2)])
               (#,derived-stx (clause ...) break ... body* ...
                (mutable-treelist-add! mtl body))
               mtl))]
          [(~or* (name : t0 (clause ...) break:break-clause ... body ...+)
                 (name (clause ...) : t0 break:break-clause ... body ...+)
                 (name (clause ...) break:break-clause ... body ...+))
           #:with t (if (attribute t0) #'t0 #'(MTreeListof Any Any))
           (parser (syntax/loc stx (name : t (clause ...) : t break ... body ...)))]))
      parser)
    (values (make-for/mutable-treelist #'for)
            (make-for/mutable-treelist #'for*))))
(provide for/mutable-treelist for*/mutable-treelist)
