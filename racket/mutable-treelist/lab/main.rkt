#lang typed/racket/base

(require typed/racket/unsafe)

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
