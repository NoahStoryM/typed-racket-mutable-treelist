#lang typed/racket/base

(require (for-syntax racket/base syntax/parse)
         racket/treelist
         typed/racket/unsafe
         "private/types.rkt")

(provide (all-from-out "private/types.rkt"))

(unsafe-require/typed/provide racket/mutable-treelist
  [mutable-treelist? (pred Mutable-TreeListTop)]
  [mutable-treelist  (∀ (t) (→ t * (Mutable-TreeListof t t)))]
  [make-mutable-treelist (∀ (t) (case→ (→ Integer (Mutable-TreeListof (Option t) (Option t)))
                                       (→ Integer t (Mutable-TreeListof t t))))]

  [treelist-copy (∀ (t) (→ (TreeListof t) (Mutable-TreeListof t t)))]
  [mutable-treelist-snapshot (∀ (r) (→ (Mutable-TreeListof Nothing r) (TreeListof r)))]

  [mutable-treelist-empty? (→ Mutable-TreeListTop Boolean)]
  [mutable-treelist-length (→ Mutable-TreeListTop Index)]

  [mutable-treelist-first (∀ (r) (→ (Mutable-TreeListof Nothing r) r))]
  [mutable-treelist-last  (∀ (r) (→ (Mutable-TreeListof Nothing r) r))]
  [mutable-treelist-ref   (∀ (r) (→ (Mutable-TreeListof Nothing r) Integer r))]

  [mutable-treelist-cons!   (∀ (w) (→ (Mutable-TreeListof w Any) w Void))]
  [mutable-treelist-add!    (∀ (w) (→ (Mutable-TreeListof w Any) w Void))]
  [mutable-treelist-set!    (∀ (w) (→ (Mutable-TreeListof w Any) Integer w Void))]
  [mutable-treelist-insert! (∀ (w) (→ (Mutable-TreeListof w Any) Integer w Void))]

  [mutable-treelist-append!  (∀ (t) (→ (Mutable-TreeListof t Any) (∪ (Mutable-TreeListof Nothing t) (TreeListof t)) Void))]
  [mutable-treelist-prepend! (∀ (t) (→ (Mutable-TreeListof t Any) (∪ (Mutable-TreeListof Nothing t) (TreeListof t)) Void))]

  [mutable-treelist-reverse!    (→ Mutable-TreeListTop Void)]
  [mutable-treelist-delete!     (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-take!       (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-drop!       (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-take-right! (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-drop-right! (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-sublist!    (→ Mutable-TreeListTop Integer Integer Void)]

  [mutable-treelist->vector (∀ (r) (→ (Mutable-TreeListof Nothing r) (Mutable-Vectorof r)))]
  [mutable-treelist->list   (∀ (r) (→ (Mutable-TreeListof Nothing r) (Listof           r)))]
  [vector->mutable-treelist (∀ (t) (→ (Vectorof t) (Mutable-TreeListof t t)))]
  [list->mutable-treelist   (∀ (t) (→ (Listof   t) (Mutable-TreeListof t t)))]

  [mutable-treelist-map! (∀ (w r) (→ (Mutable-TreeListof w r) (→ r w) Void))]

  [mutable-treelist-for-each (∀ (r) (→ (Mutable-TreeListof Nothing r) (→ r Any) Void))]

  [mutable-treelist-member? (∀ (r t) (case→ (→ Mutable-TreeListTop Any Boolean)
                                            (→ (Mutable-TreeListof Nothing r) t (→ t r Any) Boolean)))]

  [mutable-treelist-find (∀ (r) (→ (Mutable-TreeListof Nothing r) (→ r Any) r))]

  [mutable-treelist-sort! (∀ (a b) (case→ (→ (Mutable-TreeListof a a) (→ a a Boolean)
                                             [#:key (Option (→ a a))]
                                             [#:cache-keys? Boolean]
                                             Void)
                                          (→ (Mutable-TreeListof a a) (→ b b Boolean)
                                             #:key (→ a b)
                                             [#:cache-keys? Boolean]
                                             Void)))]

  [in-mutable-treelist (∀ (r) (→ (Mutable-TreeListof Nothing r) (Sequenceof r)))])

(define-syntaxes (for/mutable-treelist for*/mutable-treelist)
  (let ()
    (define-splicing-syntax-class length-clause
      [pattern (~seq #:length n:expr (~optional (~seq #:fill fill:expr)))])
    (define-splicing-syntax-class break-clause
      [pattern (~seq (~or* #:break #:final) guard:expr)])
    (define ((make-for/mutable-treelist derived-stx) stx)
      (syntax-parse stx
        #:datum-literals (:)
        [(_ : t1 (~optional length:length-clause) (clause ...) : t2 break:break-clause ... body ...+)
         #:with (maybe-length ...) (if (attribute length) #'length #'())
         (quasisyntax/loc stx
           (ann
            (vector->mutable-treelist
             (#,derived-stx maybe-length ... (clause ...) : t2 break ... body ...))
            t1))]
        [(_ (~optional length:length-clause) (clause ...) : t break:break-clause ... body ...+)
         #:with (maybe-length ...) (if (attribute length) #'length #'())
         (quasisyntax/loc stx
           (vector->mutable-treelist
            (#,derived-stx maybe-length ... (clause ...) : t break ... body ...)))]
        [(_ (~optional length:length-clause) (clause ...) break:break-clause ... body ...+)
         #:with (maybe-length ...) (if (attribute length) #'length #'())
         (quasisyntax/loc stx
           (vector->mutable-treelist
            (#,derived-stx maybe-length ... (clause ...) : Any break ... body ...)))]))
    (values (make-for/mutable-treelist #'for/vector)
            (make-for/mutable-treelist #'for*/vector))))
(provide for/mutable-treelist for*/mutable-treelist)
