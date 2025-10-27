#lang typed/racket/base/optional

(require (for-syntax racket/base syntax/parse)
         racket/treelist
         "private/types.rkt")

(provide (all-from-out "private/types.rkt"))

(require/typed/provide racket/mutable-treelist
  [mutable-treelist? (pred Mutable-TreeListTop)]
  [mutable-treelist  (∀ (t) (→ t * (Mutable-TreeListof t t)))]
  [make-mutable-treelist (∀ (t) (case→ (→ Integer (Mutable-TreeListof (Option t) (Option t)))
                                       (→ Integer t (Mutable-TreeListof t t))))]

  [treelist-copy (∀ (t) (→ (TreeListof t) (Mutable-TreeListof t t)))]
  [mutable-treelist-snapshot (∀ (+t) (→ (Mutable-TreeListof Nothing +t) (TreeListof +t)))]

  [mutable-treelist-empty? (→ Mutable-TreeListTop Boolean)]
  [mutable-treelist-length (→ Mutable-TreeListTop Index)]

  [mutable-treelist-first (∀ (+t) (→ (Mutable-TreeListof Nothing +t) +t))]
  [mutable-treelist-last  (∀ (+t) (→ (Mutable-TreeListof Nothing +t) +t))]
  [mutable-treelist-ref   (∀ (+t) (→ (Mutable-TreeListof Nothing +t) Integer +t))]

  [mutable-treelist-cons!   (∀ (-t) (→ (Mutable-TreeListof -t Any) -t Void))]
  [mutable-treelist-add!    (∀ (-t) (→ (Mutable-TreeListof -t Any) -t Void))]
  [mutable-treelist-set!    (∀ (-t) (→ (Mutable-TreeListof -t Any) Integer -t Void))]
  [mutable-treelist-insert! (∀ (-t) (→ (Mutable-TreeListof -t Any) Integer -t Void))]

  [mutable-treelist-append!  (∀ (t) (→ (Mutable-TreeListof t Any) (∪ (Mutable-TreeListof Nothing t) (TreeListof t)) Void))]
  [mutable-treelist-prepend! (∀ (t) (→ (Mutable-TreeListof t Any) (∪ (Mutable-TreeListof Nothing t) (TreeListof t)) Void))]

  [mutable-treelist-reverse!    (→ Mutable-TreeListTop Void)]
  [mutable-treelist-delete!     (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-take!       (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-drop!       (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-take-right! (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-drop-right! (→ Mutable-TreeListTop Integer Void)]
  [mutable-treelist-sublist!    (→ Mutable-TreeListTop Integer Integer Void)]

  [mutable-treelist->vector (∀ (+t) (→ (Mutable-TreeListof Nothing +t) (Mutable-Vectorof +t)))]
  [mutable-treelist->list   (∀ (+t) (→ (Mutable-TreeListof Nothing +t) (Listof           +t)))]
  [vector->mutable-treelist (∀ (t) (→ (Vectorof t) (Mutable-TreeListof t t)))]
  [list->mutable-treelist   (∀ (t) (→ (Listof   t) (Mutable-TreeListof t t)))]

  [mutable-treelist-map! (∀ (-t +t) (→ (Mutable-TreeListof -t +t) (→ +t -t) Void))]

  [mutable-treelist-for-each (∀ (+t) (→ (Mutable-TreeListof Nothing +t) (→ +t Any) Void))]

  [mutable-treelist-member? (∀ (+t t) (case→ (→ Mutable-TreeListTop Any Boolean)
                                             (→ (Mutable-TreeListof Nothing +t) t (→ t +t Any) Boolean)))]

  [mutable-treelist-find (∀ (+t) (→ (Mutable-TreeListof Nothing +t) (→ +t Any) +t))]

  [mutable-treelist-sort! (∀ (a b) (case→ (→ (Mutable-TreeListof a a) (→ a a Boolean)
                                             [#:key (Option (→ a a))]
                                             [#:cache-keys? Boolean]
                                             Void)
                                          (→ (Mutable-TreeListof a a) (→ b b Boolean)
                                             #:key (→ a b)
                                             [#:cache-keys? Boolean]
                                             Void)))]

  [in-mutable-treelist (∀ (+t) (→ (Mutable-TreeListof Nothing +t) (Sequenceof +t)))])

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
