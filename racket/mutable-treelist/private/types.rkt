#lang typed/racket/base

;; TODO (Mutable-TreeListof t) → (Mutable-TreeListof t t)
(struct (-t +t) Mutable-TreeListof ([_ : (Parameter -t +t)]))
#;(struct (a ...) _ ([_ : (Parameter a ...)]) #:type-name Mutable-TreeListof) ; not work well
(define-type (MTreeListof -t +t) (Mutable-TreeListof -t +t)) ; avoid printing #(struct:Mutable-TreeListof ...)
(provide (rename-out [MTreeListof Mutable-TreeListof]))

(define-type Mutable-TreeListTop (MTreeListof Nothing Any))
(define-type Mutable-TreeListBot (MTreeListof Any Nothing))
(provide Mutable-TreeListTop Mutable-TreeListBot)
