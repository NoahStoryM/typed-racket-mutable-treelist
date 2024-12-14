#lang typed/racket/base

(require "../../lab.rkt" typed/rackunit)


(test-case "mtreelist creation"
  (for ([mtl : (MTreeListof Integer Integer)
             (in-list (list (mtreelist 1 1 1)
                            (make-mtreelist 3 1)))])
    (check-true (mtreelist? mtl))
    (check-equal? (mtreelist-length mtl) 3)
    (check-equal? (mtreelist->list mtl) '(1 1 1)))

  (: mtl (MTreeListof (Option Integer) (Option Integer)))
  (define mtl (make-mtreelist 3))
  (check-equal? (mtreelist->list mtl) '(#f #f #f))
  (mtreelist-set! mtl 1 0)
  (check-equal? (mtreelist->list mtl) '(#f  0 #f)))

(test-case "mtreelist length"
  (: mtl (MTreeListof Integer Integer))
  (define mtl (mtreelist 1 2 3))

  (check-false (mtreelist-empty? mtl))
  (check-eq? (mtreelist-length mtl) 3)

  (for ([i (in-range (mtreelist-length mtl))])
    (mtreelist-delete! mtl 0))
  (check-true (mtreelist-empty? mtl))
  (check-eq? (mtreelist-length mtl) 0))

(test-case "mtreelist ref"
  (: mtl (MTreeListof Integer Integer))
  (define mtl (mtreelist 1 2 3))

  (check-equal? (mtreelist-first mtl) 1)
  (check-equal? (mtreelist-ref mtl 1) 2)
  (check-equal? (mtreelist-last  mtl) 3))

(test-case "mtreelist insert! and set!"
  (: mtl0 (MTreeListof Integer Integer))
  (define mtl0 (mtreelist -1 -2 -3))
  (: mtl1 (MTreeListof Natural Integer))
  (define mtl1 mtl0)

  (mtreelist-cons! mtl1 0)
  (mtreelist-add!  mtl0 -4)
  (mtreelist-insert! mtl1 5 5)
  (check-equal? (mtreelist->list mtl0) '(0 -1 -2 -3 -4 5))

  (mtreelist-delete! mtl1 5)
  (mtreelist-delete! mtl0 2)
  (check-equal? (mtreelist->list mtl1) '(0 -1 -3 -4))

  (mtreelist-set! mtl1 3  0)
  (mtreelist-set! mtl0 2 -1)
  (mtreelist-set! mtl0 1 -2)
  (mtreelist-set! mtl0 0 -3)
  (check-equal? (mtreelist->list mtl1) '(-3 -2 -1 0)))

(test-case "mtreelist delete!"
  (: mtl (MTreeListof Integer Integer))
  (define mtl (mtreelist))

  (set! mtl (mtreelist 0 1 2 3 4 5))
  (mtreelist-reverse! mtl)
  (check-equal? (mtreelist->list mtl) '(5 4 3 2 1 0))

  (set! mtl (mtreelist 0 1 2 3 4 5))
  (mtreelist-delete! mtl 3)
  (check-equal? (mtreelist->list mtl) '(0 1 2 4 5))

  (set! mtl (mtreelist 0 1 2 3 4 5))
  (mtreelist-take! mtl 3)
  (check-equal? (mtreelist->list mtl) '(0 1 2))

  (set! mtl (mtreelist 0 1 2 3 4 5))
  (mtreelist-drop! mtl 3)
  (check-equal? (mtreelist->list mtl) '(3 4 5))

  (set! mtl (mtreelist 0 1 2 3 4 5))
  (mtreelist-take-right! mtl 3)
  (check-equal? (mtreelist->list mtl) '(3 4 5))

  (set! mtl (mtreelist 0 1 2 3 4 5))
  (mtreelist-drop-right! mtl 3)
  (check-equal? (mtreelist->list mtl) '(0 1 2))

  (set! mtl (mtreelist 0 1 2 3 4 5))
  (mtreelist-sublist! mtl 2 4)
  (check-equal? (mtreelist->list mtl) '(2 3)))

(test-case "mtreelist append!"
  (: mtl (MTreeListof Integer Integer))
  (define mtl (mtreelist 0 1 2))
  (mtreelist-append! mtl mtl)
  (check-equal? (mtreelist->list mtl) '(0 1 2 0 1 2)))

(test-case "mtreelist conversion"
  (: mtl (MTreeListof Integer Integer))
  (define mtl (mtreelist 0 1 2))

  (define vec (mtreelist->vector mtl))
  (check-equal? (vector->list vec) '(0 1 2))
  (check-false (immutable? vec))
  (set! mtl (vector->mtreelist vec))

  (define ls (mtreelist->list mtl))
  (check-equal? ls '(0 1 2))
  (set! mtl (list->mtreelist ls)))

(test-case "mtreelist map!"
  (: mtl (MTreeListof Natural Integer))
  (define mtl (mtreelist 0 1 2))
  (mtreelist-map! mtl abs)
  (check-equal? (mtreelist->list mtl) '(0 1 2)))

(test-case "mtreelist for-each"
  (: mtl (MTreeListof Nothing Integer))
  (define mtl (mtreelist 0 1 2))
  (: ls (Listof Any))
  (define ls '())
  (mtreelist-for-each mtl (λ (i) (set! ls (cons i ls))))
  (check-equal? ls '(2 1 0)))

(test-case "mtreelist find"
  (: mtl (MTreeListof Integer Integer))
  (define mtl (mtreelist -2 -1 0 1 2))
  (check-eq? (mtreelist-find mtl index?) 0))

(test-case "mtreelist sort!"
  (: mtl (MTreeListof Integer Integer))
  (define mtl (mtreelist -2 -1 0 1 2))
  (mtreelist-sort! mtl >)
  (check-equal? (mtreelist->list mtl) '(2 1 0 -1 -2)))

(test-case "mtreelist iteration"
  (: mtl (MTreeListof Natural Integer))
  (define mtl (mtreelist -2 -1 0 1 2))
  (: ls (Listof Any))
  (define ls (for/list ([i : Integer (in-mtreelist mtl)]) i))
  (check-equal? ls '(-2 -1 0 1 2)))

(test-case "for[*]/mtreelist"
  (define mtl0 (for/mtreelist ([i 5]) i))
  (mtreelist-set! mtl0 0 'a)
  (check-equal? (mtreelist->list mtl0) '(a 1 2 3 4))

  (define mtl1 (for/mtreelist : (MTreeListof Natural Integer) ([i 5]) i))
  (mtreelist-set! mtl1 0 9)
  (check-equal? (mtreelist->list mtl1) '(9 1 2 3 4))

  (define mtl2 (for/mtreelist ([i 5]) : (MTreeListof Natural Integer) i))
  (mtreelist-set! mtl2 0 9)
  (check-equal? (mtreelist->list mtl2) '(9 1 2 3 4))

  (define mtl3 (for/mtreelist : (MTreeListof Natural Integer) ([i 5]) : (MTreeListof Natural Natural) i))
  (mtreelist-set! mtl3 0 9)
  (check-equal? (mtreelist->list mtl3) '(9 1 2 3 4)))
