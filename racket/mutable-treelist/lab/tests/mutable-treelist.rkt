#lang typed/racket/base

(require "../../lab.rkt"
         racket/treelist
         typed/rackunit)


(test-case "mutable-treelist creation"
  (for ([mtl : (Mutable-TreeListof Integer Integer)
             (in-list (list (mutable-treelist 1 1 1)
                            (make-mutable-treelist 3 1)))])
    (check-true (mutable-treelist? mtl))
    (check-equal? (mutable-treelist-length mtl) 3)
    (check-equal? (mutable-treelist->list mtl) '(1 1 1)))

  (: mtl (Mutable-TreeListof (Option Integer) (Option Integer)))
  (define mtl (make-mutable-treelist 3))
  (check-equal? (mutable-treelist->list mtl) '(#f #f #f))
  (mutable-treelist-set! mtl 1 0)
  (check-equal? (mutable-treelist->list mtl) '(#f  0 #f)))

(test-case "copy and snapshot"
  (: tl (TreeListof Integer))
  (define tl (treelist 1 2 3))
  (check-equal? (mutable-treelist->list (treelist-copy tl)) '(1 2 3))
  (: mtl (Mutable-TreeListof Integer Integer))
  (define mtl (mutable-treelist 4 5 6))
  (check-equal? (treelist->list (mutable-treelist-snapshot mtl)) '(4 5 6)))

(test-case "mutable-treelist length"
  (: mtl (Mutable-TreeListof Integer Integer))
  (define mtl (mutable-treelist 1 2 3))

  (check-false (mutable-treelist-empty? mtl))
  (check-eq? (mutable-treelist-length mtl) 3)

  (for ([i (in-range (mutable-treelist-length mtl))])
    (mutable-treelist-delete! mtl 0))
  (check-true (mutable-treelist-empty? mtl))
  (check-eq? (mutable-treelist-length mtl) 0))

(test-case "mutable-treelist ref"
  (: mtl (Mutable-TreeListof Integer Integer))
  (define mtl (mutable-treelist 1 2 3))

  (check-equal? (mutable-treelist-first mtl) 1)
  (check-equal? (mutable-treelist-ref mtl 1) 2)
  (check-equal? (mutable-treelist-last  mtl) 3))

(test-case "mutable-treelist insert! and set!"
  (: mtl0 (Mutable-TreeListof Integer Integer))
  (define mtl0 (mutable-treelist -1 -2 -3))
  (: mtl1 (Mutable-TreeListof Natural Integer))
  (define mtl1 mtl0)

  (mutable-treelist-cons! mtl1 0)
  (mutable-treelist-add!  mtl0 -4)
  (mutable-treelist-insert! mtl1 5 5)
  (check-equal? (mutable-treelist->list mtl0) '(0 -1 -2 -3 -4 5))

  (mutable-treelist-delete! mtl1 5)
  (mutable-treelist-delete! mtl0 2)
  (check-equal? (mutable-treelist->list mtl1) '(0 -1 -3 -4))

  (mutable-treelist-set! mtl1 3  0)
  (mutable-treelist-set! mtl0 2 -1)
  (mutable-treelist-set! mtl0 1 -2)
  (mutable-treelist-set! mtl0 0 -3)
  (check-equal? (mutable-treelist->list mtl1) '(-3 -2 -1 0)))

(test-case "mutable-treelist delete!"
  (: mtl (Mutable-TreeListof Integer Integer))
  (define mtl (mutable-treelist))

  (set! mtl (mutable-treelist 0 1 2 3 4 5))
  (mutable-treelist-reverse! mtl)
  (check-equal? (mutable-treelist->list mtl) '(5 4 3 2 1 0))

  (set! mtl (mutable-treelist 0 1 2 3 4 5))
  (mutable-treelist-delete! mtl 3)
  (check-equal? (mutable-treelist->list mtl) '(0 1 2 4 5))

  (set! mtl (mutable-treelist 0 1 2 3 4 5))
  (mutable-treelist-take! mtl 3)
  (check-equal? (mutable-treelist->list mtl) '(0 1 2))

  (set! mtl (mutable-treelist 0 1 2 3 4 5))
  (mutable-treelist-drop! mtl 3)
  (check-equal? (mutable-treelist->list mtl) '(3 4 5))

  (set! mtl (mutable-treelist 0 1 2 3 4 5))
  (mutable-treelist-take-right! mtl 3)
  (check-equal? (mutable-treelist->list mtl) '(3 4 5))

  (set! mtl (mutable-treelist 0 1 2 3 4 5))
  (mutable-treelist-drop-right! mtl 3)
  (check-equal? (mutable-treelist->list mtl) '(0 1 2))

  (set! mtl (mutable-treelist 0 1 2 3 4 5))
  (mutable-treelist-sublist! mtl 2 4)
  (check-equal? (mutable-treelist->list mtl) '(2 3)))

(test-case "mutable-treelist append! and prepend!"
  (: mtl (Mutable-TreeListof Integer Integer))
  (define mtl (mutable-treelist -1 0 1))
  (: mtl0 (Mutable-TreeListof Integer Integer))
  (define mtl0 (mutable-treelist 2 3 4))
  (: tl (TreeListof Integer))
  (define tl (treelist -4 -3 -2))
  (mutable-treelist-append!  mtl mtl0)
  (mutable-treelist-prepend! mtl tl)
  (check-equal? (mutable-treelist->list mtl) '(-4 -3 -2 -1 0 1 2 3 4)))

(test-case "mutable-treelist conversion"
  (: mtl (Mutable-TreeListof Integer Integer))
  (define mtl (mutable-treelist 0 1 2))

  (define vec (mutable-treelist->vector mtl))
  (check-equal? (vector->list vec) '(0 1 2))
  (check-false (immutable? vec))
  (set! mtl (vector->mutable-treelist vec))

  (define ls (mutable-treelist->list mtl))
  (check-equal? ls '(0 1 2))
  (set! mtl (list->mutable-treelist ls)))

(test-case "mutable-treelist map!"
  (: mtl (Mutable-TreeListof Natural Integer))
  (define mtl (mutable-treelist 0 1 2))
  (mutable-treelist-map! mtl abs)
  (check-equal? (mutable-treelist->list mtl) '(0 1 2)))

(test-case "mutable-treelist for-each"
  (: mtl (Mutable-TreeListof Nothing Integer))
  (define mtl (mutable-treelist 0 1 2))
  (: ls (Listof Any))
  (define ls '())
  (mutable-treelist-for-each mtl (λ (i) (set! ls (cons i ls))))
  (check-equal? ls '(2 1 0)))

(test-case "mutable-treelist find"
  (: mtl (Mutable-TreeListof Integer Integer))
  (define mtl (mutable-treelist -2 -1 0 1 2))
  (check-eq? (mutable-treelist-find mtl index?) 0))

(test-case "mutable-treelist sort!"
  (: mtl (Mutable-TreeListof Integer Integer))
  (define mtl (mutable-treelist -2 -1 0 1 2))
  (mutable-treelist-sort! mtl >)
  (check-equal? (mutable-treelist->list mtl) '(2 1 0 -1 -2)))

(test-case "mutable-treelist iteration"
  (: mtl (Mutable-TreeListof Natural Integer))
  (define mtl (mutable-treelist -2 -1 0 1 2))
  (: ls (Listof Any))
  (define ls (for/list ([i : Integer (in-mutable-treelist mtl)]) i))
  (check-equal? ls '(-2 -1 0 1 2)))

(test-case "for[*]/mutable-treelist"
  (define mtl0 (for/mutable-treelist #:length 10 #:fill 'b ([i 5]) i))
  (mutable-treelist-set! mtl0 0 'a)
  (check-equal? (mutable-treelist->list mtl0) '(a 1 2 3 4 b b b b b))

  (define mtl1 (for/mutable-treelist : (Mutable-TreeListof Natural Integer) ([i 5]) : Natural i))
  (mutable-treelist-set! mtl1 0 9)
  (check-equal? (mutable-treelist->list mtl1) '(9 1 2 3 4))

  (define mtl2 (for/mutable-treelist ([i 5]) : Natural i))
  (mutable-treelist-set! mtl2 0 9)
  (check-equal? (mutable-treelist->list mtl2) '(9 1 2 3 4))

  (define mtl3 (for/mutable-treelist ([i 5]) i))
  (mutable-treelist-set! mtl3 0 9)
  (check-equal? (mutable-treelist->list mtl3) '(9 1 2 3 4)))
