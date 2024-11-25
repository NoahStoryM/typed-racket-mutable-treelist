#lang info

(define license 'MIT)
(define collection "unsafe")
(define version "0.0")

(define pkg-desc "Mutable treelists for typed racket")

(define deps '("base" "typed-racket-lib"))
(define build-deps '("rackunit-typed"))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
