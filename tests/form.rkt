#lang racket

(module+ test
  (require mixfix
           rackunit
           (for-syntax racket/base syntax/parse))

  (define-mixfix-rule ({~datum let~} x:id {~datum :=} v:expr)
    (define x v))

  (let~ x := 1)
  (let~ y := (add1 x))
  (check-equal? y 2))
