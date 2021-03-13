#lang racket

(module+ test
  (require mixfix
           rackunit
           (for-syntax syntax/parse))

  (define-literal this
    (λ (stx) #'42))
  (define-mixfix-rule (a {~datum +} b)
    (+ a b))

  (check-equal? (this + 2) 44)
  (check-equal? (2 + this) 44)
  (check-equal? this 42))
