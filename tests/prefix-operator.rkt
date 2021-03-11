#lang racket

(module+ test
  (require mixfix
           rackunit
           (for-syntax syntax/parse))

  (define-literals (@))
  (define-mixfix-rule ({~literal @} a b)
    (+ a b))

  (check-equal? (@ 1 2) 3))
