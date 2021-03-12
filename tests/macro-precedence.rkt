#lang racket

(module+ test
  (require mixfix
           rackunit
           (for-syntax racket/base syntax/parse))

  (define-mixfix-rule (c {~datum +one})
    (add1 c))

  (check-equal? (1337 +one) 1338)
  (check-equal? (symbol->string (quote +one)) "+one"))
