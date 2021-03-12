#lang racket

(module+ test
  (require mixfix
           rackunit
           syntax/parse/define
           (for-syntax racket/base syntax/parse))

  (define-syntax-parse-rule (check-expand found expected)
    #:with result (local-expand #'found 'expression '())
    (check-equal? result expected))

  (define-mixfix-rule (#:x x)
    1)

  (check-equal? (#:x 8) 1)
  (check-equal? (#:x 9) 2)
  (check-expand (#:x 9) 1)


  (define-mixfix-rule (#:x 9)
    2)

  (check-equal? (#:x 8) 1)
  (check-equal? (#:x 9) 2)
  (check-expand (#:x 9) 2))
