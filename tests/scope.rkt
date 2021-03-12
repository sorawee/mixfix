#lang racket

(module+ test
  (require mixfix
           rackunit
           (for-syntax racket/base syntax/parse))

  (define-mixfix-rule (c {~datum ?} t {~datum :} e)
    (if c t e))

  (check-equal? (#true ? 1 : 2) 1)
  (check-equal? (#false ? 1 : 2) 2)

  (check-equal?
   (let ([x 42])
     (define-mixfix-rule (c {~datum ?} t {~datum :} e)
       (+ x (if c t e)))

     (#true ? 1 : 2))
   43)

  (check-equal? (#true ? 1 : 2) 1))
