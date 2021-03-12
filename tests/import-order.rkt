#lang racket

(module sub racket
  (require mixfix
           (for-syntax racket/base syntax/parse))

  (define-mixfix-rule (#:x a b c ...)
    #:name x-op
    1)

  (define-mixfix-rule (#:x a b ...)
    #:name y-op
    2)

  (define-mixfix-rule (#:x a ...)
    #:name z-op
    3)

  (define-mixfix-set forward-set (x-op y-op z-op))
  (define-mixfix-set backward-set (z-op y-op x-op))

  (define-mixfix-set backward-set-1 (z-op))
  (define-mixfix-set backward-set-2 (backward-set-1 y-op))
  (define-mixfix-set backward-set-3 (backward-set-2 x-op))

  (define-mixfix-set backward-set-1* (x-op))
  (define-mixfix-set backward-set-2* (y-op backward-set-1*))
  (define-mixfix-set backward-set-3* (z-op backward-set-2*))

  (define-mixfix-set forward-set-1 (x-op))
  (define-mixfix-set forward-set-2 (forward-set-1 y-op))
  (define-mixfix-set forward-set-3 (forward-set-2 z-op))

  (define-mixfix-set forward-set-1* (z-op))
  (define-mixfix-set forward-set-2* (y-op forward-set-1*))
  (define-mixfix-set forward-set-3* (x-op forward-set-2*))

  (provide x-op y-op z-op forward-set backward-set
           forward-set-3 forward-set-3*
           backward-set-3 backward-set-3*))

(module+ test
  (require mixfix
           rackunit
           (submod ".." sub))

  (let ()
    (import-mixfix x-op y-op z-op)
    (check-equal? (#:x q q) 3)
    (check-equal? (#:x q) 3)
    (check-equal? (#:x) 3))

  (let ()
    (import-mixfix z-op y-op x-op)
    (check-equal? (#:x q q) 1)
    (check-equal? (#:x q) 2)
    (check-equal? (#:x) 3))

  (let ()
    (import-mixfix forward-set)
    (check-equal? (#:x q q) 3)
    (check-equal? (#:x q) 3)
    (check-equal? (#:x) 3))

  (let ()
    (import-mixfix backward-set)
    (check-equal? (#:x q q) 1)
    (check-equal? (#:x q) 2)
    (check-equal? (#:x) 3))

  (let ()
    (import-mixfix forward-set-3)
    (check-equal? (#:x q q) 3)
    (check-equal? (#:x q) 3)
    (check-equal? (#:x) 3))

  (let ()
    (import-mixfix forward-set-3*)
    (check-equal? (#:x q q) 3)
    (check-equal? (#:x q) 3)
    (check-equal? (#:x) 3))

  (let ()
    (import-mixfix backward-set-3)
    (check-equal? (#:x q q) 1)
    (check-equal? (#:x q) 2)
    (check-equal? (#:x) 3))

  (let ()
    (import-mixfix backward-set-3*)
    (check-equal? (#:x q q) 1)
    (check-equal? (#:x q) 2)
    (check-equal? (#:x) 3)))
