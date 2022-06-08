#lang info
(define collection "mixfix")
(define deps '("base"))
(define build-deps '("rackunit-doc"
                     "scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/mixfix.scrbl" ())))
(define pkg-desc "Mixfix for Racket")
(define version "0.0")
(define pkg-authors '(sorawee))
(define license '(Apache-2.0 OR MIT))
