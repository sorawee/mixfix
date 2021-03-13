#lang racket/base

(provide (rename-out [@#%app #%app])
         define-mixfix
         define-mixfix-rule
         define-mixfix-set
         import-mixfix
         define-literal
         (for-syntax yield-mixfix))

(require syntax/parse/define
         (for-syntax syntax/apply-transformer
                     syntax/parse
                     racket/base))

(begin-for-syntax
  (struct mixfix-transformer (t))
  (struct mixfix-transformer-set (s))
  (define mixfix-transformers '())
  (struct exn:fail:mixfix:unsupported exn:fail ())
  (define (yield-mixfix)
    (raise (exn:fail:mixfix:unsupported "unsupported"
                                        (current-continuation-marks))))

  (define-splicing-syntax-class options
    #:attributes (the-id)
    (pattern {~seq {~alt {~optional {~or* {~seq #:name name:id}}}} ...}
             #:with the-id #'{~? name the-transformer}))

  (define (get-transformer-id-list ts stx)
    (for/fold ([transformers '()]) ([t (in-list ts)])
      (define the-value (syntax-local-value t (λ () #f)))
      (cond
        [(mixfix-transformer? the-value) (cons t transformers)]
        [(mixfix-transformer-set? the-value)
         (append (reverse (mixfix-transformer-set-s the-value))
                 transformers)]
        [else
         (raise-syntax-error
          #f
          "identifier not bound to mixfix-transformer or mixfix-transformer-set"
          stx
          t)]))))

(define-syntax-parse-rule (stash x)
  #:do [(set! mixfix-transformers (cons #'x mixfix-transformers))]
  (begin))

(define-syntax-parse-rule (define-mixfix :options transformer)
  (begin
    (define-syntax the-id (mixfix-transformer transformer))
    (stash the-id)))

(define-syntax-parser @#%app
  [(_ . xs)
   (define stx
     (for/or ([transformer-id (in-list mixfix-transformers)])
       (define transformer (syntax-local-value transformer-id (λ () #f)))
       (cond
         [(mixfix-transformer? transformer)
          (with-handlers ([exn:fail:mixfix:unsupported? (λ (e) #f)])
            ((mixfix-transformer-t transformer) #'xs))]
         [else #f])))
   (cond
     [stx stx]
     [else #'(#%app . xs)])])

(define-syntax-parser import-mixfix
  [(_ x:id ...)
   (define new-transformers (get-transformer-id-list (attribute x)
                                                     this-syntax))
   (set! mixfix-transformers (append new-transformers mixfix-transformers))
   #'(begin)])

(define-syntax-parse-rule (define-mixfix-rule pattern options:options
                            pattern-directive ... template)
  (define-mixfix {~@ . options}
    (syntax-parser
      #:track-literals
      [pattern pattern-directive ... #'template]
      [_ (yield-mixfix)])))

(define-syntax-parse-rule (define-mixfix-set name:id (x:id ...))
  #:with (the-set ...) (reverse (get-transformer-id-list (attribute x)
                                                         this-syntax))
  (define-syntax name
    (mixfix-transformer-set (list (quote-syntax the-set) ...))))

(define-syntax-parse-rule (define-literal op:id
                            transformer-expr:expr)
  (define-syntax (op stx)
    (syntax-parse stx
      [:id (transformer-expr stx)]
      [(arg . args) #'(@#%app arg . args)])))
