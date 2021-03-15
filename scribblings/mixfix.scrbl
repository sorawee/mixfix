#lang scribble/manual
@require[scribble/example
         @for-label[mixfix
                    syntax/parse
                    syntax/parse/define
                    racket/contract
                    rackunit
                    (except-in racket/base #%app)]]

@(define evaluator (make-base-eval))
@(evaluator '(require mixfix))
@(define (techref . rest)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") rest))

@title{mixfix}
@author[@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]]

@defmodule[mixfix]

This library allows users to define and use @deftech{mixfix operators} in Racket.

@section{Overview}

The Racket language uses the prefix notation: in a macro invocation @racket[(m x ....)], the macro operator @racket[m] must be an identifier located at the head position, and bound to a @techref{syntax transformer}.

The mixfix notation, on the other hand, allows a macro invocation to have multiple operator tokens in arbitrary position (or even no operator token at all!). For example, one can define the conditional ternary operator via @racket[define-mixfix-rule] and then use it as follows:

@examples[#:label #f #:eval evaluator
  (require (for-syntax racket/base syntax/parse))
  (define-mixfix-rule (c {~datum ?} t {~datum :} e)
    (if c t e))

  (#true ? 1 : 2)
]

Unlike regular macros, syntax transformers in this system are (by default) not associated with any identifier. Instead, a macro invocation simply tries every syntax transformer from various mixfix operators that are in scope, following the ordering that respects the scope hierarchy.

@examples[#:label #f #:eval evaluator
  (let ([x 42])
    (code:comment @#,elem{"Shadows" the previous conditional ternary operator})
    (define-mixfix-rule (c {~datum ?} t {~datum :} e)
      (+ x (if c t e)))

    (#true ? 1 : 2))

  (code:comment @#,elem{The original operator is "restored" outside of @racket[let]})
  (#true ? 1 : 2)
]

While @racket[define-mixfix-rule] is convenient, the library provides the more general @racket[define-mixfix] which allows users to specify an arbitrary syntax transformer.
Using @racket[define-mixfix], users need to manually @deftech{yield} the control to other operators
via @racket[yield-mixfix] when the mixfix operator does not want to transform the input syntax.

@examples[#:label #f #:eval evaluator
  (define-mixfix
    (λ (stx)
      (syntax-parse stx
        [({~datum the} {~datum meaning} {~datum of} {~datum life}) #'42]
        [_ (yield-mixfix)])))

  (the meaning of life)

  (code:comment @#,elem{The "meaning of life" operator is tried,})
  (code:comment @#,elem{but it yields (to the regular function application)})
  (+ 1 2 3)
]

Mixfix operators, regular macros, and core forms can coexist. However, regular macros and core forms will have a higher precedence over mixfix operators.

@examples[#:label #f #:eval evaluator
  (define-mixfix-rule (x {~datum ++})
    (add1 x))

  (1337 ++)

  (code:comment @#,elem{@racket[quote-syntax] takes control here})
  (quote-syntax ++)
]

@section{Operator management}

Similar to regular macros, mixfix operators can be imported and exported from a @techref{module}. Every mixfix operator defining form supports the @racket[#:name] option. The given identifier will be associated with the mixfix operator, allowing users to provide the identifier from a module. It is recommended that the given identifier is not used for any other purpose.

@examples[#:label #f #:eval evaluator
  (module submodule racket
    (require mixfix)
    (define seven 7)
    (define-mixfix-rule (x #:+ y)
      #:name +-operator
      (+ seven x y))
    (define-mixfix-rule (x #:* y)
      #:name *-operator
      (* -1 x y))
    (provide +-operator *-operator))
]

Users can then use @racket[import-mixfix] to import mixfix operators in the specified order.

@examples[#:label #f #:eval evaluator
  (require 'submodule)
  (let ()
    (import-mixfix +-operator *-operator)
    (list (1 #:+ 2) (3 #:* 4)))
]

Specifying many mixfix operators to import can be cumbersome. Therefore, the library allows users to group mixfix operators together as a @deftech{mixfix operator set} via @racket[define-mixfix-set]. Importing a mixfix operator set will import every mixfix operator in the set in the specified order.

@examples[#:label #f #:eval evaluator
  (define-mixfix-set arithmetic-operator-set (+-operator *-operator))
  (let ()
    (import-mixfix arithmetic-operator-set)
    (list (1 #:+ 2) (3 #:* 4)))
]

@section{Caveats}

@subsection{Try order}

Mixfix operators are discovered as the macro expander expands the program. When several mixfix operators are defined (or imported) in the same scope level, those that are defined later will be tried first.

@examples[#:label #f #:eval evaluator
  (define-mixfix-rule (#:x a)
    #:name x-operator
    (add1 a))

  (code:comment @#,elem{@racket[x-operator] is tried and used.})
  (#:x 99)

  (define-mixfix-rule (#:x 99)
    #:name y-operator
    0)

  (code:comment @#,elem{@racket[y-operator] is tried and yielded.})
  (code:comment @#,elem{@racket[x-operator] is tried and used.})
  (#:x 42)

  (code:comment @#,elem{@racket[y-operator] is tried and used.})
  (#:x 99)
]

The interaction of this behavior and @techref{partial expansion} in a @techref{module context} or an @techref{internal-definition context} could lead to a surprising outcome, however. Therefore, mixfix operators should be carefully designed and defined.

@examples[#:label #f #:eval evaluator
  (module another-submodule racket
    (require mixfix)

    (code:comment @#,elem{(1) @racket[x-operator] is discovered.})
    (define-mixfix-rule (#:x a)
      #:name x-operator
      (add1 a))

    (code:comment @#,elem{(2) @racket[x-operator] is used.})
    (#:x 99)

    (code:comment @#,elem{(3) The function application is partially expanded.})
    (values
      (code:comment @#,elem{(5) The arguments are expanded.})
      (code:comment @#,elem{@racket[y-operator] is used.})
      (#:x 99))

    (code:comment @#,elem{(4) @racket[y-operator] is discovered.})
    (define-mixfix-rule (#:x 99)
      #:name y-operator
      0))

  (require 'another-submodule)
]

@subsection{Unintentional yielding}

When using @racket[define-mixfix-rule], users need to be careful that the pattern matching failure will not result in an unintentional yielding.

As an example, users might want to create a shorthand lambda notation as follows:

@examples[#:label #f #:eval evaluator
  (define-mixfix-rule ({~and arg:id {~not {~datum :}}} ... {~datum :} body:expr)
    (λ (arg ...) body))

  (define f (x : (y : (+ x y))))
  ((f 1) 2)
]

But when the lambda notation is ill-formed, the operator would yield to next operators (function application in this case).
Unintentional yielding creates an obscure error at best and incorrect program at worst.

@examples[#:label #f #:eval evaluator
  (eval:error (define g (x : x 1)))
]

One possible solution to this problem is to use the cut (@racket[~!]) operator from @racketmodname[syntax/parse], which can be used to commit the parsing.

@examples[#:label #f #:eval evaluator
  (define-mixfix-rule ({~and arg:id {~not {~datum :}}} ... {~datum :} ~! body:expr)
    (λ (arg ...) body))

  (define f (x : (y : (+ x y))))
  ((f 1) 2)
  (eval:error (define g (x : x 1)))
]

@section{Tips & Tricks}

@subsection{Using an identifier macro at a head position}

An @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro} can be used along with mixfix operators. However, when it is used at a head position, it becomes a regular macro invocation with a higher precedence over mixfix operations.

@examples[#:label #f #:eval evaluator
  (define-syntax (this stx) #'42)
  (define-mixfix-rule (x {~datum +} y)
    (+ x y))

  (99 + this)
  (code:comment @#,elem{Unexpected result})
  (this + 99)
]

To make the identifier macro cooperate with mixfix operators properly, users need to expand the identifier macro to the function application form provided by the @racketmodname[mixfix] library when it is at a head position. The @racketmodname[mixfix] library provides @racket[define-literal] to help automating this process.

@examples[#:label #f #:eval evaluator
  (define-literal this (λ (stx) #'42))
  (define-mixfix-rule (x {~datum +} y)
    (+ x y))

  (99 + this)
  (this + 99)
]

@subsection{Custom function application integration}

Several Racket libraries override @seclink["expand-steps" #:doc '(lib "scribblings/reference/reference.scrbl")]{@racketid[#%app]}. Users may wish to use these libraries along with the mixfix system. Unfortunately, a straightforward attempt will not work because the mixfix system also overrides @racketid[#%app], causing a conflict. However, users can instead create a mixfix operator that always transforms the input syntax with these libraries' @racketid[#%app], thus achieving the same effect.

@examples[#:label #f #:eval evaluator
  (module very-fancy-app racket
    (code:comment @#,elem{Flip all arguments})
    (provide (rename-out [$#%app #%app]))
    (require syntax/parse/define)
    (define-syntax-parser $#%app
      [(_ x ...) #`(#%app #,@(reverse (syntax->list #'(x ...))))]))

  (code:comment @#,elem{Rename @racketid[#%app]})
  (require (only-in 'very-fancy-app [#%app very-fancy-app:#%app]))

  (code:comment @#,elem{Make the fallback mixfix operator catches everything})
  (define-mixfix-rule (arg ...)
    (very-fancy-app:#%app arg ...))

  (define-mixfix-rule (c {~datum ?} t {~datum :} e)
    (if c t e))

  (#true ? 1 : 2)
  (12 34 -)
]

@;{NOTE: after here, the previous section completely wrecks
   the function application, so either make a new evaluator
   or move the examples before the previous section.}

@section{Performance}

The flexibility that this library provides comes at the cost of performance. However, it will only affect compile-time performance. The run-time performance is completely unaffected.

@section{Reference}

@defform[(define-mixfix maybe-option transformer-expr)
         #:grammar
         [(maybe-option (code:line)
                        (code:line #:name name-id))]
         #:contracts ([transformer-expr (-> syntax? syntax?)])]{
  Defines a mixfix operator that associates with @racket[transformer-expr]. If @racket[name-id] is given, additionally binds @racket[name-id] to the mixfix operator, so that it can be @racket[provide]d or used in a @tech{mixfix operator set}.
}

@defproc[(yield-mixfix) any]{
  @tech{Yields} the control to other mixfix operators. This binding is provided @racket[for-syntax].
}

@defidform[#%app]{
  Tries invoking various mixfix operators. This form is used implicitly at every function application. Users do not need to use it directly.
}

@defform[(define-mixfix-rule pattern maybe-option pattern-directive ... template)]{
  Defines a mixfix operator via pattern matching à la @racket[define-syntax-parse-rule]. When the @racket[pattern] fails to match, the mixfix operator will automatically @tech{yield}.
}

@defform[(import-mixfix id ...)]{
  Imports @racket[id]s which must be identifiers bound to a mixfix operator or a @tech{mixfix operator set} in the specified order.
}

@defform[(define-mixfix-set name-id (id ...))]{
  Binds @racket[name-id] to a @tech{mixfix operator set} that contains @racket[id]s which must be identifiers bound to a mixfix operator or a @tech{mixfix operator set} in the specified order.
}

@defform[(define-literal id transformer-expr)
         #:contracts ([transformer-expr (-> syntax? syntax?)])]{
  Defines @racket[id] as an @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro} that cooperates with mixfix operators. The identifier macro is associated with @racket[transformer-expr].
}

@section{Gallery}

@subsection{Parenthesis shape}

@(define new-evaluator (make-base-eval))
@(new-evaluator '(require mixfix
                            (for-syntax syntax/parse
                                        racket/base)))

@examples[#:label #f
          #:eval new-evaluator
          #:preserve-source-locations
  (define-mixfix-rule (x {~seq {~literal +} xs} ...+)
    #:when (eq? (syntax-property this-syntax 'paren-shape) #\{)
    (+ x xs ...))

  (apply + '(1 2 3))
  {4 + 5 + 6}
]

@subsection{Mixfix operators within mixfix operators}


@examples[#:label #f
          #:eval new-evaluator
          #:preserve-source-locations

  (define-mixfix-rule ({~datum $} ~! . _)
    #:fail-when #true "unknown testing form"
    (void))

  (define-mixfix-rule ({~datum $} x {~datum is} y {~datum because-of} z)
    (let ([x* x] [y* y] [z* z])
      (unless (equal? x* y*)
        (raise-arguments-error 'test "not equal"
                               "expected" y*
                               "got" x*))
      (unless (equal? z* y*)
        (raise-arguments-error 'test "wrong explanation"
                               "expected" y*
                               "explanation" z*))))

  (define-mixfix-rule ({~datum $} x {~datum is} y)
    (let ([y* y])
      ($ x is y* because-of y*)))

  (define (times-two x)
    (+ x x))

  ($ (times-two 3) is 6)
  (eval:error ($ (times-two 3) is 7))
  ($ (times-two 3) is 6 because-of (* 2 3))
  (eval:error ($ (times-two 3) is 6 because-of (+ 2 3)))
  (eval:error ($ (times-two 3) is 6 because-of))
]

@subsection{Usage in @racket[module+]}

@examples[#:label #f
          #:preserve-source-locations
  (module plus racket/base
    (require mixfix
             (for-syntax syntax/parse racket/base))
    (define-mixfix-rule (a {~datum +} b)
      (+ a b))
    (module+ test
      (require rackunit)
      (check-equal? (1 + 2) 3)
      (check-equal? (4 + 5) 6)))
  (require (submod 'plus test))
]

@section{Acknowledgements}

I would like to thank @link["https://users.cs.northwestern.edu/~syt3996/"]{Shu-Hung You} for suggesting a way to remove the limitation when defining mixfix operators in a module but using them in @racket[module+] submodules.
