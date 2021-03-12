#lang scribble/manual
@require[scribble/example
         @for-label[mixfix
                    syntax/parse
                    syntax/parse/define
                    racket/contract
                    (except-in racket/base #%app)]]

@(define evaluator (make-base-eval))
@(evaluator '(require mixfix))

@title{mixfix}
@author[@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]]

@defmodule[mixfix]

This library allows users to define and use @deftech{mixfix operators} in Racket.

@section{Overview}

The Racket language uses the prefix notation: in a macro invocation @racket[(m x ....)], the macro operator @racket[m] must be an identifier located at the head position, and bound to a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax transformer}.

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
  (define-mixfix-rule (x {~datum +one})
    (add1 x))

  (1337 +one)

  (code:comment @#,elem{@racket[quote-syntax] takes control here})
  (quote-syntax +one)
]

@section{Operator management}

Similar to regular macros, mixfix operators can be imported and exported from a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{module}. Each mixfix operator defining form supports the @racket[#:name] option. The given identifier will be associated with the mixfix operator, allowing users to provide the identifier from a module. It is recommended that the given identifier is not used for any other purpose.

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

The interaction of this behavior and @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{partial expansion} in a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{module context} or an @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{internal-definition context} could lead to a surprising outcome, however.

@examples[#:label #f #:eval evaluator
  (module another-submodule racket
    (require mixfix)

    (code:comment @#,elem{(1) the macro expander discovers @racket[x-operator].})
    (define-mixfix-rule (#:x a)
      #:name x-operator
      (add1 a))

    (code:comment @#,elem{(2) the macro expander expands @racket[(#:x 99)].})
    (#:x 99)

    (code:comment @#,elem{(3) the macro expander only partially expands @racket[let].})
    (let ()
      (code:comment @#,elem{(5) the macro expander expands the body of @racket[let].})
      (#:x 99))

    (code:comment @#,elem{(4) the macro expander discovers @racket[y-operator].})
    (define-mixfix-rule (#:x 99)
      #:name y-operator
      0))

  (require 'another-submodule)
]

To avoid a surprising result like this, it is recommended that mixfix operators should not overlap with each other.

@subsection{Recognization of literals via binding}

Racket can recognize literals via binding (e.g., with @racket[free-identifier=?] or @racket[~literal]). One commonly used approach is to bind these literals to a syntax transformer that always raises a syntax error to signal that they are used in an unexpected context. This approach however does not work well with mixfix operators when a literal to be recognized is at the head position, since it becomes a regular macro with a higher precedence over mixfix operators.

@examples[#:label #f #:eval evaluator
  (define-syntax (this stx) (raise-syntax-error #f "out of context" stx))
  (define-mixfix-rule ({~literal this} x)
    (list 'this x))

  (eval:error this)
  (code:comment @#,elem{Unexpected error})
  (eval:error (this 7))
]

Instead, users can define such literals with @racket[define-literals], which will make the defined literals cooperate with mixfix operators properly.

@examples[#:label #f #:eval evaluator
  (define-literals (this))
  (define-mixfix-rule ({~literal this} x)
    (list 'this x))

  (eval:error this)
  (this 7)
]

More generally, @racket[define-literals] allows users to supply a syntax transformer, which will be used when the defined literals are used as @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macros}.

@examples[#:label #f #:eval evaluator
  (define-literals (this)
    (λ (stx) #'42))
  (define-mixfix-rule ({~literal this} x)
    (list 'this x))

  this
  (this 7)
]


@section{Tips & Tricks}

@subsection{Custom function application integration}

Several Racket libraries override @seclink["expand-steps" #:doc '(lib "scribblings/reference/reference.scrbl")]{@racketid[#%app]}. Users may wish to use these libraries along with the mixfix system. Unfortunately, a straightforward attempt will not work because the mixfix system also overrides @racketid[#%app], causing a conflict. However, users can instead create a mixfix operator that always transforms the input syntax with these libraries' @racketid[#%app], thus achieving the same effect.

@examples[#:label #f #:eval evaluator
  (module very-fancy-app racket
    (code:comment @#,elem{Flip all arguments})
    (provide (rename-out [$#%app #%app]))
    (define-syntax ($#%app stx)
      (syntax-case stx ()
        [(_ x ...) #`(#%app #,@(reverse (syntax->list #'(x ...))))])))

  (code:comment @#,elem{Don't import directly})
  (require (only-in 'very-fancy-app [#%app very-fancy-app:#%app]))

  (code:comment @#,elem{Make the fallback mixfix operator catches everything})
  (define-mixfix-rule (arg ...)
    (very-fancy-app:#%app arg ...))

  (define-mixfix-rule (c {~datum ?} t {~datum :} e)
    (if c t e))

  (#true ? 1 : 2)
  (12 34 -)
]

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

@defform[(define-literals (id ...) maybe-transformer)
         #:grammar
         [(maybe-transformer (code:line)
                             (code:line transformer-expr))]
         #:contracts ([transformer-expr (-> syntax? syntax?)])]{
  Defines @racket[id]s as @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macros} that cooperate with mixfix operators. The identifier macros are associated with @racket[transformer-expr]. If not given, @racket[transformer-expr] defaults to a procedure that always raises an ``out of context'' syntax error.
}
