#lang scribble/manual

@(require (for-label racket/base
                     racket/match
                     defpat/match-case-lambda
                     generic-bind))

@title{match-case-lambda}

@defmodule[defpat/match-case-lambda]

@defform[(match*-case-lambda clause ...)
         #:grammar ([clause [args body ...+]
                            [args (=> id) body ...+]
                            [args #:when cond-expr body ...+]]
                    [args [arg-pat ...]
                          rest-id
                          [arg-pat ... . rest-id]])]{
like @racket[case-lambda], except that each @racket[arg-pat] can be an
arbitrary @racket[match] pattern.

As an example,
@racketblock[(match*-case-lambda
               [[(list x y) (vector z)]
                body])]
is equivalent to
@racketblock[(case-lambda
               [(tmp1 tmp2)
                (match* [tmp1 tmp2]
                  [[(list x y) (vector z)]
                   body])])]

Clauses with the same arity are grouped together into a single
@racket[case-lambda] clause with multiple @racket[match*] clauses
within it.
}

