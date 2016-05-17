#lang scribble/manual

@(require (for-label racket/base
                     defpat/opt-match-case-lambda
                     defpat/match-case-lambda
                     ))

@title{opt-match-case-lambda}

@defmodule[defpat/opt-match-case-lambda]

@defform[(match*-case-lambda/opt clause ...)
         #:grammar ([clause [args body ...+]]
                    [args (arg-pat ... [arg-pat default-expr] ...)
                          rest-id
                          (arg-pat ... [arg-pat default-expr] ... . rest-id)])]{
like @racket[match*-case-lambda], except that it supports optional arguments.
}

