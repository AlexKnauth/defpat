#lang scribble/manual

@(require (for-label racket/base
                     defpat/opt-case-lambda
                     ))

@title{opt-case-lambda}

@defmodule[defpat/opt-case-lambda]

@defform[(case-lambda/opt clause ...)
         #:grammar ([clause [args body ...+]]
                    [args (arg-id ... [arg-id default-expr] ...)
                          rest-id
                          (arg-id ... [arg-id default-expr] ... . rest-id)])]{
like @racket[case-lambda], except that it supports optional arguments.
}

