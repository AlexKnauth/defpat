#lang scribble/manual

@(require (for-label racket/base
                     racket/match
                     defpat/defpat
                     generic-bind))

@title{defpat}

@defmodule[defpat/defpat]{
This module provides the forms @racket[defpat] and @racket[my-match-lambda].

@racket[defpat] is a version of @racket[define] for functions where the
arguments can be @racket[match] patterns.  
@margin-note{see also @racket[define/match] from @racketmodname[racket/match] and
                      @racket[~define] from @racketmodname[generic-bind]}

@racket[my-match-lambda] is a version of @racket[lambda] where (again) the
arguments can be @racket[match] patterns.
@margin-note{see also @racket[match-lambda], @racket[match-lambda*], and @racket[match-lambda**]
                      from @racketmodname[racket/match], and
                      @racket[~lambda] from @racketmodname[generic-bind]}
}

@defform*[[(defpat id expr)
           (defpat (head args) body ...+)]
          #:grammar ([head id
                           (head args)]
                     [args (code:line arg ...)
                           (code:line arg ... @#,racketparenfont{.} rest-id)]
                     [arg (code:line arg-pat)
                          (code:line [arg-pat default-expr])
                          (code:line keyword arg-pat)
                          (code:line keyword [arg-pat default-expr])])]{
like @racket[define], except that each @racket[arg-pat] can be an arbitrary @racket[match] pattern.

The @racket[arg-pat] can't start with a @litchar{[} though, otherwise it couldn't tell between
@racket[arg-pat] and @racket[[arg-pat default-expr]].

Also, you have to use square brackets to specify an optional argument (unlike @racket[define] and
@racket[lambda]).

@racketblock[(defpat (head . args) body ...)]
expands to
@racketblock[(defpat head (my-match-lambda args body ...))]
}

@defform[(my-match-lambda kw-formals body ...+)
         #:grammar ([kw-formals (arg ...)
                                (arg ...+ . rest-id)
                                rest-id]
                    [arg (code:line arg-pat)
                         (code:line [arg-pat default-expr])
                         (code:line keyword arg-pat)
                         (code:line keyword [arg-pat default-expr])])]{
like @racket[lambda], except that each @racket[arg-pat] can be an arbitrary @racket[match] pattern.
@margin-note*{Just as with @racket[defpat], the @racket[arg-pat] can't start with a @litchar{[}, and
              you have to use square brackets to specify an optional argument}

It is very similar to @racket[match-lambda**], except that it doesn't support multiple clauses, and
it allows optional arguments, keyword arguments, and a rest argument.  

As an example,
@racketblock[(my-match-lambda ((list x y) (vector z))
               body)]
expands to
@racketblock[(lambda (%1 %2)
               (match-define (list x y) %1)
               (match-define (vector z) %2)
               body)]
and for keyword-arguments,
@racketblock[(my-match-lambda (#:kw (list x y))
               body)]
expands to
@racketblock[(lambda (#:kw %#:kw)
               (match-define (list x y) %#:kw)
               body)]
}

