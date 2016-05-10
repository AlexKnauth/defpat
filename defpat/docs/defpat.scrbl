#lang scribble/manual

@(require scribble-code-examples
          (for-label racket/base
                     racket/match
                     defpat/defpat
                     generic-bind))

@title{defpat}

source code: @url{https://github.com/AlexKnauth/defpat}

@defmodule[defpat/defpat]{
This module provides the forms @racket[defpat] and @racket[pat-lambda].

@racket[defpat] is a version of @racket[define] for functions where the
arguments can be @racket[match] patterns.  
@margin-note{see also @racket[define/match] from @racketmodname[racket/match] and
                      @racket[~define] from @racketmodname[generic-bind]}

@racket[pat-lambda] is a version of @racket[lambda] where (again) the
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
like @racket[define], except that each @racket[arg-pat] can be an
arbitrary @racket[match] pattern.

@code-examples[#:lang "racket/base" #:context #'here]{
(require defpat/defpat)
(defpat (distance (list x1 y1) (list x2 y2))
  (sqrt (+ (* (- x2 x1) (- x2 x1))
           (* (- y2 y1) (- y2 y1)))))
(distance (list 0 0) (list 3 4))
(distance (list 0 3) (list 4 0))
}

The @racket[arg-pat] can't start with a @litchar{[} though, because
square brackets are used to specify optional arguments:

@code-examples[#:lang "racket/base" #:context #'here]{
(require defpat/defpat)
(defpat (distance (list x1 y1) [(list x2 y2) (list 0 0)])
  ; if the second point is not specified, it computes the
  ; distance to the origin
  (sqrt (+ (* (- x2 x1) (- x2 x1))
           (* (- y2 y1) (- y2 y1)))))
(distance (list 0 3) (list 4 0))
(distance (list 3 4))
}

@racketblock[(defpat (head . args) body ...)]
expands to
@racketblock[(defpat head (pat-lambda args body ...))]
}

@defform[(pat-lambda kw-formals body ...+)
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
@racketblock[(pat-lambda ((list x y) (vector z))
               body)]
expands to
@racketblock[(lambda (%1 %2)
               (match-define (list x y) %1)
               (match-define (vector z) %2)
               body)]
and for keyword-arguments,
@racketblock[(pat-lambda (#:kw (list x y))
               body)]
expands to
@racketblock[(lambda (#:kw %#:kw)
               (match-define (list x y) %#:kw)
               body)]
}

