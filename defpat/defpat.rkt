#lang sweet-exp racket

provide defpat
        pat-lambda
        rename-out
          pat-lambda my-match-lambda

require
        only-in generic-bind ~define
        only-in (prefix-in ~ generic-bind) ~$
        for-syntax   racket/base
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     racket/list
                     for-syntax racket/base

module+ test
  require rackunit

define-syntax defpat
  lambda (stx)
    syntax-parse stx
      [(defpat (f:expr . arg-pats) body:expr ...+)
       #'(defpat f
           (pat-lambda arg-pats
             body ...))]
      [(defpat id:id expr:expr)
       #'(define id expr)]


begin-for-syntax
  define-syntax kw make-rename-transformer(#'keyword)
  ;; parse-arg-pats : Stx [#:i Natural] -> (Values Stx (Listof Stx))
  ;; interp:
  ;;   given the arg-pats from pat-lambda, produce two values
  ;;   the first value is the kw-formals for use in lambda,
  ;;   and the second value is a list of definitions to bind the
  ;;   pattern variables in the arg-pats
  ;; examples:
  ;;   > (parse-arg-pats #'())
  ;;   (values #'() (list))
  ;;   > (parse-arg-pats #'rest-id)
  ;;   (values #'%& (list #'(~define (~$ rest-id) %&)))
  ;;   > (parse-arg-pats #'(arg-id))
  ;;   (values #'(%1) (list #'(~define (~$ arg-id) %1)))
  ;;   > (parse-arg-pats #'(arg-id1 arg-id2))
  ;;   (values #'(%1 %2) (list #'(~define (~$ arg-id1) %1) #'(~define (~$ arg-id2) %2)))
  define-syntax-class arg+default
    [pattern (~and stx [arg default])
             #:when (equal? #\[ (syntax-property #'stx 'paren-shape))]
  define (parse-arg-pats stx #:i [i 1]) ;Note: this i has to be a number
    syntax-parse stx
      [() (values #'() (list))]
      [rest-id:id (with-syntax ([%& (datum->syntax stx '%& stx stx)])
                    (values #'%& (list #'(~define (~$ rest-id) %&))))]
      [(arg:expr . rst) (define-values (arg.arg arg.def)
                          (parse-arg-pat #'arg #:i i))
                        (define-values (rst.args rst.defs)
                          (parse-arg-pats #'rst #:i (add1 i)))
                        (values (quasisyntax/loc stx
                                  (#,arg.arg . #,rst.args))
                                (cons arg.def rst.defs))]
      [(kw:kw arg:expr . rst) (define-values (arg.arg arg.def)
                                (parse-arg-pat #'arg #:i (syntax-e #'kw)))
                              (define-values (rst.args rst.defs)
                                (parse-arg-pats #'rst #:i i)) ;don't add 1
                              (values (quasisyntax/loc stx
                                        (kw #,arg.arg . #,rst.args))
                                      (cons arg.def rst.defs))]
  ;
  ;; parse-arg-pat : Stx #:i (or/c Natural Keyword) -> (Values Stx Stx)
  define (parse-arg-pat stx #:i i) ;Note: this i can be either a number or a keyword
    syntax-parse stx
      [a+d:arg+default (define-values (a+d.arg.arg a+d.arg.def)
                         (parse-arg-pat #'a+d.arg #:i i))
                       (values (quasisyntax/loc stx
                                 [#,a+d.arg.arg a+d.default])
                               a+d.arg.def)]
      [arg-pat (with-syntax ([%i (datum->syntax stx (string->symbol (format "%~a" i)) stx stx)])
                 (values #'%i #'(~define (~$ arg-pat) %i)))]



define-syntax pat-lambda
  lambda (stx)
    syntax-parse stx
      [(pat-lambda arg-pats body:expr ...+)
       (define-values (args defs)
         (parse-arg-pats #'arg-pats))
       (with-syntax ([args args] [(def ...) defs])
         #'(lambda args
             def ...
             body ...))]



module+ test
  defpat (f0) "I am the result of f0"
  check-equal? (f0) "I am the result of f0"
  ;
  defpat (f1 x) x
  check-equal? (f1 "hello") "hello"
  ;
  defpat (f2 x y) x
  check-equal? (f2 "x" "y") "x"
  ;
  defpat (f3 (list x y)) x
  check-equal? (f3 '(x y)) 'x
  ;
  defpat (f4 [(list x y) '(x y)]) x
  check-equal? (f4) 'x
  check-equal? (f4 (list "x" "y")) "x"
  ;
  defpat (f5 [(list-no-order (list (or '#:m '#:mass) m)
                             (list (or '#:v '#:velocity) v))
              '([#:m 2] [#:v 1])])
    check-true (or (equal? (first (first %1)) '#:m)
                   (equal? (first (second %1)) '#:m)
                   (equal? (first (first %1)) '#:mass)
                   (equal? (first (second %1)) '#:mass))
    {1/2 * m * sqr(v)}
  check-equal? (f5) 1
  check-equal? (f5 '([#:m 4] [#:v 1])) 2
  check-equal? (f5 '([#:mass 6] [#:v 1])) 3
  check-equal? (f5 '([#:v 1] [#:m 2])) 1
  ;
  defpat (f6 #:m m #:v v)
    {1/2 * m * sqr(v)}
  check-equal? (f6 #:m 2 #:v 1) 1
  ;
  defpat (f7 #:m bla-bla-bla #:v more-bla-bla-bla)
    {1/2 * %#:m * sqr(%#:v)}
  check-equal? (f7 #:m 2 #:v 1) 1
  ;
  defpat (f8 #:arg [(list x y) '(x y)] . rst)
    check-equal? (first %#:arg) x
    check-equal? (second %#:arg) y
    check-equal? %& rst
    (cons x rst)
  check-equal? (f8) '(x)
  check-equal? (f8 'I '#:am '(the rest)) '(x I #:am (the rest))
  check-equal? (f8 #:arg '(#(I #:am (x)) #hash([I . #:am] [y . ()])) 'I '#:am '(the rest))
               '(#(I #:am (x)) I #:am (the rest))

