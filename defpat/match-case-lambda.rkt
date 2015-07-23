#lang racket

(provide match*-case-lambda)

(require (for-syntax racket/base
                     racket/list
                     syntax/parse
                     unstable/list
                     ))

(begin-for-syntax
  (define-syntax-class match*-clause
    #:attributes ([pat 1] [body 1] [id 1] args n) ; in is a procedure-arity
    [pattern [[pat:expr ...] body ...+]
      #:with [id:id ...] (generate-temporaries #'(pat ...))
      #:with args #'(id ...)
      #:attr n (length (syntax->list #'(pat ...)))]
    [pattern [[fst-pat:expr ... . rst:id] body ...+]
      #:with [pat ...] #'[fst-pat ... rst]
      #:with [id:id ...] (generate-temporaries #'(pat ...))
      #:with args #'(id ... . rst)
      #:with n (arity-at-least (length (syntax->list #'(fst-pat ...))))])
  (define (clause-n clause)
    (syntax-parse clause
      [:match*-clause (attribute n)]))
  (define (group-clauses clauses)
    (group-by clause-n clauses))
  )

(define-syntax match*-case-lambda
  (syntax-parser
    [(match*-case-lambda clause:match*-clause ...)
     #:with [case-lambda-clause ...]
     (for/list ([group (in-list (group-clauses (syntax->list #'(clause ...))))])
       (define/syntax-parse fst:match*-clause (first group))
       (define/syntax-parse [clause:match*-clause ...] group)
       #'[fst.args
          (match* [fst.id ...]
            [[clause.pat ...] clause.body ...]
            ...)])
     #'(case-lambda case-lambda-clause ...)]))

