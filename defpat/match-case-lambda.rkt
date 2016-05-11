#lang racket

(provide match*-case-lambda)

(require (for-syntax racket/base
                     racket/list
                     syntax/parse
                     unstable/list
                     ))

(begin-for-syntax
  (define-syntax-class match*-clause
    #:attributes ([pat 1] [body 1] [id 1] args n)
    ;; The n attribute is the procedure-arity for this clause, and
    ;; it's used to group clauses with the same arity together.
    ;; The pat and id lists should have the same length, because each
    ;; id will be matched against the corresponding pat.
    ;; The args attribute will be used directly in the case-lambda, so
    ;; each id should be bound by something in args.
    ;; The body attribute will be used as the body of a match* clause,
    ;; in the context of all of the pats, but not necessarily all of
    ;; the ids. 
    [pattern [[pat:expr ...] body ...+]
      #:with [id:id ...] (generate-temporaries #'(pat ...))
      #:with args #'(id ...)
      #:attr n (length (syntax->list #'(pat ...)))]
    [pattern [[fst-pat:expr ... . rst:id] body ...+]
      #:with [pat ...] #'[fst-pat ... rst]
      #:with [fst-id:id ...] (generate-temporaries #'(fst-pat ...))
      #:with [id:id ...] #'[fst-id ... rst]
      #:with args #'(fst-id ... . rst)
      #:attr n (arity-at-least (length (syntax->list #'(fst-pat ...))))])
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

