#lang racket/base

(provide case-lambda/opt)

;; A version of case-lambda that allows optional arguments.

(require racket/local
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     ))

(module+ test
  (require rackunit))

(begin-for-syntax
  (define-syntax-class opt-case-lambda-clause
    #:attributes ([norm 1] [extra-def 1])
    [pattern (~and clause [(arg:id ...) body:expr ...+])
             #:with [norm ...] #'[clause]
             #:with [extra-def ...] #'[]]
    [pattern (~and clause [(arg:id ... . rest:id) body:expr ...+])
             #:with [norm ...] #'[clause]
             #:with [extra-def ...] #'[]]
    [pattern [(arg:id ... [opt-arg:id default:expr] ...+) body:expr ...+]
             #:with tmp-f:id (generate-temporary)
             #:with [extra-def ...] #'[(define (tmp-f arg ... [opt-arg default] ...) body ...)]
             #:with [norm ...]
             (for/list ([i (in-range (add1 (length (stx->list #'[opt-arg ...]))))])
               (define/syntax-parse [opt-arg* ...] (take (syntax->list #'[opt-arg ...]) i))
               #'[(arg ... opt-arg* ...) (tmp-f arg ... opt-arg* ...)])]
    [pattern [(arg:id ... [opt-arg:id default:expr] ...+ . rest:id) body:expr ...+]
             #:with tmp-f:id (generate-temporary)
             #:with [extra-def ...] #'[(define (tmp-f arg ... [opt-arg default] ... . rest) body ...)]
             #:with [norm ...]
             (cons
              #'[(arg ... opt-arg ... . rest) (apply tmp-f arg ... opt-arg ... rest)]
              (for/list ([i (in-range (length (stx->list #'[opt-arg ...])))])
                (define/syntax-parse [opt-arg* ...] (take (syntax->list #'[opt-arg ...]) i))
                #'[(arg ... opt-arg* ...) (tmp-f arg ... opt-arg* ...)]))]
    ))

(define-syntax case-lambda/opt
  (lambda (stx)
    (syntax-parse stx
      [(case-lambda/opt clause:opt-case-lambda-clause ...)
       #'(local [clause.extra-def ... ...]
           (case-lambda clause.norm ... ...))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (let ([f (case-lambda/opt [([a 5]) a])])
    (check-equal? (f) 5)
    (check-equal? (f 4) 4))
  (let ([f (case-lambda/opt [(a [b 5]) (list a b)])])
    (check-equal? (f 1) (list 1 5))
    (check-equal? (f 1 2) (list 1 2)))
  (let ([f (case-lambda/opt [(a) (list 'first-case a)]
                            [(a [b 5]) (list 'second-case a b)])])
    (check-equal? (f 1) (list 'first-case 1))
    (check-equal? (f 1 2) (list 'second-case 1 2)))
  (let ([f (case-lambda/opt [(a b) (list 'first-case a b)]
                            [(a [b 5]) (list 'second-case a b)])])
    (check-equal? (f 1) (list 'second-case 1 5))
    (check-equal? (f 1 2) (list 'first-case 1 2)))
  (let ([f (case-lambda/opt [([a 5] [b 6] . rst) (list a b rst)])])
    (check-equal? (f) (list 5 6 (list)))
    (check-equal? (f 1) (list 1 6 (list)))
    (check-equal? (f 1 2) (list 1 2 (list)))
    (check-equal? (f 1 2 3) (list 1 2 (list 3)))
    (check-equal? (f 1 2 3 4) (list 1 2 (list 3 4))))
  (let ([f (case-lambda/opt [(a b) (list 'first-case a b)]
                            [(a [b 5] . rst) (list 'second-case a b rst)])])
    (check-equal? (f 1) (list 'second-case 1 5 (list)))
    (check-equal? (f 1 2) (list 'first-case 1 2))
    (check-equal? (f 1 2 3) (list 'second-case 1 2 (list 3)))
    (check-equal? (f 1 2 3 4) (list 'second-case 1 2 (list 3 4))))
  )
