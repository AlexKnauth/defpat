#lang racket/base

(provide match*-case-lambda/opt)

;; A version of match*-case-lambda that allows optional arguments.

(require racket/local
         defpat/defpat
         defpat/match-case-lambda
         unstable/match
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     ))

(module+ test
  (require rackunit))

(begin-for-syntax
  ;; brackets? : Syntax -> Boolean
  ;; Returns true if stx has square brackets on the outside, false otherwise.
  (define (brackets? stx)
    (equal? (syntax-property stx 'paren-shape) #\[))
  
  (define-syntax-class pat
    #:attributes (pat)
    [pattern pat #:when (not (brackets? #'pat))])
  (define-syntax-class opt
    #:attributes (pat default)
    [pattern (~and stx [pat default:expr]) #:when (brackets? #'stx)])
  
  (define-syntax-class opt-case-lambda-clause
    #:attributes ([norm 1] [extra-def 1])
    [pattern [(arg:pat ... opt-arg:opt ...) body:expr ...+]
             #:with tmp-f:id (generate-temporary)
             #:with [tmp-arg:id ...] (generate-temporaries #'[arg ... ])
             #:with [tmp-opt-arg:id ...] (generate-temporaries #'[opt-arg ...])
             #:with [extra-def ...] #'[(defpat (tmp-f arg ... opt-arg ...) body ...)]
             #:with [norm ...]
             (for/list ([i (in-range (add1 (length (stx->list #'[tmp-opt-arg ...]))))])
               (define/syntax-parse [tmp-opt-arg* ...] (take (syntax->list #'[tmp-opt-arg ...]) i))
               (define/syntax-parse [opt-arg-pat* ...] (take (syntax->list #'[opt-arg.pat ...]) i))
               #'[(tmp-arg ... tmp-opt-arg* ...)
                  #:when (match*? [tmp-arg ... tmp-opt-arg* ...] [arg.pat ... opt-arg-pat* ...])
                  (tmp-f tmp-arg ... tmp-opt-arg* ...)])]
    [pattern [(arg:pat ... opt-arg:opt ... . rest:id) body:expr ...+]
             #:with tmp-f:id (generate-temporary)
             #:with [tmp-arg:id ...] (generate-temporaries #'[arg ... ])
             #:with [tmp-opt-arg:id ...] (generate-temporaries #'[opt-arg ...])
             #:with tmp-rest:id (generate-temporary #'rest)
             #:with [extra-def ...] #'[(defpat (tmp-f arg ... opt-arg ... . rest) body ...)]
             #:with [norm ...]
             (cons
              #'[(tmp-arg ... tmp-opt-arg ... . tmp-rest)
                 #:when (match*? [tmp-arg ... tmp-opt-arg ... tmp-rest]
                                 [arg.pat ... opt-arg.pat ... rest])
                 (apply tmp-f tmp-arg ... tmp-opt-arg ... tmp-rest)]
              (for/list ([i (in-range (length (stx->list #'[tmp-opt-arg ...])))])
                (define/syntax-parse [tmp-opt-arg* ...] (take (syntax->list #'[tmp-opt-arg ...]) i))
                (define/syntax-parse [opt-arg-pat* ...] (take (syntax->list #'[opt-arg.pat ...]) i))
               #'[(tmp-arg ... tmp-opt-arg* ...)
                  #:when (match*? [tmp-arg ... tmp-opt-arg* ...] [arg.pat ... opt-arg-pat* ...])
                  (tmp-f tmp-arg ... tmp-opt-arg* ...)]))]
    ))

(define-syntax match*-case-lambda/opt
  (lambda (stx)
    (syntax-parse stx
      [(match*-case-lambda/opt clause:opt-case-lambda-clause ...)
       #'(local [clause.extra-def ... ...]
           (match*-case-lambda clause.norm ... ...))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (let ([f (match*-case-lambda/opt [([a 5]) a])])
    (check-equal? (f) 5)
    (check-equal? (f 4) 4))
  (let ([f (match*-case-lambda/opt [(a [b 5]) (list a b)])])
    (check-equal? (f 1) (list 1 5))
    (check-equal? (f 1 2) (list 1 2)))
  (let ([f (match*-case-lambda/opt [(a) (list 'first-case a)]
                                   [(a [b 5]) (list 'second-case a b)])])
    (check-equal? (f 1) (list 'first-case 1))
    (check-equal? (f 1 2) (list 'second-case 1 2)))
  (let ([f (match*-case-lambda/opt [(a b) (list 'first-case a b)]
                                   [(a [b 5]) (list 'second-case a b)])])
    (check-equal? (f 1) (list 'second-case 1 5))
    (check-equal? (f 1 2) (list 'first-case 1 2)))
  (let ([f (match*-case-lambda/opt [([a 5] [b 6] . rst) (list a b rst)])])
    (check-equal? (f) (list 5 6 (list)))
    (check-equal? (f 1) (list 1 6 (list)))
    (check-equal? (f 1 2) (list 1 2 (list)))
    (check-equal? (f 1 2 3) (list 1 2 (list 3)))
    (check-equal? (f 1 2 3 4) (list 1 2 (list 3 4))))
  (let ([f (match*-case-lambda/opt [(a b) (list 'first-case a b)]
                                   [(a [b 5] . rst) (list 'second-case a b rst)])])
    (check-equal? (f 1) (list 'second-case 1 5 (list)))
    (check-equal? (f 1 2) (list 'first-case 1 2))
    (check-equal? (f 1 2 3) (list 'second-case 1 2 (list 3)))
    (check-equal? (f 1 2 3 4) (list 'second-case 1 2 (list 3 4))))
  (let ([f (match*-case-lambda/opt [((list a)) a])])
    (check-equal? (f (list 1)) 1)
    (check-equal? (f (list 2)) 2))
  (let ([f (match*-case-lambda/opt [((list a)) a]
                                   [((vector a)) a])])
    (check-equal? (f (list 1)) 1)
    (check-equal? (f (vector 1)) 1))
  (let ([f (match*-case-lambda/opt [([(list a) (list 5)]) a])])
    (check-equal? (f) 5)
    (check-equal? (f (list 1)) 1))
  (let ([f (match*-case-lambda/opt [((list a) [(list b) (list 5)]) (list a b)]
                                   [(a) (list a)]
                                   [(a (vector b)) (list a b)]
                                   [(a b) (list a b)])])
    (check-equal? (f 0) (list 0))
    (check-equal? (f (list 0)) (list 0 5))
    (check-equal? (f (list 0) (list 1)) (list 0 1))
    (check-equal? (f (list 0) 1) (list (list 0) 1))
    (check-equal? (f 0 (vector 1)) (list 0 1))
    (check-equal? (f 0 (list 1)) (list 0 (list 1))))
  )
