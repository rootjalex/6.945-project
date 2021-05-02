#|
TODO: define built-in types and associated wrappers here. Expand from what exists in type-resolver.scm
|#

(define string-type)
(define string-type?)
(receive (constructor predicate) (primitive-type 'string-type)
  (set! string-type constructor)
  (set! string-type? predicate))

(define-generic-procedure-handler annotate-expr
  (match-args string? any-object?)
  (lambda (expr env)
    (make-texpr (string-type) expr)))

(define-generic-procedure-handler program-constraints-1
  (match-args type-expression?
              (disjoin boolean? number? symbol? string?))
  (lambda (type expr)
    '()))

(define-generic-procedure-handler simplify-annotated-program-1
  (match-args type-expression?
              (disjoin boolean? number? symbol? string?))
  (lambda (type expr)
    expr))

(define pair-type)
(define pair-type?)
(receive (constructor predicate)
    (define-parametric-type-operator 'type:pair)
  (set! pair-type constructor)
  (set! pair-type? predicate))

(define vector-type)
(define vector-type?)
(receive (constructor predicate)
    (define-parametric-type-operator 'type:vector)
  (set! vector-type constructor)
  (set! vector-type? predicate))

