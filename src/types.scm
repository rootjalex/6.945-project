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

