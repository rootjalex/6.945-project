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



(define (evaluate-cdr)
  (let ((car-type (type-variable 'car))
        (cdr-type (type-variable 'cdr)))
    (procedure-type (list (pair-type car-type cdr-type)) cdr-type)))

(define (evaluate-car)
  (let ((car-type (type-variable 'car))
        (cdr-type (type-variable 'cdr)))
    (procedure-type (list (pair-type car-type cdr-type)) car-type)))

(define (evaluate-vector-ref)
  (let ((vector-element (type-variable 'element))
        (cdr-type (type-variable 'cdr)))
    (procedure-type (list (pair-type car-type cdr-type)) car-type)))

(define (make-top-level-env-frame)
  (let ((binary-numerical
         (let ((v (numeric-type)))
           (procedure-type (list v v) v)))
        (binary-comparator
         (let ((v (numeric-type)))
           (procedure-type (list v v) (boolean-type)))))
    (list (cons '+ binary-numerical)
          (cons '- binary-numerical)
          (cons '* binary-numerical)
          (cons '/ binary-numerical)
          (cons '= binary-comparator)
          (cons '< binary-comparator)
          (cons '> binary-comparator)
          (cons '<= binary-comparator)
          (cons '>= binary-comparator)
          (cons 'car evaluate-car)
          (cons 'cdr evaluate-cdr))))