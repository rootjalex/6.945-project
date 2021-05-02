#|
TODO: define built-in types and associated wrappers here. Expand from what exists in type-resolver.scm
|#

(define pair-type)
(define pair-type?)
(receive (constructor predicate)
    (define-parametric-type-operator 'type:pair)
  (set! pair-type constructor)
  (set! pair-type? predicate))


(define (evaluate-cdr)
  (let ((car-type (type-variable 'car))
        (cdr-type (type-variable 'cdr)))
    (procedure-type (list (pair-type car-type cdr-type)) cdr-type)))

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
          (cons 'cdr evaluate-cdr))))