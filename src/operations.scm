#|
TODO: define built-in operations and associated wrappers here. Expand from what exists in type-resolver.scm
      re-define the interface for providing built-in operation's resultant types as a register! system
|#

(define top-level-env-frame '())

(define (make-top-level-env-frame)
  top-level-env-frame)

(define (register-primitive-op! op-name signature)
  (set! top-level-env-frame (cons (cons op-name signature) top-level-env-frame)))

; This is taken from types-resolver.scm's default env frame
(let ((binary-numerical
       (let ((v (numeric-type)))
         (procedure-type (list v v) v)))
      (binary-comparator
       (let ((v (numeric-type)))
         (procedure-type (list v v) (boolean-type)))))
  (register-primitive-op! '+ binary-numerical)
  (register-primitive-op! '- binary-numerical)
  (register-primitive-op! '* binary-numerical)
  (register-primitive-op! '/ binary-numerical)
  (register-primitive-op! '= binary-comparator)
  (register-primitive-op! '< binary-comparator)
  (register-primitive-op! '> binary-comparator)
  (register-primitive-op! '<= binary-comparator)
  (register-primitive-op! '>= binary-comparator))

(define (evaluate-cdr)
  (let ((car-type (type-variable 'car))
        (cdr-type (type-variable 'cdr)))
    (procedure-type (list (pair-type car-type cdr-type)) cdr-type)))
(register-primitive-op! 'cdr evaluate-cdr)

(define (evaluate-set-cdr!)
  (let ((car-type (type-variable 'car))
        (cdr-type (type-variable 'cdr))
        (cdr-type-new (type-variable 'cdr))
        (undef-type (type-variable 'undefined)))
    (procedure-type (list (pair-type car-type cdr-type) cdr-type-new) undef-type)))
(register-primitive-op! 'set-cdr! evaluate-set-cdr!)

(define (evaluate-car)
  (let ((car-type (type-variable 'car))
        (cdr-type (type-variable 'cdr)))
    (procedure-type (list (pair-type car-type cdr-type)) car-type)))
(register-primitive-op! 'car evaluate-car)
