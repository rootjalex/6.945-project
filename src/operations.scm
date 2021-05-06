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

(define (evaluate-car)
  (let ((car-type (type-variable 'car))
        (cdr-type (type-variable 'cdr)))
    (procedure-type (list (pair-type car-type cdr-type)) car-type)))
(register-primitive-op! 'car evaluate-car)


(define top-level-effects-frame '())

(define (make-top-level-effects-frame)
  top-level-effects-frame)

(define (top-level-env-effects)
  (list (make-top-level-effects-frame)))

(define (register-primitive-effect! op-name effect-ctor processing)
  (set! top-level-effects-frame (cons (list op-name effect-ctor processing) top-level-effects-frame)))

; Used to not include arg types in pure effects
(define (disclude-args . args)
  (declare (ignore args))
  '())

; processing technique for most non-pure effects
(define (no-op . args)
  args)

(define (register-pure-primitive! name)
  (register-primitive-effect! name effect:pure disclude-args))

(register-pure-primitive! '+)
(register-pure-primitive! '-)
(register-pure-primitive! '*)
(register-pure-primitive! '/)
(register-pure-primitive! '=)
(register-pure-primitive! '>)
(register-pure-primitive! '<)
(register-pure-primitive! '<=)
(register-pure-primitive! '>=)
(register-pure-primitive! 'car)
(register-pure-primitive! 'cdr)
