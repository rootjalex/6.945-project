#|
TODO: define built-in types and associated wrappers here. Expand from what exists in type-resolver.scm
|#

(define pair-type)
(define pair-type?)
(receive (constructor predicate)
    (define-parametric-type-operator 'type:pair)
  (set! pair-type constructor)
  (set! pair-type? predicate))


(define (get-from-env name env)
  (cdr (let loop ((env env))
         (or (assq name (car env))
             (if (pair? (cdr env))
                 (loop (cdr env))
                 (let ((tcell (make-type-cell name)))
                   (set-car! env (cons tcell (car env)))
                   tcell))))))

(define (get-var-type name env)
  (let ((var-type (get-from-env name env)))
    (if (type-expression? var-type)
        var-type
        (evaluate-var-type var-type))))

(define (evaluate-var-type t)
  (guarantee procedure? t)
  (t))

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