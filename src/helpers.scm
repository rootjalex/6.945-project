#|
Some useful helper functions that we've needed.
|#

(define (get-from-env name env constructor)
  (cdr (let loop ((env env))
         (or (assq name (car env))
             (if (pair? (cdr env))
                 (loop (cdr env))
                 (let ((tcell (constructor name)))
                   (set-car! env (cons tcell (car env)))
                   tcell))))))

(define (get-var-type name env)
  (let ((var-type (get-from-env name env make-type-cell)))
    (if (type-expression? var-type)
        var-type
        (evaluate-var-type var-type))))

(define (evaluate-var-type t)
  (guarantee procedure? t)
  (t))

#|
Corresponding versions for effects in an effectful env
|#

(define (make-default-effect-cell name)
  (cons name
        (lambda args
          (declare (ignore args))
          (list (effect:unknown)))))

(define (get-var-effect name env)
  (get-from-env name env make-default-effect-cell))

(define (add-effect-frame names env)
  (cons (map make-default-effect-cell names) env))

(define (insert-effects-ctor! name ctor env)
  (let ((cell (cons name ctor)))
    (set-car! env (cons cell (car env)))))
