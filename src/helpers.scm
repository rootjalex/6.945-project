#|
Some useful helper functions that we've needed.
|#

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

