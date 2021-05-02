#|
Annotation of expressions.
Different from the original in that side-effects are also annotated. 
|#

(define (annotate-program expr)
  (annotate-expr expr (top-level-env)))

; The third argument is the side-effects environment.
(define annotate-expr
  (simple-generic-procedure 'annotate-expr 3 #f))

(define-generic-procedure-handler annotate-expr
  (match-args boolean? any-object? any-object?)
  (lambda (expr env senv)
    (declare (ignore env))
    (declare (ignore senv))
    (make-pure-texpr (boolean-type) expr)))

(define-generic-procedure-handler annotate-expr
  (match-args number? any-object? any-object?)
  (lambda (expr env senv)
    (declare (ignore env))
    (declare (ignore senv))
    (make-pure-texpr (number-type) expr)))

(define-generic-procedure-handler annotate-expr
  (match-args symbol? any-object? any-object?)
  (lambda (expr env senv)
    (make-effectful-texpr
     (get-var-type expr env)
     expr
     (get-effect-type expr senv))))







