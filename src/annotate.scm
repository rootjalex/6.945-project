#|
Annotation of expressions.
Different from the original in that side-effects are also annotated. 
|#

(define (et-annotate-program expr)
  (et-annotate-expr expr (top-level-env)))

; The third argument is the side-effects environment.
(define et-annotate-expr
  (simple-generic-procedure 'et-annotate-expr 3 #f))

(define-generic-procedure-handler et-annotate-expr
  (match-args boolean? any-object? any-object?)
  (lambda (expr env senv)
    (declare (ignore env))
    (declare (ignore senv))
    (make-pure-etexpr (boolean-type) expr)))

(define-generic-procedure-handler et-annotate-expr
  (match-args number? any-object? any-object?)
  (lambda (expr env senv)
    (declare (ignore env))
    (declare (ignore senv))
    (make-pure-etexpr (number-type) expr)))

(define-generic-procedure-handler et-annotate-expr
  (match-args symbol? any-object? any-object?)
  (lambda (expr env senv)
    (make-effectful-etexpr
     (get-var-type expr env)
     expr
     (get-effect-type expr senv))))

(define-generic-procedure-handler et-annotate-expr
  (match-args string? any-object?)
  (lambda (expr env senv)
    (declare (ignore env))
    (declare (ignore senv))
    (make-pure-etexpr (string-type) expr)))

; TODO: need to make `effect:union` and `effect:disjoint`
(define (make-if-effects predicate consequent alternative)
  (effect:union predicate (effect:disjoint consequent alternative)))

(define-generic-procedure-handler et-annotate-expr
  (match-args if-expr? any-object? any-object?)
  (lambda (expr env senv)
    (let ((predicate (et-annotate-expr (if-predicate expr) env senv))
          (consequent (et-annotate-expr (if-consequent expr) env senv))
          (alternative (et-annotate-expr (if-alternative expr) env senv)))
      (let ((pred-effect (etexpr-effect predicate))
            (cons-effect (etexpr-effect consequent))
            (alt-effect (etexpr-effect alternative)))
        (make-effectful-etexpr
         (type-variable)
         (make-if-expr predicate consequent alternative)
         (make-if-effects pred-effect cons-effect alt-effect))))))

(define-generic-procedure-handler et-annotate-expr
  (match-args lambda-expr? any-object? any-object?)
  (lambda (expr env senv)
    (let ((env* (new-frame (lambda-bvl expr) env)))
      (let ((arg-types (map (lambda (name) (get-var-type name env*)) (lambda-bvl expr)))
            (annotated-body (et-annotate-expr (lambda-body expr) env* senv)))
        (let ((proc-type (procedure-type arg-types (type-variable)))
              (proc-expr (make-lambda-expr (lambda-bvl expr) annotated-body))
              (proc-effect (etexpr-effect annotated-body)))
          ; TODO: Does this repeat effects....?
          (make-effectful-etexpr proc-type proc-expr proc-effect))))))


(define-generic-procedure-handler et-annotate-expr
  (match-args combination-expr? any-object? any-object?)
  (lambda (expr env senv)
    (error "AJ didn't understand what combinations are used for, so this isn't implemented yet" expr)))

(define-generic-procedure-handler et-annotate-expr
  (match-args define-expr? any-object? any-object?)
  (lambda (expr env senv)
    (let* ((name (define-name expr))
           (type (define-var-type name env))
           (effects (list (effect:allocate type)))
           ; TODO: need to add effects to senv, for recursive calls
           (expr (make-define-expr name (et-annotate-expr (define-value expr) env senv))))
      (make-effectful-etexpr type expr effects))))

(define-generic-procedure-handler et-annotate-expr
  (match-args begin-expr? any-object? any-object?)
  (lambda (expr env senv)
    (let* ((type (type-variable))
           ; TODO: does senv change at all here....?
           (parts (map (lambda (subexpr) (et-annotate-expr subexpr env senv)) (begin-exprs expr)))
           (effects (reduce effect:union (effect:pure) (map etexpr-effect parts)))
           (expr (make-begin-expr parts)))
      (make-effectful-etexpr type expr effects))))







