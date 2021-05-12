#|
Annotation of expressions.
Different from the original in that side-effects are also annotated. 
|#


; TODO: should this be the same environment? maybe not.
(define (label-effects texpr)
  (label-effects-expr texpr (top-level-env)))

(define label-effects-expr
  (simple-generic-procedure 'label-effects-expr 2 #f))

(define (texpr-pred type?)
  (lambda (texpr)
    (and (texpr? texpr)
         (type? (texpr-type texpr)))))

(define boolean-texpr? (texpr-pred boolean-type?))

#|
(boolean-texpr? (annotate-program '#t))
;Value: #t
|#

(define numeric-texpr? (texpr-pred numeric-type?))

#|
(boolean-texpr? (annotate-program '6))
;Value: #f

(numeric-texpr? (annotate-program '6))
;Value: #t
|#

(define procedure-texpr? (texpr-pred procedure-type?))

(define variable-texpr? (texpr-pred type-variable?))

(define (make-effect-wrapper texpr)
  (list 'effect-wrapper texpr))

(define (effect-wrapper? arg)
  (and (list? arg)
       (n:= 2 (length arg))
       (eqv? (car arg) 'effect-wrapper)))


(define-generic-procedure-handler label-effects-expr
  (match-args boolean-texpr? any-object?)
  (lambda (texpr env)
    (declare (ignore env))
    texpr))

(define-generic-procedure-handler label-effects-expr
  (match-args numeric-texpr? any-object?)
  (lambda (texpr env)
    (declare (ignore env))
    texpr))















(define (et-annotate-program expr)
  (et-annotate-expr expr (top-level-env) (top-level-env-effects)))

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
    (make-pure-etexpr (numeric-type) expr)))

(define-generic-procedure-handler et-annotate-expr
  (match-args symbol? any-object? any-object?)
  (lambda (expr env senv)
    (write-line expr)
    (let* ((type (get-var-type expr env))
           (effect (get-var-effect expr senv))
           (ctor (effect:get-ctor effect))
           (processing (effect:get-processing effect))
           (final-effects (apply ctor (processing (list type)))))
      (make-effectful-etexpr type expr final-effects))))

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
      (let ((pred-effect (etexpr-effects predicate))
            (cons-effect (etexpr-effects consequent))
            (alt-effect (etexpr-effects alternative)))
        (make-effectful-etexpr
         (type-variable)
         (make-if-expr predicate consequent alternative)
         (make-if-effects pred-effect cons-effect alt-effect))))))

(define-generic-procedure-handler et-annotate-expr
  (match-args lambda-expr? any-object? any-object?)
  (lambda (expr env senv)
    (let ((env* (new-frame (lambda-bvl expr) env))
          (senv* (new-effect-frame (lambda-bvl expr) senv)))
      (let ((arg-types (map (lambda (name) (get-var-type name env*)) (lambda-bvl expr)))
            (annotated-body (et-annotate-expr (lambda-body expr) env* senv*)))
        (let ((proc-type (procedure-type arg-types (type-variable)))
              (proc-expr (make-lambda-expr (lambda-bvl expr) annotated-body))
              (proc-effect (etexpr-effects annotated-body)))
          ; TODO: Does this repeat effects....?
          (make-effectful-etexpr proc-type proc-expr proc-effect))))))

(define-generic-procedure-handler et-annotate-expr
  (match-args combination-expr? any-object? any-object?)
  (lambda (expr env senv)
    (let* ((operator-name (combination-operator expr))
           (effect (get-var-effect operator-name senv))
           (type (type-variable))
           (et-operands (map (lambda (operand) (et-annotate-expr operand env senv)) (combination-operands expr)))
           (comb-expr (make-combination-expr (et-annotate-expr operator-name env senv) et-operands))
           (operand-types (map etexpr-type et-operands))
           (final-effects (apply (effect:get-ctor effect) ((effect:get-processing effect) operand-types)))
           (effects (effect:union* (cons final-effects (map etexpr-effects et-operands)))))
      (make-effectful-etexpr type comb-expr effects))))

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
           (effects (effect:union* (map etexpr-effects parts)))
           (lambda-expr (make-begin-expr parts)))
      (make-effectful-etexpr type lambda-expr effects))))




