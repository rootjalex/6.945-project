


(define (make-effect name)
  (lambda args
    (cons name args)))

(define (make-effect? name)
  (lambda (arg)
    (and (list? arg)
         (n:> 0 (length arg))
         (eqv? name (car arg)))))
  
(define effect:write (make-effect 'effect:write))
(define effect:write? (make-effect? 'effect:write))

(define effect:io (make-effect 'effect:io))
(define effect:io? (make-effect? 'effect:io))

(define effect:pure (make-effect 'effect:pure))
(define effect:pure? (make-effect? 'effect:pure))

(define effect:unknown (make-effect 'effect:unknown))
(define effect:unknown? (make-effect? 'effect:unknown))

(define (make-write-line-handler)
  (lambda targs
    (effect:io (map texpr-type targs))))

(define top-level-effects-frame '())

(define (make-top-level-effects-frame)
  top-level-effects-frame)

(define (top-level-env-effects)
  (list (make-top-level-effects-frame)))

(define (register-primitive-effect! op ctor)
  (set! top-level-effects-frame (cons (cons op ctor) top-level-effects-frame)))

(register-primitive-effect! 'write-line (make-write-line-handler))

(define (effect-annotate-partial texpr env)
  (effect-annotate-expr (texpr-type texpr) (texpr-expr texpr) env))

(define (effect-annotate-program texpr)
  (effect-annotate-partial texpr (top-level-env-effects)))

(define effect-annotate-expr
  (simple-generic-procedure 'effect-annotate-expr 3 #f))

(define (make-effectful type expr effects)
  (list 'effectful type expr effects))

(define-generic-procedure-handler effect-annotate-expr
  (match-args type-expression? (disjoin boolean? number? symbol?) any-object?)
  (lambda (type expr env)
    (declare (ignore env))
    (make-effectful type expr '())))

(define (effectful:get-effects effectful)
  (cadddr effectful))

(define (effects:union e0 e1)
  (lset-union equal? e0 e1))

(define-generic-procedure-handler effect-annotate-expr
  (match-args type-expression? if-expr? any-object?)
  (lambda (type expr env)
    (let ((annotated-pred (effect-annotate-partial (if-predicate expr) env))
          (annotated-cons (effect-annotate-partial (if-consequent expr) env))
          (annotated-altn (effect-annotate-partial (if-alternative expr) env)))
      (let* ((pred-effect (effectful:get-effects annotated-pred))
             (cons-effect (effectful:get-effects annotated-pred))
             (altn-effect (effectful:get-effects annotatedd-altn))
             (effects (effects:union (effects:union cons-effect altn-effect) pred-effect)))
        (make-effectful type (make-if-expr annotated-pred annotated-cons annotated-altn) effects)))))

(define (make-default-effect-cell name)
  (cons name
        (lambda args
          (declare (ignore args))
          (effect:unknown))))

(define (make-default-effect-cell name)
  (cons name
        (lambda args
          (declare (ignore args))
          (effect:unknown))))

(define (get-var-effect name env)
  (get-from-env name env make-default-effect-cell))

(define (add-effect-frame names env)
  (cons (map make-default-effect-cell names) env))

(define-generic-procedure-handler effect-annotate-expr
  (match-args type-expression? lambda-expr? any-object?)
  (lambda (type expr env)
    (let* ((env* (add-effect-frame (lambda-bvl expr) env))
           (body (effect-annotate-partial (lambda-body expr) env*)))
      ; TODO: should this just be an allocate? or a union of allocate and the body effects? somthing?
      (make-effectful type (make-lambda-expr (lambda-bvl expr) body) (effectful:get-effects body)))))

(define-generic-procedure-handler effect-annotate-expr
  (match-args type-expression? combination-expr? any-object?)
  (lambda (type expr env)
    (let* ((operator (combination-operator expr))
           (eval-op (effect-annotate-partial operator env))
           (effects-ctor (if (symbol? (texpr-expr operator))
                             (get-var-effect (texpr-expr operator) env)
                             (effectful:get-effects eval-op)))
           (operands (map (lambda (op) (effect-annotate-partial op env)) (combination-operands expr))))
      ; TODO: well uhhh what exactly should this do?
      (make-effectful type (make-combination-expr eval-op operands) (apply effects-ctor (combination-operands expr))))))

(define-generic-procedure-handler effect-annotate-expr
  (match-args type-expression? define-expr? any-object?)
  (lambda (type expr env)
    ; TODO: this is the hard part, we need to create the effect constructor
    (let ((body (effect-annotate-partial (define-value expr) env)))
      ; TODO: uhhh, should this remove the effects on the body?
      (make-effectful type (make-define-expr (define-name expr) body) (effectful:get-effects body)))))

; TODO: begin-expr?
; TODO: need to actually recursively construct, this crap discards...
