#|
Methods for pretty-printing the annotated (types + effects) code
|#

(define (simplify-effectful-program eprog)
  (simplify-effectful-program-1 (effectful:get-type eprog)
                                (effectful:get-expr eprog)
                                (effectful:get-effects eprog)))

(define simplify-effectful-program-1
  (simple-generic-procedure 'simplify-effectful-program-1 3 #f))


(define-generic-procedure-handler simplify-effectful-program-1
  (match-args type-expression?
              (disjoin boolean? number? symbol?)
              list?)
  (lambda (type expr effects)
    expr))
 
(define-generic-procedure-handler simplify-effectful-program-1
  (match-args type-expression? if-expr? list?)
  (lambda (type expr effects)
    (declare (ignore type))
    (declare (ignore effects))
    (make-if-expr
     (simplify-effectful-program (if-predicate expr))
     (simplify-effectful-program (if-consequent expr))
     (simplify-effectful-program (if-alternative expr)))))

(define-generic-procedure-handler simplify-effectful-program-1
  (match-args procedure-type? lambda-expr? list?)
  (lambda (type expr effects)
    `(lambda ,(lambda-bvl expr)
       ,@(map declare-type-expr
              (lambda-bvl expr)
              (procedure-type-domains type))
       ,@(splice-begin
          (simplify-effectful-program (lambda-body expr))))))

(define-generic-procedure-handler simplify-effectful-program-1
  (match-args type-expression? combination-expr? list?)
  (lambda (type expr effects)
    (make-begin-expr
      (list
        (declare-effects effects)
        (make-combination-expr
          (simplify-effectful-program (combination-operator expr))
          (map simplify-effectful-program (combination-operands expr)))))))

(define (declare-effects effects)
  (list 'declare-effects effects))

(define-generic-procedure-handler simplify-effectful-program-1
  (match-args type-expression? define-expr? list?)
  (lambda (type expr effects)
    ; TODO: how should effects be displayed for define stmts...?
    (make-begin-expr
     (list
      (make-define-expr (define-name expr)
                        (simplify-effectful-program (define-value expr)))
      (declare-type-expr (define-name expr)
                         (texpr-type (define-value expr)))
      (declare-effects effects)))))

(define-generic-procedure-handler simplify-effectful-program-1
  (match-args type-expression? begin-expr? list?)
  (lambda (type expr effects)
    ; TODO: Should we declare all the effects for begins...?
    (make-begin-expr
     (append-map (lambda (x)
                   (splice-begin (simplify-effectful-program x)))
                 (begin-exprs expr)))))

(define (infer-program-types-effects expr)
  (let ((texpr (annotate-program expr)))
    (let ((constraints (program-constraints texpr)))
      (let ((all-constraints (handle-parametric-constraints constraints)))
        (let ((dict (unify-constraints all-constraints)))
          (let ((effectful-expr (effect-annotate-program texpr)))
            (if dict
                ((match:dict-substitution dict) effectful-expr)
                '***type-error***)))))))

(define (clean-infer-types-effects expr)
  (let ((effectful (infer-program-types-effects expr)))
    (simplify-effectful-program effectful)))
