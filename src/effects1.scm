


(define (make-effect name)
  (lambda args
    (cons name args)))

(define (make-effect? name)
  (lambda (arg)
    (and (list? arg)
         (n:< 0 (length arg))
         (eqv? name (car arg)))))
  
(define effect:write (make-effect 'effect:write))
(define effect:write? (make-effect? 'effect:write))

(define effect:io (make-effect 'effect:io))
(define effect:io? (make-effect? 'effect:io))

(define effect:pure (make-effect 'effect:pure))
(define effect:pure? (make-effect? 'effect:pure))

(define effect:unknown (make-effect 'effect:unknown))
(define effect:unknown? (make-effect? 'effect:unknown))

(define effect:allocate (make-effect 'effect:allocate))
(define effect:allocate? (make-effect? 'effect:allocate))

(define top-level-effects-frame '())

(define (make-top-level-effects-frame)
  top-level-effects-frame)

(define (top-level-env-effects)
  (list (make-top-level-effects-frame)))

(define (register-primitive-effect! op ctor)
  (set! top-level-effects-frame (cons (cons op ctor) top-level-effects-frame)))

(define (make-write-line-handler)
  (lambda targs
    (list (effect:io (texpr-type (car targs))))))
(register-primitive-effect! 'write-line (make-write-line-handler))

(define (make-set-cdr!-handler)
  (lambda targs
    (if (n:= 2 (length targs))
        ; effect:write (location) (type-written)
        (list (effect:write (list 'cdr-of (texpr-expr (car targs))) (texpr-type (cadr targs))))
        (error "set-cdr! used without only two arguments" targs))))
(register-primitive-effect! 'set-cdr! (make-set-cdr!-handler))

(define (effect-annotate-partial texpr env)
  (effect-annotate-expr (texpr-type texpr) (texpr-expr texpr) env))

; this is the top-level thing we call
(define (effect-annotate-program texpr)
  (effect-annotate-partial texpr (top-level-env-effects)))

(define effect-annotate-expr
  (simple-generic-procedure 'effect-annotate-expr 3 #f))

(define (make-effectful type expr effects)
  (list 'effectful type expr effects))

(define (effectful? eff)
  (and (list? eff)
       (n:= 4 (length eff))
       (eqv? 'effectful (car eff))))

(define-generic-procedure-handler effect-annotate-expr
  (match-args type-expression? (disjoin boolean? number? symbol?) any-object?)
  (lambda (type expr env)
    (declare (ignore env))
    ; this is not exactly true for symbol, but the combination-expr has a special handler for that.
    (make-effectful type expr '())))

(define (effectful:get-effects effectful)
  (cadddr effectful))

(define (effectful:get-type effectful)
  (cadr effectful))

(define (effectful:get-expr effectful)
  (caddr effectful))

(define (effects:union e0 e1)
  (lset-union equal? e0 e1))

(define (effects:union* . args)
  (reduce effects:union '() args))

(define-generic-procedure-handler effect-annotate-expr
  (match-args type-expression? if-expr? any-object?)
  (lambda (type expr env)
    (let ((annotated-pred (effect-annotate-partial (if-predicate expr) env))
          (annotated-cons (effect-annotate-partial (if-consequent expr) env))
          (annotated-altn (effect-annotate-partial (if-alternative expr) env)))
      (let* ((pred-effect (effectful:get-effects annotated-pred))
             (cons-effect (effectful:get-effects annotated-pred))
             (altn-effect (effectful:get-effects annotatedd-altn))
             (effects (effects:union* cons-effect altn-effect pred-effect)))
        (make-effectful type (make-if-expr annotated-pred annotated-cons annotated-altn) effects)))))

(define (make-default-effect-cell name)
  (cons name
        (lambda args
          (declare (ignore args))
          (list (effect:unknown)))))

(define (get-from-env name env constructor)
  (cdr (let loop ((env env))
         (or (assq name (car env))
             (if (pair? (cdr env))
                 (loop (cdr env))
                 (let ((tcell (constructor name)))
                   (set-car! env (cons tcell (car env)))
                   tcell))))))

(define (get-var-effect name env)
  (get-from-env name env make-default-effect-cell))

(define (add-effect-frame names env)
  (cons (map make-default-effect-cell names) env))

(define (insert-effects-ctor! name ctor env)
  (let ((cell (cons name ctor)))
    (set-car! env (cons cell (car env)))))

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
                             ; TODO: this doesn't work for non-symbols I think? that's bad.
                             (error "We don't support non-symbol operators for combination expressions yet")))
           (operands (map (lambda (op) (effect-annotate-partial op env)) (combination-operands expr))))
      ; TODO: well uhhh what exactly should this do?
      (make-effectful type (make-combination-expr eval-op operands) (apply effects-ctor (combination-operands expr))))))

(define (lambda-texpr? texpr)
  (and (texpr? texpr)
       (lambda-expr? (texpr-expr texpr))))

(define (handle-lambda-definition name lambda-texpr lambda-effectful env)
  (let* ((lambda-expr (texpr-expr lambda-texpr))
         (lambda-type (texpr-type lambda-texpr))
         (lambda-args (lambda-bvl lambda-expr))
         (lambda-targs (procedure-type-domains lambda-type))
         (lambda-effects (effectful:get-effects lambda-effectful))
         (effects-ctor (create-effects-constructor lambda-args lambda-targs lambda-effects)))
    (insert-effects-ctor! name effects-ctor env)))

(define (construct-effect:allocate type)
  (list (effect:allocate type)))

(define (construct-effectful-define type expr body)
  (let ((define-expr (make-define-expr (define-name expr) body))
        (allocator (construct-effect:allocate (texpr-type (define-value expr)))))
    (if (lambda-texpr? (define-value expr))
        (make-effectful type define-expr allocator)
        (make-effectful type define-expr (effects:union allocator (effectful:get-effects body))))))

(define-generic-procedure-handler effect-annotate-expr
  (match-args type-expression? define-expr? any-object?)
  (lambda (type expr env)
    (let ((body (effect-annotate-partial (define-value expr) env)))
      (if (lambda-texpr? (define-value expr))
          (handle-lambda-definition (define-name expr) (define-value expr) body env))
      (construct-effectful-define type expr body))))

; this is map but sequential
(define (gather f l)
  (if (null? l)
      l
      (let* ((first (f (car l)))
             (rest (gather f (cdr l))))
        (cons first rest))))

(define-generic-procedure-handler effect-annotate-expr
  (match-args type-expression? begin-expr? any-object?)
  (lambda (type expr env)
    ; TODO: is there a way to force the map to be sequential..?
    (let* ((body (gather (lambda (texpr) (effect-annotate-partial texpr env)) (begin-exprs expr)))
           (new-expr (make-begin-expr body))
           (effects (apply effects:union* (map effectful:get-effects body))))
      (make-effectful type new-expr effects))))


(define (create-effects-constructor arg-names arg-types effects)
  (if (n:= (length arg-names) (length arg-types))
      (lambda inputs
        (let ((type-mapping (map cons arg-types (map texpr-type inputs)))
              (name-mapping (map cons arg-names (map texpr-expr inputs))))
          (if (n:= (length inputs) (length type-mapping))
              (construct-effects type-mapping name-mapping effects)
              (error "call to user-defined function has incorrect numbere of arguments"))))
      (error "create-effects-constructor received bad arguments" arg-names arg-types effects)))

(define (construct-effects tmap nmap effects)
  (let ((replacer (lambda (e) (construct-effect-replacement e tmap nmap))))
    (map replacer effects)))

(define construct-effect-replacement
  (simple-generic-procedure 'construct-effect-replacment 3 #f))

(define-generic-procedure-handler construct-effect-replacement
  (match-args effect:unknown? list? list?)
  (lambda (effect tmap nmap)
    effect))

(define-generic-procedure-handler construct-effect-replacement
  (match-args effect:pure? list? list?)
  (lambda (effect tmap nmap)
    effect))

(define-generic-procedure-handler construct-effect-replacement
  (match-args effect:io? list? list?)
  (lambda (effect tmap nmap)
    ; io just reports types
    (effect:io (replace-mapping (cdr effect) tmap))))

(define (replace-mapping l m)
  (let ((replacer
         (lambda (elem)
           (let ((found (assoc elem m)))
             (if found
                 (cdr found)
                 elem)))))
    (map replacer l)))

(define-generic-procedure-handler construct-effect-replacement
  (match-args effect:write? list? list?)
  (lambda (effect tmap nmap)
    ; write is of the form (effect:write (location) (type))
    ; where location is (name place)
    ; only placee and type should be modified, if possible
    (effect:write (replace-location (cadr effect) nmap)
                  (car (replace-mapping (cddr effect) tmap)))))

(define (replace-location location nmap)
  ; location is of the form (identifier place)
  ; we can only replace place
  (cons (car location) (replace-mapping (cdr location) nmap)))



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
  ; TODO: should symbols be included here???
  (lambda (type expr effects)
    expr))
 
(define-generic-procedure-handler simplify-effectful-program-1
  (match-args type-expression? if-expr? list?)
  (lambda (type expr effects)
    ; TODO: how should we annotate effects???
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
       ; TODO: need effect declarations
      ;  ,@(map declare-effects effects)
       ,@(splice-begin
          (simplify-effectful-program (lambda-body expr))))))

(define-generic-procedure-handler simplify-effectful-program-1
  (match-args type-expression? combination-expr? list?)
  (lambda (type expr effects)
    ; TODO: should effects be displayed here? it's difficult to say...
    ; ^ yes? since it's when the defined proc are used.. maybe have a check
    ; for when type is of the top env and don't print if it is unless
    ; we want effects for those 
    (make-begin-expr
      (list
        (make-combination-expr
          (simplify-effectful-program (combination-operator expr))
          (map simplify-effectful-program (combination-operands expr)))
        (declare-effects effects)))))


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
    ; TODO: also how do we declare effects for begins...
    (make-begin-expr
     (append-map (lambda (x)
                   (splice-begin (simplify-effectful-program x)))
                 (begin-exprs expr)))))




