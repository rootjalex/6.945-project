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

; unclear where these should go.

(define (make-default-effect-cell name)
  (list name (lambda () (effect:unknown)) disclude-args))

(define (get-var-effect name env)
  (get-from-env name env make-default-effect-cell))

(define (make-pure-cell name)
  (list name effect:simple-pure disclude-args))

(define (new-effect-frame names env)
  (cons (map make-pure-cell names) env))

;;;; Effectful typed expressions

(define (make-pure-etexpr type expr)
  `(et ,type ,expr ,(list (effect:pure))))

(define (make-effectful-etexpr type expr effects)
  `(et ,type ,expr ,effects))

(define (etexpr? object)
  (and (list? object)
       (= (length object) 4)
       (eq? (car object) 'et)
       (type-expression? (cadr object))))

(define (etexpr-type expr)
  (cadr expr))

(define (etexpr-expr expr)
  (caddr expr))

(define (etexpr-effects expr)
  (cadddr expr))