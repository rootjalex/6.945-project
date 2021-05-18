#|
Definitions of the necessary side-effects for annotation, and a registration system for labeling built-ins with side-effects.
Another file will handle the analysis.
|#

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


#|
Some example effects handlers, for use in examples.scm
|#
(define (effects-handler:write-line . targs)
  (list (effect:io (texpr-type (car targs)))))
(register-primitive-effect! 'write-line effects-handler:write-line)

(define (effects-handler:set-cdr! . targs)
  (if (n:= 2 (length targs))
      ; effect:write (location) (type-written)
      (list (effect:write (list 'cdr-of (texpr-expr (car targs))) (texpr-type (cadr targs))))
      (error "set-cdr! used without only two arguments" targs)))
(register-primitive-effect! 'set-cdr! effects-handler:set-cdr!)

(define (effects-handler:+ . targs)
  (declare (ignore targs))
  (list (effect:pure)))
(register-primitive-effect! '+ effects-handler:+)

