#|
TODO: define the necessary side-effects for annotation, and create a registration system for labeling built-ins with side-effects
      another file will handle the analysis.

For testing, run:
(load "<dir>/sdf/manager/load")
(manage 'new 'unification)
|#

; Modelled after primitive-type set-up in type-resolver.scm
(define primitive-effects '())

(define (primitive-effect? object)
  (any (lambda (pred)
         (pred object))
       primitive-effects))

(define (primitive-effect name args-sat)

  (define (constructor . args)
    (guarantee args-sat args)
    (cons name args))

  (define (predicate object)
    (and (pair? object)
         (eq? name (car object))
         (args-sat (cdr object))))

  (guarantee symbol? name)
  (guarantee procedure? args-sat)
  (set! primitive-effects
        (cons predicate primitive-effects))
  (values constructor predicate))


; TODO: what makes the args satisfiable for effects? is this even useful?
(define (args-sat:n-types n)
  (define (n-types args)
    (and (n:= n (length args))
         (every type-expression? args)))
  n-types)

(define effect:io)
(define effect:io?)
(receive (constructor predicate) (primitive-effect 'effect:io (args-sat:n-types 1))
  (set! effect:io constructor)
  (set! effect:io? predicate))

#|
(effect:io? (effect:io (boolean-type)))
;Value: #t

(effect:io? (effect:io '(? l:2)))
;Value: #t
|#

; Write effect says what type it wrote to and what type it wrote.
(define effect:write)
(define effect:write?)
(receive (constructor predicate) (primitive-effect 'effect:write (args-sat:n-types 2))
  (set! effect:write constructor)
  (set! effect:write? predicate))

#|
(effect:write? (effect:write (boolean-type) (boolean-type)))
;Value: #t

(effect:write? (effect:write '(? l:2) (boolean-type)))
;Value: #t

(effect:io? (effect:write (boolean-type)))
;Value: #f

(effect:io? (effect:write (boolean-type) (boolean-type)))
;Value: #f
|#

; Pure / no effect takes no arguments.
(define effect:pure)
(define effect:pure?)
(receive (constructor predicate) (primitive-effect 'effect:pure null?)
  (set! effect:pure constructor)
  (set! effect:pure? predicate))

#|
(effect:pure? (effect:pure))
;Value: #t

(effect:pure? (effect:pure (boolean-type)))
;The object ((boolean-type)) is not an object satisfying #[compiled-procedure ("list" #x5) #x1c #x1141\
bf81c].
|#

; Parameter is the type of allocation
(define effect:allocate)
(define effect:allocate?)
(receive (constructor predicate) (primitive-effect 'effect:allocate (args-sat:n-types 1))
  (set! effect:allocate constructor)
  (set! effect:allocate? predicate))

; Instead of wrongfully labelling effect:pure as the default, effect:unknown should be the default.
(define effect:unknown)
(define effect:unknown?)
(receive (constructor predicate) (primitive-effect 'effect:unknown null?)
  (set! effect:unknown constructor)
  (set! effect:unknown? predicate))

(define (make-default-effect . args)
  (declare (ignore args))
  ; TODO: are args important for unknown? I think it would just be var name.
  (effect:unknown))


(define (strip-pure effects)
  (delete! (effect:pure) effects))

; helper functions for merging effects
(define (effect:union e1 e2)
  (let ((together (lset-union equal? e1 e2)))
    (if (n:> 1 (length together))
        (strip-pure together)
        together)))

; TODO: effect:disjoint


(define (effect:union* effects)
  (reduce effect:union (effect:pure) effects))