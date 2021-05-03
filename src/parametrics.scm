

; (define stupid-map '())

; (define stupider-map '())

; (define (add-extra! t1 t2)
;   (let ((inside (assoc t1 stupid-map)))
;     (write-line (list "adding extrap..." t1 t2))
;     (if inside
;         (append! inside (list t2))
;         (set! stupid-map (cons (list t1 t2) stupid-map)))))

; (define (get-extra t)
;   (let ((inside (assoc t stupid-map)))
;     (if inside
;         (cdr inside)
;         (error "get-extra failed" t stupid-map))))

; (define (add-proc-type! proc type)
;   (guarantee (lambda (a) (not (assoc a stupider-map))) proc)
;   (write-line (list "adding..." proc type))
;   (set! stupider-map (cons (list proc type) stupider-map)))

; (define (get-proc-type proc)
;   (let ((alist (assoc proc stupider-map)))
;     (if alist
;         (cadr alist)
;         (error "get-proc-type failed" stupider-map proc))))

; (define-generic-procedure-handler annotate-expr
;   (match-args lambda-expr? any-object?)
;   (lambda (expr env)
;     (let* ((names (lambda-bvl expr))
;            (types (map type-variable names))
;            (frame (map cons names types))
;            (env* (cons frame env))
;            (return-type (type-variable))
;            (param-type
;             (lambda ()
;               (let ((new-types (map type-variable names))
;                     (new-return-type (type-variable)))
;                 (for-each
;                  (lambda (old new)
;                    (add-extra! old new))
;                  types new-types)
;                 (add-extra! return-type new-return-type)
;                 (procedure-type new-types new-return-type))))
;            (expr (make-lambda-expr names (annotate-expr (lambda-body expr) env*))))
;       (add-proc-type! param-type (procedure-type types return-type))
;       (make-texpr param-type expr))))

; ;; coderef: constrain-lambda
; ; (define-generic-procedure-handler program-constraints-1
; ;   (match-args procedure? lambda-expr?)
; ;   (lambda (type expr)
; ;     (cons (constrain (procedure-type-codomain type)
; ;                      (texpr-type (lambda-body expr)))
; ;           (program-constraints (lambda-body expr)))))

; (define (confirm-lengths arg-lists ret-list)
;   (let ((l (length ret-list)))
;     (if (every (lambda (args) (n:= (length args) l)) arg-lists)
;         #t
;         (error "confirm-lengths failed" l (map length arg-lists)))))

; ; (define-generic-procedure-handler program-constraints-1
; ;   (match-args procedure? lambda-expr?)
; ;   (lambda (type expr)
; ;     (let ((actual-type (get-proc-type type)))
; ;       (write-line actual-type)
; ;       (write-line (procedure-type? actual-type))
; ;       (write-line (parametric-type-operands actual-type))
; ;       (error "nice"))))

; ;; coderef: simplify-lambda
; ; (define-generic-procedure-handler simplify-annotated-program-1
; ;   (match-args procedure? lambda-expr?)
; ;   (lambda (type expr)
; ;     `(lambda ,(lambda-bvl expr)
; ;        ,@(map declare-type-expr
; ;               (lambda-bvl expr)
; ;               (procedure-type-domains type))
; ;        ,@(splice-begin
; ;           (simplify-annotated-program (lambda-body expr))))))

; (define-generic-procedure-handler program-constraints-1
;   (match-args procedure? lambda-expr?)
;   (lambda (type expr)
;     (let* ((actual-type (get-proc-type type))
;            (domains (procedure-type-domains actual-type))
;            (codomain (procedure-type-codomain actual-type))
;            (arg-types (map get-extra domains))
;            (ret-types (get-extra codomain))
;            (actual-constraints (cons (constrain (texpr-type (lambda-body expr)))
;                                      (program-constraints (lambda-body expr)))))
;       (confirm-lengths arg-types ret-types)
;       (write-line actual-constraints)
;       (write-line arg-types)
;       (write-line ret-types)
;       (error "nice"))))


#|
in `program-constraints-1` for combination-expr, we need to have the following schema:

if operator is type B
then construct B' = (procedure-type arg-types type)
and add command `DUPLICATE B B'`
where DUPLICATE B B' is:
  find connected components CC from B
  for all variables v in CC,
    v -> v' 
  duplicate each constraint in CC with the v' variables.
|#

(define (handle-parametric-constraints constraints)
  (receive (duplicates regulars) (partition parametric-constraint? constraints)
    (append regulars (construct-parametric-constraints duplicates regulars))))

(define (make-parametric-constraint id1 id2)
  (list 'PARAMETRIC-CONSTRAINT id1 id2))

(define (parametric-constraint? constraint)
  (and (list? constraint)
       (n:= 3 (length constraint))
       (eq? 'PARAMETRIC-CONSTRAINT (car constraint))))

(define (parametric-constraint-original constraint)
  (guarantee parametric-constraint? constraint)
  (cadr constraint))

(define (parametric-constraint-replacement constraint)
  (guarantee parametric-constraint? constraint)
  (caddr constraint))

#|
(parametric-constraint-original (make-parametric-constraint (boolean-type) '(? type:2)))
;Value: (boolean-type)

(parametric-constraint-replacement (make-parametric-constraint (boolean-type) '(? type:2)))
;Value: (? type:2)
|#

; TODO: I'm not sure if this is complete or not, what is the ordering...?
(define (gather-parametrics duplicates)
  (let ((alist '()))
    (define (loop dups)
      (if (null? dups)
          alist
          (let* ((dup (car dups))
                 (original (parametric-constraint-original dup))
                 (replacement (parametric-constraint-replacement dup))
                 (inside (assoc original alist)))
            (if inside
                (begin
                  (append! inside (list replacement))
                  (loop (cdr dups)))
                (begin
                  (set! alist (cons (list original replacement) alist))
                  (loop (cdr dups)))))))
    (loop duplicates)))

(define (construct-parametric-constraints duplicates regulars)
  (let ((alist-duplicates (gather-parametrics duplicates)))
    (define (loop alist accum)
      (if (null? alist)
          accum
          (loop (cdr alist) (append accum (add-parametric-constraints (car alist) regulars)))))
    (loop alist-duplicates '())))

(define (add-parametric-constraints mapping regulars)
  (let* ((original (car mapping))
         (related-constraints (find-related-constraints original regulars))
         (parametric-types (gather-all-parametric-types related-constraints)))
    (define (loop substs acc)
      (if (null? substs)
          acc
          (loop (cdr substs)
                (append acc
                        (construct-single-duplicate original (car substs) parametric-types related-constraints)))))
    (loop (cdr mapping) '())))

(define gather-parametric-types
  (simple-generic-procedure 'gather-parametric-types 1 #f))

; primitive types don't have sub-types
(define-generic-procedure-handler gather-parametric-types
  (match-args primitive-type?)
  (lambda (type)
    (declare (ignore type))
    '()))

; type-variables are there only related parametric types
(define-generic-procedure-handler gather-parametric-types
  (match-args type-variable?)
  (lambda (type)
    (list type)))

(define-generic-procedure-handler gather-parametric-types
  (match-args procedure-type?)
  (lambda (type)
    (define (arg-loop args acc)
      (if (null? args)
          acc
          (arg-loop (cdr args) (append acc (gather-parametric-types (car args))))))
    (append (gather-parametric-types (procedure-type-codomain type))
            (arg-loop (procedure-type-domains type) '()))))

; TODO: NEED TO HANDLE REGULAR, NON-PROCEDURAL PARAMETRIC TYPES!!!

(define (gather-single-types constraint)
  (append (gather-parametric-types (constraint-lhs constraint))
          (gather-parametric-types (constraint-rhs constraint))))

(define (gather-all-parametric-types constraints)
  (define (loop consts acc)
    (if (null? consts)
        acc
        (loop (cdr consts)
              (append acc (gather-single-types (car constraints))))))
  (loop constraints '()))

(define (construct-single-duplicate original replacement parametrics constraints)
  (delete! original parametrics equal?)
  (let ((parametric-replacements (map (lambda (type) (type-variable)) parametrics)))
    (substitute-constraints constraints (cons original parametrics) (cons replacement parametric-replacements))))

(define (find-related-constraints original constraints)
  (let ((constraint-types (map gather-single-types constraints)))
    (define (loop relevants consts const-types acc)
      (cond ((null? consts) acc)
            ((member (car consts) acc) (loop relevants (cdr consts) (cdr const-types) acc))
            (else
             ; There's gotta be a more efficient method for this, this is really a CC check which should be linear...
             (let* ((constraint (car consts))
                    (related-types (car const-types)))
               (define (inner-loop relvs)
                 (cond ((null? relvs) (loop relevants (cdr consts) (cdr const-types) acc))
                       ((member (car relvs) related-types)
                        ; Restart with new relevants
                        (loop (lset-union equal? relevants related-types) constraints constraint-types (cons constraint acc)))
                       (else 
                        (inner-loop (cdr relvs)))))
               (inner-loop relevants)))))
    (loop (list original) constraints constraint-types '())))


#|
; make sure it catches everything relevant
(find-related-constraints
 '(? type:1)
 (list
  (constrain '(? type:1) '(? t:2))
  (constrain '(? t:3) '(? t:4))
  (constrain '(? t:3) '(? t:5))
  (constrain '(? t:2) (procedure-type (list '(? t:3)) '(? t:6)))))
;Value: ((= (? t:3) (? t:5)) (= (? t:3) (? t:4)) (= (? t:2) (type:procedure ((? t:3)) (? t:6))) (= (? type:1) (? t:2)))

; make sure it doesn't catch irrelevant information
(find-related-constraints
 '(? type:1)
 (list
  (constrain '(? type:1) '(? t:2))
  (constrain '(? t:3) '(? t:4))
  (constrain '(? t:3) '(? t:5))
  (constrain '(? t:7) '(? t:8)) ; not supposed to be caught
  (constrain '(? t:2) (procedure-type (list '(? t:3)) '(? t:6)))))
;Value: ((= (? t:3) (? t:5)) (= (? t:3) (? t:4)) (= (? t:2) (type:procedure ((? t:3)) (? t:6))) (= (? type:1) (? t:2)))
|#


(define (substitute-constraints constraints originals replacements)
  (let ((alist (map cons originals replacements)))
    (map (lambda (constraint) (substitute-constraint constraint alist)) constraints)))

(define (substitute-constraint constraint mapping)
  (constrain (substitute-types (constraint-lhs constraint) mapping)
             (substitute-types (constraint-rhs constraint) mapping)))

(define substitute-types
  (simple-generic-procedure 'substitute-types 2 #f))

(define-generic-procedure-handler substitute-types
  (match-args primitive-type? alist?)
  (lambda (type mapping)
    (declare (ignore mapping))
    type))

(define-generic-procedure-handler substitute-types
  (match-args type-variable? alist?)
  (lambda (type mapping)
    (let ((rep (assoc type mapping)))
      (if rep
          (cdr rep)
          (error "All type variables should have a mapping" type mapping)))))

(define-generic-procedure-handler substitute-types
  (match-args procedure-type? alist?)
  (lambda (type mapping)
    (procedure-type (map (lambda (t) (substitute-types t mapping)) (procedure-type-domains type))
                    (substitute-types (procedure-type-codomain type) mapping))))

; TODO: NEED TO HANDLE REGULAR, NON-PROCEDURAL PARAMETRIC TYPES!!!

(define-generic-procedure-handler program-constraints-1
  (match-args type-expression? combination-expr?)
  (lambda (type expr)
    (let ((replacement (type-variable))
          (original (texpr-type (combination-operator expr)))
          (args (map texpr-type (combination-operands expr))))
      (cons (make-parametric-constraint original replacement)
            (cons (constrain replacement (procedure-type args type))
                  ; TODO: FIGURE OUT WHAT THIS IS DOING
                  (append (program-constraints (combination-operator expr))
                          (append-map program-constraints (combination-operands expr))))))))

(define (noisy-infer-program-types expr)
  (let ((texpr (annotate-program expr)))
    (pp (simplify-annotated-program texpr))
    (let ((constraints (program-constraints texpr)))
      (for-each (lambda (c) (pp c) (newline)) constraints)
      (let ((all-constraints (handle-parametric-constraints constraints)))
        (let ((dict (unify-constraints all-constraints)))
          (if dict
              ((match:dict-substitution dict) texpr)
              '***type-error***))))))


#|
(define id-example
  '(begin
     (define id
       (lambda (x) x))
     (id 2)
     (id #t)))
;Value: id-example

(define id-infer
  (noisy-infer-program-types id-example))
(begin (define id (lambda (x) (declare-type x (? x:45)) x))
       (declare-type id (type:procedure ((? x:45)) (? type:46)))
       (id 2)
       (id #t))
(= (? type:49) (? type:48))

(= (? id:44) (type:procedure ((? x:45)) (? type:46)))

(= (? type:46) (? x:45))

(parametric-constraint (? id:44) (? type:51))

(= (? type:51) (type:procedure ((numeric-type)) (? type:47)))

(parametric-constraint (? id:44) (? type:50))

(= (? type:50) (type:procedure ((boolean-type)) (? type:48)))

;Value: id-infer

id-infer
;Value: (t (boolean-type) (begin (t (type:procedure ((? type:46)) (? type:46)) (define id (t (type:procedure ((? type:46)) (? type:\
46)) (lambda (x) (t (? type:46) x))))) (t (numeric-type) ((t (type:procedure ((? type:46)) (? type:46)) id) (t (numeric-type) 2))) \
(t (boolean-type) ((t (type:procedure ((? type:46)) (? type:46)) id) (t (boolean-type) #t)))))

(pp (simplify-annotated-program id-infer))
(begin (define id (lambda (x) (declare-type x (? type:46)) x))
       (declare-type id (type:procedure ((? type:46)) (? type:46)))
       (id 2)
       (id #t))
;Unspecified return value



(define id-example
  '(begin
     (define id
       (lambda (x) x))
     (define aj
       (id 2))
     (define em
       (id #t))))

define id-infer
  (noisy-infer-program-types id-example))
(begin (define id (lambda (x) (declare-type x (? x:61)) x))
       (declare-type id (type:procedure ((? x:61)) (? type:62)))
       (define aj (id 2))
       (declare-type aj (? type:64))
       (define em (id #t))
       (declare-type em (? type:66)))
(= (? type:67) (? em:65))

(= (? id:60) (type:procedure ((? x:61)) (? type:62)))

(= (? type:62) (? x:61))

(= (? aj:63) (? type:64))

(parametric-constraint (? id:60) (? type:69))

(= (? type:69) (type:procedure ((numeric-type)) (? type:64)))

(= (? em:65) (? type:66))

(parametric-constraint (? id:60) (? type:68))

(= (? type:68) (type:procedure ((boolean-type)) (? type:66)))

;Value: id-infer

(pp (simplify-annotated-program id-infer))
(begin (define id (lambda (x) (declare-type x (? type:62)) x))
       (declare-type id (type:procedure ((? type:62)) (? type:62)))
       (define aj (id 2))
       (declare-type aj (numeric-type))
       (define em (id #t))
       (declare-type em (boolean-type)))
;Unspecified return value
|#


