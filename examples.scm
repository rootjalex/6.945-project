; (load "/Users/alexanderroot/Classes/6.945/sdf/manager/load")
; (load "~/Documents/2021Spring/6.945/sdf/manager/load")
; (manage 'new 'unification)
(load "files")

;;; Parametric Types
(define id-example
  '(define id
     (lambda (x) x)))
;Value: id-example

(define id-infer
  (noisy-infer-program-types id-example))
;Value: id-infer

(pp (simplify-annotated-program id-infer))
#|
(begin (define id (lambda (x) (declare-type x (? type:86)) x))
       (declare-type id (type:procedure ((? type:86)) (? type:86))))
;Unspecified return value
|#

(define id-example
  '(begin
    (define id
      (lambda (x y) x))
    (define aj
      (id 2 3))
    (define em
      (id #t #f))))

(define id-infer
  (noisy-infer-program-types id-example))
;Value: id-infer

(pp (simplify-annotated-program id-infer))
#|
(begin (define id (lambda (x y) (declare-type x (? type:4)) (declare-type y (? y:3)) x))
       (declare-type id (type:procedure ((? type:4) (? y:3)) (? type:4)))
       (define aj (id 2 3))
       (declare-type aj (numeric-type))
       (define em (id #t #f))
       (declare-type em (boolean-type)))
;Unspecified return value
|#

;;; Built in Parametric Procedure
(define cdr-example
  '(define prog
     (lambda (l)
       (cdr l))))
;Value: cdr-example

(define em
  (noisy-infer-program-types cdr-example))
;Value: em

(pp (simplify-annotated-program em))
#|
(begin (define prog (lambda (l) (declare-type l (type:pair (? car:21) (? type:23))) (cdr l)))
       (declare-type prog (type:procedure ((type:pair (? car:21) (? type:23))) (? type:23))))
;Unspecified return value
|#



;;; Side Effects
(define double-effect-example
  '(begin
     (define foo
       (lambda (x y)
         (begin
           (set-cdr! x 4)
           (write-line y))))

     (define z (list 1 2 3))

     (foo z 4)))
;Value: double-effect-example

(pp (clean-infer-types-effects double-effect-example))
#|
(begin
 (define foo
   (lambda (x y)
     (declare-type x (type:pair (? car:7) (? cdr:6)))
     (declare-type y (? y:3))
     (declare-effects ((effect:write (cdr-of x) (numeric-type))))
     (set-cdr! x 4)
     (declare-effects ((effect:io (? y:3))))
     (write-line y)))
 (declare-type foo (type:procedure ((type:pair (? car:7) (? cdr:6)) (? y:3)) (? type:12)))
 (declare-effects
  ((effect:allocate (type:procedure ((type:pair (? car:7) (? cdr:6)) (? y:3)) (? type:12)))))
 (define z
   (begin (declare-effects ((effect:unknown))) (list 1 2 3)))
 (declare-type z (type:pair (? type:29) (? type:28)))
 (declare-effects ((effect:unknown) (effect:allocate (type:pair (? type:29) (? type:28)))))
 (declare-effects ((effect:write (cdr-of z) (numeric-type)) (effect:io ((numeric-type)))))
 (foo z 4))
;Unspecified return value
|#


;;; Parametric Types and Side Effects Combination
(define multiple-effects-example
  '(begin
     (define foo
       (lambda (x y)
         (begin
           (set-cdr! x 4)
           (write-line y))))
     (define z (list 1 2 3))
     (foo z 4)
     (foo z #t)))
;Value: multiple-effects-example

(pp (clean-infer-types-effects multiple-effects-example))
#|
(begin
 (define foo
   (lambda (x y)
     (declare-type x (type:pair (? car:40) (? cdr:39)))
     (declare-type y (? y:36))
     (declare-effects ((effect:write (cdr-of x) (numeric-type))))
     (set-cdr! x 4)
     (declare-effects ((effect:io (? y:36))))
     (write-line y)))
 (declare-type foo (type:procedure ((type:pair (? car:40) (? cdr:39)) (? y:36)) (? type:45)))
 (declare-effects
  ((effect:allocate (type:procedure ((type:pair (? car:40) (? cdr:39)) (? y:36)) (? type:45)))))
 (define z
   (begin (declare-effects ((effect:unknown))) (list 1 2 3)))
 (declare-type z (type:pair (? type:76) (? type:63)))
 (declare-effects ((effect:unknown) (effect:allocate (type:pair (? type:76) (? type:63)))))
 (declare-effects ((effect:write (cdr-of z) (numeric-type)) (effect:io ((numeric-type)))))
 (foo z 4)
 (declare-effects ((effect:write (cdr-of z) (numeric-type)) (effect:io ((boolean-type)))))
 (foo z #t))
;Unspecified return value
|#
