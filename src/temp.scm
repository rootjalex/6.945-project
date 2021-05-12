; (load "/Users/alexanderroot/Classes/6.945/sdf/manager/load")
; (load "~/Documents/2021Spring/6.945/sdf/manager/load")
; (manage 'new 'unification)
; TODO: implement load-spec in files.scm (manage 'new 'type-inference)

(define fact-iterative
  '(define fact
     (lambda (n)
       (begin
         (define iter
           (lambda (product counter)
             (if (> counter n)
                 product
                 (iter (* product counter)
                       (+ counter 1)))))
         (iter 1 1)))))

(define foo
  (noisy-infer-program-types fact-iterative))
#|
(begin
 (define fact
   (lambda (n)
     (declare-type n (? n:2))
     (define iter
       (lambda (product counter)
         (declare-type product (? product:4))
         (declare-type counter (? counter:5))
         (if (> counter n)
             product
             (iter (* product counter) (+ counter 1)))))
     (declare-type iter (type:procedure ((? product:4) (? counter:5)) (? type:11)))
     (iter 1 1)))
 (declare-type fact (type:procedure ((? n:2)) (? type:14))))
(= (? fact:1) (type:procedure ((? n:2)) (? type:14)))
(= (? type:14) (? type:13))
(= (? type:13) (? type:12))
(= (? iter:3) (type:procedure ((? product:4) (? counter:5)) (? type:11)))
(= (? type:11) (? type:10))
(= (boolean-type) (? type:9))
(= (? type:10) (? product:4))
(= (? type:10) (? type:8))
(= (type:procedure ((numeric-type) (numeric-type)) (boolean-type)) (type:procedure ((? counter:5) (? n:2)) (? type:9)))
(= (? iter:3) (type:procedure ((? type:6) (? type:7)) (? type:8)))
(= (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) (type:procedure ((? product:4) (? counter:5)) (? type:6)))
(= (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) (type:procedure ((? counter:5) (numeric-type)) (? type:7)))
(= (? iter:3) (type:procedure ((numeric-type) (numeric-type)) (? type:12)))
;Value: foo
|#

(pp (simplify-annotated-program foo))
#|
(begin
 (define fact
   (lambda (n)
     (declare-type n (numeric-type))
     (define iter
       (lambda (product counter)
         (declare-type product (numeric-type))
         (declare-type counter (numeric-type))
         (if (> counter n)
             product
             (iter (* product counter) (+ counter 1)))))
     (declare-type iter (type:procedure ((numeric-type) (numeric-type)) (numeric-type)))
     (iter 1 1)))
 (declare-type fact (type:procedure ((numeric-type)) (numeric-type))))
;Unspecified return value
|#





(define car-wrapper
  '(define wrapper
     (lambda (a)
       (car a))))

(define bar
  (noisy-infer-program-types car-wrapper))
#|
(begin (define wrapper (lambda (a) (declare-type a (? a:16)) (car a))) (declare-type wrapper (type:procedure ((? a:16)) (? type:19))))
(= (? wrapper:15) (type:procedure ((? a:16)) (? type:19)))
(= (? type:19) (? type:18))
(= (? car:17) (type:procedure ((? a:16)) (? type:18)))
;Value: bar
|#

(pp (simplify-annotated-program bar))
#|
(begin
  (define wrapper
    (lambda (a)
      (declare-type a (? a:16))
      (car a)))
  (declare-type wrapper
                (type:procedure ((? a:16)) (? type:19))))
|#
;Unspecified return value

(define silly-program
  '(define prog
     (lambda (l)
       (begin
         (write-line l)
         (set-cdr! l 4)
         (write-line l)))))

(define fizz
  (noisy-infer-program-types silly-program))
#|
(begin (define prog (lambda (l) (declare-type l (? l:2)) (write-line l) (set-cdr! l 4) (write-line l))) (declare-type prog (type:procedure ((? l:2)) (? type:9))))
(= (? prog:1) (type:procedure ((? l:2)) (? type:9)))
(= (? type:9) (? type:8))
(= (? type:8) (? type:7))
(= (? write-line:3) (type:procedure ((? l:2)) (? type:4)))
(= (? set-cdr!:5) (type:procedure ((? l:2) (numeric-type)) (? type:6)))
(= (? write-line:3) (type:procedure ((? l:2)) (? type:7)))
;Value: fizz
|#


#|
(pp (simplify-annotated-program fizz))
(begin (define prog (lambda (l) (declare-type l (? l:2)) (write-line l) (set-cdr! l 4) (write-line l))) (declare-type prog (type:procedure ((? l:2)) (? type:9))))
;Unspecified return value
|#


(define redef-example
  '(define redef
     (lambda ()
       (begin

         (define em
           (+ 1 2))

         (define +
           (lambda (a b)
             (cons a b)))
         (define ajr
           (+ 1 2))))))



(define em
  (noisy-infer-program-types redef-example))
#|
(begin
 (define redef
   (lambda ()
     (define em
       (+ 1 2))
     (declare-type em (? type:34))
     (define +
       (lambda (a b)
         (declare-type a (? a:36))
         (declare-type b (? b:37))
         (cons a b)))
     (declare-type + (type:procedure ((? a:36) (? b:37)) (? type:40)))
     (define ajr
       (+ 1 2))
     (declare-type ajr (? type:42))))
 (declare-type redef (type:procedure () (? type:44))))
(= (? redef:32) (type:procedure () (? type:44)))
(= (? type:44) (? type:43))
(= (? type:43) (? ajr:41))
(= (? em:33) (? type:34))
(= (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) (type:procedure ((numeric-type) (numeric-type)) (? type:34)))
(= (? +:35) (type:procedure ((? a:36) (? b:37)) (? type:40)))
(= (? type:40) (? type:39))
(= (? cons:38) (type:procedure ((? a:36) (? b:37)) (? type:39)))
(= (? ajr:41) (? type:42))
(= (? +:35) (type:procedure ((numeric-type) (numeric-type)) (? type:42)))
;Value: em
|#

(define cdr-example
  '(define prog
     (lambda (l)
       (cdr l))))

(define em
  (noisy-infer-program-types cdr-example))
#|
(begin (define prog (lambda (l) (declare-type l (? l:92)) (cdr l))) (declare-type prog (type:procedure ((? l:92)) (? type:96))))
(= (? prog:91) (type:procedure ((? l:92)) (? type:96)))
(= (? type:96) (? type:95))
(= (type:procedure ((type:pair (? car:94) (? cdr:93))) (? cdr:93)) (type:procedure ((? l:92)) (? type:95)))
;Value: em
|#

(pp (simplify-annotated-program em))
#|
(begin (define prog (lambda (l) (declare-type l (type:pair (? car:94) (? type:96))) (cdr l)))
       (declare-type prog (type:procedure ((type:pair (? car:94) (? type:96))) (? type:96))))

|#
(define car-example
  '(define prog
     (lambda (l)
       (car l))))
;Value: car-example

(define em
  (noisy-infer-program-types car-example))
#|
(begin (define prog (lambda (l) (declare-type l (? l:7)) (car l)))
       (declare-type prog (type:procedure ((? l:7)) (? type:11))))
(= (? prog:6) (type:procedure ((? l:7)) (? type:11)))
(= (? type:11) (? type:10))
(= (type:procedure ((type:pair (? car:9) (? cdr:8))) (? car:9))
   (type:procedure ((? l:7)) (? type:10)))
;Value: em
|#
(pp (simplify-annotated-program em))
#|
(begin (define prog (lambda (l) (declare-type l (type:pair (? type:11) (? cdr:8))) (car l)))
       (declare-type prog (type:procedure ((type:pair (? type:11) (? cdr:8))) (? type:11))))
;Unspecified return value
|#

(define cdr-write-example
  '(define prog
     (lambda (l)
       (begin
         (write-line l)
         (cdr l)))))

(define em
  (noisy-infer-program-types cdr-write-example))
#|
(begin (define prog (lambda (l) (declare-type l (? l:113)) (write-line l) (cdr l)))
       (declare-type prog (type:procedure ((? l:113)) (? type:120))))
(= (? prog:112) (type:procedure ((? l:113)) (? type:120)))
(= (? type:120) (? type:119))
(= (? type:119) (? type:118))
(= (? write-line:114) (type:procedure ((? l:113)) (? type:115)))
(= (type:procedure ((type:pair (? car:117) (? cdr:116))) (? cdr:116))
   (type:procedure ((? l:113)) (? type:118)))
|#
(pp (simplify-annotated-program em))
#|
(begin
 (define prog
   (lambda (l)
     (declare-type l (type:pair (? car:117) (? type:120)))
     (write-line l)
     (cdr l)))
 (declare-type prog (type:procedure ((type:pair (? car:117) (? type:120))) (? type:120))))
;Unspecified return value
|#


(define string-example
  '(define str-ex
     "em"))


(define id-example
  '(begin
     (define id
       (lambda (x) x))
     (id 2)))

(define id-infer
  (noisy-infer-program-types id-example))
#|
(begin (define id (lambda (x) (declare-type x (? x:15)) x))
       (declare-type id (type:procedure ((? x:15)) (? type:16)))
       (id 2))
(= (? type:18) (? type:17))

(= (? id:14) (type:procedure ((? x:15)) (? type:16)))

(= (? type:16) (? x:15))

(= (? id:14) (type:procedure ((numeric-type)) (? type:17)))

;Value: id-infer
|#

(define id-example1
  '(define id
     (lambda (x) x)))



(define id-infer1
  (noisy-infer-program-types id-example1))
#|
(begin (define id (lambda (x) (declare-type x (? x:20)) x))
       (declare-type id (type:procedure ((? x:20)) (? type:21))))
(= (? id:19) (type:procedure ((? x:20)) (? type:21)))

(= (? type:21) (? x:20))

;Value: id-infer1
|#





#|
Parametrics example
TODO: USE THIS FOR DRAFT AND PRESENTATION

(manage 'new 'unification)

(define id-example
  '(begin
     (define id
       (lambda (x) x))
     (define aj
       (id 2))
     (define em
       (id #t))))
;Value: id-example

(define id-infer
  (infer-program-types id-example))
;Value: id-infer

id-infer
;Value: ***type-error***

(load "parametrics")
;Loading "parametrics.scm"... done
;Value: infer-program-types

(define id-infer
  (infer-program-types id-example))
;Value: id-infer

id-infer
;Value: (t (boolean-type) (begin (t (type:procedure ((? type:29)) (? type:29)) (define id (t (type:procedure ((? type:29)) (? type:\
29)) (lambda (x) (t (? type:29) x))))) (t (numeric-type) (define aj (t (numeric-type) ((t (type:procedure ((? type:29)) (? type:29)\
) id) (t (numeric-type) 2))))) (t (boolean-type) (define em (t (boolean-type) ((t (type:procedure ((? type:29)) (? type:29)) id) (t\
 (boolean-type) #t)))))))

(pp (simplify-annotated-program id-infer))
(begin (define id (lambda (x) (declare-type x (? type:29)) x))
       (declare-type id (type:procedure ((? type:29)) (? type:29)))
       (define aj (id 2))
       (declare-type aj (numeric-type))
       (define em (id #t))
       (declare-type em (boolean-type)))
;Unspecified return value

|#


#|
(define cdr-example
  '(begin
     (define foo
       (lambda (x)
         (cdr x)))
     (foo (cons 1 2))
     (foo (cons #t #f))))
;Value: cdr-example

(define cdr-infer
  (noisy-infer-program-types cdr-example))
(begin (define foo (lambda (x) (declare-type x (? x:14)) (cdr x))) (declare-type foo (type:procedure ((? x:14)) (? type:18))) (foo (cons 1 2)) (foo (cons #t #f)))
(= (? type:24) (? type:23))

(= (? foo:13) (type:procedure ((? x:14)) (? type:18)))

(= (? type:18) (? type:17))

(= (type:procedure ((type:pair (? car:16) (? cdr:15))) (? cdr:15)) (type:procedure ((? x:14)) (? type:17)))

(= (? foo:13) (type:procedure ((? type:20)) (? type:21)))

(= (? cons:19) (type:procedure ((numeric-type) (numeric-type)) (? type:20)))

(= (? foo:13) (type:procedure ((? type:22)) (? type:23)))

(= (? cons:19) (type:procedure ((boolean-type) (boolean-type)) (? type:22)))

;Value: cdr-infer

cdr-infer
;Value: ***type-error***

(load "parametrics")
;Loading "parametrics.scm"... done
;Value: infer-program-types

(define cdr-infer
  (noisy-infer-program-types cdr-example))
(begin (define foo (lambda (x) (declare-type x (? x:26)) (cdr x))) (declare-type foo (type:procedure ((? x:26)) (? type:30))) (foo (cons 1 2)) (foo (cons #t #f)))
(= (? type:36) (? type:35))

(= (? foo:25) (type:procedure ((? x:26)) (? type:30)))

(= (? type:30) (? type:29))

(parametric-constraint (type:procedure ((type:pair (? car:28) (? cdr:27))) (? cdr:27)) (? type:41))

(= (? type:41) (type:procedure ((? x:26)) (? type:29)))

(parametric-constraint (? foo:25) (? type:39))

(= (? type:39) (type:procedure ((? type:32)) (? type:33)))

(parametric-constraint (? cons:31) (? type:40))

(= (? type:40) (type:procedure ((numeric-type) (numeric-type)) (? type:32)))

(parametric-constraint (? foo:25) (? type:37))

(= (? type:37) (type:procedure ((? type:34)) (? type:35)))

(parametric-constraint (? cons:31) (? type:38))

(= (? type:38) (type:procedure ((boolean-type) (boolean-type)) (? type:34)))

;All type variables should have a mapping (? type:30) ((... ? type:39) (... ? type:42) (... ? type:43) (... ? type:44) (... ? type:45) ...)
;To continue, call RESTART with an option number:
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
#|
(begin (define id (lambda (x y) (declare-type x (? x:200)) (declare-type y (? y:201)) x))
       (declare-type id (type:procedure ((? x:200) (? y:201)) (? type:202)))
       (define aj (id 2 3))
       (declare-type aj (? type:204))
       (define em (id #t #f))
       (declare-type em (? type:206)))
(= (? type:207) (? em:205))

(= (? id:199) (type:procedure ((? x:200) (? y:201)) (? type:202)))

(= (? type:202) (? x:200))

(= (? aj:203) (? type:204))

(parametric-constraint (? id:199) (? type:209))

(= (? type:209) (type:procedure ((numeric-type) (numeric-type)) (? type:204)))

(= (? em:205) (? type:206))

(parametric-constraint (? id:199) (? type:208))

(= (? type:208) (type:procedure ((boolean-type) (boolean-type)) (? type:206)))

;Value: id-infer

id-infer
;Value: (t (boolean-type) (begin (t (type:procedure ((? type:202) (? y:201)) (? type:202)) (define id (t (type:\
procedure ((? type:202) (? y:201)) (? type:202)) (lambda (x y) (t (? type:202) x))))) (t (numeric-type) (define\
 aj (t (numeric-type) ((t (type:procedure ((? type:202) (? y:201)) (? type:202)) id) (t (numeric-type) 2) (t (n\
umeric-type) 3))))) (t (boolean-type) (define em (t (boolean-type) ((t (type:procedure ((? type:202) (? y:201))\
 (? type:202)) id) (t (boolean-type) #t) (t (boolean-type) #f)))))))



(pp (simplify-annotated-program id-infer))
(begin (define id (lambda (x y) (declare-type x (? type:202)) (declare-type y (? y:201)) x))
       (declare-type id (type:procedure ((? type:202) (? y:201)) (? type:202)))
       (define aj (id 2 3))
       (declare-type aj (numeric-type))
       (define em (id #t #f))
       (declare-type em (boolean-type)))
;Unspecified return value
|#


(define cdr-example
  '(define prog
     (lambda (l)
       (cdr l))))
;Value: cdr-example

(define em
  (noisy-infer-program-types cdr-example))
#|
(begin (define prog (lambda (l) (declare-type l (? l:18)) (cdr l))) (declare-type prog (type:procedure ((? l:18)) (? type:22))))
(= (? prog:17) (type:procedure ((? l:18)) (? type:22)))

(= (? type:22) (? type:21))

(= (type:procedure ((type:pair (? car:20) (? cdr:19))) (? cdr:19)) (type:procedure ((? l:18)) (? type:21)))

;Value: em

(pp (simplify-annotated-program em))
(begin (define prog (lambda (l) (declare-type l (type:pair (? car:20) (? type:22))) (cdr l)))
       (declare-type prog (type:procedure ((type:pair (? car:20) (? type:22))) (? type:22))))
;Unspecified return value
|#


(define double-effect-example
  '(begin
     (define foo
       (lambda (x y)
         (begin
           (set-cdr! x 4)
           (write-line y))))

     (define z (list 1 2 3))

     (foo z 4)))

#|
(pp (clean-infer-types-exprs double-effect-example))
(begin
 (define foo
   (lambda (x y)
     (declare-type x (? x:29))
     (declare-type y (? y:30))
     (declare-effects ((effect:write (cdr-of x) (numeric-type))))
     (set-cdr! x 4)
     (declare-effects ((effect:io (? y:30))))
     (write-line y)))
 (declare-type foo (type:procedure ((? x:29) (? y:30)) (? type:36)))
 (declare-effects
  ((effect:allocate (type:procedure ((? x:29) (? y:30)) (? type:36)))))
 (define z
   (begin (declare-effects ((effect:unknown))) (list 1 2 3)))
 (declare-type z (? z:37))
 (declare-effects ((effect:unknown) (effect:allocate (? z:37))))
 (declare-effects
  ((effect:write (cdr-of z) (numeric-type)) (effect:io ((numeric-type)))))
 (foo z 4))
;Unspecified return value
|#

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
#|
(pp (clean-infer-types-exprs multiple-effects-example))
(begin
 (define foo
   (lambda (x y)
     (declare-type x (? x:56))
     (declare-type y (? y:57))
     (declare-effects ((effect:write (cdr-of x) (numeric-type))))
     (set-cdr! x 4)
     (declare-effects ((effect:io (? y:57))))
     (write-line y)))
 (declare-type foo (type:procedure ((? x:56) (? y:57)) (? type:63)))
 (declare-effects
  ((effect:allocate (type:procedure ((? x:56) (? y:57)) (? type:63)))))
 (define z
   (begin (declare-effects ((effect:unknown))) (list 1 2 3)))
 (declare-type z (? z:64))
 (declare-effects ((effect:unknown) (effect:allocate (? z:64))))
 (declare-effects
  ((effect:write (cdr-of z) (numeric-type)) (effect:io ((numeric-type)))))
 (foo z 4)
 (declare-effects
  ((effect:write (cdr-of z) (numeric-type)) (effect:io ((boolean-type)))))
 (foo z #t))
;Unspecified return value
|#

(define gerry-program
  '(define list-ref
     (lambda (lst n)
       (if (= n 0)
           (car lst)
           (list-ref (cdr lst) (- n 1))))))

(pp (simplify-annotated-program (infer-program-types gerry-program)))
#|
(begin
 (define list-ref
   (lambda (lst n)
     (declare-type lst (type:pair (? type:358) (type:pair (? type:358) (? type:356))))
     (declare-type n (numeric-type))
     (if (= n 0)
         (car lst)
         (list-ref (cdr lst) (- n 1)))))
 (declare-type
  list-ref
  (type:procedure
   ((type:pair (? type:358) (type:pair (? type:358) (? type:356))) (numeric-type))
   (? type:358))))
;Unspecified return value

; Hmm, this is bad, Gerry was right...
|#

(define write-example
  '(define prog
     (lambda (l)
       (write-line l))))

#|
(define write-texpr (annotate-program write-example))
;Value: write-texpr

(pp (effect-annotate-program write-texpr))
(effectful
 (? prog:379)
 (define prog
   (effectful
    (type:procedure ((? l:380)) (? type:383))
    (lambda (l)
      (effectful (? type:382) ((effectful (? write-line:381) write-line ()) (effectful (? l:380) l ())) (effect:io ((? l:380)))))
    (effect:io ((? l:380)))))
 (effect:io ((? l:380))))
;Unspecified return value
|#

(define silly-effect-example
  '(+ 1 2))

(pp (clean-infer-types-exprs silly-effect-example))
#|
(begin
  (declare-effects ((effect:pure)))
  (+ 1 2))
;Unspecified return value
|#

#|
TODOs:
Operations file: AJ
Redefine the interface to register built in operations (+, -, cons, car) for types 

Types file: Emily 
Expand built in types ()

SideEffects file: 
explicitly label built ins with side effects (ex. set, )
places for side effects (ex. (set-cdr! x 4) should know what x is .. x is list)

To Consider: 
Figure out how to load both files
Make sure to document copied code
Look at parametric code (in type-resolver.scm)
Read paper by GJS: https://dl.acm.org/doi/10.1145/73560.73564

|#
