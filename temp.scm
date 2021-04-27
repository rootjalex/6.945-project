; (load "/Users/alexanderroot/Classes/6.945/sdf/manager/load")
; (manage 'new 'unification)


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