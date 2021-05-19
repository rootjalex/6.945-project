# 6.945-project
MIT 6.945 Project (Type Inference + Side Effects for Scheme)

Final project for Emily Chen and AJ Root, 6.945 @ MIT, Spring 2021

## Using the system
We set up [files.scm](src/files.scm) to properly load the files in order. To use our system, please do the following for set-up (from the [src](src) directory):


```
(load "<directory of sdf>/manager/load")
(manage 'new 'unification)
(load "files.scm")
```

## Tests
See [examples.scm](src/examples.scm) for our set of illustrative tests. We show one such example here:

```
(define id-example
  '(begin
    (define id
      (lambda (x y) x))
    (define aj
      (id 2 3))
    (define em
      (id #t #f))))
;Value: id-example

(pp (clean-infer-types-effects id-example))
(begin (define id (lambda (x y) (declare-type x (? type:4)) (declare-type y (? y:3)) x))
       (declare-type id (type:procedure ((? type:4) (? y:3)) (? type:4)))
       (declare-effects ((effect:allocate (type:procedure ((? type:4) (? y:3)) (? type:4)))))
       (define aj (begin (declare-effects ()) (id 2 3)))
       (declare-type aj (numeric-type))
       (declare-effects ((effect:allocate (numeric-type))))
       (define em (begin (declare-effects ()) (id #t #f)))
       (declare-type em (boolean-type))
       (declare-effects ((effect:allocate (boolean-type)))))
;Unspecified return value
```
