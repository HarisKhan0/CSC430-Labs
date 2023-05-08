#lang racket
(require rackunit)

;Warmup :?
(define p1 (lambda (x) (lambda (y) x)))
(define one (lambda (v) (lambda (f) (v f))))

(define plusone (lambda (x) (+ x 1)))

(check-equal? ((one plusone) 2) 3)

;Define a function called two that accepts a function and an argument and
;applies the function to the result of applying the function to the argument.

(define two (lambda (v) (lambda (f) (v (v f)))))

(check-equal? ((two plusone) 2) 4)

(define zero (lambda (v) (lambda (f) f)))

(check-equal? ((zero plusone) 2) 2)

#|
Let’s use the term “number-like functions” for functions like zero, one, and two.
Define a function called add1 that accepts a number-like function and returns a new
number-like function that does the function "one more time". So, for instance, calling
add1 with two should produce the function that applies its first argument to its second
argument three times. This function should not use racket’s numbers at all, just the
number-like functions described above.
|#

(define add1 (lambda (v) (lambda (f) (lambda (g) ((v f) ((v f) g))))))
(check-equal? (((add1 two) plusone) 2) 6)
#|
Define a function called ’add’ that accepts two functions like zero and one and
returns a function that applies its first argument to its second argument a number
of times that corresponds to the sum of the two ’numbers’ it was given. So, if called
with the ’three’ function and the two function, it should produce a function that
applies its first argument to its second argument five times.
|#

(define add (lambda (v) (lambda (f) (lambda (g) (lambda (e) ((v g) ((f g) e)))))))
(check-equal? ((((add one) two) plusone) 2) 5)

(define tru (lambda (v) (lambda (f) v)))
(check-equal? ((tru 1) 2) 1)
(define fals (lambda (v) (lambda (f) f)))
(check-equal? ((fals 1) 2) 2)

(define if (lambda (v) (lambda (f) (lambda (g) ((v f) g)))))
(check-equal? (((if tru) 1) 2) 1)
(check-equal? (((if fals) 1) 2) 2)

(module sim-VVQS5 racket
  (provide
   [rename-out (#%lam-app #%app)]
   #%module-begin
   #%datum
   + - * / = equal? <= => if else
   true false)
 
  (define-syntax (#%lam-app stx)
    (syntax-case stx (=> if else)
      [(_ (args ...) => body)
       #'(lambda (args ...) body)]
      [(_ e1 if e2 else e3)
       #'(if e2 e1 e3)]
      [(_ e ...)
       #'(#%app e ...)])))

(module my-module (submod ".." sim-VVQS5)
 
  {+ 4 5}


  )

(require 'my-module)

