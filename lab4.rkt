#lang racket
(require rackunit)

(define (curried-add a)
  (lambda (b) (+ b a)))

(check-equal? ((curried-add 1) 2) 3)

#|
Develop the curry2 function it takes a function of two arguments f,
and produces a function that we’ll call M. The function M takes one
argument and produces a function that we’ll call N. N takes one argument
and produces the result of calling the input function f on the two given
arguments. In other words, it has the type
|#

(define (curry2 f)
  (lambda (m) (lambda (n) (f m n))))

(check-equal? (((curry2 +) 1) 2) 3)

;Develop the curry3 function it takes a function of three arguments,
;and produces a function that takes one argument and produces a
;function that takes one argument and produces a function that takes
;one argument and produces the result of calling the input function
;on the three given arguments. In other words, it has the type

(define (curry3 f)
  (lambda (a) (lambda (b) (lambda (c) (f a b c)))))

(check-equal? ((((curry3 +) 1) 2) 3) 6)

(define (contains? l s)
  (cond
    [(empty? l) #f]
    [(equal? (first l) s) #t]
    [else (contains? (rest l) s)]))

(define (in-list-many? l q) (map ((curry2 contains?) l) q))
