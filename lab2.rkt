#lang typed/racket
(require typed/rackunit)

;This is a function to append strings in a reversed order.
(define (rev-str-app [w : (Listof String)]) : String
  (match w
  ['() ""]
  [(list s ...) (string-append (rev-str-app (rest s)) (first s))])) ;Could also just match on _

(check-equal? (rev-str-app (list "ball" "juice" "frog")) "frogjuiceball")
(check-equal? (rev-str-app (list "ball")) "ball")
(check-equal? (rev-str-app (list "")) "")

;I used print-type by just applying it in the terminal as I would normally. It showed that
;rev-str-app had a type of (-> (Listof String) String) Which makes sense as it takes in a
;list of strings and returns a single string. Plus returns a case statement with a bunch of
; different transformations which makes sense as + takes in a number of data types and things
; and applies to them.

;Definiton of a bicycle using structs for each type
(define-type bicycles (U Trek Bianchi Gunnar))
(struct Trek ([n : Real]) #:transparent)
(struct Bianchi ([n : Real]) #:transparent)
(struct Gunnar ([n : Real]) #:transparent)

;Develop the function only-treks, that consumes a list of bicycles and returns a list containing only
;the Treks. Hint: it’s fine to use a match as part of the list template, but don’t use a match to determine
;what kind of bicycle you have; this will impair the abstraction that’s supposed to happen two problems in
;the future. (Also, don’t use the built-in filter function; the idea of this lab is to give you practice in
;writing recursive functions on lists. Unless you re-implement filter. Then it’s fine.)

;This function returns a list of only the treks provided in it's list given
(define (only-treks [b : (Listof bicycles)]) : (Listof bicycles)
  (cond
    [(empty? b) '()]
    [(Trek? (first b)) (cons (first b) (only-treks (rest b)))]
    [else (only-treks (rest b))]))

(check-equal? (only-treks (list (Trek 10) (Bianchi 10) (Trek 20)))
              (list (Trek 10) (Trek 20)))
(check-equal? (only-treks (list (Gunnar 10) (Bianchi 10))) '())
(check-equal? (only-treks '()) '())

;This function takes a list of bikes and only returns bianchis
(define (only-bianchis [b : (Listof bicycles)]) : (Listof bicycles)
  (cond
    [(empty? b) '()]
    [(Bianchi? (first b)) (cons (first b) (only-bianchis (rest b)))]
    [else (only-bianchis (rest b))]))

(check-equal? (only-bianchis (list (Bianchi 10) (Trek 10)))
              (list (Bianchi 10)))
(check-equal? (only-bianchis '()) '())
(check-equal? (only-bianchis (list (Gunnar 10) (Trek 10))) '())

;This function takes in a predicate and a list of bicycles and only returns what satsifies the predicate
(define (onlyThese [b : (Listof bicycles)] [p : (-> bicycles Boolean)]) : (Listof bicycles)
  (cond
    [(empty? b) '()]
    [(p (first b)) (cons (first b) (onlyThese (rest b) p))]
    [else (onlyThese (rest b) p)]))

(check-equal? (onlyThese (list (Bianchi 10) (Trek 10)) Bianchi?)
              (list (Bianchi 10)))
(check-equal? (onlyThese '() Bianchi?) '())
(check-equal? (onlyThese (list (Gunnar 10) (Trek 10)) Bianchi?) '())

;This function appends to lists of symbols
(define (my-append [first : (Listof Symbol)] [second : (Listof Symbol)]) : (Listof Symbol)
  ;Going to assume that it means symbols per the example
  ;Also going to assume len of l1 n l2 ==
  (match first
    ['() second]
    [(cons f r) (cons f (my-append r second))]))

(check-equal? (my-append '(a b c) '(d e f)) '(a b c d e f))
(check-equal? (my-append '() '()) '())

;This function returns n integers of list l where both are given as arguments
(define (my-take [l : (Listof Real)] [n : Real]) : (Listof Real)
  ;Assuming that it means listof Reals
  (match l
   ['() l]
   [(cons f r) (if (> n 0) (cons f (my-take r (- n 1))) '())]))

(check-equal? (my-take (list 1 2 3 4) 2) '(1 2))
(check-equal? (my-take (list 1 2 3 4) 7) '(1 2 3 4))
(check-equal? (my-take '() 10) '())