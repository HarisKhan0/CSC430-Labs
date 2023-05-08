#lang typed/racket
(require typed/rackunit)

;Create a function to receive two parameters sunny and friday return true if sunny is false and friday is true.

;(check-equal? (and (==> false true) true) true)

(define (==> [sunny : Boolean] [friday : Boolean])
  (or (not sunny) friday)
) ;Dont

(check-equal? (and (==> false true) true) true)

;Write a function to insert an _ at a specified distance i.

;(check-equal? (string-insert "Hello World" 5) "Hello_ World")

(define (string-insert [str : String] [i : Integer])
    (string-append (substring str 0 i) ;textbook code
                 "_"
                 (substring str i))
  )

(check-equal? (string-insert "Hello World" 5) "Hello_ World")

;Change defined functions so that they have no 'Magic Numbers'

;(check-equal? (profit 5) 415.2)

(define BASEPEOPLE 120) ;Macros
(define PEOPLECHANGE 15)
(define TICKETCHANGEINCR 0.1)
(define PRICEINITIAL 5.0)
(define FIXEDCOST 180)
(define COSTPERS 0.04)

(define (profit [price : Real])
  (- (* (+ BASEPEOPLE
           (* (/ PEOPLECHANGE TICKETCHANGEINCR)
              (- PRICEINITIAL price)))
        price)
     (+ FIXEDCOST
        (* COSTPERS
           (+ BASEPEOPLE
              (* (/ PEOPLECHANGE TICKETCHANGEINCR)
                 (- PRICEINITIAL price)))))))

(check-equal? (profit 5) 415.2)

;Develop a function to return the amount of interest earned on a sum annualy
;(check-= (interest 100) 4 0.01 "Correct Interest")

(define (interest [deposit : Integer]) : Real
  (cond
    [(<= deposit 1000) (* deposit 0.04)]
    [(<= deposit 5000) (* deposit 0.045)]
    [(> deposit  5000) (* deposit 0.05)]
    [else deposit]
 )
)

(check-= (interest 100) 4 0.01 "Correct Interest")
(check-= (interest 1000) 40 0.01 "Correct Interest")
(check-= (interest 2000) 90 0.01 "Correct Interest")
(check-= (interest 6000) 300 0.01 "Correct Interest")

;Define a function to return the amount of floor space taken by furniture

;(check-equal? (furniture-footprint (desk 100 50 20)) 100000)

(define-type furniture (U desk bookshelf))

(struct desk ([width : Number] [height : Number] [depth : Number]) #:transparent)
(struct bookshelf ([depth : Number] [shelves : Number] [shelf_width : Number]) #:transparent)


(define mahogany (desk 100 50 20)) ;Define func
(define birch (bookshelf 200 10 30))

;Purpose statements for furniture
;Units data def

(define (furniture-footprint [f : furniture]) : Number
  (match f
    [(desk w h d) (* w h d)]
    [(bookshelf num shelf swidth) (* swidth num)]
  )
)

(check-equal? (furniture-footprint (desk 100 50 20)) 100000)
(check-equal? (furniture-footprint (bookshelf 200 10 30)) 6000)



