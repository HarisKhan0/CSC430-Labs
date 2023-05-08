#lang typed/racket
(require typed/rackunit)

(define (parse000 [sexp : Sexp]) : Boolean
  (match sexp
    [(list (? real? first) 'chris (? symbol? third)) #t]
    [_ #f]))

;(cons 1 (cons 'chris (cons 'symb '())))
;(parse000 (cons 1 (cons 'chris (cons 'symb '()))))

(define-type result (U #f Symbol))

(define (parse001 [sexp : Sexp]) : result
  (match sexp
    [(list (? real? first) 'chris (? symbol? third)) third]
    [_ #f]))

;Develop the parse002 function, that also uses a single pattern and that
;succeeds for s-expressions that are lists of length three whose second
;element is a list of real numbers. On success, it should return the
;list of numbers. On failure, it should return #f.

(define-type result002 (U (Listof Real) #f #t))

(define (parse002 [sexp : Sexp]) : result002
  (match sexp
  [(list a (list (? real? b) ...) c) (cast b (Listof Real))]
  [_ #f]))

(check-equal? (parse002 '(1 (2) 3)) '(2))
(check-equal? (parse002 '(1 2 3)) #f)
(check-equal? (parse002 '(a (b) c)) #f)

(define (ohno [e : Any]) : Symbol
  (cond
    [(real? e) 'okay]
    [else (error 'ohno "invalid input not a number, got ~e" e)])
  )

(check-exn #px"invalid input not a number, got"
           (lambda () (ohno 'Yikers)))
(check-equal? (ohno 2) 'okay)

(define-type ExprC (U numC binop appC leq0? idC '()))
(struct numC ([n : Real]) #:transparent)
(struct binop ([s : ExprC][l : ExprC][r : ExprC]) #:transparent)
(struct appC ([fun : Symbol][arg : (Listof ExprC)]) #:transparent)
(struct leq0? ([cond : ExprC][then : ExprC][else : ExprC]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)

; Operators
; Divides two numbers, errors on a divide by 0
(define (my-divide [l : Real] [r : Real]) : Real
  (if (eq? r 0) (error 'interp "VVQS: divide by 0")
      (/ l r)))
; + * - / operators
(define operators (hash (idC '+) + (idC '*) * (idC '-) - (idC '/) my-divide))

; Operator test cases
(check-equal? ((hash-ref operators (idC '+)) 2 5) 7)
(check-equal? ((hash-ref operators (idC '*)) 3 6) 18)
(check-equal? ((hash-ref operators (idC '-)) 5 2) 3)
(check-equal? ((hash-ref operators (idC '/)) 5 5) 1)
(check-exn #px"VVQS: divide by 0" (λ () (my-divide 1 0)))

; Interprets the given expression, using the list of funs to resolve applications.
(define (interp [exp : ExprC] [fds : (Listof FunDefC)]) : Real
  (match exp
    [(numC n) n]
    [(idC _) (error 'interp "VVQS: shouldn't get here: ~e" exp)]
    [(binop s l r) ((hash-ref operators s) (interp l fds) (interp r fds))]
    [(leq0? c t e) (cond
                   [(<= (interp c fds) 0) (interp t fds)]
                   [else (interp e fds)])]
    [(appC f a) (define fd (get-fundef f fds))
                (if (= (length (FunDefC-args fd)) (length a))
                    (interp (subst a (FunDefC-args fd) (FunDefC-body fd)) fds) 
                    (error 'interp "VVQS: wrong arity")
                    )
                ]
    )
  )

; interp test cases
; numC
(define numC0 (numC 0))
(define numC1 (numC 1))
(define numC5 (numC 5))
(define numC10 (numC 10))

; idC
(define idC-add (idC '+))
(define idC-mult (idC '*))
(define idC-sub (idC '-))
(define idC-div (idC '/))
(define idC-a (idC 'a))
(define idC-b (idC 'b))
(define idC-c (idC 'c))

(define binop-add6 (binop idC-add numC1 numC5))
(define binop-mult5 (binop idC-mult numC1 numC5))
(define binop-sub5 (binop idC-sub numC10 numC5))
(define binop-div2 (binop idC-div numC10 numC5))

(check-equal? (interp numC5 '()) 5)
(check-exn #px"VVQS: shouldn't get here: " (λ () (interp (idC 'a) '())))
(check-equal? (interp binop-mult5 '()) 5)

; TODO test cases for appC
; TODO test case for other error
#;(check-exn #px"VVQS: Invalid interp" (λ () (interp '() '())))

;; Subst what for in
(define (subst [what : (Listof ExprC)] [for : (Listof Symbol)] [in : ExprC]) : ExprC
  (match in
    [(numC n) in]
    [(idC s) (if (member s for) (list-ref what (cast (index-of for s) Integer)) in)]
    [(appC f (list args ...)) (appC f
                                    (map (λ ([x : ExprC]) (subst what for x)) args))]
    [(binop s l r) (binop s (subst what for l) (subst what for r))]
    [(leq0? c t e) (leq0? (subst what for c) (subst what for t) (subst what for e))]
    )
  )

; subst test cases

(check-equal? (subst (list numC0) (list 'a) numC1) numC1)
(check-equal? (subst (list numC0) (list 'b) (idC 'a)) (idC 'a))
(check-equal? (subst (list (numC 7)) (list 'x) (appC 'f (list (numC 7)))) (appC 'f (list (numC 7))))


; Find the function n in the list of fundefs
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "VVQS: reference to undefined function")]
    [(cons? fds)
     (cond
       [(equal? n (FunDefC-name (first fds))) (first fds)]
       [else (get-fundef n (rest fds))])]))
