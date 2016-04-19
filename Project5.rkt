#lang racket
 (require plai-typed)

;;Botan Özdemir
;defining expression type
;S->aS
;Use in other methods
;; Grammar
;; ExprC ::= <number>
;; s : symbol
;; ExprC ::=(numC <ExprC>)
;        (idC <s>)
;;       (appC <s> <ExprC>)
;;       (plusC <ExprC> <ExprC>)
;;       (multC <ExprC> <ExprC>)
;;       (ifCond <ExprC> <ExprC> <ExprC>)
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [factC (l : ExprC)]
  [ifC (c : ExprC) (y : ExprC) (n : ExprC)])



;FundefC,Function Definition Structure
(define-type FunDefC
  [fdC (name : symbol) (arg : s-expression ) (body : ExprC)])

;get-fundef method
;(listof FunDefC) -> FunDefC
;Template
;(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
;  (cond
;    [(empty? fds) ...]
;    [else ...(first fds) ...(get-fundef (rest fds))])
;Purpose
;Find function name, identifier, definitions
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

;; parse s-expression -> ExprC
;; examples
;; '7 -> (numS 7)
;; '(+ 3 4) -> (msl-(numS 3) (numS 4))
;; '(+ (+ 3 4) 35) -> (plusS (plusS (numS 3) (numS 4)) (numS 35))
;Template
;(define (parse [s : s-expression]) : ArithS
;(cond
;[(s-exp-number? s)(numS (s-exp->number s))]
;[(s-exp-symbol? s) (idS (s-exp->symbol s))]
;[(s-exp-list? s)
;(let ([sl (s-exp->list s)])
;(case (s-exp->symbol (first sl))
;[(+) (plusS ....]
;[(*) (multS .....]
;[(-) (cond .....
;[(function) (appS .....]
;[(if) (ifS .....]
;[else ....]
;[else ....)
;Purpose
;; convert a quoted s expression into the equivalent ArithS form
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s)(numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [(function)(appC (s-exp->symbol (second sl)) (parse (third sl)))]
         [(if) (ifC (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else ((error 'parse "invalid list input"))]))]
    [else (error 'parse "invalid input")]))


(define fnc (list 
                    [fdC 'double 'x (parse '(+ x x))]
                    [fdC 'myfnc '(x y) (parse '(+ x 2y))]
                    [fdC 'quadruple 'x (parse '(* 4 x))]
                    [fdC 'sqr 'y (parse '(* y y))]
                    [fdC 'new 'z (parse '(+ x (+ y y)))]))

;Substitution
;ExprC -> ExprC
;Template
; (define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
;  (type-case ExprC in
;    [numC (n) ...]
;    [idC (s) ...]
;    [appC (f a) ...]
;    [plusC (l r) ...]
;    [multC (l r) ...]
;    [subC (l r) ...]
;    [ifC (c y n) ...])
;Purpose
;Substitute expressions with other expression in ExprC according to cond
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
  [numC (n) in]
  [idC (s) (cond
             [(symbol=? s for) what]
             [else in])]
  [appC (f a) (appC f (subst what for a))]
  [plusC (l r) (plusC (subst what for l)
                      (subst what for r))]
  [multC (l r) (multC (subst what for l)
                      (subst what for r))]
  [factC (l) (factC (subst what for l)
                      )]  
  [ifC (c y n) (ifC (subst what for c)
                      (subst what for y)
                      (subst what for n))]))




;Interpreter
;ExprC : (listof FunDefC) -> number
;Template
;(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
;  (type-case ExprC in
;    [numC (n) ...]
;    [idC (s) ...]
;    [appC (f a) ...]
;    [plusC (l r) ...]
;    [multC (l r) ...]
;    [subC (l r) ...]
;    [ifC (c y n)...])
;Purpose
;interprate expressions to numbers,booleans.
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
              (interp (subst a
                             (fdC-arg fd)
                             (fdC-body fd))
                      fds))]  
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp (factC (- x 1)) fds))])]
    [ifC (c y n) (cond
           [(> (interp c fds) 0) (interp y fds)]
           [else (interp n fds)])]))


;;tests
;;interp--> -->parse
;;(test (interp (  (parse(...))
(test (interp (parse '7)  fnc) 7)
(test (interp (parse '3)  fnc) 3)
(test (interp (parse '6)  fnc) 6)
(test (interp (parse '(+ 3 4))  fnc) 7)
(test (interp (parse '(+ 4 46))  fnc) 50)

(test (interp (parse '(* 1 222342))  fnc) 222342)
(test (interp (parse '(* 4 -5))  fnc) -20)
(test (interp (parse '(- 11 -11))  fnc) 22)
(test (interp (parse '(* 5 5))  fnc) 25)
(test (interp (parse '(+ 2 4))  fnc) 6)

(test (interp (parse '(function double 3))  fnc) 6)
(test (interp (parse '(function double 6))  fnc) 12)
(test (interp (parse '(function double 0))  fnc) 0)
(test (interp (parse '(function double -5))  fnc) -10)
(test (interp (parse '(function double 1))  fnc) 2)

(test (interp (parse '(function quadruple 3))  fnc) 12)
(test (interp (parse '(function quadruple 2))  fnc) 8)
(test (interp (parse '(function quadruple 4))  fnc) 16)
(test (interp (parse '(function quadruple -3))  fnc) -12)
(test (interp (parse '(function quadruple 0))  fnc) 0)


(test (interp (parse '(function sqr 4))  fnc) 16)
(test (interp (parse '(function sqr 8))  fnc) 64)
(test (interp (parse '(function sqr 1))  fnc) 1)
(test (interp (parse '(function sqr -1))  fnc) 1)
(test (interp (parse '(function sqr 0))  fnc) 0)



(test (interp (factC 0) fnc) 1)
(test (interp (factC 1) fnc) 1)
(test (interp (factC 2) fnc) 2)
(test (interp (factC 3) fnc) 6)
(test (interp (factC 4) fnc) 24) 
(test (interp (factC 5) fnc) 120)
(test (interp (factC 2) fnc) 7)


;;-------------------------------------------------LAMBDA CALCULUS PROJECT 5 ------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------


;; λ-expression grammar
;; λ -> v
;; λ -> (λ λ)
;; λ -> (λ v λ)
;; λ-exp is grammar 
(define-type λ-exp
  (λ-sym (v : symbol))
  (λ-app (l : λ-exp)(r : λ-exp))
  (λ-def (v : symbol)(p : λ-exp))
  )

;; parselambda s-expression ->  λ-exp
;Template
;(define (parselambda [se : s-expression]) :  λ-exp
;(cond
;[(s-exp-symbol? s) (λ-sym (s-exp->symbol s))]
;[(s-exp-list? s)
;(let ([sl (s-exp->list s)])
;(case (s-exp->symbol (first sl))
;[(+) (λ ....]
;[(*) (λ .....]
;[(-) (cond .....
;[(λ-app .....]
;[(if)  .....]
;[else ....]
;[else ....)
;Purpose
;; convert a quoted s expression into the equivalent ArithS form
(define (parselambda (se : s-expression)) : λ-exp
  (cond
    [(s-exp-symbol? se)(λ-sym (s-exp->symbol se))]
    [(s-exp-list? se)
     (let ([se-list (s-exp->list se)])
       (cond
         [(= 2 (length se-list))
          (λ-app (parselambda (first se-list))(parselambda (second se-list)))]
         [(= 3 (length se-list))
          (if (and (symbol=? 'λ (s-exp->symbol (first se-list)))
                   (s-exp-symbol? (second se-list)))
              (λ-def (s-exp->symbol(second se-list))
                     (parselambda (third se-list)))
              (error 'parselambda "Invalid λ-def")
              )]
         [else (error 'parselambda "Invalid se λ-exp")]
         ))]
    [else (error 'parselambda "Invalid λ-exp")]
))


;; A set represented as a list.
;; appendlist : (listof symbol) (listof symbol) -> (listof symbol)
;; Purpose : Append two sets.
(define (appendlist (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
  (foldr (λ (x y)
           (if (member x y)
               y
               (cons x y))) 
         empty
         (append s1 s2)))

;; Tests:
(test (appendlist empty empty) empty)
(test (appendlist empty (list 'y)) (list 'y))
(test (appendlist (list 'x)(list 'x 'y)) (list 'x 'y))
(test (appendlist (list 'x)(list 'x 'y)) (list 'x 'y))
(test (appendlist (list 'z)(list 'x 'y)) (list '(x y z)))
(test (appendlist (list 'x)(list 'x)) (list 'x))


;; difelement : (listof symbol) (listof symbol) -> (listof symbol)
;; Purpose : Find the elements which exist in s1 but not exits in s2
(define (difelement (s1 : (listof symbol))  (s2 : (listof symbol))) : (listof symbol)
  (filter (λ (x)
            (not (member x s2)))
          s1))

;; Tests:
(test (difelement empty (list 'x)) empty)
(test (difelement (list 'y) empty) (list 'y))
(test (difelement (list 'y) (list 'y)) empty)
(test (difelement (list 'x)(list 'x 'y)) empty)
(test (difelement (list 'x 'y)(list 'x))(list 'y))



;; free-identifier : λ-exp -> (listof symbol)
;; Purpose
;; To find free identifiers in given λ expression.
(define (free-identifier (lam : λ-exp)) : (listof symbol)
  (type-case λ-exp lam
    (λ-sym (v) (list v))
    (λ-app (l r)(appendlist 
                 (free-identifier l)
                 (free-identifier r)))
    (λ-def (v p)(difelement (free-identifier p)
                                (list v)))
    ))

;;test for free identifiers
(test (free-identifier (parselambda (symbol->s-exp 'x))) (list 'x))
(test (free-identifier (parselambda (symbol->s-exp 'y))) (list 'y))
(test (free-identifier (parselambda (symbol->s-exp '3))) (list '3))
(test (free-identifier (parselambda (symbol->s-exp '0))) (list '0))
(test (free-identifier (parselambda (symbol->s-exp 'h))) (list 'h))
(test (free-identifier (parselambda '(λ x x))) empty)
(test (free-identifier (parselambda '(λ y x))) (list 'x))
(test (free-identifier (parselambda '(λ x z))) (list 'z))
(test (free-identifier (parselambda '(λ x y))) (list 'y))
(test (free-identifier (parselambda '((λ x x)(λ y z)))) (list 'z))
(test (free-identifier (parselambda '((λ y x)(λ y x)))) (list 'x))
(test (free-identifier (parselambda '((λ y x)(λ y z)))) (list 'x 'z))
(test (free-identifier (parselambda '((λ x y)(λ y z)))) (list 'y 'z))
(test (free-identifier (parselambda '((λ x (λ y (y x))) (λ y z)))) (list 'z))
(test (free-identifier (parselambda '((λ f y)(λ z z)))) (list 'y))
(test (free-identifier (parselambda '(λ x (λ y (y x))))) empty)
(test (free-identifier (parselambda '(λ y (λ y (y x))))) '(x))
(test (free-identifier (parselambda '(λ x (λ y (y y))))) '())
(test (free-identifier (parselambda '(λ y (λ x (y x))))) empty)
(test (free-identifier (parselambda '(λ x (λ y z)))) (list 'z))


