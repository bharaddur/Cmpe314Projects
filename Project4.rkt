#lang plai-typed
;;Define msl type
;;Botan Özdemir
;; AE ::= <number>
;; AE ::= (plus <AE> <AE>)
;;        (mul <AE> <AE>)
;;        (bminus <AE> <AE>)
;;        (app <s> <AE>)
;;        (id <s> )
;;        (num <n> )
;;        (if <AE> <AE><AE>)
;Defining ArithS
(define-type ArithS
  [numS (n : number)]
  [idS (s : symbol)]
  [appS (fun : symbol) (arg : ArithS)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [ifS (c : ArithS) (s : ArithS) (n : ArithS)])


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
  [ifC (c : ExprC) (s : ExprC) (n : ExprC)])




;desugaring with bminusS and uminusS
;ArtihS-->ExprC
;Template
;(define (desugar ...) : ...
;(type-case ....
;[numS (n) ...]
;[plusS (l r) ....]
;[idS (s) ...]
;[multS (l r) ....]
;[appS (fun arg) ....]
;[bminusS (l r) ....]
;[uminusS (e) ....]
;[ifS (c y n) ....))
;Purpose
;Desugar parser ArithS to ExprC for eval(interp)
;This function helps the calculate substraction with 2 different style-desugaring
(define (desugar [as : ArithS]) : ExprC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [idS (s) (idC s)]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [appS (fun arg) (appC fun (desugar arg))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [ifS (c y n) (ifC (desugar c) (desugar y) (desugar n))]))




;;tests for Desuggaring

(test (desugar (numS 7 )) (numC 7))
(test (desugar (numS 0 )) (numC 0))
(test (desugar (numS -5 )) (numC -5))
(test (desugar (numS -1 )) (numC -1))
(test (desugar (numS 23 )) (numC 23))


(test (desugar (plusS (numS 7 ) (numS 4))) (plusC (numC 7) (numC 4)))
(test (desugar (plusS (numS 5 ) (numS 4 )))(plusC (numC 5) (numC 4)))
(test (desugar (plusS (numS 1 ) (numS 4 )))(plusC (numC 1) (numC 4)))
(test (desugar (plusS (numS 51 ) (numS 43 )))(plusC (numC 51) (numC 43)))
(test (desugar (plusS (numS 10) (numS 0))) (plusC (numC 10) (numC 0)))


(test (desugar (multS (numS 5 ) (numS 4 ))) (multC (numC 5) (numC 4)))
(test (desugar (multS (numS 6 ) (numS 6 ))) (multC (numC 6) (numC 6)))
(test (desugar (multS (numS -5 ) (numS 1 ))) (multC (numC -5) (numC 1)))
(test (desugar (multS (numS 11 ) (numS 6 ))) (multC (numC 11) (numC 6)))
(test (desugar (multS (numS 2 ) (numS 4 ))) (multC (numC 2) (numC 4)))

(test (desugar (uminusS (numS 5 )))  (multC (numC -1) (numC 5)))
(test (desugar (uminusS (numS 7 )))  (multC (numC -1) (numC 7)))
(test (desugar (uminusS (numS 0 )))  (multC (numC -1) (numC 0)))
(test (desugar (uminusS (numS 1 )))  (multC (numC -1) (numC 1)))
(test (desugar (uminusS (numS -5 ))) (multC (numC -1) (numC -5)))




;FundefC,Function Definition Structure
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

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
;interpuate expressions to numbers,booleans.
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f arg) (let [(fd (get-fundef f fds))]
                    (interp (subst (numC (interp arg fds))
                                   (fdC-arg fd)
                                   (fdC-body fd))
                            fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [ifC (c y n) (cond
           [(> (interp c fds) 0) (interp y fds)]
           [else (interp n fds)])]))


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
  [ifC (c y n) (ifC (subst what for c)
                      (subst what for y)
                      (subst what for n))]))
;------------------------------------Project 4-----------------------------
;--------------------------------------------------------------------------
;; parse s-expression -> ArithS
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
(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s)(numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (cond 
                [(equal? (length sl) 3) (bminusS (parse (second sl)) (parse (third sl)))]
                [(equal? (length sl) 2) (uminusS (parse (second sl)))]
                [else (error 'parse "invalid minus")])]
         [(function) (appS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else ((error 'parse "invalid list input"))]))]
    [else (error 'parse "invalid input")]))



;;Test for parser
(test (parse '7) (numS 7))
(test (parse '3) (numS 3))
(test (parse '6) (numS 6))
(test (parse '23) (numS 23))
(test (parse '0) (numS 0))

(test (parse '(+ 3 4)) (plusS (numS 3) (numS 4)))
(test (parse '(+ -3 24)) (plusS (numS -3) (numS 24)))
(test (parse '(+ 1 0)) (plusS (numS 1) (numS 0)))
(test (parse '(+ 0 -3)) (plusS (numS 0) (numS -3)))
(test (parse '(+ 11 -11)) (plusS (numS 11) (numS -11)))

(test (parse '(* 0 -11)) (multS (numS 0) (numS -11)))
(test (parse '(* 1 222342)) (multS (numS 1) (numS 222342)))
(test (parse '(* 2 3)) (multS (numS 2) (numS 3)))
(test (parse '(* 4 -5)) (multS (numS 4) (numS -5)))
(test (parse '(* 5 5)) (multS (numS 5) (numS -5)))


(test (parse '(function double 3))(appS 'double (numS 3)))
(test (parse '(function double 6))(appS 'double (numS 6)))
(test (parse '(function double 0))(appS 'double (numS 0)))
(test (parse '(function double -5))(appS 'double (numS -5)))
(test (parse '(function double 1))(appS 'double (numS 1)))

(test (parse '(function quadruple 3))(appS 'quadruple (numS 3)))
(test (parse '(function quadruple 2))(appS 'quadruple (numS 2)))
(test (parse '(function quadruple 4))(appS 'quadruple (numS 4)))
(test (parse '(function quadruple -3))(appS 'quadruple (numS -3)))
(test (parse '(function quadruple 0))(appS 'quadruple (numS 0)))

(test(parse '(+ x x))(plusS (idS 'x) (idS 'x)))
(test(parse '(* x x))(multS (idS 'x) (idS 'x)))
(test(parse '(- x x))(bminusS (idS 'x) (idS 'x)))
(test(parse '(+ 2x x))(plusS (idS '2x) (idS 'x)))
(test(parse '(+ x -x))(plusS (idS 'x) (idS '-x)))

(test (parse '(function sqr 4))(appS 'double (numS 3)))
(test (parse '(function sqr 8))(appS 'double (numS 2)))
(test (parse '(function sqr 1))(appS 'double (numS 4)))
(test (parse '(function sqr -1))(appS 'double (numS -3)))
(test (parse '(function sqr 0))(appS 'double (numS 0)))




(define fnc (list 
                    [fdC 'double 'x (desugar(parse '(+ x x)))]
                    [fdC 'quadruple 'x (desugar(parse '(* 4 x)))]
                    [fdC 'sqr 'y (desugar(parse '(* y y)))]
                    [fdC 'new 'z (desugar(parse '(+ x (+ y y))))]))


;;test for interp numC
(test (interp (numC 7)    fnc) 7)
(test (interp (numC -3)    fnc) -3)
(test (interp (numC 7)    fnc) 7)
(test (interp (numC 0)    fnc) 0)
(test (interp (numC 17)    fnc) 17)

;;test for interp appC

(test (interp (appC 'double (numC 7))    fnc) 14)
(test (interp (appC 'double (numC -5))   fnc) -10)
(test (interp (appC 'double (numC 2))    fnc) 4)
(test (interp (appC 'double (numC 0))    fnc) 0)
(test (interp (appC 'quadruple (numC 1))    fnc) 4)

;;test for plusC 
(test(interp (plusC (numC 4) (numC 6))   fnc) 10)
(test(interp (plusC (numC -5) (numC 5))    fnc) 0)
(test(interp (plusC (numC 1) (numC 2))    fnc) 3)
(test(interp (plusC (numC 110) (numC 6))    fnc) 116)
(test(interp (plusC (numC 0) (numC -5))    fnc) -5)


;;test for multC
(test(interp (multC (numC 4) (numC 6))    fnc) 24)
(test(interp (multC (numC 0) (numC -34234))    fnc) 0)
(test(interp (multC (numC 1) (numC 43))    fnc) 43)
(test(interp (multC (numC -5) (numC 3))  fnc) -15)
(test(interp (multC (numC 1) (numC 1))  fnc) 1)

;;tests for if greater-than-zero primitive
;; Important for Project 3------------------------------------------
(test (interp (ifC (numC 7) (numC 1) (numC 0))   fnc) 1)
(test (interp (ifC (numC -20) (numC 1) (numC 0))    fnc) 0)
(test (interp (ifC (numC 0) (numC 1) (numC 0))    fnc) 0)
(test (interp (ifC (numC 1) (numC 1) (numC 0))    fnc) 1)
(test (interp (ifC (plusC  (numC 5) (numC -6)) (numC 1) (numC 0))   fnc) 0)
(test (interp (ifC (multC (numC -1) (numC -2)) (numC 1) (numC 0))   fnc) 1)
(test (interp (ifC (appC 'double (numC 7)) (numC 1) (numC 0))    fnc) 1)
(test (interp (ifC (plusC (numC 1) (numC 2)) (numC 1) (numC 0))  fnc) 1)
(test (interp (ifC (multC (numC -5) (numC 3)) (numC 1) (numC 0))  fnc) 0)
(test (interp (ifC (appC 'double (numC 0)) (numC 1) (numC 0))  fnc) 0)




;;tests
;;interp-->desugar-->parse
;;(test (interp (desugar (parse(...))
(test (interp (desugar (parse '7))  fnc) 7)
(test (interp (desugar (parse '3))  fnc) 3)
(test (interp (desugar (parse '6))  fnc) 6)
(test (interp (desugar (parse '(+ 3 4)))  fnc) 7)
(test (interp (desugar (parse '(+ 4 46)))  fnc) 50)

(test (interp (desugar (parse '(* 1 222342)))  fnc) 222342)
(test (interp (desugar (parse '(* 4 -5)))  fnc) -20)
(test (interp (desugar (parse '(- 11 -11)))  fnc) 22)
(test (interp (desugar (parse '(* 5 5)))  fnc) 25)
(test (interp (desugar (parse '(+ 2 4)))  fnc) 6)


(test (interp (desugar (parse '(function double 3)))  fnc) 6)
(test (interp (desugar (parse '(function double 6)))  fnc) 12)
(test (interp (desugar (parse '(function double 0)))  fnc) 0)
(test (interp (desugar (parse '(function double -5)))  fnc) -10)
(test (interp (desugar (parse '(function double 1)))  fnc) 2)

(test (interp (desugar (parse '(function quadruple 3)))  fnc) 12)
(test (interp (desugar (parse '(function quadruple 2)))  fnc) 8)
(test (interp (desugar (parse '(function quadruple 4)))  fnc) 16)
(test (interp (desugar (parse '(function quadruple -3)))  fnc) -12)
(test (interp (desugar (parse '(function quadruple 0)))  fnc) 0)


(test (interp (desugar (parse '(function sqr 4)))  fnc) 16)
(test (interp (desugar (parse '(function sqr 8)))  fnc) 64)
(test (interp (desugar (parse '(function sqr 1)))  fnc) 1)
(test (interp (desugar (parse '(function sqr -1)))  fnc) 1)
(test (interp (desugar (parse '(function sqr 0)))  fnc) 0)

      

