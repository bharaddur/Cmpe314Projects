#lang plai-typed


;--------------------------------P1------------------------------------------
;;Define msl type
;;Botan Özdemir
;; AE ::= <number>
;; AE ::= (add <AE> <AE>)
;;      | (mul <AE> <AE>)
;;      | (sub <AE> <AE>)
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

;; parse s-expression -> ArithS
;; convert a quoted s expression into the equivalent ArithS form
;; examples
;; '7 -> (numS 7)
;; '(+ 3 4) -> (msl-(numS 3) (numS 4))
;; '(+ (+ 3 4) 35) -> (plusS (plusS (numS 3) (numS 4)) (numS 35))
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
                [else (error 'parse "invalid minus")])
         ]
         [(functions) (appS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


;;Test for parse
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



;desugaring with bminusS and uminusS
;as -> s-expression
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
    [ifS (c s n) (ifC (desugar c) (desugar s) (desugar n))]))




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

(test (desugar (uminusS (numS 5 ))) (plusC (numC -1) (multC (numC -1) (numC 5))))
(test (desugar (uminusS (numS 7 ))) (plusC (numC -1) (multC (numC -1) (numC 7))))
(test (desugar (uminusS (numS 0 ))) (plusC (numC -1) (multC (numC -1) (numC 0))))
(test (desugar (uminusS (numS 1 )))  (plusC (numC -1) (multC (numC -1) (numC 1))))
(test (desugar (uminusS (numS -5 ))) (plusC (numC -1) (multC (numC -1) (numC -5))))



;; parse-infix
;;list of s-exp -> AE
;; Purpose
;;Parse given list of s-expression infix to ArithS.
(define (parse-infix [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (if (or (not (= (length sl) 3))(not (s-exp-symbol? (second sl))))
           (error 'parse-infix "input contract violation!")
           (case (s-exp->symbol (second sl))
             [(+) (plusS (parse-infix (first sl)) (parse-infix (third sl)))]
             [(*) (multS (parse-infix (first sl)) (parse-infix (third sl)))]
             [(-) (msl-sub (parse-infix (first sl)) (parse-infix (third sl)))]
             [(^) (expS (parse-infix (first sl)) (parse-infix (third sl)))]
             [else (error 'parse-infix "invalid list input")])))]
    (else (error 'parse "invalid input"))))



;unparser-infix
;ArithS -> s-expression
;This function takes ArithS type object and unparses it into infix bracketed s-expression.
  (define (unparser-infix [expr : ArithS])
    (type-case ArithS expr
      (numS (n) (list (number->s-exp n)))
      (plusS (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '+))) (unparser-infix rhs)))
      (bminusS (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '-))) (unparser-infix rhs)))
      (multS (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '*))) (unparser-infix rhs)))
      (expoS (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '^))) (unparser-infix rhs)))
               )
      )



;unparser-prefix
;ArithS -> s-expression
;This function takes ArithS type object and unparses it into prefix bracketed s-expression.
   (define (unparser-prefix [expr : ArithS]) 
     (type-case ArithS expr
       (numS (n) (list (number->s-exp n)))
       (plusS (lhs rhs) (append (list(symbol->s-exp '+)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
       (msl-sub (lhs rhs) (append (list(symbol->s-exp '-)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
       (multS (lhs rhs) (append (list(symbol->s-exp '*)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
       (expoS (lhs rhs) (append (list(symbol->s-exp '^)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
       ))

;;unparser-reverse-polish
;ArithS --> s-expression
;This function unparser the expression reverse polish
(define (unparser-reverse-polish [expr : ArithS])
  (type-case ArithS expr
    (numS (n) (list (number->s-exp n)))
    (plusS (lhs rhs) (append (append (unparser-reverse-polish lhs) (unparser-reverse-polish rhs))(list (symbol->s-exp '+))))
    (msl-sub (lhs rhs) (append  (append (unparser-reverse-polish lhs) (unparser-reverse-polish rhs)) (list(symbol->s-exp '-))))
    (multS (lhs rhs) (append (append (unparser-reverse-polish lhs) (unparser-reverse-polish rhs))(list (symbol->s-exp '*))))
    (expoS (lhs rhs) (append (append (unparser-reverse-polish lhs) (unparser-reverse-polish rhs))(list (symbol->s-exp '^))))))


;;tests for unparser-prefix
(test (unparser-prefix (numS 5)) (s-exp->list '(5)))
(test (unparser-prefix (numS 8)) (s-exp->list '(8)))
(test (unparser-prefix (numS 15)) (s-exp->list '(15)))
(test (unparser-prefix (numS 20)) (s-exp->list '(20)))
(test (unparser-prefix (numS 12)) (s-exp->list '(5)))

(test (unparser-prefix (plusS (numS 5) (numS 6))) (s-exp->list '(+ 5 6)))
(test (unparser-prefix (plusS (numS 6) (numS 8))) (s-exp->list '(+ 6 8)))
(test (unparser-prefix (plusS (numS 1) (numS 0))) (s-exp->list '(+ 1 0)))
(test (unparser-prefix (plusS (numS 9) (numS 10))) (s-exp->list '(+ 9 10)))
(test (unparser-prefix (plusS (numS 3) (numS 16))) (s-exp->list '(+ 3 16)))

(test (unparser-prefix (multS (numS 5) (numS 6))) (s-exp->list '(* 5 6)))
(test (unparser-prefix (multS (numS 0) (numS 0))) (s-exp->list '(* 0 0)))
(test (unparser-prefix (multS (numS 3) (numS 3))) (s-exp->list '(* 3 3)))
(test (unparser-prefix (multS (numS 15) (numS 6))) (s-exp->list '(* 15 6)))
(test (unparser-prefix (multS (numS 5) (numS 6))) (s-exp->list '(* 4 6)))

(test (unparser-prefix (plusS (multS (numS 3) (numS 4)) (numS 9))) (s-exp->list '(+ * 3 4 9 )))
(test (unparser-prefix (plusS (multS (numS 5) (numS 0)) (numS -9))) (s-exp->list '(+ * 3 4 -9 )))
(test (unparser-prefix (plusS (multS (numS 6) (numS 4)) (numS 2))) (s-exp->list '(+ * 6 4 2 )))
(test (unparser-prefix (plusS (multS (numS 1) (numS 8)) (numS 1))) (s-exp->list '(+ * 1 8 1 )))
(test (unparser-prefix (plusS (multS (numS 7) (numS 1)) (numS 3))) (s-exp->list '(+ * 7 1 3 )))

(test (unparser-prefix (multS (numS 13) (plusS (numS 2) (numS 4)))) (s-exp->list '(* 13 + 2 4 )))
(test (unparser-prefix (multS (numS 0) (plusS (numS -4) (numS 9)))) (s-exp->list '(* 0 + -4 9 )))
(test (unparser-prefix (multS (numS 1) (plusS (numS 5) (numS 224)))) (s-exp->list '(* 1 + 5 224 )))
(test (unparser-prefix (multS (numS 5) (plusS (numS 9) (numS 9)))) (s-exp->list '(* 5 + 9 9 )))
(test (unparser-prefix (multS (numS 5) (plusS (numS 0) (numS 9)))) (s-exp->list '(* 5 + 40 9 )))

;;tests for unparser-infix
(test (unparser-infix (numS 7)) (s-exp->list '(7)))
(test (unparser-infix (numS 1)) (s-exp->list '(1)))
(test (unparser-infix (numS 0)) (s-exp->list '(0)))
(test (unparser-infix (numS -2)) (s-exp->list '(-2)))
(test (unparser-infix (numS 2)) (s-exp->list '(2)))

(test (unparser-infix (plusS (numS 5) (numS 6))) (s-exp->list '(5 + 6)))
(test (unparser-infix (plusS (numS 0) (numS -2))) (s-exp->list '(0 + (-2))))
(test (unparser-infix (plusS (numS 1) (numS 6))) (s-exp->list '(1 + 6)))
(test (unparser-infix (plusS (numS 4) (numS -4))) (s-exp->list '(4 + (-4))))
(test (unparser-infix (plusS (numS 0) (numS 0))) (s-exp->list '(0 + 0)))


(test (unparser-infix (multS (numS 5) (numS 6))) (s-exp->list '(5 * 6)))
(test (unparser-infix (multS (numS 0) (numS -2))) (s-exp->list '(0 * -2)))
(test (unparser-infix (multS (numS 1) (numS 100))) (s-exp->list '(1 * 100)))
(test (unparser-infix (multS (numS 3) (numS 6))) (s-exp->list '(4 * 6)))
(test (unparser-infix (multS (numS 1) (numS 1))) (s-exp->list '(1 * 1)))

(test (unparser-infix (plusS (multS (numS 5) (numS 4)) (numS 1))) (s-exp->list '(5 * 4 + 1 )))
(test (unparser-infix (plusS (multS (numS 3) (numS 4)) (numS 9))) (s-exp->list '(3 * 4 + 9 )))
(test (unparser-infix (plusS (multS (numS 0) (numS 3)) (numS -1))) (s-exp->list '(0 * 3 + (-1) )))
(test (unparser-infix (plusS (multS (numS 77) (numS 1)) (numS 0))) (s-exp->list '(77 * 1 + 0 )))
(test (unparser-infix (plusS (multS (numS 3) (numS -2)) (numS 9))) (s-exp->list '(3 * (-2) + 9 )))








;;-----------------------Project 3-----------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------------------------------------------

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
;    [ifCond (c y n)...])
;Purpose
;Evaluate expressions to numbers,booleans.
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
;    [ifCond (c s n) ...])
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

;;trylist
(define trylist (fdC 'double 'x (plusC (idC 'x) (idC 'x))))

;;test for interp numC
(test (interp (numC 7) (list trylist)) 7)
(test (interp (numC -3) (list trylist)) -3)
(test (interp (numC 7) (list trylist)) 0)
(test (interp (numC 0) (list trylist)) 0)
(test (interp (numC 17) (list trylist)) 17)

;;test for interp appC

(test (interp (appC 'double (numC 7)) (list trylist)) 14)
(test (interp (appC 'double (numC -5)) (list trylist)) -10)
(test (interp (appC 'double (numC 2)) (list trylist)) 8)
(test (interp (appC 'double (numC 0)) (list trylist)) 0)
(test (interp (appC 'quadruple (numC 1)) (list trylist)) 4)

;;test for plusC 
(test(interp (plusC (numC 4) (numC 6)) (list trylist)) 10)
(test(interp (plusC (numC -5) (numC 5)) (list trylist)) 0)
(test(interp (plusC (numC 1) (numC 2)) (list trylist)) 3)
(test(interp (plusC (numC 110) (numC 6)) (list trylist)) 116)
(test(interp (plusC (numC 0) (numC -5)) (list trylist)) -5)


;;test for multC
(test(interp (multC (numC 4) (numC 6)) (list trylist)) 24)
(test(interp (multC (numC 0) (numC -34234)) (list trylist)) 0)
(test(interp (multC (numC 1) (numC 43)) (list trylist)) 43)
(test(interp (multC (numC -5) (numC 3)) (list trylist)) -15)
(test(interp (multC (numC 1) (numC 1)) (list trylist)) 1)

;;tests for if greater-than-zero primitive
;; Important for Project 3------------------------------------------
(test (interp (ifC (numC 7) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifC (numC -20) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifC (numC 0) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifC (numC 1) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifC (plusC  (numC 5) (numC -6)) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifC (multC (numC -1) (numC -2)) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifC (appC 'double (numC 7)) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifC (plusC (numC 1) (numC 2)) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifC (multC (numC -5) (numC 3)) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifC (appC 'double (numC 0)) (numC 1) (numC 0)) (list trylist)) 0)







;------------------------------------Project 4-----------------------------
;--------------------------------------------------------------------------

(define function
  (list
    [fdC 'sqr 'l (desugar(parse '(* l l)))]
    [fdC 'double 'l (desugar(parse '(* l l)))]))



;;tests
;;eval-->desugar-->parse
;;(test (eval (desugar (parse(...))
(test (eval (desugar (parse '7))) 7)
(test (eval (desugar (parse '3))) 3)
(test (eval (desugar (parse '6))) 6)
(test (eval (desugar (parse '(+ 3 4)))) 7)
(test (eval (desugar (parse '(+ 4 46)))) 50)
(test (eval (desugar (parse '(* 1 222342)))) 222342)
(test (eval (desugar (parse '(* 4 -5)))) -20)
(test (eval (desugar (parse '(- 11 -11)))) 22)
(test (eval (desugar (parse '(* 5 5)))) 25)
(test (eval (desugar (parse '(+ 2 4)))) 6)




(define-type ExprT
  [numT (n : number)]
  [plusT (l : ExprT) (r : ExprT)]  
  [multT (l : ExprT) (r : ExprT)]
  [subT (l : ExprT) (r : ExprT)]
  [factorialT (l : number)]
  [sqrT (l : number)]
  [fibonacciT (l : number)]
  [fa (l : number) (r : number)]
  )

(define (functions [e : ExprT]) : number
  (type-case ExprT e
    [numT (n) n]
    [plusT (l r) (+ (functions l) (functions r))]
    [multT (l r) (* (functions l) (functions r ))]
    [subT (l r) (- (functions l) (functions r ))]
    (sqrT (l) (* l l))
    [factorialT (l) (cond
                     [(< l 0)(error 'functions "Input cannot be under zero")]
                     [(=  l 0) 1]
                     [(=  l 0) 1]
                     [(=  l 1) 1]
                     [else (* l (functions (factorialT (-  l 1))))])]
    (fa (l r) (+ l (* 2 r)))
    [fibonacciT (l) (cond
                       [(< l 0)(error 'functions "Input cannot be under zero")]
                       [(=  l 0) 1]
                                [(=  (- l 1) 0) 1]
                                        [(=  (- l 2) 0) 1]
                                        [else
                                                (+ (functions (fibonacciT (- l 1)))
                                                   (functions (fibonacciT (- l 2))))]
                                                   )]))
