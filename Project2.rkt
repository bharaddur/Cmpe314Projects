#lang plai-typed
;;Define msl type
;;Botan Özdemir
;; AE ::= <number>
;; AE ::= (add <AE> <AE>)
;;      | (mul <AE> <AE>)
;;      | (sub <AE> <AE>)
(define-type msl
  [msl-num (n : number)]
  [msl-add (lhs : msl) (rhs : msl)]
  [msl-mul (lhs : msl) (rhs : msl)]
  [msl-sub (lhs : msl) (rhs : msl)]
  ;[msl-expo (lhs : msl) (rhs : msl)]
  )

;; eval msl -> number
;; evaluate an msl expression
;Template
;(define (AE-Eval (ae : AE)) : number
;  (type-case AE ae
;    (AEnumber (n) n)
;    (AEadd (l r) ...)
;    (AEmul (l r) ...)))
(define (eval [expr : msl])
  (type-case msl expr
    [msl-num (n) n]
    [msl-add (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-sub (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-mul (lhs rhs) (* (eval lhs) (eval rhs))]
    ;[msl-expo (lhs rhs) (** (eval lhs) (eval rhs))]
    ))

;;tests
(test (eval (msl-num 7))  7)
(test (eval (msl-add (msl-num 3) (msl-num 4)))  7)
(test (eval (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))  42)
(test (eval (msl-mul (msl-num 0) (msl-num 4)))  0)
(test (eval (msl-sub (msl-num 7) (msl-num 4)))  3)
(test (eval (msl-add (msl-num 32) (msl-num 4)))  3)
(test (eval (msl-num 3))  7)
(test (eval (msl-mul (msl-num 3) (msl-num 4)))  12)
;(test (eval (msl-expo (msl-num 3) (msl-num 4)))  12)

          


;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (msl-num 7)
;; '(+ 3 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(+ (+ 3 4) 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))
(define (parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-add (parse (second sl)) (parse (third sl)))]
         [(*) (msl-mul (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(test (parse '7) (msl-num 7))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(+ (+ 3 4) 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))



;Defining ArithS
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (l : ArithS)])



;desugaring with bminusS and uminusS
;as -> s-expression
;This function helps the calculate substraction with 2 different style-desugaring
(define (desugar [as : ArithS]) : msl
  (type-case ArithS as
    [numS (n) (msl-num n)]
    [plusS (l r) (msl-add (desugar l)
                        (desugar r))]
    [multS (l r) (msl-mul (desugar l)
                        (desugar r))]
    [bminusS (l r) (msl-add (desugar l)
                      (msl-mul (msl-num -1) (desugar r)))]
    [uminusS (l) (desugar (bminusS (numS -1) l))]))



;;tests for Desuggaring

(test (desugar (numS 7 )) (msl-num 7))
(test (desugar (numS 0 )) (msl-num 0))
(test (desugar (numS -5 )) (msl-num -5))
(test (desugar (numS -1 )) (msl-num -1))
(test (desugar (numS 23 )) (msl-num 23))


;(test (desugar (plusS (numS 7 ) (numS 4))) (msl-num 11))
;(test (desugar (plusS (numS 5 ) (numS 4 ))) (msl-num 15))
;(test (desugar (plusS (numS 1 ) (numS 4 ))) (msl-num 5))
;(test (desugar (plusS (numS 51 ) (numS 43 ))) (msl-num 94))
(test (desugar (plusS (numS 10 ) (numS 0 ))) (msl-num 10))


;(test (desugar (multS (numS 5 ) (numS 4 ))) (msl-num 20))
;(test (desugar (multS (numS 6 ) (numS 6 ))) (msl-num 36))
;(test (desugar (multS (numS -5 ) (numS 1 ))) (msl-num -5))
;(test (desugar (multS (numS 11 ) (numS 6 ))) (msl-num 66))
(test (desugar (multS (numS 2 ) (numS 4 ))) (msl-num 8))

;(test (desugar (bminusS (numS 5 ) (numS 4 ))) (msl-num 1))
;(test (desugar (bminusS (numS 8 ) (numS 4 ))) (msl-num 4))
;(test (desugar (bminusS (numS 4 ) (numS 4 ))) (msl-num 0))
;(test (desugar (bminusS (numS 3 ) (numS 4 ))) (msl-num -1))
(test (desugar (bminusS (numS 0 ) (numS 0 ))) (msl-num 0))

;(test (desugar (uminusS (numS 5 )))  (msl-num -5))
;(test (desugar (uminusS (numS 7 ))) (msl-num -7))
;(test (desugar (uminusS (numS 0 )))  (msl-num 0))
;(test (desugar (uminusS (numS 1 )))  (msl-num -1))
(test (desugar (uminusS (numS -5 )))  (msl-num -5))



;; parse-infix
;;list of s-exp -> AE
;; Purpose
;;Parse given list of s-expression infix to msl.
(define (parse-infix [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (if (or (not (= (length sl) 3))(not (s-exp-symbol? (second sl))))
           (error 'parse-infix "input contract violation!")
           (case (s-exp->symbol (second sl))
             [(+) (msl-add (parse-infix (first sl)) (parse-infix (third sl)))]
             [(*) (msl-mul (parse-infix (first sl)) (parse-infix (third sl)))]
             [(-) (msl-sub (parse-infix (first sl)) (parse-infix (third sl)))]
             [else (error 'parse-infix "invalid list input")])))]
    (else (error 'parse "invalid input"))))



;unparser-infix
;msl -> s-expression
;This function takes msl type object and unparses it into infix bracketed s-expression.
  (define (unparser-infix [expr : msl])
    (type-case msl expr
      (msl-num (n) (list (number->s-exp n)))
      (msl-add (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '+))) (unparser-infix rhs)))
      (msl-sub (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '-))) (unparser-infix rhs)))
      (msl-mul (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '*))) (unparser-infix rhs))
      ;(msl-exp (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '**))) (unparser-infix rhs)))
               )
      ))



;unparser-prefix
;msl -> s-expression
;This function takes msl type object and unparses it into prefix bracketed s-expression.
   (define (unparser-prefix [expr : msl]) 
     (type-case msl expr
       (msl-num (n) (list (number->s-exp n)))
       (msl-add (lhs rhs) (append (list(symbol->s-exp '+)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
       (msl-sub (lhs rhs) (append (list(symbol->s-exp '-)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
       (msl-mul (lhs rhs) (append (list(symbol->s-exp '*)) (append (unparser-prefix lhs) (unparser-prefix rhs)))
       ;(msl-exp (lhs rhs) (append (list(symbol->s-exp '**)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
       )))

;;unparser-reverse-polish
;msl --> s-expression
;This function unparser the expression reverse polish
(define (unparser-reverse-polish [expr : msl])
  (type-case msl expr
    (msl-num (n) (list (number->s-exp n)))
    (msl-add (lhs rhs) (append (append (unparser-reverse-polish lhs) (unparser-reverse-polish rhs))(list (symbol->s-exp '+))))
    (msl-sub (lhs rhs) (append  (append (unparser-reverse-polish lhs) (unparser-reverse-polish rhs)) (list(symbol->s-exp '-))))
    (msl-mul (lhs rhs) (append (append (unparser-reverse-polish lhs) (unparser-reverse-polish rhs))(list (symbol->s-exp '*))))))


;;tests for unparser-prefix
(test (unparser-prefix (msl-num 5)) (s-exp->list '(5)))
(test (unparser-prefix (msl-num 8)) (s-exp->list '(8)))
(test (unparser-prefix (msl-num 15)) (s-exp->list '(15)))
(test (unparser-prefix (msl-num 20)) (s-exp->list '(20)))
(test (unparser-prefix (msl-num 12)) (s-exp->list '(5)))

(test (unparser-prefix (msl-add (msl-num 5) (msl-num 6))) (s-exp->list '(+ 5 6)))
(test (unparser-prefix (msl-add (msl-num 6) (msl-num 8))) (s-exp->list '(+ 6 8)))
(test (unparser-prefix (msl-add (msl-num 1) (msl-num 0))) (s-exp->list '(+ 1 0)))
(test (unparser-prefix (msl-add (msl-num 9) (msl-num 10))) (s-exp->list '(+ 9 10)))
(test (unparser-prefix (msl-add (msl-num 3) (msl-num 16))) (s-exp->list '(+ 3 16)))

(test (unparser-prefix (msl-mul (msl-num 5) (msl-num 6))) (s-exp->list '(* 5 6)))
(test (unparser-prefix (msl-mul (msl-num 0) (msl-num 0))) (s-exp->list '(* 0 0)))
(test (unparser-prefix (msl-mul (msl-num 3) (msl-num 3))) (s-exp->list '(* 3 3)))
(test (unparser-prefix (msl-mul (msl-num 15) (msl-num 6))) (s-exp->list '(* 15 6)))
(test (unparser-prefix (msl-mul (msl-num 5) (msl-num 6))) (s-exp->list '(* 4 6)))

(test (unparser-prefix (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9))) (s-exp->list '(+ * 3 4 9 )))
(test (unparser-prefix (msl-add (msl-mul (msl-num 5) (msl-num 0)) (msl-num -9))) (s-exp->list '(+ * 3 4 -9 )))
(test (unparser-prefix (msl-add (msl-mul (msl-num 6) (msl-num 4)) (msl-num 2))) (s-exp->list '(+ * 6 4 2 )))
(test (unparser-prefix (msl-add (msl-mul (msl-num 1) (msl-num 8)) (msl-num 1))) (s-exp->list '(+ * 1 8 1 )))
(test (unparser-prefix (msl-add (msl-mul (msl-num 7) (msl-num 1)) (msl-num 3))) (s-exp->list '(+ * 7 1 3 )))

(test (unparser-prefix (msl-mul (msl-num 13) (msl-add (msl-num 2) (msl-num 4)))) (s-exp->list '(* 13 + 2 4 )))
(test (unparser-prefix (msl-mul (msl-num 0) (msl-add (msl-num -4) (msl-num 9)))) (s-exp->list '(* 0 + -4 9 )))
(test (unparser-prefix (msl-mul (msl-num 1) (msl-add (msl-num 5) (msl-num 224)))) (s-exp->list '(* 1 + 5 224 )))
(test (unparser-prefix (msl-mul (msl-num 5) (msl-add (msl-num 9) (msl-num 9)))) (s-exp->list '(* 5 + 9 9 )))
(test (unparser-prefix (msl-mul (msl-num 5) (msl-add (msl-num 0) (msl-num 9)))) (s-exp->list '(* 5 + 40 9 )))

;;tests for unparser-infix
(test (unparser-infix (msl-num 7)) (s-exp->list '(7)))
(test (unparser-infix (msl-num 1)) (s-exp->list '(1)))
(test (unparser-infix (msl-num 0)) (s-exp->list '(0)))
(test (unparser-infix (msl-num -2)) (s-exp->list '(-2)))
(test (unparser-infix (msl-num 2)) (s-exp->list '(2)))

(test (unparser-infix (msl-add (msl-num 5) (msl-num 6))) (s-exp->list '(5 + 6)))
(test (unparser-infix (msl-add (msl-num 0) (msl-num -2))) (s-exp->list '(0 + (-2))))
(test (unparser-infix (msl-add (msl-num 1) (msl-num 6))) (s-exp->list '(1 + 6)))
(test (unparser-infix (msl-add (msl-num 4) (msl-num -4))) (s-exp->list '(4 + (-4))))
(test (unparser-infix (msl-add (msl-num 0) (msl-num 0))) (s-exp->list '(0 + 0)))


(test (unparser-infix (msl-mul (msl-num 5) (msl-num 6))) (s-exp->list '(5 * 6)))
(test (unparser-infix (msl-mul (msl-num 0) (msl-num -2))) (s-exp->list '(0 * -2)))
(test (unparser-infix (msl-mul (msl-num 1) (msl-num 100))) (s-exp->list '(1 * 100)))
(test (unparser-infix (msl-mul (msl-num 3) (msl-num 6))) (s-exp->list '(4 * 6)))
(test (unparser-infix (msl-mul (msl-num 1) (msl-num 1))) (s-exp->list '(1 * 1)))

(test (unparser-infix (msl-add (msl-mul (msl-num 5) (msl-num 4)) (msl-num 1))) (s-exp->list '(5 * 4 + 1 )))
(test (unparser-infix (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9))) (s-exp->list '(3 * 4 + 9 )))
(test (unparser-infix (msl-add (msl-mul (msl-num 0) (msl-num 3)) (msl-num -1))) (s-exp->list '(0 * 3 + (-1) )))
(test (unparser-infix (msl-add (msl-mul (msl-num 77) (msl-num 1)) (msl-num 0))) (s-exp->list '(77 * 1 + 0 )))
(test (unparser-infix (msl-add (msl-mul (msl-num 3) (msl-num -2)) (msl-num 9))) (s-exp->list '(3 * (-2) + 9 )))











;;-----------------------Project 3-----------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------------------------------------------

;defining expression type
;S->aS
;Use in other methods
;; Grammar
;; ExprC ::= <number>
;; s::=symbol
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
  [ifCond (expression1 : ExprC) (expression2 : ExprC) (expression : ExprC)])


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
  [ifCond (c y n) (ifCond (subst what for c)
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
    [ifCond (c y n) (cond
           [(> (interp c fds) 0) (interp y fds)]
           [else (interp n fds)])]))

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
(test (interp (ifCond (numC 7) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifCond (numC -20) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifCond (numC 0) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifCond (numC 1) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifCond (plusC  (numC 5) (numC -6)) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifCond (multC (numC -1) (numC -2)) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifCond (appC 'double (numC 7)) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifCond (plusC (numC 1) (numC 2)) (numC 1) (numC 0)) (list trylist)) 1)
(test (interp (ifCond (multC (numC -5) (numC 3)) (numC 1) (numC 0)) (list trylist)) 0)
(test (interp (ifCond (appC 'double (numC 0)) (numC 1) (numC 0)) (list trylist)) 0)






    






