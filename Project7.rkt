#lang plai-typed
;;---------------------------------------------PROJECT 7----------------------------------------------
;;-----------------------------------------------------------------------------------------------------
;;BOTAN ÖZDEMİR
;; RESOURCES https://vimeo.com/album/1987162   (CHRIS STEPHENSON VIMEO VIDEOS)

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





;Substitution
;λ-exp-> λ-exp
;Template
; (define (subst [what : λ-exp] [for : symbol] [in : λ-exp]) : λ-exp
;  (type-case ExprC in
;    (λ-sym (v) ...)
;    (λ-app (l r) ...)
;    (λ-def (v p) ...))
;Purpose
;Substitute λ-exp with other expression in λ-exp according to cond
(define (subst [what : λ-exp] [for : symbol] [in : λ-exp]) : λ-exp 
  (type-case λ-exp in
    (λ-sym (v) (if (symbol=? v for) 
                   what
                   in))
    (λ-app (l r) (λ-app (subst what for l)
                        (subst what for r)))
    (λ-def (v p)(λ-def v (subst what for p)))
    )
  )

;; Beta transformation is the transformation method which is convert lambda application to lambda substitution.
;; Chris explain this transformation in vimeo videos.
;; There is also Beta reduction to solve this method
;; β-transformation : λ-exp -> λ-exp
;; Purpose :Beta-transformation and reduction implementation of λ-calculus.
;; Template :
; (define (β-transformation (lex : λ-exp)) : λ-exp
;  (type-case λ-exp le
;    (λ-sym (v) ...)
;    (λ-app (l r) .....)
;    (λ-def (v p) .....)))
(define (β-transformation (lex : λ-exp)) : λ-exp
  (type-case λ-exp lex
    (λ-sym (v) lex) 
    (λ-app (l r) (if (λ-def? l)
                     (subst r (λ-def-v l) (λ-def-p l))
                     (λ-app (β-transformation l) (β-transformation r))))
    (λ-def (v p) (λ-def v (β-transformation p)))))


;; Functions in lambda calculus form
(define double (λ (x) (+ x x)))

(define quadruple (λ (x) (double (double x))))

(define sqr (λ (x)  (* x x)))

(define fact (λ (x) (cond
               [(= x 1) 1]
               [else (* x (fact (- x 1)))])))


(sqr 10)
(double 10)
(quadruple 10)
(fact 4)


;;doublelambdafor beta transformation
(define doublelambda
  (parselambda '(λ f (λ x (f (f x))))))

;;triplelambdafor beta transformation
(define triplelambda
  (parselambda '(λ f (λ x (f (f (f x)))))))


(define try 
  (λ-app doublelambda triplelambda))


;;Tests

(test (β-transformation doublelambda)(λ-def 'f (λ-def 'x (λ-app (λ-sym 'f) (λ-app (λ-sym 'f) (λ-sym 'x))))))

(test (β-transformation triplelambda)(λ-def 'f (λ-def 'x (λ-app (λ-sym 'f) (λ-app (λ-sym 'f) (λ-app (λ-sym 'f) (λ-sym 'x)))))))

(test(β-transformation(β-transformation triplelambda))(λ-def 'f (λ-def 'x (λ-app (λ-sym 'f) (λ-app (λ-sym 'f) (λ-app (λ-sym 'f) (λ-sym 'x)))))))

(test(β-transformation(β-transformation doublelambda))(λ-def 'f (λ-def 'x (λ-app (λ-sym 'f) (λ-app (λ-sym 'f) (λ-sym 'x))))))


(test (β-transformation 
     (β-transformation 
      (β-transformation 
       (β-transformation 
        (β-transformation 
         (β-transformation try))))))(λ-def
 'x
 (λ-def
  'x
  (λ-app
   (λ-app
    (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
    (λ-app
     (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
     (λ-app
      (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
      (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x)))))))
   (λ-app
    (λ-app
     (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
     (λ-app
      (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
      (λ-app
       (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
       (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x)))))))
    (λ-app
     (λ-app
      (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
      (λ-app
       (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
       (λ-app
        (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
        (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x)))))))
     (λ-app
      (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
      (λ-app
       (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
       (λ-app
        (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
        (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x)))))))))))))

;Bigtest
(test
 (β-transformation
  (β-transformation 
   (β-transformation 
    (β-transformation 
     (β-transformation 
      (β-transformation 
       (β-transformation 
        (β-transformation 
         (β-transformation try)))))))))
 (λ-def
 'x
 (λ-def
  'x
  (λ-app
   (λ-app
    (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
    (λ-app
     (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
     (λ-app
      (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
      (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x)))))))
   (λ-app
    (λ-app
     (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
     (λ-app
      (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
      (λ-app
       (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
       (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x)))))))
    (λ-app
     (λ-app
      (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
      (λ-app
       (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
       (λ-app
        (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
        (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x)))))))
     (λ-app
      (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
      (λ-app
       (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
       (λ-app
        (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x))))
        (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-app (λ-sym 'x) (λ-sym 'x)))))))))))))