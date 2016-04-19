#lang plai-typed

(define-type msl
  [msl-num (n : number)]
  [msl-add (lhs : msl) (rhs : msl)]
  [msl-mul (lhs : msl) (rhs : msl)]
  [msl-sub (lhs : msl) (rhs : msl)]
  )

;; eval msl -> number
;; evaluate an msl expression
;; examples
;; (msl-num 7) -> 7
;; (msl-add (msl-num 5) (msl-num 6)) -> 10
;; (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)) -> 42

(define (eval [expr : msl])
  (type-case msl expr
    [msl-num (n) n]
    [msl-add (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-sub (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-mul (lhs rhs) (* (eval lhs) (eval rhs))]))

;;tests
(test (eval (msl-num 7))  7)
(test (eval (msl-add (msl-num 3) (msl-num 4)))  7)
(test (eval (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))  42)
(test (eval (msl-mul (msl-num 0) (msl-num 4)))  0)
(test (eval (msl-sub (msl-num 7) (msl-num 4)))  3)
(test (eval (msl-add (msl-num 32) (msl-num 4)))  3)
(test (eval (msl-num 3))  7)
(test (eval (msl-mul (msl-num 3) (msl-num 4)))  12)
