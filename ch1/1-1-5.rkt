#lang sicp

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; for applicative-order, (p) is expanded first, and then the condition expression is evaluated
(if (= x 0) 0 (p))

; for normal-order, the condition exp is evaluated first, and (p) will be evaluated only if
; the value returned by the condition exp is false.