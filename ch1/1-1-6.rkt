#lang sicp

; why this cannot be used? because the value of else clause cannot be returned
; instantly. As it's recursive, second si could either be true or false.
(define (ni p t e)
  (cond (p t)
        (else e)))

(define (si g x)
  (ni (ge? g x)
      g
      (si (i g x) x)))

(define (sqr x) (* x x))

(define (ge? g x)
  (< (abs (- (sqr g) x)) 0.001))

(define (i g x)
  (avr g (/ x g)))

(define (avr x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (si 1.0 x))

(sqrt 9)