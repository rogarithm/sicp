#lang sicp

(define (sqr x) (* x x))
(define (cb x) (* x x x))
(define (next g x) (/ (+ (/ x (sqr g))
                         (* 2 g))
                      3))

;(define (ge? g x)
;  (< (abs (- (cb g) x)) 0.001))

(define (i g x) (next g x))

(define (how-far? g x)
  (abs (- (cb g) x)))

(define (ge? g x)
  (< (abs (- (how-far? g x) (how-far? (i g x) x))) 0.000000001)) ;<-is this really refined???

(define (si g x)
  (if (ge? g x)
      g
      (si (i g x) x)))

(define (cbrt x)
  (si 1.0 x))