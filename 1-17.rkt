#lang sicp

(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (* a (halve b))))
        (else (+ (* a (dec b)) a))))