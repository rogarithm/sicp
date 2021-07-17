#lang sicp

(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n)
  (lambda (x) (if (= n 1) (f x) ((compose (repeated f (- n 1)) f) x))))
(define (sqr x) (* x x))
((repeated sqr 2) 5)