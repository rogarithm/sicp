#lang sicp

(define (compose f g) (lambda (x) (f (g x))))
(define (inc x) (+ x 1))
(define (sqr x) (* x x))
((compose sqr inc) 6)