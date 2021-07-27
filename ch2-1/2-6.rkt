#lang sicp

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 1) zero
;(lambda (zero) (lambda (x) (zero ((1 zero) x))))

;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x)) x))))

(define (inc x) (+ x 1))
(zero inc)
(zero (inc 1))