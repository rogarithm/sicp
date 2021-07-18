#lang sicp

(define (cube x) (* x x x))
(define (inc x) (+ x 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (product-o f a next b)
  (if (> a b)
      1
      (* (f a)
         (product f (next a) next b))))

(define (product-a f a next b)
  (accumulate * 1 f a next b))

(define (product-i f a next b)
  (accumulate-iter * 1 f a next b))