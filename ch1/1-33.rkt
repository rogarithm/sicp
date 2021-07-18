#lang sicp

(define (cube x) (* x x x))
(define (inc x) (+ x 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (filtered-a filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((not (filter a)) (filtered-a filter combiner null-value term (next a) next b))
        (else (combiner (term a) (filtered-a filter combiner null-value term (next a) next b)))))

(define (cube-odd a b)
  (filtered-a odd? + 0 cube a inc b))

; a
(filtered-a prime? + 0 square a inc b)

; b
(filtered-a is-gcd-one? * 1 identity a inc b)