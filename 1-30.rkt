#lang sicp

(define (cube x) (* x x x))
(define (inc x) (+ x 1))

; until a passes b, keep add (f a) and change a to next value. 
(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a)
         (sum f (next a) next b))))

(define (sum2 f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (f a) result))))
  (iter a 0))