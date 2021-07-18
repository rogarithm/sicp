#lang sicp

(define (cube x) (* x x x))
(define (inc x) (+ x 1))

; until a passes b, keep add (f a) and change a to next value. 
(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a)
         (sum f (next a) next b))))

(define (sum-iter f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (f a) result))))
  (iter a 0))

; a
(define (product f a next b)
  (if (> a b)
      1
      (* (f a)
         (product f (next a) next b))))

(define (pi-elem n)
  (* (/ (* 2 n) (- (* 2 n) 1))
     (/ (* 2 n) (+ (* 2 n) 1))))

(define (pi a b)
  (* 2 (product-iter pi-elem a inc b)))

; b
(define (product-iter f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (f a) result))))
  (iter a 1))