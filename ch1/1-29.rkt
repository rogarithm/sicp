#lang sicp

(define (cube x) (* x x x))

; until a passes b, keep add (f a) and change a to next value. 
(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a)
         (sum f (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b)
  (define (inner f a b n)
    (define (y k) (f (+ a (* k (h n)))))
    (define (h n) (/ (- b a) n))
    (define (mid-ys k n)
      (cond
        ((= k n) 0)
        ((odd? k) (+ (* 4 (y k)) (mid-ys (inc k) n)))
        ((even? k) (+ (* 2 (y k)) (mid-ys (inc k) n)))))
    (* (/ (h n) 3) (+ (y a) (mid-ys 1 n) (y (+ a (* n (h n)))))))
  (inner f a b 10000))
;0.24666666999999992
;0.24966666666700024
;0.2499666666666678

;0.24998750000000042