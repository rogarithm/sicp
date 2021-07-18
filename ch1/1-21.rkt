#lang sicp

(define (sqr x) (* x x))
(define (inc x) (+ x 1))
(define (divides? a b) (= (remainder b a) 0))

(define (smallest-div n)
  (define (find-div n test-div)
    (cond ((> (sqr test-div) n) n)
          ((divides? test-div n) test-div)
          (else (find-div n (inc test-div)))))
  (find-div n 2))

(define (prime? n)
  (= n (smallest-div n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (sqr (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))