#lang sicp

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (sqrt x)
  (define (sqr x) (* x x))
  (define (avr x y) (/ (+ x y) 2))
  (define (i g x) (avr g (/ x g)))
  (define (how-far? g x) (abs (- (sqr g) x)))
  (define (ge? g x)
    (define tolerance 0.00001)
    (< (abs (- (i g x) g)) tolerance))
  (define (si g x)
    (if (ge? g x)
        g
        (si (i g x) x)))
  (si 1.0 x))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (dec count))))
  (fib-iter 1 0 n))

(define (phi n)
  (define unit (/ (+ 1 (sqrt 5)) 2.0))
  (cond
    [(= n 0) 1]
    [else (* unit (phi (dec n)))]))

(define (closer-nat n)
  (define (strip-nat n)
    (cond
      [(and (>= n 0) (< n 1.0)) n]
      [else (strip-nat (dec n))]))
  (if (and (>= (strip-nat n) 0)
           (< (strip-nat n) 0.5))
      (- n (strip-nat n))
      (inc (- n (strip-nat n)))))

(define (is-equal? x)
  (= (fib x) (closer-nat (/ (phi x) (sqrt 5)))))


(define (psi n)
  (define unit (/ (- 1 (sqrt 5)) 2.0))
  (cond
    [(= n 0) 1]
    [else (* unit (psi (dec n)))]))

(define (is-equal?2 x)
  (= (fib x) (/ (- (phi x) (psi x)) (sqrt 5))))
; its value is #t when 0 and 1, and 2.
