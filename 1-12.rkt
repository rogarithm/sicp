#lang sicp

(define (p-val n)
  (define (xth-perfect-triangle x)
    (cond
      [(= x 1) 1]
      [else (+ x (xth-perfect-triangle (- x 1)))]))
  (define (what-floor x)
    (cond
      [(= n 1) 1]
      [(and (> n (xth-perfect-triangle x)) (<= n (xth-perfect-triangle (+ x 1)))) (+ x 1)]
      [else (what-floor (+ x 1))]))
  (define (left? n)
    (cond
      [(= n 1) #t]
      [else (= (- n 1) (xth-perfect-triangle (- (what-floor 1) 1)))]))
  (define (right? n)
    (cond
      [(= n 1) #t]
      [else (= n (xth-perfect-triangle (what-floor 1)))]))
  (define (up-lft-val n)
    (p-val (- n (what-floor 1))))
  (define (up-rht-val n)
    (p-val (+ (- n (what-floor 1)) 1)))
  (cond
    [(or (left? n) (right? n)) 1]
    [else (+ (up-lft-val n) (up-rht-val n))]))