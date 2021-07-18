#lang sicp

(define tolerance 0.00001)
(define (avr x y) (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (g-val x) (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
(define (flp n) (fixed-point (lambda (x) (/ (log 1000) (log x))) n))
(define (flp-avr n) (fixed-point (lambda (x) (avr x (/ (log 1000) (log x)))) n))