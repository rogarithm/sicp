#lang sicp

; utilities
(define (dec x) (- x 1))

(define (power n k)
  (if (= k 0)
      1
      (* n (power n (dec k)))))

(define (make-int a b)
  (cons a b))

; or does make-int need to be
; (cons (num-power-2 a) (num-power-3 b))?
; if so, what & how to implement 'car' and 'cdr'?

(define (num-power-2 int)
  (power 2 (car int)))

(define (num-power-3 int)
  (power 3 (cdr int)))