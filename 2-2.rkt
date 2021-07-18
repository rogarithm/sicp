#lang sicp

; utility
(define (avr x y)
  (/ (+ x y) 2))

; for line data
(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

(define (midpoint-segment line)
  (make-point (avr (x-point (start-segment line)) (x-point (end-segment line)))
              (avr (y-point (start-segment line)) (y-point (end-segment line)))))

; for point data
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

; for printing
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))