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

; u: upper / d: down / l: left / r: right
(define (make-rectangle ul ur dl dr)
  (cons (cons (make-segment ul ur)
              (make-segment ur dr))
        (cons (make-segment dl dr)
              (make-segment dl ul))))

(define (top-segment rectangle)
  (car (car rectangle)))
(define (bottom-segment rectangle)
  (car (cdr rectangle)))
(define (left-segment rectangle)
  (cdr (cdr rectangle)))
(define (right-segment rectangle)
  (cdr (car rectangle)))

(define (line-length line)
  (sqrt (+ ((lambda (x) (* x x)) (- (x-point (start-segment line)) (x-point (end-segment line))))
           ((lambda (x) (* x x)) (- (y-point (start-segment line)) (y-point (end-segment line)))))))

(define (circumference rectangle)
  (+ (line-length (top-segment rectangle))
     (line-length (bottom-segment rectangle))
     (line-length (left-segment rectangle))
     (line-length (right-segment rectangle))))

(define (area rectangle)
  (* (line-length (top-segment rectangle))
     (line-length (left-segment rectangle))))